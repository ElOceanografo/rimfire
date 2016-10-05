using DataFrames, DataFramesMeta, SDWBA, Distributions, FileIO

const sound_speed = 1500.0

srand(697180031068324823)

dB_mean(x) = 10 * log10(mean(10.^(x ./ 10)))

rdata = load(File(format"RData", "net_data.Rdata"))
individuals = rdata["individuals"]

counts = rdata["counts"] 


individuals[:model] = copy(individuals[:Group])
individuals[:model][individuals[:LifeStage] .== "Nauplius"] = "Nauplius"
individuals = @where(individuals, (:model .== "Copepods") | (:model .== "Cladocerans") | (:model .== "Nauplius"))

counts[:model] = copy(counts[:Group])
counts[:model][counts[:LifeStage] .== "Nauplius"] = "Nauplius"
counts = @where(counts, (:model .== "Copepods") | (:model .== "Cladocerans") | (:model .== "Nauplius"))


summary_lengths = @linq individuals |>
	by([:trip, :Lake, :model], mean_length=mean(:Length))

# Length-weight regressions from Culver et al. 1985, CJFAS
# W = a*L^b (W in micrograms, L in mm)
lw_regression = DataFrame(
	model = ["Copepods", "Cladocerans", "Nauplius"],
	a = [7, 7.4977, 3.0093],
	b = [2.1, 1.5644, 1.7064])

individuals = join(individuals, lw_regression, on=[:model], kind=:left)
individuals = @transform(individuals, weight = :a .* :Length.^:b * 1e-6)


models = Dict("Copepods" => Models.calanoid_copepod,
			  "Nauplius" => Models.nauplius,
			  "Cladocerans" => Models.daphnia2)

nsim = 1000
zoop_ts = by(individuals, [:trip, :Lake, :model]) do df
	scat = models[first(df[:model])]
	sigma = max(std(df[:Length]), 0.0002)
	L = Normal(mean(df[:Length]) / 1e3, sigma / 1e3)
	TS120 = zeros(nsim)
	TS710 = zeros(nsim)
	for i in 1:nsim
		s = rescale(scat, rand(L) / length(scat))
		TS120[i] = target_strength(s, 120e3, sound_speed)
		TS710[i] = target_strength(s, 710e3, sound_speed)
	end
	DataFrame(freq = [120, 710],
		TS = [dB_mean(TS120), dB_mean(TS710)],
		weight = [mean(df[:weight]), mean(df[:weight])])
end

proportions = @based_on(groupby(counts, [:trip, :Lake, :model]), n = sum(:Count))
totals = @by(counts, [:trip, :Lake], total = sum(:Count))
proportions = @linq proportions |>
	join(totals, on=[:trip, :Lake]) |>
	transform(proportion = :n ./ :total)


zoop_ts = join(zoop_ts, proportions, on=[:trip, :Lake, :model])


writetable("nets/zoop_ts.csv", zoop_ts)

