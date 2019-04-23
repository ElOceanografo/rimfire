using DataFrames
using DataFramesMeta
using SDWBA
using Distributions
using FileIO
using Random

const sound_speed = 1500.0

Random.seed!(697180031068324823)

dB_mean(x) = 10 * log10(mean(10 .^(x ./ 10)))
dB_var(x) = 10 * log10(var(10 .^ (x ./ 10)))

function zoop_volume(s::Scatterer)
    v = 0.0
    for i in 2:length(s.a)
        # volume for truncated cone
        h = sqrt(sum((s.r[:, i-1] - s.r[:, i]).^2))
        v += pi * h / 3 * (s.a[i-1]^2 + s.a[i-1]*s.a[i] + s.a[i]^2)
    end
    return v
end


rdata = load(File(format"RData", "net_data.Rdata"))
individuals = rdata["individuals"]

counts = rdata["counts"]


individuals[:model] = copy(individuals[:Group])
individuals[:model][individuals[:LifeStage] .== "Nauplius"] .= "Nauplius"
individuals = @where(individuals, (:model .== "Copepods") .| (:model .== "Cladocerans") .| (:model .== "Nauplius"))

counts[:model] = copy(counts[:Group])
counts = @where(counts, (:model .== "Copepods") .| (:model .== "Cladocerans") .| (:model .== "Nauplius"))
counts[ismissing.(counts[:LifeStage]), :LifeStage] = "Adult"
counts[:model][counts[:LifeStage] .== "Nauplius"] .= "Nauplius"


summary_lengths = @linq individuals |>
	by([:trip, :Lake, :model], mean_length=mean(:Length))

# Length-dry weight regressions from Culver et al. 1985, CJFAS
# W = a*L^b (W in micrograms, L in mm)
lw_regression = DataFrame(
	model = ["Copepods", "Cladocerans", "Nauplius"],
	a = [7, 7.4977, 3.0093],
	b = [2.1, 1.5644, 1.7064])

individuals = join(individuals, lw_regression, on=[:model], kind=:left)
# factor of 1e-6 to convert dry weights from μg to grams
individuals = @transform(individuals, dryweight = :a .* :Length.^:b * 1e-6)


models = Dict("Copepods" => Models.calanoid_copepod,
			  "Nauplius" => Models.nauplius,
			  "Cladocerans" => Models.daphnia2)


avg_nauplius_length = mean(@where(individuals, :model .== "Nauplius")[:Length])
avg_cladoceran_length = mean(@where(individuals, :model .== "Cladocerans")[:Length])

individuals_imputed = DataFrame(
    trip = ["2014-06", "2014-09", "2014-09"],
    Lake = ["Eleanor", "Cherry", "Eleanor"],
    model = ["Nauplius", "Nauplius", "Cladocerans"],
    Length = [avg_nauplius_length, avg_nauplius_length, avg_cladoceran_length])
individuals_imputed = join(individuals_imputed, lw_regression, on=[:model], kind=:left)
individuals_imputed = @transform(individuals_imputed, dryweight = :a .* :Length.^:b * 1e-6)

individuals = @select(individuals, :trip, :Lake, :model, :Length, :dryweight)
individuals_imputed = @select(individuals_imputed, :trip, :Lake, :model, :Length, :dryweight)
individuals = [individuals; individuals_imputed]

nsim = 1000
zoop_ts = by(individuals, [:trip, :Lake, :model]) do df
	scat = models[first(df[:model])]
    # in case all the lengths are the same,
	sigma = std(df[:Length])
	sigma = isfinite(sigma) & (sigma > 0) ? sigma : 0.0002
	L = Normal(mean(df[:Length]) / 1e3, sigma / 1e3)
	TS120 = zeros(nsim)
	TS710 = zeros(nsim)
    volume = zeros(nsim)
	for i in 1:nsim
		s = rescale(scat, rand(L) / length(scat))
		TS120[i] = target_strength(s, 120e3, sound_speed)
		TS710[i] = target_strength(s, 710e3, sound_speed)
        volume[i] = zoop_volume(s)
	end
	DataFrame(freq = [120, 710],
		TS = [dB_mean(TS120), dB_mean(TS710)],
		TS_var = [var(TS120), var(TS710)],
        volume = [mean(volume), mean(volume)],
		dryweight = [mean(df[:dryweight]), mean(df[:dryweight])])
end

totals = @by(counts, [:trip, :Lake], total = sum(:Count))
proportions = @linq counts |>
    by([:trip, :Lake, :model], n=sum(:Count)) |>
	join(totals, on=[:trip, :Lake], kind=:left) |>
	transform(proportion = :n ./ :total)


zoop_ts = join(zoop_ts, proportions, on=[:trip, :Lake, :model], kind=:left)
# convert dry to wet biomass (equation from Wiebe et al. 1975). Factor of 1e3
# is because equation was fit using DW values in mg, but wet-weight values in g.
zoop_ts[:weight] =  10 .^(-1.983 .+ 0.922 * log10.(zoop_ts[:dryweight] * 1e3))

save("nets/zoop_ts.csv", zoop_ts)
