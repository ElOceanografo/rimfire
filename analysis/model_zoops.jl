using DataFrames, DataFramesMeta, SDWBA, Distributions

const sound_speed = 1500.0

srand(697180031068324823)

dB_mean(x) = 10 * log10(mean(10.^(x ./ 10)))

rdata = read_rda("net_data.Rdata", convertdataframes=true)
individuals = @linq rdata["individuals"] |>
	where(:LifeStage .!= "Nauplius")

counts = rdata["counts"]

summary_lengths = @linq individuals |>
	by([:trip, :Lake, :Group], mean=mean(:Length), std=std(:Length))


nsim = 1000
zoop_ts = by(individuals, [:trip, :Lake, :Group]) do df
	animal = first(df[:Group])
	if animal == "Copepods"
		scat = Models.calanoid_copepod
	else animal == "Cladocerans"
		scat = Models.daphnia
	end

	sigma = max(std(df[:Length]), 0.1)
	L = Normal(mean(df[:Length]) / 1e3, sigma / 1e3)
	TS120 = zeros(nsim)
	TS710 = zeros(nsim)
	for i in 1:nsim
		s = rescale(scat, rand(L) / length(scat))
		TS120[i] = target_strength(s, 120e3, sound_speed)
		TS710[i] = target_strength(s, 710e3, sound_speed)
	end
	DataFrame(freq = [120, 710], TS = [dB_mean(TS120), dB_mean(TS710)])
end

proportions = @based_on(groupby(counts, [:trip, :Lake, :Group]), n = sum(:Count))
proportions = @linq proportions |>
	unstack(:Group, :n) |>
	transform(Total =  :Cladocerans + :Copepods) |>
	transform(pcope = :Copepods ./ :Total) |>
	select(:trip, :Lake, :pcope)


zoop_ts = join(zoop_ts, proportions, on=[:trip, :Lake])
@byrow! zoop_ts begin
	if :Group == "Cladocerans"
		:pcope = 1 - :pcope
	end
end
rename!(zoop_ts, :pcope, :proportion)


writetable("nets/zoop_ts.csv", zoop_ts)

