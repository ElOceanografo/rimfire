GRAPHICS_DIR = graphics/
FIGURES = $(addprefix $(GRAPHICS_DIR),\
		  maps.png \
		  example_echograms.png \
		  echo_zooplankton.png \
		  echo_fish.png \
		  echo_classification.png \
		  dB_difference.png\
		  profiles.png \
		  net_vs_acoustics.png \
		  zoop_length_indy_tahoe.png \
		  zoop_length_cherry_eleanor.png\
		  zoop_composition_indy_tahoe.png \
		  zoop_composition_cherry_eleanor.png\
		  seasonal_biomass.png \
		  variograms.png \
		  inlet_distance.png)

all : $(FIGURES) 
	
graphics/maps.png: maps.R tracks.Rdata lake_outlines.Rdata
	R CMD BATCH maps.R /dev/null

graphics/echo_zooplankton.png: acoustics.R acoustics.Rdata
	R CMD BATCH acoustics.R /dev/null

graphics/echo_fish.png: acoustics.R acoustics.Rdata nets/zoop_ts.csv
	R CMD BATCH acoustics.R /dev/null

graphics/echo_classification.png: acoustics.R acoustics.Rdata nets/zoop_ts.csv
	R CMD BATCH acoustics.R /dev/null

graphics/dB_difference.png: acoustics.R acoustics.Rdata nets/zoop_ts.csv
	R CMD BATCH acoustics.R /dev/null

graphics/example_echograms.png: acoustics.R acoustics.Rdata nets/zoop_ts.csv fish_TS.csv
	R CMD BATCH acoustics.R /dev/null
	
graphics/profiles.png: acoustics.R acoustics.Rdata nets/zoop_ts.csv fish_TS.csv
	R CMD BATCH acoustics.R /dev/null
	
graphics/seasonal_biomass.png: acoustics.R acoustics.Rdata nets/zoop_ts.csv fish_TS.csv
	R CMD BATCH acoustics.R /dev/null
	
graphics/net_vs_acoustics.png: acoustics.R acoustics.Rdata
	R CMD BATCH acoustics.R /dev/null

graphics/zoop_length_indy_tahoe.png: nets.R net_data.Rdata
	R CMD BATCH nets.R /dev/null

graphics/zoop_length_cherry_eleanor.png: nets.R net_data.Rdata
	R CMD BATCH nets.R /dev/null

graphics/zoop_composition_indy_tahoe.png: nets.R net_data.Rdata
	R CMD BATCH nets.R /dev/null

graphics/zoop_composition_cherry_eleanor.png: nets.R net_data.Rdata
	R CMD BATCH nets.R /dev/null

graphics/inlet_distance.png: tracks.Rdata lake_outlines.Rdata
	R CMD BATCH geostats.R

graphics/variograms.png: tracks.Rdata lake_outlines.Rdata
	R CMD BATCH geostats.R
	
nets/zoop_ts.csv: model_zoops.jl net_data.Rdata
	julia model_zoops.jl

acoustics.Rdata: load_acoustics.R ../acoustics/Exports/*
	R CMD BATCH load_acoustics.R /dev/null

net_data.Rdata: load_net_data.R nets/RimFireZooplanktonMeasurements.csv nets/RimFireZooplanktonCounts.csv
	R CMD BATCH load_net_data.R

lake_outlines.Rdata: load_mapdata.R NHD_H_1804_Shape NHD_H_1605_Shape
	R CMD BATCH load_mapdata.R /dev/null

NHD_H_1804_Shape :  | NHD_H_1804_Shape.zip 
	mkdir NHD_H_1804_Shape
	unzip NHD_H_1804_Shape.zip -d NHD_H_1804_Shape

NHD_H_1804_Shape.zip:
	wget https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU4/HighResolution/Shape/NHD_H_1804_Shape.zip

NHD_H_1605_Shape :  | NHD_H_1605_Shape.zip 
	mkdir NHD_H_1605_Shape
	unzip NHD_H_1605_Shape.zip -d NHD_H_1605_Shape

NHD_H_1605_Shape.zip:
	wget https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU4/HighResolution/Shape/NHD_H_1605_Shape.zip

clean:
	rm *.Rout


