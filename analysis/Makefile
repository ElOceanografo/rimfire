GRAPHICS_DIR = graphics/
FIGURES = $(addprefix $(GRAPHICS_DIR),\
		  maps.eps \
		  ctd.eps \
		  example_echograms.tiff \
		  profiles.eps \
		  net_vs_acoustics.eps \
		  seasonal_biomass.eps \
		  variograms.eps)
TABLES = nets/ts.display.table.csv \
		  nets/ts.display.table.fish.csv \
		  nets/zoop_composition_cherry_eleanor.csv \
		  nets/zoop_composition_indy_tahoe.csv \
		  lake_biomass_display.csv

all : $(FIGURES) $(TABLES)
	

######################################################	
# Figure recipes
######################################################	

graphics/maps.eps: maps.R tracks.Rdata lake_outlines.Rdata
	R CMD BATCH maps.R /dev/null

graphics/ctd.eps: ctd.R 
	R CMD BATCH ctd.R /dev/null

graphics/example_echograms.tiff: acoustics.R acoustics.Rdata nets/zoop_ts.csv fish_TS.csv
	R CMD BATCH acoustics.R /dev/null
	
graphics/profiles.eps: acoustics.R acoustics.Rdata nets/zoop_ts.csv fish_TS.csv
	R CMD BATCH acoustics.R /dev/null
	
graphics/seasonal_biomass.eps: acoustics.R acoustics.Rdata nets/zoop_ts.csv fish_TS.csv
	R CMD BATCH acoustics.R /dev/null
	
graphics/net_vs_acoustics.eps: acoustics.R acoustics.Rdata
	R CMD BATCH acoustics.R /dev/null

graphics/variograms.eps: tracks.Rdata lake_outlines.Rdata
	R CMD BATCH geostats.R /dev/null

######################################################	
# Output table recipes
######################################################	

nets/zoop_ts.csv: model_zoops.jl net_data.Rdata
	julia model_zoops.jl

fish_TS.csv: single_targets.R single_targets.Rdata
	R CMD BATCH single_targets.R /dev/null

nets/ts.display.table.csv: acoustics.R nets/zoop_ts.csv
	R CMD BATCH acoustics.R /dev/null

nets/ts.display.table.fish.csv: single_targets.R single_targets.Rdata
	R CMD BATCH single_targets.R /dev/null

nets/zoop_composition_cherry_eleanor.csv: nets.R net_data.Rdata
	R CMD BATCH nets.R /dev/null
	
nets/zoop_composition_indy_tahoe.csv: nets.R net_data.Rdata
	R CMD BATCH nets.R /dev/null

lake_biomass_display.csv: acoustics.R acoustics.Rdata net_data.Rdata nets/zoop_ts.csv fish_TS.csv
	R CMD BATCH acoustics.R /dev/null

#####################################################	
# Data formatting and downloads
######################################################	

acoustics.Rdata: load_acoustics.R ../acoustics/Exports/*
	R CMD BATCH load_acoustics.R /dev/null
	
single_targets.Rdata: load_acoustics.R ../acoustics/Exports/*
	R CMD BATCH load_acoustics.R /dev/null

net_data.Rdata: load_net_data.R nets/RimFireZooplanktonMeasurements.csv nets/RimFireZooplanktonCounts.csv
	R CMD BATCH load_net_data.R

lake_outlines.Rdata: load_mapdata.R NHD_H_1804_Shape NHD_H_1605_Shape
	R CMD BATCH load_mapdata.R /dev/null

NHD_H_1804_Shape:  | NHD_H_1804_Shape.zip 
	mkdir NHD_H_1804_Shape
	unzip NHD_H_1804_Shape.zip -d NHD_H_1804_Shape

NHD_H_1804_Shape.zip:
	wget https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU4/HighResolution/Shape/NHD_H_1804_Shape.zip

NHD_H_1605_Shape:  | NHD_H_1605_Shape.zip 
	mkdir NHD_H_1605_Shape
	unzip NHD_H_1605_Shape.zip -d NHD_H_1605_Shape

NHD_H_1605_Shape.zip:
	wget https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU4/HighResolution/Shape/NHD_H_1605_Shape.zip

clean:
	rm *.Rout


