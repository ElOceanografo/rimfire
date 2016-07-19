GRAPHICS_DIR = graphics/
FIGURES = $(addprefix $(GRAPHICS_DIR),\
		  tracklines.pdf echo_zooplankton.png echo_fish.png \
		  echo_classification.png dB_difference.png\
		  zoop_profiles.png fish_profiles.png \
		  zoop_length_distributions.pdf)

all : $(FIGURES) 
	
graphics/tracklines.pdf: maps.R acoustics.Rdata lake_outlines.Rdata
	R CMD BATCH maps.R /dev/null

graphics/echo_zooplankton.png: acoustics.R acoustics.Rdata
	R CMD BATCH acoustics.R /dev/null

graphics/echo_fish.png: acoustics.R acoustics.Rdata nets/zoop_ts.csv
	R CMD BATCH acoustics.R /dev/null

graphics/echo_classification.png: acoustics.R acoustics.Rdata nets/zoop_ts.csv
	R CMD BATCH acoustics.R /dev/null

graphics/dB_difference.png: acoustics.R acoustics.Rdata nets/zoop_ts.csv
	R CMD BATCH acoustics.R /dev/null

graphics/zoop_profiles.pdf: acoustics.R acoustics.Rdata
	R CMD BATCH acoustics.R /dev/null

graphics/fish_profiles.pdf: acoustics.R acoustics.Rdata
	R CMD BATCH acoustics.R /dev/null

graphics/zoop_length_distributions.pdf: nets.R net_data.Rdata
	R CMD BATCH nets.R

nets/zoop_ts.csv: model_zoops.jl net_data.Rdata
	julia model_zoops.jl

acoustics.Rdata: load_acoustics.R ../acoustics/Exports/*
	R CMD BATCH load_acoustics.R /dev/null

net_data.Rdata: load_net_data.R
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


