GRAPHICS_DIR = graphics/
FIGURES = $(addprefix $(GRAPHICS_DIR),\
		  tracklines.pdf Cherry_echograms.png Eleanor_echograms.png\
		  acoustic_profiles.pdf acoustic_profiles_date.pdf \
		  zoop_length_distributions.pdf)

all : $(FIGURES) 
	
graphics/tracklines.pdf: maps.R acoustics.Rdata lake_outlines.Rdata
	R CMD BATCH maps.R /dev/null

graphics/Cherry_echograms.png: acoustics.R acoustics.Rdata
	R CMD BATCH acoustics.R /dev/null

graphics/Eleanor_echograms.png: acoustics.R acoustics.Rdata nets/zoop_ts.csv
	R CMD BATCH acoustics.R /dev/null

graphics/acoustic_profiles.pdf: acoustics.R acoustics.Rdata
	R CMD BATCH acoustics.R /dev/null

graphics/acoustic_profiles_date.pdf: acoustics.R acoustics.Rdata
	R CMD BATCH acoustics.R /dev/null

graphics/zoop_length_distributions.pdf: nets.R net_data.Rdata
	R CMD BATCH nets.R

nets/zoop_ts.csv: model_zoops.jl net_data.Rdata
	julia model_zoops.jl

acoustics.Rdata: load_acoustics.R
	R CMD BATCH load_acoustics.R /dev/null

net_data.Rdata: load_net_data.R
	R CMD BATCH load_net_data.R

lake_outlines.Rdata: load_mapdata.R Shape
	R CMD BATCH load_mapdata.R /dev/null

Shape :  | NHD_H_1804_Shape.zip 
	unzip NHD_H_1804_Shape.zip 

NHD_H_1804_Shape.zip:
	wget https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU4/HighResolution/Shape/NHD_H_1804_Shape.zip

clean:
	rm *.Rout


