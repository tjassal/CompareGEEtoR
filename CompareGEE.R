# Title:  CompareGEE.R
# 28 Jan 2020  
# Author: Tim Assal
#code will load mask of AOI; code will add all spectral bands and indices; 
#then load NDVI from GEE and compare
#this code can serve as a blueprint to compare GEE and data pulled down from Earth Explorer 
#be sure to read the notes at the beginning of the GEE script with respect to type of Landsat data

#this is the shortform of the code. I created a short form for github that 
#only loads a small subset of the area

library(sp)      #classes and methods for spatial data library
library(rgdal)   #Geospatial Data Abstraction Library
library(raster)  #Geographic data analysis and modeling library
library(stringr) #strip filename
library(sf)

########
#1. Load AOI - extent for working grids
#######
JME.AOI<-raster("SourceData/Mask/JME_AOI_subset.tif")
plot(JME.AOI)
# #subset the AOI
# mask.c<-crop(JME.AOI, extent(380000, 390000, 3940000, 3960000))
# plot(mask.c)


######
#2. Load 2016 Landsat imagery into a raster stack
######

#Load ndvi
ndvi.name<- list.files(path="SourceData/OLI/", pattern="*_ndvi.tif", full.names=F, recursive=FALSE)
print(ndvi.name)
EE.ndvi<-raster(paste("SourceData/OLI/", ndvi.name, sep=""))
plot(EE.ndvi)


#get CFmask and assign CFmask by name (called BQA in this version of OLI)
CFmask <- list.files(path="SourceData/OLI/", pattern="*pixel_qa.tif$", full.names=F, recursive=FALSE)
print(CFmask)
#assign to raster class
CFmask1<-raster(paste("SourceData/OLI/", CFmask, sep=""))
#crop to AOI
CFmask.c<-CFmask1*JME.AOI
plot(CFmask.c)

#reclassify Fmask from Landsat- output is 0 and NoData
#values 322 and 386 are clear pixels - those are the only two values that will be set to 0; all others = NA
CFmask.r<-reclassify(CFmask.c, c(-Inf, 321, NA, 321, 322, 0, 323, 385, NA, 385, 386, 0, 387, Inf, NA))
CFmask.r
plot(CFmask.r)

#apply mask - add layers together (including original mask. Areas of 0 remain same, areas of NA in mask become   NA in NDVI
ndvi.mask<-EE.ndvi+CFmask.r #I removed the orignal mask
plot(ndvi.mask)
#plot(ndvi.mask)

#crop to AOI
EE.ndvi.c<-ndvi.mask*JME.AOI
plot(EE.ndvi.c)
EE.ndvi.c

# writeRaster(mask.c,filename="SourceData/Mask/JME_AOI_subset", format="GTiff", datatype='FLT4S',overwrite=TRUE) 
# writeRaster(CFmask.c,filename="SourceData/OLI/pixel_qu", format="GTiff", datatype='FLT4S',overwrite=TRUE) 


######
#3. Add GEE ndvi
######

########################
#I created a GEE ndvi layer using the CompareGEEtoR script
#see the notes at the beginning of that script with respect to projections, etc. 
GEE.ndvi<-raster("SourceData/GEE/GEE_ndvi20161011.tif")
plot(GEE.ndvi)
st_crs(GEE.ndvi)
GEE.ndvi


#clip it to the master raster b/c it came in very large from GEE
GEE.ndvi.C<-GEE.ndvi+CFmask.r
plot(GEE.ndvi.C)
GEE.ndvi.C
# #scale to match ndvi
# GEE.ndvi.C2<-GEE.ndvi.C*10000
# #must round so the numbers match both formulas
GEE.ndvi.C2<-round(GEE.ndvi.C, digits = 0)


######
#Band math to compare
######
GEE.ndvi
par(mfrow=c(1,2))  
plot(GEE.ndvi.C2)

delta.ndvi<-GEE.ndvi.C2-EE.ndvi.c
#if error - the data is not snapped
plot(delta.ndvi)
delta.ndvi
#if all values are 0, then the two datasets are the same!


# ####
# #export the cropped raster so I can check in QGIS for now
# setwd("/Users/tassal/Documents/Admin/GoogleEarthEngine/CompareGEEtoR/DerivedData")
# getwd()
# writeRaster(ndvi.S,filename="ndvi_fromR", format="GTiff", dataType='FLT4S',overwrite=TRUE) 
# writeRaster(GEE.ndvi.C3,filename="ndvi_rounded_from_GEE", format="GTiff", dataType='FLT4S',overwrite=TRUE) 



######
#STOP - old code below
######



#count obs; ignore NA
ndvi.LeafOff.obs<-sum(!is.na(ndvi.S))
plot(ndvi.LeafOff.obs)
#reclass to show at least 3 obs
ndvi.LeafOff.obs.R<-reclassify(ndvi.LeafOff.obs, c(-Inf, 3, 0, 3, Inf, 1))
plot(ndvi.LeafOff.obs.R)

#add two layers for max and min
ndvi.OFF.mean<-mean(ndvi.S, na.rm = TRUE) #ignore NA value and do the calculation on the values that are present; this fxn works correctly (not sure why I'm getting no values after I join with plots)
plot(ndvi.OFF.mean)
# #calc SD
# ndvi.OFF.SD<-calc(ndvi.S, fun=sd, na.rm=TRUE)
# plot(ndvi.OFF.SD) 

#I don't need to put them in a separate stack since they're aren't many of them. 
#remove original stack
rm(ndvi.S)
#can export here if need be
###############
# END NDVI
###############

########
# 2B - process EVI
########
#set working dir to external hard drive
setwd("/Volumes/MacExt/Imagery/Landsat/NM/OLI_2016_LeafOff/")
getwd()

#create empty raster stack
evi.S<-stack()
# loop through the sub directories (use [-1] to lop off the current directory: ".")
for (dir in list.dirs()[-1]) {
  # get into the sub directory
  setwd(dir)
  #Load ndvi from each directory
  evi.name<- list.files(pattern="*_evi.tif", full.names=F, recursive=FALSE)
  print(evi.name)
  evi<-raster(evi.name)
  #crop 
  evi.c<-evi*mask.c
  
  ###Need to investigate how these values differ for the reclass.
  #get and assign CFmask by name (called BQA in this version of OLI)
  CFmask <- list.files(pattern="*pixel_qa.tif$", full.names=F, recursive=FALSE)
  #assign to raster class
  CFmask1<-raster(CFmask)
  #crop to AOI
  CFmask.c<-CFmask1*mask.c
  
  #reclassify Fmask from Landsat- output is 0 and NoData
  #values 322 and 386 are clear pixels - those are the only two values that will be set to 0; all others = NA
  CFmask.r<-reclassify(CFmask.c, c(-Inf, 321, NA, 321, 322, 0, 323, 385, NA, 385, 386, 0, 387, Inf, NA))
  CFmask.r
  
  #apply mask - add layers together (including original mask. Areas of 0 remain same, areas of NA in mask become   NA in NDVI
  evi.mask<-evi.c+CFmask.r #I removed the orignal mask
  
  #crop to AOI
  evi.c.m<-evi.mask*JME.AOI
  
  #add newly constructed ndvi mask to stack  
  evi.S<-addLayer(evi.S, evi.c.m)
  # 
  # pop back up to the parent directory
  ##setwd("../") - use this for a PC; no slash on a MAC
  setwd("..")
  
}
#end loop

names(evi.S) #get names of raster stack layers

#RENAME each layer of raster stack
fn1<-list.dirs() #note- first vector position is a "."
#delete first record since it's junk;
fn2<-fn1[2:9] #ensure last number is one more than number of sub dirs
#concatenate file names
fn3<-paste("evi", str_sub(fn2, start=13, end=20),  sep="")
#change name of stack, number of names must match columns
names(evi.S)<-c(fn3[1], fn3[2],fn3[3], fn3[4],fn3[5], fn3[6],fn3[7], fn3[8])
#check output of names raster stack layers 
names(evi.S)
plot(evi.S)
nlayers(evi.S) #15

#add two layers for max and min
evi.OFF.mean<-mean(evi.S, na.rm = TRUE) #ignore NA value and do the calculation on the values that are present
plot(evi.OFF.mean)
# #calc SD
# evi.OFF.SD<-calc(evi.S, fun=sd, na.rm=TRUE)
# plot(evi.OFF.SD) 

#remove original stack
rm(evi.S)
#can export here if need be
###############
# END EVI
###############

########
# 2B - process TCap - LeafOff
########

#set working dir to external hard drive
setwd("/Volumes/MacExt/Imagery/Landsat/NM/OLI_2016_LeafOff/")
getwd()

#create empty raster stacks
brightness.OFF.S<-stack()
greenness.OFF.S<-stack()
wetness.OFF.S<-stack()

# get into the parent directory
#setwd(path)
# loop through the sub directories (use [-1] to lop off the current directory: ".")
for (dir in list.dirs()[-1]) {
  # get into the sub directory
  setwd(dir)
  #Load bands from each directory
  band2.name<- list.files(pattern="*_band2.tif", full.names=F, recursive=FALSE)
  print(band2.name)
  band3.name<- list.files(pattern="*_band3.tif", full.names=F, recursive=FALSE)
  band4.name<- list.files(pattern="*_band4.tif", full.names=F, recursive=FALSE)
  band5.name<- list.files(pattern="*_band5.tif", full.names=F, recursive=FALSE)
  band7.name<- list.files(pattern="*_band7.tif", full.names=F, recursive=FALSE)
  print(band7.name)
  #assign names
  band2<-raster(band2.name)
  band3<-raster(band3.name)
  band4<-raster(band4.name)
  band5<-raster(band5.name)
  band7<-raster(band7.name)
  
  #crop 
  band2.c<-band2*mask.c
  band3.c<-band3*mask.c
  band4.c<-band4*mask.c
  band5.c<-band5*mask.c
  band7.c<-band7*mask.c
  
  
  ###Need to investigate how these values differ for the reclass.
  #get and assign CFmask by name (called BQA in this version of OLI)
  CFmask <- list.files(pattern="*pixel_qa.tif$", full.names=F, recursive=FALSE)
  #assign to raster class
  CFmask1<-raster(CFmask)
  #crop to AOI
  CFmask.c<-CFmask1*mask.c
  
  #reclassify Fmask from Landsat- output is 0 and NoData
  #values 322 and 386 are clear pixels - those are the only two values that will be set to 0; all others = NA
  CFmask.r<-reclassify(CFmask.c, c(-Inf, 321, NA, 321, 322, 0, 323, 385, NA, 385, 386, 0, 387, Inf, NA))
  CFmask.r
  plot(CFmask.r)
  
  #apply mask - add layers together (including original mask. Areas of 0 remain same, areas of NA in mask become   NA in NDVI
  band2.mask<-band2.c+CFmask.r #I removed the orignal mask
  band3.mask<-band3.c+CFmask.r
  band4.mask<-band4.c+CFmask.r
  band5.mask<-band5.c+CFmask.r
  band7.mask<-band7.c+CFmask.r
  
  #calculate Tasseled cap indices
  #calc brightness
  bright<-(band2.mask*0.2043)+(band3.mask*0.4158)+(band4.mask*0.5524)+(band5.mask*0.5741)+(band7.mask*0.2303)
  #calc greeness
  green<-(band2.mask*-0.1603)+(band3.mask*0.2819)+(band4.mask*-0.4934)+(band5.mask*0.7940)+(band7.mask*-0.1446)
  #calc wetness
  wet<-(band2.mask*0.0315)+(band3.mask*0.2021)+(band4.mask*0.3102)+(band5.mask*0.1594)+(band7.mask*-0.6109)
  
  #add newly constructed index to respective stack  
  brightness.OFF.S<-addLayer(brightness.OFF.S, bright)
  greenness.OFF.S<-addLayer(greenness.OFF.S, green)
  wetness.OFF.S<-addLayer(wetness.OFF.S, wet)
  # 
  # pop back up to the parent directory
  ##setwd("../") - use this for a PC; no slash on a MAC
  setwd("..")
  
}
#end loop


names(brightness.OFF.S) #get names of raster stack layers

#RENAME each layer of raster stack
fn1<-list.dirs() #note- first vector position is a "."
#delete first record since it's junk;
fn2<-fn1[2:9] #ensure last number is one more than number of sub dirs
#concatenate file names
fn3<-paste("bright", str_sub(fn2, start=13, end=20),  sep="")
#change name of stack, number of names must match columns
names(brightness.OFF.S)<-c(fn3[1], fn3[2],fn3[3], fn3[4],fn3[5], fn3[6],fn3[7], fn3[8])
#check output of names raster stack layers 
names(brightness.OFF.S)
plot(brightness.OFF.S)
nlayers(brightness.OFF.S) #8

#concatenate file names
fn3<-paste("green", str_sub(fn2, start=13, end=20),  sep="")
#change name of stack, number of names must match columns
names(greenness.OFF.S)<-c(fn3[1], fn3[2],fn3[3], fn3[4],fn3[5], fn3[6],fn3[7], fn3[8])

#concatenate file names
fn3<-paste("wet", str_sub(fn2, start=13, end=20),  sep="")
#change name of stack, number of names must match columns
names(wetness.OFF.S)<-c(fn3[1], fn3[2],fn3[3], fn3[4],fn3[5], fn3[6],fn3[7], fn3[8])

#add two layers for max and min
bright.OFF.mean<-mean(brightness.OFF.S, na.rm = TRUE) #ignore NA value and do the calculation on the values that are present; this fxn works correctly (not sure why I'm getting no values after I join with plots)
plot(bright.OFF.mean)

#add two layers for max and min
green.OFF.mean<-mean(greenness.OFF.S, na.rm = TRUE) 
plot(green.OFF.mean)

#add two layers for max and min
wet.OFF.mean<-mean(wetness.OFF.S, na.rm = TRUE) 
plot(wet.OFF.mean)

#remove stacks
rm(brightness.OFF.S)
rm(greenness.OFF.S)
rm(wetness.OFF.S)
###############
# END TCap
###############

# 
# ########
# # 2B - process SAVI
# ########
# 
# #create empty raster stack
# savi.S<-stack()
# # loop through the sub directories (use [-1] to lop off the current directory: ".")
# for (dir in list.dirs()[-1]) {
#   # get into the sub directory
#   setwd(dir)
#   #Load ndvi from each directory
#   savi.name<- list.files(pattern="*_savi.tif", full.names=F, recursive=FALSE)
#   print(savi.name)
#   savi<-raster(savi.name)
#   #crop 
#   savi.c<-savi*mask.c
#   
#   ###Need to investigate how these values differ for the reclass.
#   #get and assign CFmask by name (called BQA in this version of OLI)
#   CFmask <- list.files(pattern="*pixel_qa.tif$", full.names=F, recursive=FALSE)
#   #assign to raster class
#   CFmask1<-raster(CFmask)
#   #crop to AOI
#   CFmask.c<-CFmask1*mask.c
#   
#   #reclassify Fmask from Landsat- output is 0 and NoData
#   #values 322 and 386 are clear pixels - those are the only two values that will be set to 0; all others = NA
#   CFmask.r<-reclassify(CFmask.c, c(-Inf, 321, NA, 321, 322, 0, 323, 385, NA, 385, 386, 0, 387, Inf, NA))
#   CFmask.r
#   
#   #apply mask - add layers together (Areas of 0 remain same, areas of NA in mask become NA in NDVI)
#   savi.c.m<-CFmask.r+savi.c
#   #plot(ndvi.c.m)
# 
#     #add newly constructed ndvi mask to stack  
#   savi.S<-addLayer(savi.S, savi.c.m)
#   # 
#   # pop back up to the parent directory
#   ##setwd("../") - use this for a PC; no slash on a MAC
#   setwd("..")
#   
# }
# #end loop
# 
# names(savi.S) #get names of raster stack layers
# 
# #RENAME each layer of raster stack
# fn1<-list.dirs() #note- first vector position is a "."
# #delete first record since it's junk;
# fn2<-fn1[2:16] #ensure last number is one more than number of sub dirs
# #concatenate file names
# fn3<-paste("savi", str_sub(fn2, start=13, end=20),  sep="")
# #change name of stack, number of names must match columns
# names(savi.S)<-c(fn3[1], fn3[2],fn3[3], fn3[4],fn3[5], fn3[6],fn3[7], fn3[8], fn3[9], fn3[10], fn3[11], fn3[12],fn3[13], fn3[14],fn3[15])
# 
# #check output of names raster stack layers 
# names(savi.S)
# plot(savi.S)
# nlayers(savi.S) #15
# 
# #add two layers for max and min
# savi.max<-max(savi.S, na.rm = TRUE) #ignore NA value and do the calculation on the values that are present
# plot(savi.max)
# savi.min<-min(savi.S, na.rm = TRUE) 
# plot(savi.min)
# #calc SD
# savi.SD<-calc(savi.S, fun=sd, na.rm=TRUE)
# plot(savi.SD) 
# savi.SD
# savi.delta<-savi.max-savi.min
# plot(savi.delta)
# 
# #I really don't need all of the inputs at this point; so create a new stack here
# savi.S.Reduced<-stack()
# savi.S.Reduced<-addLayer(savi.S.Reduced, savi.min, savi.max, savi.delta, savi.SD)
# #update names; 
# names(savi.S.Reduced)<-c("savi_min", "savi_max", "savi_delta", "savi_SD")
# names(savi.S.Reduced)
# plot(savi.S.Reduced)
# #remove original stack
# rm(savi.S)
# #can export here if need be
# ###############
# # END SAVI
# ###############
# 
# ########
# # 2D - process NDMI
# ########
# #create empty raster stack
# ndmi.S<-stack()
# # loop through the sub directories (use [-1] to lop off the current directory: ".")
# for (dir in list.dirs()[-1]) {
#   # get into the sub directory
#   setwd(dir)
#   #Load ndvi from each directory
#   ndmi.name<- list.files(pattern="*_ndmi.tif", full.names=F, recursive=FALSE)
#   print(ndmi.name)
#   ndmi<-raster(ndmi.name)
#   #crop 
#   ndmi.c<-ndmi*mask.c
#   
#   ###Need to investigate how these values differ for the reclass.
#   #get and assign CFmask by name (called BQA in this version of OLI)
#   CFmask <- list.files(pattern="*pixel_qa.tif$", full.names=F, recursive=FALSE)
#   #assign to raster class
#   CFmask1<-raster(CFmask)
#   #crop to AOI
#   CFmask.c<-CFmask1*mask.c
#   
#   #reclassify Fmask from Landsat- output is 0 and NoData
#   #values 322 and 386 are clear pixels - those are the only two values that will be set to 0; all others = NA
#   CFmask.r<-reclassify(CFmask.c, c(-Inf, 321, NA, 321, 322, 0, 323, 385, NA, 385, 386, 0, 387, Inf, NA))
#   CFmask.r
#   
#   #apply mask - add layers together (Areas of 0 remain same, areas of NA in mask become NA in NDVI)
#   ndmi.c.m<-CFmask.r+ndmi.c
#   #plot(ndvi.c.m)
# 
#   #add newly constructed ndvi mask to stack  
#   ndmi.S<-addLayer(ndmi.S, ndmi.c.m)
#   # 
#   # pop back up to the parent directory
#   ##setwd("../") - use this for a PC; no slash on a MAC
#   setwd("..")
#   
# }
# #end loop
# 
# names(ndmi.S) #get names of raster stack layers
# 
# #RENAME each layer of raster stack
# fn1<-list.dirs() #note- first vector position is a "."
# #delete first record since it's junk;
# fn2<-fn1[2:16] #ensure last number is one more than number of sub dirs
# #concatenate file names
# fn3<-paste("ndmi", str_sub(fn2, start=13, end=21),  sep="")
# #change name of stack, number of names must match columns
# names(ndmi.S)<-c(fn3[1], fn3[2],fn3[3], fn3[4],fn3[5], fn3[6],fn3[7], fn3[8], fn3[9], fn3[10], fn3[11], fn3[12],fn3[13], fn3[14],fn3[15])
# 
# #check output of names raster stack layers 
# names(ndmi.S)
# plot(ndmi.S)
# nlayers(ndmi.S) #15
# 
# #add two layers for max and min
# ndmi.max<-max(ndmi.S, na.rm = TRUE) #ignore NA value and do the calculation on the values that are present
# plot(ndmi.max)
# ndmi.min<-min(ndmi.S, na.rm = TRUE) 
# plot(ndmi.min)
# #calc SD
# ndmi.SD<-calc(ndmi.S, fun=sd, na.rm=TRUE)
# plot(ndmi.SD) 
# ndmi.SD
# ndmi.delta<-ndmi.max-ndmi.min
# plot(ndmi.delta)
# 
# #I really don't need all of the inputs at this point; so create a new stack here
# ndmi.S.Reduced<-stack()
# ndmi.S.Reduced<-addLayer(ndmi.S.Reduced, ndmi.min, ndmi.max, ndmi.delta, ndmi.SD)
# #update names; 
# names(ndmi.S.Reduced)<-c("ndmi_min", "ndmi_max", "ndmi_delta", "ndmi_SD")
# names(ndmi.S.Reduced)
# #remove original stack
# rm(ndmi.S)
# #can export here if need be
# ###############
# # END NDMI
# ###############



######
#2. Load 2016 Landsat imagery into a raster stack
######

#set working dir to external hard drive
setwd("/Volumes/MacExt/Imagery/Landsat/NM/OLI_2016_LeafOn/")
getwd()

########
# 2A - process NDVI - LEAFON
########

#create empty raster stack
ndvi.S<-stack()

# get into the parent directory
#setwd(path)
# loop through the sub directories (use [-1] to lop off the current directory: ".")
for (dir in list.dirs()[-1]) {
  # get into the sub directory
  setwd(dir)
  #Load ndvi from each directory
  ndvi.name<- list.files(pattern="*_ndvi.tif", full.names=F, recursive=FALSE)
  print(ndvi.name)
  ndvi<-raster(ndvi.name)
  #plot(ndvi)
  #crop 
  ndvi.c<-ndvi*mask.c
  plot(ndvi.c)
  # Fmask.c
  
  ###Need to investigate how these values differ for the reclass.
  #get and assign CFmask by name (called BQA in this version of OLI)
  CFmask <- list.files(pattern="*pixel_qa.tif$", full.names=F, recursive=FALSE)
  #assign to raster class
  CFmask1<-raster(CFmask)
  #crop to AOI
  CFmask.c<-CFmask1*mask.c
  
  #reclassify Fmask from Landsat- output is 0 and NoData
  #values 322 and 386 are clear pixels - those are the only two values that will be set to 0; all others = NA
  CFmask.r<-reclassify(CFmask.c, c(-Inf, 321, NA, 321, 322, 0, 323, 385, NA, 385, 386, 0, 387, Inf, NA))
  CFmask.r
  plot(CFmask.r)
  
  #apply mask - add layers together (including original mask. Areas of 0 remain same, areas of NA in mask become   NA in NDVI
  ndvi.mask<-ndvi.c+CFmask.r #I removed the orignal mask
  plot(ndvi.mask)
  #plot(ndvi.mask)
  
  #crop to AOI
  ndvi.c.m<-ndvi.mask*JME.AOI
  
  #add newly constructed ndvi mask to stack  
  ndvi.S<-addLayer(ndvi.S, ndvi.c.m)
  # 
  # pop back up to the parent directory
  ##setwd("../") - use this for a PC; no slash on a MAC
  setwd("..")
  
}
#end loop

names(ndvi.S) #get names of raster stack layers

#RENAME each layer of raster stack
#code will gather the file names (in order); #strip out first record since it's usually "."; #before running, change "ndvi" to match index, make sure #of vector columns match the number of layers in stack
#get list of subdirs in main dir
fn1<-list.dirs() #note- first vector position is a "."
#delete first record since it's junk;
fn2<-fn1[2:8] #ensure last number is one more than number of sub dirs
#concatenate file names
fn3<-paste("ndvi", str_sub(fn2, start=13, end=20),  sep="")
#change name of stack, number of names must match columns
names(ndvi.S)<-c(fn3[1], fn3[2],fn3[3], fn3[4],fn3[5], fn3[6],fn3[7])

#check output of names raster stack layers 
names(ndvi.S)
plot(ndvi.S)
nlayers(ndvi.S) #18

#count obs; ignore NA
ndvi.LeafOn.obs<-sum(!is.na(ndvi.S))
plot(ndvi.LeafOn.obs)
#reclass to show at least 3 obs
ndvi.LeafOn.obs.R<-reclassify(ndvi.LeafOn.obs, c(-Inf, 3, 0, 3, Inf, 1))
plot(ndvi.LeafOn.obs.R)

#add two layers for max and min
ndvi.ON.mean<-mean(ndvi.S, na.rm = TRUE) #ignore NA value and do the calculation on the values that are present; this fxn works correctly (not sure why I'm getting no values after I join with plots)
plot(ndvi.ON.mean)
# #calc SD
# ndvi.OFF.SD<-calc(ndvi.S, fun=sd, na.rm=TRUE)
# plot(ndvi.OFF.SD) 

#I don't need to put them in a separate stack since they're aren't many of them. 
#remove original stack
rm(ndvi.S)
#can export here if need be
###############
# END NDVI
###############


########
# 2B - process EVI
########
#set working dir to external hard drive
setwd("/Volumes/MacExt/Imagery/Landsat/NM/OLI_2016_LeafOn/")
getwd()

#create empty raster stack
evi.S<-stack()
# loop through the sub directories (use [-1] to lop off the current directory: ".")
for (dir in list.dirs()[-1]) {
  # get into the sub directory
  setwd(dir)
  #Load ndvi from each directory
  evi.name<- list.files(pattern="*_evi.tif", full.names=F, recursive=FALSE)
  print(evi.name)
  evi<-raster(evi.name)
  #crop 
  evi.c<-evi*mask.c
  
  ###Need to investigate how these values differ for the reclass.
  #get and assign CFmask by name (called BQA in this version of OLI)
  CFmask <- list.files(pattern="*pixel_qa.tif$", full.names=F, recursive=FALSE)
  #assign to raster class
  CFmask1<-raster(CFmask)
  #crop to AOI
  CFmask.c<-CFmask1*mask.c
  
  #reclassify Fmask from Landsat- output is 0 and NoData
  #values 322 and 386 are clear pixels - those are the only two values that will be set to 0; all others = NA
  CFmask.r<-reclassify(CFmask.c, c(-Inf, 321, NA, 321, 322, 0, 323, 385, NA, 385, 386, 0, 387, Inf, NA))
  CFmask.r
  
  #apply mask - add layers together (including original mask. Areas of 0 remain same, areas of NA in mask become   NA in NDVI
  evi.mask<-evi.c+CFmask.r #I removed the orignal mask
  
  #crop to AOI
  evi.c.m<-evi.mask*JME.AOI
  
  #add newly constructed ndvi mask to stack  
  evi.S<-addLayer(evi.S, evi.c.m)
  # 
  # pop back up to the parent directory
  ##setwd("../") - use this for a PC; no slash on a MAC
  setwd("..")
  
}
#end loop

names(evi.S) #get names of raster stack layers

#RENAME each layer of raster stack
fn1<-list.dirs() #note- first vector position is a "."
#delete first record since it's junk;
fn2<-fn1[2:8] #ensure last number is one more than number of sub dirs
#concatenate file names
fn3<-paste("evi", str_sub(fn2, start=13, end=20),  sep="")
#change name of stack, number of names must match columns
names(evi.S)<-c(fn3[1], fn3[2],fn3[3], fn3[4],fn3[5], fn3[6],fn3[7])
#check output of names raster stack layers 
names(evi.S)
plot(evi.S)
nlayers(evi.S) #15

#add two layers for max and min
evi.ON.mean<-mean(evi.S, na.rm = TRUE) #ignore NA value and do the calculation on the values that are present
plot(evi.ON.mean)
# #calc SD
# evi.OFF.SD<-calc(evi.S, fun=sd, na.rm=TRUE)
# plot(evi.OFF.SD) 

#remove original stack
rm(evi.S)
#can export here if need be
###############
# END EVI
###############
# 

########
# 2B - process TCap - LeafOn
########

#set working dir to external hard drive
setwd("/Volumes/MacExt/Imagery/Landsat/NM/OLI_2016_LeafOn/")
getwd()

#create empty raster stacks
brightness.ON.S<-stack()
greenness.ON.S<-stack()
wetness.ON.S<-stack()

# get into the parent directory
#setwd(path)
# loop through the sub directories (use [-1] to lop off the current directory: ".")
for (dir in list.dirs()[-1]) {
  # get into the sub directory
  setwd(dir)
  #Load bands from each directory
  band2.name<- list.files(pattern="*_band2.tif", full.names=F, recursive=FALSE)
  print(band2.name)
  band3.name<- list.files(pattern="*_band3.tif", full.names=F, recursive=FALSE)
  band4.name<- list.files(pattern="*_band4.tif", full.names=F, recursive=FALSE)
  band5.name<- list.files(pattern="*_band5.tif", full.names=F, recursive=FALSE)
  band7.name<- list.files(pattern="*_band7.tif", full.names=F, recursive=FALSE)
  print(band7.name)
  #assign names
  band2<-raster(band2.name)
  band3<-raster(band3.name)
  band4<-raster(band4.name)
  band5<-raster(band5.name)
  band7<-raster(band7.name)
  
  #crop 
  band2.c<-band2*mask.c
  band3.c<-band3*mask.c
  band4.c<-band4*mask.c
  band5.c<-band5*mask.c
  band7.c<-band7*mask.c
  
  
  ###Need to investigate how these values differ for the reclass.
  #get and assign CFmask by name (called BQA in this version of OLI)
  CFmask <- list.files(pattern="*pixel_qa.tif$", full.names=F, recursive=FALSE)
  #assign to raster class
  CFmask1<-raster(CFmask)
  #crop to AOI
  CFmask.c<-CFmask1*mask.c
  
  #reclassify Fmask from Landsat- output is 0 and NoData
  #values 322 and 386 are clear pixels - those are the only two values that will be set to 0; all others = NA
  CFmask.r<-reclassify(CFmask.c, c(-Inf, 321, NA, 321, 322, 0, 323, 385, NA, 385, 386, 0, 387, Inf, NA))
  CFmask.r
  plot(CFmask.r)
  
  #apply mask - add layers together (including original mask. Areas of 0 remain same, areas of NA in mask become   NA in NDVI
  band2.mask<-band2.c+CFmask.r #I removed the orignal mask
  band3.mask<-band3.c+CFmask.r
  band4.mask<-band4.c+CFmask.r
  band5.mask<-band5.c+CFmask.r
  band7.mask<-band7.c+CFmask.r
  
  #calculate Tasseled cap indices
  #calc brightness
  bright<-(band2.mask*0.2043)+(band3.mask*0.4158)+(band4.mask*0.5524)+(band5.mask*0.5741)+(band7.mask*0.2303)
  #calc greeness
  green<-(band2.mask*-0.1603)+(band3.mask*0.2819)+(band4.mask*-0.4934)+(band5.mask*0.7940)+(band7.mask*-0.1446)
  #calc wetness
  wet<-(band2.mask*0.0315)+(band3.mask*0.2021)+(band4.mask*0.3102)+(band5.mask*0.1594)+(band7.mask*-0.6109)
  
  #add newly constructed index to respective stack  
  brightness.ON.S<-addLayer(brightness.ON.S, bright)
  greenness.ON.S<-addLayer(greenness.ON.S, green)
  wetness.ON.S<-addLayer(wetness.ON.S, wet)
  # 
  # pop back up to the parent directory
  ##setwd("../") - use this for a PC; no slash on a MAC
  setwd("..")
  
}
#end loop

names(brightness.ON.S) #get names of raster stack layers

#RENAME each layer of raster stack
fn1<-list.dirs() #note- first vector position is a "."
#delete first record since it's junk;
fn2<-fn1[2:8] #ensure last number is one more than number of sub dirs
#concatenate file names
fn3<-paste("bright", str_sub(fn2, start=13, end=20),  sep="")
#change name of stack, number of names must match columns
names(brightness.ON.S)<-c(fn3[1], fn3[2],fn3[3], fn3[4],fn3[5], fn3[6],fn3[7])
#check output of names raster stack layers 
names(brightness.ON.S)
plot(brightness.ON.S)
nlayers(brightness.ON.S) #7

#concatenate file names
fn3<-paste("green", str_sub(fn2, start=13, end=20),  sep="")
#change name of stack, number of names must match columns
names(greenness.ON.S)<-c(fn3[1], fn3[2],fn3[3], fn3[4],fn3[5], fn3[6],fn3[7])

#concatenate file names
fn3<-paste("wet", str_sub(fn2, start=13, end=20),  sep="")
#change name of stack, number of names must match columns
names(wetness.ON.S)<-c(fn3[1], fn3[2],fn3[3], fn3[4],fn3[5], fn3[6],fn3[7])

#add two layers for max and min
bright.ON.mean<-mean(brightness.ON.S, na.rm = TRUE) #ignore NA value and do the calculation on the values that are present; this fxn works correctly (not sure why I'm getting no values after I join with plots)
plot(bright.ON.mean)

#add two layers for max and min
green.ON.mean<-mean(greenness.ON.S, na.rm = TRUE) 
plot(green.ON.mean)

#add two layers for max and min
wet.ON.mean<-mean(wetness.ON.S, na.rm = TRUE) 
plot(wet.ON.mean)

#remove stacks
rm(bright.ON.S)
rm(green.ON.S)
rm(wet.ON.S)
###############
# END TCap - LeafOFF
###############


####
#Create Delta's
####
ndvi.DELTA<-ndvi.ON.mean-ndvi.OFF.mean
plot(ndvi.DELTA)
evi.DELTA<-evi.ON.mean-evi.OFF.mean
plot(evi.DELTA)
bright.DELTA<-bright.ON.mean-bright.OFF.mean
plot(bright.DELTA)
green.DELTA<-green.ON.mean-green.OFF.mean
plot(green.DELTA)
wet.DELTA<-wet.ON.mean-wet.OFF.mean
plot(wet.DELTA)

#plot both side by side
par(mfrow=c(1,2))
plot(ndvi.DELTA)
plot(evi.DELTA)

######
#3. Load abiotic data
######

#####
#import elevation
#####
path="~/Desktop/NM/FromZ/DerivedData/Variables/"
#path="/Volumes/TWRX/NM_Research/DerivedData/Variables/"
setwd(path)
#this file looks to be the proper one used
elev.R<-raster("elev.tif")
plot(elev.R)
# #check projection
# projection(elev1.R)
# #manually crop raster
# #crop bands a diff way
# elev.R<-crop(elev1.R, extent(-71.99, -71.6423, -38.2577, -38.0191))
# plot(elev.R)
# points(fieldpts.DF2)

###
#derive variables from elev
slope<-terrain(elev.R, opt=c('slope'), unit='radians')
aspect<-terrain(elev.R, opt=c('aspect'), unit='radians')
n.ness<-cos(aspect) #northness - calc from aspect in radians
e.ness<-sin(aspect) #eastness - calc from aspect in radians
aspect.slope<-terrain(elev.R, opt=c('aspect'), unit='degrees') #0 - 360
# #reclass to east, west, etc. - update for South America???
# asp.C<-reclassify(aspect.slope, c(-Inf, 45, 1, 45, 135, 2, 135, 225, 3, 225, 315, 4, 315, Inf, 1)) #classify aspect (North = 1, E=2, S=3, W=4)
hillshade<-hillShade(slope, aspect, angle=45, direction=0)
plot(hillshade)
# ####

###
#calculate heat load index - See McCune and Keon 2002
#1. calculate aspect and slope in radians (see above)
#2. get latitude in radians. There were complications from using the latitude for each cell, so I used the central latitude of the study area since my AOI is small (see notes 1/6/15). Convert DD to Radians. (Radians = DD * (pi/180)). I will use (35.906 latitude) or 0.6266779 radians
35.906 *(pi/180)
#3. convert aspect to folded aspect (folded along the NE-SW). If aspect in radians: Folded Apsect = pi - |Aspect - pi|
f.aspect<-abs(pi-(abs(aspect-(5*(pi/4)))))
f.aspect
#4. Apply equation #3 of McCunes b/c Dillon et al 2011 used it and it performs best for areas 30-60 deg N.
h.load<-0.339+0.808*cos(0.6266779)*cos(slope)-1*cos(f.aspect)*sin(slope)*sin(0.6266779)-0.196*sin(0.6266779)*sin(slope)+0*sin(f.aspect)*sin(slope)
plot(h.load, col = rainbow(25, alpha = 0.35), add = TRUE)
###

########
#Crop to AOI
#######
elev.C<-elev.R*JME.AOI
slope.C<-slope*JME.AOI
n.ness.C<-n.ness*JME.AOI
e.ness.C<-e.ness*JME.AOI
h.load.C<-h.load*JME.AOI
hill.C<-hillshade*JME.AOI
plot(hill.C)

########
#Calc topo indices
#######
library(spatialEco)
#calculate topographic position index
tpi.C<-tpi(elev.C, scale = 3, win = "rectangle", normalize = FALSE)
#tpi.R
#plot(tpi.C)
#calculate terrain ruggedness index
tri.C<-tri(elev.C, s = 3, exact = TRUE)
#tri.R
plot(tri.C)

#create stack for abiotic vars
abiotic.S<-stack()
#add new layers to stack
abiotic.S<-addLayer(abiotic.S, elev.C, slope.C, n.ness.C, e.ness.C, h.load.C, tpi.C, tri.C)
abiotic.S
names(abiotic.S)
#update names; note max is raster #11 and min is raster #12
names(abiotic.S)<-c("elev", "slope", "n.ness", "e.ness", "h.load", "tpi", "tri")
names(abiotic.S)
plot(abiotic.S)

##############
#3b. Calc SD for NDVI and EVI from entire growing season
#############

######
#2. Load 2016 Landsat imagery into a raster stack
######

#set working dir to external hard drive
setwd("/Volumes/MacExt/Imagery/Landsat/NM/OLI_2016/")  #NOTE - this is the entire growing season
getwd()
########
# 2A - process NDVI
########

#create empty raster stack
ndvi.S<-stack()

# get into the parent directory
#setwd(path)
# loop through the sub directories (use [-1] to lop off the current directory: ".")
for (dir in list.dirs()[-1]) {
  # get into the sub directory
  setwd(dir)
  #Load ndvi from each directory
  ndvi.name<- list.files(pattern="*_ndvi.tif", full.names=F, recursive=FALSE)
  print(ndvi.name)
  ndvi<-raster(ndvi.name)
  #plot(ndvi)
  #crop 
  ndvi.c<-ndvi*mask.c
  plot(ndvi.c)
  # Fmask.c
  
  ###Need to investigate how these values differ for the reclass.
  #get and assign CFmask by name (called BQA in this version of OLI)
  CFmask <- list.files(pattern="*pixel_qa.tif$", full.names=F, recursive=FALSE)
  #assign to raster class
  CFmask1<-raster(CFmask)
  #crop to AOI
  CFmask.c<-CFmask1*mask.c
  
  #reclassify Fmask from Landsat- output is 0 and NoData
  #values 322 and 386 are clear pixels - those are the only two values that will be set to 0; all others = NA
  CFmask.r<-reclassify(CFmask.c, c(-Inf, 321, NA, 321, 322, 0, 323, 385, NA, 385, 386, 0, 387, Inf, NA))
  CFmask.r
  plot(CFmask.r)
  
  #apply mask - add layers together (including original mask. Areas of 0 remain same, areas of NA in mask become   NA in NDVI
  ndvi.mask<-ndvi.c+CFmask.r #I removed the orignal mask
  plot(ndvi.mask)
  #plot(ndvi.mask)
  
  #crop to AOI
  ndvi.c.m<-ndvi.mask*JME.AOI
  
  #add newly constructed ndvi mask to stack  
  ndvi.S<-addLayer(ndvi.S, ndvi.c.m)
  # 
  # pop back up to the parent directory
  ##setwd("../") - use this for a PC; no slash on a MAC
  setwd("..")
  
}
#end loop

names(ndvi.S) #get names of raster stack layers

#RENAME each layer of raster stack
#code will gather the file names (in order); #strip out first record since it's usually "."; #before running, change "ndvi" to match index, make sure #of vector columns match the number of layers in stack
#get list of subdirs in main dir
fn1<-list.dirs() #note- first vector position is a "."
#delete first record since it's junk;
fn2<-fn1[2:16] #ensure last number is one more than number of sub dirs
#concatenate file names
fn3<-paste("ndvi", str_sub(fn2, start=13, end=20),  sep="")
#change name of stack, number of names must match columns
names(ndvi.S)<-c(fn3[1], fn3[2],fn3[3], fn3[4],fn3[5], fn3[6],fn3[7], fn3[8], fn3[9], fn3[10], fn3[11], fn3[12],fn3[13], fn3[14],fn3[15])

#check output of names raster stack layers 
names(ndvi.S)
plot(ndvi.S)
nlayers(ndvi.S) #15
# 
# ###troubleshoot###
# library(bfastSpatial)
# obs<-countObs(ndvi.S)
# obs
# ######

# #add two layers for max and min
# ndvi.max<-max(ndvi.S, na.rm = TRUE) #ignore NA value and do the calculation on the values that are present; this fxn works correctly (not sure why I'm getting no values after I join with plots)
# plot(ndvi.max)
# ndvi.min<-min(ndvi.S, na.rm = TRUE) 
# plot(ndvi.min)
#calc SD
ndvi.ALL.SD<-calc(ndvi.S, fun=sd, na.rm=TRUE)
plot(ndvi.ALL.SD) 
# ndvi.SD
# ndvi.delta<-ndvi.max-ndvi.min
# plot(ndvi.delta)

#remove original stack
rm(ndvi.S)
#can export here if need be
###############
# END NDVI
###############

########
# 2B - process EVI
########

#create empty raster stack
evi.S<-stack()
# loop through the sub directories (use [-1] to lop off the current directory: ".")
for (dir in list.dirs()[-1]) {
  # get into the sub directory
  setwd(dir)
  #Load ndvi from each directory
  evi.name<- list.files(pattern="*_evi.tif", full.names=F, recursive=FALSE)
  print(evi.name)
  evi<-raster(evi.name)
  #crop 
  evi.c<-evi*mask.c
  
  ###Need to investigate how these values differ for the reclass.
  #get and assign CFmask by name (called BQA in this version of OLI)
  CFmask <- list.files(pattern="*pixel_qa.tif$", full.names=F, recursive=FALSE)
  #assign to raster class
  CFmask1<-raster(CFmask)
  #crop to AOI
  CFmask.c<-CFmask1*mask.c
  
  #reclassify Fmask from Landsat- output is 0 and NoData
  #values 322 and 386 are clear pixels - those are the only two values that will be set to 0; all others = NA
  CFmask.r<-reclassify(CFmask.c, c(-Inf, 321, NA, 321, 322, 0, 323, 385, NA, 385, 386, 0, 387, Inf, NA))
  CFmask.r
  
  #apply mask - add layers together (including original mask. Areas of 0 remain same, areas of NA in mask become   NA in NDVI
  evi.mask<-evi.c+CFmask.r #I removed the orignal mask
  
  #crop to AOI
  evi.c.m<-evi.mask*JME.AOI
  
  #add newly constructed ndvi mask to stack  
  evi.S<-addLayer(evi.S, evi.c.m)
  # 
  # pop back up to the parent directory
  ##setwd("../") - use this for a PC; no slash on a MAC
  setwd("..")
  
}
#end loop

names(evi.S) #get names of raster stack layers

#RENAME each layer of raster stack
fn1<-list.dirs() #note- first vector position is a "."
#delete first record since it's junk;
fn2<-fn1[2:16] #ensure last number is one more than number of sub dirs
#concatenate file names
fn3<-paste("evi", str_sub(fn2, start=13, end=20),  sep="")
#change name of stack, number of names must match columns
names(evi.S)<-c(fn3[1], fn3[2],fn3[3], fn3[4],fn3[5], fn3[6],fn3[7], fn3[8], fn3[9], fn3[10], fn3[11], fn3[12],fn3[13], fn3[14],fn3[15])
#check output of names raster stack layers 
names(evi.S)
plot(evi.S)
nlayers(evi.S) #15

# #add two layers for max and min
# evi.max<-max(evi.S, na.rm = TRUE) #ignore NA value and do the calculation on the values that are present
# plot(evi.max)
# evi.min<-min(evi.S, na.rm = TRUE) 
# plot(evi.min)
#calc SD
evi.ALL.SD<-calc(evi.S, fun=sd, na.rm=TRUE)
plot(evi.ALL.SD) 
# evi.SD
# evi.delta<-evi.max-evi.min
# plot(evi.delta)

#remove original stack
rm(evi.S)
#can export here if need be
###############
# END EVI
###############

########
# 2X - process Tasseled Cap - get SD
########
#create empty raster stacks
brightness.S<-stack()
greenness.S<-stack()
wetness.S<-stack()

# get into the parent directory
#setwd(path)
# loop through the sub directories (use [-1] to lop off the current directory: ".")
for (dir in list.dirs()[-1]) {
  # get into the sub directory
  setwd(dir)
  #Load bands from each directory
  band2.name<- list.files(pattern="*_band2.tif", full.names=F, recursive=FALSE)
  print(band2.name)
  band3.name<- list.files(pattern="*_band3.tif", full.names=F, recursive=FALSE)
  band4.name<- list.files(pattern="*_band4.tif", full.names=F, recursive=FALSE)
  band5.name<- list.files(pattern="*_band5.tif", full.names=F, recursive=FALSE)
  band7.name<- list.files(pattern="*_band7.tif", full.names=F, recursive=FALSE)
  print(band7.name)
  #assign names
  band2<-raster(band2.name)
  band3<-raster(band3.name)
  band4<-raster(band4.name)
  band5<-raster(band5.name)
  band7<-raster(band7.name)
  
  #crop 
  band2.c<-band2*mask.c
  band3.c<-band3*mask.c
  band4.c<-band4*mask.c
  band5.c<-band5*mask.c
  band7.c<-band7*mask.c
  
  
  ###Need to investigate how these values differ for the reclass.
  #get and assign CFmask by name (called BQA in this version of OLI)
  CFmask <- list.files(pattern="*pixel_qa.tif$", full.names=F, recursive=FALSE)
  #assign to raster class
  CFmask1<-raster(CFmask)
  #crop to AOI
  CFmask.c<-CFmask1*mask.c
  
  #reclassify Fmask from Landsat- output is 0 and NoData
  #values 322 and 386 are clear pixels - those are the only two values that will be set to 0; all others = NA
  CFmask.r<-reclassify(CFmask.c, c(-Inf, 321, NA, 321, 322, 0, 323, 385, NA, 385, 386, 0, 387, Inf, NA))
  CFmask.r
  plot(CFmask.r)
  
  #apply mask - add layers together (including original mask. Areas of 0 remain same, areas of NA in mask become   NA in NDVI
  band2.mask<-band2.c+CFmask.r #I removed the orignal mask
  band3.mask<-band3.c+CFmask.r
  band4.mask<-band4.c+CFmask.r
  band5.mask<-band5.c+CFmask.r
  band7.mask<-band7.c+CFmask.r
  
  #calculate Tasseled cap indices
  #calc brightness
  bright<-(band2.mask*0.2043)+(band3.mask*0.4158)+(band4.mask*0.5524)+(band5.mask*0.5741)+(band7.mask*0.2303)
  #calc greeness
  green<-(band2.mask*-0.1603)+(band3.mask*0.2819)+(band4.mask*-0.4934)+(band5.mask*0.7940)+(band7.mask*-0.1446)
  #calc wetness
  wet<-(band2.mask*0.0315)+(band3.mask*0.2021)+(band4.mask*0.3102)+(band5.mask*0.1594)+(band7.mask*-0.6109)
  
  #add newly constructed index to respective stack  
  brightness.S<-addLayer(brightness.S, bright)
  greenness.S<-addLayer(greenness.S, green)
  wetness.S<-addLayer(wetness.S, wet)
  # 
  # pop back up to the parent directory
  ##setwd("../") - use this for a PC; no slash on a MAC
  setwd("..")
  
}
#end loop
#calc SD
bright.ALL.SD<-calc(brightness.S, fun=sd, na.rm=TRUE)
plot(bright.ALL.SD) 
green.ALL.SD<-calc(greenness.S, fun=sd, na.rm=TRUE)
plot(green.ALL.SD) 
wet.ALL.SD<-calc(wetness.S, fun=sd, na.rm=TRUE)
plot(wet.ALL.SD) 

#remove stacks
rm(brightness.S)
rm(greenness.S)
rm(wetness.S)

###############
# END Tasseled Cap
###############


######
#4. Combine spectral and abioitc stacks
######

master.S<-stack()
master.S<-addLayer(master.S, ndvi.OFF.mean, ndvi.ON.mean, ndvi.DELTA, ndvi.ALL.SD, 
                   evi.OFF.mean, evi.ON.mean, evi.DELTA, evi.ALL.SD, 
                   bright.OFF.mean, bright.ON.mean, bright.DELTA, bright.ALL.SD,
                   green.OFF.mean, green.ON.mean, green.DELTA, green.ALL.SD,
                   wet.OFF.mean, wet.ON.mean, wet.DELTA, wet.ALL.SD,
                   abiotic.S)
names(master.S) #27 total layers
names(master.S)<-c("ndvi.OFF", "nvdi.ON", "ndvi.Delta", "ndvi.ALL.SD",
                   "evi.OFF", "evi.ON", "evi.Delta", "evi.ALL.SD", 
                   "bright.OFF", "bright.ON", "bright.Delta", "bright.ALL.SD", 
                   "green.OFF", "green.ON", "green.Delta", "green.ALL.SD", 
                   "wet.OFF", "wet.ON", "wet.Delta", "wet.ALL.SD", 
                   names(abiotic.S))



#remove all of the layers I just added to the stack....
rm(ndvi.OFF.mean, ndvi.ON.mean, ndvi.DELTA, ndvi.ALL.SD, 
evi.OFF.mean, evi.ON.mean, evi.DELTA, evi.ALL.SD, 
bright.OFF.mean, bright.ON.mean, bright.DELTA, bright.ALL.SD,
green.OFF.mean, green.ON.mean, green.DELTA, green.ALL.SD,
wet.OFF.mean, wet.ON.mean, wet.DELTA, wet.ALL.SD,abiotic.S)

######
#5. Load sampling plot shapefile and table with sampling information
######

###############################
#Load .shp/points
#############################
#set dir
path="~/Desktop/NM/FromZ/DerivedData/Sampling/"
#path="/Volumes/TWRX/NM_Research/DerivedData/Sampling/"
setwd(path)
#load .shp
sample.pts<-readOGR(".", "JME_sample_pts")
plot(sample.pts)

###############################
#Extract to points
#############################
#extract data from grids
ex.rs<-extract(master.S, sample.pts)
head(ex.rs)
# Must convert SpatialPoints object to a dataframe
sample.pts <- as.data.frame(sample.pts)
#bind all newly extracted objects together
sample.pts<-cbind(sample.pts,ex.rs) #row names error, but it should work b/c both files have rows 0 to 491

######
#6. Ready to do EDA or run models
######

#need to load sampling xls and load with 

###############################
#Load xls; merge DFs
#############################
#export table to save all the work already done
path="~/Desktop/NM/FromZ/DerivedData/Sampling/"
#path="/Volumes/TWRX/NM_Research/DerivedData/Sampling/"
setwd(path)
##add sample plots - save excel sheet as csv; name 2016 as T3
field.dat<-read.table("sampling_datasheet_T3.csv", header=TRUE, sep=",")
head(field.dat)
#merge two DFs spatial on IDENT; tabular on IDENT
#######
#I need to do this in a few steps b/c merge only takes two DFs
LCS.DF<-merge(sample.pts, field.dat, by="IDENT")
head(LCS.DF) #it only kept records where there was data


#delete columns that contain NAs
names(LCS.DF) #delete 'Sampled' and Prox2Rd
LCS.DF<-LCS.DF[c(-5,-6)] #it had a bunch of NAs, so we were losing values

#remove all input datat that I don't need anymore
#rm(master.S)
rm(abiotic.S)
#rm(ndvi.S)
rm(ex.rs, aspect, aspect.slope, e.ness, e.ness.C, n.ness, n.ness.C, elev.R, elev.C, f.aspect, h.load, h.load.C, hill.C, hillshade, ndvi, ndvi.c, ndvi.c.m, ndvi.max, ndvi.min, slope.C, slope, tpi.C, tri.C)
rm(band.2, band2.c, band2.mask, band.3, band3.c, band3.mask, band.4, band4.c, band4.mask, band.5, band5.c, band5.mask, band.7, band7.c, band7.mask,
   evi, evi.c, evi.c.m, evi.mask, green, greenness.ON.S, wet, wetness.ON.S, band2, band3, band4, band5, band7, bright, brightness.ON.S)


#see BAND_process_Landsat_for_TJAssal for ESA or JME_model
#look up my quick code fxn for ggplot
#create new fields for models
LCS.DF$pConif<-LCS.DF$Tree/10
LCS.DF$pDecid<-LCS.DF$Shrub/10
LCS.DF$pOther<-LCS.DF$Other/10

#export table to save all the work already done
path="~/Desktop/NM/FromZ/EDA/"
setwd(path)
#export table to xls
write.csv(LCS.DF, file = "BAND_sample_exportfromR_Seasons.csv")



##########################
#############################
# now go to JME_randomForest_model.R
###############################
###############################


#below is old code from model test runs

##########
###load file here so I don't have to run the first 1100 lines of code
##########
path="~/Desktop/NM/FromZ/EDA/"
setwd(path)
#import table
LCS.DF<-read.csv("BAND_sample_exportfromR.csv", header=TRUE, sep = ",")

# #basic boxplots
# #look up my quick code fxn for ggplot
#plot(LCS.DF$PercentT, LCS.DF$ndvi_2016max) #gives a boxplt
# plot(LCS.DF$PercentT, LCS.DF$ndvi_2016min) 
# plot(LCS.DF$PercentT, LCS.DF$ndvi_2016delta)
# plot(LCS.DF$PercentT, LCS.DF$evi_2016max) #gives a boxplt
# plot(LCS.DF$PercentT, LCS.DF$evi_2016min) 
# plot(LCS.DF$PercentT, LCS.DF$evi_2016delta)
# 
# plot(LCS.DF$PercentS, LCS.DF$ndvi_2016max) #gives a boxplt
# plot(LCS.DF$PercentS, LCS.DF$ndvi_2016min) 
# plot(LCS.DF$PercentS, LCS.DF$ndvi_2016delta)
# 
# plot(LCS.DF$PercentO, LCS.DF$ndvi_2016max) #gives a boxplt
# plot(LCS.DF$PercentO, LCS.DF$ndvi_2016min) 
# plot(LCS.DF$PercentO, LCS.DF$ndvi_2016delta)
# 
# hist(LCS.DF$Tree)
# hist(LCS.DF$Shrub)
# hist(LCS.DF$Other)
# max(LCS.DF$Tree)
# min(LCS.DF$Tree)


#do a test run of RF model here....
library(randomForest)
set.seed(101)

dim(LCS.DF) #429 obs

# #fit the random forest with select variables
# JME.rf=randomForest(pTree ~ ndvi_min+ndvi_max+ndvi_SD+band4_min+band4_max+band4_delta+band4_SD+elev+h.load+n.ness+e.ness+tri+tpi , data = LCS.DF, subset = train, importance=TRUE)
# JME.rf #not a great model
# varImpPlot(JME.rf)

#####
#Coniferious Model
####
#use all data 
#predict model for pTree (need to remove pShrub and pOther b/c I don't have that data spatially across AOI)
names(LCS.DF) 
LCS.DF.Tree<-LCS.DF[c(5:21, 30)] #create subset with needed vars
#also get rid of IDENT and coords (for now - since i can't spatiallyl predict those)
names(LCS.DF.Tree) 
LCS.DF.Tree<-LCS.DF.Tree[c(-1, -2)]
sapply(LCS.DF.Tree, typeof)

##scale pTree so that I can assess error
#LCS.DF.Tree$pTree<-LCS.DF.Tree$pTree*100

##separate training and test sets
#training Sample with 300 observations (~70% of the data)
train=sample(1:nrow(LCS.DF.Tree),300)

#make response variable a factor
sapply(LCS.DF.Tree, typeof)
LCS.DF.Tree$pTree<-as.double(LCS.DF.Tree$pTree)

#create a varaible for Tree/No Tree
LCS.DF.Tree$Binary<-ifelse(LCS.DF.Tree$pTree < 0.1, "Non-forest", "Forest")
#convert to factor
LCS.DF.Tree$Binary<-as.factor(LCS.DF.Tree$Binary)
#remove pTree - otherwise it will overinflate
names(LCS.DF.Tree)
#drop ptree 
LCS.DF.Tree<-LCS.DF.Tree[c(-16)]
#fit a classification tree
JME.rf=randomForest(Binary ~ . , data = LCS.DF.Tree, ntree=1000, importance = TRUE, proximity=TRUE)
JME.rf
#overall accuracy is 355/429 = 80%; oob error = 19.2%
varImpPlot(JME.rf)

####drop all EVI variables 
names(LCS.DF.Tree)
#drop ptree 
LCS.DF.Tree2<-LCS.DF.Tree[c(-5:-8)]

#training Sample with 300 observations (~70% of the data)
train=sample(1:nrow(LCS.DF.Tree2),300)
#fit a classification tree
JME.rfb=randomForest(Binary ~ . , data = LCS.DF.Tree2, ntree=1000, importance = TRUE, proximity=TRUE)
JME.rfb
#overall accuracy is 355/429 = 79%; oob error = 20.95
varImpPlot(JME.rfb)



library(rfUtilities)
(cl<-multi.collinear(LCS.DF))

#add pTree back in; but make sure binary isn't in there
names(LCS.DF) 
LCS.DF.Tree<-LCS.DF[c(7:21, 30)] #create subset with needed vars
#also get rid of IDENT and coords (for now - since i can't spatiallyl predict those)
names(LCS.DF.Tree) 
#create a varaible for Tree/No Tree
LCS.DF.Tree$Binary<-ifelse(LCS.DF.Tree$pTree < 0.1, "Non-forest", "Forest")
#convert to factor
LCS.DF.Tree$Binary<-as.factor(LCS.DF.Tree$Binary)
#create a new subset for training data
##separate training and test sets
#training Sample with 300 observations (~70% of the data)
train=sample(1:nrow(LCS.DF.Tree),300)
JME.rf=randomForest(pTree ~ . , data = LCS.DF.Tree, subset = train, importance = TRUE)
JME.rf #76% of varaince explained!!!
#eval fit with rfUtilities
rf.regression.fit(JME.rf) #R2 = 0.752, RMSE = 0.118 or 11.8% - same as Karlson et al!

#fit an NDVI and abiotic variable only
names(LCS.DF.Tree) 
LCS.DF.Tree.NDVI<-LCS.DF.Tree[c(1:4,9:17)] #create subset with needed vars
names(LCS.DF.Tree.NDVI)
train=sample(1:nrow(LCS.DF.Tree.NDVI),300)
JME.rf2=randomForest(pTree ~ . , data = LCS.DF.Tree.NDVI, subset = train, importance = TRUE, ntree=500)
JME.rf2 #73.75% of varaince explained!!!
rf.regression.fit(JME.rf2) #R2 = 0.734; RMSE = 0.122
varImp(JME.rf2)
ggpairs(LCS.DF.Tree.NDVI)
#work backwards with var importance and drop correlated vars... how does model change?
#ndvi on/off and ndvi delta and sd are each highly correlated
#NDVI off and delta have higher variable importance; so I'll keep those
names(LCS.DF.Tree) 
LCS.DF.Tree.NDVI2<-LCS.DF.Tree[c(-2, -4, -5, -6, -7, -8)] #create subset with needed vars
names(LCS.DF.Tree.NDVI2)
train=sample(1:nrow(LCS.DF.Tree.NDVI2),300)
JME.rf2b=randomForest(pTree ~ . , data = LCS.DF.Tree.NDVI2, subset = train, importance = TRUE, ntree=1000)
JME.rf2b #73.75% of varaince explained!!!
rf.regression.fit(JME.rf2b) #R2 = 0.751; RMSE = 0.119 (the model is a little better)
varImp(JME.rf2b)

#write model out longhand
#reclassify column Forest = 1; Non-Forest = 0
LCS.DF.Tree$BinPred<-ifelse(LCS.DF.Tree$Binary=="Forest", 1, 0)
#make sure it's a factor
sapply(LCS.DF.Tree.NDVI2, typeof)
#convert to factor
LCS.DF.Tree.NDVI2$BinPred<-as.factor(LCS.DF.Tree.NDVI2$BinPred)


JME.rf2b.long=randomForest(pTree ~ ndvi.OFF + ndvi.Delta + elev+slope+e.ness+h.load+tri+BinPred , data = LCS.DF.Tree, subset = train, importance = TRUE, ntree=1000)
rf.regression.fit(JME.rf2b.long)
varImp(JME.rf2b.long)

#make it more parsimonious; drop two lowest vars tpi; n.ness
names(LCS.DF.Tree.NDVI2) 
LCS.DF.Tree.NDVI2<-LCS.DF.Tree.NDVI2[c(-5, -8)] #create subset with needed vars
names(LCS.DF.Tree.NDVI2)
train=sample(1:nrow(LCS.DF.Tree.NDVI2),300)
JME.rf2c=randomForest(pTree ~ . , data = LCS.DF.Tree.NDVI2, subset = train, importance = TRUE, ntree=1000)
JME.rf2c #73.75% of varaince explained!!!
rf.regression.fit(JME.rf2c) #R2 = 0.725; RMSE = 0.127 (the model is a little worse than 2b
varImp(JME.rf2c)



#fit an EVI and abiotic variable only
names(LCS.DF.Tree) 
LCS.DF.Tree.EVI<-LCS.DF.Tree[c(5:17)] #create subset with needed vars
names(LCS.DF.Tree.EVI)
train=sample(1:nrow(LCS.DF.Tree.EVI),300)
JME.rf3=randomForest(pTree ~ . , data = LCS.DF.Tree.EVI, subset = train, importance = TRUE)
JME.rf3 #71.56% of varaince explained!!!
rf.regression.fit(JME.rf3) #R2 = 0.704; RMSE = 0.129

#interp RMSE
0.129/mean(LCS.DF.Tree.EVI$pTree) 


#implement RF using caret
library(caret)
model.rf <- train( pTree~ .,
                  data = LCS.DF.Tree.NDVI,
                  method = "rf",
                  ntree = 500) # How many trees to grow in total?
print(model.rf)
#this is great, but I don't know how to decide what variables I can use to run....
#if I can figure that out, I can run in the RF package and get Varimp plots, etc...
varImp(model.rf)


library(GGally)
ggpairs(LCS.DF.Tree)



#predicted values from Classfication tree
predicted<-JME.rf$predicted

#make spatial prediction from classification tree using raster fnx
#determine what vars were actually used
#make stack of just those vars
tree.BIN.pred1<-predict(master.S, JME.rf, progress='text', type='prob')
plot(tree.BIN.pred1)
#now I need to classify it:
# # add predicted values from Classfication tree to dataframe
# LCS.DF.Tree$RFpred<-JME.rf$predicted
# #create auc.roc plot
# library(PresenceAbsence)
# auc.roc.plot(LCS.DF.Tree, threshold = 10, xlab="1-Specificity (false positives)",
#              ylab="Sensitivity (true positives)", main="ROC plot", color=TRUE,
#              find.auc=TRUE, opt.thresholds=TRUE, opt.methods=9) 

#for now, do it manually for now so I can run thru example below
#classify
tree.binmap<-reclassify(tree.BIN.pred1, c(-Inf, 0.6, 0, 0.6, Inf, 1)) 
plot(tree.binmap)


#make regression prediction
#add binary prediction to master.S
master.S2<-addLayer(master.S, tree.binmap)
#rename layer to BinPred so it matches the model variable name
R.names<-names(master.S) #get original names b/c they won't change
#rename - keep the original names, then simply add the new name in...
names(master.S2)<-c(R.names, "BinPred") 
names(master.S2) #works!         

tree.CONT.pred2b<-predict(master.S2, JME.rf2b.long, progress='text')
plot(tree.CONT.pred2b)


###############



#export to test 
#####
#path="/Volumes/TWRX/NM_Research/DerivedData/randomForest/"
path="~/Desktop/NM/FromZ/DerivedData/randomForest/"
setwd(path)
ster(tree.BIN.pred1,filename="conif_mask2", format="GTiff", dataType='FLT4S',overwrite=TRUE) 
ster(tree.CONT.pred2b,filename="conif_percent2", format="GTiff", dataType='FLT4S',overwrite=TRUE) 

###############################
############Create deciduous models
######################

#predict model for pTree (need to remove pShrub and pOther b/c I don't have that data spatially across AOI)
names(LCS.DF) 
LCS.DF.Decid<-LCS.DF[c(5:21, 31)] #create subset with needed vars
#also get rid of IDENT and coords (for now - since i can't spatiallyl predict those)
names(LCS.DF.Decid) 
LCS.DF.Decid<-LCS.DF.Decid[c(-1, -2)]
sapply(LCS.DF.Decid, typeof)

##scale pTree so that I can assess error
#LCS.DF.Tree$pTree<-LCS.DF.Tree$pTree*100

##separate training and test sets
#training Sample with 300 observations (~70% of the data)
train=sample(1:nrow(LCS.DF.Decid),300)

#create a varaible for Tree/No Tree
LCS.DF.Decid$Binary<-ifelse(LCS.DF.Decid$pShrub < 0.1, "Non-forest", "Forest")
#convert to factor
LCS.DF.Decid$Binary<-as.factor(LCS.DF.Decid$Binary)
#remove pTree - otherwise it will overinflate
names(LCS.DF.Decid)
#drop ptree 
LCS.DF.Decid<-LCS.DF.Decid[c(-16)]
#fit a classification tree
JME.rf.Decid=randomForest(Binary ~ . , data = LCS.DF.Decid, ntree=1000, importance = TRUE, proximity=TRUE)
JME.rf.Decid
#overall accuracy is 355/429 = 80%; oob error = 19.2%
varImpPlot(JME.rf.Decid)

#make continuous

#add pshrub back in; but make sure binary isn't in there
names(LCS.DF) 
LCS.DF.Decid<-LCS.DF[c(7:21, 31)] #create subset with needed vars
#also get rid of IDENT and coords (for now - since i can't spatiallyl predict those)
names(LCS.DF.Decid) 
#create a varaible for Tree/No Tree
LCS.DF.Decid$Binary<-ifelse(LCS.DF.Decid$pShrub < 0.1, "Non-forest", "Forest")
#convert to factor
LCS.DF.Decid$Binary<-as.factor(LCS.DF.Decid$Binary)
#create a new subset for training data
##separate training and test sets
#training Sample with 300 observations (~70% of the data)
train=sample(1:nrow(LCS.DF.Decid),300)
JME.rfDecid2=randomForest(pShrub ~ . , data = LCS.DF.Decid, subset = train, importance = TRUE, ntree=1000)
JME.rfDecid2 #76% of varaince explained!!!
#eval fit with rfUtilities
rf.regression.fit(JME.rfDecid2) #R2 = 0.693; RMSE = 0.129
varImp(JME.rfDecid2)

JME.rfDecid2#remove NDVI
names(LCS.DF.Decid)
LCS.DF.Decid2<-LCS.DF.Decid[c(-1:-4)] #create subset with needed vars
names(LCS.DF.Decid2)

##separate training and test sets
#training Sample with 300 observations (~70% of the data)
train=sample(1:nrow(LCS.DF.Decid2),300)
JME.rfDecid2b=randomForest(pShrub ~ . , data = LCS.DF.Decid2, subset = train, importance = TRUE, ntree=1000)
JME.rfDecid2b #76% of varaince explained!!!
#eval fit with rfUtilities
rf.regression.fit(JME.rfDecid2b) #R2 = 0.693; RMSE = 0.129
varImp(JME.rfDecid2b)

ggpairs(LCS.DF.Decid2)


#reclassify column Forest = 1; Non-Forest = 0
LCS.DF.Decid2$BinPred<-ifelse(LCS.DF.Decid2$Binary=="Forest", 1, 0)
#make sure it's a factor
sapply(LCS.DF.Decid2, typeof)
#convert to factor
LCS.DF.Decid2$BinPred<-as.factor(LCS.DF.Decid2$BinPred)


#wrie out longhand
JME.rfDecid2b=randomForest(pShrub ~ evi.OFF+ei.ON+evi.Delta+elev+slope+e.ness+h.load+tri+BinPred , data = LCS.DF.Decid2, subset = train, importance = TRUE, ntree=1000)
JME.rfDecid2b #76% of varaince explained!!!
#eval fit with rfUtilities
rf.regression.fit(JME.rfDecid2b) #R2 = 0.693; RMSE = 0.129
varImp(JME.rfDecid2b)


#wrie out longhand - same model, but swap EVI for NDVI - DOESN"T WORK; but try again later...
JME.rfDecid2c=randomForest(pShrub ~ ndvi.OFF+ndvi.ON+ndvi.Delta+elev+slope+e.ness+h.load+tri+BinPred , data = LCS.DF.Decid, subset = train, importance = TRUE, ntree=1000)
JME.rfDecid2b #76% of varaince explained!!!
#eval fit with rfUtilities
rf.regression.fit(JME.rfDecid2b) #R2 = 0.693; RMSE = 0.129
varImp(JME.rfDecid2b)

###stick with this for now....

#make spatial prediction from classification tree using raster fnx
#determine what vars were actually used
#make stack of just those vars
decid.BIN.pred1<-predict(master.S, JME.rf.Decid, progress='text', type='prob')
plot(decid.BIN.pred1)
#now I need to classify it:
# # add predicted values from Classfication tree to dataframe
# LCS.DF.Tree$RFpred<-JME.rf$predicted
# #create auc.roc plot
# library(PresenceAbsence)
# auc.roc.plot(LCS.DF.Tree, threshold = 10, xlab="1-Specificity (false positives)",
#              ylab="Sensitivity (true positives)", main="ROC plot", color=TRUE,
#              find.auc=TRUE, opt.thresholds=TRUE, opt.methods=9) 

#for now, do it manually for now so I can run thru example below
#classify
decid.binmap<-reclassify(decid.BIN.pred1, c(-Inf, 0.6, 0, 0.6, Inf, 1)) 
plot(decid.binmap)


###contin decid maps


#make regression prediction
#add binary prediction to master.S
master.S2<-addLayer(master.S, decid.binmap)
#rename layer to BinPred so it matches the model variable name
R.names<-names(master.S) #get original names b/c they won't change
#rename - keep the original names, then simply add the new name in...
names(master.S2)<-c(R.names, "BinPred") 
names(master.S2) #works!         

decid.CONT.pred2b<-predict(master.S2, JME.rfDecid2b, progress='text')
plot(decid.CONT.pred2b)

#####
#create other layer
#####

other.CONT<-1-(tree.CONT.pred2b+decid.CONT.pred2b)
plot(other.CONT)

#plot all
par(mfrow=c(1,3))
plot(tree.CONT.pred2b)
plot(decid.CONT.pred2b)
plot(other.CONT)

#export to test 
#####
#path="/Volumes/TWRX/NM_Research/DerivedData/randomForest/"
path="~/Desktop/NM/FromZ/DerivedData/randomForest/"
setwd(path)
ster(decid.BIN.pred1,filename="decid_mask2", format="GTiff", dataType='FLT4S',overwrite=TRUE) 
ster(decid.CONT.pred2b,filename="decid_percent2", format="GTiff", dataType='FLT4S',overwrite=TRUE) 
ster(other.CONT,filename="other_percent2", format="GTiff", dataType='FLT4S',overwrite=TRUE) 







#try all 48 predictors
oob.err=double(48)
test.err=double(48)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:48) 
{
  rf=randomForest(medv ~ . , data = Boston , subset = train,mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,Boston[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(Boston[-train,], mean( (medv - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}
test.err
oob.err
#plot it
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
min(oob.err) #11.40
#see graph; the red line is minimized (11.25) at 7 variables; this is very similar to Fig 4 in Carlson. 

#now I'll run a model with the top 7 variables since in the above model I discovered 7 vars minimizes oob.err
JME.rf2=randomForest(pTree ~ ndmi_min + BinPred + ndvi_min + evi_SD +savi_SD + band7_max + savi_min + evi_min + band2_min + band6_min +band5_max , data = LCS.DF.Tree, subset=train, importance = TRUE, rsq=TRUE)
JME.rf2
varImpPlot(JME.rf2)
#calc longhand
actual<-LCS.DF.Tree$pTree
predicted<-unname(predict(JME.rf2, LCS.DF.Tree))
R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2)) #R2 = 0.795mse
plot(actual, predicted)

#just out of curiousity; check collinearity on these variables...
names(LCS.DF.Tree)
library(GGally)
#variables used in the reduced model
LCS.DF.Tree.sub<-LCS.DF.Tree[c(1, 49, 13, 8, 12, 38, 9, 5, 17, 33, 30)]
ggpairs(LCS.DF.Tree.sub)

######
######
#do a model with NDVI and bands only
#####
####
#just subset the dataframe
names(LCS.DF.Tree)
LCS.DF.Tree.subNDVI<-LCS.DF.Tree[c(1:4, 17:49)]
JME.rf3=randomForest(pTree ~ . , data = LCS.DF.Tree.subNDVI, subset=train, importance = TRUE)
JME.rf3
varImpPlot(JME.rf3)
#calc longhand
actual<-LCS.DF.Tree.subNDVI$pTree
predicted<-unname(predict(JME.rf3, LCS.DF.Tree.subNDVI))
R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2)) #R2 = 0.74
plot(actual, predicted)

######
######
#do a model with EVI and bands only
#####
####
#just subset the dataframe
names(LCS.DF.Tree)
LCS.DF.Tree.subEVI<-LCS.DF.Tree[c(5:8, 17:49)]
JME.rf3=randomForest(pTree ~ . , data = LCS.DF.Tree.subEVI, subset=train, importance = TRUE)
JME.rf3
varImpPlot(JME.rf3)
#calc longhand
actual<-LCS.DF.Tree.subEVI$pTree
predicted<-unname(predict(JME.rf3, LCS.DF.Tree.subEVI))
R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2)) #R2 = 0.7601
plot(actual, predicted)

######
######
#do a model with SAVI and bands only
#####
####
#just subset the dataframe
names(LCS.DF.Tree)
LCS.DF.Tree.subSAVI<-LCS.DF.Tree[c(9:12, 17:49)]
JME.rf3=randomForest(pTree ~ . , data = LCS.DF.Tree.subSAVI, subset=train, importance = TRUE)
JME.rf3
varImpPlot(JME.rf3)
#calc longhand
actual<-LCS.DF.Tree.subSAVI$pTree
predicted<-unname(predict(JME.rf3, LCS.DF.Tree.subSAVI))
R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2)) #R2 = 0.759
plot(actual, predicted)

######
######
#do a model with NDMI and bands only
#####
####
#just subset the dataframe
names(LCS.DF.Tree)
LCS.DF.Tree.subNDMI<-LCS.DF.Tree[c(13:16, 17:49)]
JME.rf3=randomForest(pTree ~ . , data = LCS.DF.Tree.subNDMI, subset=train, importance = TRUE)
JME.rf3
varImpPlot(JME.rf3)
#calc longhand
actual<-LCS.DF.Tree.subNDMI$pTree
predicted<-unname(predict(JME.rf3, LCS.DF.Tree.subNDMI))
R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2)) #R2 = 0.7624
plot(actual, predicted)

#check collinearity with rfUtilities
library(rfUtilities)
(cl<-multi.collinear(LCS.DF.Tree.subNDMI))

test <- data.frame(v1=seq(0.1, 5, length=100), v2=seq(0.1, 5, length=100),
                  v3=dnorm(runif(100)), v4=dnorm(runif(100)))
(cl<-multi.collinear(test))
ggpairs(test)

#eval fit with rfUtilities
rf.regression.fit(JME.rf3)

#check collinearity
###
names(LCS.DF.Tree.subNDMI)
LCS.DF.Tree.subNDMI.sub<-LCS.DF.Tree[c(1:28)]
cor(LCS.DF.Tree.subNDMI.sub, method="pearson")
library(GGally)
#spectral index variables
ggpairs(LCS.DF.Tree.subNDMI.sub)

ggpairs(LCS.DF[,9:24]) 

range(LCS.DF.Tree$ndmi_SD)
range(LCS.DF.Tree$ndmi_min)
range(LCS.DF.Tree$ndmi_max)
range(LCS.DF.Tree$ndmi_delta)

######
# back to original model
######
#calc psuedo R2
JME.rf$rsq
mean(JME.rf$rsq) #average from all the runs?

#calc longhand
actual<-LCS.DF.Tree$pTree
predicted<-unname(predict(JME.rf, LCS.DF.Tree))
R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2)) #R2 = 0.92
plot(actual, predicted)
#how to get RMSE? might need to run caret package....

#JME.rf$mse


#now make prediction spatial
#add binary prediction to master.S
master.S2<-addLayer(master.S, tree.binmap)
#rename layer to BinPred so it matches the model variable name
R.names<-names(master.S) #get original names b/c they won't change
#rename - keep the original names, then simply add the new name in...
names(master.S2)<-c(R.names, "BinPred") 
names(master.S2) #works!                 
                
tree.CONT.pred1<-predict(master.S2, JME.rf, progress='text')
plot(tree.CONT.pred1)

#export to test 
#####
path="/Volumes/TWRX/NM_Research/DerivedData/randomForest/"
setwd(path)
ster(tree.BIN.pred1,filename="conif_mask1", format="GTiff", dataType='FLT4S',overwrite=TRUE) 
ster(tree.CONT.pred1,filename="conif_percent1", format="GTiff", dataType='FLT4S',overwrite=TRUE) 



plot(actual, predicted)


# original values
# not all the original values were predicted
# hence we need to subset based on what was predicted
original  <- LCS.DF.Tree$pTree[sort(as.numeric(names(JME.rf$predicted)))]

# fit a linear model
# Y ~ X, where Y is the dependent variable and X is the independent variable
fit       <- lm(predicted~original)
summary(fit)
plot(original, predicted, pch=19)
abline(fit, col=2)

#assess performance
oob.err=double(13)
test.err=double(13)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:13) 
{
  rf=randomForest(pTree ~ . , data = LCS.DF.Tree, subset = train,mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,LCS.DF.Tree[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(LCS.DF.Tree[-train,], mean( (pTree - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}
test.err
oob.err

#Plotting both Test Error and Out of Bag Error

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

#apply to the validation data
RFestimated = predict(JME.rf, data=train)
#plot estimated vs. obs
plot(train, RFestimated)

#####
#End Coniferious Model
####

#####
#Decid Model
####
#predict model for pTree (need to remove pShrub and pOther b/c I don't have that data spatially across AOI)
names(LCS.DF) 
LCS.DF.Shrub<-LCS.DF[c(1:50, 52)] #create subset with needed vars

##separate training and test sets
#training Sample with 300 observations (~70% of the data)
train=sample(1:nrow(LCS.DF.Shrub),300)


JME.rf=randomForest(pShrub ~ . , data = LCS.DF.Shrub, subset = train, importance = TRUE)
JME.rf #note great; only 40% of variance explained
plot(JME.rf)
varImpPlot(JME.rf)

#####
#End Decid Model
####


#####
#Other Model - I probably won't really run this....
####
#predict model for pTree (need to remove pShrub and pOther b/c I don't have that data spatially across AOI)
names(LCS.DF) 
LCS.DF.Other<-LCS.DF[c(1:50, 53)] #create subset with needed vars

##separate training and test sets
#training Sample with 300 observations (~70% of the data)
train=sample(1:nrow(LCS.DF.Other),300)


JME.rf=randomForest(pOther ~ . , data = LCS.DF.Other, subset = train, importance = TRUE)
JME.rf #note great; only 40% of variance explained
plot(JME.rf)
varImpPlot(JME.rf)

#####
#End Decid Model
####



###########
############
### OLD
#############
#############
######
# check for collinearity
######
names(LCS.DF)

#long hand
test.DF<-LCS.DF[c(7:9)]#band diffs with ndvi diffs
cor(test.DF, method="pearson")

library(GGally)
#spectral index variables
ggpairs(LCS.DF[,9:24]) 
#spectral band variables - full (tough to interpt)
ggpairs(LCS.DF[,26:49]) 
#spectral band variables - min and delta only 
#subset to create new dataframe 
LCS.DF.Sub1<-LCS.DF[c(26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48)] 
#now calc cor matrix
ggpairs(LCS.DF.Sub1)
#spectral band variables - max and delta only 
#subset to create new dataframe
LCS.DF.Sub2<-LCS.DF[c(10, 11, 15, 16, 19,20,23,24,27,28,31,32,35,36,39,40)]
#now calc cor matrix
ggpairs(LCS.DF.Sub2)
#abiotic variables
ggpairs(LCS.DF[,50:56]) 
#spectral band variables - ndvi with band 5
#subset to create new dataframe
LCS.DF.Sub3<-LCS.DF[c(10:13, 38:41)]
ggpairs(LCS.DF.Sub3) 
#spectral band variables - ndvi with band 3
#subset to create new dataframe
LCS.DF.Sub4<-LCS.DF[c(10:13, 30:33)]
ggpairs(LCS.DF.Sub4) 

####test a linear model
#
######
#Run model
#######
#fxn to rescale cell values between 0 and 1
rasterRescale<-function(r){
  ((r-cellStats(r,"min"))/(cellStats(r,"max")-cellStats(r,"min")))
}

#delete Prox2Rd column
names(LCS.DF)
LCS.DF<-LCS.DF[c(-7)] #it had a bunch of NAs, so we were losing values

# ##remove two IDENTS that have NA values IDENT = 362 & 289
LCS.DF.Clean<-LCS.DF[complete.cases(LCS.DF),]
# LCS.DF.Clean<-LCS.DF[which(LCS.DF$IDENT!=362 & LCS.DF$IDENT!=289),]

#tree model
tree1 <- glm(formula=pTree~ndvi_min+ndvi_max+ndvi_SD+elev+h.load+n.ness+e.ness+tri+tpi, data=LCS.DF.Clean)
summary(tree1)
tree1.st<-step(tree1)
summary(tree1.st)
#predict to a raster
#subset the raster stack to only include the rasters I need
names(master.S)
master.S.subset<-stack()
master.S.subset<-addLayer(master.S.subset, master.S[[1]], master.S[[17]], master.S[[21]], master.S[[19]], master.S[[20]])
master.S.subset
#update names; note max is raster #11 and min is raster #12
names(master.S.subset)<-c("ndvi_min", "elev", "h.load", "n.ness", "e.ness")

#now there shouldn't be holes in the dataset
RpredictTree1 <- predict(master.S.subset,tree1.st, progress='text')
plot(RpredictTree1)
#truncate < 0 to 0; > 1 to 1
RpTree.rc<-reclassify(RpredictTree1, c(-Inf,0,0, 1, Inf,1))
plot(RpTree.rc)

#export to test 
#####
path="/Volumes/TWRX/NM_Research/DerivedData/Run2/"
setwd(path)
ster(RpredictTree1,filename="tree2c", format="GTiff", dataType='FLT4S',overwrite=TRUE) 
ster(RpTree.rc,filename="tree2c_RC", format="GTiff", dataType='FLT4S',overwrite=TRUE) 

#tree model - no step
tree2d <- glm(formula=pTree~ndvi_min+ndvi_delta+band5_max+band5_delta+elev+h.load+n.ness+e.ness+tri+tpi, data=LCS.DF.Clean)
summary(tree2d)
T3tree2d.st<-step(tree2d)
summary(T3tree2d.st)
#predict to a raster; subset the raster stack to only include the rasters I need
names(master.S)
master.S.subset<-stack()
master.S.subset<-addLayer(master.S.subset, master.S[[1]], master.S[[3]], master.S[[30]], master.S[[41]], master.S[[45]], master.S[[44]])
master.S.subset
#update names; note max is raster #11 and min is raster #12
names(master.S.subset)<-c("ndvi_min", "ndvi_delta", "band5_max", "elev", "h.load", "e.ness")

#now there shouldn't be holes in the dataset
RpredictTree2d <- predict(master.S.subset,tree2d.st, progress='text')
plot(RpredictTree2d)
#truncate < 0 to 0; > 1 to 1
RpTree.rc<-reclassify(RpredictTree2d, c(-Inf,0,0, 1, Inf,1))
plot(RpTree.rc)
path="/Volumes/TWRX/NM_Research/DerivedData/Run3/"
setwd(path)
ster(RpredictTree2d,filename="tree3d", format="GTiff", dataType='FLT4S',overwrite=TRUE) 
ster(RpTree.rc,filename="tree3d_RC", format="GTiff", dataType='FLT4S',overwrite=TRUE) 



#shrub model
shrub2d <- glm(formula=pShrub~ndvi_min+ndvi_delta+band3_delta+band5_delta+elev+h.load+n.ness+e.ness+tri+tpi, data=LCS.DF.Clean)
summary(shrub2d)
shrub2d.st<-step(shrub2d)
summary(shrub2d.st)
#predict to a raster; subset the raster stack to only include the rasters I need
names(master.S)
master.S.subset<-stack()
master.S.subset<-addLayer(master.S.subset, master.S[[3]], master.S[[23]], master.S[[31]], master.S[[43]], master.S[[44]], master.S[[47]])
master.S.subset
# #update names; note max is raster #11 and min is raster #12
names(master.S.subset)<-c("ndvi_delta", "band3_delta", "band5_delta", "n.ness", "e.ness", "tri")

RpredictShrub2d <- predict(master.S.subset,shrub2d.st, progress='text')
plot(RpredictShrub2d)
#truncate < 0 to 0; > 1 to 1
RpShrub.rc<-reclassify(RpredictShrub2d, c(-Inf,0,0, 1, Inf,1))
plot(RpShrub.rc)
#export
#ster(RpredictShrub1,filename="shrub2", format="GTiff", dataType='FLT4S',overwrite=TRUE) 
ster(RpShrub.rc,filename="shrub3d_RC", format="GTiff", dataType='FLT4S',overwrite=TRUE) 


#create an other layer using band math
#coverother = 100 - (covertree + covershurb) 
#this is how I wrote it up in the ms.
RpOther<-1 - (RpTree.rc + RpShrub.rc)
RpOther
plot(RpOther)
#ster(RpredictShrub1,filename="shrub2", format="GTiff", dataType='FLT4S',overwrite=TRUE) 
ster(RpOther,filename="other3d_RC", format="GTiff", dataType='FLT4S',overwrite=TRUE) 

#I checked this in ArcMap and it checks out just fine with all three layers. 
