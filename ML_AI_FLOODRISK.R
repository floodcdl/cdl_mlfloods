library(terra)
library(tidyverse)
library(rpart)
library(rpart.plot)
#open the points
#change
clmtphspop <- rast("E:/cdl/ML_AI/clmtPhsPop.tif")
landuse <- rast("E:/cdl/ML_AI/LandCover.tif")
procVal <- rast("E:/cdl/ML_AI/procVal2.tif")
procInf <- rast("E:/cdl/ML_AI/procInf2.tif")
procRelief <- rast("E:/cdl/ML_AI/procRelief2.tif")


darPoints <- vect("E:/cdl/ML_AI/darrange_points.shp")
darDistrict <- vect("E:/cdl/ML_AI/darrang_district.shp")
darSubDistrict <- vect("E:/cdl/ML_AI/darrang_subdistrict.shp")


darFldHaz <- rast("E:/cdl/ML_AI/flooded.tif")
FldHaz <- darFldHaz
names(FldHaz) <- "flood"
names(darFldHaz) <- "risk"
crs(darFldHaz, proj=T, describe=FALSE, parse=FALSE)
darFldHaz[darFldHaz<1] <- 0.1
darFldHaz[darFldHaz>=1] <- 3
plot(darFldHaz)

darFldVul <- rast("E:/cdl/ML_AI/landuseComp.tif")
darFldVul <- rast(vals=values(darFldVul),ext=ext(darFldHaz),crs=crs(darFldHaz, proj=T, describe=FALSE, parse=FALSE),
              nrow=dim(darFldHaz)[1],ncol=dim(darFldHaz)[2])
plot(darFldVul)

procR <- c(procInf, procRelief)

tt <- cut(darFldVul[],labels = c(1,2,3,4,5), include.lowest =T,breaks = 5, na.rm = T)
clone <- darFldVul
values(clone) <- tt
darFldVul <- clone+1
plot(darFldVul)

darFldRisk <- darFldHaz * darFldVul * procVal
plot(darFldRisk)
plot(procVal)
head(darFldHaz)
#create a flood risk map
tt <- cut(darFldRisk[],labels = c(1,2,3,4,5), include.lowest =T,breaks = 5, na.rm = T)
clone <- darFldRisk
values(clone) <- tt
clone <- clone+1
clone <- crop(clone , darDistrict, overwrite = T)
clone <- mask(clone , darDistrict, overwrite = T)
plot(clone)
clone[clone == 2] <- 1
clone[clone == 5] <- 4 
#extract the training points (left side)
tp <- terra::extract(clone, darPoints) %>%
  round()
unique(tp$risk)
fldhz <- terra::extract(FldHaz, darPoints) %>%
  round()
head(fldhz)
procI <- terra::extract(procInf, darPoints)
procR <- terra::extract(procRelief, darPoints) 
clmt <- read.csv('E:/cdl/ML_AI/clmtPhsPop.csv')
lu <- read.csv('E:/cdl/ML_AI/landuse.csv')

image <- c(FldHaz,clmtphspop,landuse, procInf, procRelief)
#image <- c(clmtphspop,landuse)
head(image)
unique(image$aet[])
df <- data.frame(tp,fldhz,clmt,lu, procI, procR)
#df <- data.frame(tp,clmt,lu)
head(df)

df <- subset(df, select = -ID)
df <- subset(df, select = -ID.1)
df <- subset(df, select = -ID.2)
df <- subset(df, select = -ID.3)
model.class <- rpart(as.factor(risk)~., data = df, method = 'class')
#plot the decision tree
rpart.plot(model.class, box.palette = 0, main = "Classification Tree")
pr_f <- predict(image, model.class, type ='class', progress = 'text') %>% 
  terra::as.factor()
plot(pr_f)

#build a model, here with glm
model <- glm(formula=risk~., data=df)
#predict to a raster
r1 <- predict(image, model)
plot(r1)
plot(darFldRisk)


#are the results resonable
test <- terra::extract(pr_f, darPoints) %>% 
  as.data.frame() #%>% 
  #rename(id = ".")
test[test == 2] <- 4
head(tp)
head(test)
testProbs <- data.frame(
  obs = as.factor(round(tp$risk)),
  pred = as.factor(round(test$lyr1))
) %>% 
  mutate(correct = ifelse(obs == pred, 1, 0))

confMatrix <- confusionMatrix(testProbs$obs, testProbs$pred)
confMatrix


subDistRiskModel <- terra::extract(pr_f, darSubDistrict, fun = mean, na.rm= T, df = T)
darSubDistrict$RiskModel <- subDistRiskModel$lyr1

subDistRiskModel <- terra::extract(darFldRisk, darSubDistrict, fun = mean, na.rm= T, df = T)
darSubDistrict$RiskReal <- subDistRiskModel$risk

outfile <- "E:/cdl/ML_AI/darSubDistRisk2.shp"
writeVector(darSubDistrict, outfile, overwrite=TRUE)


