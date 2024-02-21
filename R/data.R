###################
# CAUSAL DISCOVERY IN A COMPLEX INDUSTRIAL SYSTEM
# load and reshape the data
###################

options("digits.secs"=17)

source("functions.R")


# get all PV names

sysover <- getSystemoverview()
subs <- getSubsyslist()$Subsystem

######
# temporal aggregation (mean)

maxtime <- rep(NA, nrow(sysover))
starttime <- c(as.POSIXct("2022-12-30 16:00:00", tz = "UTC"),
              as.POSIXct("2023-01-05 18:00:00", tz = "UTC"),
              as.POSIXct("2023-01-13 15:00:00", tz = "UTC"))
div <- 1 # 1 for mean-aggregating to seconds, 600 for mean-aggregating to one observation per 10 minutes

dir.create(paste("subsample", div, sep = ""))

for (p in 1:3){
  
dir.create(paste("subsample", div, "/Period_", p, sep = "")) 
  
for (i in 1:nrow(sysover)){

pv <- sysover$Name[i]
s <- sysover$Subsystem[i]
dir.create(paste("subsample", div, "/Period_", p, "/", s, sep = "")) 

tmp <- paste("accp_dataset/", ddfile(s, pv, p), sep = "")
dd <- fread(cmd = paste('unzip -p accp_dataset.zip', tmp))

dd <- dd[dd$Timestamp >= starttime[p],]

if (nrow(dd) == 0){
  
  aa <- data.frame(Timestamp = NULL, Value = NULL)
  fwrite(aa, paste("subsample", div, "/", ddfile(s, pv, p), sep = ""), row.names = FALSE)
  next
  
}

dd$Timestamp <- as.numeric(round(dd$Timestamp - starttime[p]))
dd$timestamp <- as.numeric(floor(dd$Timestamp/div))
aa <- aggregate(dd$Value, by = list(Timestamp = dd$Timestamp), mean)

colnames(aa) <- c("Timestamp", "Value")

maxtime[i] <- max(aa$Timestamp)

fwrite(aa, paste("subsample", div, "/", ddfile(s, pv, p), sep = ""), row.names = FALSE)

print(p)
print(i)
}
}


# expand to have NA for missing seconds (for seconds with no obs)
# cut after maxt (seconds)

maxt <- rep(209400,3)/div

countMax.l <- countNA.l <- list()

for (p in 1:3){

countMax <- countNA <- rep(NA, nrow(sysover))  

for (i in 1:nrow(sysover)){
  
  pv <- sysover$Name[i]
  s <- sysover$Subsystem[i]

  dd <- fread(paste("subsample", div, "/", ddfile(s, pv, p), sep = ""), header = TRUE, 
              colClasses = c("numeric", "numeric"))
  
  if (nrow(dd) == 0){
    
    aa <- data.frame(Timestamp = NULL, Value = NULL)
    fwrite(aa, paste("subsample", div, "/", ddfile(s, pv, p), sep = ""), row.names = FALSE)
    next
    
  }  
  
  ee <- data.frame(Timestamp = 0:max(dd$Timestamp), Value = NA)
  ff <- rep(NA, nrow(ee))
  ff[dd$Timestamp + 1] <- dd$Value
  ee$Value <- ff
  
  countMax[i] <- sum(ee$Timestamp <= maxt[p])
  
  ee <- ee[ee$Timestamp <= maxt[p], ]
  fwrite(ee, paste("subsample", div, "/", ddfile(s, pv, p), sep = ""), row.names = FALSE)
  
  countNA[i] <- sum(is.na(ee$Value))
  
  print(i)

}

countMax.l[[p]] <- countMax
countNA.l[[p]] <- countNA
}

subpvs1 <- which(countMax.l[[1]] == max(na.omit(countMax.l[[1]])) & (countNA.l[[1]]) < 1e4)
subpvs2 <- which(countMax.l[[2]] == max(na.omit(countMax.l[[2]])) & (countNA.l[[2]]) < 1e4)
subpvs3 <- which(countMax.l[[3]] == max(na.omit(countMax.l[[3]])) & (countNA.l[[3]]) < 1e4)
save(subpvs1, file = "subpvs1")
save(subpvs2, file = "subpvs2")
save(subpvs3, file = "subpvs3")


# carry forward values 

for (p in 1:3){
  
  subpvs <- list(subpvs1, subpvs2, subpvs3)[[p]]

  for (i in subpvs){
  
    pv <- sysover$Name[i]
    s <- sysover$Subsystem[i]
  
  dd <- fread(paste("subsample", div, "/", ddfile(s, pv, p), sep = ""), header = TRUE, 
              colClasses = c("numeric", "numeric"))
  if (is.na(dd$Value[1])){
    ii <- min(which(!is.na(dd$Value)))
    dd$Value[1:(ii-1)] <- dd$Value[ii]
  }
  
  ee <- na.locf(dd)
  
  
  fwrite(ee, paste("subsample", div, "/", ddfile(s, pv, p), sep = ""), row.names = FALSE)
  
  print(i)
}
}