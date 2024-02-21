###################
# CAUSAL DISCOVERY IN A COMPLEX INDUSTRIAL SYSTEM
# Create small data set
###################

source("functions.R")

# load metadata
sysover <- getSystemoverview()
usubsys <- getSubsyslist()$Subsystem
p <- 1
div <- 1


# loop over subsystems and save first hour of the data to the appropriate folders

main <- "pv1hour1sec"
dir.create(file.path(main))
dd.l <- list()

for (i in 1:length(usubsys)){
  ss <- usubsys[i]
  
  dir.create(file.path(main, ss))
  
  for (pv in sysover$Name[sysover$Subsystem == ss]){
    tmp <- paste("subsample", div, "/", ddfile(ss, pv, p), sep = "")
    if (!file.exists(tmp)) next
    dd <- as.data.frame(fread(tmp, header = TRUE, 
                              colClasses = c("numeric", "numeric")))
    dd <- dd[dd$Timestamp < 3600,]
    if (nrow(dd) > 0){
      dd <- dd[,2,drop = FALSE]
      colnames(dd) <- pv
    }
    dd.l[[length(dd.l) + 1]] <- dd
    
    fwrite(dd, paste(main, ss, paste(pv, ".csv", sep = ""), sep = "/"), row.names = FALSE)
  }
  print(i)
}

dd.l <- dd.l[sapply(dd.l, nrow) == 3600]
dd.l <- as.data.frame(dd.l)

write.csv(dd.l, "preprocessedData.csv")
