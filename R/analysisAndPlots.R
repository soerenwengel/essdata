###################
# CAUSAL DISCOVERY IN A COMPLEX INDUSTRIAL SYSTEM
# Analysis and plots
###################

source("C:/Users/swmo/Desktop/-/forsk/ESS/clear2024/data/functions.R")

sysover <- getSystemoverview()
subs <- getSubsyslist()$Subsystem

pvs <- sysover$Name
load("C:/Users/swmo/Desktop/-/forsk/ESS/clear2024/data/subpvs1") # run data.R to create this file
subpvs <- subpvs1

######
# correlation

corr <- matrix(0, nrow = length(subpvs), ncol = length(subpvs))
div <- 1
p <- 1

for (i in 1:(length(subpvs)-1)){
  ii <- subpvs[i]
  pv <- sysover$Name[ii]
  ss <- sysover$Subsystem[ii]
  
  ddi <- fread(paste("subsample", div, "/", ddfile(ss, pv, p), sep = ""), header = TRUE, 
              colClasses = c("numeric", "numeric"))
  
  for (j in (i+1):length(subpvs)){
    jj <- subpvs[j]
    pv <- sysover$Name[jj]
    ss <- sysover$Subsystem[jj]
    
    ddj <- fread(paste("subsample", div, "/", ddfile(ss, pv, p), sep = ""), header = TRUE, 
                 colClasses = c("numeric", "numeric"))
    corr[i,j] <- cor(ddi$Value, ddj$Value)
    
  }
  
  print(i)
}

corr <- corr + t(corr)
diag(corr) <- 1
rownames(corr) <- colnames(corr) <- pvs[subpvs]

subsys <- rep(NA, length(subpvs))

for (i in 1:length(subsys)){
  
  ii <- which(sysover$Name == pvs[subpvs[i]])
  subsys[i] <- sysover$Subsystem[ii]
  
}

# rename PVs according to subsystem
# note that the pearson correlation may be misleading due to autocorrelation of time series


p <- ggcorrplot(scorr, hc.order = FALSE)
p + scale_x_discrete(labels = ssubsys) + scale_y_discrete(labels = ssubsys) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


######################
# Subsystem Granger-causal analysis (one observation per second, 1 hour of data)

usubsys <- subs
grang <- matrix(NA, nrow = length(usubsys), ncol = length(usubsys))
div <- 1
p <- 1

for (i in 1:length(usubsys)){
  ss <- usubsys[i]
  pvss <- which(subsys == ss)
  
  if (length(pvss) == 0) next
  dd.l <- list()
  for (pv in pvs[subpvs][sysover$Subsystem[subpvs] == ss]){
    dd <- as.data.frame(fread(paste("subsample", div, "/", ddfile(ss, pv, p), sep = ""), header = TRUE, 
                              colClasses = c("numeric", "numeric")))
    dd <- dd[dd$Timestamp < 3600,]
    if (nrow(dd) < 3600 | sum(is.na(dd)) > 0){
      print("skipped")
      next
    }
    dd.l[[length(dd.l) + 1]] <- dd[,2,drop=FALSE]
  }
  ddi <- do.call(cbind, dd.l)
  ddi <- as.data.frame(ddi)
  ddiVar <- apply(ddi, 2, var)
  if (sum(ddiVar > 0) == 0) next
  ddi <- ddi[,ddiVar > 0, drop = FALSE]
  
  for (j in 1:length(usubsys)){
    if (i == j) next
    
    ss <- usubsys[j]
    pvss <- which(subsys == ss)
    if (length(pvss) == 0) next
    dd.l <- list()
    for (pv in pvs[subpvs][sysover$Subsystem[subpvs] == ss]){
      dd <- as.data.frame(fread(paste("subsample", div, "/", ddfile(ss, pv, p), sep = ""), header = TRUE, 
                                colClasses = c("numeric", "numeric")))
      dd <- dd[dd$Timestamp < 3600,]
      if (nrow(dd) < 3600 | sum(is.na(dd)) > 0){
        print("skipped")
        next
      }
      dd.l[[length(dd.l) + 1]] <- dd[,2,drop=FALSE]
    }
    ddj <- do.call(cbind, dd.l)
    ddj <- as.data.frame(ddj)
    ddjVar <- apply(ddj, 2, var)
    if (sum(ddjVar > 0) == 0) next
    ddj <- ddj[,ddjVar > 0, drop = FALSE]
    
    grang[i,j] <- condGranger(cbind(ddi, ddj, ddj), 
                              nx = ncol(ddi), ny = ncol(ddj), order = 50)$prob
    # from i to j
    
  }
  
  print(i)
}

diag(grang) <- NA
rownames(grang) <- colnames(grang) <- usubsys

ggplot(melt(sgrang), aes(Var1, Var2, fill=Value)) +
  geom_tile(height=0.8, width=0.8) +
  scale_fill_binned(breaks = c(1e-15, 1e-8, 1e-3, 1e-2), type = "viridis") +
  theme_minimal() +
  coord_equal() +
  labs(x = NULL, y = NULL, fill="p-value") +
  theme(axis.text.x=element_text(size=13, angle=90, vjust=0.5, hjust=1, 
                                 margin=margin(-3,0,0,0)),
        axis.text.y=element_text(size=13, margin=margin(0,-3,0,0)),
        panel.grid.major=element_blank())


######
# plotting
# make data frame with data from the first hour

# plot with first hour of data
df.l <- list()
div <- 1
p <- 1

for (i in 1:length(subpvs)){
  ii <- subpvs[i]
  pv <- sysover$Name[ii]
  ss <- sysover$Subsystem[ii]
  
  
  ddi <- fread(paste("subsample", div, "/", ddfile(ss, pv, p), sep = ""), header = TRUE, 
                     colClasses = c("numeric", "numeric"))
  df.l[[length(df.l) + 1]] <- ddi[ddi$Timestamp < 3600 & ddi$Timestamp %% 10 == 0,]
  
  
}

df <- do.call(rbind, df.l)

ggplot(df, aes(x = Timestamp, y = Value)) + 
  facet_wrap(~ variable_name, scales = "free_y", ncol = 13) +
  geom_point(alpha = .3, size = .1) + theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.spacing = unit(0, "lines")) + xlab("Timestamp") + ylab("Measured value")


########
# plot with strong subsampling to cover entire period
df.l <- list()

for (i in 1:length(subpvs)){
  ii <- subpvs[i]
  pv <- sysover$Name[ii]
  ss <- sysover$Subsystem[ii]
  
  
  ddi <- fread(paste("subsample", div, "/", ddfile(ss, pv, p), sep = ""), header = TRUE, 
               colClasses = c("numeric", "numeric"))
  df.l[[length(df.l) + 1]] <- ddi[ddi$Timestamp %% 600 == 0,]
  
}

df <- do.call(rbind, df.l)

ggplot(df, aes(x = Timestamp, y = Value)) + 
  facet_wrap(~ variable_name, scales = "free_y", ncol = 13) +
  geom_point(alpha = .3, size = .1) + theme_bw() +
  theme(
    strip.background = element_blank(),
    #strip.text.x = element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.spacing = unit(0, "lines")) + xlab("Timestamp") + ylab("Measured value")
ggsave(file = "C:/Users/swmo/Desktop/-/forsk/ESS/clear2024/data/pvPlotLong.pdf", 
       width = 7, height = 3)


# plot with running max of absolute value
df.l <- list()

for (i in 1:length(subpvs)){
  ii <- subpvs[i]
  pv <- sysover$Name[ii]
  ss <- sysover$Subsystem[ii]
  
  
  ddi <- fread(paste("subsample", div, "/", ddfile(ss, pv, p), sep = ""), header = TRUE, 
               colClasses = c("numeric", "numeric"))

  ddi <- ddi[ddi$Timestamp %% 600 == 0,]
  ddi$Value <- cummax(abs(ddi$Value))
  df.l[[length(df.l) + 1]] <- ddi
  
  
}

df <- do.call(rbind, df.l)

ggplot(df, aes(x = Timestamp, y = Value)) + 
  facet_wrap(~ variable_name, scales = "free_y", ncol = 13) +
  geom_point(alpha = .3, size = .1) + theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.spacing = unit(0, "lines")) + xlab("Timestamp") + ylab("Running maximum of absolute value")


# plot with stronger aggregation to cover entire period
df.l <- list()
div <- 600

for (i in 1:length(subpvs)){
  ii <- subpvs[i]
  pv <- sysover$Name[ii]
  ss <- sysover$Subsystem[ii]
  
  
  ddi <- fread(paste("subsample", div, "/", ddfile(ss, pv, p), sep = ""), header = TRUE, 
               colClasses = c("numeric", "numeric"))

  df.l[[length(df.l) + 1]] <- ddi
  
}

df <- do.call(rbind, df.l)

ggplot(df, aes(x = Timestamp, y = Value)) + 
  facet_wrap(~ variable_name, scales = "free_y", ncol = 13) +
  geom_point(alpha = .3, size = .1) + theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.spacing = unit(0, "lines")) + xlab("Timestamp") + ylab("Measured value")

######
# illustrate irregular sampling

dd.l <- list()
p <- 1

for (i in 1:length(subpvs)){
  ii <- subpvs[i]
  pv <- sysover$Name[ii]
  ss <- sysover$Subsystem[ii]
  
  tmp <- paste("accp_dataset/", ddfile(ss, pv, p), sep = "")
  dd <- fread(cmd = paste('unzip -p accp_dataset.zip', tmp))
  
  dd$Timestamp <- as.POSIXct(dd$Timestamp, tz = "UTC")
  
  t0 <- as.POSIXct("2022-12-30 17:00:00", tz = "UTC")
  t1 <- as.POSIXct("2022-12-30 17:00:10", tz = "UTC")
  dd <- dd[t0 < dd$Timestamp & dd$Timestamp < t1,]
  dd.l[[length(dd.l) + 1]] <- dd
  
  print(i)
}

dd <- do.call(rbind, dd.l)

ggplot(dd, aes(x = Timestamp, y = Value)) + 
  facet_wrap(~ variable_name, scales = "free_y", ncol = 11) +
  geom_point(alpha = 1, size = .1) + 
  geom_vline(aes(xintercept = Timestamp), alpha = .1) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0, "lines")) + xlab("Timestamp") + ylab("Measured value")

