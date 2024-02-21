###################
# CAUSAL DISCOVERY IN A COMPLEX INDUSTRIAL SYSTEM
# Create adjacency matrix of the causal graph
###################


source("functions.R")

# load metadata
sysover <- getSystemoverview()
usubsys <- getSubsyslist()$Subsystem


###################
# adjacency matrix of causal graph
# rows are 'to' and columns are from, that is, 
# the entry in the i'th row and the j'th column describes the edge j -> i
# 0 = no edge, 1 = weak edge, 2 = strong edge
# by convention, every diagonal element is 2

# we remove those systems not represented in the causal graph:
# D is a subsystem of T6
# HP/LP/MP/SP line warm end are measured between subsystems, and not proper subsystems themselves
# TG is thought to be irrelevant in stable operations

usubsys <- usubsys[!(usubsys %in% c("D",
                                    "HPWE",
                                    "LPWE",
                                    "MPWE",
                                    "SPWE",
                                    "TG",
                                    "HX1",
                                    "HX2",
                                    "HX3",
                                    "HX4",
                                    "HX5",
                                    "HX6"
                                    ))]

A <- matrix(0, nrow = length(usubsys), ncol = length(usubsys),
            dimnames = list(usubsys, usubsys))
diag(A) <- 2

#T1
A["T1", "OG"] <- 2
A["T1", c("T2", "AH")] <- 1

#T2
A["T2", "T1"] <- 2
A["T2", "T3"] <- 1

#T3
A["T3", "T2"] <- 2
A["T3", c("A1", "A2", "A3", "TS", "AH", "T4", "T5")] <- 1

#T4
A["T4", c("T3", "A3")] <- 2
A["T4", c("AH", "T5")] <- 1

#T5
A["T5", "T4"] <- 2

#T6
A["T6", "T5"] <- 2
A["T6", c("C1")] <- 1

#A1
A["A1", "T2"] <- 1

#A2
A["A2", "T2"] <- 1

#A3
A["A3", "T3"] <- 2

#TS
A["TS", "T3"] <- 1

#AH
A["AH", "T6"] <- 2

#S
A["S", "T6"] <- 2

#HS
A["HS", "S"] <- 2

#TV
A["TV", "T6"] <- 1

#C1
A["C1", "CG"] <- 2
A["C1", c("HS", "T6")] <- 1

#C2
A["C2", c("C1", "CG", "TV")] <- 2

#C3
A["C3", c("C2", "CG", "SP")] <- 2

#CG
A["CG", c("C1", "C2", "C3")] <- 2

#SP
A["SP", "C3"] <- 2
A["SP", c("HS", "OG", "LS", "TV")] <- 1

#LS
A["LS", c("SP", "LP")] <- 1

#LP
A["LP", c("LS", "OG", "AH", "T5")] <- 1

#HP
A["HP", c("SP", "LP")] <- 2
A["HP", "OG"] <- 1

#OG
A["OG", "HP"] <- 2
A["OG", c("T1", "T2", "T3", "T4")] <- 1


