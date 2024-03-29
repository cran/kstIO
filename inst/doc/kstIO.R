### R code from vignette source 'kstIO.Rnw'

###################################################
### code chunk number 1: kstIO.Rnw:59-64
###################################################
options(width = 80)
library("kstIO")
library("kst")
wd <- getwd()
setwd(tempdir())


###################################################
### code chunk number 2: write-functions
###################################################
# Obtain data from the pks package
data(DoignonFalmagne7)
ksp <- kspace(kstructure(as.pattern(DoignonFalmagne7$K, as.set=TRUE)))
b <- kbase(ksp)
d <- as.binmat(DoignonFalmagne7$N.R, uniq=FALSE)
r <- as.relation(ksp)
ksp
b
head(d)
# Write data to files
write_kstructure(ksp, "DF7.struct")
write_kspace(ksp, "DF7.space", format="matrix")
write_kbase(b, "DF7.bas", format="KST")
write_kdata(d, "DF7.data", format="SRBT")
write_surmiserelation(r, "DF7.rel")


###################################################
### code chunk number 3: show_example-file
###################################################
txt <- readLines("DF7.bas")
for (i in txt)
  cat(paste(i, "\n", sep=""))


###################################################
### code chunk number 4: read-functions
###################################################
# Read the data files stored before
read_kfamset("DF7.space")
read_kstructure("DF7.struct", format="SRBT")
read_kspace("DF7.space", format="matrix")
read_kbase("DF7.bas", format="auto")
head(read_kdata("DF7.data"))
read_surmiserelation("DF7.rel")


###################################################
### code chunk number 5: kstIO.Rnw:263-264
###################################################
setwd(wd)


