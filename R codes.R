
###################################################################
# Description       : This program produces graphs of Self-productivity,
# cross-productivity, Dynamic Complementarity between Investments and Cognitive Skills
###################################################################

###################################################################
# DATA

# Factors.csv contains the  grid of values of factors (cognitive skills, noncognitive skills 
# health and investment are individual-specifically, randomly drawn from 
# the estimated distributions of these factors that are estimated in Matlab. 

# Deciles1.csv contains the deciles of the factors individual-specifically, randomly drawn from 
# the estimated distributions of these factors without investment factor.

# Deciles2.csv contains the deciles of the factors individual-specifically, randomly drawn from 
# the estimated distributions of these factors with investment factor. 

# Coefs.csv contains the coefficients estimated in Matlab
# Xcescfmean.csv contains means of endowments to be used as inputs in the functions
###################################################################

rm(list=ls())

#set.seed(23031984)

dir<-("~/Desktop/Job application/Github/Sample codes/R/") 

# dir_data           <- paste(dir,c("Data/"), sep="")
# dir_output <-paste(dir, c("Output/"), sep="")
# dir_outputPF      <- paste(dir,c("Output/Final/"), sep="")

setwd(dir)

Factors <- read.csv("Factors.csv", header=F)
Factors         <- data.frame(Factors)

Deciles1 <- read.csv("Deciles1.csv", header=F)
Deciles1         <- data.frame(Deciles1)

Deciles2 <- read.csv("Deciles2.csv", header=F)
Deciles2         <- data.frame(Deciles2)

coefs <- read.csv("coefs.csv", header=F)
coefs         <- data.frame(coefs)

Xcescfmean <- read.csv("Xcescfmean.csv", header=F)
Xcescfmean         <- data.frame(Xcescfmean)

skill              <- c(1,2,3)        # 1 for cognitive skill, 2 for noncognitive
nS                 <- length(skill) # Number of production functions

cogcfcoef <- coefs[[1]]
ncogcfcoef <- coefs[[2]]
healthcfcoef <- coefs[[3]]
#Xcescfmean <- Xcescfmean[[1]]
Xcescfmean <- t(Xcescfmean)

cogcfcoef1 <- cogcfcoef[1]



###################################################################
# SETTINGS FOR THE GRAPHS
###################################################################
treat             <- 0 

# Colors for the graph 
# Create a function interpolating colors in the range of specified colors 
jet.colors <- colorRampPalette(c("yellow", "red"))

# Generate the desired number of colors from this palette
nbcol     <- 100 
color     <- jet.colors(nbcol) 

###################################################################
# SKILLS AT T+1 AS A FUNCTIN OF SKILLS AT T: COGNITIVE SKILLS
###################################################################
###################################################################
# The grid of values of endowments to be used as inputs in the functions 
###################################################################
ngrid             <- 40 

x1seq    <- Factors$V1
x2seq    <- Factors$V2
x3seq    <- Factors$V3

x1step = (max(x1seq)-min(x1seq))/39;

###############################
# Figure 1.1

ycog11 <- function(x){
  out <- ((cogcfcoef[1]*x^cogcfcoef[12] + cogcfcoef[2]*Xcescfmean[2]^cogcfcoef[12]
           + cogcfcoef[3]*Xcescfmean[3]^cogcfcoef[12] + cogcfcoef[4]*Xcescfmean[4]^cogcfcoef[12] + 
           cogcfcoef[5]*Xcescfmean[5]^cogcfcoef[12]+ 
           cogcfcoef[6]*Xcescfmean[6]^cogcfcoef[12])^(1/cogcfcoef[12]))*
  (exp(cogcfcoef[7] + cogcfcoef[8]*Xcescfmean[7] + cogcfcoef[9]*Xcescfmean[8] + 
       + cogcfcoef[10]*Xcescfmean[9] + cogcfcoef[11]*Xcescfmean[10]));
}

pdf("Fig11c.pdf")
par(mar=c(4.5,4.5,1,1))
par(oma = c( 0, 0, 0, 0) )

YMIN <- min(ycog11(x1seq))-0.1
YMAX <- max(ycog11(x1seq))+0.1

plot(x1seq, ycog11(x1seq), type="l",
     xlab="Decile of cognitive skills at age 12", ylab="Cognitive skills at age 15",
     cex.lab=.8, xaxt="n",yaxt="n",  ylim=c(YMIN,YMAX))

axis(1,at=x1seq[seq(1,ngrid,ngrid/10)], labels=seq(1,10), cex.axis =.7)
axis(2,at=seq(0.4,1.8, 0.2), cex.lab = .8, cex.axis=.7)
dev.off()

###################################################################
# SELF-PRODUCTIVITY - COGNITIVE SKILLS
###################################################################
###############################
# Graph 2D - Figure 1.2
mpccog1 <- function(x){
  out <- (1/cogcfcoef[12])*((cogcfcoef[1]*x^cogcfcoef[12] + cogcfcoef[2]*Xcescfmean[2]^cogcfcoef[12]
                               + cogcfcoef[3]*Xcescfmean[3]^cogcfcoef[12] + cogcfcoef[4]*Xcescfmean[4]^cogcfcoef[12] + 
                               cogcfcoef[5]*Xcescfmean[5]^cogcfcoef[12]+ 
                             cogcfcoef[6]*Xcescfmean[6]^cogcfcoef[12])^((1/cogcfcoef[12])-1))*
    cogcfcoef[1]*cogcfcoef[12]*x^(cogcfcoef[12] - 1)*
  (exp(cogcfcoef[7] + cogcfcoef[8]*Xcescfmean[7] + cogcfcoef[9]*Xcescfmean[8] + 
       + cogcfcoef[10]*Xcescfmean[9] + cogcfcoef[11]*Xcescfmean[10]));
}


pdf("Fig12c.pdf")

par(mar=c(4.5,4.5,1,1))
par(oma = c( 0, 0, 0, 0) )

YMIN <- min(mpccog1(x1seq))-0.1
YMAX <- max(mpccog1(x1seq))+0.1

plot(x1seq, mpccog1(x1seq), type="l",
     xlab="Decile of cognitive skills at age 12", ylab="Change in cognitive skills at age 15",
     cex.lab=.8, xaxt="n",yaxt="n",  ylim=c(YMIN,YMAX))

axis(1,at=x1seq[seq(1,ngrid,ngrid/10)], labels=seq(1,10), cex.axis =.7)
axis(2,at=seq(0.4,1.28, 0.1), cex.lab = .8, cex.axis=.7)
dev.off()

###############################
# Graph 3D

# Generate Grid of values of endowments to be used as inputs in the functions 
#########################################################
ngrid             <- 10 

x1seq    <- Deciles1$V1
x2seq    <- Deciles1$V2
x3seq    <- Deciles1$V3

###############################
# Figure 2.1

mpccog21 <- function(x,y){
  return ((1/cogcfcoef[12])*((cogcfcoef[1]*x^cogcfcoef[12] + cogcfcoef[2]*y^cogcfcoef[12]
                               + cogcfcoef[3]*Xcescfmean[3]^cogcfcoef[12] + cogcfcoef[4]*Xcescfmean[4]^cogcfcoef[12] + 
                              cogcfcoef[5]*Xcescfmean[5]^cogcfcoef[12]+
                               cogcfcoef[6]*Xcescfmean[6]^cogcfcoef[12])^((1/cogcfcoef[12])-1))* 
  cogcfcoef[1]*cogcfcoef[12]*x^(cogcfcoef[12] - 1)* 
  (exp(cogcfcoef[7] + cogcfcoef[8]*Xcescfmean[7] + cogcfcoef[9]*Xcescfmean[8] +
       + cogcfcoef[10]*Xcescfmean[9] + cogcfcoef[11]*Xcescfmean[10])));
}

#*(sd(alldata$cog2)/sd(type))
z1 <- outer(x1seq, x2seq, mpccog21)

bwr.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                 "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
pdf("Fig21c.pdf")
par(mar=c(4.5,4.5,1,1))
par(oma = c( 0, 0, 0, 0) )
wireframe(z1,
          xlab=list(label="Cognitive skills at age 12", cex=0.8),
          ylab=list(label="Noncognitive skills at age 12", cex=0.8),
          zlab=list(label="Change in cognitive skills at age 15", cex=0.8, rot = -90),
          screen=list(z=40, y=0, x=-80),
          scales=list(arrows=F, tck = c(0.8, 0.6, 0.4), col="black",distance =c(0.8, 1, 0.8)),
          colorkey=F,
          col.regions = bwr.colors(80),
          ticktype = "detailed",
          par.settings = list(axis.line = list(col = 'transparent')),
          #par.settings = list(box.3d = list(col=NA)),
          grid = TRUE,
          bty = "g",
          drape= T)
dev.off()

###############################
#FIGURE 2.2

mpccog22 <- function(x,y){
  return ((1/cogcfcoef[12])*((cogcfcoef[1]*x^cogcfcoef[12] + cogcfcoef[2]*Xcescfmean[2]^cogcfcoef[12]
                              + cogcfcoef[3]*y^cogcfcoef[12] + cogcfcoef[4]*Xcescfmean[4]^cogcfcoef[12] + 
                                cogcfcoef[5]*Xcescfmean[5]^cogcfcoef[12]+
                                cogcfcoef[6]*Xcescfmean[6]^cogcfcoef[12])^((1/cogcfcoef[12])-1))* 
            cogcfcoef[1]*cogcfcoef[12]*x^(cogcfcoef[12] - 1)* 
            (exp(cogcfcoef[7] + cogcfcoef[8]*Xcescfmean[7] + cogcfcoef[9]*Xcescfmean[8] +
                   + cogcfcoef[10]*Xcescfmean[9] + cogcfcoef[11]*Xcescfmean[10])));
}

#*(sd(alldata$cog2)/sd(type))
z1 <- outer(x1seq, x3seq, mpccog22)

bwr.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                 "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
pdf("Fig22c.pdf")
par(mar=c(4.5,4.5,1,1))
par(oma = c( 0, 0, 0, 0) )
wireframe(z1,
          xlab=list(label="Cognitive skills at age 12", cex=0.8),
          ylab=list(label="Health at age 12", cex=0.8),
          zlab=list(label="Change in cognitive skills at age 15", cex=0.8, rot = -90),
          screen=list(z=40, y=0, x=-80),
          scales=list(arrows=F, tck = c(0.8, 0.6, 0.4), col="black"),
          colorkey=F,
          col.regions = bwr.colors(80),
          ticktype = "detailed",
          par.settings = list(axis.line = list(col = 'transparent')),
          bty = "g",
          drape= T)
dev.off()



#########################################################
# GRAPH 3: CROSS-PRODUCTIVITY - COGNITIVE SKILLS 
#########################################################
# Generate Grid of values of endowments to be used as inputs in the functions 

ngrid             <- 40 

x1seq    <- Factors$V1
x2seq    <- Factors$V2
x3seq    <- Factors$V3
#########################################################
# Figure 3.1: from noncognitive skills to cognitive skills - 2D 
mpccog31 <- function(x){
  out <- ((1/cogcfcoef[12])*((cogcfcoef[1]*Xcescfmean[1]^cogcfcoef[12] + cogcfcoef[2]*x^cogcfcoef[12]
                              + cogcfcoef[3]*Xcescfmean[3]^cogcfcoef[12] + cogcfcoef[4]*Xcescfmean[4]^cogcfcoef[12] + 
                                cogcfcoef[5]*Xcescfmean[5]^cogcfcoef[12]+
                                cogcfcoef[6]*Xcescfmean[6]^cogcfcoef[12])^((1/cogcfcoef[12])-1))* 
            cogcfcoef[2]*cogcfcoef[12]*x^(cogcfcoef[12] - 1)* 
            (exp(cogcfcoef[7] + cogcfcoef[8]*Xcescfmean[7] + cogcfcoef[9]*Xcescfmean[8] +
                   + cogcfcoef[10]*Xcescfmean[9] + cogcfcoef[11]*Xcescfmean[10])));
}

pdf("Fig31c.pdf")
par(mar=c(4.5,4.5,1,1))
par(oma = c( 0, 0, 0, 0) )

YMIN <- min(mpccog31(x2seq))-0.01
YMAX <- max(mpccog31(x2seq))+0.01

plot(x2seq, mpccog31(x2seq), type="l",
     xlab="Decile of noncognitive skills at age 12", ylab="Change in cognitive skills at age 15",
     cex.lab=.8, xaxt="n",yaxt="n",  ylim=c(0,0.35))

axis(1,at=x2seq[seq(1,ngrid,ngrid/10)], labels=seq(1,10), cex.axis =.7)
axis(2,at=seq(0,0.35, 0.05), cex.lab = .8, cex.axis=.7)
dev.off()

#########################################################
# From health to cognitive skills - 2D - Figure 3.2

mpccog32 <- function(x){
  out <- ((1/cogcfcoef[12])*((cogcfcoef[1]*Xcescfmean[1]^cogcfcoef[12] + cogcfcoef[2]*Xcescfmean[2]^cogcfcoef[12]
                              + cogcfcoef[3]*x^cogcfcoef[12] + cogcfcoef[4]*Xcescfmean[4]^cogcfcoef[12] + 
                                cogcfcoef[5]*Xcescfmean[5]^cogcfcoef[12]+
                                cogcfcoef[6]*Xcescfmean[6]^cogcfcoef[12])^((1/cogcfcoef[12])-1))* 
            cogcfcoef[3]*cogcfcoef[12]*x^(cogcfcoef[12] - 1)* 
            (exp(cogcfcoef[7] + cogcfcoef[8]*Xcescfmean[7] + cogcfcoef[9]*Xcescfmean[8] +
                   + cogcfcoef[10]*Xcescfmean[9] + cogcfcoef[11]*Xcescfmean[10])));
}

pdf("Fig32c.pdf")
par(mar=c(4.5,4.5,1,1))
par(oma = c( 0, 0, 0, 0) )

YMIN <- min(mpccog32(x3seq))-0.01
YMAX <- max(mpccog32(x3seq))+0.01

plot(x3seq, mpccog32(x3seq), type="l",
     xlab="Decile of health at age 12", ylab="Change in cognitive skills at age 15",
     cex.lab=.8, xaxt="n",yaxt="n",  ylim=c(0,0.35))

axis(1,at=x3seq[seq(1,ngrid,ngrid/10)], labels=seq(1,10), cex.axis =.7)
axis(2,at=seq(0,0.35, 0.05), cex.lab = .8, cex.axis=.7)
dev.off()


#########################################################
## GRAPH 4 and 5: CROSS-PRODUCTIVITY - COGNITIVE SKILLS - 3D
#########################################################
#########################################################
# Generate Grid of values of endowments to be used as inputs in the functions 
#########################################################
ngrid             <- 10 

x1seq    <- Deciles1$V1
x2seq    <- Deciles1$V2
x3seq    <- Deciles1$V3

#########################################
# COGNITIVE - CROSS-PRODUCTIVITY FROM NON-COGNITIVE and HEALTH TO
# COGNITIVE 

####################
# FROM NON-COGNITIVE TO COGNITIVE - Figure 4.1
#####################

mpccog41 <- function(x,y){
  return ((1/cogcfcoef[12])*((cogcfcoef[1]*y^cogcfcoef[12] + cogcfcoef[2]*x^cogcfcoef[12]
                              + cogcfcoef[3]*Xcescfmean[3]^cogcfcoef[12] + cogcfcoef[4]*Xcescfmean[4]^cogcfcoef[12] + 
                                cogcfcoef[5]*Xcescfmean[5]^cogcfcoef[12]+
                                cogcfcoef[6]*Xcescfmean[6]^cogcfcoef[12])^((1/cogcfcoef[12])-1))* 
            cogcfcoef[2]*cogcfcoef[12]*x^(cogcfcoef[12] - 1)* 
            (exp(cogcfcoef[7] + cogcfcoef[8]*Xcescfmean[7] + cogcfcoef[9]*Xcescfmean[8] +
                   + cogcfcoef[10]*Xcescfmean[9] + cogcfcoef[11]*Xcescfmean[10])));
}

#*(sd(alldata$cog2)/sd(type))
z1 <- outer(x2seq, x1seq, mpccog41)

bwr.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                 "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
pdf("Fig41c.pdf")
par(mar=c(4.5,4.5,1,1))
par(oma = c( 0, 0, 0, 0) )
wireframe(z1,
          xlab=list(label="Noncognitive skills at age 12", cex=0.8),
          ylab=list(label="Cognitive skills at age 12", cex=0.8),
          zlab=list(label="Change in cognitive skills at age 15", cex=0.8, rot = -90),
          screen=list(z=40, y=0, x=-80),
          scales=list(arrows=F, tck = c(0.8, 0.6, 0.4), col="black"),
          colorkey=F,
          col.regions = bwr.colors(80),
          ticktype = "detailed",
          par.settings = list(axis.line = list(col = 'transparent')),
          bty = "g",
          drape= T)
dev.off()


####################
# FIGURE 4.2C FROM NON-COGNITIVE TO COGNITIVE - 
#####################

mpccog42 <- function(x,y){
  return ((1/cogcfcoef[12])*((cogcfcoef[1]*Xcescfmean[1]^cogcfcoef[12] + cogcfcoef[2]*x^cogcfcoef[12]
                              + cogcfcoef[3]*y^cogcfcoef[12] + cogcfcoef[4]*Xcescfmean[4]^cogcfcoef[12] + 
                                cogcfcoef[5]*Xcescfmean[5]^cogcfcoef[12]+
                                cogcfcoef[6]*Xcescfmean[6]^cogcfcoef[12])^((1/cogcfcoef[12])-1))* 
            cogcfcoef[2]*cogcfcoef[12]*x^(cogcfcoef[12] - 1)* 
            (exp(cogcfcoef[7] + cogcfcoef[8]*Xcescfmean[7] + cogcfcoef[9]*Xcescfmean[8] +
                   + cogcfcoef[10]*Xcescfmean[9] + cogcfcoef[11]*Xcescfmean[10])));
}

#*(sd(alldata$cog2)/sd(type))
z1 <- outer(x2seq, x3seq, mpccog42)

bwr.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                 "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
pdf("Fig42c.pdf")
par(mar=c(4.5,4.5,1,1))
par(oma = c( 0, 0, 0, 0) )
wireframe(z1,
          xlab=list(label="Noncognitive skills at age 12", cex=0.8),
          ylab=list(label="Health at age 12", cex=0.8),
          zlab=list(label="Change in cognitive skills at age 15", cex=0.8, rot = -90),
          screen=list(z=40, y=0, x=-80),
          scales=list(arrows=F, tck = c(0.8, 0.6, 0.4), col="black"),
          colorkey=F,
          col.regions = bwr.colors(80),
          ticktype = "detailed",
          par.settings = list(axis.line = list(col = 'transparent')),
          bty = "g",
          drape= T)
dev.off()


####################
#FIGURE 5.1C FROM HEALTH TO COGNITIVE
#####################

mpccog51 <- function(x,y){
  return ((1/cogcfcoef[12])*((cogcfcoef[1]*y^cogcfcoef[12] + cogcfcoef[2]*Xcescfmean[2]^cogcfcoef[12]
                              + cogcfcoef[3]*x^cogcfcoef[12] + cogcfcoef[4]*Xcescfmean[4]^cogcfcoef[12] + 
                                cogcfcoef[5]*Xcescfmean[5]^cogcfcoef[12]+
                                cogcfcoef[6]*Xcescfmean[6]^cogcfcoef[12])^((1/cogcfcoef[12])-1))* 
            cogcfcoef[3]*cogcfcoef[12]*x^(cogcfcoef[12] - 1)* 
            (exp(cogcfcoef[7] + cogcfcoef[8]*Xcescfmean[7] + cogcfcoef[9]*Xcescfmean[8] +
                   + cogcfcoef[10]*Xcescfmean[9] + cogcfcoef[11]*Xcescfmean[10])));
}

#*(sd(alldata$cog2)/sd(type))
z1 <- outer(x3seq, x1seq, mpccog51)

bwr.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                 "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
pdf("Fig51c.pdf")
par(mar=c(4.5,4.5,1,1))
par(oma = c( 0, 0, 0, 0) )
wireframe(z1,
          xlab=list(label="Health at age 12", cex=0.8),
          ylab=list(label="Cognitive skills at age 12", cex=0.8),
          zlab=list(label="Change in cognitive skills at age 15", cex=0.8, rot = -90),
          screen=list(z=40, y=0, x=-80),
          scales=list(arrows=F, tck = c(0.8, 0.6, 0.4), col="black"),
          colorkey=F,
          col.regions = bwr.colors(80),
          ticktype = "detailed",
          par.settings = list(axis.line = list(col = 'transparent')),
          bty = "g",
          drape= T)
dev.off()


####################
#FIGURE 5.2C FROM HEALTH TO COGNITIVE
#####################

mpccog52 <- function(x,y){
  return ((1/cogcfcoef[12])*((cogcfcoef[1]*Xcescfmean[1]^cogcfcoef[12] + cogcfcoef[2]*y^cogcfcoef[12]
                              + cogcfcoef[3]*x^cogcfcoef[12] + cogcfcoef[4]*Xcescfmean[4]^cogcfcoef[12] + 
                                cogcfcoef[5]*Xcescfmean[5]^cogcfcoef[12]+
                                cogcfcoef[6]*Xcescfmean[6]^cogcfcoef[12])^((1/cogcfcoef[12])-1))* 
            cogcfcoef[3]*cogcfcoef[12]*x^(cogcfcoef[12] - 1)* 
            (exp(cogcfcoef[7] + cogcfcoef[8]*Xcescfmean[7] + cogcfcoef[9]*Xcescfmean[8] +
                   + cogcfcoef[10]*Xcescfmean[9] + cogcfcoef[11]*Xcescfmean[10])));
}

#*(sd(alldata$cog2)/sd(type))
z1 <- outer(x3seq, x2seq, mpccog52)

bwr.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                 "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
pdf("Fig52c.pdf")
par(mar=c(4.5,4.5,1,1))
par(oma = c( 0, 0, 0, 0) )
wireframe(z1,
          xlab=list(label="Health at age 12", cex=0.8),
          ylab=list(label="Noncognitive skills at age 12", cex=0.8),
          zlab=list(label="Change in cognitive skills at age 15", cex=0.8, rot = -90),
          screen=list(z=40, y=0, x=-80),
          scales=list(arrows=F, tck = c(0.8, 0.6, 0.4), col="black"),
          colorkey=F,
          col.regions = bwr.colors(80),
          ticktype = "detailed",
          par.settings = list(axis.line = list(col = 'transparent')),
          bty = "g",
          drape= T)
dev.off()




################################################################################
### MARGINAL PRODUCT OF PARENTAL INVESTMENTS - COGNITIVE SKILLS - 2D
#################################################################################

##################################
# Generate Grid of values of endowments to be used as inputs in the functions 
##################################
ngrid             <- 40 

x1seq    <- Factors$V1
x2seq    <- Factors$V2
x3seq    <- Factors$V3

##################################
# COGNITIVE SKILLS - FIGURE 6.1C 
##################################

mpccog6 <- function(x){
  out <- ((1/cogcfcoef[12])*((cogcfcoef[1]*x^cogcfcoef[12] + cogcfcoef[2]*Xcescfmean[2]^cogcfcoef[12]
                              + cogcfcoef[3]*Xcescfmean[3]^cogcfcoef[12] + cogcfcoef[4]*Xcescfmean[4]^cogcfcoef[12] + 
                                cogcfcoef[5]*Xcescfmean[5]^cogcfcoef[12]+
                                cogcfcoef[6]*Xcescfmean[6]^cogcfcoef[12])^((1/cogcfcoef[12])-1))* 
            cogcfcoef[4]*cogcfcoef[12]*Xcescfmean[4]^(cogcfcoef[12] - 1)* 
            (exp(cogcfcoef[7] + cogcfcoef[8]*Xcescfmean[7] + cogcfcoef[9]*Xcescfmean[8] +
                   + cogcfcoef[10]*Xcescfmean[9] + cogcfcoef[11]*Xcescfmean[10])));
}

pdf("Fig6c.pdf")
par(mar=c(4.5,4.5,1,1))
par(oma = c( 0, 0, 0, 0) )

YMIN <- min(mpccog6(x1seq))-0.01
YMAX <- max(mpccog6(x1seq))+0.01

plot(x1seq, mpccog6(x1seq), type="l",
     xlab="Decile of cognitive skills at age 12", 
     ylab="Marginal product of parental investments on cognitive skills at age 15",
     cex.lab=.8, xaxt="n",yaxt="n",  ylim=c(YMIN,YMAX))

axis(1,at=x1seq[seq(1,ngrid,ngrid/10)], labels=seq(1,10), cex.axis =.7)
axis(2,at=seq(0.1,0.5, 0.05), cex.lab = .8, cex.axis=.7)
dev.off()



################################################################################
### MARGINAL PRODUCT OF PARENTAL INVESTMENTS FIGURES 7 - 3D
#################################################################################
##################################
# Generate Grid of values of endowments to be used as inputs in the functions 
##################################
ngrid             <- 10 

x1seq    <- Deciles2$V1
x2seq    <- Deciles2$V2
x3seq    <- Deciles2$V3

##################################
# COGNITIVE SKILLS - FIGURE 7.1C, 7.2C and 7.3C
##################################

### FIGURE 7.1C

mpccog71 <- function(x,y){
  return ((1/cogcfcoef[12])*((cogcfcoef[1]*x^cogcfcoef[12] + cogcfcoef[2]*y^cogcfcoef[12]
                              + cogcfcoef[3]*Xcescfmean[3]^cogcfcoef[12] + cogcfcoef[4]*Xcescfmean[4]^cogcfcoef[12] + 
                                cogcfcoef[5]*Xcescfmean[5]^cogcfcoef[12]+
                                cogcfcoef[6]*Xcescfmean[6]^cogcfcoef[12])^((1/cogcfcoef[12])-1))* 
            cogcfcoef[4]*cogcfcoef[12]*Xcescfmean[4]^(cogcfcoef[12] - 1)* 
            (exp(cogcfcoef[7] + cogcfcoef[8]*Xcescfmean[7] + cogcfcoef[9]*Xcescfmean[8] +
                   + cogcfcoef[10]*Xcescfmean[9] + cogcfcoef[11]*Xcescfmean[10])));
}

#*(sd(alldata$cog2)/sd(type))
z1 <- outer(x1seq, x2seq, mpccog71)

bwr.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                 "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
pdf("Fig71c.pdf")
par(mar=c(4.5,4.5,1,1))
par(oma = c( 0, 0, 0, 0) )
wireframe(z1,
          xlab=list(label="Cognitive skills at age 12", cex=0.8),
          ylab=list(label="Noncognitive skills at age 12", cex=0.8),
          zlab=list(label="Marginal product of parental investments on cognitive skills at age 15", cex=0.8, rot = -90),
          screen=list(z=40, y=0, x=-80),
          scales=list(arrows=F, tck = c(0.8, 0.6, 0.4), col="black"),
          colorkey=F,
          col.regions = bwr.colors(80),
          ticktype = "detailed",
          par.settings = list(axis.line = list(col = 'transparent')),
          bty = "g",
          drape= T)
dev.off()


### FIGURE 7.2C

mpccog72 <- function(x,y){
  return ((1/cogcfcoef[12])*((cogcfcoef[1]*x^cogcfcoef[12] + cogcfcoef[2]*Xcescfmean[2]^cogcfcoef[12]
                              + cogcfcoef[3]*y^cogcfcoef[12] + cogcfcoef[4]*Xcescfmean[4]^cogcfcoef[12] + 
                                cogcfcoef[5]*Xcescfmean[5]^cogcfcoef[12]+
                                cogcfcoef[6]*Xcescfmean[6]^cogcfcoef[12])^((1/cogcfcoef[12])-1))* 
            cogcfcoef[4]*cogcfcoef[12]*Xcescfmean[4]^(cogcfcoef[12] - 1)* 
            (exp(cogcfcoef[7] + cogcfcoef[8]*Xcescfmean[7] + cogcfcoef[9]*Xcescfmean[8] +
                   + cogcfcoef[10]*Xcescfmean[9] + cogcfcoef[11]*Xcescfmean[10])));
}

#*(sd(alldata$cog2)/sd(type))
z1 <- outer(x1seq, x3seq, mpccog72)

bwr.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                 "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
pdf("Fig72c.pdf")
par(mar=c(4.5,4.5,1,1))
par(oma = c( 0, 0, 0, 0) )
wireframe(z1,
          xlab=list(label="Cognitive skills at age 12", cex=0.8),
          ylab=list(label="Health at age 12", cex=0.8),
          zlab=list(label="Marginal product of parental investments on cognitive skills at age 15", cex=0.8, rot = -90),
          screen=list(z=40, y=0, x=-80),
          scales=list(arrows=F, tck = c(0.8, 0.6, 0.4), col="black"),
          colorkey=F,
          col.regions = bwr.colors(80),
          ticktype = "detailed",
          par.settings = list(axis.line = list(col = 'transparent')),
          bty = "g",
          drape= T)
dev.off()


### FIGURE 7.3C

mpccog73 <- function(x,y){
  return ((1/cogcfcoef[12])*((cogcfcoef[1]*Xcescfmean[1]^cogcfcoef[12] + cogcfcoef[2]*x^cogcfcoef[12]
                              + cogcfcoef[3]*y^cogcfcoef[12] + cogcfcoef[4]*Xcescfmean[4]^cogcfcoef[12] + 
                                cogcfcoef[5]*Xcescfmean[5]^cogcfcoef[12]+
                                cogcfcoef[6]*Xcescfmean[6]^cogcfcoef[12])^((1/cogcfcoef[12])-1))* 
            cogcfcoef[4]*cogcfcoef[12]*Xcescfmean[4]^(cogcfcoef[12] - 1)* 
            (exp(cogcfcoef[7] + cogcfcoef[8]*Xcescfmean[7] + cogcfcoef[9]*Xcescfmean[8] +
                   + cogcfcoef[10]*Xcescfmean[9] + cogcfcoef[11]*Xcescfmean[10])));
}

#*(sd(alldata$cog2)/sd(type))
z1 <- outer(x2seq, x3seq, mpccog73)

bwr.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                 "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
pdf("Fig73c.pdf")
par(mar=c(4.5,4.5,1,1))
par(oma = c( 0, 0, 0, 0) )
wireframe(z1,
          xlab=list(label="Noncognitive skills at age 12", cex=0.8),
          ylab=list(label="Health at age 12", cex=0.8),
          zlab=list(label="Marginal product of parental investments on cognitive skills at age 15", cex=0.8, rot = -90),
          screen=list(z=40, y=0, x=-80),
          scales=list(arrows=F, tck = c(0.8, 0.6, 0.4), col="black"),
          colorkey=F,
          col.regions = bwr.colors(80),
          ticktype = "detailed",
          par.settings = list(axis.line = list(col = 'transparent')),
          bty = "g",
          drape= T)
dev.off()
