library(cols4all)
library(gifski)
windowsFonts(C = windowsFont("Comic Sans MS"))

dados <- read.csv("Temperatura.csv",sep=",",header=T,skip=1)
Temp.mensal <- dados[,2:13]
Temp <- as.matrix(Temp.mensal)
Temp <- matrix(as.double(Temp),ncol=12)
radius <- 0.5 + 0.35*Temp

#defines the angles for coordinates, the rotations for the text and the seven months
angles <- seq(from=pi/2,to=-3*pi/2,length.out=13)[-13]
rotation <- seq(from=0,to=360,length.out=13)[-13]
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
col.viridis <- c4a("viridis",146)

#draws any parametric curve
parametric.draw <- function(range,funct,precision=100,...)
{
  points <- seq(from=range[1],to=range[2],length.out=precision)
  coords <- list(x = funct$x(points), y = funct$y(points))
  lines(coords$x,coords$y,...)
}

#plots the base
plot.base <- function()
{
  par(bg="#222222",mar=rep(0, 4))
  plot(0,type="n",ann=F,axes=F,xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),asp=1)
  circle <- function(r) list(x = function(t) r*cos(t), y = function(t) r*sin(t))
  
  for(i in 1:12)
  {
    text(1.57*cos(angles[i]),1.57*sin(angles[i]),months[i],col="white",srt= -rotation[i],cex=1.15)
    lines(x=c(0.35*cos(angles[i]),1.5*cos(angles[i])),y=c(0.35*sin(angles[i]),1.5*sin(angles[i])),lwd=0.01,lty=5,col="#444444")
  }  
  #the function that i'm using to translate Temperature to radius is
  # r(T) = 0.5 + 0.35T (T is in Celsius)
  parametric.draw(range = c(0,2*pi),funct = circle(1.20),col="red",lwd=2.75)
  text(0,1.275,"2°C",col="red",cex=1.15,family="C")
  parametric.draw(range = c(0,2*pi),funct = circle(0.85),col="red",lwd=1.75)
  text(0,0.925,"1°C",col="red",cex=1,family="C")
  parametric.draw(range = c(0,2*pi),funct = circle(0.5),col="pink",lwd=1.25)
  text(0,0.575,"0°C",col="pink",cex=0.85,family="C")
  parametric.draw(range = c(0,2*pi),funct = circle(1.5),col="white",lwd=1)
}

#plots a specific frame
plot_frame <- function(frame_number,coordin) { 
    
    plot.base()
    
    for(i in 1:frame_number)
    {
    col <- 12 - (-i)%%12
    col.2 <- 12 - (-i - 1)%%12
    
    row <- floor((i-1)/12) + 1
    row.2 <- floor(i/12) +1
    
    
    lines(x=c(coordin[row,col]*cos(angles[col]),coordin[row.2,col.2]*cos(angles[col.2])),
          y=c(coordin[row,col]*sin(angles[col]),coordin[row.2,col.2]*sin(angles[col.2])),col=col.viridis[row.2],lwd=3)
    }
    text(0,0,c(1880:2025)[floor((frame_number-1)/12)+1],col="white",cex=2.3,family="C")
} 

#organizes every plot, fps, number of frames etc.
animation <- function(n=1750,frames,spf)
{
  op <- par(no.readonly = T)
  par(bg="#222222",mar=rep(0, 4))
  
  for(i in 1:ceiling(1/spf))
  plot.base()
  
  for(j in 1:frames)
  {
    plot_frame(floor(j*n/frames),radius)
  }
  
  for(k in 1:ceiling(3/spf))
    plot_frame(n,radius)
  
  par(op)
}

save_gif(
  animation(n=1750,frames=12*5,spf=1/12),
  gif_file = "fast.gif",
  width = 800,
  height = 800,
  delay = 1/12,
  loop = TRUE,
  progress = TRUE)
