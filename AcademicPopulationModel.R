##This is a function for the academic population model##
#Depends: ggplot2, ggthemes, reshape2

acad.mod <- function(S, D, P,
                     v, o, a, b,
                     time){
  for(t in 2:time){
    #Base model
    S[t] <- S[t-1] + (v*P[t-1] + o*D[t-1] - a*(S[t-1]/2) - b*(S[t-1]/4))
    
    D[t] <- D[t-1] + (a*(S[t-1]/2) - b*(D[t-1]/2) - o*D[t-1])
    
    P[t] <- P[t-1] + (b*(S[t-1]/4+D[t-1]/2) - v*P[t-1])
  }
  
  results = matrix(ncol=3, nrow=time)
  results[,1] <- S
  results[,2] <- D
  results[,3] <- P
  return(results)
}

#Define rates
v = 0.05
o = 0.05
a = 0.5
b = 0.3
time = 40

#Set up storage vectors and initial conditions
S = numeric(time)
D = numeric(time)
P = numeric(time)
S[1] = 0.33
D[1] = 0.33
P[1] = 0.34

#Run the model
model.run <- acad.mod(S=S, D=D, P=P,
                      v=v, o=o, a=a, b=b,
                      time=time)

#set up time plotting vector
t.plot <- seq(1,time,1) 

S <- model.run[,1]
D <- model.run[,2]
P <- model.run[,3]


df <- data.frame(PhD = S,
                 PostDoc = D,
                 Prof = P)
library(reshape2)
df.m <- melt(df)
df.m$Time <- t.plot
names(df.m) <- c("State", "value", "Time")



library(ggplot2)
library(ggthemes)
ggplot(data=df.m, aes(x=Time, y=value)) +
  geom_line(aes(color=State, linetype=State), size=1) +
  theme_bw() +
  ylab("Proportion of Academia") + 
  xlab("Time") +
  theme_economist() +
  scale_colour_economist() +
  scale_y_continuous(limits=c(0,1))


