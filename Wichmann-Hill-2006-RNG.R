library(tictoc)    ### For timing

###
### Wichmann-Hill, 2006 Algorithm
### B.A. Wichmann, I.D.Hill, 2006, Generating good pseudo-random numbers.  Computational Statistics and Data Analysis
### Algorithm (p1616)
### 
# ix := 11 600 × ix mod 2 147 483 579;
# iy := 47 003 × iy mod 2 147 483 543;
# iz := 23 000 × iz mod 2 147 483 423;
# it := 33 000 × it mod 2 147 483 123;
# W := ix/2 147 483 579.0 + iy/2 147 483 543.0 + iz/2 147 483 423.0 + it/2 147 483 123.0; return W −⌊W⌋.
### p1620, Generating Many sequences.
# cycle the ix & iy to create seed sets for other sequences. These guarentee no overlap between sequences for a very long time
# ix := 46 340 x ix mod 2 147 483 579;
# iy := 22 000 x iy mod 2 147 483 543;


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##NG("Function", 
	# "MultiSeed.WH(InitSeed, N=1, runin=0, runout=0)", 
	# "Generates new random seeds for each Simulation from the seeds for each previous simulation. InitSeed is a vector of 4 initial seeds (<2^31), N is the number of simulations to be run (each corresponding to one column of the Seed, runin is the initial run-in period, runout 'spreads' out the initial seeds for each parallel random number stream by cycling the initialization)")
	
MultiSeed.WH <- function(InitSeed, N=1, runin=0, runout=1){
	RandSeed.WH <<- matrix(InitSeed, nrow=4, ncol=N)
	if(N>1){
		for(iC in 2:N){
			RandSeed.WH[1:2,iC] <<-(c(46340, 22000)* RandSeed.WH[1:2,iC-1])  %% c(2147483579, 2147483543)
			if(runout>0){
				for(iR in 1:runout){
					RandSeed.WH[1:2,iC] <<-(c(46340, 22000)* RandSeed.WH[1:2,iC])  %% c(2147483579, 2147483543)
				}
			}
		}		
	}
	if(runin>0){
		for(iR in 1:runin){
			RandSeed.WH <<- (c(11600, 47003, 23000, 3300) * RandSeed.WH) %% c(2147483579, 2147483543, 2147483423, 2147483123) 	
		}
	}
}

##NG("Function", 
	# "GetRandUnif(NRow)", 
	# "Creates a matrix of random uniform variables.  Each column is an independent, parallel stream of random numbers, the number of which is the number of columns of the random seed (RandSeed.WH)")
GetRandUnif <- function(NRow){
  ResMat <- matrix(0, NRow, ncol(RandSeed.WH) )
  for( i in 1:NRow){
	RandSeed.WH <<- (c(11600, 47003, 23000, 3300) * RandSeed.WH) %% c(2147483579, 2147483543, 2147483423, 2147483123)
	ResMat[i, ] <- apply(RandSeed.WH, 2, function(V){ sum( V/ c(2147483579, 2147483543, 2147483423, 2147483123) )}) %%1
  }
  return(  ResMat)
}

##########################################
### some testing

MultiSeed.WH(1:4, 1)
RandSeed.WH

MultiSeed.WH(1:4, 5)
RandSeed.WH
MultiSeed.WH(1:4, 5, runin=30)
RandSeed.WH

GetRandUnif(20)


MultiSeed.WH(1:4, 50, runin=30)
GetRandUnif(20)[,1:5]   #### 

tic("Test 1000 parallel random sequences.")
MultiSeed.WH(1:4, 1000)
toc()
RandSeed.WH[, 1:30]
tic("Fetch a 1000 x 1000 matrix of random numbers.")
Rmat<- GetRandUnif(1000)
toc()

summary( apply(Rmat, 1, mean) )
summary( apply(Rmat, 2, mean) )
summary( apply(Rmat, 1, sd) )
summary( apply(Rmat, 2, sd) )

tic("Use runif() to fetch a 1000 x 1000 matrix of random numbers. (Mersenne-Twister).")
Rmat<- matrix(runif(1e6), nrow=1000, ncol=1000)
toc()


### Correlations within random streams
MultiSeed.WH(1:4, 10)
toc()
RandSeed.WH[, 1:30]
tic("Fetch a 1000 x 1000 matrix of random numbers.")
Rmat<- GetRandUnif(1000)

cor(Rmat)
help(cor)

### Correlations across random streams
MultiSeed.WH(1:4, 1000)
Rmat <-GetRandUnif(10)
cor(t(Rmat))
