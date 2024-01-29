library(plotrix) ### for draw.ellipse, draw.circle
BeeSim = new.env()

### Initialize data frames beetles and food
#' ---
#' title: BeeSim Monte Carlo Environment
#' author: Detlef Groth, University of Potsdam
#' date: 2024-01-08
#' ---
#'
#' ## BeeSim - object
#' 
#' The BeeSim object has the following attributes:
#' 
#' - _beetles_ - data frame containing beetle information with columns
#'    - _x_         - x-coordinate
#'    - _y_         - y-coordinate
#'    - _sex_       - beetle's sex, F or M
#'    - _color_     - beetle's color, either red or orange
#'    - _points_    - number of points, either 4,5,6, or 7
#'    - _age_       - age of beetle given in iterations
#'    - _energy_    - energy units, starts with 10 units, increase by two if food item is eaten
#' 
#' - _food_ - data frame with the coordinates of x and y for the food items, columns are: 
#'    - _x_         - x-coordinate
#'    - _y_         - y-coordinate
#'    - _age_       - age of food, food can exists at minimum for ten iterations, therafter has a 10 percent chance to disappear in every iteration
#' 
#' - _monitor_ - data frame with the counts of beetles and food items
#'    - _iter_      - iteration number
#'    - _beetles_   - number of beetles
#'    - _food_      - number of food items
#' 
#' The BeeSim object has the following methods:
#' 
#' ## BeeSim$new(n) - initialize the BeeSim object with n beetles and initial food items
#' 
#' This is the function which should be called first to intialize the beetles
#' and the food data frame before starting with the iterations.
#' 
#' Arguments:
#' 
#' - _n_ - number of beetles
#' 

BeeSim$new <- function (n) {
    self=BeeSim
    ## the function which should be used first (constructor)
    ## create n beetles, around 50% males (M) and females (F)
    ## using rbinom function
    ## the beetles should have mostly red color but 20% of the beetles should be orange
    ## again you can use the rbinom function
    ## with x and y coordinates between 0 and 50 with runif
    ## store them in a data frame BeeSim$beetles with columns
    ## x, y, sex, color, points (random number between 4 and 7 with even distribution), 
    ## energy (should be initialized with a value 10)
    self$beetles=data.frame(x=runif(n,min=0,max=50),
                            y=runif(n,min=0,max=50),
                            sex=c("F","M")[rbinom(n,1,p=0.5)+1],
                            color=c("red","orange")[rbinom(n,1,p=c(0.8,0.2))+1],
                            points=sample(4:7,n,replace=TRUE),
                            age=sample(0:5,n,replace=TRUE),
                            energy=rep(10,n))
    self$food=data.frame(x=c(rnorm(10,mean=12,sd=5),rnorm(20,mean=42,sd=5),runif(20,0,50)),
                         y=c(rnorm(10,mean=12,sd=5),rnorm(20,mean=42,sd=5),runif(20,0,50)),
                         age=rep(0,50))
    self$monitor=data.frame(iter=0,beetles=nrow(self$beetles),food=nrow(self$food))
}

### Draw an individual beetle of the beetles data frame 
### Private Function
BeeSim$.drawBeetle <- function (i,min.age=10) {
    self=BeeSim
     ## draw the ith beetle from the beetles data frame
     rad2deg <- function(rad) {(rad * 180) / (pi)}
     deg2rad <- function(deg) {(deg * pi) / (180)}
     draw.beetle = function (x,y,a=1.5,b=2,sex='F',
                                col='red',points=7) {
        draw.ellipse(x,y,a=a,b=b,col=col)
        for (i in 1:points) {
            t=deg2rad((360/points)*i)
            xp = a*cos(t)*0.75 + x;
            yp = b*sin(t)*0.75 + y;
            points(xp,yp,col='black',pch=19,cex=1)
        }
        if (sex=="M") {
            points(x,y,pch=19,cex=1,col=4)
        }

     }
     ## extract the data for the ith beetle and plot
     if (self$beetles$age[i]<min.age) {
         # baby beetles, just points
         points(self$beetles[i,'x'],self$beetles[i,'y'],col=self$beetles[i,'color'],pch=19,cex=1.5)
     } else {
         col=self$beetles[i,'color']
         if (self$beetles$energy[i] <= 10) {
             # not enough energy for mating make them pale
             if (col=="orange") {
                 col="#ffcc99"
             } else {
                 col="#ff6666"
             }
         }
         draw.beetle(x=self$beetles[i,'x'],
                     y=self$beetles[i,'y'],
                     sex=self$beetles[i,'sex'],
                     col=col,
                     points=self$beetles[i,'points'])
     }
}
### Draw the landscape where the beetles live
### the trees indicate regions with higher amount of food
#'
#' ## BeeSim$drawlandscape(n) - draw the landscape without food and beetles
#' 
#' This is the drawing function which is called within drawBeetles.
#' So it should be only used by the user to show a landscape without
#' beetles and without food.
#' 
#' Arguments:
#' 
#' - None
#' 

BeeSim$drawLandscape <- function () {
    plot(1,type="n",xlim=c(0,50),ylim=c(0,50),asp=1,xlab="",ylab="")
    draw.circle(25,25,3,col="skyblue")
    drawTree <- function (x,y) {
        polygon(x=c(-0.5,0.5,0.5,1.5,0,-1.5,-0.5)+x,y=c(0,0,1,1,3,1,1)+y,
            col="green")
    }   
    drawTree(10,10)
    drawTree(12,12)  
    drawTree(14,10)  
    drawTree(38,38)
    drawTree(40,40)  
    drawTree(42,38)  
    drawTree(44,40)  
    drawTree(46,38)  
    
}
#' ## BeeSim$drawBeetles() - draw the landscape and food and beetles
#' 
#' This is the drawing function which is called within drawBeetles.
#' So it should be used by the user to show the landscape with
#' the current beetle and food positions
#' 
#' Arguments:
#' 
#' - None
#' 

BeeSim$drawBeetles <- function () {
    self=BeeSim
    self$drawLandscape()
    points(self$food,pch=15,col="grey80")
    ## loop over beetles data frame and plot them, beetle by beetle    
    for (i in 1:nrow(BeeSim$beetles)) {
        self$.drawBeetle(i)
    }
    idx=nrow(self$monitor)
    title(sprintf("iter: %i beetles: %i food items: %i",self$monitor$iter[idx],self$monitor$beetles[idx],self$monitor$food[idx]))
}

#' ## BeeSim$iter() - perform an iteration
#' 
#' Beetles move randomly and then look for food in their environment.
#' If the food is within their sight they move to the sight and eat the food.
#' 
#' Arguments:
#' 
#' - _sd_    - standard deviation for the random movement, default: 1
#' - _sight_ - sight distance to look for food, default: 2
#' - _debug_ - should debug messages be displayed, default: TRUE
#' 


BeeSim$iter <- function (sd=1,sight=2,debug=TRUE) {
    self=BeeSim
    ## * move randomly every beetle within the grid
    ##   - sight x and y: mean=2,sd=1
    ##   - create vector of x and y changes
    x=rnorm(nrow(self$beetles),mean=0,sd=sd)
    y=rnorm(nrow(self$beetles),mean=0,sd=sd)
    ##   - add these values to the coordinates
    self$beetles$x=self$beetles$x+x
    self$beetles$y=self$beetles$y+y    
    ##   - stay within the grid (0,50) -1 => 1 51 => 49
    ##     idx=which(x<0)
    idx=which(self$beetles$x<0)
    if (length(idx)>0) {
        self$beetles$x[idx]=abs(self$beetles$x[idx])
    }
    idx=which(self$beetles$x>50)
    if (length(idx)>0) {
        self$beetles$x[idx]=self$beetles$x[idx]-50
    }
    idx=which(self$beetles$y<0)
    if (length(idx)>0) {
        self$beetles$y[idx]=abs(self$beetles$y[idx])
    }
    idx=which(self$beetles$y>50)
    if (length(idx)>0) {
        self$beetles$y[idx]=self$beetles$y[idx]-50
    }
    ## * look for food dist(rbind(self$beetles[,1:2],self$food)
    ##   -  combine food and beetles in one distance matrix
    
    D=as.matrix(dist(rbind(self$beetles[,1:2],self$food[,1:2])))
    n=nrow(self$beetles)
    for (i in 1:n) {
        ## food distances
        f=D[i,(n+1):ncol(D)]
        if (any(f<sight)) {
            ## update energy=energy+2
            idx=sample(which(f<2),1)
            #print(paste(i,"is eating",idx))
            ## remove food from grid
            ## place a value of 100 to D for this food item
            ## so that it is not eaten again
            D[,n+idx]=100
            self$beetles[i,'energy']=self$beetles[i,'energy']+2
        }
    }
    ## * Update Beetles:
    ##   - update age for all beetles: age = age +1
    self$beetles$age=self$beetles$age+1
    ##   - update energy for all beetles: energy = energy - 1
    self$beetles$energy=self$beetles$energy-1
    ##   - check energy if < 0 remove beetle from data frame
    idx=which(self$beetles$energy>=0)
    if (length(idx)<nrow(self$beetles)) {
        if (debug) {
            print(paste(nrow(self$beetles) - length(idx),"starving and dying ..."))
        }
        self$beetles=self$beetles[idx,]
    }
    ## Update food:
    ## remove all items where D==100 or lower than 99
    # remove eaten food
    idx=which(D[1,c((n+1):ncol(D))]<99)
    self$food=self$food[idx,]
    ### check if beetle is in lake, if yes remove it 
    D=as.matrix(dist(rbind(BeeSim$beetles[,1:2],data.frame(x=25,y=25))))
    idx=which(D[ncol(D),1:(ncol(D)-1)]<3)
    if (length(idx)>0) {
        if (debug) {
            print(paste(length(idx),"beetle(s) in lake, dying ..."))
        }
        self$beetles=self$beetles[-idx,]
    }
    ## remove 10% of food older than 10 iterations
    ## TODO: No age, just 5% deleting?
    self$food$age=self$food$age+1
    idx=which(self$food$age>10)
    if (length(idx)>0) {
        remove=idx[which(rbinom(length(idx),1,p=0.1)==1)]
        if (debug) {
            print(paste("removing", length(remove),"food items ..."))
        }
        if (length(remove)>0) {
            self$food=self$food[-remove,]
        }
        #print(paste()
    }
    ##   - create new food, add food items at the end of food data frame
    newfood=data.frame(x=c(rnorm(10,mean=12,sd=5),rnorm(20,mean=42,sd=5),runif(20,0,50)),
                       y=c(rnorm(10,mean=12,sd=5),rnorm(20,mean=42,sd=5),runif(20,0,50)),age=rep(0,50))
    self$food=rbind(self$food,newfood)
    ### Next: 
    ### remove food outside of grid
    idx=unique(c(which(self$food$x>50),which(self$food$x<0),which(self$food$y>50),which(self$food$y<0)))
    if (length(idx)>0) {
        self$food=self$food[-idx,]
    }
    # restrict food to 1000 units
    if (nrow(self$food)>1000) {
        idx=sample(1:nrow(self$food),1000)
        self$food=self$food[idx,]
    }
    idx=nrow(self$monitor)
    self$monitor=rbind(self$monitor,data.frame(iter=self$monitor$iter[idx]+1,beetles=nrow(self$beetles),food=nrow(self$food)))
    ### Mating: Might be placed in own function
    ### let girls look for boys if energy > 10 and age > 10
    ### check if male is in sight (D<5 for instance)
    ### if yes create new beetle with age=0 and place it randomly in an area close to the two beetles
    ### properties, color, sex, points should be sampled from mother and fathers properties
    ### remove 5 energy points from male and female and place these nergy points into the newborn
}

#'
#' ## BeeSim$mating(sight=3) - perform mating between males and females
#' 
#' This function performs mating between males and females and creates offspring.
#' If females have enough energy they look for males  and mate the offspring is 
#' inheriting the properties from both parents and are released close to their 
#' parents with age zero and some energy taken from the parents.
#' 
#' Arguments:
#' 
#' - _sight_      - sight distance to look for mating partner, default: 3
#' - _min.female_ - minimal amount of energy for a female to search for mates, default: 10
#' - _min.male_   - minimal amount of energy for a male to agree on mating, default: 5
#' - _min.age_    - required age for mating, default: 10
#' - _childs_     - number of childs, default: 5
#' - _debug_ - should debug messages be displayed, default: TRUE

BeeSim$mating <- function (sight=4,min.female=10,min.male=5,min.age=10,childs=5,debug=TRUE) {
    self=BeeSim
    candidate.female=which(self$beetles$age>min.age & self$beetles$sex=="F" & self$beetles$energy>min.female)
    childs.df=NULL
    if (length(candidate.female)>0) {
        D=as.matrix(dist(rbind(BeeSim$beetles[,1:2])))
        for (f in candidate.female) {
            if (any(D[f,]<sight)) {
                for (b in which(D[f,]<sight)) {
                    if (b == f) {
                        next
                    }
                    if (self$beetles$sex[b] == "M" &  self$beetles$energy[b]>min.male & self$beetles$age[b]>min.age) {
                        if (debug) {
                            print(paste(f,"mates prosperous",b)) 
                        }
                        # newborns data
                        x=runif(childs,min=min(self$beetles[c(f,b),'x']), max=min(self$beetles[c(f,b),'x']))
                        y=runif(childs,min=min(self$beetles[c(f,b),'y']), max=min(self$beetles[c(f,b),'y']))
                        sex=sample(c("M","F"),childs,replace=TRUE)
                        color=sample(self$beetles$color[c(f,b)],childs,replace=TRUE)
                        points=sample(self$beetles$points[c(f,b)],childs,replace=TRUE)
                        if (class(childs.df)=="NULL") {
                            childs.df=data.frame(x=x,y=y,sex=sex,color=color,points=points,age=0,energy=1)
                        } else {
                            childs.df=rbind(childs.df,data.frame(x=x,y=y,sex=sex,color=color,points=points,age=0,energy=1))
                        }
                        # reduce energy for mating pair, females invest more, most of the energy comes from females
                        self$beetles[f,'energy']=self$beetles[f,'energy']-(childs-1)
                        self$beetles[b,'energy']=self$beetles[b,'energy']-1
                        break
                    }
                }
            }
        }
    }
    if (class(childs.df)!="NULL") {
        self$beetles=rbind(self$beetles,childs.df)
    }
    ## remix order of beetles to avoid bias for females on top of 
    self$beetles=self$beetles[sample(1:nrow(self$beetles)),]
}

#'
#' ## BeeSim$plotMonitor() - plot simulation monitor
#' 
#' This function plots the changes in food and and beetle count over the 
#' iterations. Should be called after a certain number of iterations.
#' 
#' Arguments: None
#' 
BeeSim$plotMonitor <- function () {
    self=BeeSim
    plot(self$monitor$food ~ self$monitor$iter,col="grey60",type="l",ylim=c(0,max(self$monitor$food)),ylab="Counts",xlab="Iterations",main="Model Monitoring",lwd=3)
    grid()
    points(self$monitor$beetles ~ self$monitor$iter,col="blue",type="l",lwd=3)
    
}

#' ## BeeSim$main() - function to run a simulation
#' 
#' This function is the starting place to run a simulation.
#' 
#' 
#' Arguments: 
#' 
#' - _iter_    - number of iterations, default: 100
#' - _beetles_ - number of beetles, default: 20   
#' - _pdf_     - should a pdf (beetles-simul.pdf) be produced, default: FALSE
#' 
BeeSim$main <- function (iter=100,beetles=20,pdf=FALSE) {
    if(pdf) {
        pdf("beetles-simul.pdf"); 
    }
    BeeSim$new(beetles) ; 
    for (i in 1:iter)  { 
        BeeSim$iter(); 
        BeeSim$mating(); 
        BeeSim$drawBeetles(); 
        Sys.sleep(0.2) 
    } 
    BeeSim$plotMonitor(); 
    if (pdf) {
        dev.off()
    }
}

#' ## Example:
#' 
#' Here an example run:
#' 
#' ```{r fig=TRUE,fig.width=16,fig.height=9}
#' set.seed(123)
#' source('beesim.R')
#' par(mfrow=c(2,3),mai=c(0.2,0.2,0.7,0.1))
#' BeeSim$new(20)   # create 20 agents
#' BeeSim$drawBeetles()
#' BeeSim$iter()
#' for (i in 1:5) { BeeSim$iter() }
#' BeeSim$drawBeetles()
#' for (i in 1:10) { BeeSim$iter() ; BeeSim$mating()}
#' BeeSim$drawBeetles()
#' for (i in 1:35) { BeeSim$iter(debug=FALSE) ; BeeSim$mating(debug=FALSE)}
#' BeeSim$drawBeetles()
#' for (i in 1:35) { BeeSim$iter(debug=FALSE) ; BeeSim$mating(debug=FALSE)}
#' BeeSim$drawBeetles()
#' BeeSim$plotMonitor()
#' ```
#'
#' ## Possible Homework projects:
#' 
#' - __Predator:__
#'    - add a predator to the grid which eats beetles
#'    - the system should be stable, so the predator might take the energy from the beetle with some loss
#'    - it should as well mate, but produces less offspring
#'    - numbers of predators should be as well monitored
#' - __Infectious agent:__
#'    - there should be a very low chance that a beetle might be infected with some infectious agent, may be per iteration 1/100
#'    - that agent should be transmissible to other beetles within a certain distance and chance proportional to that distance
#'    - the beetle might die with a 50 percent chance after 10 iterations if infected
#'    - monitor the number of infected beetles, they should be at least some values with more than 10 infected beetles more than once
#' - __Seasonal food amount changes:__
#'    - there should be seasonality of the produced food amount and the lake area
#'    - use for instance a sinus curve 1 year should be 53 iterations (1 iteration is one week then) to create variable amounts of food, beetween 10 and 100 food items during the year
#'    - in the wet season, with more food, the lake area in the middle should enlarge, having 4 times the lake area than in the dry season
#'    - trees should be yellow in dry season, green in wet season
#'    - monitor as well the produced food amount
#'    - add season indicators to the monitor plot
#' - __Seasonal food location changes:__
#'    - there should be seasonality of the locality of the produced food
#'    - within one year, should be 53 iterations - 1 iteration is one week then, the two food centers should in circular manner move around the lake 
#'    - to indicate this move the trees on the plot around (you can as well create other type of plants, trees usually do not move)
#'    - lake size should depending on the season as enlarge (up to 4 times the area) and decrease to the original size again#
#'    - add the lake size as well to the monitor plot
#' - __Point size genetics:__
#'    - the sight variable should be depending on the number of points
#'    - more points cost however more enery per iteration
#'    - below some example settings:
#'    - with 4 points energy loss in one iteration will be always 1
#'    - with 5 points energy loss in one iteration will be in 10% of the cases 2
#'    - with 6 points energy loss in one iteration will be in 20% of the cases 2
#'    - with 7 points energy loss in one iteration will be in 30% of the cases 2
#'    - females favor males with more points probably of chosing a male should be based on the number of points
#'    - males with 7 points should have a higher chance of being selected, but 4 point males should be not without chance
#'    - monitor the average point count as well in the monitor plot
#'    - explore two settings, one which favors fewer points, one which favors more points
#'    - but both point groups should at least survive 25 iterations
#' - __Male competition:__
#'    - male compete with each other
#'    - after each iteration you check all males if there is an other mail within their sight distance
#'    - if this is the case you perform a match between them
#'    - you randomly assign one of them winning where the winning chances are based on there current energy level
#'    - the loser is moved by two units to the center where the lake is
#'    - the loser loses two energy units, the winner only one
#'    - males with 4 points never play, so they are only moved towards the center but there is no loss of energy due to a battle
#' 
#' If you have other ideas for projects just tell me.
#'
#' ## Homework report - Deadline February 6th (no extension!)
#' 
#' The homework report should be in paper style like the penguin report. 
#' Please form groups of 2-5 people,
#' may be each one responsible for one text section and some for the coding.
#' Please do topic registration until January 16th with an E-Mail to me. 
#' Topic selection and authors (students) should be mentioned in this E-mail.
#' 
#' The following topics should be in the paper:
#' 
#' - Abstract: summarizing with a few sentences the paper
#' - Introduction: some text about Monte Carlo Simulation and may be ecological models or 
#'   other examples of Monte Carlo simulations known in literature, hypothesis might be that we can create an artifical society of lady beetles and ...
#' - Data and Methods: shortly describe data generation properties of agents (beetles, food) and the landscape
#' - Statistical Analysis: what version of R and which packages where used, cite them properly, methods and atributes of the R environment in a table
#' - Results: should be descriptive, we could as well do inferential statistics evaluation differences between several runs with different parameter settings 
#' (like initial agent counts, with 10 agents we might for instance never get a working society), figures of landscape, a few example runs, after a few iterations
#' - Discussion: based on the results, some personal thoughts about Monte Carlo simulations
#' - References: R, used R packages, plotrix, cited papers in Introduction and Discussion
#' 
#' 
#' ## Document processing
#' 
#' This document was processed using this little Bash script on Linux:
#' 
#' ```
#' function r2html {
#'     if [ -z $1 ]; then
#'         echo "Usage: r2html RFILE ?CSSFILE?"
#'         return
#'     fi
#'     if [ -z $2 ]; then 
#'         CSS=NULL
#'     else
#'         CSS=$2
#'     fi
#'     ext="${1##*.}"
#'     out=`basename $1 .$ext`.html
#'     perl -ne "/^#' ?(.*)/ and print \"\$1\n\"" $1 > temp.rmd
#'     echo "library(rmarkdown);rmarkdown::render('temp.rmd',output_file='$out',
#'         html_document(toc=TRUE,css='$CSS'))" | Rscript -
#' }
#' ```
#'
#' Command line was: `r2html beesim.R beesim.css`
#' 
#' ## License and Copyright
#' 
#' ```
#' Copyright 2023-2024, Detlef Groth, University of Potsdam, Germany
#'
#' Permission is hereby granted, free of charge, to any person obtaining a
#' copy of this software and associated documentation files (the
#' 'Software', to deal in the Software without restriction,
#' including without limitation the rights to use, copy, modify, merge,
#' publish, distribute, sublicense, and/or sell copies of the Software, and to
#' permit persons to whom the Software is furnished to do so, subject to the
#' following conditions: 
#'
#' The above copyright notice and this permission notice shall be included in
#' all copies or substantial portions of the Software. 
#'
#' THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
#' EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#' MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
#' NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
#' DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
#' OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
#' USE OR OTHER DEALINGS IN THE SOFTWARE. 
#' ```

