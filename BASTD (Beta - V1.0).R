#To do list for Jason: ------
# Need to adapt code for more advanced analysis of the CR SST, where the code includes anakysis of RTs for separate stimuli
# Need an option to estimate SSRT by block rather than by total file (advanced version)

#Read me ------------------------------------------------------------------------------------------
#Batch analysis of stop-signal task data (BASTD) is an open-source script developed by Jason He - Johns Hopkins School of Medicine, Department of Radiology
#This script allows for batch analysis of performance data outputted from either choice-reaction stop-signal tasks or anticipated response stop-signal tasks
#It is currently flexible with the Verbruggen et al., (2008) STOP-IT task and the He et al., (2020) OSARI task, but can be edited to suit different versions of the stop-signal task
#If you are unable to adapt the code for your own personal use, or you simply want some help, feel free to get into contact with me via email
#I am contactable at my personal email jasonhe93@gmail.com or my current work email jasonhe@jhu.edu - I am more than happy to provide any help with using code I've provided on my Github (including this one)

#There is some assumed knowledge required to use this code. However, I have tried to make this as hands off as possible ...
# such that the user would only have to edit the working directory line ...
# as well as specify the type of task being analyzed (either choice-reaction or anticipated response)...
#Beyond that and determining whether you wish to plot individual level plots (instructions are below) ...
# this code should be relatively 'hands off' 
#The code has been heavily annotated in case you need to understand how it works - again, if you get stuck, feel free to contact me

#Setup --------------------------------------------------------------------------------------------
#First, you must install the packages below (note that not all the packages are used within the code, they are just the packages I tend to use when I work in R)
#Remove the "#" aspect of the line of code to install the package (you can do this quickly if you highlight them all and command + shift + c on Mac OS)
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("dplyr")
#install.packages("sm")
#install.packages("Hmisc")
#install.packages("plyr")
#install.packages("Rmisc")
#install.packages("retimes")
#install.packages("data.table")
#install.packages("tidyr")
#install.packages("lme4")
#install.packages("multcomp")
#install.packages("pastecs")
#install.packages("effects")
#install.packages("car")
#install.packages("DataCombine")
#install.packages("gridExtra")
#install.packages("leaps")
#install.packages("ppcor")
#install.packages("ggm")

#Once the packages are installed, the code below 'library(X)', where X == the package will make the package active 
library(dplyr)
library(ggplot2)
library(ggpubr)
library(sm)
library(Hmisc)
library(plyr)
library(Rmisc)
library(retimes)
library(data.table)
library(tidyr)
library(lme4)
library(multcomp)
library(pastecs)
library(effects)
library(car)
library(DataCombine)
library(gridExtra)
library(leaps)
library(ppcor)
library(ggm)

# Batch analysis of stop-signal task data (BASTD) --------------------------------------------------------------------------------
#For quick use, 
#STEP 1: Set your working directory 
#STEP 2: Clarify whether you want to visualise individual performance (change value from 1 to 0 if you do not want to visualise - LINE 86)
#STEP 3: Specify whether you've used the Choice-reaction or Anticipated Response version of the task (LINE 95 or 96 = defaulted to Choice-reaction)

#Set working directory and set specifications 
setwd("") #setwd to where the participant data is stored (simply write your working directory inbetween the quotation marks)

#Note this is where the folders containing the combined output file and individual plots will saved ...
#... combined output files are saved as 'CombinedBASTD.csv' in a folder called 'Combined data'
#... individual plots are saved as 'X.pdf' in a folder called 'Individual Plots', where X is the participant's original file name

  #Note that, this code assumes that each participant's .txt file is saved in the same folder, rather than within folders 
    #While it is possible to write code that loops in and out of folders, this makes the code messy,
      #just drag and drop them all into a single folder if this is not what you have already 
        #Note to self - consider making the code adapt to cases where each participant's files are stored within folders (toggled using specifications)
          baseDirectory <- getwd() #this sets the user selected directory as the 'baseDirectory' (used for code below)
      
visualise <- 1 #Change the value to 0 if you do not wish to visualise the data (it is set to 1 by default)
  #The reason why I've included the ability to toggle this off is that if you have a large number of participants...
    # it can take quite some time to analyze and plot all the data (turning off visualise makes the code much faster)
        
        cols <- c("0" = "red", "1" = "royalblue", "2" = "royalblue") #These are the colors for the plots, by default royalblue == correct, and red == incorrect
                  #feel free to change the colors to your liking by replacing string with a different value (usually beginning with a '#' for hexidecimal based colors)
                    #or string within the quotations (see: for different options )
    
Participants <- list.files(pattern = ".txt") #This makes a list of all the participants within the working directory set above
Version <- c("Choice reaction Stop-signal task") #remove the '#' in front of this line if the version you are using is the choice-reaction stop-signal task
#Version <- c("Anticipated response Stop-signal task") #remove the '#' in front of this ine if the version you are using is the anticipated response stop-signal task

PerformanceList <- list()
BASTDLIST <- list()

for (p in 1:length(Participants)){
    #Read in and analyze raw stop-signal data----------------------------------------------------------------------------------------------------------
    paste0("Now Processing participant file: ", Participants[p]) #Prints which participant is currently being processed (referred to by file name)
    
    Master <- read.csv(file = Participants[[p]], header = TRUE, sep = "\t")
    
    if(Version=="Anticipated response Stop-signal task"){ 
      Master <- Master[Master$TrialType=="main",] #remove practice trials, since these are not included when analyzing performance
      
      colnames(Master) <- c("block", 'trialType', "trial", "signal", "response", "rt", "ssd") #rename columns so that they are consistent with CR SST
      
      #Estimate whether a participant's performance was correct (this is not done in the code of the task for OSARI)
      Master$signal <- ifelse(Master$signal == 0, Master$signal <- 1, Master$signal <- 0) #signal is currently back to front (0 should be no signal, but atm 0 means is signal)
      Master$correct <- ifelse(Master$signal== 0 & Master$response == 1, 2, #if there is no signal (a go-trial) and there is a response (coded by a 1), then participant 'correctly' responds (coded as a 1)
                             ifelse(Master$signal==1 & Master$response == 0, 2, #if there is a signal (stop-trial) and there is no response (coded by a 0), then participant 'correctly' inhibited their response (also coded as a 1)
                                    0)) #if none of the above, participant must have been incorrect in either not responding (an omission error) or was unable to inhibit (both coded as 0)
      
      Master$rt <- Master$rt*1000 #Change RTs so that values are in ms
    } #this section renames columns so that the OSARI output is consistent with the choice-reaction version of the stop-signal task (which includes creating a column for 'correct', which isn't calculated in the OSARI code)
   
    Data <- Master #Assign the Master dataframe to a dataframe called 'Data' (I typically do this so that a clean version of the Master file is maintained)
    Data$correct <- as.numeric(as.character(Data$correct)) #Make sure columns are numeric
    Data$rt <- as.numeric(as.character(Data$rt))
    Data$ssd <- as.numeric(as.character(Data$ssd))

    #Calculate all performance outcome variables -------------------------------------------------------------------------------------------------------------------------------------------------
    #Go Performance --------------------------------------------------------------------------------------------------------------------------------
    AllGoTrials <- Data[Data$signal==0,] #All Go trials
    NGoTrials <- nrow(AllGoTrials) #sum the number of Go-trials
    
    if(Version=="Anticipated response Stop-signal task"){ 
    GoTrial_RT <- mean(AllGoTrials$rt[AllGoTrials$rt>300]) 
    } #Go-trial RTs below 300 ms are considered to be premature responses in the CR version of the stop-signal task 
    #note that you can change the cut-off values if you have reason to believe otherwise - there are no hard or fast rules here)
    
    if(Version=="Choice reaction Stop-signal task"){ 
      GoTrial_RT <- mean(AllGoTrials$rt[AllGoTrials$rt>140]) 
    } #Go-trial RTs below 140 ms are considered to be premature responses in the CR version of the stop-signal task 
    #note that you can change the cut-off values if you have reason to believe otherwise - there are no hard or fast rules here)
    #However, 140 ms has been historically chosen for speeded responses made to momentary stimuli (such as those in the choice-reaction stop-signal task 
    # since it is unlikely a person can process, make a decision AND respond for a given stimuli at a speed faster than 140ms)
    
    GoTrial_ACC <- (sum(AllGoTrials$correct==2)/NGoTrials)*100 #Calculate Go trial accuracy 
      #(i.e., percentage of Go-trials accurately perfromed) estimated by summing the total number of Go-trials where the participant was accurate and dividing by the total number of go-trials multiplied by 100 
    
    GoTrial_ResponseRate <- (sum(AllGoTrials$response==0)/NGoTrials)*100 #Calculate Go trial response rate
      #(i.e., the opposisite of omission errors) estimated by summing the total number of Go-trials where the participant did not make a response and dividing by the total number of go-trials multiplied by 100
    
    #Fit an ex-gaussian distribution to the Go-trial RT distribution of the participant's Go-trial RTs 
      #Warning, while the code below works, it assumes that the RT distributions can indeed be fit with an ex-Gaussian model
      #There are instances where this may not be the case - for e.g., see Ratcliffe et al., (20XX)
      #If you wish to learn more about why it might be worth looking at mu/sigma/tau of an individual's Go-trial RT distribution, see XXX
      #also note that you are fitting the model to your Go-trial RT distributions AFTER removing those premature responses (which could be an issue)
  
    AllGoTrials <- na.omit(AllGoTrials) #Remove trials where there were no responses made (not applicable to OSARI since trials are repeated if there is an omission)
    exgaus <- retimes::mexgauss(AllGoTrials$rt) #Apply 'retimes' package to model the rt distribution
    #Extract each of the variables from the spit out dataframe
    mu <- exgaus[1] #mu
    sigma <- exgaus[2] # sigma
    tau <- exgaus[3] # tau 
    
    #Stop Performance ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    AllStopTrials <- subset(Data, Data$signal==1) #subset all the Stop-trials
    NStopTrials <- nrow(AllStopTrials) #Calculate the total number of Stop-trials 
    StopTrial_ACC <- (sum(AllStopTrials$correct/2)/NStopTrials)*100 #Calculate Stop Accuracy 
      #(i.e., the percentage of Stop-trials accurately inhibited) estimated by summing total number of Stop-trials where the participant was able to accurately inhibit their response and dividing this by the total number of Stop-trials multiplied by 100
    
    #Mean SSRTs
    MeanSSD <- mean(AllStopTrials$ssd) #Calculate the average SSD (approx to when P(Respond|Signal)=.50)
    MeanSSRT <- GoTrial_RT - MeanSSD #Calculate SSRT via the Mean method - which is a participant's mean Go-trial RT subtracted by their mean SSD (note that Integrated SSRTs are preferred - see XXX)
    
    #Integrated SSRTs
    RTOrder <- sort(AllGoTrials$rt)#Rank order the Go-trial RTs 
    n <- NGoTrials * (StopTrial_ACC/100) #Calculate n, where n is obtained by multiplying the number of go-trials by the probability of responding at a given delay (StopTrial_ACC)
    NthRT <- RTOrder[n] #Find the RT that corresponds to the Nth RT 
    IntegrationSSRT <- NthRT - MeanSSD #Calculate SSRT via the Integration method 
    
    #Participant and task details -------------------------------------------------------------------------------------
    file_name <- Participants[p] #Create a column which identifies the file that was analyzed
      #This will eventually be adjusted to be able to identify participant numbers based on the name of the .txt file - at least for OSARI output
    
    #Combine all the performance variables into a single row for each participant (via a column bind)
    Performance <- cbind(
                        file_name, #file name 
                          GoTrial_RT, GoTrial_ACC, GoTrial_ResponseRate, mu, sigma, tau, #Go-trial related variables
                         StopTrial_ACC, MeanSSD, MeanSSRT, IntegrationSSRT #Stop-trial and stopping related variables
                         ) 
                        
    PerformanceList[[p]] <- as.data.frame(Performance) #Put each pparticipant's performance into a list
    
    #Create plots displaying performance at the trial level ----
    AllGoTrials <- Data[Data$signal==0,] #subset to all the Go-trials
    
    AllGoTrials$trial <- 1:nrow(AllGoTrials) #Adjust the Go
    
    plot1 <- ggplot(AllGoTrials, aes(x=trial, y=rt, color = as.factor(AllGoTrials$correct)))+
      geom_point(size =1.5) +theme(legend.position="none")  +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      geom_vline(linetype = "solid", xintercept=64) + geom_vline(linetype = "solid", xintercept=128)+
      labs(y = expression("Response Time (ms)", x = "Trial Number"))+
      scale_colour_manual(values = cols)
    
    if(Version=="Anticipated response Stop-signal task"){ 
      plot2 <- ggplot(AllGoTrials, aes(x = rt)) +
      geom_density(alpha = .3)+
      geom_vline(aes(xintercept = mean(rt, na.rm = T)),
                 colour = "black", size = .3)+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      theme(legend.position="none") +
      labs(y = "Density", x = "Response Time (ms)")+
      scale_fill_manual(values = c("white", "gray")) +
      guides(fill=guide_legend(title="Stimulus"))
    } #The AR SST only has one stimulus type so the code is slightly different
    
    if(Version == "Choice reaction Stop-signal task"){
      plot2 <- ggplot(AllGoTrials, aes(x = rt, fill = as.factor(stimulus))) +
        geom_density(alpha = .3)+
        geom_vline(aes(xintercept = mean(rt, na.rm = T)),
                   colour = "black", size = .3)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        theme(legend.position="none") +
        labs(y = "Density", x = "Response Time (ms)")+
        scale_fill_manual(values = c("white", "gray")) +
        guides(fill=guide_legend(title="Stimulus"))
    } #The CR SST only has two different stimuli so the code plots two separate density plots here

    
    RTplots <- ggarrange(plot1, plot2, ncol = 1, align = "hv")
    
    plot3 <- ggplot(AllStopTrials, aes(x=trial, y=ssd, color = as.factor(AllStopTrials$correct)))+
      geom_point(size =2) +theme(legend.position="none")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      theme(legend.position="none") +
      labs(y = "SSD", x = "trial")+
      scale_colour_manual(values = cols)+
      geom_hline(linetype = "dashed", yintercept=MeanSSD)
    
    SSDData <- as.data.frame(table(AllStopTrials$ssd, AllStopTrials$correct)) #Combine the SSDs to columns showing the accuracy and frequency of an individual's performance (into a dataframe called SSDData)
    colnames(SSDData) <- c("SSD", "Correct", "Frequency") #Rename those columns so they make sense
    SSDData$SSD <- as.numeric(as.character(SSDData$SSD))
    SSDCorrect <- SSDData[SSDData$Correct==2,]  #Split the SSDData into those that were correct
    SSDCorrect$Correct <- as.numeric(as.character(SSDCorrect$Correct))/2
    SSDIncorrect <- SSDData[SSDData$Correct==0,] # and into those that were incorrect
    SSDCorrect$Frequency <- SSDCorrect$Frequency + SSDIncorrect$Frequency #This calculates the total frequency of a given SSD (how often the SSD came up during task completion)
    SSDCorrect$PRespond <- as.numeric(as.character(SSDCorrect$Correct))/SSDCorrect$Frequency #This calculates the Probability of responding (or P(Respond|Signal)) for each SSD

    SSDCorrect$Group <- 1
    
    plot4 <- ggplot(SSDCorrect, aes(x=SSDCorrect$SSD, y = SSDCorrect$PRespond, group = SSDCorrect$Group))+
      geom_point(size = 2) +theme(legend.position="none")+
      geom_line()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      theme(legend.position="none") +
      labs(y = "P(Respond|Signal)", x = "SSDs")+
      scale_colour_manual(values = cols)+ylim(0,1)+
      geom_hline(linetype = "dashed", yintercept=.50)
    
    SSDPlots <- ggarrange(plot3, plot4, ncol = 1, align = "hv")
    
    #Text box for plots  --------
    
    #Text for Anticipated Response version of the Stop-signal task
    if(Version=="Anticipated response Stop-signal task"){ 
    text = paste("\n   Mean Go-trial RT:", round(GoTrial_RT, digits = 2), "ms", 
                 "\n   Go-trial Accuracy:", round(GoTrial_ACC, digits = 2), "%",
                 "\n   Go-trial Response Rate:", 100-round(GoTrial_ResponseRate, digits = 2), "%",
                 "\n   Mean SSD:", round(MeanSSD, digits = 2), "ms",
                 "\n   Mean Method SSRT:", round(MeanSSRT, digits = 2), "ms",
                 "\n   Integration Method SSRT:", round(IntegrationSSRT, digits = 2), "ms")
                 
    
    textPlot <- ggplot() +
      annotate("text", x = 4, y = 25, label = text) +
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank())
    
    blanktextPlot <- ggplot() +
      annotate("text", x = 4, y = 25, label = text, color = "white") +
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank())
    }
    
    if(Version == "Choice reaction Stop-signal task"){
      text = paste("\n   Mean Go-trial RT:", round(GoTrial_RT, digits = 2), "ms", 
                   "\n   Go-trial Accuracy:", round(GoTrial_ACC, digits = 2), "%",
                   "\n   Go-trial Response Rate:", 100-round(GoTrial_ResponseRate, digits = 2), "%",
                   "\n   Mean SSD:", round(MeanSSD, digits = 2), "ms",
                   "\n   Mean Method SSRT:", round(MeanSSRT, digits = 2), "ms",
                   "\n   Integration Method SSRT:", round(IntegrationSSRT, digits = 2), "ms")
      
      textPlot <- ggplot() +
        annotate("text", x = 4, y = 25, label = text) +
        theme_bw() +
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank())
      
      blanktextPlot <- ggplot() +
        annotate("text", x = 4, y = 25, label = text, color = "white") +
        theme_bw() +
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank())
    }
    
    textPlots <- ggarrange(textPlot, blanktextPlot, ncol = 1, nrow = 2)
  
    BASTD <- ggarrange(RTplots, SSDPlots, textPlots, ncol = 3, align = "hv")
    
    BASTD <- annotate_figure(BASTD, top = text_grob(paste(Participants[p])
                                                    , color = "black"))
    
    BASTD <- annotate_figure(BASTD, top = text_grob("Batch Analysis of Stop-signal Task Data (BASTD)", color = "black", face = "bold"))
    dir.create("Individual Plots", showWarnings = FALSE) #Creates a directory to put the plots (i.e., the .csv file) into (this is just your base directory + plots)
    setwd(paste0(baseDirectory,"/Individual Plots")) #Switch to the folder that was just created so we can save the plots in that folder
    ggsave(paste0(Participants[p],".pdf"), width = 10, height = 5)
    
    setwd(baseDirectory) #return to base directory for remaining participants
}

setwd(baseDirectory) #return to base directory 
dir.create("Combined Data", showWarnings = FALSE) #Creates a directory to put the plots (i.e., the .csv file) into (this is just your base directory + plots)
setwd(paste0(baseDirectory,"/Combined Data")) #Switch to the folder that was just created so we can save the plots in that folder
Data <- rbindlist(PerformanceList, fill = TRUE) #Combine the rows containing each participant's performance
write.csv(Data, file = "CombinedBASTD.csv") #save the file
