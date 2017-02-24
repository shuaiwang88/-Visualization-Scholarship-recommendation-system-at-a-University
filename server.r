########## Load libraries 
#library(party)
#library(rpart.plot)
#library(rpart)

#install.packages('shiny')
library(shiny)
#library(lattice)
#install.packages("scatterplot3d", dependencies = TRUE)
#library(scatterplot3d)

# A read-only data set that will load once, when Shiny starts, and will be
# available to each user session
#source('XXXX.R', local=TRUE)

result_library <- read.csv("E4.1G4.1B2.8M_OpTesultUnderRep.csv")
subset_result <- result_library

enroll_coms_plot <- function (aSubSet_Result) {
  
  aSubSet_Result$GPA_ACT = aSubSet_Result$GPA_RD*10 + aSubSet_Result$HIGHEST.ACT.COMP..SCORE
  xyCombo <- split(seq(1:nrow(aSubSet_Result)), list(aSubSet_Result$GPA_ACT), drop=FALSE)
  xyAward.df <- sapply(xyCombo, function(.indx){
    c(COMS=aSubSet_Result$GPA_RD[.indx][1]*10+subset_result$HIGHEST.ACT.COMP..SCORE[.indx][1],
      GPA=aSubSet_Result$GPA_RD[.indx][1],
      ACT=aSubSet_Result$HIGHEST.ACT.COMP..SCORE[.indx][1],
      Award=mean(aSubSet_Result$Allocate[.indx]) )         # total price paid  
  })  
  xyAward.df<- data.frame(t(xyAward.df))
  
  
  par(mfrow=c(2,1))
  plot(xyAward.df$Award~xyAward.df$COMS, xlab="Composite Score = 10 x GPA + ACT", ylab="Average Scholarship", 
       main="Scholarship Versus Composite Scores")
  
  #sunflowerplot(xyAward.df$COMS, xyAward.df$Award)
  
  
  library(rpart)  
  fit <- rpart(Award ~ COMS,  method="class", data=xyAward.df)  #grow a tree  
  plot(fit, uniform=TRUE,  main="Regreesion Tree Based on Composite Score")
  text(fit, use.n=FALSE, all=TRUE, cex=.75)
}

shinyServer(function(input, output) {
  subset_result <- result_library;

  output$coms_plot <- renderPlot({
    subset_result <- result_library
    
    subset_result$GPA_ACT = subset_result$GPA_RD*10 + subset_result$HIGHEST.ACT.COMP..SCORE
    xyCombo <- split(seq(1:nrow(subset_result)), list(subset_result$GPA_ACT), drop=FALSE)
    xyAward.df <- sapply(xyCombo, function(.indx){
      c(COMS=subset_result$GPA_RD[.indx][1]*10+subset_result$HIGHEST.ACT.COMP..SCORE[.indx][1],
        GPA=subset_result$GPA_RD[.indx][1],
        ACT=subset_result$HIGHEST.ACT.COMP..SCORE[.indx][1],
        Award=mean(subset_result$Allocate[.indx]) )         # total price paid  
    })  
    xyAward.df<- data.frame(t(xyAward.df))
    
    
    par(mfrow=c(2,1))
    plot(xyAward.df$Award~xyAward.df$COMS, xlab="Composite Score = 10 x GPA + ACT", ylab="Average Scholarship")
    #sunflowerplot(xyAward.df$COMS, xyAward.df$Award)
    
    
    library(rpart)  
    fit <- rpart(Award ~ COMS,  method="class", data=xyAward.df)  #grow a tree  
    #printcp(fit) # display the results 
    #plotcp(fit) # visualize cross-validation results 
    #summary(fit) # detailed summary of splits
    
    # plot tree 
    plot(fit, uniform=TRUE,  main="Regreesion Tree Based on Composite Score")
    text(fit, use.n=FALSE, all=TRUE, cex=.7)  
    
    #sunflowerplot(subset_result$GPA_RD, subset_result$HIGHEST.ACT.COMP..SCORE)
    
    #xyplot(xyAward.df$Award ~ ACT | GPA, data=xyAward.df, notch=TRUE)
    #pdf(paste("3dPlot.pdf"))
    #scatterplot3d(xyAward.df$Award ~ xyAward.df$GPA + xyAward.df$ACT)
    #dev.off()
    #contour(xyAward.df$Award ~ xyAward.df$GPA + xyAward.df$ACT)
    #plot(xyAward.df$Award)
    #plot3d(subset_result$GPA_RD, subset_result$HIGHEST.ACT.COMP..SCORE)
    
  })
  
  
  output$scholarship_table <- renderDataTable({
    subset_result <- result_library;
    subset_result <- subset(subset_result, subset_result$HIGHEST.ACT.COMP..SCORE == as.numeric(input$act) )
    subset_result <- subset(subset_result, subset_result$GPA_RD == as.numeric(input$gpa) )
#    subset_result <- subset(subset_result, subset_result$HS.COUNTY.TIER == input$tier )
#   subset_result <- subset(subset_result, subset_result$PellGrant == as.numeric(input$pell) )
    subset_result <- subset_result[c("HIGHEST.ACT.COMP..SCORE","GPA_RD","HS.COUNTY.TIER", "ETHNICITY.DESC",
                                     #"APP.INTENDING.COLLEGE.CODE",
                                     #"APP.MAJOR", 
                                     "PellGrant", "Allocate") ]    
  })
  
  output$gpa_act_table <- renderDataTable({
    subset_result <- result_library
    
    subset_result <- subset(subset_result, subset_result$HIGHEST.ACT.COMP..SCORE == as.numeric(input$act) )
    subset_result <- subset(subset_result, subset_result$GPA_RD == as.numeric(input$gpa) )
#    subset_result <- subset(subset_result, subset_result$HS.COUNTY.TIER == input$tier )
    
    subset_result$GPA_ACT = subset_result$GPA_RD*100 + subset_result$HIGHEST.ACT.COMP..SCORE
    xyCombo <- split(seq(1:nrow(subset_result)), list(subset_result$GPA_ACT), drop=FALSE)
    xyAward.df <- sapply(xyCombo, function(.indx){
      c(COMS=subset_result$GPA_RD[.indx][1]*10+subset_result$HIGHEST.ACT.COMP..SCORE[.indx][1],
        GPA=subset_result$GPA_RD[.indx][1],
        ACT=subset_result$HIGHEST.ACT.COMP..SCORE[.indx][1],
        Award=mean(subset_result$Allocate[.indx]) )           
    })
    
    xyAward.df<- data.frame(t(xyAward.df))
  })
  
  output$program_plot <- renderPlot({
    subset_result <- result_library
    subset_result <- subset(subset_result, subset_result$APP.MAJOR == input$program )
    
    enroll_coms_plot(subset_result)

    #par(mfrow=c(3,1))
    #sunflowerplot(subset_result$GPA_RD, subset_result$HIGHEST.ACT.COMP..SCORE)
    #plot(subset_result$Allocate ~ factor(subset_result$GPA_RD),)
    #plot(subset_result$Allocate ~ factor(subset_result$HIGHEST.ACT.COMP..SCORE),)
  })
  
  
  output$college_plot <- renderPlot({
    subset_result <- result_library
    subset_result <- subset(subset_result, subset_result$APP.INTENDING.COLLEGE.CODE == input$college )
    
    
    enroll_coms_plot(subset_result)
    
    #par(mfrow=c(2,1))
    #sunflowerplot(subset_result$GPA_RD, subset_result$HIGHEST.ACT.COMP..SCORE)
    #plot(subset_result$Allocate ~ factor(subset_result$GPA_RD),)
    #plot(subset_result$Allocate ~ factor(subset_result$HIGHEST.ACT.COMP..SCORE),)
    
    })
  

  output$ethnicity_plot <- renderPlot({
    subset_result <- result_library
    subset_result <- subset(subset_result, subset_result$ETHNICITY.DESC == input$ethnity )

    
    enroll_coms_plot(subset_result)
    
    #par(mfrow=c(2,1))
    #sunflowerplot(subset_result$GPA_RD, subset_result$HIGHEST.ACT.COMP..SCORE)
    #plot(subset_result$Allocate ~ factor(subset_result$GPA_RD),)
    #plot(subset_result$Allocate ~ factor(subset_result$HIGHEST.ACT.COMP..SCORE),)
  })

  
  output$tier_plot <- renderPlot({
    subset_result <- result_library
    subset_result <- subset(subset_result, subset_result$HS.COUNTY.TIER == input$tier )
    
    
    enroll_coms_plot(subset_result)
    
    #par(mfrow=c(2,1))
    #sunflowerplot(subset_result$GPA_RD, subset_result$HIGHEST.ACT.COMP..SCORE)
    #plot(subset_result$Allocate ~ factor(subset_result$GPA_RD),)
    #plot(subset_result$Allocate ~ factor(subset_result$HIGHEST.ACT.COMP..SCORE),)
  })

  output$cluster_plot <- renderPlot({
    subset_result <- result_library
    subset_result <- subset(subset_result, subset_result$HIGHEST.ACT.COMP..SCORE == as.numeric(input$act) )
    subset_result <- subset(subset_result, subset_result$GPA_RD == as.numeric(input$gpa) )

    #node <- sort(table(subset_result$Node),decreasing=TRUE)[1]
    node <- subset_result$Node[1]
    #THis is where the problem is 
    subset_result <- subset(result_library, subset_result$Node == node )
    
    
    enroll_coms_plot(subset_result)
    
    #par(mfrow=c(2,1))
    #sunflowerplot(subset_result$GPA_RD, subset_result$HIGHEST.ACT.COMP..SCORE)
    #plot(subset_result$Allocate ~ factor(subset_result$GPA_RD),)
    #plot(subset_result$Allocate ~ factor(subset_result$HIGHEST.ACT.COMP..SCORE),)
  })
  
  output$complete_table <- renderDataTable({
    subset_result <- result_library;
    subset_result <- subset_result[c("HIGHEST.ACT.COMP..SCORE","GPA_RD","HS.COUNTY.TIER", "ETHNICITY.DESC",
                                     #"APP.INTENDING.COLLEGE.CODE",
                                     #"APP.MAJOR", 
                                     "PellGrant", "Allocate") ]    
  })
  
  
})


