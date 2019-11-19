library(shiny)
library(shinythemes)


# Define UI for miles per gallon app ----
ui <- fluidPage(theme = shinytheme('slate'),
                
                # App title ----
                headerPanel("Synergy With Control Group"),
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                  
                  # Input: Selector for variable to plot against mpg ----
                  selectInput("color1", "Color Group 1:", 
                              c('Empty' = 'white', 
                                'Green' = 'green',
                                'Purple' = 'purple',
                                'Blue' = 'blue',
                                'Red' = 'red')),
                  
                  # Input for color
                  selectInput('color2', 'Color Group 2:',
                              c('Empty' = 'white', 
                                'Green' = 'green',
                                'Purple' = 'purple',
                                'Blue' = 'blue',
                                'Red' = 'red')),
                  
                  selectInput('color3', 'Color Group 3:',
                              c('Empty' = 'white', 
                                'Green' = 'green',
                                'Purple' = 'purple',
                                'Blue' = 'blue',
                                'Red' = 'red')),
                  
                  selectInput('color4', 'Color Group 4:',
                              c('Empty' = 'white', 
                                'Green' = 'green',
                                'Purple' = 'purple',
                                'Blue' = 'blue',
                                'Red' = 'red')),
                  # Input box for CSV file
                  fileInput("file1", "Input Your CSV File",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv"), buttonLabel = 'Browse', placeholder = 'No File Selected'
                  ),
                  tags$hr(),
                  # Does this have headers??
                  checkboxInput("header", "My File has Headers", TRUE),
                  # Download button
                  downloadButton('downloadImage', 'Download Image'),
                  # Disclaimer
                  hr(),
                  print('-----------disclaimer----------------')
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                  
                  # Output: Formatted text for caption ----
                  h3(textOutput("caption")),
                  
                  # Output: Plot of the requested variable against mpg ----
                  plotOutput("SynergyPlot")
                  
                )
)



# Data pre-processing ----
# Tweak the "am" variable to have nicer factor labels -- since this
# doesn't rely on any user inputs, we can do this once at startup
# and then use the value throughout the lifetime of the app
da=read.csv("ZR75.csv")
namgr=c("BYL","GSK","BYL & GSK","Independence")
tr=da[,1]
C=da[tr=="C",2];A=da[tr=="A",2];B=da[tr=="B",2];D=da[tr=="AB",2] #SFs
n0=length(C);n1=length(A);n2=length(B);n3=length(D)  #n
lC=log(C);lA=log(A);lB=log(B);lD=log(D)
y0=mean(lC);y1=mean(lA);y2=mean(lB);y3=mean(lD)
s0=var(lC)*(n0-1);s1=var(lA)*(n1-1);s2=var(lB)*(n2-1);s3=var(lD)*(n3-1)
sy=s0+s1+s2+s3
dft=n0+n1+n2+n3-4;denf=1/n0+1/n1+1/n2+1/n3
tss=(y1+y2-y3-y0)/sqrt(sum(sy)/dft)/denf	
pv=2*(1-pt(abs(tss),df=dft))
pvP=1-pt(tss,df=dft) # one-sided p-value
print("Testing for Bliss independence in the presence of the control group:")
print(paste("T-stat=",round(tss,3),", y1+y2-y3-y0=",round(y1+y2-y3,3),", exp(y1+y2-y3-y0)=",round(exp(y1+y2-y3-y0),3),", p-value=",round(pv,3),sep=""))

m=cbind(lC,lA,lB,lD)	
indep=rep(0,n1*n2*n3)
k=0
for(i1 in 1:n1)
  for(i2 in 1:n2)
    for(i3 in 1:n0)
    {
      k=k+1
      indep[k]=m[i1,2]+m[i2,3]-2*m[i3,1]
    }

lCA=rep(0,n1*n0);lCB=rep(0,n2*n0);lCD=rep(0,n3*n0)
k=0
for(i0 in 1:n0)
  for(i1 in 1:n1)
  {
    k=k+1
    lCA[k]=lA[i1]-lC[i0]
  }	

k=0
for(i0 in 1:n0)
  for(i1 in 1:n1)
  {
    k=k+1
    lCB[k]=lB[i1]-lC[i0]
  }	

k=0
for(i0 in 1:n0)
  for(i2 in 1:n2)
  {
    k=k+1
    lCB[k]=lB[i2]-lC[i0]
  }	

k=0
for(i0 in 1:n0)
  for(i2 in 1:n3)
  {
    k=k+1
    lCD[k]=lD[i2]-lC[i0]
  }



# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })
  
  # # Return the formula text for printing as a caption ----
  # output$caption <- renderText({
  #   formulaText()
  # })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$SynergyPlot <- renderPlot({
    par(mfrow=c(1,1),mar=c(4,4,1,1))
    
    yl=c(.1,.2,.4,.7,1);lyl=log(yl)
    boxplot(list(lCA,lCB,lCD,indep),names=namgr,xlab="",ylab="",ylim=range(lyl),axes=F, col = c(input$color1, input$color2, input$color3, input$color4))
    mtext(side=1,"Treatment groups",cex=1.25,line=2.75)
    mtext(side=2,"Surviving fraction from control, %",cex=1.25,line=2.5)
    axis(side=2,at=lyl,as.character(yl*100))
    axis(side=1,at=1:4,namgr)
    lines(x=c(3,3,4,4),y=c(log(.7),log(.8),log(.8),log(.7)),lwd=3)
    text(3.5,log(.88),paste("P-value =",round(pv,3)),font=2)
    yn=log(c(60,27,12,18)/100)
    text(1:4,yn,paste("n=",c(n1*n0,n2*n0,n3*n0,n0*n1*n2),sep=""),cex=1.25)
  })
}

shinyApp(ui, server)
