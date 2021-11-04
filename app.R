library(shiny)
library(ggplot2)
library(plotly)
library(skimr)

## Loeme andmed, kustutame kõik read kus on puuduvaid andmeid
carClaims<-read.csv("data/car_insurance_claim.csv",sep = ",", na.strings=c("", "","NA"), header = T)
## Eemaldame ID, BIRTH ja CLAIM_FLAG tunnused. Kasutame ainult need andmed, kus CLM_AMT ei ole tühi
carClaims = carClaims[c(2,4:25,27)]
carClaims = na.omit(carClaims)
### Andmete puhastamine
## Konverteerime tüübid
carClaims <- type.convert(carClaims)
## Eemaldame kõik $ märgid andmestikust
indx <- sapply(carClaims, is.factor) 
carClaims[indx] <- lapply(carClaims[indx], function(x) as.factor(gsub("\\$", "", x)))
## Vahetame kõik komad "," punktide "." vastu
indx <- sapply(carClaims, is.factor) 
carClaims[indx] <- lapply(carClaims[indx], function(x) as.factor(gsub(",", "", x)))
## Eemaldame kõik keeldud nõuded (claim_amount=0)
carClaims<-carClaims[!(carClaims$CLM_AMT==0),]
## Uuendame tüübid uuesti, faktorid muutuvad arvulisteks tunnusteks
carClaims <- type.convert(carClaims)
## Eemaldame ekslikud andmed. Tühjad auto vanused ja negatiivsed auto vanused
carClaims<-carClaims[!(carClaims$CAR_AGE<1),]
## Sorteerime tegurid ja täisarvud erinevatesse andmestikutesse
carClaimsFactors <- carClaims[,c(6,8:11,13,16,17,20,24)]
carClaimsNums <- carClaims[,c(1:5,7,12,14,15,18,19,21:23)]

ui <- fluidPage(
     titlePanel("Autode kindlustusjuhtumite andmete visualiseerimine", windowTitle = "Kindlustusjuhtumid"),
     helpText("ITB8812 Andmete visualiseerimine (Virumaa).",br(),"Andmete visualiseerimise projekt.",br(), "Vladimir Andrianov, Vadim Aland"),
     br(),
     tabsetPanel(
         tabPanel("SISSEJUHATUS",
                  sidebarLayout(
                      sidebarPanel(img(src = "cars.jpg", height = 160, width = 250)),
                      mainPanel(
                                h1("Sissejuhatus:"),
                                br(),
                                p("Autorid valisid oma projekti jaoks andmestiku, mis sisaldab auto kindlustus juhtumite andmeid. Andmestik on võetud Kaggle.com veebilehe anmdestikude andmebaasist."),
                                br(),
                                h1("Projekti eesmärgid:"),
                                br(),
                                p("1. Visualiseerida andmeid."),
                                p("2. Kursuses 'Andmete visualiseerimine' omandatud meetodite ja teadmiste rakendamine."),
                                )
                                )
                  ),
         tabPanel(
           "ANDMED",
                  mainPanel(
                  verbatimTextOutput("summary"),
                  dataTableOutput("tabel"),),
           
                  ),
         tabPanel(
           "TUNNUSED",
           mainPanel(
                      h1("Tunnused:"),
                      p("INDEX - identification variable (do not use)"),
                      p("TARGET_FLAG - was car in a crash 1=yes 0=no"),
                      p("TARGET_AMT - if car was in a crash, what was the cost"),
                      p("AGE - age of driver"),
                      p("BLUEBOOK - value of vehicle"),
                      p("CAR_AGE - vehicle age"),
                      p(" CAR_TYPE - type of car"),
                      p("CAR_USE - vehicle use"),
                      p("CLM_FREQ - # claim past 5 years"),
                      p("  EDUCATION - max education level"),
                      p("HOMEKIDS - # children at home"),
                      p("   HOME_VAL - home value"),
                      p(" INCOME - income"),
                      p("   JOB - job cathegory"),
                      p("  KIDSDRIV - # driving children"),
                      p("   MSTATUS - marital status"),
                      p("MVR_PTS - motor vehicle record points"),
                      p("OLDCLAIM - total claims past 5 years"),
                      p("PARENT1 - single parent"),
                      p("RED_CAR - a red car"),
                      p("REVOKED - licence revoked past 7 years"),
                      p("SEX - gender"),
                      p("TIF - time in force"),
                      p("TRAVTIME - distance to work"),
                      p("URBANICITY - home/work area"),
                      p("YOJ - years on job")
                    )
                  ),

        tabPanel(
        "HISTOGRAAM",
        sidebarLayout(
          sidebarPanel(
              sliderInput(inputId = "bins",
                  label = "Tulpade arv visualiseerimiseks:",
                  min = 3,
                  max = 50,
                  value = 20)
              ),
          mainPanel(
            br(),
            p("Sellel paneelil kuvame liiklusõnnetuste tabamuste sagedust sõltuvalt vanusest"),
              plotOutput(outputId = "Histogram") 
                    )
                    )
                  ),
        tabPanel(
          "SÕLTUVUS 1",
          mainPanel(
            sidebarLayout(
              sidebarPanel(
                radioButtons(inputId = "HOMEKIDS",
                             label = "1. Vali lapse kogust:",
                             choices = c(0, 1, 2, 3, 4, 5)),
                selectInput(inputId = "CAR_TYPE",
                            label = "2. Vali auto tüüpi:",
                            choices = sort(unique(carClaims$CAR_TYPE)),
                            multiple = TRUE)
                          ),
              mainPanel(
                        br(),
                        p("Sellel paneelil kuvame õnnetuste arv, sõltuvalt auto tüübist ja laste arvust perekonnas. Tulpdiagrammi ehitamiseks valige vähemalt auto tüübi."),
                        plotOutput("hourlyPlot"),
                        tableOutput("employTable"))
                          )
          
            
                    )
                  ),
        tabPanel(
          "SÕLTUVUS 2",
          sidebarLayout(
            sidebarPanel(selectInput("var",
                                     label = "Vali tunnust :",
                                     choices = names(carClaimsNums[,-(13)]))),
            mainPanel(
              br(),
              p("Sellel paneelil kuvame õnnetuste sagedust sõltuvalt vasakult valitud kriteeriumist ning era- ja äritranspordi kontekstis."),
              plotlyOutput("distPlot", width = "80%",height="auto"))      
                        )
                ),
)
)
    
server <- function(input, output, session) {

  output$summary <- renderPrint({
    dataset <- carClaims
    skim(dataset)
  })

    output$tabel <- renderDataTable(
      carClaims,
      options =list(
            searching = FALSE,ordering=F, lengthMenu = c(5, 10, 20),
            pageLength = 5,scrollX = TRUE
            )
      )  
    
    output$distPlot <- renderPlotly({
      p <- ggplot(carClaims, aes_string(x = "CLM_AMT", y =input$var,
                                  colour = "CAR_USE",CAR_TYPE="CAR_TYPE")) +
        geom_point(data=carClaims,aes_string(x = "CLM_AMT", y =input$var,CAR_TYPE="CAR_TYPE"),
                   colour = "grey")+geom_point()+
        facet_wrap(~ CAR_USE)+
        theme(legend.position = "none")+
        scale_color_manual(values=c("#8B4513","#009FE3","#00983A","#FFDE00","#E30613","#82368C"))+
        xlab("CLM_AMT")
      ggplotly(p,tooltip = c("CLM_AMT",input$var,"CAR_TYPE"))
    })
 
    output$Histogram <- renderPlot({
      x <- carClaims$AGE
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, col = "#75AADB", border = "white",
           xlab = "Age",
           main = "Histogram of driver's age") })

    output$hourlyPlot <- renderPlot({
      carClaims %>%
        filter(HOMEKIDS == input$HOMEKIDS,
               CAR_TYPE %in% input$CAR_TYPE) %>%
        ggplot(aes(CLM_AMT)) +
        geom_histogram()
    }) 
}
shinyApp(ui = ui, server = server)