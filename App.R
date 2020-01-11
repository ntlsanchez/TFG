library(shiny);library(foreign);library(forestplot);library(grid);library(foreign)
library(survival);library(KMsurv);library(ggplot2);library(survminer);library(shinydashboard)
library(shinydashboardPlus);library(flexdashboard);library(plotly);library(stringr);
library(DT);library(crosstalk);library(quantmod);library(openxlsx);library(lubridate)#paquete para fechas;
library(rlang);library(purrr);library(magrittr);library(dplyr);library(ggfortify);
library(nortest);library(epitools)


##Función que te devuelve TRUE si está vacío, NULL o NA
IsNullOrEmpty <- function(s) {
  return(is.null(s) || is.na(s) || s == "" || rlang::is_empty(s))
}

AnyNullOrEmpty <- function(...) {
  list(...) %>% some(IsNullOrEmpty)
}

NoneEmpty <- function(...) {
  !AnyNullOrEmpty(...)
}

##################
##Carga de datos##
##################

dat <- read.spss("TRC_TFG.sav", to.data.frame = TRUE)
dat <- as.data.frame(dat)

##Eliminamos columnas que no necesitamos##
dat <- dat[,-c(1,20,21,24,26:35)]

##0=alive,  1=dead: Se define la variable de forma binaria para que fincione la función Surv##
dat$VIU_O_MORT_lf <- ifelse(dat$VIU_O_MORT_lf=="VIU",0,1)

##Validación de la base de datos##
suma_de_NA <- sum(is.na(dat)) 
#Se observa que hay 1 NA por lo que al solo haber 1, podemos omitirlo
dat <- na.omit(dat)

dataset <- as.data.frame(dat)

##Categorizamos la edad##
dataset$Edat_Cat[dataset$EDAT<62]<- "[21,62)"
dataset$Edat_Cat[dataset$EDAT>=62 & dataset$EDAT<68]<- "[62,68)"
dataset$Edat_Cat[dataset$EDAT>=68 & dataset$EDAT<74]<- "[68,74)"
dataset$Edat_Cat[dataset$EDAT>=74]<- "[74,86]"
dataset$Edat_Cat <- as.factor(dataset$Edat_Cat)

##Se definen las varibales numéricas que categorizará el usuario##
dataset$crea_Cat <- rep(0,nrow(dataset))
dataset$fe_Cat<- rep(0,nrow(dataset))
dataset$fgr_Cat<- rep(0,nrow(dataset))
dataset$qrs_Cat<- rep(0,nrow(dataset))


###Definir la clase de cada variable###
#Variables factor:#
dataset$SEXE <- as.factor(dataset$SEXE)
dataset$CF  <- as.factor(dataset$CF)
dataset$CF_4  <- as.factor(dataset$CF_4)
dataset$BBE  <- as.factor(dataset$BBE)
dataset$ISQUEMIC  <- as.factor(dataset$ISQUEMIC)
dataset$INFART  <- as.factor(dataset$INFART)
dataset$FA  <- as.factor(dataset$FA)
dataset$TV  <- as.factor(dataset$TV)
dataset$MODE  <- as.factor(dataset$MODE)
dataset$COMPLICACIONS  <- as.factor(dataset$COMPLICACIONS)
dataset$COMPLICACIONS_CAT  <- as.factor(dataset$COMPLICACIONS_CAT)
dataset$SUCCES_IMPLANT  <- as.factor(dataset$SUCCES_IMPLANT)
dataset$Edat_Cat <-as.factor(dataset$Edat_Cat)
dataset$crea_Cat <-as.factor(dataset$crea_Cat)
dataset$fe_Cat<- as.factor(dataset$fe_Cat)
dataset$fgr_Cat<-as.factor(dataset$fgr_Cat)
dataset$qrs_Cat<-as.factor(dataset$qrs_Cat)
dataset$END_POINT_1 <- as.factor(dataset$END_POINT_1)
dataset$TRANSPLANT_lf <- as.factor(dataset$TRANSPLANT_lf)

##Variables numéricas##
dataset$EDAT <- as.numeric(dataset$EDAT)
dataset$FE <- as.numeric(dataset$FE)
dataset$CREA <- as.numeric(dataset$CREA)
dataset$FGR <- as.numeric(dataset$FGR)
dataset$QRS <- as.numeric(dataset$QRS)

##Se recodifica la variable COMPLICACIONS en si o no, para un futuro análisis##
dataset$Com <- ifelse(dataset$COMPLICACIONS=="NO","Sense complicacions", "Amb complicacions")
dataset$Com <- as.factor(dataset$Com)


##################
########UI########
##################

ui <- dashboardPagePlus(skin = "green",
        header = dashboardHeaderPlus(
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "gears"
      ),
  dashboardSidebar(
    sidebarMenu(##Barra lateral izquierda 
      menuItem("Base de datos", tabName = "Basededatos", icon=icon("database")),
      menuItem("Análisis descriptivo", tabName = "Descriptivos",  icon=icon("chart-line"),
               menuSubItem("Descriptiva univariante", tabName="uni"),
               menuSubItem("Descriptiva bivariante", tabName="biv"),
               menuSubItem("Kaplan i Meier",  tabName ="KM1"),
               menuSubItem("Curva de supervivencia", tabName="Sup")),
      menuItem("Modelo de Cox", tabName = "Cox",
              menuSubItem("Selección de variables", tabName="SV"),
              menuSubItem("Validación de supuestos",  tabName ="VS"),
              menuSubItem("Grafico del modelo Cox", tabName="Plot_Cox")),
               
      menuItem("Analisis de las complicaciones", tabName = "AnalisisC"),hr(),
      sliderInput("edat",
                  "Edat_Cat",
                  min = min(dataset$EDAT),
                  max = max(dataset$EDAT),
                  value = 42),
      sliderInput("crea",
                  "crea_Cat",
                  min = min(dataset$CREA),
                  max = max(dataset$CREA),
                  value = 1),
      sliderInput("fe",
                  "fe_Cat",
                  min = min(dataset$FE),
                  max = max(dataset$FE),
                  value = 25),
      sliderInput("fgr",
                  "fgr_Cat",
                  min = min(dataset$FGR),
                  max = max(dataset$FGR),
                  value = 198),
      sliderInput("qrs",
                  "qrs_Cat",
                  min = min(dataset$QRS),
                  max = max(dataset$QRS),
                  value = 206))),
    dashboardBody(##Cuerpo de la página
    tabItems(
      tabItem(tabName = "Basededatos", mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Contextualizacion",hr(),p("Se analiza una cohorte de pacientes en el ámbito de la cardiología. Concretamente, se
                    estudiará una cohorte de pacientes con una terapia de Resincronización Cardiaca (TRC). Los
                    pacientes de resincronización cardiaca, son una tipología de pacientes que tienen una mala
                    sincronización en el funcionamiento de las cavidades del corazón. Un dispositivo de
                    resincronización cardiaca es un dispositivo que tiene como objetivo volver a sincronizar las
                    contracciones de los ventrículos del corazón mediante estimulaciones eléctricas artificiales. Se
                    estudiará diferentes aspectos que son de interés en la práctica clínica diaria y se hará un balance
                    de la experiencia de nuestro centro al largo de los años. Por otro lado, se dispondrá de una base
                    de datos con 671 pacientes operados entre el año 1999 y 2015 en la unidad de arritmias del
                    Hospital Clínico de Barcelona. Por lo que respecta a las técnicas utilizadas, se centrará en un
                    análisis de supervivencia, modelos lineales entre otros.")),
                    tabPanel("Tabla de datos", icon = icon("table"),dataTableOutput("table1"),dataTableOutput("table2")),
                    tabPanel("Variables", hr(), h2(strong("Lista de variables")), 
                             p(strong("SEXE:"),"(HOME, DONA) Sexo del individuo."),
                             p(strong("EDAT"),": Edad del individuo."),
                             p(strong("FE:")," Es una variable numérica, que indica la fracción de eyección, es decir, es una medida del porcenaje de sangre que sale del corazón cada vez que se contrae."),
                             p(strong("CF:")," Indica la clase funcional, en otras palabras, es la capacidad de movilidad que tiene la persona."),
                             p(strong("CREA:")," Creatinina. La insuficiencia renal es un importante factor pronóstico en pacientes con insuficiencia cardiaca. Para valorar la función renal se suelen utilizar las cifras de creatinina."),
                             p(strong("FGR:")," Es el filtrado glomerular, esto es el volumen de fluido filtrado por unidad de tiempo desde los capilares glomerulares renales hacia el interior de la cápsula de Bowman. Normalmente se mide en mililitros por minuto."),
                             p(strong("BBE:")," Bloqueo de rama izquierda."),
                             p(strong("ISQUEMIC:")," Si el individuo presenta isquemia cardiaca."),
                             p(strong("INFART:")," Si ha tenido un infarto."),
                             p(strong("FA:")," Fibrilación auricular."),
                             p(strong("TV:")," Si ha presentado una taquicardia en la parte superior."),
                             p(strong("QRS:")," El complejo QRS es la representación gráfica de la despolarización de los ventrículos del corazón formando una estructura picuda en el electrocardiograma."),
                             p(strong("MODE:")," Indica si la TRC se hace sola (TRC-P) o la TRC se le añade desfibrilador (TRC-D)."),
                             p(strong("COMPLICACIONS:")," Existencia de complicaciones en el proceso del tratamiento."),
                             p(strong("VIU_O_MORT_lf:")," Indicador de si ha pasado el evento de interés."),
                             p(strong("TEMPS_KM_EP1:")," Tiempo que pasa dentro del estudio."))))),
      tabItem(tabName = "KM1", fluidRow(valueBoxOutput("pval"), box( solidHeader = TRUE,title="Grafico de Kaplan i Meier",
            collapsible = TRUE,status = "success",selectInput(inputId = "s", 
            label = "Select Stratification Variable:",choices = c(names(dataset)),selected = "SEXE"),
            plotOutput(outputId = "km")))),
      tabItem(tabName = "uni", box(status = "primary", solidHeader = TRUE,title="Descriptiva univariante",
            collapsible = TRUE,selectInput(inputId = "n", label = "Seleccione una variable:",
            choices = c(names(dataset)),selected = "SEXE"),
            plotlyOutput(outputId = "univ")),box(status = "warning", solidHeader = TRUE,title="Estadísticos principales",
            dataTableOutput("summary")),
              fluidRow(
                box(status="danger",
                  width = 12,
                  title = "Click or select on the plot to fill the table!",
                  plotOutput('seleccionable',
                             click = "user_click",
                             brush = "user_brush"),
                  dataTableOutput("tabla_seleccionable")))),
      
      tabItem(tabName = "biv",h2("Análisis bivariante"),fluidRow(column(6,
                    selectInput(inputId = "x", label = "Selecciona una variable para el eje x:",
                    choices = names(dataset),selected = "SEXE")),column(6,
                    selectInput(inputId = "y", label = "Selecciona una variable para el eje y:",
                    choices = names(dataset),selected = "EDAT"))),
                    fluidRow(plotlyOutput("plot_P")),hr(),
              box(status="primary",title="Variable X", verbatimTextOutput("lillie_X")),
              box(status="primary",title="Variable Y",verbatimTextOutput("lillie_Y")),
              
              fluidRow(
                box(solidHeader = TRUE,status="primary",title="Tabla de contingencia",
                    verbatimTextOutput("tabla_factor")),infoBoxOutput("corre")),
              fluidRow(
                box(status="danger",
                    width  = 12,
                    title = "Click or select on the plot to fill the table!",
                    plotOutput('seleccionable2',
                               click = "user_click",
                               brush = "user_brush"),
                    dataTableOutput("tabla_seleccionable2")))),
                
      tabItem(tabName = "Sup",h4("Curva de supervivencia"), plotOutput(outputId = "plot_sup")),
              
      tabItem(tabName = "AnalisisC",uiOutput("indepe"),
              fluidRow(
              box(status="danger",solidHeader = TRUE,title="Summary modelo GLM",verbatimTextOutput("summary_glm")), 
              box(status="warning", solidHeader = TRUE,title="Intervalo de confianza",verbatimTextOutput("confint_glm")), 
              box(status="primary",solidHeader = TRUE,title="Exp(Coeficientes)",verbatimTextOutput("exp_glm"))),
              fluidRow(h3("Odds Ratio(OR) y Riesgo relativo(RR)"),box(status="warning",dataTableOutput("OR")),
                       box(status="warning",dataTableOutput("RR")))),
      tabItem(tabName = "SV",
              uiOutput("independent"),
              fluidRow(box(status="danger",solidHeader = TRUE,title="Summary modelo Cox",verbatimTextOutput("sum1")), 
              box(status="warning", solidHeader = TRUE,title="Supuesto de riesgos proporcionales",verbatimTextOutput("RP")), 
              box(status="primary",solidHeader = TRUE,title="Coeficientes del modelo de Cox",dataTableOutput("tabla_cox")))
              ),
      tabItem(tabName="Plot_Cox", plotOutput("grafico_cox")),
      
      tabItem(tabName = "VS", mainPanel(
        tabsetPanel(type = "tabs",#Residuos del modelo de Cox
                    tabPanel("Residuos Schoenfeld",hr(), plotOutput("cox_resi")),
                    tabPanel("Linealidad", icon = icon("table"),hr(),plotOutput("plot_l")),
                    tabPanel("Residuos beta", hr(),plotOutput("plot_beta"))))))),
    rightsidebar= rightSidebar(#Barra lateral derecha
        background = "dark",
        rightSidebarTabContent(
          id = "right_sidebar_tab_1",
          icon = "filter",
          active = TRUE,
          uiOutput("right_menu"))))
              
              
##################
######Server######
##################

server <- function(input, output, session){
  #Se crea el filtro de la base de datos
  dim_SEXE <- dataset %>% distinct(SEXE) %>% as.data.frame() %>% c("All", .)
  dim_EDAT   <- dataset %>% distinct(EDAT) %>% as.data.frame() %>% c("All", .)
  dim_FE   <- dataset %>% distinct(FE) %>% as.data.frame() %>% c("All", .)
  dim_CF <- dataset %>% distinct(CF) %>% as.data.frame() %>% c("All", .)
  dim_CF_4 <- dataset %>% distinct(CF_4) %>% as.data.frame() %>% c("All", .)
  dim_CREA <- dataset %>% distinct(CREA) %>% as.data.frame() %>% c("All", .)
  dim_FGR <- dataset %>% distinct(FGR) %>% as.data.frame() %>% c("All", .)
  dim_BBE <- dataset %>% distinct(BBE) %>% as.data.frame() %>% c("All", .)
  dim_ISQUEMIC <- dataset %>% distinct(ISQUEMIC) %>% as.data.frame() %>% c("All", .)
  dim_INFART <- dataset %>% distinct(INFART) %>% as.data.frame() %>% c("All", .)
  dim_FA <- dataset %>% distinct(FA) %>% as.data.frame() %>% c("All", .)
  dim_TV <- dataset %>% distinct(TV) %>% as.data.frame() %>% c("All", .)
  dim_QRS <- dataset %>% distinct(QRS) %>% as.data.frame() %>% c("All", .)
  
  selected_CREA <- reactive({
    ifelse(is.null(input$CREA), "All", input$CREA)
  })
  
  selected_FGR <- reactive({
    ifelse(is.null(input$FGR), "All", input$FGR)
  })
  
  selected_BBE <- reactive({
    ifelse(is.null(input$BBE), "All", input$BBE)
  })
  
  selected_ISQUEMIC <- reactive({
    ifelse(is.null(input$ISQUEMIC), "All", input$ISQUEMIC)
  })
  
  selected_INFART <- reactive({
    ifelse(is.null(input$INFART), "All", input$INFART)
  })
  
  selected_FA <- reactive({
    ifelse(is.null(input$FA), "All", input$FA)
  })
  
  selected_TV <- reactive({
    ifelse(is.null(input$TV), "All", input$TV)
  })
  
  selected_QRS <- reactive({
    ifelse(is.null(input$QRS), "All", input$QRS)
  })
  
  selected_SEXE <- reactive({
    ifelse(is.null(input$SEXE), "All", input$SEXE)
  })
  
  selected_EDAT <- reactive({
    ifelse(is.null(input$EDAT), "All", input$EDAT)
  })
  
  selected_FE <- reactive({
    ifelse(is.null(input$FE), "All", input$FE)
  })
  
  selected_CF <- reactive({
    ifelse(is.null(input$CF), "All", input$CF)
  })
  
  selected_CF_4 <- reactive({
    ifelse(is.null(input$CF_4), "All", input$CF_4)
  })
  
  bajas<- reactive({
    
    dataset$Edat_Cat <- ifelse(dataset$EDAT < input$edat, paste0("Edat<",input$edat), paste0("Edat>=",input$edat))
    dataset$Edat_Cat <- as.factor(dataset$Edat_Cat)
    
    dataset$crea_Cat <- ifelse(dataset$CREA < input$crea, paste0("Crea<",input$crea), paste0("crea>=",input$crea))
    dataset$crea_Cat <- as.factor(dataset$crea_Cat)
    
    dataset$fe_Cat <- ifelse(dataset$FE < input$fe, paste0("FE<",input$fe), paste0("FE>=",input$fe))
    dataset$fe_Cat <- as.factor(dataset$fe_Cat)
    
    dataset$fgr_Cat <- ifelse(dataset$FGR < input$fgr, paste0("FGR <",input$fgr), paste0("FGR>=",input$fgr))
    dataset$fgr_Cat <- as.factor(dataset$fgr_Cat)
    
    dataset$qrs_Cat <- ifelse(dataset$QRS < input$qrs, paste0("QRS <",input$qrs), paste0("QRS>=",input$qrs))
    dataset$qrs_Cat <- as.factor(dataset$qrs_Cat)
    
    req(NoneEmpty(input$SEXE, input$EDAT, input$FE, 
                  input$CF,input$CF_4, input$CREA, input$FGR, input$BBE, input$ISQUEMIC, 
                  input$INFART, input$FA, input$TV, input$QRS))
    data <- dataset
    
    if (selected_SEXE() != "All") {
      data %<>% filter(SEXE == selected_SEXE())
    }
    
    if (selected_EDAT() != "All") {
      data %<>% filter(EDAT == selected_EDAT())
    }
    
    if (selected_FE() != "All") {
      data %<>% filter(FE == selected_FE())
    }
    
    if (selected_CF() != "All") {
      data %<>% filter(CF == selected_CF())
    }
    
    if (selected_CF_4() != "All") {
      data %<>% filter(CF_4 == selected_CF_4())
    }
    
    if (selected_CREA() != "All") {
      data %<>% filter(CREA == selected_CREA())
    }
    
    if (selected_FGR() != "All") {
      data %<>% filter(FGR == selected_FGR())
    }
    
    if (selected_BBE() != "All") {
      data %<>% filter(BBE == selected_BBE())
    }
    
    if (selected_ISQUEMIC() != "All") {
      data %<>% filter(ISQUEMIC == selected_ISQUEMIC())
    }
    
    if (selected_INFART() != "All") {
      data %<>% filter(INFART == selected_INFART())
    }
    
    if (selected_FA() != "All") {
      data %<>% filter( FA== selected_FA())
    }
    
    if (selected_TV() != "All") {
      data %<>% filter(TV == selected_TV())
    }
    
    if (selected_QRS() != "All") {
      data %<>% filter(QRS == selected_QRS())
    }
    
    return(data)
  })
  
  
  ## Right sidebar
  output$right_menu <- renderMenu({
    tagList(
      selectInput(
        "SEXE",
        "SEXE",#titulo
        dim_SEXE,
        selected = isolate(selected_SEXE())
      ),
      selectInput(
        "EDAT",
        "EDAT",#titulo
        dim_EDAT,
        selected = isolate(selected_EDAT())
      ),
      selectInput(
        "FE",
        "FE",
        dim_FE,
        selected = isolate(selected_FE())
      ),
      selectInput(
        "CF",
        "CF",
        dim_CF,
        selected = isolate(selected_CF())
      ),
      selectInput(
        "CF_4",
        "CF_4",
        dim_CF_4,
        selected = isolate(selected_CF_4())
      ),
      selectInput(
        "CREA",
        "CREA",
        dim_CREA,
        selected = isolate(selected_CREA())
      ),
      selectInput(
        "FGR",
        "FGR",
        dim_FGR,
        selected = isolate(selected_FGR())
      ),
      selectInput(
        "BBE",
        "BBE",
        dim_BBE,
        selected = isolate(selected_BBE())
      ),
      selectInput(
        "ISQUEMIC",
        "ISQUEMIC",
        dim_ISQUEMIC,
        selected = isolate(selected_ISQUEMIC())
      ),
      selectInput(
        "INFART",
        "INFART",
        dim_INFART,
        selected = isolate(selected_INFART())
      ),
      selectInput(
        "FA",
        "FA",
        dim_FA,
        selected = isolate(selected_FA())
      ),
      selectInput(
        "TV",
        "TV",
        dim_TV,
        selected = isolate(selected_TV())
      ),
      selectInput(
        "QRS",
        "QRS",
        dim_QRS,
        selected = isolate(selected_QRS())
      )
    )
    
  })
  
  ##Código para la tabla de la que al seleccionar sale la tabla correspondiente
  interaction_type <- "click"
  observeEvent(input$user_click, interaction_type <<- "click")
  observeEvent(input$user_brush, interaction_type <<- "brush")
  
  
  output$seleccionable <- renderPlot({
    variable1 <- input$s
    B <- bajas()
    B %>%
      ggplot(aes(!!as.name(input$n), VIU_O_MORT_lf)) +
      geom_jitter(width = 0.25, aes(colour =  VIU_O_MORT_lf)) +
      theme(legend.position = "none") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1,
                                   size=14, face = "bold"),
        axis.text.y = element_text(size=14, face = "bold")) + 
      xlab("Grupo tarifa")
  })
  
  
  brush_data <- reactive({
    B <- bajas()
    user_brush <- input$user_brush
    user_click <- input$user_click
    if (interaction_type == "brush") {
      brushedPoints(B, user_brush)
    } else if (interaction_type == "click") {
      nearPoints(B, user_click, threshold = 10)
    }
  })
  
  output$tabla_seleccionable <- renderDT({
    df <- brush_data()
    req(nrow(df) > 0)
    df %>% datatable(
      rownames = FALSE,
      colnames = colnames(dataset),
      selection = "single",
      extensions = "Buttons",
      filter = "top",
      options = list(
        dom = "Blfrtip",
        scrollX = TRUE
        ,buttons = list("copy", list(
          extend = "collection",
          buttons = c("csv", "excel", "pdf"),
          text = "Download"))))
  })
  
  
  variable <- reactive({
    variable1 <- input$n
      })
  
  output$summary <- renderDataTable({
    B <- bajas()
    variable1 <- input$n
    if(class(B[[variable1]])=="factor"){
    a <- B[[variable1]] %>% table()
    b <- B[[variable1]] %>% table() %>% prop.table() 
    b <- round(b,2)
    c <- as.data.frame(t(rbind(a,b)))
    colnames(c) <- c("N","%")
    
    return(c)
    }else{
      dataset
      s_dat <- as.matrix(summary(B[[variable1]]))
      s_dat <- round(as.data.frame(t(s_dat)),2)
      return(s_dat)
    }
  })
    
  
  output$cox_resi <- renderPlot({
    mod0 <- runRegression()
    riesgo0 <- cox.zph(mod0)
    #Riesgos proporcionales
    ggcoxzph(riesgo0)#Schoenfeld
  })
  
  output$plot_l <- renderPlot({
    mod0 <- runRegression()
   #Linealidad
   ggcoxdiagnostics(mod0, type = "schoenfeld", ox.scale = "time")
  })
  
  output$plot_beta <- renderPlot({
    mod0 <- runRegression()
    #Residuales Dfbeta
    ggcoxdiagnostics(mod0, type = "dfbeta")
  })
  
  output$univ <- renderPlotly({
    B <- bajas()
    variable1 <- variable()
    
    if(class(B[[variable1]])=="factor"){
      B <- bajas()
   
      B %>%
        group_by(!!as.name(variable1)) %>%
        summarise(models=n()) %>%
        ungroup %>%
        plot_ly() %>%
        
        add_bars(
          x = as.formula(paste0("~`", variable1, "`")),
          y = ~models,
          name = "2019",
          yaxis = "y2",
          marker = list(
            color = "rgba(226, 0, 116, 0.2)",
            line = list(color = "rgb(226, 0, 116, 0.1)", width = 0.9)))
      }else{
      plot_ly(B, x = B[[variable1]], type="box",mode = 'markers') }
         
 })
 
  output$table1 <- renderDataTable({
    B <- bajas()
   head(B[,1:12],4)
  })
  
  output$table2 <- renderDataTable({
    B <- bajas()
    head(B[,c(13:15,18:21)],4)
  })

  output$km <- renderPlot({
    B <- bajas()
    kmdata <-  surv_fit(as.formula(paste('Surv(TEMPS_KM_EP1,VIU_O_MORT_lf)~',input$s)), data=B)
    autoplot(kmdata)
  })
  
  output$pval <- renderValueBox({
    B <- bajas()
    sdf <- survdiff(as.formula(paste('Surv(TEMPS_KM_EP1,VIU_O_MORT_lf)~',input$s)), data=B,rho=0)
    
    infoBox(
      "Test Log-Rank",round(1-pchisq(sdf$chisq,length(sdf$n)-1),3), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE)
  })
  
  
  runSur <- reactive({
    B <- bajas()
    survfit(as.formula(paste("Surv(TEMPS_KM_EP1,VIU_O_MORT_lf) ~ ",paste("1"))),data=B)
  })
  
  # Plot the survival graph
  output$plot_sup <- renderPlot({
    cosa <- runSur()
    autoplot(runSur(),xlab="Days", ylab="S(t)", surv.colour = 'orange')
  })
  
  output$independent <- renderUI({
    checkboxGroupInput("independent", "Independent Variables:",choices =  names(dataset)[2:14])
  })
  
  runRegression <- reactive({
    B <- bajas()
    coxph(as.formula(paste("Surv(TEMPS_KM_EP1,VIU_O_MORT_lf)~",paste0(input$independent,collapse="+"))),data=dataset)
  })
  
  
  output$sum1 <- renderPrint({
    if(!is.null(input$independent)){
      summary(runRegression())
    } else {
      print(data.frame(Warning="Debe seleccionar al menos 1 variable."))
    }
  })
  
  
  output$RP <- renderPrint({
    if(!is.null(input$independent)){
      mod0 <- runRegression()
      riesgo0 <- cox.zph(mod0)
      round(riesgo0$table[,3],2)
    } else {
      print(data.frame(Warning="Debe seleccionar al menos 1 variable."))
    } 
  })
  
  
  output$tabla_cox <- renderDataTable({
    if(!is.null(input$independent)){
      mod0 <- runRegression()
      smod1 <- summary(mod0)
      coxcoe <- as.data.frame(round(smod1$coefficients[,2],3))
      colnames(coxcoe) <- c("exp(Coef)")
      coxcoe
    } else {
      print(data.frame(Warning="Debe seleccionar al menos 1 variable."))
    }
    
  })
  
  
  output$plot_P <- renderPlotly({
    B <- bajas()
    X <- input$x
    Y <- input$y
    if((class(B[[Y]])=="numeric") && (class(B[[X]])=="numeric")){
     p <-  plot_ly(B, x =~B[[X]], y=~B[[Y]])
      return(p)
    }else{
      if((class(B[[Y]])=="factor") && (class(B[[X]])=="factor")){
        pp_f <- B %>% ggplot(data = B, mapping=aes(x =!!as.name(X), fill =!!as.name(Y)))+
          geom_bar(position = "fill") 
      return(ggplotly(pp_f))
      }else{
        return(plot_ly(B, x =~B[[X]], y=~B[[Y]], type="box",mode = 'markers'))
      }}
      })
  
  
  
  output$lillie_X <- renderPrint({
    B <- bajas()
    X <- input$x
    Y <- input$y
    if(class(B[[X]])=="numeric"){
      return(lillie.test(B[[X]]))
    }else{return(paste0("Esta variable es categórica"))}
  })
  
  output$lillie_Y <- renderPrint({
    B <- bajas()
    X <- input$x
    Y <- input$y
    
    if(class(B[[Y]])=="numeric"){
      return(lillie.test(B[[Y]]))
    }else{return(paste0("Esta variable es categórica"))}
    
  })
  
  
  output$corre <- renderInfoBox({
    B <- bajas()
    X <- input$x
    Y <- input$y
    
    if((class(B[[X]])=="numeric") && (class(B[[Y]])=="numeric")){
      co<- infoBox(
        "Correlación",round(cor(B[[X]],B[[Y]], method = "spearman"),3), icon = icon("thumbs-up", lib = "glyphicon"),
        color = "yellow", fill = TRUE)#utilizo spearman porque no se distribuyen como una normal
      return(co)
    }else{
      if((class(B[[X]])=="factor") && (class(B[[Y]])=="factor")){
      co <- infoBox(
        "Grado de asociación", round(chisq.test(table(B[[X]],B[[Y]]))$p.value,3) , icon = icon("thumbs-up", lib = "glyphicon"),
        color = "red", fill = TRUE)
      return(co)}
    else{
      pv <- wilcox.test(as.formula(paste(input$y,"~",input$x)),alternative="two.sided", data=B)
      pv<- round(pv$p.value,3)
      co <- infoBox(
      h6("Willcoxon Test"), paste(pv) , icon = icon("thumbs-up", lib = "glyphicon"),
      color = "red", fill = TRUE)
      return(co)
      }}
})

  
  output$tabla_factor <- renderPrint({
    B <- bajas()
    X <- input$x
    Y <- input$y
    
   if(class(B[[X]])=="factor" && class(B[[Y]])=="factor"){
    prop <- table(B[[X]],B[[Y]])
    return(prop)
   }else{return(paste0("Alguna variable es numérica"))}
  
    })
  
  
  output$seleccionable2 <- renderPlot({
    X <- input$x
    Y <- input$y
    B <- bajas()
    B %>%
      ggplot(aes(!!as.name(input$x), !!as.name(input$y))) +
      geom_jitter(width = 0.25, aes(colour =  !!as.name(input$y))) +
      theme(legend.position = "none") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1,
                                   size=14, face = "bold"),
        axis.text.y = element_text(size=14, face = "bold"))
  })
  
  
  brush_data2 <- reactive({
    B <- bajas()
    user_brush <- input$user_brush
    user_click <- input$user_click
    if (interaction_type == "brush") {
      brushedPoints(B, user_brush)
    } else if (interaction_type == "click") {
      nearPoints(B, user_click, threshold = 10)
    }
  })
  
  output$tabla_seleccionable2 <- renderDT({
    df <- brush_data()
    req(nrow(df) > 0)
    df %>% datatable(
      rownames = FALSE,
      colnames = colnames(dataset),
      selection = "single",
      extensions = "Buttons",
      filter = "top",
      options = list(
        dom = "Blfrtip",
        scrollX = TRUE
        ,buttons = list("copy", list(
          extend = "collection",
          buttons = c("csv", "excel", "pdf"),
          text = "Download"
        ))
      )
    )
  })
  
  output$grafico_cox <- renderPlot({
    mod0 <- runRegression()
    ggforest(mod0, data=dataset)
    
  })
  
  
  
  output$indepe <- renderUI({
    checkboxGroupInput("indepe", "Independent Variables:",choices =  names(dataset)[2:14])
  })
  
  runRegression_glm <- reactive({
    fit <- glm(as.formula(paste("Com~",paste0(input$indepe,collapse="+"))),data=dataset,family=binomial(link=logit))
  })
  
  output$summary_glm <- renderPrint({
  
    if(!is.null(input$independent)){
      fit <- runRegression_glm()
      summary(fit)
    } else {
      print(data.frame(Warning="Debe seleccionar al menos 1 variable."))
    }
    
  })
 
  output$confint_glm <- renderPrint({

    if(!is.null(input$independent)){
      fit <- runRegression_glm()
      confint(fit)
    } else {
      print(data.frame(Warning="Debe seleccionar al menos 1 variable."))
    }

  })
  # 95% CI for the coefficients
  output$exp_glm <- renderPrint({

    if(!is.null(input$independent)){
      fit <- runRegression_glm()
      exp(coef(fit))
    } else {
      print(data.frame(Warning="Debe seleccionar al menos 1 variable."))
    }
  })

  
  output$OR <- renderDataTable({
    OR0 <- oddsratio(dataset$SEXE,dataset$Com)
    a <- OR0$measure[2,]
    OR1 <- oddsratio(dataset$CF_4,dataset$Com)
    b <- OR1$measure[2,]
    OR2 <- oddsratio(dataset$INFART,dataset$Com)
    c <- OR2$measure[2,]
    OR3 <- oddsratio(dataset$MODE,dataset$Com)
    d <- OR3$measure[2,]
    OR4 <- oddsratio(dataset$ISQUEMIC,dataset$Com)
    e <- OR4$measure[2,]
    OR5 <- oddsratio(dataset$BBE,dataset$Com)
    f <- OR5$measure[2,]
    OR6 <- oddsratio(dataset$FA,dataset$Com)
    g <- OR6$measure[2,]
    OR7 <- oddsratio(dataset$TV,dataset$Com)
    h <- OR7$measure[2,]
    
    OR <- rbind(a,b,c,d,e,f,g,h)
    rownames(OR) <- c("SEXE(HOME)","CF_4(Sí)","INFART(Sí)", "MODE(TRC-P)", "ISQUEMIC(Sí)","BBE(Sí)","FA(Sí)","TV(Sí)")
    colnames(OR) <- c("OR", "IClow", "ICupper")
    
    return(round(OR,3))
  })
  
  output$RR <- renderDataTable({
    OR0 <- riskratio(dataset$SEXE,dataset$Com)
    a <- OR0$measure[2,]
    OR1 <- riskratio(dataset$CF_4,dataset$Com)
    b <- OR1$measure[2,]
    OR2 <- riskratio(dataset$INFART,dataset$Com)
    c <- OR2$measure[2,]
    OR3 <- riskratio(dataset$MODE,dataset$Com)
    d <- OR3$measure[2,]
    OR4 <- riskratio(dataset$ISQUEMIC,dataset$Com)
    e <- OR4$measure[2,]
    OR5 <- riskratio(dataset$BBE,dataset$Com)
    f <- OR5$measure[2,]
    OR6 <- riskratio(dataset$FA,dataset$Com)
    g <- OR6$measure[2,]
    OR7 <- riskratio(dataset$TV,dataset$Com)
    h <- OR7$measure[2,]
    
    
    OR <- rbind(a,b,c,d,e,f,g,h)
    rownames(OR) <- c("SEXE(HOME)","CF_4(Sí)","INFART(Sí)", "MODE(TRC-P)", "ISQUEMIC(Sí)","BBE(Sí)","FA(Sí)","TV(Sí)")
    colnames(OR) <- c("RR", "IClow", "ICupper")
    
    return(round(OR,3))
  })
}
    
shinyApp(ui, server)



