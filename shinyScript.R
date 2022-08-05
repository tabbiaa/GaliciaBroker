library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)

canal="Analisis por Canal"
RangoFechas=paste0(min(PolizasConReservaoPagos$mesOcurrencia,
                       Polizas$mesOcurrencia),"-",max(PolizasConReservaoPagos$mesOcurrencia,
                                                      Polizas$mesOcurrencia))

canales=as.list(
  paste0(Polizas [,which(colnames(Polizas)=="DescCanalPoliza")] %>% distinct_all() %>% 
         pull(), "-"))
 
canales[[length(canales)]]=substr(canales[[length(canales)]],1,nchar(canales[[length(canales)]])-1) 


canalesconTot=Polizas [,which(colnames(Polizas)=="DescCanalPoliza")] %>% distinct_all() %>% 
  pull()

canalesconTot=c("Total",canalesconTot)

nP=Polizas %>% ungroup() %>% select(NroPoliza,NroInciso) %>% distinct_all() %>% nrow()

nPcS=PolizasConReservaoPagos %>%ungroup() %>%  select(NroPoliza,NroInciso) %>% distinct_all() %>% nrow()
nPsS=nP-nPcS

nPSsP=PagosMasReservasSinPoliza%>%ungroup() %>%  select(NroPoliza,NroInciso) %>% distinct_all() %>% nrow()

PolizaCanal=Polizas %>%ungroup() %>%  group_by(DescCanalPoliza) %>% summarise(Expuestos=sum(Expuestos),Prima_Devengada=sum(primaValuacion))
SiniestroCanal=PolizasConReservaoPagos %>%ungroup() %>%  group_by(DescCanalPoliza) %>% summarise(Siniestros=sum(importeAjustado))
PolizaySiniestroCanal=PolizaCanal %>% left_join(SiniestroCanal)
PolizaySiniestroCanal %<>%rbind(data.frame(DescCanalPoliza="Total",
                                           Expuestos=sum(PolizaySiniestroCanal$Expuestos),
                                           Prima_Devengada=sum(PolizaySiniestroCanal$Prima_Devengada),
                                           
                                           Siniestros=sum(PolizaySiniestroCanal$Siniestros) ) ) %>% 
  mutate(
    LR=paste0(round(Siniestros/Prima_Devengada,2)*100,"%"),
    Prima_Devengada=lapply( Prima_Devengada,separadorMiles),
    Siniestros=lapply( Siniestros,separadorMiles),
    Expuestos=lapply( Expuestos,separadorMiles))


ExpuestosMes=Polizas %>% ungroup() %>% group_by(mesOcurrencia) %>% summarise(Expuestos=sum(Expuestos),Prima=sum(primaValuacion))

frec_int_Mes=PolizasConReservaoPagos %>% ungroup() %>% group_by(Grupo2, mesOcurrencia) %>%
  summarise(siniestralidad=sum(importeAjustado),Q=sum(cant_Sin))

listaCobertura=PolizasConReservaoPagos %>% ungroup() %>% select(Grupo2) %>% distinct_all() %>% pull()
listaCobertura=c("Total Cartera",listaCobertura)

listaFechas=ExpuestosMes %>% ungroup() %>% select(mesOcurrencia) %>% distinct_all() %>% pull()


ui<-fluidPage(sidebarLayout(
  sidebarPanel (
  img(src="Logo.png"),
  br(),
  div(titlePanel(strong(h2("Analisis Siniestralidad"))),style="color:darkgreen"),
  br(),
  h4(canal),
  br(),
  h4(paste0("Canales: ")),
  (canales) ,
   br(),
   br(),
  (paste0("Rango de Fechas= ",RangoFechas)),
  br(),
  (paste0("Numero de Polizas: ",formatC(nP, format="f", big.mark=",", digits=0))),
  br(),
 (paste0("Numero de Polizas con siniestros: ",formatC(nPcS, format="f", big.mark=",", digits=0))),
 br(),
 (paste0("Numero de Polizas sin siniestros: ",formatC(nPsS, format="f", big.mark=",", digits=0))),
 br(),
 (paste0("Numero de Polizas en base de siniestros no join base poliza: ",formatC(nPSsP, format="f", big.mark=",", digits=0))),
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Totales Por Canal",
                         br(),
                         tableOutput("tableTotalesCanal")
                         
                         
                         ),
                tabPanel("Totales Producto",
                         br(),
                         selectInput("CanalSeleccionadoProducto",label=strong("Canal"),
                                     choices =canalesconTot ),
                         tableOutput("tableTotalesProducto")
                         
                         
                ),
                tabPanel("Analisis Graficos por Cobertura Mes",
                         
                         br(),
                         fluidRow(
                           column(5,selectInput("Cobertura",label=strong("Cobertura"),
                                     choices =listaCobertura )),
                        
                        column(3,   selectInput("MinDate",label=strong("Fecha Minima"),
                                     choices =listaFechas ,selected=min(listaFechas))),
                         column(3, selectInput("MaxDate",label=strong("Fecha Maxima"),
                                     choices =listaFechas,selected=max(listaFechas))
                                     )),
                        plotOutput("graficoSeveridad"),
                        plotOutput("graficoFrecuencia"),
                        plotOutput("graficoLR")
                         )
      
      
    )
  )
  ))

server<-function(input,output){
  
  output$tableTotalesCanal=renderTable(PolizaySiniestroCanal)
  
 baseProducto =reactive({
   armaTablaProductos(Polizas,input$CanalSeleccionadoProducto,PolizasConReservaoPagos)
  })
  
  output$tableTotalesProducto=renderTable(baseProducto())
   

    
  BasePlotsSeveridad=reactive({
    RealizaGraficos("Severidad",input$Cobertura,frec_int_Mes,ExpuestosMes,input$MinDate,input$MaxDate)
  })
  BasePlotsFrecuencia=reactive({
    RealizaGraficos("Frecuencia",input$Cobertura,frec_int_Mes,ExpuestosMes,input$MinDate,input$MaxDate)
  })
  BasePlotsLossRatio=reactive({
    RealizaGraficos("LR",input$Cobertura,frec_int_Mes,ExpuestosMes,input$MinDate,input$MaxDate)
  })
  
  
  output$graficoSeveridad=renderPlot(
    BasePlotsSeveridad()
  )
  output$graficoFrecuencia=renderPlot(
    BasePlotsFrecuencia()
  )
  output$graficoLR=renderPlot(
    BasePlotsLossRatio()
  )
   
  }

shinyApp(ui=ui,server=server)
#shinyApp(ui, server, options = list(launch.browser = TRUE))
