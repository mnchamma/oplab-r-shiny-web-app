####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/

library(httr)
library(shiny)
library(hash)
library(shinythemes)
library(bslib)
library(thematic)
library(infotheo)

thematic::thematic_shiny()
thematic::thematic_on()
data(airquality)

### INSIRA O SEU TOKEN ENTRE ASPAS ''
token <- '' 

if(token == ''){
  print('TOKEN INVÁLIDO')
  stopifnot(token != '')
}


ativo <- 'PETR4'
from <- '20140101'
to <- '20221001'


### FUNÇÕES PARA CALCULAR INDICADORES DOS PAINÉIS

var_stdv_amostra <- function(lista_indicador){
  ## Mostra a dispersão das observações de uma variável em torno de sua média
  n <- length(lista_indicador) ## tamanho da amostra
  soma <- sum(lista_indicador)
  media = soma/n
  soma_var = 0
  for (i in lista_indicador) {
    soma_var = soma_var + (i - media)**2
  }
  var = soma_var / (n-1)
  ## STDV ou desvio padrão é uma medida derivada da variância, tornando mais simples 
  ## a interpretação da dispersão em torno da média
  stdv = var**0.5
  return(c(var,stdv,sd(lista_indicador)))
}

std_movel_lista <- function(lista_indicador,periodos){
  lista_calculo = c()
  if (length(lista_indicador) <= periodos || periodos <= 0){
    print('Problemas no tamanho da amostra!!!')
    return(0)
  } else {
    for (j in 1:periodos){
      lista_calculo = c(lista_calculo,NaN)
      #print(j)
    }
    print(length(lista_indicador))
    #print(lista_indicador[1:26])
    contador = 1
    while (contador+periodos <= length(lista_indicador)){
      amostra = lista_indicador[as.integer(contador):as.integer(contador+periodos)]
      indicador = sd(amostra)
      lista_calculo <- c(lista_calculo,indicador)
      contador = contador + 1
    }
    return(lista_calculo)
  }
}

return_lista <- function(lista_indicador){
  lista_calculo = c(0)
  if (length(lista_indicador) <= 2){
    print('Problemas no tamanho da amostra!!!')
    return(0)
  } else {
    contador = 1
    while (contador < length(lista_indicador)){
      indicador = (lista_indicador[as.integer(contador+1)] / lista_indicador[as.integer(contador)])-1
      lista_calculo <- c(lista_calculo,indicador)
      contador = contador + 1
    }
    return(lista_calculo)
  }
}


### FUNÇÕES PARA COLETAR OS DADOS NA API

get_dados <- function(token,ativo){
  concatenated <- paste("https://api.oplab.com.br/v3/market/historical/",ativo,"/1d?from=",from,"&to=",to,"?smooth=true",sep="")
  r <- GET(concatenated,add_headers("Access-Token"=token), encode = "json")
  
  dados <- content(r)$data
  close_list <- c()
  
  for (i in dados){
    close_list = c(close_list,i$close)
  }
  return (close_list)
    
}

get_fundamentos <- function(token,ativo){
  concatenated <- paste("https://api.oplab.com.br/v3/market/stocks/",ativo,"?with_financials=bpp,dre,dfc",sep="")
  r <- GET(concatenated,add_headers("Access-Token"=token), encode = "json")
  dados <- content(r)$financial
  datas_list <- c()
  passivo_cirq <- c()
  dre_faturamento <- c()
  dre_ebit <- c()
  dfc_ger_caixa <- c()
  bpp <- hash(dados$bpp)
  dre <- hash(dados$dre)
  dfc <- hash(dados$dfc)

  for (k in keys(bpp)){
    datas_list = c(datas_list,k)
    passivo_cirq = c(passivo_cirq,bpp[[k]]$'2_01'$value)
    dre_faturamento = c(dre_faturamento,dre[[k]]$'3_01'$value)
    dre_ebit = c(dre_ebit,dre[[k]]$'3_05'$value)
    dfc_ger_caixa = c(dfc_ger_caixa,dfc[[k]]$'6_05'$value)
  }
  dic = hash()
  dic['datas'] <- datas_list
  dic['2_01'] <- passivo_cirq
  dic['3_01'] <- dre_faturamento
  dic['3_05'] <- dre_ebit
  dic['6_05'] <- dfc_ger_caixa
  return (dic)
  
}


# FUNÇÕES PARA DEFINIR UI PRO APP E PLOTAR GRÁFICOS ----
ui <- fluidPage(
  # Themes
  theme = bslib::bs_theme(
    bg = "#002B36", fg = "#EEE8D5", primary = "#2AA198",
    # bslib also makes it easy to import CSS fonts
    base_font = bslib::font_google("Ubuntu")
  ),
  
  # App title ----
  titlePanel("INVESTING TOOLS - ASSET PROFILE"),
  
  fluidRow(
    
      column(3,
             wellPanel(
               # Input: Slider for the number of bins ----
               textInput(inputId = 'ativo',
                         label = "Ativo para pesquisar:",'PETR4')
             )       
      ),    
      column(5,offset = 3,
             wellPanel(
               # Input: Slider for the number of bins ----
               sliderInput(inputId = "bins",
                           label = "Number of bins:",
                           min = 1,
                           max = 100,
                           value = 50)
             )       
      ),      
      column(6,
            plotOutput(outputId = "linePlot")
      ),
      column(6,
             plotOutput(outputId = "distPlot")
      ),
      column(3,
             plotOutput(outputId = "funds1Plot")
      ),
      column(3,
             plotOutput(outputId = "funds2Plot")
      ),   
      column(3,
             plotOutput(outputId = "funds3Plot")
      ),  
      column(3,
             plotOutput(outputId = "funds4Plot")
      )    
      
        
  )  
  

)

### DIFININDO ORDEM LÓGICA NO SERVIDOR PARA PLOTAR
server <- function(input, output) {
  

  output$distPlot <- renderPlot({
    x    <- na.omit(get_dados(token,input$ativo))
    xx   <- return_lista(x)
    xx    <- na.omit(xx)
    bins <- seq(min(xx), max(xx), length.out = input$bins + 1)
    
    hist(xx, breaks = bins, col = "#2AA198", border = "#EEE8D5",
         xlab = "Preços",
         main = paste("Histogram de preços",input$ativo))
    
  })
  
  output$linePlot <- renderPlot({
    
    y    <- get_dados(token,input$ativo)
    y    <- na.omit(y)
    datas_list <- c(1:length(y))
    
    
    plot(datas_list,y,type = 'l',col = 'white',xlab = "Preços X Tempo",
         main = paste("Linha de preços",input$ativo))
    
  })
  
  output$funds1Plot <- renderPlot({
    d    <- get_fundamentos(token,input$ativo)
    y    <- values(d['2_01'])
    datas_list <- c(1:length(y))
    plot(datas_list,values(d['2_01']),type = 'l',
         main = paste("Passivo_Circ",input$ativo))
  })  
  output$funds2Plot <- renderPlot({
    d    <- get_fundamentos(token,input$ativo)
    y    <- values(d['3_01'])
    datas_list <- c(1:length(y))
    plot(datas_list,values(d['3_01']),type = 'l',
         main = paste("Faturamento",input$ativo))
  })  
  output$funds3Plot <- renderPlot({
    d    <- get_fundamentos(token,input$ativo)
    y    <- values(d['3_05'])
    datas_list <- c(1:length(y))
    plot(datas_list,values(d['3_05']),type = 'l',
         main = paste("EBIT",input$ativo))
  })  
  output$funds4Plot <- renderPlot({
    d    <- get_fundamentos(token,input$ativo)
    y    <- values(d['6_05'])
    datas_list <- c(1:length(y))
    plot(datas_list,values(d['6_05']),type = 'l',
         main = paste("Caixa",input$ativo))
  })    
  
}

### CRIAR SHINY APP
shinyApp(ui = ui, server = server)
