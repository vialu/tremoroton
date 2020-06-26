library(ggplot2)
# library(dplyr)
# library(tidyr)
library(gridExtra)
library(shiny)
library(plotly)
# library(stringr)
library(signal)
library(seewave)
library(markdown)
library("fftw")
library(pracma)

par(mar=c(1,1,1,1))

btFilter <- function(sign, low, high, srate, ord){
  nlow       <- low/(srate/2)
  nhigh      <- high/(srate/2)
  cutP       <- c(nlow,nhigh)
  filt       <- butter(ord, cutP, type = "pass")
  filtSignal <- filter(filt, sign)
  return(filtSignal) 
}

rectify <- function(x) {
  sqrt(x^2)
}

# font style
f <- list(
  family = "Arial",
  size = 18,
  color = "black")
# annotations
right_side <- list(
  text = "Right Side",
  font = f,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "right",
  align = "right",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

left_side <- list(
  text = "Left Side",
  font = f,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "right",
  align = "right",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

source("ccoh2.R") # modification of ccoh from "seewave packeage" adding a taper. 

#####################################_________UI____________###################################

ui <- navbarPage("Tremoroton",
                 
                 
                 #####################_______GENERAL_VIEW_____####################
                 tabPanel("General view", 
                          
                          fluidRow(column = 12,
                                   plotlyOutput("plot"), 
                                   hr(),         
                                   fluidRow(
                                     column( width = 2, offset = 2,
                                             numericInput(inputId = "rACC",
                                                          label   = "right ACC",
                                                          value   = 2  ),
                                             numericInput(inputId = "lACC",
                                                          label   = "left ACC",
                                                          value   = 5 )),
                                     column(width = 2, 
                                            # left
                                            numericInput(inputId = "rEXT",
                                                         label   = "right extensors",
                                                         value   = 3 ),
                                            numericInput(inputId = "lEXT",
                                                         label   = "left extensors",
                                                         value   = 6 )),
                                     column(width = 2, 
                                            # left
                                            numericInput(inputId = "rFLX",
                                                         label   = "right flexors",
                                                         value   = 4 ),
                                            numericInput(inputId = "lFLX",
                                                         label   = "left flexors",
                                                         value   = 7 )),
                                     column(width = 3,
                                            numericInput(inputId = "srate",
                                                         label = "Sampling rate in Hz",
                                                         value = 1000, 
                                                         min = 100),
                                            
                                            
                                            br(),
                                            fileInput(inputId = "file1", 
                                                      label = "Choose File",
                                                      accept = c(
                                                        "text/plain",
                                                        "text/csv",
                                                        "text/comma-separated-values,text/plain",
                                                        ".csv"))
                                            
                                     )
                                     
                                   )
                                   
                                   
                          )),
                 ###########################_________TIME_DOMAIN______#####################
                 tabPanel("Time domain",
                          
                          fluidRow(column = 12,
                                   
                                   plotlyOutput("plot_time"), 
                                   br(),
                                   br(),
                                   hr(),                  
                                   fluidRow(column(width = 4, offset = 1,
                                                   selectInput("chan_1","channel 1", c("Accelerometer right" = "ACC_r", 
                                                                                       "Accelerometer left" = "ACC_l", 
                                                                                       "Extensors right" = "EXT_r", 
                                                                                       "Extensors left" = "EXT_l", 
                                                                                       "Flexor right" = "FLX_r", 
                                                                                       "Flexor left" = "FLX_l"
                                                   ), selected = "Accelerometer right"),
                                                   selectInput("chan_2","channel 2", c("Accelerometer right" = "ACC_r", 
                                                                                       "Accelerometer left" = "ACC_l", 
                                                                                       "Extensors right" = "EXT_r", 
                                                                                       "Extensors left" = "EXT_l", 
                                                                                       "Flexor right" = "FLX_r", 
                                                                                       "Flexor left" = "FLX_l"
                                                   ) , selected = "Accelerometer left")),
                                            column(width = 4, offset = 1,
                                                   h4("There is a two-side scroll bar below the plots ")
                                                   
                                            ))
                                   
                                   
                          ) 
                          
                 ),
                 ##########_____________FRECUENCY_DOMAIN_____________####################
                 tabPanel("Frequency domain",
                          
                          fluidRow(column = 12,
                                   plotlyOutput("plot_frex"), 
                                   br(),
                                   br(),
                                   hr(),                  
                                   fluidRow(
                                     column(width = 2, offset = 5,
                                            actionButton("run_frex", "FFT transformation")
                                     ))
                                   
                          )),
                 ########______________COHERENCE________________#########################
                 tabPanel("Coherence",
                          
                          fluidRow(
                            column(10,
                                   plotlyOutput("plot_coh")),
                            column(2,
                                   textOutput("segment_size"),
                                   hr(),
                                   textOutput("frequency_resolution"),
                                   hr()
                            ),
                            
                            hr(), 
                            fluidRow(
                              column(width = 4, offset = 1,
                                     selectInput("chan1","channel 1", c("Accelerometer right" = "ACC_r", 
                                                                        "Accelerometer left" = "ACC_l", 
                                                                        "Extensors right" = "EXT_r", 
                                                                        "Extensors left" = "EXT_l", 
                                                                        "Flexor right" = "FLX_r", 
                                                                        "Flexor left" = "FLX_l"
                                     ), selected = "Accelerometer right"),
                                     selectInput("chan2","channel 2", c("Accelerometer right" = "ACC_r", 
                                                                        "Accelerometer left" = "ACC_l", 
                                                                        "Extensors right" = "EXT_r", 
                                                                        "Extensors left" = "EXT_l", 
                                                                        "Flexor right" = "FLX_r", 
                                                                        "Flexor left" = "FLX_l"
                                     ) , selected = "Accelerometer left")),
                              column( width = 4,
                                      numericInput("seg_size","window size as power of 2",value = 11)),
                              column( width = 2,
                                      actionButton("run_coh", "run coherence"))
                            )
                          ))
                 ,
                 
                 tabPanel("Spectrogram",
                          
                          fluidRow(column = 12,
                                   plotlyOutput("plot_spectr"), 
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                            
                                   br(),
                                   br(),
                                   hr(),                  
                                   fluidRow(
                                     column(width = 2, offset = 5,
                                            actionButton("run_Spectr", "Spectrogram")
                                     ))
                                   
                          )
                          
                 ),
                 
                 
                 
                 
                 ####################### Documentation #######################
                 tabPanel("Documentation",
                          fluidRow(
                            column(8
                                   ,includeMarkdown("Documentation.md")
                                   
                                   
                            ))
                          
                 )
                 
)



################################______SERVER________###############################################

server <- server <- function(input, output) {
  
  #### load data
  data <- eventReactive(input$file1,{
    
    req(input$file1)
    inFile    <- input$file1
    read.delim(inFile$datapath, header = F, sep = "")
  })
  
  
  
  ##### General view
  ext   <- reactive({dim(data())[1]}) # extention of data in pnts
  srate <- reactive({input$srate})   # to be specify
  tsec  <- reactive({ext()/srate()}) # Extention in seconds
  ttime <- reactive({seq(1/srate(),tsec(), by = 1/srate())}) #time vector
  # seg = 1:ext # to be specify
  frex  <- reactive({seq(0,srate()/2, length.out = floor(ext()/2 + 1))})
  
  # select ACC and EMG channels
  ACCch <- reactive({c(input$rACC, input$lACC)})
  EMGch <- reactive({c(input$rEXT,input$rFLX,input$lEXT,input$lFLX)})
  
  # 
  ACC  <- reactive({data()[,ACCch()]})
  EMG  <- reactive({data()[,EMGch()]})
  
  # Apply corresponding filters
  ACCf <- reactive({apply(ACC(),2,btFilter,2,20,srate(),3)})
  EMGf <- reactive({apply(EMG(),2,btFilter,20,350,srate(),3)})
  
  
  # rectify and smooth EMG
  rec_EMG <- reactive({apply(EMGf(),2,rectify)})
  
  # and smooth
  
  finEMG <- reactive({
    s1 <- apply(rec_EMG(), 2, smooth)
    s2 <- apply(s1,2,smooth)
    s2
  })
  
  

  
  
  data_time <- reactive({
    
    dt <- data.frame(ACCf(), finEMG())
    names(dt) <- c("ACC_r", "ACC_l", "EXT_r", "EXT_l", "FLX_r", "FLX_l" )
    dt
  })
  
  
  AC_r  <-  reactive({plot_ly( data_time(), x = ttime(), y = data_time()[,1])   %>% add_lines( name = "Right ACC", line = list(width = .5, color="blue")) %>% layout(annotations = right_side,yaxis = list( title = "ACC"))     })
  
  AC_l  <-  reactive({plot_ly( data_time(), x = ttime(), y = data_time()[,2])   %>% add_lines( name = "Left ACC", line = list(width = .5, color="blue")) %>% layout(annotations = left_side)     })
  
  EXT_r <-  reactive({plot_ly( data_time(), x = ttime(), y = data_time()[,3]) %>% add_lines( name = "Right Extensors", line = list(width = .5,color="green")) %>% layout(yaxis = list( title = "EMG ext"))  })
  FLX_r <-  reactive({plot_ly( data_time(), x = ttime(), y = data_time()[,4]) %>% add_lines( name = "Right Flexors", line = list(width = .5,color="purple")) %>% layout(xaxis = list( title = "time in seconds"),yaxis = list( title = "EMG flex"))})
  EXT_l <-  reactive({plot_ly( data_time(), x = ttime(), y = data_time()[,5]) %>% add_lines( name = "Left  Extensors", line = list(width = .5,color="green"))})
  FLX_l <-  reactive({plot_ly( data_time(), x = ttime(), y = data_time()[,6]) %>% add_lines( name = "Left  Flexors", line = list(width = .5,color="purple")) %>% layout(xaxis = list( title = "time in seconds"))})
  
  output$plot <- renderPlotly({subplot(AC_r(),AC_l(),EXT_r(),EXT_l(),FLX_r(),FLX_l(), nrows = 3, shareX = T, shareY = F, titleX = T, titleY = T) %>% layout(showlegend =F) })
  
  ##### time domain
  dat_1 <- reactive(data_time()[,input$chan_1])
  dat_2 <- reactive(data_time()[,input$chan_2])
  # 
  t1 <- reactive({plot_ly(x = ttime(), y = dat_1()) %>% add_lines( name = input$chan_1, line = list(width = .5))%>% rangeslider()  })
  t2 <- reactive({plot_ly(x = ttime(), y = dat_2()) %>% add_lines( name = input$chan_2, line = list(width = .5))  })
  
  output$plot_time <-renderPlotly({subplot(t1(),t2(),nrows = 2, shareX = T, shareY = T, titleX = T) })
  
  
  ###### Frex Domain
  
  frex_data <- eventReactive(input$run_frex,{
    
    # get analitic signal
    ana <- matrix(nrow = ext(),ncol = 6)
    
    for (ti in 1:6)
    {
      ana[,ti] = fft(data_time()[,ti]/ext(),inverse = FALSE)
    }
    
    # get power spectra
    
    pow <- matrix(nrow = ext()/2 + 1, ncol = 6)
    
    for (ti in 1:6)
    {
      pow[,ti] = abs(ana[1:(ext()/2 + 1), ti])^2
    }
    pow
  })
  
  # selecting frequencies of intrest
  maxFrex  <- reactive(which.min(abs(frex() - 25)) )
  minFrex  <- reactive(which.min(abs(frex() - 2)) )
  
  
  frex_data1 <- reactive(frex_data()[minFrex():maxFrex(),])
  pfrex    <- reactive(frex()[minFrex():maxFrex()]) #  x axis frequencies
  
  # smoothing data
  
  frex_data2 <- reactive(
    apply(frex_data1(),2,smooth)
  )
  frex_data3 <- reactive(as.data.frame(frex_data2()))
  
  # get maximum for scaling plots
  
  
  
  AC_r_f  <-  reactive({plot_ly( frex_data3(), x = pfrex(), y = frex_data3()[,1]) %>% 
      add_lines( name = "Right ACC", line = list(width = 1, color="blue"))         %>% layout(annotations = right_side, yaxis = list( title = "ACC")   )})
  AC_l_f  <-  reactive({plot_ly( frex_data3(), x = pfrex(), y = frex_data3()[,2]) %>% 
      add_lines( name = "Left ACC", line = list(width = 1, color="blue"))          %>% layout(annotations = left_side)})
  EXT_r_f <-  reactive({plot_ly( frex_data3(), x = pfrex(), y = frex_data3()[,3]) %>% 
      add_lines( name = "Right Extensors", line = list(width =1, color="green")) %>% layout(yaxis = list( title = "EMG ext"))   })   
  FLX_r_f <-  reactive({plot_ly( frex_data3(), x = pfrex(), y = frex_data3()[,4]) %>% 
      add_lines( name = "Right Flexors", line = list(width = 1, color="purple"))     %>% layout(xaxis = list( title = "frequency in Hz"),yaxis = list( title = "EMG flex"))})
  EXT_l_f <-  reactive({plot_ly( frex_data3(), x = pfrex(), y = frex_data3()[,5]) %>% 
      add_lines( name = "Left  Extensors", line = list(width = 1, color="green"))})  
  FLX_l_f <-  reactive({plot_ly( frex_data3(), x = pfrex(), y = frex_data3()[,6]) %>% 
      add_lines( name = "Left  Flexors", line = list(width = 1, color="purple"))     %>% layout(xaxis = list( title = "frequency in Hz"))})
  
  output$plot_frex <- renderPlotly({subplot(AC_r_f(),AC_l_f(),EXT_r_f(),EXT_l_f(),FLX_r_f(),FLX_l_f(), nrows = 3, titleX = T,titleY = T, shareX = T) %>% layout(showlegend = FALSE) })
  
  
  
  ###########  run coherence
  
  
  
  segSize <- reactive({(2^input$seg_size)})
  n_seg   <- reactive(floor(ext()/segSize()))
  
  
  # frex2 <- reactive({cohe()[[1]]$freq})
  dat1 <- reactive(data_time()[,input$chan1])  
  dat2 <- reactive(data_time()[,input$chan2])
  
  cohSel <- eventReactive(input$run_coh,{
    cc  <- ccoh2(dat1(),dat2(), srate(), wl = segSize(), plot = F)
    cc
  })
  
  coh2pl <- reactive(apply(cohSel()$coh,2,mean))
  
  maxFrex2  <- reactive(which.min(abs(cohSel()$freq - (25/1000))) )
  minFrex2  <- reactive(which.min(abs(cohSel()$freq - (2/1000))) )
  
  cfrex       <- reactive({cohSel()$freq[minFrex2():maxFrex2()]})
  
  coh2plot <- reactive({ coh2pl()[minFrex2():maxFrex2()] })
  # confidence interval
  
  co        <- reactive({1 - 0.05^(1/(n_seg() - 1))}) 
  
  Frequency <- reactive(cfrex()*1000)
  Coherence <- reactive(coh2pl()[minFrex2():maxFrex2()])
  
  ch_c95up <- reactive({tanh(atanh(sqrt(coh2pl()[minFrex2():maxFrex2()])) + (1.96/sqrt(2*n_seg())))^2})
  ch_c95lo <- reactive({tanh(atanh(sqrt(coh2pl()[minFrex2():maxFrex2()])) - (1.96/sqrt(2*n_seg())))^2})
  
  
  coherence <- reactive({data.frame(Frequency(), coh2plot(),ch_c95up(),ch_c95lo(),co()) })
  
  p <- reactive({ggplot(coherence(), aes(Frequency(),coh2plot())) + geom_line()+
      geom_ribbon(aes(ymin=ch_c95lo(), ymax=ch_c95up(), x = Frequency()), alpha = 0.3)+
      geom_hline(yintercept = co(),linetype = "dashed", color = "red")+
      scale_color_discrete( labels = c("Coherence", "95% confidence interval","95% confident limit"))+
      scale_colour_manual("",values = "blue") +
      scale_fill_manual("",values = "grey12") +
      labs(x = "Frequency in Hz", y = "Coherence")
  })
  
  
  
  
  output$plot_coh <- renderPlotly({ggplotly(p())})
  
  output$segment_size <- renderText({paste("The size of the window is ", segSize(), "pnts","(",round(segSize()/srate(),digits = 2)," seconds)")})
  output$frequency_resolution <- renderText({paste("The frequency resolution is  ", round(srate()/(segSize()), digits = 2), "Hz")})
  
  ######Spectrogram #####
  
  winL <- reactive(2^(nextpow2(srate()))) #window length to use
  
  spec_data <- eventReactive(input$run_Spectr,{
    spi <- list()
    
    for (ti in 1:6){
      d1 <- spectro(data_time()[,ti],wl= winL(), srate(),ovlp = 50, fftw = T)
      
      spi[[ti]] <- d1[["amp"]]
      
    }
    spi
    
  })
  
  stime <- reactive({
    d1 <- spectro(data_time()[,1],wl = winL(), srate(),ovlp = 50,fftw = T)
    t1 <- d1[["time"]]
    t1
  })
  
  sfreq <- reactive({
    d1 <- spectro(data_time()[,1], wl = winL(), srate() ,ovlp = 50, fftw = T)
    t1 <- d1[["freq"]]*1000
    t1
  })
  
  # 
  
  indFr <- reactive(which(abs(sfreq()-25)==min(abs(sfreq()-25))))
  
  # 
  
  
  AC_r_s  <-  reactive({plot_ly(x =stime(), y = sfreq()[1:indFr()], z = spec_data()[[1]][1:indFr(),], type = "contour",ncontours = 100,contours = list(coloring = 'fill',showlines =F)) %>% layout(annotations = right_side, yaxis = list( title = "ACC")) }) 
  AC_l_s  <-  reactive({plot_ly(x =stime(), y = sfreq()[1:indFr()], z = spec_data()[[2]][1:indFr(),], type = "contour",ncontours = 100,contours = list(coloring = 'fill',showlines =F)) %>% layout(annotations = left_side) }) 
  EXT_r_s <-  reactive({plot_ly(x =stime(), y = sfreq()[1:indFr()], z = spec_data()[[3]][1:indFr(),], type = "contour",ncontours = 100,contours = list(coloring = 'fill',showlines =F)) %>% layout(yaxis = list( title = "EMG ext"))}) 
  FLX_r_s <-  reactive({plot_ly(x =stime(), y = sfreq()[1:indFr()], z = spec_data()[[4]][1:indFr(),], type = "contour",ncontours = 100,contours = list(coloring = 'fill',showlines =F)) %>% layout(yaxis = list( title = "EMG flex"))}) 
  EXT_l_s <-  reactive({plot_ly(x =stime(), y = sfreq()[1:indFr()], z = spec_data()[[5]][1:indFr(),], type = "contour",ncontours = 100,contours = list(coloring = 'fill',showlines =F)) }) 
  FLX_l_s <-  reactive({plot_ly(x =stime(), y = sfreq()[1:indFr()], z = spec_data()[[6]][1:indFr(),], type = "contour",ncontours = 100,contours = list(coloring = 'fill',showlines =F)) }) 
  
  
  output$plot_spectr <- renderPlotly({subplot(AC_r_s(),AC_l_s(),EXT_r_s(),EXT_l_s(),FLX_r_s(),FLX_l_s(), nrows = 3, titleX = T, titleY = T,shareX = T) %>% layout(showlegend = FALSE) })
  
  
  # output$plot_spectr <- renderPlotly({ AC_r_s() })
  
  output$test <- renderDataTable(sfreq())
  
}



##########################_______RUN________################################
shinyApp(ui, server)

