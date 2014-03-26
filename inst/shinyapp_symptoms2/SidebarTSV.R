
  # If missing input, return to avoid error later in function
  
     print("Preparing renderUI")
     if(is.null(input$dataFileTxt)) return()
  
      
      data <-  read.csv(input$dataFileTxt$datapath, header=TRUE, sep="\t")
      names.data=names(data)
      setNames(names.data, 1:length(names.data))
      
      
      tagList(
        selectInput(inputId="PatientIDVar",
                    label="Variable with the unique patient identifier", 
                    choices=names.data),
        
        
        selectInput(inputId="DateIDVar",
                    label="Variable with the date of measurments", 
                    choices=names.data, selected=names.data[2]), 
        
        #possibly: add the format of the dates
        
        
        selectInput(inputId="MeasurementIDVar",
                    label="Variable with the order of measurments", 
                    choices=names.data, selected=names.data[3]), 
        
        checkboxGroupInput(inputId="SymptomsIDVar",
                           label="Symptoms variables", 
                           choices=names.data, selected=names.data[-c(1:3, 4)])
      )#end tagList
      
      #checkboxGroupInput(inputId="DateVar",
      #                   label="Variable with date of measurement", 
      #                   choices=names.data)
  
