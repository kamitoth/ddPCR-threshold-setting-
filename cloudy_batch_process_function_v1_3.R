fu <- function(location=choose.dir(),
               dw = readline("Which well (default = all)?"),
               well = ifelse(dw != "",dw,  "all"), 
               ds = readline("What is the droplet volume (nL; default = 0.797 nL)?"),
               ds2 = as.numeric(unlist(ds)),
               dVol = ifelse(!is.na(ds2),ds2,  0.797), 
               channel = menu(c("Channel 1", "Channel 2"), title="Which channel?"),
               sV = readline("What is the sample volume (\u00B5L; default = 5 \u00B5L)?"),
               sV2 = as.numeric(unlist(sV)),
               sVol = ifelse(!is.na(sV2),sV2, 5),
               df = readline("What is the dilution factor?"),
               dilf = as.numeric(unlist(df))){
  source("https://raw.githubusercontent.com/kamitoth/ddPCR-threshold-setting-/main/read_QX_600mod.R")
  source("https://raw.githubusercontent.com/Gromgorgel/ddPCR/master/Cloudy-V3-07.R")
  # Read the data file. Change "directory" to the path to the Amplitude data. The script will automatically read all *.csv files in the folder. 
  cat("Select amplitude data folder!\n")
  data <- read.QX(location, nr.r = NA, autoname = TRUE)

  # selection of method based on the selected Channel
  # note: duplex and eva methods are not available with this function
  method <- if(channel == 1){method = 'simplex'} else{method = 'simplex2'}
  #calculating results either for a single or for all of the wells
  results <- cloudy(data, method = method, well = well, dVol = dVol, vec = T)
  #just to make sure that the sample volume is asked before the dilution factor
  sVol <- sVol
  # Creating row with total accepted droplets and dilution factor
  if(all(well == "all") && any(unlist(lapply(data, function(x) dim(x)[2] > 1))))  {
    #accepted <- ifelse(results["positive",]+results["negative",]<10000, "less than 10000 accepted droplets",results["positive",]+results["negative",])
    accepted <- results["positive",]+results["negative",]
    # Replace 1000 with your dilution factor
    # The code is slightly more complicated for multiple dilutions
    df <- rep(dilf, ncol(results))
    results <- rbind(results,df)
    ccp_diluted_sample <- round(results["targ.in.sVol",]/sVol,digits=0)
    ccp_pcr_solution <- round(results["targ.in.sVol",]/20, digits=0)
    #calculating copies per unit and adding it to results
    # change 5 to the actual volume of sample per well in ?L 
    cp_per_unit <- round(results["targ.in.sVol",]/sVol*results["df",], digits=0)
    warning <- ifelse(accepted < 10000, "less than 10000 accepted droplets","")
    results <- rbind(results, accepted, ccp_diluted_sample, ccp_pcr_solution, cp_per_unit, warning)
    results <- as.data.frame(t(results))
  }
  else{
    accepted <- results[,"positive"]+results[,"negative"]
    df <- rep(dilf, nrow(results))
    results <- cbind(results,df)
    ccp_diluted_sample <- round(results[,"targ.in.sVol"]/sVol,digits=0)
    ccp_pcr_solution <- round(results[,"targ.in.sVol"]/20, digits=0)
    cp_per_unit <- round(results[,"targ.in.sVol"]/sVol*results[,"df"], digits=0)
    warning <- ifelse(accepted < 10000, "less than 10000 accepted droplets","")
    results <- cbind(results, accepted=accepted, ccp_diluted_sample=ccp_diluted_sample, ccp_pcr_solution=ccp_pcr_solution, cp_per_unit=cp_per_unit, warning=warning)
    #results <- t(results) 
    cloudy(data, well = well, method = method, dVol = dVol, plots =TRUE)
  }
  
  results$position <- rownames(results)
  
  #The results can be looked in RStudio:
  View(results)
  
  #...copied to Excel
  
  #clipr::write_clip(results)
  
  #and saved as xlsx file one step above the 'amplitude data' folder
  dir <- setwd(location); setwd('..')
  if(!require(xlsx)){
    install.packages("xlsx") 
    library(xlsx)}
  cat("Select the folder and give a file name with xlsx extension to save the results! (e.g. results.xlsx)\n")
  file = file.choose()
  #file = choose.files(default = "results.xlxs", caption = "Select files", multi = F)
  xlsx::write.xlsx(results, file = file,  
                   col.names = TRUE, row.names = TRUE, append = FALSE)
  
  # the necessary data copied and pasted into form 512
  # These are the Well, Threshold level, Positive, Negative and Analysed partitions columns 
  if(!require(clipr)){
    install.packages("clipr") 
    library(clipr)}
  subset <- results[,c('position', 'threshold', 'positive', 'negative', 'accepted', 'targ.in.sVol', 'targ.lower', 'targ.upper',
                       'lambda', 'populations', '%rain', 'resolution', '%comparted', 'ccp_pcr_solution', 
                       'ccp_diluted_sample')]
  # if more than one well was analysed, reorder the wells as "A1, B1, C1..."
  if(nrow(subset) > 1){
    order <- order(paste(gsub("[^[:digit:]]", "", rownames(subset)), gsub("[[:digit:]]", "", rownames(subset))))
    subset <- subset[order,]
  }
  rownames(subset) <- NULL
  colnames(subset) <- NULL
  clipr::write_clip(subset)
  cat("You can paste the results into Form-512 with Ctrl + V\n")
}

fu()
