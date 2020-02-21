######################################################################################
## Code to parse PAM output data and return                                         ##
##  light curve data and regression parameters                                      ##
## Returns list: [[1]] = regression parameters, [[2]] = light curve data            ##
##--------------------------                                                        ##
##  Data types:                                                                     ##
## 'F'      SAT Normal operation and saturating flash                               ##
## 'FO'     Fo,Fm determination                                                     ##
## 'SICS'   Start of induction curve                                                ##
## 'SICE'   End of induction curve                                                  ##
## 'SLCS'   Start of rapid light curve                                              ##
## 'SLCE'   End of rapid light curve                                                ##
## 'REG1'   Parameters fit by regression 1 (Platt et al., 1980)                     ##
## 'REG2'   Parameters fit by regression 2 (Jassby and Platt, 1976)                 ##
##--------------------------                                                        ##
## Regression parameters:                                                           ##
## 'alpha'  initial slope of rapid light curve (RLC) ~ photosynthetic efficiency    ##
## 'ETRm'   Maximum electron transport rate [umol/m2/s of electrons]                ##
## 'Ek'     Minimum saturating irradiance [umol/m2/s of photons]                    ##
## (Note: Ek = ETRm/alpha)                                                          ##
## REG1 also returns two additonal parameters: 'beta' and 'ETRmPot'                 ##
## with ETRmPot/beta representing the 'photoinhibition index'                       ##
## (i.e., PAR needed to photoinhibit ETRmPot by the factor of 1/e)                  ##
##-------------------------                                                         ##
## Photoinduction curves plot ETR vs. PAR using the light curve data (SLCS to SLCE) ##
## Data types and regressions described in:                                         ##
## http://www.walz.com/downloads/manuals/junior-pam/jpm_071206.pdf                  ##
######################################################################################
## For reference, REG1 and REG2 equations are as follows:                           ##
## REG1                                                                             ##
##   ETR = ETRmPot*(1 - e^(-1*alpha*PPFD/ETRmPot))*(e^(-1*beta*PPFD/ETRmPot))       ##
##   with ETRm = ETRmPot*(alpha/(alpha+beta))*(beta/(alpha+beta))^(beta/alpha)      ##
## REG2                                                                             ##
##   ETR = ETRm*TANH(alpha*PPFD/ETRm)                                               ##
######################################################################################



ParsePAM_KBG <- function(nfile) {
  
##### READ IN DATA #####

   pamdat <- readLines(nfile) 
   n.reg1 <- grep("REG1",pamdat) #lines with regression1 output
   n.reg2 <- grep("REG2",pamdat) #lines with regression1 output
   n.slcs <- grep("SLCS",pamdat) #beginning of light curve
   n.slce <- grep("SLCE",pamdat) #end of light curve
   n.sics <- grep("SICS",pamdat) #beginning of induction curve
   n.sice <- grep("SICE",pamdat) #end of induction curve
   
   if (length(n.reg1)-length(n.slcs)!=0) {print("Number of regressions does not match number of light curves")}
   if (length(n.slcs)-length(n.slce)!=0) {print("Remove incomplete light curve at end of file and retry")} #; return()}
      


##### REGRESSION PARAMETERS ##### #####
   #first construct dataframe to hold values
   out.reg <- as.data.frame(matrix(NA,nrow=length(n.reg1),ncol=13))
   colnames(out.reg) <- c("Run","LN","Mem","Date","Time","REG1.alpha","REG1.ETRm","REG1.Ek","REG1.beta","REG1.ETRmPot",
      "REG2.alpha","REG2.ETRm","REG2.Ek")

   for (i in 1:length(n.reg1)) {
   
#      out.reg[i,"Run"] <- i
      out.reg[i,"LN"] <- n.reg1[i]
      
      tmp <- gsub("\"","",unlist(strsplit(pamdat[n.reg1[i]],";")))
      nn <- grep("[0-9]-[0-9]",tmp)
      out.reg[i,"Date"] <- tmp[max(nn)] #Will be 2 if 'Datetime' and 'Date' else 1
      nn <- grep("[0-9]:[0-9]",tmp)
      out.reg[i,"Time"] <- tmp[max(nn)] #Will be 2 if 'Datetime' and 'Time' else 1
      
      #REG1 parameters
      tmp <- unlist(strsplit(tmp[grep("alpha:",tmp,fixed=T)],",")) 
      tmp <- gsub(" ","",tmp) #remove white spaces (needed for negative values)
      if (!is.null(tmp)) {
         out.reg[i,"REG1.alpha"] <- as.numeric(gsub("#1:alpha:","",tmp[1]))
         out.reg[i,"REG1.ETRm"] <- as.numeric(gsub("ETRm:","",tmp[2]))
         #out.reg[i,"REG1.Ek"] <- as.numeric(gsub("\\( beta: .*","",gsub("Ek: ","",tmp[3])))
         out.reg[i,"REG1.ETRmPot"] <- as.numeric(gsub("\\)","",gsub("ETRmPot:","",tmp[4])))
         tmp <- unlist(strsplit(tmp[3],"(",fixed=TRUE))
         out.reg[i,"REG1.Ek"] <- as.numeric(gsub("Ek:","",tmp[1]))
         out.reg[i,"REG1.beta"] <- as.numeric(gsub("beta:","",tmp[2]))
      } 
      
      #REG2 parameters
      tmp <- gsub("\"","",unlist(strsplit(pamdat[n.reg2[i]],";")))
      tmp <- unlist(strsplit(tmp[grep("alpha:",tmp,fixed=T)],","))
      tmp <- gsub(" ","",tmp) #remove white spaces (needed for negative values)
      if (!is.null(tmp)) {
         out.reg[i,"REG2.alpha"] <- as.numeric(gsub("#1:alpha:","",tmp[1]))
         out.reg[i,"REG2.ETRm"] <- as.numeric(gsub("ETRm:","",tmp[2]))
         out.reg[i,"REG2.Ek"] <- as.numeric(gsub("Ek:","",tmp[3]))
      } 
   }


#### CREATE DATAFRAME FOR LIGHT CURVE VALUES ######
   #first construct dataframe to hold values
   
#values are between SCLS and SLCE - should be 9 lines (and may have REG1 and REG2, but not always)
   colnms <- gsub("\"","",unlist(strsplit(pamdat[grep("Date",pamdat)],";"))) #column headings
   out.rlc <- as.data.frame(matrix(NA,nrow=length(n.slcs)*9 + length(n.sics)*14,ncol=length(colnms)+4))
   colnames(out.rlc) <- c("Run","LN","Mem",colnms,"Curve") #Note: Fm_l = Fm' and Fo_l = Fo'

if(length(n.sics) > 0){
  out.rlc$Run <- c(rep(1:length(n.slcs),each=9), rep((length(n.slcs)+1):(length(n.slcs)+length(n.sics)),each=14))
} else {
  out.rlc$Run <- c(rep(1:length(n.slcs),each=9))
}



#### POPULATE DATA FRAME WITH LIGHT CURVES VALUES ####
   for (i in 1:length(n.slcs)) {
   
      n.ind <- which(out.rlc$Run==i) #dataframe indices for Run i
      out.rlc[n.ind,"LN"] <- n.slcs[i]

      n.rlc <- if (length(grep("\"FO\"",pamdat[n.slcs[i]+1],fixed=T)) > 0) n.slcs[i] else n.slcs[i]+2 #start of light curve
      
      for (j in 1:9) {

        if ((n.rlc+j < n.slce[i]) && (length(grep("\"FO\"",pamdat[n.rlc+j],fixed=T))>0 |length(grep("\"F\"",pamdat[n.rlc+j],fixed=T))>0)) {
          
          tmp <- gsub("\"","",unlist(strsplit(pamdat[n.rlc+j],";"))) #should have 21 elements
          if (length(tmp)!=length(colnms)) print(paste("Run",i,"does not have the correct number of elements!"))
          
          #Populate RLC values - should be elements 4:21 (or 23?)
          
          for (k in 1:length(colnms)) {
            
            if (colnms[k] %in% c("Datetime","Date","Time","Type","Mark","1:Fo'")) {
              out.rlc[n.ind[j],colnms[k]] <- tmp[k] #use as is
              
            } else {
              out.rlc[n.ind[j],colnms[k]] <- as.numeric(gsub(" ","",gsub(",*","",tmp[k])))
            }
         
            out.rlc[n.ind[j],"Curve"] <- "Light" 
          }
          
          out.rlc[n.ind[j],"Mem"] <- out.rlc[n.ind[1],"No."] #this should be "F0"
        }
            
      }  
   }


#### POPULATE DATA FRAME WITH INDUCTION CURVES VALUES ####

if(length(n.sics) > 0){

for (i in (length(n.slcs)+1):(length(n.slcs)+length(n.sics))) {
  
  n.ind <- which(out.rlc$Run==i) #dataframe indices for Run i
  out.rlc[n.ind,"LN"] <- n.sics[i - (length(n.slcs))]
               
               n.ilc <- if (length(grep("\"FO\"",pamdat[n.sics[i - (length(n.slcs))]+1],fixed=T)) > 0) n.sics[i - (length(n.slcs))] else n.sics[i - (length(n.slcs))]+2 #start of light curve
               
               for (j in 1:14) {
                 
                 if ((n.ilc+j < n.sice[i - (length(n.slcs))]) && (length(grep("\"FO\"",pamdat[n.ilc+j],fixed=T))>0 | length(grep("\"F\"",pamdat[n.ilc+j],fixed=T))>0)) {
                   
                   tmp <- gsub("\"","",unlist(strsplit(pamdat[n.ilc+j],";"))) #should have 21 elements
                   if (length(tmp)!=length(colnms)) print(paste("Run",i,"does not have the correct number of elements!"))
                   
                   #Populate ilc values - should be elements 4:21 (or 23?)
                   
                   for (k in 1:length(colnms)) {
                     
                     if (colnms[k] %in% c("Datetime","Date","Time","Type","Mark","1:Fo'")) {
                       out.rlc[n.ind[j],colnms[k]] <- tmp[k] #use as is
                       
                     } else {
                       out.rlc[n.ind[j],colnms[k]] <- as.numeric(gsub(" ","",gsub(",*","",tmp[k])))
                      
                     }
                     out.rlc[n.ind[j],"Curve"] <- "Induction"  
                   }
                   
                   out.rlc[n.ind[j],"Mem"] <- out.rlc[n.ind[1],"No."] #this should be "F0"
                 }
                 
               }  
               
}
}

## Clean up light/induction curve data frame 

#   out.rlc[out.rlc=="-"] <- NA #PAM uses '-' for NA, might need to replace '-' entries
   colnms <- gsub("1:","",colnms) #remove the "1:"
   colnms <- gsub(",*","",colnms) #sometimes Fv/Fm is followed by commas (this removes)
   colnames(out.rlc)[4:ncol(out.rlc)] <- c(colnms, "Curve")
 
## Reorder by Memory line

    out.rlc<-out.rlc[order(out.rlc$Mem),]


##### LINK CURVE AND REGRESSION DATA #####
   for (i in 1:nrow(out.reg)) {
   
      nn <- which(out.rlc$LN==out.reg$LN[i]-1)[1]
      out.reg$Run[i] <- out.rlc$Run[nn]
      out.reg$Mem[i] <- out.rlc$Mem[nn]
   
   }
  


##### OUTPUT VALUES INTO THE DATAFRAMES ####
   return(list("Regression parameters"=out.reg,"Light curve data"=out.rlc))


#### END FUNCTION #####
}


