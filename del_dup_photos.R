#########################################
#   Things need to modified by user     # 
#########################################
# repeating sources in file names
dup_source <- c("(1)", "(2)", "(3)") 
# setting working Picture directory
mypath <- "C:/Users/lim23/Pictures"
#########################################

# setting working directory
setwd(mypath)

# get the sub-directories names
sub_direc <- list.files(mypath)

for (i in 1:length(sub_direc)){
  # form the sub-sub-file directory
  sub_sub_direc <- paste(mypath,"/",sub_direc[i], sep = "")
  temp <- list.files(sub_sub_direc)
  del <- vector()
  for(j in 1:length(temp)){
    for(k in 1:length(dup_source)){
      # decide which to delete based on repeat sources
      temp_temp <- grepl(dup_source[k], temp[j], fixed=TRUE)   
      if(length(temp_temp)!=0){
        del[k] <- temp_temp
        if(del[k]==TRUE){
          del_pp <- paste(mypath,"/",sub_direc[i],"/",temp[j], sep="")
          file.remove(del_pp)
          # print(del_pp)
        }
      }
    }
  }
}