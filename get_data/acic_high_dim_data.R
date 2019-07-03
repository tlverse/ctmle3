library(here)
library(dplyr)
## code to get all datasets corresponding to DGP


#lookup table for IDs and their DGP
id_table = read.csv(file = 'get_data/highDim_trueATE.csv')

## download all High D if you dare (3gb)

#file_names = list.files(path = "~/Documents/high_dimensional_datasets/", 
 #                       pattern = "*.CSV", ignore.case = T, full.names = F)

##now find matches (eventually if need be will make a function)
dgp_3 = as.character((id_table %>% filter(DGPid == 3) %>% select(filename))$filename)
dgp_11 = as.character((id_table %>% filter(DGPid == 11) %>% select(filename))$filename)
dgp_16 = as.character((id_table %>% filter(DGPid == 16) %>% select(filename))$filename)


dgp_3_files = file_names[match(dgp_3, gsub(".csv", "", file_names, ignore.case = T))]
dgp_11_files = file_names[match(dgp_11, gsub(".csv", "", file_names, ignore.case = T))]
dgp_16_files = file_names[match(dgp_16, gsub(".csv", "", file_names, ignore.case = T))]

## save matches to pull them later from future storage site
#save(dgp_3_files, file= "dgp_3_files.Rdata")
#save(dgp_11_files, file = "dgp_11_files.Rdata")
#save(dgp_16_files, file = "dgp_16_files.Rdata")

