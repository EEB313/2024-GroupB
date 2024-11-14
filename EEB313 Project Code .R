
#### EEB313 Project ####

#### calculating diversity per site ####

all_insect_data <- read.csv("Arthropod.List.csv")


library(vegan)
library(plyr)
library(tidyverse)

#new_insect <- all_insect_data[-1] %>% 
  #t() %>% 
  #as.data.frame() %>% 
 # rownames_to_column(var="Plots") %>% 
  
  new_insect <- all_insect_data %>% 
    gather(var, val, 2:ncol(data)) %>% 
    spread(Series.Description, val)
  

New_insect <- new_insect[-c(1,2,3), ]


write.csv(New_insect, '/Users/gabi/Downloads/New_insect.csv')

test_data <- read.csv("final_arthro.csv")

library(vegan)

data.frame(Plot = test_data$Plot,
           Shannon =  diversity(x = test_data[, -1], 
                                index = 'shannon'
           )
)

calculate_shannon <- function(x) {
  species_counts <- x[-1]
  total_individuals <- sum(species_counts)
  proportions <- species_counts / total_individuals
  shannon_index <- -sum(proportions * log(proportions), na.rm = TRUE)
  
  return(shannon_index)
}

library(vegan)

data.frame(Plot = test_data$Plot,
           Shannon =  diversity(x = test_data, ## exclude Treatment col
                                index = 'shannon'
           )
)


ddply(test_data,~Plot,function(x) {
           data.frame(SHANNON=diversity(x[-1], index="shannon"))
   })

install.packages("iNEXT")
library(iNEXT)

#shannontest <- ChaoShannon(test_data[-1], datatype = "abundance", transform = FALSE)

test_data_withoutplot <- test_data[-1]


