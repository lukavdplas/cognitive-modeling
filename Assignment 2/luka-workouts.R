library(ggplot2)
library(reshape2)

#import fMRI data

neural_responses <- read.table("./data/NeuralResponses")

#simulate subjects

simulate_subject <- function(neural_data) {
  subject_table <- neural_data
  for (i in 1:nrow(neural_data)) {
    subject_table[i,] <- subject_table[i,] + rnorm(ncol(subject_table), mean = 0, sd = 1)
  }
  subject_table
}

subject_sims <- list()

for (i in 1:12) {
  subject_sims[[i]] <- simulate_subject(neural_responses)
}

#make RDMs

make_rdm <- function(data) {
  cor_matrix <- 1 - cor(t(data))
  
  cor_matrix
}

og_cor <- make_rdm(neural_responses)
subject_cors <- list()
for (i in 1:12) {
  subject_cors[[i]] <- make_rdm(subject_sims[[i]])
}

mean_rdm <- Reduce('+', subject_cors) / lenth(subject_cors)

#plot RDMs

plot_rdm <- function(rdm_data) {
  melted_data <- melt(rdm_data)
  
  plot <- ggplot(data = melted_data, aes(Var2, Var1, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "darkslategray", high = "darkseagreen1", name = "Dissimilarity", limit = c(0, 1.5)) +
    xlab("image") +
    ylab("image") +
    theme_minimal()
    
  plot
}

mean_plot <- plot_rdm(mean_rdm)
og_plot <- plot_rdm(og_cor)
subject_plot <- plot_rdm(subject_cors[[1]])

#import category vectors

category_vectors <- read.table("./data/CategoryVectors")
category_labels <- read.table("./data/CategoryLabels")

#make RDM for category

category_rdm <- function(cat_vector) {
  rdm <- matrix(nrow = length(cat_vector), ncol = length(cat_vector))
  for (i in 1:length(cat_vector)) {
    for(j in 1:length(cat_vector)) {
      rdm[i,j] <- as.numeric(cat_vector[i] == cat_vector[j])
    }
  }
  rdm
}

category_rdm_for_index <- function(index) {
  category_rdm(category_vectors[,index]) 
}

#compare same vs different in one category

compare_category <- function(cat_index, correlations, filter = 0) {
  
  cat_rdm <- category_rdm_for_index(cat_index)
  
  same_cat <- c()
  diff_cat <- c()
  
  for (i in 1:nrow(cat_rdm)) {
    for (j in 1:ncol(cat_rdm)) {
      if (i < j) { #remove i == j and make sure that i,j and j,i are not both included
        include <- TRUE
        if (filter > 0) {
          if (category_vectors[i,filter] == 0 | category_vectors[j,filter] == 0) {
            include <- FALSE
          }
        }
        if (include) {
          if (cat_rdm[i,j] == 1) {
            same_cat <- c(same_cat, c(correlations[i,j]))
          }
          else {
            diff_cat <- c(diff_cat, c(correlations[i,j]))
          }
        }
        
      }
    }
  }
  
  #perform t-test
  t.test(same_cat, diff_cat, alternative = "two.sided", paired = FALSE)
}

compare_category(1, og_cor) #original data, animacy
compare_category(1, subject_cors[[1]]) #subject data, animacy
compare_category(1, mean_rdm) #mean of subject data, animacy
compare_category(6, og_cor) #original data, faces
compare_category(6, og_cor, filter = 1) #original data, faces, filter on animacy

compare_category(3, og_cor) #original data, faces
compare_category(3, og_cor, filter = 1) #original data, faces, filter on animacy