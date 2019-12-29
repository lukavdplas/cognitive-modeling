library(ggplot2)
library(reshape2)

#import fMRI data

fMRIs <- read.table("./data/NeuralResponses")

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
  subject_sims[[i]] <- simulate_subject(fMRIs)
}

#make RDMs

make_rdm <- function(data) {
  cor_matrix <- 1 - cor(t(data))
  
  cor_matrix
}

og_cor <- make_rdm(fMRIs)
subject_cors <- list()
for (i in 1:12) {
  subject_cors[[i]] <- make_rdm(subject_sims[[i]])
}

mean_rdm <- Reduce('+', subject_cors) / length(subject_cors)

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
        
        #check if i,j should be included based on filter
        if (filter > 0) {
          if (category_vectors[i,filter] == 0 | category_vectors[j,filter] == 0) {
            include <- FALSE
          }
          else { include <- TRUE}
        }
        else { include <- TRUE }
        
        #add correlation[i,j] to vector
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
  
  #print descriptives
  print('Same category:')
  print(c(mean(same_cat), sd(same_cat)))
  
  print('Different category:')
  print(c(mean(diff_cat), sd(diff_cat)))
  
  #perform t-test
  t.test(same_cat, diff_cat, alternative = "two.sided", paired = FALSE)
  
}

#perform t-tests

#compare_category(1, og_cor) #original data, animacy
#compare_category(1, subject_cors[[1]]) #subject data, animacy
#compare_category(1, mean_rdm) #mean of subject data, animacy
#compare_category(6, og_cor) #original data, faces
#compare_category(6, og_cor, filter = 1) #original data, faces, filter on animacy

#compare_category(3, og_cor) #original data, human/nonhuman
#compare_category(3, og_cor, filter = 1) #original data, human/nonhuman, filter on animacy

# perform 2-way anova to compare 2 categories

perform_anova <- function(cat1, cat2, correlations) {
  cat1_rdm <- category_rdm_for_index(cat1)
  cat2_rdm <- category_rdm_for_index(cat2)
  
  results <- data.frame()
  
  for (i in 1:nrow(correlations)) {
    for (j in 1:ncol(correlations)) {
      if (i < j) { #remove i == j and make sure that i,j and j,i are not both included
        
        #add correlation to dataframe
        results <- rbind(results, c(cat1_rdm[i,j], cat2_rdm[i,j], correlations[i,j])                         )
      }
    }
  }

  colnames(results) <- c("same_cat1", "same_cat2", "correlation")

  anova <- aov(correlation ~ same_cat1 + same_cat2 + same_cat1:same_cat2, data = results)
  summary(anova)
}

#perform_anova(1,6, og_cor)

#load individual neural data
macaque <- read.table('./data/NeuroRDM')
macaque_rdm <- unname(macaque)
macaque_rdm <- as.matrix(macaque_rdm)


plot_macaque <- function() {
  melted_data <- melt(macaque_rdm)
  
  plot <- ggplot(data = melted_data, aes(Var2, Var1, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "darkslategray", high = "darkseagreen1", name = "Dissimilarity", limit = c(0, 10)) +
    xlab("image") +
    ylab("image") +
    theme_minimal()
  
  plot
}

#compare two RDMS

compare_rdms <- function(rdm1, rdm2, filter = 0) {
  #give the pairwise values
  rdm1_values <- c()
  rdm2_values <- c()
  
  for (i in 1:nrow(rdm1)) {
    for (j in 1:ncol(rdm1)) {
      if (i < j) {
        #check if i,j should be included based on filter
        if (filter > 0) {
          if (category_vectors[i,filter] == 0 | category_vectors[j,filter] == 0) {
            include <- FALSE
          }
          else { include <- TRUE}
        }
        else { include <- TRUE }
        
        #add to vectors
        if (include) {
          rdm1_values <- c(rdm1_values, c(rdm1[i,j]))
          rdm2_values <- c(rdm2_values, c(rdm2[i,j]))          
        }
      }
    }
  }

  list(rdm1_values, rdm2_values)
}

correlate_rdms <- function(rdm1, rdm2, filter = 0) {
  values <- compare_rdms(rdm1, rdm2, filter)
  cor.test(values[[1]], values[[2]])
}

plot_human_macaque_scatter <- function() {
  values <- compare_rdms(mean_rdm, macaque_rdm)
  human_macaque_df <- data.frame(Human = values[[1]], Macaque = values[[2]])
  
  ggplot(data = human_macaque_df) +
    geom_point(aes(x = Human, y = Macaque), color = "darkseagreen4", size=0.5) +
    theme_classic()
}


correlate_rdms(mean_rdm, macaque_rdm)
correlate_rdms(mean_rdm, macaque_rdm, filter = 1)
correlate_rdms(mean_rdm, macaque_rdm, filter = 2)

behaviour <- read.table('./data/BehaviourRDM')
behaviour_rdm <- unname(behaviour)
behaviour_rdm <- as.matrix(behaviour_rdm)

plot_behaviour_rdm <- function () {
  melted_data <- melt(behaviour_rdm)
  
  plot <- ggplot(data = melted_data, aes(Var2, Var1, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "darkslategray", high = "darkseagreen1", name = "Dissimilarity", limit = c(0, 0.4)) +
    xlab("image") +
    ylab("image") +
    theme_minimal()
  
  plot
}

#plot_behaviour_rdm()

correlate_rdms(mean_rdm, behaviour_rdm)
correlate_rdms(mean_rdm, behaviour_rdm, filter = 1)
correlate_rdms(mean_rdm, behaviour_rdm, filter = 2)


plot_neural_behaviour_scatter <- function() {
  values <- compare_rdms(mean_rdm, behaviour_rdm)

  df <- data.frame(Neural = values[[1]], Behaviour = values[[2]])
  
  ggplot(data = df) +
    geom_point(aes(x = Neural, y = Behaviour), size=0.5) +
    theme_classic()
}

#plot_neural_behaviour_scatter()