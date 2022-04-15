#in the paper you put in the github repo, highlight parts of the paper that you replicated here

library(tidyverse)
library(factoextra)
library(MASS)
d = read_csv("bensky_and_bell_2020.csv") 

#I will fo PCAs as my descriptive stats, the linear models as inferential stats, and figs 3 and 4 and 5 as visualization

#They log transformed all positively- skewed duration and latency variables
skimr::skim(d)
#they already include these in the dataset... the "LN" versions of some variables are log transformed

#First, they did two PCAs, one for novel object and the other for the barrier task

#running novel object PCA

novel_vars = c("Novel_LN_TimeToOrient", "Novel_LN_Approach",
                      "Novel_LN_Near5", "Novel_LN_NearandOriented")
d_novel = d %>% 
  dplyr::select(novel_vars) 
novel_pca = prcomp(d_novel, scale = TRUE)

#to get PC1 eigenvalue and % variance, I used the summary() function
novel_pca_summary = summary(novel_pca)
#%variance is 74.48% and eigenvalue is 2.979 as obtained from:
novel_pca_summary$sdev^2
#these match Table 1

#now to get the loadings I thought I could use view(novel_pca), but this gave me different results than the paper
#they mention the factoextra package in text, so i used this package's PCA summary function, get_pca()
novel_pca_factoextra = get_pca(novel_pca)
novel_pca_loadings = novel_pca_factoextra$coord
#the Dim1 column here matches the PCA loadings reported in the paper Table 1

#lastly, to get the scores of PC1 for further analysis:
novel_pca_scores = as.data.frame(novel_pca$x)
novel_PC1_scores = novel_pca_scores$PC1

#adding the scores to d
d = d %>% mutate(novel_PC1_scores = novel_PC1_scores)

#running barrier task pca, same steps as above

barrier_vars = c("Barrier_LN_FirstBout", "Barrier_LN_Apex", "Barrier_LN_EtoB")
d_barrier = d %>% 
  dplyr::select(all_of(barrier_vars))
barrier_pca = prcomp(d_barrier, scale = TRUE)
barrier_pca

barrier_pca_summary = summary(barrier_pca)
#% variance is 77.22%, matches Table 1
barrier_pca_summary$sdev^2
#eigenvalue is 2.316, which matches Table 1

#getting loadings
barrier_pca_factoextra = get_pca(barrier_pca)
barrier_pca_loadings = barrier_pca_factoextra$coord
#the Dim1 column here matches the PCA loadings reported in the paper Table 1

#to get the scores of PC1 for further analysis:
barrier_pca_scores = as.data.frame(barrier_pca$x)
barrier_PC1_scores = barrier_pca_scores$PC1

#adding the scores to d
d = d %>% mutate(barrier_PC1_scores = barrier_PC1_scores)

#linear models

#from section: "How are response to a novel object, boldness and persistence related to each other?"
#they did Pearson coreallation to see relationships between the three behavior variables
#three measures being 
# boldness - measured by d$LN_Emerge_Average (I think you have to multiply it by -1)
# response to novel object - measured by d$novel_PC1_scores
# persistence - measured by d$barrier_PC1_scores



#then, from section: "Is initial and reversal learning performance predicted by response to a novel object, boldness and/or persistence?"
#these were not a figure

#behaviors were related to initial learning performance
#for these, standard length was included as a covariate
initial_w_boldness = glm.nb(Initial_Sessions ~ LN_Emerge_Average+Length, data=d)
summary(initial_w_boldness)
summary_initial_w_boldness = summary(initial_w_boldness)

initial_w_novel = glm.nb(Initial_Sessions ~ novel_PC1_scores+Length, data=d)
summary(initial_w_novel)
summary_initial_w_novel = summary(initial_w_novel)

initial_w_barrier = glm.nb(Initial_Sessions ~ barrier_PC1_scores+Length, data=d)
summary(initial_w_barrier)
summary_initial_w_barrier = summary(initial_w_barrier)

#two behaviors were related to reversal learning performance

reversal_w_boldness = glm.nb(Reversal_Session1 ~ LN_Emerge_Average, data=d)
summary(reversal_w_boldness)
summary_reversal_w_boldness = summary(reversal_w_boldness)

reversal_w_novel = glm.nb(Reversal_Session1 ~ novel_PC1_scores, data=d)
summary(reversal_w_novel)
summary_reversal_w_novel = summary(reversal_w_novel)

#two behaviors related to number of visits to wrong cup during reversal i.e. persistence
#using a binomial glm as this is binary data
#these are visualized in Fig 4

wrong_w_boldness = glm(FirstReversal_PersistBeforeCorrect~ LN_Emerge_Average, family = "binomial",data=d)
summary(wrong_w_boldness)
summary_wrong_w_boldness = summary(wrong_w_boldness)

wrong_w_barrier = glm(FirstReversal_PersistBeforeCorrect~ barrier_PC1_scores, family = "binomial",data=d)
summary(wrong_w_barrier)
summary_wrong_w_boldness = summary(wrong_w_boldness)

#lastly we go to the section "Is initial learning performance related to flexibility?"

#first, we need the residuals of initial learning controlled for Length
initial_w_length = glm.nb(Initial_Sessions ~ Length, data=d)
#adding the residuals to our dataset:
d = d %>% 
  mutate(initial_residuals = initial_w_length$residuals)

#initial learning was not related to reversal learning
initial_w_reversal = glm.nb(Reversal_Sessions ~ initial_residuals, data=d)
summary(initial_w_reversal)
summary_initial_w_reversal = summary(initial_w_reversal)

#initial learning was correlated to first step of reversal learning
initial_w_reversal1 = glm.nb(Reversal_Session1 ~ initial_residuals, data=d)
summary(initial_w_reversal1)
summary_initial_w_reversal1 = summary(initial_w_reversal1)
colnames(d)

d= d %>% 
  dplyr::select(-reversal_w_novel)

#VISUALIZING

#Figure 3a
p1 = predict(reversal_w_novel,type = "response", se.fit = TRUE)
ggplot(d) +
  geom_point(aes(x = novel_PC1_scores, y = Reversal_Session1))+
  geom_line(aes(x = novel_PC1_scores, y = p1$fit))+
  theme_classic()

#Figure 3b
p2 = predict(reversal_w_boldness,type = "response", se.fit = TRUE)
ggplot(d) +
  geom_point(aes(x = -LN_Emerge_Average, y = Reversal_Session1))+
  geom_line(aes(x = -LN_Emerge_Average, y = p2$fit))+
  theme_classic()

#Figure 4a
p3 = predict(wrong_w_boldness,type = "response", se.fit = TRUE)
ggplot(d) +
  geom_point(aes(x = -LN_Emerge_Average, y = FirstReversal_PersistBeforeCorrect))+
  geom_line(aes(x = -LN_Emerge_Average, y = p3$fit))+
  theme_classic()

p4 = predict(wrong_w_barrier,type = "response", se.fit = TRUE)
ggplot(d) +
  geom_point(aes(x = -barrier_PC1_scores, y = FirstReversal_PersistBeforeCorrect))+
  geom_line(aes(x = -barrier_PC1_scores, y = p4$fit))+
  theme_classic()

#Fig 5
p5 = predict(initial_w_reversal1,type = "response", se.fit = TRUE)
ggplot(d) +
  geom_point(aes(x = initial_residuals, y = Reversal_Session1))+
  geom_line(aes(x = initial_residuals, y = p5$fit))+
  theme_classic()

colnames(d)






