library(tidyverse)
library(ggplot2)
library(latex2exp)
library(extrafont) 
loadfonts()

media_headers = read_csv('termpartisanship.csv')[-1]
bigram_data <- read_csv('bigram_partisanship.csv')[-1]

sum_beta_sq <- sum(bigram_data$b_p^2)

calc_part <- function(term,freq){
  where = bigram_data$phrase == term
  val = pull(bigram_data[where,'b_p'])*( freq - pull(bigram_data[where,'a_p']))
  return(val)
}

media_headers_by_orientation <- media_headers%>%
  group_by(political_orientation)%>%
  summarise_if(is.numeric, sum)

header_bias <- media_headers_by_orientation
nr_bigrams <- c()
for(i in 1:nrow(media_headers_by_orientation)){
  nr_bigrams = c(nr_bigrams, sum(media_headers_by_orientation[i,3:ncol(media_headers_by_orientation)]))
}
colnames = names(media_headers_by_orientation)
for(j in 3:ncol(media_headers_by_orientation)){
  for(i in 1:nrow(media_headers_by_orientation)){
    header_bias[i,j] = calc_part(colnames[j],(media_headers_by_orientation[i,j]/nr_bigrams[i]))
    }
}
pi = c()
for(i in 1:nrow(header_bias)){
  pi = c(pi,sum(header_bias[i,3:ncol(header_bias)])/sum_beta_sq)
}
header_bias <- media_headers_by_orientation[,c(1,2)]
header_bias['pi'] <- pi
header_bias
t_true <- header_bias$pi[2] - header_bias$pi[1]

re_sample_left <- c()
re_sample_right <- c()
re_sample_t <- c()

n_sample <- 1000
n_left <- sum(media_headers$political_orientation=='left')

for(i in 1:n_sample){
  re_sample_idx <- sample(1:nrow(media_headers),replace = FALSE)[1:n_left]
  re_sample_headers <- media_headers
  re_sample_headers$political_orientation[re_sample_idx] = 'left'
  re_sample_headers$political_orientation[-re_sample_idx] = 'right'
  
  media_headers_by_orientation <- re_sample_headers%>%
    group_by(political_orientation)%>%
    summarise_if(is.numeric, sum)
  
  header_bias <- media_headers_by_orientation
  nr_bigrams <- c()
  for(i in 1:nrow(media_headers_by_orientation)){
    nr_bigrams = c(nr_bigrams, sum(media_headers_by_orientation[i,3:ncol(media_headers_by_orientation)]))
  }
  colnames = names(media_headers_by_orientation)
  for(j in 3:ncol(media_headers_by_orientation)){
    for(i in 1:nrow(media_headers_by_orientation)){
      header_bias[i,j] = calc_part(colnames[j],(media_headers_by_orientation[i,j]/nr_bigrams[i]))
    }
  }
  pi = c()
  for(i in 1:nrow(header_bias)){
    pi = c(pi,sum(header_bias[i,3:ncol(header_bias)])/sum_beta_sq)
  }
  header_bias <- media_headers_by_orientation[,c(1,2)]
  header_bias['pi'] <- pi
  header_bias
  left_bias <- header_bias$pi[header_bias$political_orientation=='left'] 
  right_bias <- header_bias$pi[header_bias$political_orientation=='right']
  re_sample_left <- c(re_sample_left,left_bias)
  re_sample_right <- c(re_sample_right,right_bias)
  re_sample_t <- c(re_sample_t,left_bias-right_bias)
  
}



qplot(re_sample_t,
      geom="histogram",
      binwidth = 0.01,  
      main = "Permutation Test of Differences in 'Ideology'", 
      xlab = TeX("Difference in Ideology $\\hat{y}_l - \\hat{y}_r$"), 
      ylab = 'Frequency',
      xlim=c(-0.8,0.8))+
  theme(text= element_text(size=10, family="LM Roman 10"))+
  geom_vline(xintercept = t_true, color='red')+
  geom_text(aes(x=t_true-0.03, label="Observed difference", y=20), colour="red", angle=90, size=3, family="LM Roman 10")
  
ggsave(
  filename='term_slant.png',
  plot = last_plot(),
  path = "Graphs",
  scale = 1,
  width = 5*(1+sqrt(5)),
  height = 10,
  units = c("cm"),
  dpi = 500,
)


hist(re_sample_t,breaks=seq(-0.8,0.8,0.01),xlim =c(-0.8,0.8))
abline(v = t_true, col='red')
(p = (1+sum(abs(re_sample_t)>abs(t_true)))/(n_sample+1))



by_outlets = group_by(media_headers, source, political_orientation)%>%
  summarise_id(mean_Gentzkow=mean(partisanship_Gentzkow))%>%
  arrange(political_orientation)
by_outlets

by_outlets = group_by(media_headers, source, political_orientation)%>%
  summarise(mean_Matter=mean(partisanship_Matter))%>%
  arrange(political_orientation)
by_outlets

by_affiliation = group_by(media_headers,  political_orientation)%>%
  summarise(mean_Gentzkow=mean(partisanship_Gentzkow))%>%
  arrange(political_orientation)
by_affiliation

by_affiliation = group_by(media_headers, political_orientation)%>%
  summarise(mean_Matter=mean(partisanship_Matter)*10^8)%>%
  arrange(political_orientation)
by_affiliation
hist(media_headers$partisanship_Gentzkow,nclass=200)






####################
### selection success

folder_path <- "results/"
data <- read_csv(paste(folder_path,'a_searches.csv',sep=''), col_names = c('term','source','date_time','type','clicked','bot_id'))%>%
  bind_rows(read_csv(paste(folder_path,'b_searches.csv',sep=''), col_names = c('term','source','date_time','type','clicked','bot_id')))%>%
  bind_rows(read_csv(paste(folder_path,'c_searches.csv',sep=''), col_names = c('term','source','date_time','type','clicked','bot_id')))%>%
  bind_rows(read_csv(paste(folder_path,'d_searches.csv',sep=''), col_names = c('term','source','date_time','type','clicked','bot_id')))%>%
  bind_rows(read_csv(paste(folder_path,'e_searches.csv',sep=''), col_names = c('term','source','date_time','type','clicked','bot_id')))%>%
  bind_rows(read_csv(paste(folder_path,'f_searches.csv',sep=''), col_names = c('term','source','date_time','type','clicked','bot_id')))%>%
  mutate(clicked = as.factor(clicked))
data[data$clicked=='sucess','clicked'] <- 'success' 
data[data$clicked=='No Results','clicked'] <- 'failure' 

data <- filter(data,type=='political')%>%
  mutate(flag = ifelse(source %in% c('alternet.com', 'occupydemocrats.com','cnn.com','thedailybeast.com'),'left', 'right'))%>%
  droplevels()%>%
  group_by(clicked, flag)%>%
  select_at(c('clicked','flag'))%>%
  summarise(freq = n())%>%
  pivot_wider(names_from = clicked, values_from = freq)%>%
  replace_na(list(success=0))%>%
  mutate(sucess_rate <- success /(failure + success))
data


