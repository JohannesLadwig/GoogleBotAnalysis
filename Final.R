library(tidyverse)
library(ggplot2)
library(latex2exp)
library(lubridate)
library(extrafont) 
library(randomForest)
library(stringdist)

loadfonts()

source("rbo.R")



get_date <- function(location, time){
  if(location=='Philadelphia'){
    d = date(with_tz(time, tzone = "US/Eastern"))
  }else{
    d = date(with_tz(time, tzone = "US/Central"))
  }x
  return(d)}

get_bias <- function(dom){
  dom = str_replace(dom,'www.','')
  bias <- pull(outlets[outlets$outlets == dom, 'bias'][1])
  return(bias)
}

BiasToNumeric <-  function(bias){
  if(bias == 'Least'){
    return(0)
  }
  if(bias == 'None'){
    return(0)
  }
  if(bias %in% c('Left Center', 'Left Centre','Left center')){
    return(-1)
  }
  if( bias %in% c('Right Center', 'Right center')){
    return(1)
  }
  if(bias=='Right'){
    return(2)
  }
  if(bias=='Left'){
    return(-2)
  }
  if(bias=='Extreme Right'){
    return(3)
  }
  if(bias=='Extreme Left'){
    return(-3)
  }
}


outlets <- read_csv('outlets_bias.csv')
numeric_bias = c()
for(i in 1:nrow(outlets)){
  numeric_bias = c(numeric_bias, BiasToNumeric(outlets$bias[i]))
}
outlets['bias'] = numeric_bias

get_bias <- function(dom){
  dom = str_replace(dom,'www.','')
  bias <- pull(outlets[outlets$outlets == dom, 'bias'][1])
  return(bias)
}


###############
#### Left Right Partisanship, even dataset, permutation test, Gentzkow 2010, Hodler 2021 bigram Data

### Further pre-processing of raw data

processed_data <- read_csv('Final_Political_Bigram_Corpus.csv')[-1]%>%
  mutate(container = as.factor(container))%>%
  mutate(flag = as.factor(flag))%>%
  mutate(time = as.POSIXct(round.POSIXt(time, 'mins')))%>%
  mutate(domain = str_replace(domain,'www.',''))
processed_data[processed_data$flag=='dem','flag'] ='left'

min_t <- (min(processed_data$time))

processed_data = droplevels(processed_data)%>%
  filter(search_term!='Los Angeles facebook' & search_term!='Los Angeles ways ministry' & search_term!='Los Angeles recreation commission' & search_term!='Los Angeles national weather service' & search_term!= 'Van Halen junior')%>%
  mutate(location = ifelse(container %in% c('a','c','e'), 'Huston','Philadelphia'))%>%
  droplevels()%>%
  mutate(days = round(difftime(time,min_t, units='days')))
processed_data$location= as.factor(processed_data$location)

dates = c()
domain_bias = c()
for(i in 1:nrow(processed_data)){
  dates = c(dates,as.character(get_date(processed_data$location[i], processed_data$time[i])))
  domain_bias = c(domain_bias, get_bias(processed_data$domain[i]))
}


processed_data['date'] = dates
processed_data['domain_bias'] = domain_bias

processed_data <- mutate(processed_data, date = as.Date(date))%>%
  mutate(search_term = as.factor(search_term))

groups_to_clean <- processed_data%>%
  group_by(bot_id,time,flag,search_term,date)%>%
  summarize(nr =n())%>%
  filter(nr<=7)%>%
  group_by(bot_id,flag,search_term,date)%>%
  group_split()
to_clean <- tibble()
for(j in 1:length(groups_to_clean)){
  if (nrow(groups_to_clean[[j]])>=2){
    g <- groups_to_clean[[j]]
    g$new_time <- g$time[1]
    to_clean <- bind_rows(to_clean,g)
  }}

processed_data <- left_join(processed_data,to_clean[,-6], by=c('bot_id','time','flag','search_term','date'))
processed_data[!is.na(processed_data$new_time),'time'] <-  processed_data[!is.na(processed_data$new_time),'new_time']
processed_data <- select(processed_data,-new_time)

bigram_data <- read_csv('bigram_partisanship.csv')[-1]

sum_beta_sq <- sum(bigram_data$b_p^2)
sum_beta_alpha <- sum(bigram_data$b_p*bigram_data$a_p)/sum_beta_sq

calc_part <- function(term,freq){
  where = bigram_data$phrase == term
  val = pull(bigram_data[where,'b_p'])*( freq - pull(bigram_data[where,'a_p']))
  return(val)
}



## GENERATE A MORE EVEN DATASET

set.seed(1000)

buckets <- processed_data%>%
  group_by(date,search_term,location)%>%
  group_split()

even_data <- tibble()
training_perc = 1
for(i in 1:length(buckets)){
  bucket <- buckets[[i]]
  term = as.character(pull(bucket[1,'search_term']))
  left <- filter(bucket, flag=='left')%>%
    group_by(bot_id,time)%>%
    group_split()
  right <- filter(bucket, flag=='right')%>%
    group_by(bot_id,time)%>%
    group_split()
  control <- filter(bucket, flag=='control')%>%
    group_by(bot_id,time)%>%
    group_split()

  nr_select <-  floor(training_perc * min(length(left),length(right)))
  if (nr_select > 0){
    training_left_idx <-  sample(1:length(left), nr_select, replace=FALSE)
    training_right_idx <-  sample(1:length(right), nr_select, replace=FALSE)
    training_control_idx <- sample(1:length(control), min(nr_select,length(control)), replace=FALSE)
    
    selected_left <- bind_rows(left[training_left_idx])
    selected_right <- bind_rows(right[training_right_idx])
    selected_control <- bind_rows(control[training_control_idx])
    
    even_data = bind_rows(even_data, selected_left)
    even_data = bind_rows(even_data, selected_right)
    even_data = bind_rows(even_data, selected_control)
    }
  }



# Botwise
even_data_by_bot <- even_data[-c(1,3,4,5,7,9,512:513)]%>%
  group_by(flag,bot_id)%>%
  summarise_if(is.numeric, sum)
for(i in 1:nrow(even_data_by_bot) ){
  n_tot <- sum(even_data_by_bot[i,-c(1,2)])
  for(k in 3:ncol(even_data_by_bot)){
    even_data_by_bot[i,k] <- pull(even_data_by_bot[i,k]/n_tot)
  }
}


y_hat <- c()
for(i in 1:nrow(even_data_by_bot)){
  vals = c()
  for(name in names(even_data_by_bot)[-c(1,2)]){
    vals = c(vals, calc_part(name, pull(even_data_by_bot[i,name])))
  }
  y_hat <- c(y_hat,sum(vals)/sum_beta_sq)
}
y_hat_tibble <- even_data_by_bot[,c(1,2)]
y_hat_tibble['y_hat'] <- y_hat
means <- y_hat_tibble%>%
  group_by(flag)%>%
  summarise_if(is.numeric, mean)

t_test <- with(y_hat_tibble, t.test(y_hat[flag == 'left'], y_hat[flag == 'right']))

t_test

# ggplot(filter(y_hat_tibble, flag!='control'),
#        aes(y_hat, colour=flag))+
#   labs(x = TeX("$\\tilde{y}_b$"),  
#        y = 'count',
#        title = "Distribution of Estimated Bias Amongst Individual Bots")+
#   geom_freqpoly(binwidth=0.04)+
#   theme(text= element_text(size=10, family="LM Roman 10"))

qplot(filter(y_hat_tibble, flag!='control')$y_hat,
      geom="histogram",
      binwidth = 0.02,  
      main = "Distribution of Estimated Bias Amongst Individual Bots", 
      xlab = TeX("$\\hat{y}_b$"),  
      ylab = 'count')+
  theme(text= element_text(size=10, family="LM Roman 10"))

ggsave(
  filename='individual_bias.png',
  plot = last_plot(),
  path = "Graphs",
  scale = 1,
  width = 5*(1+sqrt(5)),
  height = 10,
  units = c("cm"),
  dpi = 500,
)

even_data_by_search <- even_data%>%
  group_by(time,bot_id,flag,search_term)%>%
  summarise_if(is.numeric, sum)

n_tot <- c()
for(i in 1:nrow(even_data_by_search) ){
  n_tot <- c(n_tot,sum(even_data_by_search[i,-c(1:6,506)]))
}

qplot(n_tot,
      geom="histogram",
      binwidth=1,
      main = "Count of Partisan Bigrams", 
      xlab = TeX("Number of Partisan Bigrams on SRP"),  
      ylab = 'count')+
  theme(text= element_text(size=10, family="LM Roman 10"))

ggsave(
  filename='bigram_freq.png',
  plot = last_plot(),
  path = "Graphs",
  scale = 1,
  width = 5*(1+sqrt(5)),
  height = 10,
  units = c("cm"),
  dpi = 500,
)


###########
# Domain Bias t-test
even_data_by_bot <- even_data[c(2,3,6,8,513)]%>%
  group_by(flag,bot_id)%>%
  summarise_if(is.numeric, mean)

means <- even_data_by_bot%>%
  group_by(flag)%>%
  summarise_if(is.numeric, mean)

(t_test <- with(even_data_by_bot, t.test(domain_bias[flag == 'left'], domain_bias[flag == 'right'])))

ggplot(filter(even_data_by_bot,flag!='control'),
      aes(domain_bias, colour=flag))+
  labs(x = TeX("observed bias $\\hat{\\pi}_b$"),  
       y = 'count',
      title = "Frequency Plot of Domain Based Result Bias")+
  geom_freqpoly(binwidth=0.01)+
  theme(text= element_text(size=10, family="LM Roman 10"))

ggsave(
  filename='domain_bias.png',
  plot = last_plot(),
  path = "Graphs",
  scale = 1,
  width = 5*(1+sqrt(5)),
  height = 10,
  units = c("cm"),
  dpi = 500,
)

even_data_by_bot <- even_data[c(2,3,6,7,8,513)]%>%
  group_by(flag,bot_id,time,search_term)%>%
  mutate(bottom_weighted_bias = domain_bias*rank/(36))%>%
  mutate(top_weighted_bias = domain_bias * (9-rank)/36)%>%
  summarise_if(is.numeric, sum)%>%
  group_by(flag,bot_id)%>%
  summarise_if(is.numeric,mean)
means <- even_data_by_bot%>%
  group_by(flag)%>%
  summarise_if(is.numeric, mean)

ggplot(filter(even_data_by_bot,flag!='control'),
       aes(top_weighted_bias, colour=flag))+
  labs(x = TeX("observed bias $\\hat{\\psi}_b$"),  
       y = 'count',
       title = "Frequency Plot of Weighted Domain Based Result Bias")+
  geom_freqpoly(binwidth=0.01)+
  theme(text= element_text(size=10, family="LM Roman 10"))

ggsave(
  filename='domain_bias_weighted.png',
  plot = last_plot(),
  path = "Graphs",
  scale = 1,
  width = 5*(1+sqrt(5)),
  height = 10,
  units = c("cm"),
  dpi = 500,
)



(t_test <- with(even_data_by_bot, t.test(top_weighted_bias[flag == 'left'], top_weighted_bias[flag == 'right'])))

ggplot(filter(even_data_by_bot,flag!='control'),
       aes(bottom_weighted_bias, colour=flag))+
  labs(x = TeX("Observed bias $\\hat{\\pi}_b$"),  
       y = 'count',
       title = "Histogram of Domain Based Result Bias")+
  geom_freqpoly(binwidth=0.002)+
  theme(text= element_text(size=10, family="LM Roman 10"))


even_data_rank_removed <- even_data[c(2,3,6,8,513)]%>%
  filter(rank!=8)%>%
  group_by(flag,bot_id)%>%
  mutate(bottom_weighted_bias = domain_bias*rank/(36))%>%
  mutate(top_weighted_bias = domain_bias * (8-rank)/28)%>%
  summarise_if(is.numeric, mean)

means <- even_data_rank_removed%>%
  group_by(flag)%>%
  summarise_if(is.numeric, mean)

(t_test <- with(even_data_rank_removed, t.test(domain_bias[flag == 'left'], domain_bias[flag == 'right'])))


######
##Random Forest

perf <- function(table, len){
  analysis <- data.frame('accuracy'=c(NA),'sensitivity'=c(NA),
                         'specificity'=c(NA), 'precision'=c(NA))
  
  analysis[1,1] <- (table[2,2] + table[1,1])/(len)
  analysis[1,2] <- table[2,2]/sum(table[,2])
  analysis[1,3] <- table[1,1]/sum(table[,1])
  analysis[1,4] <- table[2,2]/sum(table[2,])
  
  return(analysis)
}


bigram_data_2 <- read_csv('all_data.csv')%>%
  mutate(container = as.factor(container))%>%
  mutate(flag = as.factor(flag))

frequencies <- summarise_if(bigram_data_2, is.numeric, sum)
frequent = c()
for( j in 3:length(frequencies)){
  {
    if (frequencies[j]>25){
    frequent = c(frequent,j+9)}
  }
}
bigram_data_2 <- bigram_data_2[c(3,4,5,7,8,9,10,frequent)]
bigram_data_2[bigram_data_2$flag=='dem','flag'] ='left'
min_t <- (min(bigram_data_2$time))
bigram_data_2 = droplevels(bigram_data_2)%>%
  filter(search_term!='Los Angeles facebook' & search_term!='Los Angeles ways ministry' & search_term!='Los Angeles recreation commission' & search_term!='Los Angeles national weather service')%>%
  mutate(location = ifelse(container %in% c('a','c','e'), 'Huston','Philadelphia'))%>%
  droplevels()%>%
  mutate(time = as.POSIXct(round.POSIXt(time, 'mins')))%>%
  mutate(days = round(difftime(time,min_t, units='days')))
bigram_data_2$location= as.factor(bigram_data_2$location)

dates = c()
domain_bias = c()
for(i in 1:nrow(bigram_data_2)){
  dates = c(dates,as.character(get_date(bigram_data_2$location[i], bigram_data_2$time[i])))
  domain_bias = c(domain_bias, get_bias(bigram_data_2$domain[i]))
}


bigram_data_2['date'] = dates
bigram_data_2['domain_bias'] = domain_bias

bigram_data_2 <- mutate(bigram_data_2, date = as.Date(date))%>%
  mutate(search_term = as.factor(search_term))

groups_to_clean <- bigram_data_2%>%
  group_by(bot_id,time,flag,search_term,date)%>%
  summarize(nr =n())%>%
  filter(nr<=7)%>%
  group_by(bot_id,flag,search_term,date)%>%
  group_split()
to_clean <- tibble()
for(j in 1:length(groups_to_clean)){
  if (nrow(groups_to_clean[[j]])>=2){
    g <- groups_to_clean[[j]]
    g$new_time <- g$time[1]
    to_clean <- bind_rows(to_clean,g)
  }}

bigram_data_2 <- left_join(bigram_data_2,to_clean[,-6], by=c('bot_id','time','flag','search_term','date'))
bigram_data_2[!is.na(bigram_data_2$new_time),'time'] <-  bigram_data_2[!is.na(bigram_data_2$new_time),'new_time']
bigram_data_2 <- select(bigram_data_2,-new_time)



converted_names = make.names(names(bigram_data_2))
duplicates = converted_names[duplicated(converted_names)]

bigram_data_3 = bigram_data_2[1]
for(col in names(bigram_data_2)[2:length(bigram_data_2)]){
  new_name = make.names(col)
  if(!(new_name %in% duplicates)){
    bigram_data_3[new_name] = bigram_data_2[col]
  }else{
    if(new_name %in% names(bigram_data_3)){
      bigram_data_3[new_name] = bigram_data_3[new_name] + bigram_data_2[col]
    }else{
      bigram_data_3[new_name] =bigram_data_2[col]
    }}
}


buckets <- bigram_data_3%>%
  group_by(date,search_term,location)%>%
  group_split()

set.seed(1001)

training_data <- tibble()
testing_data <- tibble()
training_perc = 0.8
for(i in 1:length(buckets)){
  bucket <- buckets[[i]]
  term = as.character(pull(bucket[1,'search_term']))
  left <- filter(bucket, flag=='left')
  right <- filter(bucket, flag=='right')
  
  nr_select <-  floor(training_perc * min(nrow(left),nrow(right)))
  
  if (nr_select > 0){
    training_left_idx <-  sample(1:nrow(left), nr_select)
    training_right_idx <-  sample(1:nrow(right), nr_select)
    
    
    training_data = bind_rows(training_data, left[training_left_idx,])
    training_data = bind_rows(training_data, right[training_right_idx,])
    testing_data = bind_rows(testing_data, left[-training_left_idx,])
    testing_data = bind_rows(testing_data,right[-training_right_idx,])
    
  }else{
    testing_data = bind_rows(testing_data,left)
    testing_data = bind_rows(testing_data,right)
  }}

training_data <- filter(training_data,flag!='control')%>%
  droplevels()
testing_data <- filter(testing_data,flag!='control')%>%
  droplevels()
set.seed(1002)
forest <- randomForest(flag~., data=training_data[,-c(1,3,5,6,7,3685,3686,3687)], ytest=testing_data$flag, xtest=testing_data[,-c(1,3,4,5,6,7,3685,3686,3687)],maxnodes = 10, importance=TRUE, ntree=2000)

perf(forest$confusion[1:2,1:2],nrow(training_data))
(test_CM <- table(forest$test$predicted, testing_data$flag))
perf(test_CM,nrow(testing_data))


#######
##Similarity Measure
set.seed(1004)
letter_index <- function(some_list){
  string <- letters[1:length(some_list)]
  if(sum(these <- duplicated(some_list))>0){
    for(item in unique(some_list[these])){
      here = (some_list == item)
      string[here] = string[here][1]
    }
  }
  return(string)
}

SimilarityMeasure <- function(results_a, results_b){
  f_a <- results_a$flag[1]
  f_b <- results_b$flag[1]
  flag_string = ''
  if((f_a=='right'|f_b=='right')){
    flag_string = paste(flag_string,'r',sep='')
  }
  if((f_a=='left'|f_b=='left')){
    flag_string = paste(flag_string,'l',sep='')
  }
  if((f_a=='control'|f_b=='control')){
   flag_string = paste(flag_string,'c',sep='')
  }
  
  bias_sign <- 1
  if(flag_string %in%c('l','r','c')){
    bias_sign <- 1
  }else if(f_a == 'right'){
    bias_sign <- -1
  }else if(f_b =='c'){
    bias_sign <- -1
  }
  
  
  delta_t <-  difftime(results_a$time[1],results_b$time[1], units='mins')
  delta_t <- ifelse(delta_t<0, -delta_t, delta_t)
  
  l_a <- results_a$location[1]
  l_b <- results_b$location[1]
  loc_string = ''
  if((l_a =='Huston'|l_b =='Huston')){
    loc_string = paste(loc_string,'H',sep='')
  }
  if((l_a=='Philadelphia'|l_b=='Philadelphia')){
    loc_string = paste(loc_string,'P',sep='')
  }

  b = c()
  n_a = nrow(results_a)
  n_b = nrow(results_b)
  a = letter_index(results_a$title)
  j = 1
  for (i in 1:n_b){
    header = pull(results_b[i,'title'])
    match =filter(results_a, title==header)
    if (nrow(match) >= 1){
      b = c(b, letters[pull(match[1,'rank'])])
    }
    else{
      b = c(b, letters[n_a+j])
      j = j+1
    }
  }
  a = paste(a,collapse='')
  b = paste(b, collapse='')
  dl_distance = stringdist(a,b, method = 'dl')
  jaccard = 1-stringdist(a,b, method = 'jaccard')
  rbo_title <- RBO_EXT(results_a$title, results_b$title, p = 0.92975)
  
  a_domains = results_a$domain
  b_domains = results_b$domain
  
  mean_bias_a = mean(results_a$domain_bias)
  mean_bias_b = mean(results_b$domain_bias)
  
  unique_a = setdiff(a_domains,b_domains)
  unique_b = setdiff(b_domains,a_domains)
  
  if(length(unique_a)>0){
    unique_bias_a = mean(filter(results_a, domain %in% unique_a)$domain_bias)
  }else{
    unique_bias_a = 0
  }
  
  if(length(unique_b)>0){
    unique_bias_b = mean(filter(results_b, domain %in% unique_b)$domain_bias)
  }else{
    unique_bias_b = 0
  }
  #domain_based
  a = letter_index(a_domains)
  b=c()
  for (i in b_domains){
    match = a_domains == i 
    if (sum(match) >= 1){
      b = c(b, a[match][1])
    }
    else{
      b = c(b, letters[n_a+j])
      j = j+1
    }
  }
  a = paste(a, collapse='')
  b = paste(b, collapse='')
  dl_distance_domain = stringdist(a,b, method = 'dl')
  jaccard_domain = 1-stringdist(a,b, method = 'jaccard')
  rbo_domain <- RBO_EXT(results_a$domain, results_b$domain, p = 0.92975)
  
  pi_unique <- bias_sign * (unique_bias_a - unique_bias_b)
  pi_mean <- bias_sign * (mean_bias_a - mean_bias_b)
  
  return(c('jaccard_title' = jaccard,'jaccard_domain'= jaccard_domain, 'edit_distance_title'= dl_distance, 'edit_distance_domain'= dl_distance_domain, 'rbo_title' = rbo_title, 'rbo_domain' = rbo_domain, 'political_difference' = flag_string, 'delta_t' = delta_t, 'locations'=loc_string,'same_bot'=results_a$bot_id[1]==results_b$bot_id[1],'diff_unique_domain_bias' = pi_unique, 'diff_domain_bias'=pi_mean ))
}


reduced_data <- processed_data[c(1,2,3,4,6,7,8,511,512,513,510)]%>%
  group_by(bot_id,time,search_term,date,days)%>%
  filter(n() == 8)%>%
  droplevels()%>%
  mutate(adjusted_days = ifelse(days>=25, days-25,ifelse(days>=11, days-11, ifelse(days>=4, days-4, days))))%>%
  mutate(bot_index = as.numeric(strsplit(bot_id,split='')[[1]][2]))%>%
  ungroup()


buckets <- reduced_data%>%
  group_by(date,search_term,days)%>%
  group_split()

sample_1 <- tibble()
sample_2 <- tibble()

for(i in 1:length(buckets)){
  bucket <- buckets[[i]]
  term = as.character(pull(bucket[1,'search_term']))
  days = as.numeric(pull(bucket[1,'days']))
  adj_days = as.numeric(pull(bucket[1,'adjusted_days']))
  bucket_date = as.character.Date(pull(bucket[1,'date']))
  training_1 <- filter(bucket, (bot_index%%2==1)|(flag =='control'))%>%
    group_by(bot_id,time)%>%
    group_split()
  training_2 <- filter(bucket, bot_index%%2==0)%>%
    filter(flag!='control')%>%
    group_by(bot_id,time)%>%
    group_split()
  
  if (length(training_1)>1){
    for(k in 1:(length(training_1)-1)){
      for(j in (k+1):length(training_1)){
        sim = SimilarityMeasure(training_1[[k]], training_1[[j]])
        entry <- c('term'=term,'date'=bucket_date, 'days' = days,'adjusted_days'=adj_days, sim)
        sample_1 <- bind_rows(sample_1, entry)
      }}
  }
  if (length(training_2)>1){
    for(k in 1:(length(training_2)-1)){
      for(j in (k+1):length(training_2)){
        sim = SimilarityMeasure(training_2[[k]], training_2[[j]])
        entry <- c('term'=term,'date'=bucket_date, 'days' = days,'adjusted_days'=adj_days, sim)
        sample_2 <- bind_rows(sample_2, entry)
      }}
  }
}
training_diff <- bind_rows(sample_1, sample_2, .id='id')

training_diff <- training_diff%>%
  mutate(term = as.factor(term))%>%
  mutate(jaccard_title = as.numeric(jaccard_title))%>%
  mutate(jaccard_domain = as.numeric(jaccard_domain))%>%
  mutate(edit_distance_title = as.numeric(edit_distance_title))%>%
  mutate(edit_distance_domain = as.numeric(edit_distance_domain))%>%
  mutate(rbo_title = as.numeric(rbo_title))%>%
  mutate(rbo_domain = as.numeric(rbo_domain))%>%
  mutate(political_difference = as.factor(political_difference))%>%
  mutate(locations = as.factor(locations))%>%
  mutate(same_bot = as.logical(same_bot))%>%
  mutate(delta_t = as.numeric(delta_t))%>%
  mutate(diff_unique_domain_bias = as.numeric(diff_unique_domain_bias))%>%
  mutate(date = as.Date(date))%>%
  mutate(days=as.numeric(days))%>%
  mutate(adjusted_days = as.numeric(adjusted_days))%>%
  mutate(rbo_title= 1-rbo_title)%>%
  mutate(rbo_domain= 1-rbo_domain)

adjusted <- training_diff%>%
  filter(delta_t<=30)


even_data <- function(flag_1,flag_2, data,flag_1_b=NULL,flag_2_b=NULL){
  new <- tibble()
  buckets <-  data%>%
    group_by(date,term,locations)%>%
    group_split()
  for(i in 1:length(buckets)){
    bucket <- buckets[[i]]
    flag_1_data <- filter(bucket, political_difference == flag_1,  id==1)
    flag_2_data <- filter(bucket, political_difference == flag_2,id==2)
    if (!is.null(flag_1_b)){
      flag_1_b_data <- filter(bucket, (political_difference == flag_1_b)&( id==1))
      flag_1_data = bind_rows(flag_1_data,flag_1_b_data)
    }
    if (!is.null(flag_2_b)){
      flag_2_b_data <- filter(bucket, (political_difference == flag_2_b)&( id==2))
      flag_2_data = bind_rows(flag_2_data,flag_2_b_data)
    }
    nr_select <- min(nrow(flag_1_data),nrow(flag_2_data))
    if (nr_select > 0){
      selected_flag_1 <-  sample(1:nrow(flag_1_data), nr_select, replace=FALSE)
      selected_flag_2 <-  sample(1:nrow(flag_2_data), nr_select, replace=FALSE)
      new <-  bind_rows(new, flag_1_data[selected_flag_1,],flag_2_data[selected_flag_2,])
    }
    
  }
  return(new)
}
even_data_location <- function(flag_1,flag_2, data,flag_1_b=NULL,flag_2_b=NULL){
  new <- tibble()
  buckets <-  data%>%
    group_by(date,term,political_difference)%>%
    group_split()
  for(i in 1:length(buckets)){
    bucket <- buckets[[i]]
    flag_1_data <- filter(bucket, locations == flag_1,  id==1)
    flag_2_data <- filter(bucket, locations == flag_2,id==2)
    if (!is.null(flag_1_b)){
      flag_1_b_data <- filter(bucket, (locations == flag_1_b)&( id==1))
      flag_1_data = bind_rows(flag_1_data,flag_1_b_data)
    }
    if (!is.null(flag_2_b)){
      flag_2_b_data <- filter(bucket, (locations == flag_2_b)&( id==2))
      flag_2_data = bind_rows(flag_2_data,flag_2_b_data)
    }
    nr_select <- min(nrow(flag_1_data),nrow(flag_2_data))
    if (nr_select > 0){
      selected_flag_1 <-  sample(1:nrow(flag_1_data), nr_select, replace=FALSE)
      selected_flag_2 <-  sample(1:nrow(flag_2_data), nr_select, replace=FALSE)
      new <-  bind_rows(new, flag_1_data[selected_flag_1,],flag_2_data[selected_flag_2,])
    }
    
  }
  return(new)
}

test_results <- tibble()
set.seed(1001)
equalized_data <- even_data('lc','rl',adjusted)%>%
  mutate(rbo_title = as.numeric(rbo_title))%>%
  mutate(political_difference = as.factor(political_difference))
t_1 <- with(equalized_data, t.test(rbo_title[(political_difference=='lc')], rbo_title[(political_difference=='rl')]))
print(t_1)
test_results = bind_rows(test_results,tibble(hypothesis=c('H1'),p_val = c(t_1$p.value)))

equalized_data <- even_data('rc','rl',adjusted)%>%
  mutate(rbo_title = as.numeric(rbo_title))%>%
  mutate(political_difference = as.factor(political_difference))
t_2 <- with(equalized_data, t.test(rbo_title[(political_difference=='rc')], rbo_title[(political_difference=='rl')]))
print(t_2)
test_results = bind_rows(test_results,tibble(hypothesis=c('H2'),p_val = c(t_2$p.value)))

equalized_data <- even_data('c','r',adjusted)%>%
  mutate(rbo_title = as.numeric(rbo_title))%>%
  mutate(political_difference = as.factor(political_difference))
t_3 <- with(equalized_data, t.test(rbo_title[(political_difference=='c')], rbo_title[(political_difference=='r')]))
print(t_3)
test_results = bind_rows(test_results,tibble(hypothesis=c('H3'),p_val = c(t_3$p.value)))

equalized_data <- even_data('c','l',adjusted)%>%
  mutate(rbo_title = as.numeric(rbo_title))%>%
  mutate(political_difference = as.factor(political_difference))
t_4 <- with(equalized_data, t.test(rbo_title[(political_difference=='c')], rbo_title[(political_difference=='l')]))                   
print(t_4)
test_results = bind_rows(test_results,tibble(hypothesis=c('H4'),p_val = c(t_4$p.value)))

equalized_data <- even_data('r','l',adjusted)%>%
  mutate(rbo_title = as.numeric(rbo_title))%>%
  mutate(political_difference = as.factor(political_difference))

t_5 <- with(equalized_data, t.test(rbo_title[(political_difference=='l')], rbo_title[(political_difference=='r')]))                   
print(t_5)
test_results = bind_rows(test_results,tibble(hypothesis=c('H5'), p_val = c(t_5$p.value)))

equalized_data <- even_data('r','rl',adjusted)%>%
  mutate(rbo_title = as.numeric(rbo_title))%>%
  mutate(political_difference = as.factor(political_difference))
t_6 <- with(equalized_data, t.test(rbo_title[(political_difference=='r')], rbo_title[(political_difference=='rl')])) 
print(t_6)
test_results = bind_rows(test_results,tibble(hypothesis=c('H6'),p_val = c(t_6$p.value)))

equalized_data <- even_data('l','rl',adjusted)%>%
  mutate(rbo_title = as.numeric(rbo_title))%>%
  mutate(political_difference = as.factor(political_difference))

t_7<- with(equalized_data, t.test(rbo_title[(political_difference=='l')], rbo_title[(political_difference=='rl')]))  
print(t_7)
test_results = bind_rows(test_results,tibble(hypothesis=c('H7'),p_val = c(t_7$p.value)))


equalized_data <- even_data('r','rl',adjusted, 'l')%>%
  mutate(rbo_title = as.numeric(rbo_title))%>%
  mutate(political_difference = as.factor(political_difference))

t_8 <- with(equalized_data, t.test(rbo_title[(political_difference!='rl')], rbo_title[(political_difference=='rl')]))  
print(t_8)
test_results = bind_rows(test_results,tibble(hypothesis=c('H8'),p_val = c(t_8$p.value)))


equalized_data <- even_data('l','r',adjusted,'r','l')%>%
  mutate(rbo_title = as.numeric(rbo_title))%>%
  mutate(political_difference = as.factor(political_difference))

t_9 <- with(equalized_data, t.test(rbo_title[id==1], rbo_title[id==2]))
print(t_9)
test_results = bind_rows(test_results,tibble(hypothesis=c('H9'),p_val = c(t_9$p.value)))



equalized_data <- even_data_location('H','HP',adjusted, 'P')%>%
  mutate(rbo_title = as.numeric(rbo_title))%>%
  mutate(locations = as.factor(locations))%>%
  mutate(political_difference = as.factor(political_difference))%>%
  mutate(delta_t = as.numeric(delta_t))

t_10 <- with(equalized_data, t.test(rbo_title[(id==1)&(locations=='H'|locations=='P')],rbo_title[(id==2)&(locations=='HP')]))
print(t_10)
test_results = bind_rows(test_results,tibble(hypothesis=c('H10'),p_val = c(t_10$p.value)))

t_11 <- with(equalized_data, t.test(delta_t[(id==1)&(locations=='H'|locations=='P')],delta_t[(id==2)&(locations=='HP')]))
print(t_11)

test_results = bind_rows(test_results,tibble(hypothesis=c('H11'),p_val = c(t_11$p.value)))







set.seed(1002)
library(VGAM)
library(AER)
equalized_data <- mutate(equalized_data,reduced_locations=ifelse(locations=='HP','HP','same'))%>%
  droplevels()

tobit_test <-vglm(rbo_title~reduced_locations+delta_t, VGAM::tobit(Lower = 0, type.f = "cens"),data = equalized_data)
summary(tobit_test)
base_model <- update(tobit_test,.~1)
pseudo_r_squared <- 1 - as.vector(logLik(tobit_test)/logLik(base_model))

ggplot(equalized_data,
       aes(rbo_title, colour=reduced_locations))+
  xlim(0,1)+
  labs(x = TeX("RBO"),  
       y = 'count',
       title = "Frequency Plot of RBOs for Searches Conducted in Matching or Differing Locations")+
  geom_freqpoly(binwidth=0.05)+
  theme(text= element_text(size=10, family="LM Roman 10"))

ggsave(
  filename='RBO_Loc.png',
  plot = last_plot(),
  path = "/Graphs",
  scale = 1,
  width = 5*(1+sqrt(5)),
  height = 10,
  units = c("cm"),
  dpi = 500,
)





buckets <- reduced_data%>%
  group_by(date,search_term,days)%>%
  group_split()

data_for_plotting <- tibble()

for(i in 1:length(buckets)){
  bucket <- buckets[[i]]
  term = as.character(pull(bucket[1,'search_term']))
  days = as.numeric(pull(bucket[1,'days']))
  adj_days = as.numeric(pull(bucket[1,'adjusted_days']))
  bucket_date = as.character.Date(pull(bucket[1,'date']))
  control_subset <- filter(bucket, (flag =='control'))%>%
    group_by(bot_id,time)%>%
    group_split()
  r_subset <- filter(bucket, (flag =='right'))%>%
    group_by(bot_id,time)%>%
    group_split()
  l_subset <- filter(bucket, (flag =='left'))%>%
    group_by(bot_id,time)%>%
    group_split()
  training_1 <- c(control_subset,r_subset,l_subset)
  
  if ((length(control_subset)>1)&(length(r_subset)>1)&(length(l_subset)>1)){
    for(k in 1:(length(training_1)-1)){
      for(j in (k+1):length(training_1)){
        sim = SimilarityMeasure(training_1[[k]], training_1[[j]])
        entry <- c('term'=term,'date'=bucket_date, 'days' = days,'adjusted_days'=adj_days, sim)
        data_for_plotting <- bind_rows(data_for_plotting, entry)
      }}
  }
}



buckets <-  data_for_plotting%>%
  mutate(delta_t = as.numeric(delta_t))%>%
  filter(delta_t<=30)%>%
  group_by(date,term,locations)%>%
  group_split()
new <- tibble()
for(i in 1:length(buckets)){
  bucket <- buckets[[i]]
  chunk_1 <- filter(bucket, political_difference == 'l')
  chunk_2 <- filter(bucket, political_difference == 'r')
  chunk_3 <- filter(bucket, political_difference == 'rl')
  chunk_4 <- filter(bucket, political_difference == 'c')
  chunk_5 <- filter(bucket, political_difference == 'rc')
  chunk_6 <- filter(bucket, political_difference == 'lc')

  nr_select <- min(nrow(chunk_1),nrow(chunk_2),nrow(chunk_3),nrow(chunk_4),nrow(chunk_5),nrow(chunk_6))
  if (nr_select > 0){
    selected_chunk_1 <-  sample(1:nrow(chunk_1), nr_select, replace=FALSE)
    selected_chunk_2 <-  sample(1:nrow(chunk_2), nr_select, replace=FALSE)
    selected_chunk_3 <-  sample(1:nrow(chunk_3), nr_select, replace=FALSE)
    selected_chunk_4 <-  sample(1:nrow(chunk_4), nr_select, replace=FALSE)
    selected_chunk_5 <-  sample(1:nrow(chunk_5), nr_select, replace=FALSE)
    selected_chunk_6 <-  sample(1:nrow(chunk_6), nr_select, replace=FALSE)
    
    new <-  bind_rows(new, chunk_1[selected_chunk_1,], chunk_2[selected_chunk_2,], chunk_3[selected_chunk_3,], chunk_4[selected_chunk_4,], chunk_5[selected_chunk_5,], chunk_6[selected_chunk_6,])
  }
}
new <- new%>%
  mutate(term = as.factor(term))%>%
  mutate(jaccard_title = as.numeric(jaccard_title))%>%
  mutate(jaccard_domain = as.numeric(jaccard_domain))%>%
  mutate(edit_distance_title = as.numeric(edit_distance_title))%>%
  mutate(edit_distance_domain = as.numeric(edit_distance_domain))%>%
  mutate(rbo_title = as.numeric(rbo_title))%>%
  mutate(rbo_domain = as.numeric(rbo_domain))%>%
  mutate(political_difference = as.factor(political_difference))%>%
  mutate(locations = as.factor(locations))%>%
  mutate(same_bot = as.logical(same_bot))%>%
  mutate(delta_t = as.numeric(delta_t))%>%
  mutate(diff_unique_domain_bias = as.numeric(diff_unique_domain_bias))%>%
  mutate(date = as.Date(date))%>%
  mutate(days=as.numeric(days))%>%
  mutate(adjusted_days = as.numeric(adjusted_days))%>%
  mutate(rbo_title= 1-rbo_title)%>%
  mutate(rbo_domain= 1-rbo_domain)


ggplot(new, aes(x=political_difference, y=jaccard_title))+
  geom_boxplot()+
  coord_flip()+
  scale_x_discrete(breaks=c("rl","rc","r","lc","l","c"),
                     labels=c("right-left", "right-control", "right-right","left-control","left-left","control-control"))+
  labs(y = TeX("Jaccard Index $J(\\Theta_s,\\Theta_{s'})$"),  
       x = TeX("Political affiliations of bots conducting searches $s$ and $s'$"),
       title = "Jaccard Index for Combinations of Bot Groups")+
  theme(text= element_text(size=10, family="LM Roman 10"))
  
ggsave(
  filename='jaccard.png',
  plot = last_plot(),
  path = "Graphs",
  scale = 1,
  width = 5*(1+sqrt(5)),
  height = 10,
  units = c("cm"),
  dpi = 500,
)


ggplot(new, aes(x=political_difference, y=edit_distance_title))+
  geom_boxplot()+
  coord_flip()+
  scale_x_discrete(breaks=c("rl","rc","r","lc","l","c"),
                   labels=c("right-left", "right-control", "right-right","left-control","left-left","control-control"))+
  labs(y = TeX("Edit Distance $E(\\Theta_s,\\Theta_{s'})$"),  
       x = TeX("Political affiliations of bots conducting searches $s$ and $s'$"),
       title = "Edit Distance for Combinations of Bot Groups")+
  theme(text= element_text(size=10, family="LM Roman 10"))

ggsave(
  filename='edit.png',
  plot = last_plot(),
  path = "Graphs",
  scale = 1,
  width = 5*(1+sqrt(5)),
  height = 10,
  units = c("cm"),
  dpi = 500,
)

ggplot(new, aes(x=political_difference, y=rbo_title))+
  geom_boxplot()+
  coord_flip()+
  scale_x_discrete(breaks=c("rl","rc","r","lc","l","c"),
                   labels=c("right-left", "right-control", "right-right","left-control","left-left","control-control"))+
  labs(y = TeX("RBO $R(\\Theta_s,\\Theta_{s'})$"),  
       x = TeX("Political affiliations of bots conducting searches $s$ and $s'$"),
       title = "RBO for Combinations of Bot Groups")+
  theme(text= element_text(size=10, family="LM Roman 10"))

ggsave(
  filename='rbo.png',
  plot = last_plot(),
  path = "Graphs",
  scale = 1,
  width = 5*(1+sqrt(5)),
  height = 10,
  units = c("cm"),
  dpi = 500,
)





