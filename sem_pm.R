# rm(list=ls())

# libraries
library(tidySEM)
library(lavaan)
library(readxl)
library(tidyverse)


# read the data
db_pm <- read_excel("data_pm.xlsx", sheet = 1)
db_gr <- read_excel('data_pm.xlsx',sheet=2)


# preprocessing and aggregation of the raw datasets
dbpm <- db_pm %>% # disaggregated dataset
  filter(esclusi_1 == 0) %>% # exclude some outliers
  mutate(RT = ifelse(is.na(RT), 0, RT)) %>% # replace RT's NAs with 0s
  group_by(id_sogg) %>% # group by subject
  dplyr::summarise(pm_rrs = sum(Corrette)/sum(RT)*1e3) %>% # create PM_RRS variable
  mutate(pm_rrs = ifelse(is.nan(pm_rrs),0,pm_rrs)) %>% # replace 0/0 with 0
  left_join(db_gr, by = 'id_sogg') %>% # join with the aggregated database
  dplyr::select(c(id_sogg,pm_rrs,Gruppo,SpanRedux,gng_acc,
                  IE_etero,AFV_etero,Voti, Cp_etero)) %>% # select just the columns I need
  mutate(gng_acc = gng_acc*10/3) %>% # rescale gng_acc (Go NoGo task) in [0-100]
  filter(!is.na(Gruppo)) # remove the row with missing value in group

# rename the columns
names(dbpm)[c(2:9)] <- c('PM','Age','WM_Span','Go_NoGo','EI_etero',
                         'PVA_etero','Grades','PSB_etero')

# definition of the model
mod <- 
' 
Social_Skills =~ EI_etero + PVA_etero  + PSB_etero
Social_Skills ~  PM + Go_NoGo 
PSB_etero ~  PM + Go_NoGo
Grades ~   PM + Social_Skills + WM_Span 
PM ~ Go_NoGo + WM_Span

'

# fitting the model
fit <- sem(mod, dbpm, std.lv = TRUE, se = 'robust', fixed.x = TRUE, 
           group = 'Age', meanstructure = TRUE, std.ov = FALSE, missing = 'ML')

# summary of the model
summary(fit, fit.measures=TRUE, rsquare=TRUE)



# residual correlation between variables
resid(fit, type='cor')


###############---------------###############

# to get a plot with tidysem we have to
# fit two model separately: in this case the multigroup sem
# gives the same results (in terms of coefficients) as
# the two separate sem models, one for each age group


# fitting the model for 8 y.o.
fit8 <- sem(mod, dbpm[dbpm$Age==1,], std.lv = TRUE, se = 'robust', fixed.x = TRUE, 
           meanstructure = TRUE, std.ov = FALSE, missing = 'ML')

# summary of the model (just to make sure it's the same to the previous one)
summary(fit8, fit.measures=TRUE, rsquare=TRUE)

# custom layout of the model
layy <- get_layout('EI_etero','Grades', 'WM_Span',
                   'Social_Skills', 'PM', 'PSB_etero',
                   'PVA_etero','Go_NoGo', '', rows=3)

# prepare graph object
graph_data8 <- prepare_graph(model = fit8, layout = layy)

# keep only regressions' edges
edges(graph_data8) <- edges(graph_data8)[1:12,]

# define labels for the variables
lbls <- c("EI", "Go/No-Go","Grades", "PM", "PSB","PVA",
          "Social Skills \n (teachers' \n evaluation)", "WM Span")
nodes(graph_data8)[,3] <- lbls

# dashed lines for the latent variables
edges(graph_data8)[,'linetype'] <- c(rep(2,2),rep(1,10)) 

# adjust nodes position
nodes(graph_data8)[5,c(5,8,9)] <- c(3,2.6,3.4)
nodes(graph_data8)[8,c(5,8,9)] <- c(5,4.6,5.4) + .2

# adjust edges position
edges(graph_data8)[9,5] <- 'left'
edges(graph_data8)[10,5] <- 'top'
edges(graph_data8)[6,6] <- 'left'
edges(graph_data8)[12,6] <- 'top'

# modifiy edge labels size
edges(graph_data8)[,'label_size'] <- 6
 
# modifiy node labels size
nodes(graph_data8)[,'label_size'] <- 5#c(rep(5,6),4,5)

# plot the graph
plot(graph_data8)



# fitting the model for 12 y.o.
fit12 <- sem(mod, dbpm[dbpm$Age==2,], std.lv = TRUE, se = 'robust', fixed.x = TRUE, 
            meanstructure = TRUE, std.ov = FALSE, missing = 'ML')


# prepare graph object
graph_data12 <- prepare_graph(model = fit12, layout = layy)

# keep only regressions' edges
edges(graph_data12) <- edges(graph_data12)[1:12,]

# define labels for the variables
nodes(graph_data12)[,3] <- lbls

# dashed lines for the latent variables
edges(graph_data12)[,'linetype'] <- c(rep(2,2),rep(1,10)) 

# adjust nodes position
nodes(graph_data12)[5,c(5,8,9)] <- c(3,2.6,3.4)
nodes(graph_data12)[8,c(5,8,9)] <- c(5,4.6,5.4) + .2

# adjust edges position
edges(graph_data12)[9,5] <- 'left'
edges(graph_data12)[10,5] <- 'top'
edges(graph_data12)[6,6] <- 'left'
edges(graph_data12)[12,6] <- 'top'

# modifiy edge labels size
edges(graph_data12)[,'label_size'] <- 6

# modifiy node labels size
nodes(graph_data12)[,'label_size'] <- 5

# plot the graph
plot(graph_data12)




