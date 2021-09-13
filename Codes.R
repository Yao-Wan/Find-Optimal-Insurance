library(ggplot2)
library(scales)
library(reshape2)
library(plotly)

# Data Analysis

# cost of replacement of each boeing aircraft
b_cost <- 56400000
# cost of replacement of each airbus aircraft
a1_cost <- 78900000
a2_cost <- 88500000
# total insured value for boeing
b_val <- 47 * b_cost
# total insured value for airbus
a_val <- 15 * a1_cost + 24 * a2_cost
total_val <- a_val+b_val
avg_flying_days <- 342
p_accident <- 1/5000000

b_total_flights <- 6 * avg_flying_days * 47
a1_total_flights <- ceiling(2.25 * avg_flying_days * 15)
a2_total_flights <- 2 * avg_flying_days * 24



# Simulating the number of crashes in a given year for each type of aircraft 
n <- 1000000
set.seed(123)
for (i in 1:5){
  assign(paste("b_sims_year", i, sep = ""), rbinom(n, b_total_flights, p_accident))   
  assign(paste("a1_sims_year", i, sep = ""), rbinom(n, a1_total_flights, p_accident)) 
  assign(paste("a2_sims_year", i, sep = ""), rbinom(n, a2_total_flights, p_accident))
  assign(paste("damage", i, sep = ""), runif(n, min = 1000000, max = 5000000))
}

# Estimating the cost of aircraft loss using the simulated numbers 
cost1<-b_sims_year1*b_cost+a1_sims_year1*a1_cost+a2_sims_year1*a2_cost
cost2<-b_sims_year2*b_cost+a1_sims_year2*a1_cost+a2_sims_year2*a2_cost
cost3<-b_sims_year3*b_cost+a1_sims_year3*a1_cost+a2_sims_year3*a2_cost
cost4<-b_sims_year4*b_cost+a1_sims_year4*a1_cost+a2_sims_year4*a2_cost
cost5<-b_sims_year5*b_cost+a1_sims_year5*a1_cost+a2_sims_year5*a2_cost

all_damages <- damage1+damage2+damage3+damage4+damage5
all_costs <- cost1+cost2+cost3+cost4+cost5

# RCNC1 - 1 year
premium_r1 <- 0.0045*total_val
claims_r1 <- 0.9 *(cost1 + damage1)
cashflow_r1 <- premium_r1 - claims_r1 + cost1 + damage1

# RCNC1 - 5 years
premium_r1_5 <- 0.0045*total_val*5
claims_r1_5 <- 0.9 *(all_costs+all_damages)
rebate_r1_5 <- (premium_r1_5 - claims_r1_5) * 0.2
cashflow_r1_5 <- premium_r1_5 - claims_r1_5 - rebate_r1_5 + all_costs + all_damages

# RCNC2 - 1 year
premium_r2 <- 0.001*total_val
claims_r2 <- cost1 + damage1
premium_r2_add <- ifelse(claims_r2*0.9 < total_val*0.01, claims_r2*0.9, total_val*0.01)
cashflow_r2 <- premium_r2 - claims_r2 + premium_r2_add + cost1 + damage1

# RCNC2 - 5 years 
premium_r2_5 <- 0.001*total_val*5
claims_r2_5 <- all_costs + all_damages

premium_r2_add_5 <- ifelse((cost1 + damage1)*0.9 < total_val*0.01, (cost1 + damage1)*0.9, 
                           total_val*0.01) + 
  ifelse((cost2 + damage2)*0.9 < total_val*0.01, 
         (cost2 + damage2)*0.9, total_val*0.01) +
  ifelse((cost3 + damage3)*0.9 < total_val*0.01, 
         (cost3 + damage3)*0.9, total_val*0.01) +
  ifelse((cost4 + damage4)*0.9 < total_val*0.01, 
         (cost4 + damage4)*0.9, total_val*0.01) +
  ifelse((cost5 + damage5)*0.9 < total_val*0.01, 
         (cost5 + damage5)*0.9, total_val*0.01)

cashflow_r2_5 <- premium_r2_5 + premium_r2_add_5 - claims_r2_5 + all_costs + all_damages

# CTC - 1 year
premium_ctc <- 13000000
claims_ctc <- ifelse((cost1 + damage1)*0.9 < 80000000, (cost1 + damage1)*0.9, 80000000)
cashflow_ctc <- premium_ctc -claims_ctc + cost1 + damage1

# CTC - 5 years
premium_ctc_5 <- 13000000*5
claims_ctc_5 <- ifelse((cost1+damage1)*0.9 < 80000000, (cost1+damage1)*0.9, 80000000)+
  ifelse((cost2 + damage2)*0.9 < 80000000, (cost2 + damage2)*0.9, 80000000) +
  ifelse((cost3 + damage3)*0.9 < 80000000, (cost3 + damage3)*0.9, 80000000) +
  ifelse((cost4 + damage4)*0.9 < 80000000, (cost4 + damage4)*0.9, 80000000) +
  ifelse((cost5 + damage5)*0.9 < 80000000, (cost5 + damage5)*0.9, 80000000)

cashflow_ctc_5 <- premium_ctc_5 - claims_ctc_5 + all_costs + all_damages

# HIC - 1 year
premium_hic <- 0.00165*total_val
claims_hic <- ifelse((cost1 + damage1) < 24000000, 0, (cost1 + damage1) - 24000000)
cashflow_hic <- premium_hic - claims_hic + cost1 + damage1

# HIC - 5 years
premium_hic_5 <- 0.00165*total_val*5
claims_hic_5 <- ifelse((cost1 + damage1) < 24000000, 0, (cost1 + damage1) - 24000000) +
  ifelse((cost2 + damage2) < 24000000, 0, (cost2 + damage2) - 24000000) +
  ifelse((cost3 + damage3) < 24000000, 0, (cost3 + damage3) - 24000000) +
  ifelse((cost4 + damage4) < 24000000, 0, (cost4 + damage4) - 24000000) +
  ifelse((cost5 + damage5) < 24000000, 0, (cost5 + damage5) - 24000000)
rebate_ctc_5 <- (premium_ctc_5 - claims_hic_5)*0.035
cashflow_hic_5 <- premium_hic_5 - claims_hic_5 - rebate_ctc_5 + all_costs + all_damages

# summary
cashflows1 <- list(cashflow_r1, cashflow_r2, cashflow_ctc, cashflow_hic)
cashflows5 <- list(cashflow_r1_5, cashflow_r2_5, cashflow_ctc_5, cashflow_hic_5)
mean <- c()
sd <- c()
median <- c()
min <- c()
max <- c()
over_37 <- c()
for (i in cashflows1){
  mean <- c(mean, mean(i))
  sd <- c(sd, sd(i))
  median <- c(median, median(i))
  min <- c(min, min(i))
  max <- c(max, max(i))
  over_37 <- c(over_37, mean(i >= 37000000))
}
summ1 <- data.frame(mean, sd, median, min, max, over_37, 
                    row.names = c('r1', 'r2', 'ctc', 'hic'))

mean <- c()
sd <- c()
median <- c()
min <- c()
max <- c()
for (i in cashflows5){
  mean <- c(mean, mean(i))
  sd <- c(sd, sd(i))
  median <- c(median, median(i))
  min <- c(min, min(i))
  max <- c(max, max(i))
}
summ5 <- data.frame(mean, sd, median, min, max, 
                    row.names = c('r1', 'r2', 'ctc', 'hic'))


#### Distribution of aircraft replacement cost and incidental damage cost 


# 1-year Aircraft Loss 
par( mfrow=c(1,2)) 
options(scipen=999)
hist(damage1, xlab="Damage Cost", main = '1-year Damage Cost', font.main=1)
hist(cost1, xlab="Replacement Cost", main = '1-year Replacement Cost', font.main=1)



# 5-year Aircraft Loss
par( mfrow=c(1,2)) 
hist(all_damages, xlab="Damage Cost", main = '5-Year Damage Cost',font.main=2)
hist(all_costs, xlab="Replacement Cost", main = '5-Year Replacement Cost',font.main=2)



#### Total Loss under each insurance policy in Year 1 


# Boxplot for all four plans in 1 year
year_1_plot = data.frame(RCNC1 = cashflow_r1, RCNC2 = cashflow_r2, CTC = cashflow_ctc,
                         HIC = cashflow_hic)
options(scipen=999)
par( mfrow=c(1,2)) 
boxplot(year_1_plot,col = "gray50", names = c("RCNC1","RCNC2","CTC","HIC"), 
        xlab="Insurance Policy", ylab = "Total Loss($)", main = 'With Outliers')

year_1_plot = data.frame(RCNC1=cashflow_r1, RCNC2=cashflow_r2, CTC=cashflow_ctc, 
                         HIC=cashflow_hic)
options(scipen=999)

boxplot(year_1_plot,  outline = FALSE,col = "gray50", 
        names = c("RCNC1","RCNC2","CTC","HIC"), xlab="Insurance Policy", 
        ylab = "Total Loss($)", main = 'Without Outliers')



#### Total Loss under each insurance policy for 5-year period 


par( mfrow=c(1,2)) 
year_5_plot = data.frame(RCNC1 = cashflow_r1_5,RCNC2 = cashflow_r2_5,
                         CTC = cashflow_ctc_5,HIC = cashflow_hic_5)

boxplot(year_5_plot,col = "gray50", names = c("RCNC1","RCNC2","CTC","HIC"), 
        xlab="Insurance Policy", ylab = "Total Loss($)", main = 'With Outliers')

year_5_plot = data.frame(RCNC1 = cashflow_r1_5, RCNC2 = cashflow_r2_5,
                         CTC = cashflow_ctc_5, HIC = cashflow_hic_5)
boxplot(year_5_plot, outline = FALSE, col = "gray50", 
        names = c("RCNC1","RCNC2","CTC","HIC"), xlab="Insurance Policies", 
        ylab = "Total Loss($)", main = 'Without Outliers')



#### Distribution of Total Loss for Different Insurance Policies over 5 years 



# Histogram for total Loss distribution for different insurance policies in 5 years
year_5_plot = data.frame(RCNC1 = cashflow_r1_5, RCNC2 = cashflow_r2_5,
                         CTC = cashflow_ctc_5, HIC = cashflow_hic_5)

stacked_data <- melt(year_5_plot[,c("RCNC1", "RCNC2", "CTC","HIC" )], 
                     variable.name = "Insurance_Policy", value.name = "Cashflow")

p3 <- ggplot(stacked_data, aes(Cashflow,fill = Insurance_Policy))+ 
  geom_histogram(colour="grey50",alpha = .2,position = "identity") + 
  scale_fill_manual(values = c("blue", "green", "red", "yellow"))

p3 <- p3 + 
  labs(title="Total loss distribution for different insurance policies for 5 years") +
  labs(x="Total Loss", y="Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))+ theme_bw()

fig <- ggplotly(p3)
fig


#### Density Plot of Total Loss for Different Insurance Policies over 5 years


p <- ggplot(stacked_data, aes(x = Cashflow,fill = Insurance_Policy)) + 
  geom_density( colour="grey50",alpha = .2) + 
  scale_fill_manual(values = c("blue", "green", "red", "yellow")) + theme_bw()

p <- p + labs(title="Density for different insurance policies for 5 years") +
  labs(x="Total Loss", y="Density") + theme(plot.title = element_text(hjust = 0.5))

fig <- ggplotly(p)
fig


#### Distribution of Total Loss for Insurance Policy HIC in Year 1 


# total Loss distribution for insurance policy (HIC) in Year 1
yr_1 = data.frame(HIC = cashflow_hic)

p3 <- ggplot(yr_1, aes(HIC))+ geom_histogram(colour="gray50",alpha = .2,
                                             position = "identity") + 
  scale_fill_manual(values = c("yellow"))

p3 <- p3 + 
  labs(title="Distribution of Total Loss for Insurance Policy HIC in Year 1") +
  labs(x="Total Loss", y="Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))+ theme_bw()

show(p3)


#### Density Plot of Total Loss for Insurance Policy HIC in Year 1 

# Density plot for total loss distribution for insurance policy (HIC) in 1 year
yr_1 <- data.frame(HIC = cashflow_hic)

p <- ggplot(yr_1, aes(HIC)) + geom_density(colour="grey50",alpha = .2) + 
  scale_fill_manual(values = c("yellow")) + theme_bw()
p <- p + labs(title="Density for different insurance policy (HIC) in 1 years") +
  labs(x="Total Loss", y="Density") + theme(plot.title = element_text(hjust = 0.5))

show(p)



