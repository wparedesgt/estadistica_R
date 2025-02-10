#### Introduction to Exploratory Data Analysis ###

library(rafalib)
library(tidyverse)
library(UsingR)

x <- father.son$fheight

### 
round(sample(x,20), 1)

####

hist(x, breaks = seq(floor(min(x)), ceiling(max(x))), 
     main = 'Height histogram', xlab = 'Height in inches')

xs <- seq(floor(min(x)), ceiling(max(x)), 0.1)

plot(xs, ecdf(x)(xs), type = 'l', 
     xlab = 'Height in inches', ylab = 'F(x)')
####

mean(x>70)
1-pnorm(70, mean(x), sd(x))

####

ps <- seq(0.01, 0.99, 0.01)
qs <- quantile(x, ps)
normalqs <- qnorm(ps, mean(x), sd(x))
plot(normalqs, qs, xlab = 'Normal percentiles', ylab = 'Height percentiles')
abline(0,1)

###
qqnorm(x)
qqline(x)


#### Ejercicios ####

load('datos/skew.RData')

data <- dat
# Step 1: QQ-plots for all columns (V1 to V9)
par(mfrow = c(3, 3)) # Set up a 3x3 grid for QQ-plots
for (i in 1:9) {
  qqnorm(data[[paste0("V", i)]], main = paste("QQ-Plot of V", i))
  qqline(data[[paste0("V", i)]]) # Add the reference line
}
par(mfrow = c(1, 1)) # Reset the grid to one plot at a time

# Step 2: Histograms for all columns (V1 to V9)
par(mfrow = c(3, 3)) # Set up a 3x3 grid for histograms
for (i in 1:9) {
  hist(data[[paste0("V", i)]], 
       main = paste("Histogram of V", i), 
       xlab = paste("V", i), 
       col = "lightblue", 
       border = "black")
}
par(mfrow = c(1, 1)) # Reset the grid to one plot at a time

# Step 3: Histograms for skewed columns (V8 and V9)
hist(data$V8, 
     main = "Histogram of V8 (Negative Skew)", 
     xlab = "V8", 
     col = "lightblue", 
     border = "black")

hist(data$V9, 
     main = "Histogram of V9 (Positive Skew)", 
     xlab = "V9", 
     col = "lightblue", 
     border = "black")

#####
dev.off()

data("exec.pay")
hist(exec.pay)
qqnorm(exec.pay)
qqline(exec.pay)

boxplot(exec.pay, ylab = '10,000 of dollars', ylim = c(0,400))
mean(exec.pay)
median(exec.pay)

### Ejercicio Boxplots ###

data("InsectSprays")
head(InsectSprays)


boxplot(split(InsectSprays$count, InsectSprays$spray))
boxplot(InsectSprays$count~InsectSprays$spray)

boxplot(count ~ spray, data = InsectSprays,
        xlab = "Type of spray", ylab = "Insect count",
        main = "InsectSprays data", varwidth = TRUE, col = "lightgray")

data("nym.2002")
head(nym.2002)

boxplot(time~gender, data = nym.2002)

hist_male <- nym.2002 %>% filter(gender == 'Male') 
hist_female <- nym.2002 %>% filter(gender == 'Female')

hist(hist_male$time)
hist(hist_female$time)


mypar(1,3)

males <- nym.2002 %>%
  filter(gender == "Male") %>%
  pull(time)

females <- nym.2002 %>% 
  filter(gender=="Female") %>% 
  pull(time)

boxplot(females, males)
hist(females,xlim=c(range( nym.2002$time)))
hist(males,xlim=c(range( nym.2002$time)))

############ QUIZ ##########################

library(tidyverse)
library(dslabs)
data("heights")
h <- heights$height

hist(h)
boxplot(h)
