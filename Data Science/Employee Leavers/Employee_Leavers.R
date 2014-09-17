# Draft 1: Looking at 2005
# Load Data
rawData <- read.csv("HR_Extract_Year.csv")
hrData <- rawData[rawData$CAL_YEAR == 2005,]

# Convert salary to £k and filter anomalously high salaried employees
hrData$SALARY <- hrData$SALARY/1000
hrData <- hrData[which(hrData$SALARY < 2000),]
hrData$SERVICE_YEARS <- round(hrData$SERVICE_DAYS/365,2)
hrData$WEEKLY_HOURS <- hrData$HOURS

# Use binomial regression to identify valid predictors
allModel <- glm(LEAVE_FLAG ~ SALARY + 
                      SERVICE_YEARS + 
                      AGE + 
                      WEEKLY_HOURS, data = hrData, family = "binomial")
summary(allModel)$coefficients

# Filter on p-value (from z test) of < 0.1
# SALARY, SERVICE_DAYS
# PERIOD_OF_WORK_BANDS 1-2, 10-20, 2-5, 5-10,
# AGE (but no age bands)
# HOURS

# Investigate some of the relationships on their own
serviceModel <- glm(LEAVE_FLAG ~ SERVICE_YEARS, data = hrData, family = "binomial")
summary(serviceModel)$coefficients
exp(serviceModel$coefficients)
exp(confint(serviceModel))

ageModel <- glm(LEAVE_FLAG ~ AGE, data = hrData, family = "binomial")
summary(ageModel)$coefficients
exp(ageModel$coefficients)
exp(confint(ageModel))

# Even though on it's on it is within our bounds of statistical significance,
# it doesn't hold up with the other variables included. Besides it has a
# surprisingly small effect on the likelihood of an employee leaving.
salaryModel <- glm(LEAVE_FLAG ~ SALARY, data = hrData, family = "binomial")
summary(salaryModel)$coefficients
exp(salaryModel$coefficients)
exp(confint(salaryModel))

# On it's own shows no impact on the data. Neither it is doing anything useful
# when related to age
hoursModel <- glm(LEAVE_FLAG ~ WEEKLY_HOURS, data = hrData, family = "binomial")
summary(hoursModel)$coefficients
exp(hoursModel$coefficients)
exp(confint(hoursModel))

goodModel <- glm(LEAVE_FLAG ~  
                      SERVICE_YEARS + 
                      AGE, data = hrData, family = "binomial")
summary(goodModel)$coefficients
exp(goodModel$coefficients)
exp(confint(goodModel))

# Draft 2: Looking at all employees between 2001 and 2009
library(ggplot2)

# Load Data
hrData <- read.csv("HR_Extract_Emp.csv")

# Convert salary to £k and filter anomalously high salaried employees
hrData$SALARY <- hrData$SALARY/1000
hrData <- hrData[which(hrData$SALARY < 2000),]
hrData$SERVICE_YEARS <- round(hrData$SERVICE_DAYS/365,2)
hrData$WEEKLY_HOURS <- hrData$HOURS

# Use binomial regression to identify valid predictors
allModel <- glm(LEAVE_FLAG ~ SALARY + 
                      SERVICE_YEARS + 
                      AGE + 
                      WEEKLY_HOURS, data = hrData, family = "binomial")
summary(allModel)$coefficients
exp(allModel)$coefficients


# Salary is statistically insignificant and has little bearing on odds: Exclude

goodModel <- glm(LEAVE_FLAG ~ 
                      SERVICE_YEARS + 
                      AGE + 
                      WEEKLY_HOURS, data = hrData, family = "binomial")
summary(goodModel)$coefficients
coefs <- exp(goodModel$coefficients)
confInts <- exp(confint(goodModel))

# Plot fit against real data
beta0 <- summary(goodModel)$coefficients[1]
beta1 <- summary(goodModel)$coefficients[2]
beta2 <- summary(goodModel)$coefficients[3]
beta3 <- summary(goodModel)$coefficients[4]

# Set variables, cleaning for missing values
x1 <- hrData[which(!is.na(hrData$WEEKLY_HOURS)),"SERVICE_YEARS"]
x2 <- hrData[which(!is.na(hrData$WEEKLY_HOURS)),"AGE"]
x3 <- hrData[which(!is.na(hrData$WEEKLY_HOURS)),"WEEKLY_HOURS"]

gamma <- beta0 + beta1*x1 + beta2*x2 + beta3*x3

y_fit <- exp(gamma) / (1 + exp(gamma))
y_obs <- hrData[which(!is.na(hrData$WEEKLY_HOURS)),"LEAVE_FLAG"]

g <-  ggplot() + 
      geom_point(data = plotDF, aes(x = gamma, y = y_obs, col = "1"), 
                 alpha = 0.1) + 
      geom_line(data = plotDF, aes(x = gamma, y = y_fit, col = "2")) + 
      labs(title=expression(bold("Fit against Observed Values")),
           x=expression(beta[0]+x[1]*beta[1]+x[2]*beta[2]+x[3]*beta[3]), 
           y="Probability of Leaving") +
      scale_colour_manual(name = "Plot",
                          values =c('black', 'red'), 
                          labels = c('Observed', 'Fit'))
plot(g)

# Interpretation
# The model can be used to predict a probability of leaving via the equation:
