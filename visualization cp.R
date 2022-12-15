t <- read.csv("winequality-red.csv")

t$quality[t$quality=="3"] <- 4
t$quality[t$quality=="8"] <- 7
t$quality <- as.factor(t$quality)
library(caret)
cat("\n after Upsampling\n")
t <- upSample(t[,-12],t$quality,yname="quality")

library(ggplot2)
library(ggridges)

#options(digits = 3)

# data Visualization

sum(is.na(t))

str(t)

summary(t)

ggplot(data = t) + geom_bar(aes(quality), fill = "blue") + labs(title = "Quality Of Wine") + theme_bw()

t %>% pivot_longer(cols = -12, 
                  names_to = "Feature", 
                  values_to = "Value") %>%
  # Create the box plot
  ggplot(aes(x = quality, y= Value, fill = quality)) +
  geom_boxplot() +
  # Format labels
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ggtitle("Red wine quality by feature") +
  # Create grid by feature
  facet_wrap(. ~ Feature, scales = "free")

t <- t %>% 
  mutate(quality2 = factor(case_when(
    quality %in% c( "4") ~ "low",
    quality %in% c("5", "6") ~ "medium",
    quality %in% c("7") ~ "high"),
    levels = c("low", "medium", "high")))

 ggplot(t, aes(quality2, fill = quality2)) + geom_bar()

t %>% 
  pivot_longer(cols = -c(12:13), 
               names_to = "Feature", 
               values_to = "Value") %>%
  # Create the box plot
  ggplot(aes(x = quality2, y= Value, fill = quality2)) +
  geom_boxplot() +
  # Format labels
  #  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ggtitle("Red wine quality by feature") +
  # Create grid by feature
  facet_wrap(. ~ Feature, scales = "free", ncol = 3, shrink = FALSE)


                                                                                     