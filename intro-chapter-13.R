install.packages(GGally)
library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
lsmodel0 <- lm(formula = height ~ 1, data = darwin)
mean(darwin$height)
#making model summaries
summary(lsmodel0)
mean(darwin$height)
#comparing means
lsmodel1 <- lm(height ~ type, data=darwin)
broom::tidy(lsmodel1)
darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height))
#fuller summary of the module
summary(lsmodel1)
#superimposing calculated means onto a plot
darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()
confint(lsmodel1)
GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95)
#getting the other treatment mean and S.E
darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()

means <- emmeans::emmeans(lsmodel1, specs = ~ type)

means
means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))
#assumption checking
performance::check_model(lsmodel1)
performance::check_model(lsmodel1, check="homogeneity")
darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)
