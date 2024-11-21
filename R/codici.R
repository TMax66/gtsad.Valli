df %>%
 dplyr::select( gruppo, nefa) %>% 
  mutate(nefa = scale(nefa)) %>% 
  group_by(gruppo) %>%
  summarise(
    nefa = mean(nefa, na.rm = TRUE))


df %>%
  mutate(gruppo = ifelse(gruppo == "caso", 1, 0))-> df

summary(lm(nefa ~  gruppo, family = "gaussian" , data =df))

t.test(nefa~gruppo, data = df)


invlogit(1.5)


summary(glm(gruppo ~ nefa, family = "binomial" , data =df))

aov(nefa~gruppo, data = df)





pkg()
library(MASS)
library(mda)
library(FactoMineR)
library(factoextra)
library(psych)


dt <- read_excel(here("dati", "DATASET per articolo_17.10.2024(1).xlsx"))


dt %>%  clean_names() %>% 
  mutate(gruppo = ifelse(gruppo == "caso", 1, 0))-> df

  



df <- df[,  c(1,  13:36)]

 

df<- as.data.frame(df)





# rule for how many components to retain, known as parallel analysis.
# #The scree plot also shows a dashed line at the value 1. Another criterion for component
#retention is to keep all components with an eigenvalue larger or equal to 1, and is known as Kaiser’s
#rule.
 
#In theory, for a set of completely uncorrelated variables, the component eigenvalues should all
#be exactly equal to 1. Unfortunately, this property is only true asymptotically for infinitely large data
#sets. For finite data sets, especially small ones, components with an eigenvalue larger than 1 are
#always observed, even for randomly generated data
#
#This phenomenon is often cited as an example of small-sample bias, and affects not just
#principal components but many other statistics that rely on asymptotic theory . However, when the bias is known, it can be exploited to improve
#Kaiser’s rule, which is the principle behind Horn’s parallel analysis (Horn, 1965)


#library(psych)
#fa.parallel(df[, 13:36]) # Parallel analysis suggests that the number of factors =  5  and the number of components =  4 


## ANALISI CON FACTOMINER-----

res <- PCA(df, ncp = 4, scale.unit = TRUE, quali.sup = 1)

#fviz_screeplot(res, ncp=10)# sostituito da fa.parallel


# grafico variabili e grafico biplot
#
fviz_pca_biplot(res, 
                axes = c(1:2),
                addEllipses = TRUE,
                habillage=df$gruppo,
                col.var = "blue",
                label = "var") +
  scale_color_brewer(palette="Dark2")+
  theme_minimal()


# per ruotare i dati delle variabili 
# t(apply(res$var$coord, 1, function(x) {x/sqrt(res$eig[,1])}))

dt %>%  clean_names() %>% 
  mutate(gruppo = as.factor(gruppo), 
         ) %>% 
  
  bind_cols(
    res$ind$coord
  ) %>%  data.frame() %>% 
  
  mutate(Dim.1 = -1*Dim.1,
         Dim.2 = -1*Dim.2,
         Dim.3 = -1*Dim.3,
         Dim.4 = -1*Dim.4) -> dt_score


mod.com <- glm(gruppo ~ Dim.1+Dim.2+Dim.3+Dim.4 + age_year, family = binomial, data =dt_score)
summary(mod.com)
exp(coef(mod.com))

library(gtsummary)

t1 <- tbl_regression(mod.com, exponentiate = TRUE)


dt_score %>% 
  dplyr::select(gruppo, Dim.1, Dim.2, Dim.3, Dim.4) %>% 
pivot_longer(cols = 2:5, values_to = "loading", names_to = "PC") %>% 
  
  ggplot()+
  aes(x = gruppo, 
      y = loading)+
  facet_wrap(~PC)+
  
  geom_boxplot()



# grafico loading
# 

df.load <- as.data.frame(res$var$coord)

df.load$varNames <- rownames(df.load)

df.load %>%  
  pivot_longer(cols = 1:5, values_to = "loading", names_to = "PC") %>% 
  rename(variabili = varNames ) -> load_plot




ggplot(load_plot)+
  aes(x = variabili, 
      y = loading, 
      ymax = loading)+
  geom_point()+
  geom_linerange(aes(ymin = 0))+
  facet_wrap(~ PC, nrow = 1)+
  coord_flip()+
  ggtitle("variable loadings for principal components")



load_plot %>% 
  group_by(PC) %>% 
  arrange(desc(loading)) %>%  
  filter(PC == "Dim.1", 
         abs(loading) >= 0.4) %>% View()
  
library(flextable)

df.load %>% data.frame() %>% 
  mutate(variabili = rownames(df.load)) %>%  
  dplyr::select(variabili, Dim.1, Dim.2, Dim.3, Dim.4) %>% 
  mutate(across(where(is.numeric), round, 3)) %>% 
  flextable() %>% 
  color(., ~ abs(Dim.1) >= 0.40, ~ Dim.1, color = "blue") %>% 
color(., ~ abs(Dim.2) >= 0.40, ~ Dim.2, color = "blue") %>% 
color(., ~ abs(Dim.3) >= 0.40, ~ Dim.3, color = "blue") %>% 
color(., ~ abs(Dim.4) >= 0.40, ~ Dim.4, color = "blue") 
  


##################################################################



# #ANALISI CON PRCOMP
# 
# 
# prcom<-prcomp(~.-gruppo,df,scale.=T,center=T, retx = TRUE)
# 
# 
# # Screeplot
# screeplot(prcom, npcs = 10, type = "bar")
# 
# fviz_pca_var(prcom)
# 
# 
# # grafici
# library(ggbiplot)
# 
# ncomp = 4
# g <- ggbiplot(prcom, obs.scale = 1, var.scale = 1, 
#               ellipse = FALSE, groups = df$gruppo, circle = TRUE)
# 
# # g <- g + scale_color_discrete(name = "")
# 
# g <- g + theme(legend.direction = "horizontal", legend.position = "top")
# 
# #regvar <- paste(paste("PC", 1:ncomp, SEP =""), collapse = "+")
# 
# #fmla = paste("gruppo ~", regvar)
# 
# 
# 
# 
# # score individuali
#  
# df.projected <- as.data.frame(predict(prcom, df[, -1]))
# df.projected$gruppo <- df$gruppo
# 
# df.projected %>% 
#   pivot_longer(cols = 1:25, names_to = "PC", values_to = "scores" ) %>% 
#   filter(PC %in% c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")) %>%  
#   ggplot()+
#   aes(x = gruppo, 
#       y = scores)+
#   geom_boxplot()+
#   geom_jitter()+
#   facet_wrap(~PC)
# 
# 
# # loading delle variabili sulle PCA
#  
# 
# prcomLoad <- prcom$rotation
# 
# df.prcom <- as.data.frame(prcomLoad)
# 
# df.prcom$varNames <- rownames(df.prcom)
# 
# df.prcom %>% 
#   pivot_longer(cols = 1:25, values_to = "loading", names_to = "PC") %>% 
#   rename(variabili = varNames ) -> load_plot
# 
# ggplot(load_plot %>% 
#          filter(PC %in% c("PC1", "PC2", "PC3", "PC4", "PC5","PC6", "PC7", "PC8", "PC9", "PC10")))+
#   aes(x = variabili, 
#       y = loading, 
#       ymax = loading)+
#   geom_point()+
#   geom_linerange(aes(ymin = 0))+
#   facet_wrap(~ PC, nrow = 1)+
#   coord_flip()+
#   ggtitle("variable loadings for principal components")
#   
# library(flextable)
# 
# df.prcom %>% data.frame() %>% 
#   dplyr::select(varNames, PC1, PC4, PC6) %>% 
#   mutate(across(where(is.numeric), round, 3)) %>% 
#   flextable()
# 
#   
# 
# 
# mod.com <- glm(gruppo ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, family = binomial, data =df.projected)
# 
# 
# 
# 
# 
# 
# 
# #
# 
# 
# 
# 
# df %>% 
#   group_by(gruppo) %>% 
#   summarise(across(where(is.double), ~ mean(.x, na.rm = TRUE))) 
# 
# 
# res <- PCA(df, scale.unit = TRUE, quali.sup = 1)
# 
# # per ruotare i dati delle variabili 
# # t(apply(res$var$coord, 1, function(x) {x/sqrt(res$eig[,1])}))
# 
# dt %>%  clean_names() %>% 
#   mutate(gruppo = as.factor(gruppo), 
#     statoprod = as.factor(statoprod)) %>% 
#   
#   bind_cols(
#     res$ind$coord
#   ) %>%  data.frame()-> dt_score
#   
# 
# mod.com <- glm(gruppo ~ Dim.1+Dim.2+Dim.3+Dim.4, family = binomial, data =dt_score)
# 
# 
# # loading variabili
# #
# 
# df.load <- as.data.frame(res$var$coord)
# 
# df.load$varNames <- rownames(df.load)
# 
# df.load %>%  
#   pivot_longer(cols = 1:5, values_to = "loading", names_to = "PC") %>% 
#   rename(variabili = varNames ) -> load_plot
# 
# 
# ggplot(load_plot)+
#   aes(x = variabili, 
#       y = loading, 
#       ymax = loading)+
#   geom_point()+
#   geom_linerange(aes(ymin = 0))+
#   facet_wrap(~ PC, nrow = 1)+
#   coord_flip()+
#   ggtitle("variable loadings for principal components")
# 
# 
# 
# 
# res$ind$coord %>% 
# ggplot() +
#   aes(x = gruppo, 
#       y = Dim.)+
#   geom_boxplot()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# plot(res, habillage = 1)
# plotellipses(res)
# 
# fviz_pca_ind(res)
# fviz_pca_var(res)
# 
# fviz_pca_ind(res,  label="none", habillage=df$gruppo,
#              addEllipses=TRUE, ellipse.level=0.95)
# 
# fviz_pca_var(res, habillage = df$gruppo, addEllipses = TRUE )
# 
# 
# fviz_pca_biplot(res, 
#                 addEllipses = FALSE,
#                 habillage=df$gruppo,
#                 col.var = "red",
#                 label = "var") +
#   scale_color_brewer(palette="Dark2")+
#   theme_minimal()
# 
# 
# 
# dt.pca <- PCA(dt_norm_pca, quali.sup = 1,  graph = FALSE)
# 
# plot(dt.pca, choix = "var", habillage = 1)
# fviz_pca_biplot(dt.pca, choix = "var", habillage = 1)
# 
# # model <- lda(gruppo ~ ., data = dt_norm)
# #   
# #   
# # lda.data <- cbind(dt_norm, predict(model)$x)
# # 
# # ggplot(lda.data, aes(LD1)) +
# #   geom_point(aes(color = gruppo))
# # 
# # 
# # 
# # model <- fda(gruppo~., data = dt_norm)
# # predicted.classes <- model %>% predict(dt_norm)
# # mean(predicted.classes == dt_norm$gruppo)
# # 
# # 
# # plot(model)
# 

