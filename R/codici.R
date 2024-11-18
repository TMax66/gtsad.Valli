pkg()
library(MASS)
library(mda)
library(FactoMineR)
library(factoextra)


dt <- read_excel(here("dati", "DATASET per articolo_17.10.2024(1).xlsx"))


dt %>%  clean_names() %>% 
  
  mutate(#across(13:36, ~scale(.)), 
         gruppo = as.factor(gruppo), 
         statoprod = as.factor(statoprod))-> df

  



df <- df[,  c(1,10,  13:36)]

 

df<- as.data.frame(df)




prcom<-prcomp(~.-gruppo,df,scale.=T,center=T, retx = TRUE)




# score individuali
df.projected <- as.data.frame(predict(prcom, df[, -1]), stringsAsFactors = FALSE)

df.projected$gruppo <- df$gruppo

# Screeplot

screeplot(prcom, npcs = 10, type = "bar")


# loading delle variabili sulle PCA
#

prcomLoad <- prcom$rotation

df.prcom <- as.data.frame(prcomLoad)

df.prcom$varNames <- rownames(df.prcom)

df.prcom %>% 
  pivot_longer(cols = 1:25, values_to = "loading", names_to = "PC") %>% 
  rename(variabili = varNames ) -> load_plot

ggplot(load_plot %>% 
         filter(PC %in% c("PC1", "PC2", "PC3", "PC4", "PC5")))+
  aes(x = variabili, 
      y = loading, 
      ymax = loading)+
  geom_point()+
  geom_linerange(aes(ymin = 0))+
  facet_wrap(~ PC, nrow = 1)+
  coord_flip()+
  ggtitle("variable loadings for principal components")
  


# grafici
ncomp = 3

library(ggbiplot)

g <- ggbiplot(prcom, obs.scale = 1, var.scale = 1, 
              ellipse = TRUE, groups = df$gruppo, circle = TRUE)

# g <- g + scale_color_discrete(name = "")

g <- g + theme(legend.direction = "horizontal", legend.position = "top")

#regvar <- paste(paste("PC", 1:ncomp, SEP =""), collapse = "+")

#fmla = paste("gruppo ~", regvar)

mod.com <- glm(gruppo ~ PC1+PC2+PC3+PC4, family = binomial, data =df.projected)



#




df %>% 
  group_by(gruppo) %>% 
  summarise(across(where(is.double), ~ mean(.x, na.rm = TRUE))) 


res <- PCA(df, quali.sup = 1)

# per ruotare i dati delle variabili 
# t(apply(res$var$coord, 1, function(x) {x/sqrt(res$eig[,1])}))

dt %>%  clean_names() %>% 
  mutate(gruppo = as.factor(gruppo), 
    statoprod = as.factor(statoprod)) %>% 
  
  bind_cols(
    res$ind$coord
  ) %>%  data.frame()-> dt_score
  

t.test(Dim.1 ~ gruppo, data = dt_score)





ggplot() +
  aes(x = gruppo, 
      y = Dim.4)+
  geom_boxplot()











plot(res, habillage = 1)
plotellipses(res)

fviz_pca_ind(dt.pca)

fviz_pca_ind(dt.pca,  label="none", habillage=dt_norm_pca$gruppo,
             addEllipses=TRUE, ellipse.level=0.95)

fviz_pca_var(res, habillage = dt_norm_pca$gruppo, addEllipses = TRUE )


fviz_pca_biplot(res, 
                habillage = dt_norm_pca$gruppo, addEllipses = FALSE,
                col.var = "red", alpha.var ="cos2",
                label = "var") +
  scale_color_brewer(palette="Dark2")+
  theme_minimal()



dt.pca <- PCA(dt_norm_pca, quali.sup = 1,  graph = FALSE)

plot(dt.pca, choix = "var", habillage = 1)
fviz_pca_biplot(dt.pca, choix = "var", habillage = 1)

# model <- lda(gruppo ~ ., data = dt_norm)
#   
#   
# lda.data <- cbind(dt_norm, predict(model)$x)
# 
# ggplot(lda.data, aes(LD1)) +
#   geom_point(aes(color = gruppo))
# 
# 
# 
# model <- fda(gruppo~., data = dt_norm)
# predicted.classes <- model %>% predict(dt_norm)
# mean(predicted.classes == dt_norm$gruppo)
# 
# 
# plot(model)


