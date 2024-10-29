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

  

df <- df[,  c(1, 13:36)]



df %>% 
  group_by(gruppo) %>% 
  summarise(across(where(is.double), ~ mean(.x, na.rm = TRUE))) %>%  View()


res <- PCA(df, quali.sup = 1)

plot(df, habillage = 1)
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


