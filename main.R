# Carregar a biblioteca necessária
library(OpenImageR)
library(randomForest)
library(ggplot2)

# Ler as imagens fornecidas
tudo <- readImage("full-image.jpg")
verde <- readImage("verde.jpg")
cinza <- readImage("cinza.jpg")
agua <- readImage("agua.jpg")

# Exibir as imagens para conferência
imageShow(tudo)
imageShow(verde)
imageShow(cinza)
imageShow(agua)

# Converter cada canal de cor em um vetor
mverde <- cbind(c(verde[,,1]), c(verde[,,2]), c(verde[,,3]))
mcinza <- cbind(c(cinza[,,1]), c(cinza[,,2]), c(cinza[,,3]))
magua <- cbind(c(agua[,,1]), c(agua[,,2]), c(agua[,,3]))

# Amostragem de pixels das imagens
set.seed(123)  # Definir semente para reprodutibilidade
mverde <- mverde[sample(1:nrow(mverde), 10000),]
mcinza <- mcinza[sample(1:nrow(mcinza), 10000),]
magua <- magua[sample(1:nrow(magua), 5000),]

# Combinar os dados e criar as classes (0=fundo, 1=verde, 2=queimado)
dados <- rbind(cbind(mcinza, 0), cbind(mverde, 1), cbind(magua, 2))
colnames(dados) <- c("R", "G", "B", "Y")

# Treinar o modelo Random Forest
modelo <- randomForest(as.factor(Y) ~ R + G + B, data = dados)
print(modelo)
importance(modelo)

# Predição na imagem completa
mtudo <- cbind(c(tudo[,,1]), c(tudo[,,2]), c(tudo[,,3]))
colnames(mtudo) <- c("R", "G", "B")

pred <- predict(modelo, newdata = mtudo)
pred <- as.numeric(pred) - 1  # Ajustar os valores de predição

# Criar matriz de predição para visualização
ncol_original <- dim(tudo)[2]
mpred <- matrix(pred, ncol = ncol_original)

# Exibir a imagem predita
imageShow(mpred)

# Modificar a imagem original com base nas predições
tudo2 <- tudo
tudo2[,,1][pred == 2] <- 1  # Marcar pixels classificados como "agua" no canal vermelho
imageShow(tudo2)

tudo2 <- tudo
tudo2[,,2][pred == 1] <- 1  # Marcar pixels classificados como "verde" no canal azul
imageShow(tudo2)

tudo2 <- tudo
tudo2[,,1][pred == 0] <- 1  # Marcar pixels classificados como "cinza" no canal azul
imageShow(tudo2)

# Calcular a porcentagem de cada classe
porcentagens <- prop.table(table(pred)) * 100
classes <- c("Cinza", "Verde", "Água")
Classe = classes
Porcentagem = porcentagens
dados_porcentagem <- data.frame(Classe, Porcentagem)

# Plotar o gráfico de porcentagem de cada classe
ggplot(dados_porcentagem, aes(x = Classe, y = Porcentagem, fill = Classe)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f%%", Porcentagem)), vjust = -0.3) +
  theme_minimal() +
  labs(title = "Porcentagem de Pixels por Classe",
       x = "Classe",
       y = "Porcentagem") +
  scale_fill_manual(values = c("Cinza" = "gray", "Verde" = "green", "Água" = "blue"))

dados_porcentagem




# Calcular a porcentagem de pixels de cada classe na imagem completa
total_pixels <- length(pred)
contagem_classes <- table(pred)
porcentagens2 <- (contagem_classes / total_pixels) * 100
classes2 <- c("Cinza", "Verde", "Água")
Classes2 = classes2
Porcentagem2 = porcentagens2
dados_porcentagem2 <- data.frame(Classe2, Porcentagem2)

# Imprimir os valores de porcentagem no console
print(dados_porcentagem2)

# Plotar o gráfico de porcentagem de cada classe com valores no topo das barras
ggplot(dados_porcentagem, aes(x = Classes2, y = Porcentagem2, fill = Classes2)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f%%", Porcentagem2)), vjust = -0.3) +
  theme_minimal() +
  labs(title = "Porcentagem de Pixels por Classe na Imagem Completa",
       x = "Classe",
       y = "Porcentagem") +
  scale_fill_manual(values = c("Cinza" = "gray", "Verde" = "green", "Água" = "blue"))
