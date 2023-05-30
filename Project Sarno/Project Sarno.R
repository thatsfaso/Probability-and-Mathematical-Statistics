#Statistica Descrittiva

if (!require(readxl)) {
  install.packages("readxl")
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
}

library(readxl)
library(ggplot2)

PopolazioneSarno <- read_excel("PopolazioneSarno.xlsx", 
                               col_types = c("numeric", "numeric"))
View(PopolazioneSarno)

# Calcola le statistiche
media <- mean(PopolazioneSarno$Popolazione)
mediana <- median(PopolazioneSarno$Popolazione)
moda <- as.numeric(names(which.max(table(PopolazioneSarno$Popolazione))))
varianza <- var(PopolazioneSarno$Popolazione)
range <- max(PopolazioneSarno$Popolazione) - min(PopolazioneSarno$Popolazione)
minimo <- min(PopolazioneSarno$Popolazione)
massimo <- max(PopolazioneSarno$Popolazione)
somma <- sum(PopolazioneSarno$Popolazione)
conteggio <- length(PopolazioneSarno$Popolazione)

# Stampa le statistiche
cat("Media:", media, "\n")
cat("Mediana:", mediana, "\n")
cat("Moda:", moda, "\n")
cat("Varianza:", varianza, "\n")
cat("Range:", range, "\n")
cat("Minimo:", minimo, "\n")
cat("Massimo:", massimo, "\n")
cat("Somma:", somma, "\n")
cat("Conteggio:", conteggio, "\n")

tabella_statistiche <- data.frame(Statistica = c("Media", "Mediana", "Moda", "Varianza", "Range", "Minimo", "Massimo", "Somma", "Conteggio"),
                                  Valore = c(media, mediana, moda, varianza, range, minimo, massimo, somma, conteggio))
View(tabella_statistiche)

# Crea un diagramma a colonne
barplot(PopolazioneSarno$Popolazione, main = "Evoluzione demografica di Sarno", names.arg=PopolazioneSarno$Anno, las = 2, cex.names = 0.9, xlab = "Anno", ylab = "Popolazione")

#Crea un Grafico a linea di base con regressione
ggplot(PopolazioneSarno, aes(x = Anno, y = Popolazione)) +
  geom_line(color = "red", linewidth = 2) +
  geom_smooth(method = "lm", se = FALSE, color="black") +
  labs(title = "Evoluzione demografica di Sarno - Grafico a linea di base con regressione", x = "Anno", y = "Popolazione")




#import Minoranze
Etnie <- read_excel("Minoranze.xlsx", 
                        col_types = c("text", "numeric"))

# Calcola la percentuale per ogni categoria
EtniePerc <- transform(Etnie, Percentuale = (Abitanti / sum(Abitanti)) * 100)

# Crea un diagramma a torta con testo
ggplot(EtniePerc, aes(x = "", y = Percentuale, fill = Nazione)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_label(aes(label = paste0(Nazione, ": ", round(Percentuale, 1), "%")),
             position = position_stack(vjust = 0.5),
             color = "black",
             size = 4) +
  labs(title = "Distribuzione delle Etnie a Sarno - dati del 2017", fill = "Nazione")
  
