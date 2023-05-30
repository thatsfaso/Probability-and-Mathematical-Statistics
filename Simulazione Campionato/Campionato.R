#Simulazione risultati casuali

if (!require(plotly)) {
  install.packages("plotly")
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
}

library(ggplot2)
library(plotly)

n_partite <- 36
saldo <- 100
costo_perdita <- 10
guadagno_vittoria_pareggio <- 10

# Crea vettori per registrare i risultati delle partite
risultati <- sample(c("Vittoria", "Pareggio", "Sconfitta"), n_partite, replace = TRUE)

# Inizializza le variabili
portafoglio <- numeric(n_partite)
guadagno_complessivo <- 0
vittorie <- 0
sconfitte <- 0

# Simula le partite
for (i in 1:n_partite) {
  if (risultati[i] == "Vittoria" || risultati[i] == "Pareggio") {
    saldo <- saldo + guadagno_vittoria_pareggio
    portafoglio[i] <- saldo
    guadagno_complessivo <- guadagno_complessivo + guadagno_vittoria_pareggio
    vittorie <- vittorie + 1
  } else {
    saldo <- saldo - costo_perdita
    portafoglio[i] <- saldo
    sconfitte <- sconfitte + 1
  }
}

# Calcola il saldo finale del portafoglio
saldo_finale <- sum(100 + (vittorie*10) - (sconfitte*10))

# Stampa i risultati
cat("Risultati delle partite:\n")
cat("Vittorie:", vittorie, "\n")
cat("Sconfitte:", sconfitte, "\n")
cat("Guadagno complessivo:", sum((vittorie*10) - (sconfitte*10)), "\n")
cat("Saldo finale del portafoglio:", saldo_finale, "\n")


# create data
xValue <- 1:n_partite
yValue <- portafoglio
data <- data.frame(xValue,yValue)

for(j in 1:n_partite){
  cat(j, ":", portafoglio[j], "\n")
}

# Crea un grafico dell'andamento del portafoglio
p <- ggplot(data, aes(x=xValue, y=yValue)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=1.5) +
  geom_point(size=2, color="#69b3a2") +
  ylim(40, 250) +
  ggtitle("Andamento del portafoglio")

p <-ggplotly(p)
p
