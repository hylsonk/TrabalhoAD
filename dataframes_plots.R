f1 <- function(n) {
  eixo_x <- numeric(n)
  eixo_y <- numeric(n)
  for (i in 0:n) {
    eixo_x[i] <- i
    eixo_y[i] <- (n-(i-1))
  }
  data.frame(eixo_x, eixo_y)
}
df=f1(5)
df$eixo_x
df$eixo_y
#plotar gráficos (x, y, tipo de grafico, label do eixo x e y, titulo do plot, cor do plot)
plot(df$eixo_x, df$eixo_y, type='h', ylab = 'n', xlab = 's',lwd = 5, main="Teste de Plot", col="red")
plot(df$eixo_x, df$eixo_y, type='l', ylab = 'n', xlab = 's', lwd = 2,main="Teste de Plot")

