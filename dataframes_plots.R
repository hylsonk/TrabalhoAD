f1 <- function(n) {
  eixo_x <- numeric()
  eixo_y <- numeric()
  for (i in 0:n) {
    print(i)
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

f2 <- function(){
  var1 = 1
  var2 = 2
  return(data.frame(var1, var2))
}

n=5
novo_df<-data.frame("Variavel1", "Variavel2")
for (i in 1:n) {
  df=f2()
  novo_df[i] <- df
}
novo_df
