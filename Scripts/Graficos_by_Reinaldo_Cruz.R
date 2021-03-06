install.packages('ggplot2')
library('ggplot2')

#desliga a nota��o cient�fica
options(scipen = 999)

#visualiza os dados do dataframe
names(PedMunPeriodo)plo

#Gr�fico Pedidos Por Classifica��o de Municipio 2012-2018
#cria um objeto gplot
g2<-ggplot(data=PedMunPeriodo, aes(y = TotalPedidos, x = ClassMunPop, fill = ClassMunPop))
g2+
  geom_bar(stat = "identity")+
  ggtitle("Pedidos Por Classifica��o de Municipio 2012-2018")+
  ylim(0,400000)+
  ylab("Total de Pedidos")+
  xlab("Classifica��o do Munic�pio")+
  scale_color_brewer(type = "seq", palette = "Set2")



g3<- aggregate(Municipio ~ ClassMunPop + Ano, data=PedAnoMunClaspop, FUN=mean)
ggplot(g3, aes(x = ClassMunPop, y = Municipio, fill = factor(Ano))) +
  geom_col(position = "dodge") +
  labs(title = "Pedidos por Ano, por Classifica��o de Munic�pio",
       x = "Classifica��o Munic�pio",
       y = "Quantidade de Pedidos",
       fill = "Ano")

#M�dia de Pedidos Por habitantes Munc Pequenos e Muito Pequenos
names(PedporHabitante)
Pedante<-PedporHabitante[c(3,4,7)]
names(Pedante)<-c("UF","Ano","PedidoPorHabitante")
View(Pedante)
g5<-aggregate(PedidoPorHabitante ~ UF + Ano, data=Pedante, FUN=mean)
ggplot(g5, aes(x = UF, y = PedidoPorHabitante, fill = factor(Ano))) +
  geom_col(position = "dodge") +
  labs(title = "M�dia de Pedidos Por Ano por Habitante - Munic�pios Pequenos e Muito Pequenos",
       x = "Unidade da Federa��o",
       y = "M�dia dos Pedidos Por Habitante",
       fill = "Ano")

           