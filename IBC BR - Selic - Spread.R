# libs
if(!require(corrplot)){installed.packages(corrplot)}
library(corrplot)
if(!require(Hmisc)){installed.packages(Hmisc)}
library(Hmisc)

Matriz <- read.csv('C:/Users/Matheus/Desktop/UFSC/Matérias/Economia Monetária/Atividade 07/Atividade 07.csv',
                   sep = ';')
Matriz <- data.frame(Matriz)
names(Matriz)
colnames(Matriz) = c('Data','Taxa Selic','Spread','IBC-Br')
Matriz$`Taxa Selic` <- sub(',','.',Matriz$`Taxa Selic`)
Matriz$Spread <- sub(',','.',Matriz$Spread)
Matriz$`IBC-Br` <- sub(',','.',Matriz$`IBC-Br`)

Matriz$`Taxa Selic` <- as.numeric(Matriz$`Taxa Selic`)
Matriz$Spread <-  as.numeric(Matriz$Spread)
Matriz$`IBC-Br` <-  as.numeric(Matriz$`IBC-Br`)

d = which(is.na(Matriz$`IBC-Br`) == TRUE) # linhas dos NAs #

Matriz = Matriz[-d,] #excluir os NAs#



MatrizCorrel = Matriz[,2:4]
cor(MatrizCorrel)
MatrizCorrel_indices = cor(MatrizCorrel)
corrplot(MatrizCorrel_indices, method = 'number')
cor.test(MatrizCorrel$`Taxa Selic`,MatrizCorrel$`IBC-Br`)
cor.test(MatrizCorrel$`Taxa Selic`,MatrizCorrel$Spread)



m = rcorr(as.matrix(MatrizCorrel))
m$P #p-valor#

#matriz correl #
ggsave("MatrizCorrelação.png", 
       corrplot(m$r, p.mat = m$P, sig.level = 0.05,method = 'number', type = 'full',
         order = 'AOE', tl.srt = 0,
         number.cex=1.5, 
         tl.col = 'black', tl.cex = 1.10), 
       scale = 1, width = 5, height = 5)



