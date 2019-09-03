plot.categorical <- function(df,variavel,num.class,join.interval,newseg){
  
  seg.var <- number.class(df,variavel,num.class)
  
  if(newseg == T) seg.var <- new.segment(seg.var,join.interval)
  
  ifelse(length(unique(seg.var[,3][!is.na(seg.var[,3])])) > 15, anglo <- 45, anglo <- 0)
  ifelse(length(unique(seg.var[,3][!is.na(seg.var[,3])])) >  9, size.text <- 3, size.text <- 4)
  ifelse(length(unique(seg.var[,3][!is.na(seg.var[,3])])) > 15, h <- 0.9, h <- 0.5)
  
  dftab <- data.frame(aprovado_vaga = df[,'aprovado_vaga'],Class = seg.var$class)
  dftab <- data.frame(lapply(dftab,factor))
  
  tabela  <- xtabs(~ aprovado_vaga + Class,data = dftab) %>% prop.table(1)
  tabela <- melt(tabela, id = c("aprovado_vaga","Class"))
  
  p1 <- ggplot(tabela, aes(Class,value, color = factor(aprovado_vaga) )) + 
    geom_bar(aes(fill = factor(aprovado_vaga)), 
             position = "dodge", 
             stat="identity", 
             alpha = 0.8)+
    xlab(label =  '') + 
    ylab(label = '%') + 
    labs(fill = 'aprovado_vaga', color = 'aprovado_vaga') +
    geom_text(aes(label= paste(round(100*tabela$value,2),'%',sep = "")),
              size = size.text,
              position=position_dodge(0.9), 
              vjust= -0.2) +   
    ggtitle(paste('Discretizando a variável: ',variavel)) +    
    theme_bw() +
    scale_x_discrete(limits= as.character(unique(tabela$Class))) +    
    scale_y_continuous(labels = percent) +  
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = anglo,vjust = 0.9,hjust = h,size = 12))
  
  gridExtra::grid.arrange(p1)
  
  colnames(seg.var)[4] <- variavel
  
  return(seg.var)
}

### Número de Classes
number.class <- function(df,variavel,num.class){
  
  n.class <- Hmisc::cut2(df[,variavel],c(1) ,g = num.class) %>% data.frame()
  colnames(n.class) <- 'Class'
  
  n.class <- data.frame(V1 = df[,'aprovado_vaga'],
                        V2 = df[,variavel],
                        n  = as.integer(n.class$Class),
                        class = n.class$Class) %>% as.data.frame()
  
  colnames(n.class)[3] <- paste('nclass',num.class,sep="") 
  
  return(n.class)
}

### Novo Segmento
new.segment <- function(seg.var,join.interval){
  
  seg.var$class <- as.character(seg.var$class)
  teste <- t(xtabs( ~ V1 + class, data = seg.var))
  
  intervalo <- vector()
  
  for(j in 1:length(join.interval)){
    
    li <- strsplit(rownames(teste)[join.interval[[j]][1]],split = ",")[[1]][1]
    ls <- strsplit(rownames(teste)[join.interval[[j]][2]],split = ",")[[1]][2]
    
    intervalo <- c(intervalo,paste(li,',',ls))
    
    seg.var[ seg.var[,3] %in% seq(join.interval[[j]][1],join.interval[[j]][2],1) ,'class'] <- intervalo[j]
    seg.var[ seg.var[,3] %in% seq(join.interval[[j]][1],join.interval[[j]][2],1) ,3] <- j
    
  }
  
  colnames(seg.var)[3] <- paste('nclass',length(join.interval), sep = "")
  
  return(seg.var)
}

## Após detectar uma palavra e uma string 
## a função retorna o contexto da palavra.
relationship <- function(x,y){ 
  
  cat(x,'\n--------------------\n')
  
  return(y[str_detect(y,x)] %>% table) 
  
}

## Importância das Variáveis

importance.vars <- function(df,var){

Y <- df[!is.na(df[,var]),var]
X <- df[!is.na(df[,var]), colnames(df) != var ]


suppressMessages(library(infotheo))
MI = 100*(apply(X,2,function(u) mutinformation(Y,u)/entropy(Y)) %>%  
            as.table(MI) %>% 
            sort(decreasing = T)) 

bar <- MI %>% as.data.frame

colnames(bar) <- c('variavel',"MI")

ggplot(bar, aes(variavel,y = MI)) +
  geom_linerange(aes(x = variavel, 
                     ymin = 0, 
                     ymax = MI),
                 color = "black", 
                 size = 0.4, 
                 lty = 3) +
  geom_point(aes(color = variavel), size = 3) +
  geom_text(aes(label = paste(round(MI,2),"%")), 
            vjust = -0.8,
            position = position_dodge(width = 1)) +
  labs(y = " ",
       x =" ",
       title = '\n Influência Percentual das Variáveis Explicativas \n Sobre a Variável Resposta \n',
       color = "Variáveis") +
  scale_y_continuous(limits = range(0,1.1*max(bar$MI))) +
  theme_bw() +
  scale_fill_discrete(guide = guide_legend()) + 
  theme(plot.title = element_text(hjust = 0.5),legend.position="nome") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 11))

}

# Teste curva Roc

test_roc <- function(model, data){
  
  roc(data[,1],
      predict(model, data, type = "prob")[, "aprovado"])
  
}


# Programação paralela

start.cluster<-function(){
  

 (cluster <- makeCluster(detectCores() - 1)) # convention to leave 1 core for OS
  registerDoParallel(cluster)
  
  return(cluster)
}


stop.Cluster <- function(cluster){
  
  stopCluster(cluster)
  registerDoSEQ()
}  
