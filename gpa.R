notas <- read.csv("GPA.csv", header = T, sep = ";", stringsAsFactors = F)
nota_br_eua <- function(nota, creditos){
  eua <- 0
  nota <- nota*10
  if(nota <= 59.99 & nota >= 0){
    eua <- creditos*0
  }else if(nota <= 62.99 & nota >= 60){
    eua <- creditos*0.7
  }else if(nota <= 66.99 & nota >= 63){
    eua <- creditos*1.0
  }else if(nota <= 69.99 & nota >= 67){
    eua <- creditos*1.3
  }else if(nota <= 72.99 & nota >= 70){
    eua <- creditos*1.7
  }else if(nota <= 76.99 & nota >= 73){
    eua <- creditos*2.0
  }else if(nota <= 79.99 & nota >= 77){
    eua <- creditos*2.3
  }else if(nota <= 82.99 & nota >= 80){
    eua <- creditos*2.7
  }else if(nota <= 86.99 & nota >= 83){
    eua <- creditos*3.0
  }else if(nota <= 89.99 & nota >= 87){
    eua <- creditos*3.3
  }else if(nota <= 92.99 & nota >= 90){
    eua <- creditos*3.7
  }else if(nota <= 100 & nota > 93){
    eua <- creditos*4.0
  }
  return(eua)
}

total_creditos <- sum(notas$credits)

total_pontos <- 0
for(i in 1:nrow(notas)){
  total_pontos <- total_pontos + nota_br_eua(notas$score[i], notas$credits[i])
  
}
resultado <- total_pontos/total_creditos
resultado
