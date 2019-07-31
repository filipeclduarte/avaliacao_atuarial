# calcular a idade de aposentadoria

idad_apos_2019 <- function(x, sexo, t, profissao){ # sexo 1 == Feminino
  if(sexo == 1){
    if(profissao == "PROFESSORA"){
      return(52)  
    }else{  
      return(57)
    }
  }else{
    if(profissao == "PROFESSOR"){
      return(57)
    }else{
      return(62)
    }
  }
}

# testes:
#idad_apos_2019(sexo = 1, profissao = "PROFESSORA")
#idad_apos_2019(sexo = 1, profissao = "MÉDICA")
#idad_apos_2019(sexo = 2, profissao = "PROFESSOR")
#idad_apos_2019(sexo = 2, profissao = "TÉCNICO")
