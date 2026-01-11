#' CRIAÇÃO DO DATAFRAME DOS DADOS PARA ANÁLISE
#'
#' @param extensao tipo do arquivo.
#' @param separador separador de células de tabela.
#'
#' @return
#' @export
#'
#' @examples
criabase <- function (nome_dados, extensao, planilha=NULL, separador=NULL, exclusivo=NULL){
  extensao <- extensao
  separador <- separador
  planilha <- planilha

  if(extensao == 1){
    dado <- "xlsx"
    ext <<- ".xlsx"
    arquivo <<- paste(nome_dados,ext,sep="")
  }else if(extensao == 2){
    dado <- "xls"
    ext <<- ".xls"
    arquivo <<- paste(nome_dados,ext,sep="")
  }else if(extensao == 3){
    dado <- "csv"
    ext <<- ".csv"
    arquivo <<- paste(nome_dados,ext,sep="")
  }else if(extensao == 4){
    dado <- "txt"
    ext <<- ".txt"
    arquivo <<- paste(nome_dados,ext,sep="")
  }else if(extensao == 5){
    dado <- "dataframe"
  }else if(is.null(extensao)){
    dado <- "xlsx"
    ext <<- ".xlsx"
    arquivo <<- paste(nome_dados,ext,sep="")
  }
  
  meus_dados <<- data.frame()

  if(dado == "xlsx"){
    #cria dataframe a partir dos dados da planilha
    if(is.null(planilha)){
      meus_dados <<- read_excel(arquivo)
    }else{
      meus_dados <<- read_excel(arquivo, sheet = planilha)
    }
  }else if(dado == "dataframe"){
    meus_dados <<- get(nome_dados)
  }else if(dado == "csv"){
    if(is.null(separador)){
      meus_dados <<- read.csv(file = arquivo, header = TRUE)
    }else{
      meus_dados <<- read.csv(file = arquivo, header = TRUE, sep = separador)
    }
  }else if(dado == "txt"){
    if(is.null(separador)){
      if(idioma == 1){
        erro <<- "Necessário informar separador de arquivos em txt."
        erro
      }else{
        erro <<- "Requires txt file separator."
        erro
      }
    }else{
      meus_dados <<- read.delim2(arquivo, sep=separador)
    }
  }

  if(exclusivo == 2){
    #atribui dados sem grupo de sexo da planilha ao dataframe dados
    names(meus_dados) <<- c("classeCt","frequenciaAbsoluta","Species", "Linf","L50","L95","MK","BinMax","BinMin","BinWidth","SPR","yr")
    dados <<- data.frame(classeCt = c(meus_dados[,1]),
                         frequenciaAbsoluta = c(meus_dados[,2]),
                         Species = c(meus_dados[,3]),
                         Linf = c(meus_dados[,4]),
                         L50 = c(meus_dados[,5]),
                         L95 = c(meus_dados[,6]),
                         MK = c(meus_dados[,7]),
                         BinMax = c(meus_dados[,8]),
                         BinMin = c(meus_dados[,9]),
                         BinWidth = c(meus_dados[,10]),
                         SPR = c(meus_dados[,11]),
                         yr = c(meus_dados[,12]))
    dadosDois <<- data.frame(classeCt = c(meus_dados[,1]),
                         frequenciaAbsoluta = c(meus_dados[,2]))

    write.table(dadosDois, file='objeto.csv', sep=',', dec='.', row.names=FALSE)
    
    return(dados)
  }else if(exclusivo == 1){
    #atribui dados sem grupo de sexo da planilha ao dataframe dados
    names(meus_dados) <<- c("classeCt","frequenciaAbsoluta")
    dados <<- data.frame(classeCt = c(meus_dados[,1]),
                         frequenciaAbsoluta = c(meus_dados[,2]))

    write.table(dados, file='objeto.csv', sep=',', dec='.', row.names=FALSE)
    
    return(dados)
  }
}

