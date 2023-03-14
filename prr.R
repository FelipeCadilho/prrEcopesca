#' PRR Ecopesca
#'
#' @return
#' @export
#'
#' @examples
prrEcopesca <- function(){
#primeira execução com passagem de parâmetros
cat("\nEsta é a primeira execução deste pacote? S/N")
respostinha1 = toupper(readLines(n=1))
respostinha1 = excecaoA("B",respostinha1,"N")

if(respostinha1 == "S"||respostinha1 == "Y"){
  
  #chama pacote que lê planilha em excel
  install.packages("readxl")
  library("readxl")
  
  #chama pacote de controle de dados
  install.packages("dplyr")
  library("dplyr")
  
  #chama pacote de calculo de logaritmo natural
  install.packages("LBSPR")
  library("LBSPR")

}else{
  #chama pacote que lê planilha em excel
  library("readxl")
  
  #chama pacote de controle de dados
  library("dplyr")
  
  #chama pacote de calculo de logaritmo natural
  library("LBSPR")

}
#cria objeto para atribuir parâmetros de história de vida
MyPars <<- new("LB_pars")

#arquivo
nome_dados <<- NULL
while(is.null(nome_dados)){
  cat("\nQual nome do arquivo? (Sem extensão, e.g., .csv)\n")
  nome_dados <<- readLines(n=1)
  #exceção
  nome_dados <<- excecaoA("B",nome_dados,NULL)
}

#extensão do arquivo
cat("\nQual é a extensão do arquivo?\n1 - xlsx\n2 - xls\n3 - csv\n4 - txt\n5 - dataframe\n")
extensao <<- scan(n=1)
#exceção
extensao <<- excecaoA("A",extensao,1,0,6)

planilhas <<- NULL
if(extensao==1 || extensao==2){
  #planilha
  cat("\nQual nome da planilha? \n(Se for a primeira apenas pressione enter)\n")
  planilhas <<- readLines(n=1)
  #exceção
  planilhas <<- excecaoA("B",planilhas,NULL)
}

separador <<- NULL
#separador
if(extensao==3 || extensao==4){
  cat("\nQual é o separador das colunas?\n")
  separador <<- readLines(n=1)
}
#exceção
separador <<- excecaoA("B",separador,",")


#seleção de entrada de dados
cat("\nDeseja digitar os parâmetros de história de vida\nou carregá-los com uma tabela? \n(1) - Digitar\n(2) - Carregar com tabela\n")
respostinha2 = scan(n=1)
respostinha2 = excecaoA("A",respostinha2,2,0,3)

if(respostinha2 == 2){

  todos_dados <<- criabase(nome_dados,extensao,planilhas,separador,exclusivo=2)
  #atribui valores ao objeto de acordo com seus atributos
  #cat("\nQual nome da espécie?\n")
  MyPars@Species <<- todos_dados[1,3]
  
  #cat("\nQual é o valor do comprimento assintótico?\n")
  MyPars@Linf <<- todos_dados[1,4]
  
  #cat("\nQual é o valor de L50?\n")
  MyPars@L50 <<- todos_dados[1,5]
  
  #cat("\nQual é o valor de L95?\n")
  MyPars@L95 <<- todos_dados[1,6]
  
  #cat("\nQual é o valor de M/K?\n")
  MyPars@MK <<- todos_dados[1,7]
  
  #cat("\nQual é o valor da maior classe de comprimento?\n")
  MyPars@BinMax <<- todos_dados[1,8]
  
  #cat("\nQual é o valor da menor classe de comprimento?\n")
  MyPars@BinMin <<- todos_dados[1,9]
  
  #cat("\nQual é o valor da largura das classes de comprimento?\n")
  MyPars@BinWidth <<- todos_dados[1,10]
  
  #cat("\nQual é o valor de SPR alvo?\n")
  MyPars@SPR <<- todos_dados[1,11]
  
  #cat("\nQual é o valor de Ano?\n")
  yr <<- todos_dados[1,12] # first year of data
  
  cat("\nDigite o nome ou código hexadecimal da cor do primeiro dado da legenda:\n")
  corUm <<- tolower(readLines(n=1))
  
  cat("\nDigite o nome ou código hexadecimal da cor do segundo dado da legenda:\n")
  corDois <<- tolower(readLines(n=1))
  
  LenDat <<- new("LB_lengths", LB_pars=MyPars, file=paste0("objeto.csv"), dataType="freq", header=TRUE)

}else if(respostinha2 == 1){
  todos_dados <<- criabase(nome_dados,extensao,planilhas,separador,exclusivo=1)
  #atribui valores ao objeto de acordo com seus atributos
  cat("\nQual nome da espécie?\n")
  MyPars@Species <<- readLines(n=1)

  cat("\nQual é o valor do comprimento assintótico?\n")
  MyPars@Linf <<- scan(n=1)

  cat("\nQual é o valor de L50?\n")
  MyPars@L50 <<- scan(n=1)

  cat("\nQual é o valor de L95?\n")
  MyPars@L95 <<- scan(n=1)

  cat("\nQual é o valor de M/K?\n")
  MyPars@MK <<- scan(n=1)

  cat("\nQual é o valor da maior classe de comprimento?\n")
  MyPars@BinMax <<- scan(n=1)

  cat("\nQual é o valor da menor classe de comprimento?\n")
  MyPars@BinMin <<- scan(n=1)

  cat("\nQual é o valor da largura das classes de comprimento?\n")
  MyPars@BinWidth <<- scan(n=1)

  cat("\nQual é o valor de SPR alvo?\n")
  MyPars@SPR <<- scan(n=1) # Target SPR
  
  cat("\nQual é o valor de Ano?\n")
  yr <<- scan(n=1) # first year of data
  
  cat("\nDigite o nome ou código hexadecimal da cor do primeiro dado da legenda:\n")
  corUm <<- tolower(readLines(n=1))
  
  cat("\nDigite o nome ou código hexadecimal da cor do segundo dado da legenda:\n")
  corDois <<- tolower(readLines(n=1))
  
  LenDat <<- new("LB_lengths", LB_pars=MyPars, file=paste0("objeto.csv"), dataType="freq", header=TRUE)
}

Mod <- LBSPRfit(MyPars, LenDat, msg=FALSE)

MyPars@SL50 <<- Mod@SL50[yr]
MyPars@SL95 <<- Mod@SL95[yr]
grafico <<- (plotTarg(MyPars, LenDat, yr=yr, Cols = c(corUm,corDois)))

return(list(Mod@Ests,grafico))
}