#' Controle de exceção: Caracteres e Números
#'
#' @param objeto Dados da variável a ser verificada.
#' @param padrao Padrão caso a variável entre na exceção.
#' @param atual Valor a ser mantido se não entrar na exceção.
#' @param min Valor mínimo para variável entrar na exceção.
#' @param max Valor máximo para variável entrar na exceção.
#'
#' @return
#' @export
#'
#' @examples
#'

#controle de exceção de números
excecaoA <- function(funcao,objeto, padrao, min, max, atual){
  
  if(funcao=="A"){
    if(length(objeto)==0){
      return(padrao)
    }else if(is.null(objeto)){
      return(padrao)
    }else if(objeto<=min){
      return(padrao)
    }else if(objeto>=max){
      return(padrao)
    }else{
      return(objeto)
    }
  }
  
  
  #controle de exceção caracter
  if(funcao=="B"){
    if(is.null(objeto)){
      return(padrao)
    }else if(objeto==""){
      return(padrao)
    }else{
      return(objeto)
    }
  }
  
  #controle de exceção de números com duas respostas possíveis
  if(funcao=="C"){
    if(length(objeto)==0){
      return(padrao)
    }else if(is.null(objeto)){
      return(padrao)
    }else if(objeto<=min){
      return(padrao)
    }else if(objeto>=max){
      return(padrao)
    }else{
      return(atual)
    }
  }
}