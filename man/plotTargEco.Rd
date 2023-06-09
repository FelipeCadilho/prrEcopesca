% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotTarg.R
\name{plotTargEco}
\alias{plotTargEco}
\title{Trace a estrutura de comprimento amostrado contra a composição de tamanho simulado de destino}
\usage{
plotTargEco(
  LB_pars = NULL,
  LB_lengths = NULL,
  yr = 1,
  Cols = NULL,
  title = NULL,
  targtext = TRUE,
  size.axtex = 12,
  size.title = 14,
  scales = c("fixed", "free_x", "free_y", "free"),
  idioma,
  fachada
)
}
\arguments{
\item{LB_pars}{um objeto da classe \code{'LB_pars'} que contém o histórico de vida e informações de pesca}

\item{LB_lengths}{um objeto da classe \code{'LB_lengths'} que contém os dados de tamanho observados}

\item{yr}{index para dados de comprimento amostrados (o padrão é 1)}

\item{Cols}{vetor opcional de caracteres de cores para o gráfico}

\item{title}{character - título opcional para enredo}

\item{targtext}{lógico - o texto de destino do SPR deve ser exibido como um subtítulo?}

\item{size.axtex}{tamanho do texto do eixo}

\item{size.title}{tamanho do título do eixo}

\item{scales}{o argumento para a função ggplot2. As escalas são compartilhadas em todas as facetas
(o padrão, "fixo"), ou eles variam entre linhas ("free_x"), colunas ("free_y"),
ou ambas as linhas e colunas ("grátis")}

\item{idioma}{1 = português e 2 = inglês.}

\item{fachada}{se não estiver nula apresentará o rótulo de ano no gráfico}

\item{familia}{1 = Arial, 2 = Times New Roman, 3 = Courier New}
}
\value{
um objeto ggplot
}
\description{
Uma função que plota a estrutura de tamanho observada em relação à composição de tamanho esperada no PRR de destino
}
\author{
A. Hordyk
}
