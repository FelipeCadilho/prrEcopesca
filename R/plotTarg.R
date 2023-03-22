#' Trace a estrutura de comprimento amostrado contra a composição de tamanho simulado de destino
#'
#' Uma função que plota a estrutura de tamanho observada em relação à composição de tamanho esperada no PRR de destino
#'
#' @param LB_pars um objeto da classe \code{'LB_pars'} que contém o histórico de vida e informações de pesca
#' @param LB_lengths um objeto da classe \code{'LB_lengths'} que contém os dados de tamanho observados
#' @param yr index para dados de comprimento amostrados (o padrão é 1)
#' @param Cols vetor opcional de caracteres de cores para o gráfico
#' @param title character - título opcional para enredo
#' @param targtext lógico - o texto de destino do SPR deve ser exibido como um subtítulo?
#' @param size.axtex tamanho do texto do eixo
#' @param size.title tamanho do título do eixo
#' @param scales o argumento para a função ggplot2. As escalas são compartilhadas em todas as facetas
#' (o padrão, "fixo"), ou eles variam entre linhas ("free_x"), colunas ("free_y"),
#' ou ambas as linhas e colunas ("grátis")
#' @param idioma 1 = português e 2 = inglês.
#' @param familia 1 = Arial, 2 = Times New Roman, 3 = Courier New
#' @param fachada se não estiver nula apresentará o rótulo de ano no gráfico
#' @return um objeto ggplot
#' @author A. Hordyk
#' @importFrom ggplot2 ggplot aes geom_line geom_bar scale_color_manual guides guide_legend xlab ylab theme theme_bw element_text scale_fill_manual scale_fill_discrete ggtitle scale_alpha_manual annotate
#' @importFrom stats optimize quantile
#' @export

plotTargEco <- function(LB_pars=NULL, LB_lengths=NULL, yr=1, Cols=NULL, title=NULL,
                     targtext=TRUE,
                     size.axtex=12, size.title=14,
                     scales=c("fixed", "free_x", "free_y", "free"),
                     idioma,
                     fachada) {
  
  if(idioma==1){
    #português
    t1 = "LB_pars deve ser da classe 'LB_pars' Use: new('LB_lengths')"
    t2 = "LB_lengths deve ser da classe 'LB_lengths'. Use: new('LB_lengths')"
    t3 = "Deve fornecer o alvo PRR (LB_pars@SPR)"
    t4 = "Deve fornecer SL50 (LB_pars@SL50)"
    t5 = "Deve fornecer SL95 (LB_pars@SL95)"
    t6 = "Estrutura de tamanho"
    t7 = "Modelado"
    t8 = "Amostra"
    t9 = "PRR alvo: "
    t10 = "Comprimento"
    t11 = "Frequência"
  }else if(idioma==2){
    #inglês
    t1 = "LB_pars must be from the class 'LB_pars' Use: new('LB_lengths')"
    t2 = "LB_lengths must be from the class 'LB_lengths'. Use: new('LB_lengths')"
    t3 = "Must provide target SPR (LB_pars@SPR)"
    t4 = "Must provide  SL50 (LB_pars@SL50)"
    t5 = "Must provide  SL95 (LB_pars@SL95)"
    t6 = "Size Structure"
    t7 = "Target"
    t8 = "Sample"
    t9 = "SPR Target: "
    t10 = "Length"
    t11 = "Frequency"
  }
  
  if (class(LB_pars) != "LB_pars") stop(t1, call. = FALSE)
  if (class(LB_lengths) != "LB_lengths") stop(t2, call. = FALSE)

  if (length(LB_pars@SPR) < 1) stop(t3, call. = FALSE)
  if (length(LB_pars@SL50) < 1) stop(t4, call. = FALSE)
  if (length(LB_pars@SL95) < 1) stop(t5, call. = FALSE)

  scales <- match.arg(scales)
  LMids <- LB_lengths@LMids
  LB_pars@BinWidth <- LMids[2] - LMids[1]
  LB_pars@BinMin <- min(LMids) - 0.5 * LB_pars@BinWidth
  LB_pars@BinMax <- max(LMids) + 0.5 * LB_pars@BinWidth

  # escala prevista para amostra
  ScaleCatch <- function(Scale, Sample, PredCatch) {
    ind <- which.max(Sample)
    if (ind < 1) ind <- 1
    wght <- Sample[1:ind]
    sum((((PredCatch[1:ind] * Scale) -  Sample[1:ind]) * wght)^2)
  }

  nyr <- length(yr)
  years <- as.numeric(colnames(LB_lengths@LData[,yr]))
  if (any(is.na(years))) years <- 1:nyr
  if (length(years) <1) years <- 1:nyr

  pLCatch <- matrix(NA, nrow=length(LMids), ncol=nyr)
  pLSample <- matrix(NA, nrow=length(LMids), ncol=nyr)
  for (x in 1:nyr) {
    LB_pars1 <- LB_pars
    LB_pars1@SL50 <- LB_pars@SL50[x]
    LB_pars1@SL95 <- LB_pars@SL95[x]
    LB_obj <- LBSPRsim(LB_pars1, verbose=FALSE)
    pLCatch[,x] <- LB_obj@pLCatch # composição de tamanho previsto de captura - alvo
    pLSample[,x] <- LB_lengths@LData[,x] #
    Scale <- optimize(ScaleCatch, interval=c(1, 1E10), Sample=pLSample[,x], PredCatch=pLCatch[,x])$minimum
    pLCatch[,x] <- pLCatch[,x] * Scale
  }

  Dat <- data.frame(LMids=LMids, pLCatch=pLCatch, Sample=pLSample)
  longDat <- tidyr::gather(Dat, "PopType", "PLength", !! 2:ncol(Dat))


  longDat[grepl("pLCatch",longDat[,2]),2] <- "pLCatch"
  longDat[grepl("Sample",longDat[,2]),2] <- "Sample"

  Title <- t6
  Leg <- c(t7, t8)
  longDat$alphayr <- c(rep(1, length(LMids)*nyr), rep(0.6, length(LMids)*nyr))
  longDat$Year <-  rep(sort(rep(1:nyr, length(LMids))),2)

  longDat$Year <- factor(longDat$Year)
  levels(longDat$Year) <- years


  SPRtarg <- LB_pars@SPR
  if (SPRtarg <= 1) SPRtarg <- SPRtarg * 100

  targ <- paste0(t9, SPRtarg, "%")

  if (length(LB_obj@L_units) > 0) {
    XLab <- paste0("Length (", LB_obj@L_units, ")")
  } else XLab <- t10
  PopType <- PLength <- alphayr <- NULL # hack to get past CRAN
  
  windowsFonts(familia = windowsFont(letra))
  
  Plot <- ggplot(longDat, aes(x=LMids, y=PLength, fill=PopType, alpha=factor(alphayr))) +
    #facet_wrap(~Year, scales=scales) +
    geom_bar(stat="identity", position = "identity") +
    xlab(XLab) +
    ylab(t11) +
    scale_alpha_manual(values = c("0.6"=0.6, "1"=1), guide='none') +
    theme_bw() +
    theme(axis.text=element_text(size=size.axtex, family = "familia"),
          axis.title=element_text(size=size.title,face="bold", family = "familia"),
          legend.title = element_text(family = "familia"),
          legend.text = element_text(family = "familia"),
          plot.subtitle = element_text(family = "familia"),
          plot.title = element_text(face="bold", family = "familia"),
          strip.text = element_text(family = "familia"))
  if (all(is.null(Cols))) Plot <- Plot + scale_fill_discrete(Title, labels = Leg)
  if (!all(is.null(Cols))) Plot <- Plot + scale_fill_manual(Title, labels = Leg,
                                                             values=Cols)
  if (!is.null(fachada)) Plot <- Plot + facet_wrap(~Year, scales=scales)
  if (!is.null(title)) Plot <- Plot + ggtitle(label=title)
  if (targtext) Plot <- Plot + ggtitle(label=title, subtitle=targ)

  Plot
  
}
