
# to upper the first letter 
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#### --- ###
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


# function of binomial smooth
# function from here
# https://ggplot2.tidyverse.org/reference/geom_smooth.html

poisson_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}



# transform coordinates
# from here https://stackoverflow.com/questions/14404596/converting-geo-coordinates-from-degree-to-decimal
angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

### funcao para organizar o df do wordcloud

function_freq_texto_wc <- function (df_freq) {
  
  
  texto <- paste0(df_freq,collapse=" ")
  
  ### padronizar todas as palavras para minusculo 
  texto <- tolower(texto)
  
  ### reover alguns caracteres especiais
  texto <- gsub ("\\:","",texto)
  texto <- gsub (",","",texto)
  texto <- gsub ("\\?","",texto)
  texto <- gsub ("\\.","",texto)
  texto <- gsub ("\\)","",texto)
  texto <- gsub ("\\(","",texto)
  texto <- gsub ("\n","",texto)
  texto <- gsub ("@","",texto)
  texto <- gsub (";","",texto)
  texto <- gsub ("\t","",texto)
  texto <- gsub ("=","",texto)
  texto <- gsub ("-","",texto)
  texto <- gsub ("Î±","",texto)
  texto <- gsub ("Â©","",texto)
  
  ## quem sabe seja legal tirar functional e trait, já que está implicito na busca
  #texto <- gsub ("trait*","",texto)
  #texto <- gsub ("funct*","",texto)
  texto <- gsub ("approach*","",texto)
  texto <- gsub ("study","",texto)
  texto <- gsub ("studies","",texto)
  texto <- gsub ("author*","",texto)
  texto <- gsub ("elsevier","",texto)
  
  ## remover alguns pronomes
  texto <- gsub ("small*","",texto)
  texto <- gsub ("great*","",texto)
  texto <- gsub ("large*","",texto)
  texto <- gsub ("little","",texto)
  texto <- gsub ("across","",texto)
  texto <- gsub ("between","",texto)
  texto <- gsub ("where*","",texto)
  texto <- gsub ("through","",texto)
  texto <- gsub ("*though","",texto)
  texto <- gsub ("whether","",texto)
  texto <- gsub ("within","",texto)
  texto <- gsub ("high","",texto)
  texto <- gsub ("differ*","",texto)
  texto <- gsub ("distinc*","",texto)
  texto <- gsub ("however","",texto)
  texto <- gsub ("overall*","",texto)
  texto <- gsub ("analys*","",texto)
  texto <- gsub ("result*","",texto)
  texto <- gsub ("affect*","",texto)
  texto <- gsub ("compare*","",texto)
  texto <- gsub ("increase*","",texto)
  texto <- gsub ("decrease*","",texto)
  texto <- gsub ("suggest*","",texto)
  texto <- gsub ("includ*","",texto)
  texto <- gsub ("significan*","",texto)
  texto <- gsub ("research*","",texto)
  texto <- gsub ("relat*","",texto)
  texto <- gsub ("associat*","",texto)
  texto <- gsub ("explain*","",texto)
  texto <- gsub ("measure*","",texto)
  #texto <- gsub ("estim*","",texto)
  texto <- gsub ("calcul*","",texto)
  texto <- gsub ("observ*","",texto)
  texto <- gsub ("identif*","",texto)
  texto <- gsub ("consist*","",texto)
  texto <- gsub ("reveal*","",texto)
  texto <- gsub ("value*","",texto)
  texto <- gsub ("reflec*","",texto)
  texto <- gsub ("conclu*","",texto)
  texto <- gsub ("influen*","",texto)
  texto <- gsub ("important*","",texto)
  texto <- gsub ("general*","",texto)
  texto <- gsub ("proportion*","",texto)
  texto <- gsub ("provid*","",texto)
  texto <- gsub ("consider*","",texto)
  texto <- gsub ("present*","",texto)
  texto <- gsub ("studied","",texto)
  texto <- gsub ("suppor*","",texto)
  texto <- gsub ("potentia*","",texto)
  texto <- gsub ("during","",texto)
  texto <- gsub ("should","",texto)
  texto <- gsub ("would","",texto)
  texto <- gsub ("pairwise","",texto)
  texto <- gsub ("variable*","",texto)
  texto <- gsub ("factor*","",texto)
  texto <- gsub ("predict*","",texto)
  texto <- gsub ("findin*","",texto)
  texto <- gsub ("investig*","",texto)
  
  ## separar o texto
  texto_sep <- unlist(strsplit(titulo_ingles, split=" "))
  #texto <- paste0(titulo_ingles,collapse=" ")
  
  texto_sep <- texto_sep [which (nchar(texto_sep) >= 6)]
  
  ## df com as frequencias dos termos
  word <- data.frame(word= unique (texto_sep)[order(unique (texto_sep))], 
                     freq= table(texto_sep)[order(names(table(texto_sep)))])
  
  ## retornar word
  return (word)
}



