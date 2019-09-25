#' addCNA
#' @description modified oncoplot help function
#'
#' @examples
#'
#' @export

addCNA <- function(om, cna, genes){

  colnames(cna) <- gsub("\\.","-",colnames(cna))
  cna.sub <- cna[cna$Hugo_Symbol %in% genes, c(1,match(colnames(om$oncoMatrix),colnames(cna)))]
  rownames(cna.sub) <- cna.sub[,1]
  cna.sub <- cna.sub[,-1]
  cna.sub[cna.sub == -2] <- "Del"
  cna.sub[cna.sub == "2"] <- "Amp"
  cna.sub <- cna.sub[match(genes,rownames(cna.sub)),]

  for(i in 1:nrow(cna.sub)){
    amps <- which(cna.sub[i,]== "Amp")
    dels <- which(cna.sub[i,]== "Del")
    if(length(amps) > 0){
      om$oncoMatrix[i,amps] <- "Amp"
      om$numericMatrix[i,amps] <- max(as.numeric(names(om$vc))) + 1
    }
    if(length(dels)){
      om$oncoMatrix[i,dels] <- "Del"
      om$numericMatrix[i,dels] <- max(as.numeric(names(om$vc))) + 2
    }
  }

  om$vc <- c(om$vc,"Amp","Del")
  names(om$vc) <- as.character(0:(length(om$vc)-1))

  return(om)
}
