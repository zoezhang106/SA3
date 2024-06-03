sensRange <- function( func, parms = NULL, sensvar = NULL, dist = "unif",
         parInput = NULL, parRange = NULL, parMean = NULL, parCovar = NULL,
         map = 1, num = 100, ...) {

  vec2mat <- function(vec) { # make a matrix of a vector
    NN <- names(vec)
    mat <- matrix(data = vec, nrow = 1)
    colnames(mat) <- NN
    mat
  }
  if(is.vector(parInput))
    parInput <- vec2mat(parInput)
  if(is.vector(parRange))
    parRange <- vec2mat(parRange)
  if(is.vector(parMean))
    parMean  <- vec2mat(parMean)
  if(is.vector(parCovar))
    parCovar <- vec2mat(parCovar)

  if(is.vector(parInput))
    parInput <- matrix(data = parInput, nrow = 1)
  if (is.null(parms) & ! is.null(parInput))
    parms <- parInput[1,]
  if (is.null(parms))
    parms <- parMean
  if (is.null(parms)){
    if (is.vector(parRange))
      parms <- mean(parRange)
    else parms <- rowMeans(parRange)
  }
  if (is.null(parms))
    stop ("'parms' not known")
  if (is.matrix(parms) && nrow(parms) > 1)
    stop ("'parms' should be a vector")

  Solve <- function(parms) func(parms, ...)

  ip <- NULL
  if (! is.null(parInput)) {
    dist <- "input"
    nr <- nrow(parInput)
    num <- min(num, nr)
    if (num == nr)
       ip <- 1: nr
    else ip <- sample(1:nr, size = num, replace = FALSE)
    parset <-as.matrix(parInput[ip,])
    if(is.null(parms))
      parms <- parInput[1,]
  }

  Parms <- parms
  Parms_star <- Parms

  ## reference run
  yRef  <- Solve(Parms)
  yRef_star <- yRef

  if (is.vector(yRef)) {
    ynames <- names(yRef)
    yRef <- matrix(data = yRef, nrow = 1)
    colnames(yRef) <- ynames
  } else
  if (is.data.frame(yRef))
    yRef <- as.matrix(yRef)

  ## check sensitivity variables
  if (is.null(sensvar)) {
    ivar    <- 1:ncol(yRef)
    if (! is.null(map))
      ivar <- ivar[-map]
    sensvar <- colnames(yRef)[ivar]
    if(is.null(sensvar))
      sensvar <- ivar
  } else {
    ivar  <- findvar(yRef[1,], sensvar, "variables")
    if (! is.character(sensvar)) {# try to create names rather than nrs
      sv <- sensvar
      sensvar<-colnames(yRef)[ivar]
      if (is.null(sensvar))
        sensvar<-sv
    }
  }
  if (is.null(map))
    map   <- 1:nrow(yRef)
  else map <- yRef[,map]

  nout  <- length(ivar)
  if (nout == 0)
    stop (" should select at least ONE output variable - set map = NULL")
  ndim  <- nrow(yRef)
  grvar <- expand.grid(map, sensvar)
  if (ndim ==1)
    svar <- sensvar
  else svar <- paste(grvar[, 2], grvar[, 1], sep="")

  YREF  <- as.vector(yRef[, ivar])
  Sens  <- matrix(data=NA, nrow = num, ncol = length(YREF))

  ## sensitivity parameters
  senspar <- NULL
  senspar <- colnames(parMean)
  if (is.null(senspar)) senspar <- rownames(parRange)
  if (is.null(senspar)) senspar <- rownames(parCovar)
  if (is.null(senspar)) senspar <- colnames(parCovar)
  if (is.null(senspar)) senspar <- colnames(parInput)
  if (is.null(senspar)) senspar <- names(Parms)
  if (is.null(senspar)) senspar <- 1:length(Parms)

  npar  <- length(senspar)

  ipar <- findvar(Parms, senspar, "parameters")
  pp   <- unlist(Parms)[ipar]

  ## sanity checks for random parameters
  if (dist == "norm" && (is.null(parMean) | is.null(parCovar)))
    stop("parMean and parCovar should be given if dist = norm")
  if(!(dist  %in% c("norm", "input")) && is.null(parRange))
    stop("parRange should be given if dist = unif, grid or latin")

  ## generate random parameters
  if (dist == "norm")
    parset <- Norm (parCovar=parCovar, parMean=parMean, parRange, num) else
  if (dist == "unif")
    parset <- Unif(parRange, num)                      else
  if (dist == "latin")
    parset <- Latinhyper(parRange, num)                else
  if (dist == "grid")
    parset <- Grid(parRange, num)                      else
  if (dist != "input" )
    stop("dist should be one of 'norm', 'unif', 'latin', 'grid' or 'input'")


  ## The sensitivity output
  colnames(Sens) <- svar

  for (i in 1:num) {
    print(i)
    if (prod(Parms[ipar] == parset[i, ]) == 0) { # no need to run model again if same parameter value (e.g. MCMCrun!)
      Parms[ipar]  <- parset[i,]
      yRef <- Solve(Parms)
      # yRef <- Solve(Parms)
      # print(dim(yRef))
      # print(dim(yRef_star))
      # print(dim(Parms[ipar]))
      # print(dim(Parms_star[ipar]))
      # print(dim(((yRef-yRef_star)/yRef_star)))

      y_new <- ((yRef-yRef_star)/yRef_star)/as.numeric((Parms[ipar]-Parms_star[ipar])/Parms_star[ipar])
      # y_new <- ((yRef[ivar]-yRef_star[ivar])/yRef_star[ivar])/as.numeric((Parms[ipar]-Parms_star[ipar])/Parms_star[ipar])
      # print("dim(y_new)")
      # print(dim(y_new))
      # print(head(y_new))
      # print("ivar")
      # print(ivar)
      # print(dim(y_new[,ivar]))
    }
    if (is.vector(y_new)){
      # print("calculating ee1")
      Sens[i, ] <- y_new[,ivar]
      # Sens[i, ] <- yRef[ivar]
    }
    else{
      # print("calculating ee2")
      # print(dim(y_new))
      # print(ivar)
      # print(dim(y_new[,ivar]))
      Sens[i,] <- as.vector(unlist(y_new[,ivar]))   # unlist in case it is a data.frame
      # Sens[i,] <- as.vector(unlist(yRef[,ivar]))   # unlist in case it is a data.frame
    }
  }
  sens<- data.frame(cbind(parset, Sens))
  class(sens) <- c("sensRange", "data.frame")
  attr(sens, "pset") <- ip   # if parInput: which parameters were drawn...
  attr(sens, "npar") <- ncol(parset)
  attr(sens, "x")    <- map
  attr(sens, "nx")   <- length(map)
  attr(sens, "var")  <- sensvar
  return (sens)
}
