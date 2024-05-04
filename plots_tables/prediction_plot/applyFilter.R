applyFilter <- function (x, kernel) 
{
  if (class(kernel) != "convKern") 
    stop("kernel MUST be a convKern class object")
  if (length(dim(x)) > 3) 
    stop("applyFilter function works only on matrices and 3D arrays")
  extralines <- dim(kernel$matrix)[1]%/%2
  if (class(x)[1] == "matrix") {
    for (n in 1:extralines) x <- cbind(x[, 1], x, x[, ncol(x)])
    for (n in 1:extralines) x <- rbind(x[1, ], x, x[nrow(x), 
    ])
  }
  if (class(x)[1] == "array") {
    for (n in 1:extralines) x <- abind(x[, 1, ], x, x[, dim(x)[2], 
    ], along = 2)
    for (n in 1:extralines) x <- abind(x[1, , ], x, x[dim(x)[1], 
                                                      , ], along = 1)
  }
  Nrow <- dim(x)[1]
  Ncol <- dim(x)[2]
  if (class(x)[1] == "matrix") 
    Nslices <- 1
  else Nslices <- dim(x)[3]
  dataOutput <- x
  kindex <- c()
  for (n in -extralines:extralines) for (m in -extralines:extralines) kindex <- c(kindex, 
                                                                                  n * Nrow + m)
  if ((kernel$kernel == "laplacian") || (kernel$kernel == "emboss")) {
    result <- .C("applyKernelWithoutNorm", as.double(x), 
                 as.double(kernel$matrix), as.integer(extralines), 
                 as.integer(kindex), as.integer(Nrow), as.integer(Ncol), 
                 as.integer(Nslices), as.double(dataOutput))
    result <- result[[8]]
  }
  else if (kernel$kernel != "sobel") {
    result <- .C("applyKernel", as.double(x), as.double(kernel$matrix), 
                 as.integer(extralines), as.integer(kindex), as.integer(Nrow), 
                 as.integer(Ncol), as.integer(Nslices), as.double(dataOutput))
    result <- result[[8]]
  }
  if (kernel$kernel == "sobel") {
    Gx <- .C("applyKernelWithoutNorm", as.double(x), as.double(kernel$matrix), 
             as.integer(extralines), as.integer(kindex), as.integer(Nrow), 
             as.integer(Ncol), as.integer(Nslices), as.double(dataOutput))
    Gx <- Gx[[8]]
    dataOutput <- x
    Gy <- .C("applyKernelWithoutNorm", as.double(x), as.double(t(kernel$matrix)), 
             as.integer(extralines), as.integer(kindex), as.integer(Nrow), 
             as.integer(Ncol), as.integer(Nslices), as.double(dataOutput))
    Gy <- Gy[[8]]
    result <- sqrt(Gx^2 + Gy^2)
  }
  if (class(x)[1] == "matrix") {
    output <- matrix(data = result, nrow = Nrow)
    output <- output[(extralines + 1):(nrow(output) - extralines), 
                     (extralines + 1):(ncol(output) - extralines)]
  }
  if (class(x)[1] == "array") {
    output <- array(data = result, dim = c(Nrow, Ncol, Nslices))
    output <- output[(extralines + 1):(nrow(output) - extralines), 
                     (extralines + 1):(ncol(output) - extralines), ]
  }
  return(output)
}
