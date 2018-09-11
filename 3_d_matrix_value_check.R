Info_plot <- function(dat=NULL)
{
  require(plot3D)
  
  par(mfrow=c(1,1))
  len <- dim(dat)[1]
  zz <- dat
  xx <- matrix(rep(1:len,times=len),len,len)
  yy <- t(xx)
  surf3D(xx,yy,zz,
         colkey=TRUE,
         bty="b2",
         contour = TRUE,
         main="Information Matrix")
  
  zz_inv <- solve(zz)
  surf3D(xx,yy,zz_inv,
         colkey=TRUE,
         bty="b2",
         contour = TRUE,
         main="Inverse Information Matrix")
}