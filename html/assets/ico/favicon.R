library("R.utils")
use("R.devices")

for (width in c(16, 32, 48, 72)) {
  sizeTag <- sprintf("%dx%d", width, width)
  toFavicon({
    plot(1, col="blue", bg="orange", pch=21, cex=4, lwd=4, axes=FALSE)
  }, tags=sizeTag, width=width, field="pathname", path=".")
}

copyFile("favicon,32x32.png", "favicon.png", overwrite=TRUE)
