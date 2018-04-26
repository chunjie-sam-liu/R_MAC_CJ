library(grid)

stickperson <- function() {
  grid.circle(x = 0.5, y = 0.8, r = 0.1, gp = gpar(fill = "yellow"))
  grid.lines(c(0.5, 0.5), c(0.7, 0.2))
  grid.lines(c(0.5, 0.7), c(0.6, 0.7))
  grid.lines(c(0.5, 0.3), c(0.6, 0.7))
  grid.lines(c(0.5, 0.65), c(0.2, 0))
  grid.lines(c(0.5, 0.35), c(0.2, 0))
}

stickperson()

vp1 <- viewport(x = 0.5, y = 0.75, width = 0.6, height = 0.3)
pushViewport(vp1)
grid.rect(gp = gpar(lty = "dashed"))
grid.circle(gp = gpar(col = "blue"))
upViewport()
grid.circle(gp = gpar(col = "purple"))
upViewport(2)

library(grid)
pushViewport(viewport())
grid.lines(c(0.05, 0.95), c(0.95, 0.05))
grid.lines(c(0.05, 0.95), c(0.05, 0.95))
for (i in 1:100) {
  vp <- viewport(h = 0.9, w = 0.9)
  pushViewport(vp)
  grid.rect()
}
upViewport(100)

stickperson <- function() {
  grid.circle(x = 0.5, y = 0.8, r = 0.1, gp = gpar(fill = "yellow"))
  grid.lines(c(0.5, 0.5), c(0.7, 0.2))
  grid.lines(c(0.5, 0.7), c(0.6, 0.7))
  grid.lines(c(0.5, 0.3), c(0.6, 0.7))
  grid.lines(c(0.5, 0.65), c(0.2, 0))
  grid.lines(c(0.5, 0.35), c(0.2, 0))
}


for (i in 1:30) {
  vp <- viewport(h = 0.9, w = 0.9)
  pushViewport(vp)
  if (i == 5) {
    pushViewport(viewport(x = 0.8))
    stickperson()
    upViewport()
  }
  if (i == 20) {
    pushViewport(viewport(x = 0.2))
    stickperson()
    upViewport()
  }
  if (i == 30) stickperson()
}


escape_prop <-
  c(
    0.24, 0.28, 0.28, 0.33, 0.33, 0.32, 0.3, 0.21, 0.3, 0.28, 0.17,
    0.27, 0.21, 0.18, 0.22, 0.21, 0.19, 0.17, 0.17, 0.15, 0.25, 0.19,
    0.19, 0.22, 0.21, 0.18, 0.24, 0.23, 0.27, 0.16, 0.17, 0.22, 0.17,
    0.25, 0.19, 0.25, 0.12, 0.17, 0.22, 0.22
  )

nfires <- c(
  953, 620, 584, 839, 1415, 1180, 656, 408, 872, 965, 853,
  1492, 951, 772, 1541, 1114, 479, 860, 1166, 1208, 657, 1140,
  1223, 1275, 489, 932, 1096, 1378, 1033, 889, 1046, 818, 1213,
  782, 962, 1666, 2017, 1689, 1885, 1435
)


nfirescode <- nfires / max(nfires)
index <- (1:40) / 41

pushViewport(viewport(width = .9, height = .9))
pushViewport(viewport(y = .75, width = .9, height = .9))
for (i in 1:40) {
  vp <- viewport(x = index[i], y = escape_prop[i], height = .03, width = .03)
  pushViewport(vp)
  grid.circle(r = sqrt(nfirescode[i] / pi))
  upViewport()
}

grid.xaxis(at = c(0, index[c(10, 20, 30, 40)]), label = seq(1960, 2000, 10))
grid.yaxis(at = seq(0, .5, .1))
grid.text("Proportion of Escaped Fires", y = .6)


pushViewport(viewport())
burningtree <- function() {
  grid.rect(x = .5, y = .2, width = .2, height = .4, gp = gpar(fill = "grey", col = NA))
  grid.circle(x = .5, y = .5, r = .3, gp = gpar(fill = "orange", col = NA))
  pushViewport(viewport(clip = "on"))
  pushViewport(viewport(x = .5, y = 0, angle = 45))
  grid.rect(x = .5, y = .5, width = .2, height = .2, gp = gpar(fill = "grey", col = NA))
  upViewport(2)
}
burningtree()



pushViewport(viewport(width = .9, height = .9))
grid.rect(gp = gpar(lty = "dashed"))
pushViewport(viewport(y = .75, width = .9, height = .9))
grid.rect(gp = gpar(lty = "dashed"))

for (i in 1:40) {
  vp <- viewport(x = index[i], y = escape_prop[i], height = nfirescode[i] / 10, width = .03)
  pushViewport(vp)
  burningtree()
  upViewport()
}

grid.yaxis(at = seq(0, .5, .1))
grid.xaxis(at = c(0, index[c(10, 20, 30, 40)]), label = seq(1960, 2000, 10))
grid.text("Proportion of Escaped Fires", y = .6)


library(grid)
gr <- rectGrob(width = 0.1, height = 0.1, name = "gr")
grid.draw(gr)
gr1 <- editGrob(gr, vp = viewport(x = 0.2, y = 0.6), name = "gr1")
grid.draw(gr1)
gr2 <- editGrob(gr, vp = viewport(x = 0.7, y = 0.75), name = "gr2")
grid.draw(gr2)
gr3 <- editGrob(gr, vp = viewport(x = 0.5, y = 0.4), name = "gr3")
grid.draw(gr3)

gr1 <- grid.edit("gr1", vp = viewport(x = .2,y = .6, angle = 30))
gr2 <- grid.edit("gr2", vp = viewport(x = 0.7, y = 0.75, angle = 63))
gr3 <- grid.edit("gr3", vp = viewport(x = 0.5, y = 0.4, angle = 72))

for (i in 1:1000) {
  grid.edit('gr1', vp = viewport(x = 0.2, y = 0.6, angle = i))
  grid.edit("gr2", vp = viewport(x = 0.7, y = 0.75, angle = i * 2))
  grid.edit("gr3", vp = viewport(x = 0.5, y = 0.4, angle = i * 3))
}

library(grid)
pushViewport(vp = viewport())
b2 <- sqrt(1/cos(36 * pi/180) ^ 2 - 1) / 2
b3 <- sin(72 * pi/180) / (2 * (1 + cos(72 * pi / 180))) - (1 - sin(72 * pi / 180)) / 2

triangle2 <- polygonGrob(c(0, 0.5, 1), c(b3, b2 + b3, b3), name = "triangle2", gp = gpar(fill = "yellow", col = 0))
grid.draw(triangle2)

for (i in 0:2) {
  pushViewport(vp = viewport(angle = 72 * i))
  grid.draw(triangle2)
  upViewport()
}

