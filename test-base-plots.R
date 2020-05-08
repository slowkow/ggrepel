x = mtcars$wt
y = mtcars$mpg
labels = rownames(mtcars)

repel_boxes2 = ggrepel:::repel_boxes2

# ---- DEFAULTS ----
par(mfrow = 1:2)
plot(x, y, main = 'text()')
text(x, y, labels)

plot(x, y, main = 'repel_text()')
text_repel(x, y, labels)

# ---- ADJ ----
par(mfrow = c(3L, 3L), mar = c(0, 0, 0, 0))
for (xadj in c(0, .5, 1)) {
    for (yadj in c(0, .5, 1)) {
        plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
        box()
        mtext(side = 3L, line = -1, sprintf('xadj=%.1f | yadj=%.1f', xadj, yadj))
        text(x, y, labels, adj = c(xadj, yadj), col = 'gray')
        text_repel(x, y, labels, adj = c(xadj, yadj))
    }
}

# ---- POS ----
par(mfrow = c(2L, 2L))
for (pos in 1:4) {
    plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
    box()
    mtext(side = 3L, line = -1, sprintf('pos=%d', pos))
    text(x, y, labels, pos = pos, col = 'gray')
    text_repel(x, y, labels, pos = pos)
}

# ---- POS WITH OFFSET ----
par(mfrow = 3:4)
for (offset in c(0, .5, 1)) {
    for (pos in 1:4) {
        plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
        box()
        mtext(side = 3L, line = -1, sprintf('pos=%d | offset=%.1f', pos, offset))
        text(x, y, labels, pos = pos, offset = offset, col = 'gray')
        text_repel(x, y, labels, pos = pos, offset = offset)
    }
}

# --- VFONT ---
par(mfrow = c(3L, 3L), mar = c(0, 0, 0, 0))
for (ii in sample(nrow(Hershey$allowed), 9L)) {
    vfont = c(
        Hershey$typeface[Hershey$allowed[ii, 1L]],
        Hershey$fontindex[Hershey$allowed[ii, 2L]]
    )
    plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
    box()
    mtext(side = 3L, line = -1, sprintf('vfont=[%s,%s]', vfont[1L], vfont[2L]))
    text(x, y, labels, vfont = vfont, col = 'gray')
    text_repel(x, y, labels, vfont = vfont)
}

# ---- CEX ----
par(mfrow = c(3L, 3L), mar = c(0, 0, 0, 0))
for (cex in c(.5, .6, .8, .9, 1, 1.1, 1.25, 1.33, 2, 5)) {
    plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
    box()
    mtext(side = 3L, line = -1, sprintf('cex=%.2f', cex))
    text(x, y, labels, cex = cex, col = 'gray')
    text_repel(x, y, labels, cex = cex)
}

# ---- CEX AS VECTOR ----
par(mfrow = c(1L, 1L))
plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
box()
cex = runif(length(x), .5, 1.5)
text(x, y, labels, cex = cex, col = 'gray')
text_repel(x, y, labels, cex = cex)

# ---- FONT ----
## per src/library/graphics/src/plot.c:FixupFont, font
##   can be 1,2,3,4,5 on Unix, up to 32 on Windows
par(mfrow = c(2L, 2L), mar = c(0, 0, 0, 0))
for (font in 1:4) {
    plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
    box()
    mtext(side = 3L, line = -1, sprintf('font=%d', font))
    text(x, y, labels, font = font, col = 'gray')
    text_repel(x, y, labels, font = font)
}

# ---- FONT AS VECTOR ----
par(mfrow = c(1L, 1L))
plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
box()
font = sample(1:4, length(x), replace = TRUE)
text(x, y, labels, font = font, col = 'gray')
text_repel(x, y, labels, font = font)

# ---- PAR(PS) ----
par(mfrow = c(2L, 2L), mar = c(0, 0, 0, 0))
old = par('ps')
for (ps in c(8, 12, 18, 24)) {
    plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
    box()
    mtext(side = 3L, line = -1, sprintf('ps=%d', ps))
    par(ps = ps)
    text(x, y, labels, col = 'gray')
    text_repel(x, y, labels)
}
par(ps = old)

# ---- POINT.PADDING ----
par(mfrow = c(2L, 2L), mar = c(0, 0, 0, 0))
for (padding in c(0, .5, 1, 2)) {
    plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
    box()
    mtext(side = 3L, line = -1, sprintf('point.padding=%.1f', padding))
    text(x, y, labels, col = 'gray')
    text_repel(x, y, labels, point.padding = padding)
}

# ---- FORCE, FORCE_PULL----
par(mfrow = c(3L, 3L), mar = c(0, 0, 0, 0))
for (force in c(.5, 1, 5)) {
    for (force_pull in c(.5, 1, 5)) {
        plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
        box()
        mtext(side = 3L, line = -1, sprintf('force=%.1f | force_pull=%.1f', force, force_pull))
        text(x, y, labels, col = 'gray')
        text_repel(x, y, labels, force = force, force_pull = force_pull)
    }
}

# ---- MAX.TIME ----

# ---- MAX.ITER ----

# ---- MAX.OVERLAPS ----

# ---- DIRECTION ----

# ---- ERRORS ----
