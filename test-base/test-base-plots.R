library(ggrepel)
source('R/RcppExports.R')
source('R/base-repel.R')

old_wd = setwd('test-base')

fig_dir = 'figures'
dir.create(fig_dir, showWarnings = FALSE)
open_pdf = function(filename, dim) {
    pdf(file.path(fig_dir, paste0(filename, '.pdf')),
        width = 7*dim[2L], height = 7*dim[1L])
    par(mfrow = dim, mar = c(0, 0, 0, 0), ps = 14)
}

x = mtcars$wt
y = mtcars$mpg
labels = rownames(mtcars)

# ---- DEFAULTS ----
open_pdf('defaults', 1:2)
plot(x, y, main = 'text()', axes = FALSE, xlab = '', ylab = '')
text(x, y, labels)

plot(x, y, main = 'repel_text()', axes = FALSE, xlab = '', ylab = '')
text_repel(x, y, labels)
dev.off()

# ---- ADJ ----
open_pdf('adj', c(3L, 3L))
for (xadj in c(0, .5, 1)) {
    for (yadj in c(0, .5, 1)) {
        plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
        box()
        mtext(side = 3L, line = -2, sprintf('xadj=%.1f | yadj=%.1f', xadj, yadj))
        text(x, y, labels, adj = c(xadj, yadj), col = 'gray')
        text_repel(x, y, labels, adj = c(xadj, yadj))
    }
}
dev.off()

# ---- POS ----
open_pdf('pos', c(2L, 2L))
for (pos in 1:4) {
    plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
    box()
    mtext(side = 3L, line = -2, sprintf('pos=%d', pos))
    text(x, y, labels, pos = pos, col = 'gray')
    text_repel(x, y, labels, pos = pos)
}
dev.off()

# ---- POS WITH OFFSET ----
open_pdf('offset', 3:4)
for (offset in c(0, .5, 1)) {
    for (pos in 1:4) {
        plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
        box()
        mtext(side = 3L, line = -2, sprintf('pos=%d | offset=%.1f', pos, offset))
        text(x, y, labels, pos = pos, offset = offset, col = 'gray')
        text_repel(x, y, labels, pos = pos, offset = offset)
    }
}
dev.off()

# --- VFONT ---
open_pdf('vfont', c(3L, 3L))
for (ii in sample(nrow(Hershey$allowed), 9L)) {
    vfont = c(
        Hershey$typeface[Hershey$allowed[ii, 1L]],
        Hershey$fontindex[Hershey$allowed[ii, 2L]]
    )
    plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
    box()
    mtext(side = 3L, line = -2, sprintf('vfont=[%s,%s]', vfont[1L], vfont[2L]))
    text(x, y, labels, vfont = vfont, col = 'gray')
    text_repel(x, y, labels, vfont = vfont)
}
dev.off()

# ---- CEX ----
open_pdf('cex', c(3L, 3L))
for (cex in c(.5, .6, .8, .9, 1, 1.1, 1.25, 1.33, 2, 5)) {
    plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
    box()
    mtext(side = 3L, line = -2, sprintf('cex=%.2f', cex))
    text(x, y, labels, cex = cex, col = 'gray')
    text_repel(x, y, labels, cex = cex)
}
dev.off()

# ---- CEX AS VECTOR ----
open_pdf('cex_vector', c(1L, 1L))
plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
box()
cex = runif(length(x), .5, 1.5)
text(x, y, labels, cex = cex, col = 'gray')
text_repel(x, y, labels, cex = cex)
dev.off()

# ---- FONT ----
## per src/library/graphics/src/plot.c:FixupFont, font
##   can be 1,2,3,4,5 on Unix, up to 32 on Windows
open_pdf('font', c(2L, 2L))
for (font in 1:4) {
    plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
    box()
    mtext(side = 3L, line = -2, sprintf('font=%d', font))
    text(x, y, labels, font = font, col = 'gray')
    text_repel(x, y, labels, font = font)
}
dev.off()

# ---- FONT AS VECTOR ----
open_pdf('font_vector', c(1L, 1L))
plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
box()
font = sample(1:4, length(x), replace = TRUE)
text(x, y, labels, font = font, col = 'gray')
text_repel(x, y, labels, font = font)
dev.off()

# ---- PAR(PS) ----
open_pdf('ps', c(2L, 2L))
old = par('ps')
for (ps in c(8, 12, 18, 24)) {
    plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
    box()
    mtext(side = 3L, line = -2, sprintf('ps=%d', ps))
    par(ps = ps)
    text(x, y, labels, col = 'gray')
    text_repel(x, y, labels)
}
par(ps = old)
dev.off()

# ---- POINT.PADDING ----
open_pdf('point_padding', c(2L, 2L))
for (padding in c(0, .5, 1, 2)) {
    plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
    box()
    mtext(side = 3L, line = -2, sprintf('point.padding=%.1f', padding))
    text(x, y, labels, col = 'gray')
    text_repel(x, y, labels, point.padding = padding)
}
dev.off()

# ---- FORCE, FORCE_PULL----
open_pdf('force', c(3L, 3L))
for (force in c(.5, 1, 5)) {
    for (force_pull in c(.5, 1, 5)) {
        plot(x, y, axes = FALSE, main = '', xlab = '', ylab = '')
        box()
        mtext(side = 3L, line = -2, sprintf('force=%.1f | force_pull=%.1f', force, force_pull))
        text(x, y, labels, col = 'gray')
        text_repel(x, y, labels, force = force, force_pull = force_pull)
    }
}
dev.off()

# ---- MAX.TIME ----

# ---- MAX.ITER ----

# ---- MAX.OVERLAPS ----

# ---- DIRECTION ----

# ---- ERRORS ----

setwd(old_wd)
