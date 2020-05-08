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


par(ps = 20)
par(mfrow = 1:2)
