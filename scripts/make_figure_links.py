#!/usr/bin/env python

thumbs = [
    {
        'src': 'vignettes/figures/ggrepel/empty_string-1.png',
        'href': 'vignettes/ggrepel.md#hide-some-of-the-labels'
    },
    {
        'src': 'vignettes/figures/ggrepel/point_padding_na-1.png',
        'href': 'vignettes/ggrepel.md#do-not-repel-labels-from-data-points'
    },
    {
        'src': 'vignettes/figures/ggrepel/direction_y-1.png',
        'href': 'vignettes/ggrepel.md#align-text-labels'
    },
    {
        'src': 'vignettes/figures/ggrepel/xlim-1.png',
        'href': 'vignettes/ggrepel.md#limit-labels-to-a-specific-area'
    },
    {
        'src': 'vignettes/figures/ggrepel/polar-1.png',
        'href': 'vignettes/ggrepel.md#polar-coordinates'
    },
    {
        'src': 'vignettes/figures/ggrepel/math-1.png',
        'href': 'vignettes/ggrepel.md#mathematical-expressions'
    }
]

if __name__ == '__main__':
    template = '<a href="{href}"><img style="margin:1rem;" width="30%" src="{src}" /></a>'
    strings = [template.format(**thumb) for thumb in thumbs]
    print(' '.join(strings))
