
ggrepel 0.7.0 2017-09-28
----------------------------------------------------------------

FIXES

* Fix intersection between lines and rectangles, to reproduce the same
  aesthetically pleasant behavior as in version 0.6.5. 
  
  This is an improvement on the sloppy implementation introduced in 0.6.8. See
  [commit28633d] for more information.

[commit28633d]: https://github.com/slowkow/ggrepel/commit/28633db5eb3d3cc2bd935bd438a8bb36b5673951

ggrepel 0.6.12 2017-07-16
----------------------------------------------------------------

NEW FEATURE

* Reproduce identical plots by usign `seed = 1` to set the seed in
  `geom_text_repel()` or `geom_label_repel()`. By default, no seed will be set.
  
  This is an improvement on the sloppy implementation introduced in 0.6.2. See
  issues [33] and [73] for more discussion of this feature. Thanks to Pierre
  Gramme for reminding me about this via email.

[33]: https://github.com/slowkow/ggrepel/issues/33
[73]: https://github.com/slowkow/ggrepel/issues/73

ggrepel 0.6.11 2017-07-08
----------------------------------------------------------------

CHANGES (thanks to @seaaan)

* Allow certain arguments to be passed as numbers or `unit()`
  instead of only units. See [issue 79][79].

[79]: https://github.com/slowkow/ggrepel/issues/79

ggrepel 0.6.10 2017-03-07
----------------------------------------------------------------

FIXES (thanks to @zkamvar)

* Fix the crash for plots that do not specify `xlim` or `ylim`.
  See [pull 74][74].

[74]: https://github.com/slowkow/ggrepel/pull/74

ggrepel 0.6.9 2017-03-07
----------------------------------------------------------------

FIXES (thanks to @pcroteau)

* Fix the crash for plots with `facet_wrap` or `facet_grid` that have no
  labeled points. See [pull 70][70].

[70]: https://github.com/slowkow/ggrepel/pull/70

ggrepel 0.6.8 2017-02-12
----------------------------------------------------------------

NEW FEATURE (thanks to @AliciaSchep)

* Constrain repulsion force to x-axis "x" or y-axis "y" with `direction` in
  `geom_text_repel()` and `geom_label_repel()`. See [pull 68][68].

[68]: https://github.com/slowkow/ggrepel/pull/68

ggrepel 0.6.7 2017-01-09
----------------------------------------------------------------

CHANGES (thanks to @lukauskas)

* Constrain text labels to specific areas of the plot with `xlim` and `ylim` in
  `geom_text_repel()` and `geom_label_repel()`. See [pull 67][67].

[67]: https://github.com/slowkow/ggrepel/pull/67

ggrepel 0.6.6 2016-11-28
----------------------------------------------------------------

FIXES (thanks to @fawda123)

* Mathematical expressions as labels with `parse = TRUE` in 
  `geom_text_repel()` and `geom_label_repel()`. See [issue 60][60].

[60]: https://github.com/slowkow/ggrepel/issues/60

ggrepel 0.6.5 2016-11-22
----------------------------------------------------------------

CHANGES (thanks to @jiho)

* changed `alpha` in `geom_label_repel()` to control text, label
  background, label border, and segment.

* Allow `segment.colour` as well as `segment.color`.

* By default, map text color and text alpha to the segment color unless they
  are overridden.

FIXES (thanks to @jiho)

* Call `scales::alpha()` instead of `alpha()`.

ggrepel 0.6.4 2016-11-08
----------------------------------------------------------------

FIXES

* Fix a bug that caused ggrepel to fail on polar coordinates `coord_polar()`.
  See [issue 56][56].

[56]: https://github.com/slowkow/ggrepel/issues/56

ggrepel 0.6.3 2016-10-14
----------------------------------------------------------------

NEW FEATURES

* Use `point.padding=NA` to ignore data points in repulsion calculations.

ggrepel 0.6.2 2016-10-06
----------------------------------------------------------------

FIXES

* Stop the labels from escaping the plot boundaries instead of applying
  a force at the boundary.

* Call `set.seed` within `geom_text_repel()` and `geom_label_repel()` to
  allow recreating identical plots. Fixes [issue 33][33].

[33]: https://github.com/slowkow/ggrepel/issues/33

NEW FEATURES

* Add `min.segment.length` to `geom_text_repel()` and `geom_label_repel()`.

ggrepel 0.6.1 2016-10-04
----------------------------------------------------------------

CHANGES

* Tweak `repel_boxes.cpp`. Dampen forces to tune how the labels move. The
  result looks better, at least for the examples in the vignette.

ggrepel 0.6.0 2016-10-03
----------------------------------------------------------------

NEW FEATURES

* Do not draw labels with empty strings. When a label is an empty string,
  the text will not be shown, the segment will not be drawn, but the
  corresponding data point will repel other labels. See [issue 51][51].

[51]: https://github.com/slowkow/ggrepel/issues/51

* Add `segment.alpha` as an option for `geom_text_repel()` and
  `geom_label_repel()`.

* Implement `angle` aesthetic for `geom_text_repel()`, the same way as done in
  ggplot2 `geom_text()`.

CHANGES

* Move `nudge_x` and `nudge_y` out of the aesthetics function `aes()`. This
  makes ggrepel consistent with ggplot2 functions `geom_text()` and
  `geom_label()`. Backwards incompatible with 0.5.1.

* Restore `segment.color` as an option for `geom_text_repel()` and
  `geom_label_repel()`.

* Tweak `repel_boxes.cpp`. Do not weight repulsion force by ratios of bounding
  box heights and widths. This seems to perform better, especially after
  rotating text labels.

ggrepel 0.5.1 2016-02-22
----------------------------------------------------------------

* Optimize C++ code further by reducing number of calls to `rnorm()`.

ggrepel 0.5 2016-02-08
----------------------------------------------------------------

* First push to CRAN.

ggrepel 0.4.6 2016-02-07
----------------------------------------------------------------

CHANGES

* Tweak `point.padding` so that users can configure how far labels are pushed
  away from data points.

ggrepel 0.4.5 2016-02-06
----------------------------------------------------------------

CHANGES

* Optimize C++ code for a 2.5X speed improvment.

* Delete unnecessary .Rd files.

ggrepel 0.4.4 2016-02-05
----------------------------------------------------------------

FIXES

* Fix the bug when the line segment from the data point points to the origin
  at (0,0) instead of the text label.

CHANGES

* Automatically recompute repulsion between labels after resizing the plot.

ggrepel 0.4.3 2016-01-18
----------------------------------------------------------------

CHANGES

* Change distance between segment and label in `geom_label_repel()`. Now there
  is no gap between the end of the segment and the label border.

ggrepel 0.4.2 2016-01-15
----------------------------------------------------------------

FIXES

* Fix `spring_force()` so that it never returns NaN.

CHANGES

* Add `nudge_x` and `nudge_y` to better control positioning of labels.

ggrepel 0.4.1 2016-01-13
----------------------------------------------------------------

CHANGES

* Add `arrow` parameter to allow plotting arrows that point to the labeled data
  points rather than plain line segments.

* Always draw segments, even if the labeled point is very close to the label.

FIXES

* Fix `point.padding` so that horizontal and vertical padding is calculated
  correctly.

* Tweak forces to improve layout near borders and in crowded areas.

ggrepel 0.4 2016-01-12
----------------------------------------------------------------

FIXES

* Fix [issue 7][7]. Labels can now be placed anywhere in the plotting area
  instead of being limited to the x and y ranges of their corresponding data
  points.

[7]: https://github.com/slowkow/ggrepel/issues/7

* Fix DESCRIPTION to require ggplot2 >= 2.0.0

CHANGES

* Add new parameter `point.padding` to add padding around the labeled points.
  The line segment will stop before reaching the coordinates of the point. The
  text labels are also now padded from the line segment to improve legibility.

* Add volcano plot to the vignette usage examples.

* Add Travis continuous integration to test against R-devel, R-release, and
  R-oldrel.

* Dampen repulsion force to slightly improve algorithm efficiency.

* Move `intersect_line_rectangle()` to `src/repel_boxes.cpp`.


ggrepel 0.3 2016-01-08
----------------------------------------------------------------

CHANGES

* Remove unused imports: `colorspace`.

* Update NAMESPACE with new version of roxygen.

* Use spring force to attract each label to its own point.

* Change default maximum iterations from 10,000 to 2000.

* Update man pages.

* Remove unused code.

ggrepel 0.2.0 2016-01-07
----------------------------------------------------------------

CHANGES

* Update `geom_text_repel()` and `geom_label_repel()`.

    * Change `label.padding` to `box.padding`.

    * Remove unsupported parameters:
        * position
        * nudge_x
        * nudge_y
        * hjust
        * vjust

* Remove unused imports.

DOCUMENTATION

* Add roxygen docs to all functions.

ggrepel 0.1.0 2016-01-05
----------------------------------------------------------------

NEW FEATURES

* Add `geom_label_repel()`.

* Add fudge width to help with legends.

* Add `expand=TRUE` to allow text to be placed in the expanded plot area.

* Add man/ folder.

* Add links to ggplot2 docs in vignette.

MINOR FEATURES

* Add unused R implementation of `repel_boxes()`, just for your reference.

ggrepel 0.0.1 2016-01-04
----------------------------------------------------------------

* Initial release to github.
