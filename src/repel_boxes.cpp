#include <Rcpp.h>
using namespace Rcpp;

// //' Find the intersections between a line and a rectangle.
// //' @param p1 A point c(x, y)
// //' @param p2 A point c(x, y)
// //' @param b A rectangle c(x1, y1, x2, y2)
// NumericVector intersect_line_rectangle(
//     NumericVector p1, NumericVector p2, NumericVector b
// ) {
//   double slope = (p2[1] - p1[1]) / (p2[0] - p1[0]);
//   double intercept = p2[1] - p2[0] * slope;
//   NumericMatrix retval(4, 2);
//
//   double x, y;
//
//   x = b[0];
//   y = slope * x + intercept;
//   if (b[1] <= y && y <= b[3]) {
//     retval(0, _) = NumericVector::create(x, y);
//   }
//
//   x = b[2];
//   y = slope * x + intercept;
//   if (b[1] <= y && y <= b[3]) {
//     retval(1, _) = NumericVector::create(x, y);
//   }
//
//   y = b[1];
//   x = (y - intercept) / slope;
//   if (b[0] <= y && y <= b[2]) {
//     retval(2, _) = NumericVector::create(x, y);
//   }
//
//   y = b[3];
//   x = (y - intercept) / slope;
//   if (b[0] <= y && y <= b[2]) {
//     retval(3, _) = NumericVector::create(x, y);
//   }
//
//   int i = 0;
//   int imin = 0;
//   double d = euclid(retval(i, _), p1);
//   double dmin = d;
//   for (i = 1; i < 4; i++) {
//     d = euclid(retval(i, _), p1);
//     if (d < dmin) {
//       d = dmin;
//       imin = i;
//     }
//   }
//
//   return retval(imin, _);
// }

//' Euclidean distance between two numeric vectors.
//' @param p1 A numeric vector.
//' @param p2 A numeric vector.
// [[Rcpp::export]]
double euclid(NumericVector p1, NumericVector p2) {
  double retval = 0;
  int n = p1.size();
  for (int i = 0; i < n; i++) {
    retval += pow(p2[i] - p1[i], 2);
  }
  return sqrt(retval);
}

//' Move a box into the area specificied by x limits and y limits.
//' @param b A numeric vector representing a box like \code{c(x1, y1, x2, y2)}
//' @param xlim A numeric vector representing the limits on the x axis like
//'   \code{c(xmin, xmax)}
//' @param ylim A numeric vector representing the limits on the y axis like
//'   \code{c(ymin, ymax)}
// [[Rcpp::export]]
NumericVector put_within_bounds(
    NumericVector b, NumericVector xlim, NumericVector ylim
) {
  double d;
  if (b[0] < xlim[0]) {
    d = fabs(b[0] - xlim[0]);
    b[0] += d;
    b[2] += d;
  } else if (b[2] > xlim[1]) {
    d = fabs(b[2] - xlim[1]);
    b[0] -= d;
    b[2] -= d;
  }
  if (b[1] < ylim[0]) {
    d = fabs(b[1] - ylim[0]);
    b[1] += d;
    b[3] += d;
  } else if (b[3] > ylim[1]) {
    d = fabs(b[3] - ylim[1]);
    b[1] -= d;
    b[3] -= d;
  }
  return b;
}

//' Get the coordinates of the center of a box.
//' @param b A numeric vector representing a box like \code{c(x1, y1, x2, y2)}
// [[Rcpp::export]]
NumericVector centroid(NumericVector b) {
  return NumericVector::create((b[0] + b[2]) / 2, (b[1] + b[3]) / 2);
}

//' Test if a box overlaps another box.
//' @param a A numeric vector representing a box like \code{c(x1, y1, x2, y2)}
//' @param b A numeric vector representing a box like \code{c(x1, y1, x2, y2)}
// [[Rcpp::export]]
bool overlaps(NumericVector a, NumericVector b) {
  return
    b[0] <= a[2] &&
    b[1] <= a[3] &&
    b[2] >= a[0] &&
    b[3] >= a[1];
}

//' Test if a point is within the boundaries of a box.
//' @param p A point like \code{c(x, y)}
//' @param b A numeric vector representing a box like \code{c(x1, y1, x2, y2)}
// [[Rcpp::export]]
bool point_within_box(NumericVector p, NumericVector b) {
  return
    p[0] >= b[0] &&
    p[0] <= b[2] &&
    p[1] >= b[1] &&
    p[1] <= b[3];
}

//' Compute the repulsion force upon point \code{a} from point \code{b}.
//'
//' The force decays with the squared distance between the points, similar
//' to the force of repulsion between magnets.
//'
//' @param a A point like \code{c(x, y)}
//' @param b A point like \code{c(x, y)}
//' @param force Magnitude of the force (defaults to \code{1e-6})
// [[Rcpp::export]]
NumericVector repel_force(
    NumericVector a, NumericVector b, double force = 0.000001
) {
  a += rnorm(2, 0, force);
  // Constrain the minimum distance to be at least 0.01.
  double d = std::max(euclid(a, b), 0.01);
  // Compute a unit vector in the direction of the force.
  NumericVector v = (a - b) / d;
  // Divide the force by the squared distance.
  return force * v / pow(d, 2);
}

//' Compute the spring force upon point \code{a} from point \code{b}.
//'
//' The force increases with the distance between the points, similar
//' to Hooke's law for springs.
//'
//' @param a A point like \code{c(x, y)}
//' @param b A point like \code{c(x, y)}
//' @param force Magnitude of the force (defaults to \code{1e-6})
// [[Rcpp::export]]
NumericVector spring_force(
    NumericVector a, NumericVector b, double force = 0.000001
) {
  double d = euclid(a, b);
  d = d < 0.01 ? 0 : d;
  // Compute a unit vector in the direction of the force.
  NumericVector v = (a - b) / d;
  return v * force * d;
}

//' Adjust the layout of a list of potentially overlapping boxes.
//' @param boxes A list of numeric vectors representing a box like
//'   \code{list(c(x1, y1, x2, y2), c(x1, y1, x2, y2), ...)}
//' @param xlim A numeric vector representing the limits on the x axis like
//'   \code{c(xmin, xmax)}
//' @param ylim A numeric vector representing the limits on the y axis like
//'   \code{c(ymin, ymax)}
//' @param force Magnitude of the force (defaults to \code{1e-6})
//' @param maxiter Maximum number of iterations to try to resolve overlaps
//'   (defaults to 2000)
// [[Rcpp::export]]
DataFrame repel_boxes(
    NumericMatrix boxes, NumericVector xlim, NumericVector ylim,
    double force = 1e-6, int maxiter = 2000
) {
  int n = boxes.nrow();
  int iter = 0;
  bool any_overlaps = true;

  if (NumericVector::is_na(force)) {
    force = 1e-6;
  }

  // height over width
  NumericVector ratios(n);
  NumericVector b(4);
  for (int i = 0; i < n; i++) {
    b = boxes(i, _);
    ratios[i] = (b[3] - b[1]) / (b[2] - b[0]);
  }
  NumericMatrix original_centroids(n, 2);
  for (int i = 0; i < n; i++) {
    original_centroids(i, _) = centroid(boxes(i, _));
  }

  // NumericVector forces(maxiter);
  NumericVector f(2);
  NumericVector ci(2);
  NumericVector cj(2);

  while (any_overlaps && iter < maxiter) {
    iter += 1;
    any_overlaps = false;

    for (int i = 0; i < n; i++) {
      f[0] = 0;
      f[1] = 0;

      ci = centroid(boxes(i, _));

      for (int j = 0; j < n; j++) {

        cj = centroid(boxes(j, _));

        if (i == j) {
          // Repel the box from its own centroid.
          if (point_within_box(original_centroids(i, _), boxes(i, _))) {
            any_overlaps = true;
            f = f + repel_force(ci, original_centroids(i, _), force);
          }
        } else {
          // Repel the box from overlapping boxes.
          if (overlaps(boxes(i, _), boxes(j, _))) {
            any_overlaps = true;
            f = f + repel_force(ci, cj, force);
          }
          // Repel the box from overlapping centroids.
          if (point_within_box(original_centroids(j, _), boxes(i, _))) {
            any_overlaps = true;
            f = f + repel_force(ci, original_centroids(j, _), force);
          }
        }
      }

      // Pull toward the label's point.
      if (!any_overlaps) {
        f = f + spring_force(original_centroids(i, _), ci, force);
      }

      // Scale the x force by the ratio of height/width.
      f[0] = f[0] * ratios[i];

      // forces[iter - 1] += fabs(f[0]) + fabs(f[1]);

      b = boxes(i, _);
      boxes(i, _) = NumericVector::create(
        b[0] + f[0], b[1] + f[1], b[2] + f[0], b[3] + f[1]
      );
      boxes(i, _) = put_within_bounds(boxes(i, _), xlim, ylim);
    }
  }

  // Debug messages.
  // Rcout << "iter " << iter << std::endl;
//   Rcout << "c(";
//   for (int k = 0; k < maxiter; k++) {
//     Rcout << forces[k];
//     if (k < maxiter - 1) {
//       Rcout << ",";
//     }
//   }
//   Rcout << ")";

  NumericVector xs(n);
  NumericVector ys(n);

  for (int i = 0; i < n; i++) {
    b = boxes(i, _);
    xs[i] = (b[0] + b[2]) / 2;
    ys[i] = (b[1] + b[3]) / 2;
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("x") = xs,
    Rcpp::Named("y") = ys
  );
}

