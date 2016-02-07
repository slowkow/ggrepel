#include <Rcpp.h>
using namespace Rcpp;

typedef struct {
  double x, y;
} Point;

Point operator -(const Point& a, const Point& b) {
  Point p = {a.x - b.x, a.y - b.y};
  return p;
}

Point operator +(const Point& a, const Point& b) {
  Point p = {a.x + b.x, a.y + b.y};
  return p;
}

Point operator /(const Point& a, const double& b) {
  Point p = {a.x / b, a.y / b};
  return p;
}

Point operator *(const double& b, const Point& a) {
  Point p = {a.x * b, a.y * b};
  return p;
}

Point operator *(const Point& a, const double& b) {
  Point p = {a.x * b, a.y * b};
  return p;
}

typedef struct {
  double x1, y1, x2, y2;
} Box;

Box operator +(const Box& b, const Point& p) {
  Box c = {b.x1 + p.x, b.y1 + p.y, b.x2 + p.x, b.y2 + p.y};
  return c;
}

//' Euclidean distance between two points.
//' @param a A numeric vector.
//' @param b A numeric vector.
//' @return The distance between two points.
//' @noRd
// [[Rcpp::export]]
double euclid(NumericVector a, NumericVector b) {
  return sqrt(
    (a[0] - b[0]) * (a[0] - b[0]) +
    (a[1] - b[1]) * (a[1] - b[1])
  );
}

//' Euclidean distance between two points.
//' @param a A point.
//' @param b A point.
//' @return The distance between two points.
//' @noRd
double euclid(Point a, Point b) {
  return sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y));
}

//' Find the intersections between a line and a rectangle.
//' @param p1 A point like \code{c(x, y)}
//' @param p2 A point like \code{c(x, y)}
//' @param b A rectangle like \code{c(x1, y1, x2, y2)}
//' @noRd
// [[Rcpp::export]]
NumericVector intersect_line_rectangle(
    NumericVector p1, NumericVector p2, NumericVector b
) {
  double slope = (p2[1] - p1[1]) / (p2[0] - p1[0]);
  double intercept = p2[1] - p2[0] * slope;
  NumericMatrix retval(4, 2);
  std::fill(retval.begin(), retval.end(), -INFINITY);

  double x, y;

  x = b[0];
  y = slope * x + intercept;
  if (b[1] <= y && y <= b[3]) {
    retval(0, _) = NumericVector::create(x, y);
  }

  x = b[2];
  y = slope * x + intercept;
  if (b[1] <= y && y <= b[3]) {
    retval(1, _) = NumericVector::create(x, y);
  }

  y = b[1];
  x = (y - intercept) / slope;
  if (b[0] <= x && x <= b[2]) {
    retval(2, _) = NumericVector::create(x, y);
  }

  y = b[3];
  x = (y - intercept) / slope;
  if (b[0] <= x && x <= b[2]) {
    retval(3, _) = NumericVector::create(x, y);
  }

  int i = 0;
  int imin = 0;
  double d;
  double dmin = INFINITY;
  for (i = 0; i < 4; i++) {
    d = euclid(retval(i, _), p1);
    // Rcout << i << " euclid = " << d << std::endl;
    if (d < dmin) {
      dmin = d;
      imin = i;
    }
  }

  return retval(imin, _);
}

//' Move a box into the area specificied by x limits and y limits.
//' @param b A box like \code{c(x1, y1, x2, y2)}
//' @param xlim A Point with limits on the x axis like \code{c(xmin, xmax)}
//' @param ylim A Point with limits on the y axis like \code{c(xmin, xmax)}
//' @param force Magnitude of the force (defaults to \code{1e-6})
//' @noRd
Box put_within_bounds(Box b, Point xlim, Point ylim, double force = 1e-5) {
  double d;
  if (b.x1 < xlim.x) {
    d = std::max(fabs(b.x1 - xlim.x), 0.02);
    b.x1 += force / pow(d, 2);
    b.x2 += force / pow(d, 2);
  } else if (b.x2 > xlim.y) {
    d = std::max(fabs(b.x2 - xlim.y), 0.02);
    b.x1 -= force / pow(d, 2);
    b.x2 -= force / pow(d, 2);
  }
  if (b.y1 < ylim.x) {
    d = std::max(fabs(b.y1 - ylim.x), 0.02);
    b.y1 += force / pow(d, 2);
    b.y2 += force / pow(d, 2);
  } else if (b.y2 > ylim.y) {
    d = std::max(fabs(b.y2 - ylim.y), 0.02);
    b.y1 -= force / pow(d, 2);
    b.y2 -= force / pow(d, 2);
  }
  return b;
}

//' Get the coordinates of the center of a box.
//' @param b A box like \code{c(x1, y1, x2, y2)}
//' @noRd
// [[Rcpp::export]]
NumericVector centroid(NumericVector b) {
  return NumericVector::create((b[0] + b[2]) / 2, (b[1] + b[3]) / 2);
}

//' Get the coordinates of the center of a box.
//' @param b A box like \code{c(x1, y1, x2, y2)}
//' @noRd
Point centroid(Box b) {
  Point p = {(b.x1 + b.x2) / 2, (b.y1 + b.y2) / 2};
  return p;
}

//' Test if a box overlaps another box.
//' @param a A box like \code{c(x1, y1, x2, y2)}
//' @param b A box like \code{c(x1, y1, x2, y2)}
//' @noRd
bool overlaps(Box a, Box b) {
  return
    b.x1 <= a.x2 &&
    b.y1 <= a.y2 &&
    b.x2 >= a.x1 &&
    b.y2 >= a.y1;
}

// //' Test if a point is within the boundaries of a box.
// //' @param p A point like \code{c(x, y)}
// //' @param b A box like \code{c(x1, y1, x2, y2)}
// //' @noRd
// bool point_within_box(Point p, Box b) {
//   return
//     p.x >= b.x1 &&
//     p.x <= b.x2 &&
//     p.y >= b.y1 &&
//     p.y <= b.y2;
// }

//' Compute the repulsion force upon point \code{a} from point \code{b}.
//'
//' The force decays with the squared distance between the points, similar
//' to the force of repulsion between magnets.
//'
//' @param a A point like \code{c(x, y)}
//' @param b A point like \code{c(x, y)}
//' @param force Magnitude of the force (defaults to \code{1e-6})
//' @noRd
Point repel_force(
    Point a, Point b, double force = 0.000001
) {
  NumericVector r = rnorm(2, 0, force);
  a.x += r[0];
  a.y += r[1];
  // Constrain the minimum distance to be at least 0.01.
  double d = std::max(euclid(a, b), 0.02);
  // Compute a unit vector in the direction of the force.
  Point v = (a - b) / d;
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
//' @noRd
Point spring_force(
    Point a, Point b, double force = 0.000001
) {
  double d = euclid(a, b);
  Point v = {0, 0};
  if (d > 0.01) {
    // Compute a unit vector in the direction of the force.
    v = (a - b) / d;
    return v * force * d;
  }
  return v;
}

//' Adjust the layout of a list of potentially overlapping boxes.
//' @param data_points A numeric matrix with rows representing points like
//'   \code{rbind(c(x, y), c(x, y), ...)}
//' @param pad_point_x Padding around each data point on the x axis.
//' @param pad_point_y Padding around each data point on the y axis.
//' @param boxes A numeric matrix with rows representing boxes like
//'   \code{rbind(c(x1, y1, x2, y2), c(x1, y1, x2, y2), ...)}
//' @param xlim A numeric vector representing the limits on the x axis like
//'   \code{c(xmin, xmax)}
//' @param ylim A numeric vector representing the limits on the y axis like
//'   \code{c(ymin, ymax)}
//' @param force Magnitude of the force (defaults to \code{1e-6})
//' @param maxiter Maximum number of iterations to try to resolve overlaps
//'   (defaults to 2000)
//' @noRd
// [[Rcpp::export]]
DataFrame repel_boxes(
    NumericMatrix data_points,
    double pad_point_x, double pad_point_y,
    NumericMatrix boxes,
    NumericVector xlim, NumericVector ylim,
    double force = 1e-6, int maxiter = 2000
) {
  int n = boxes.nrow();
  int iter = 0;
  bool any_overlaps = true;

  if (NumericVector::is_na(force)) {
    force = 1e-6;
  }

  Point xbounds, ybounds;
  xbounds.x = xlim[0];
  xbounds.y = xlim[1];
  ybounds.x = ylim[0];
  ybounds.y = ylim[1];

  std::vector<Box> Boxes(n);
  std::vector<Box> DataBoxes(n);
  std::vector<double> ratios(n);
  std::vector<Point> original_centroids(n);
  for (int i = 0; i < n; i++) {
    Boxes[i].x1 = boxes(i, 0);
    Boxes[i].y1 = boxes(i, 1);
    Boxes[i].x2 = boxes(i, 2);
    Boxes[i].y2 = boxes(i, 3);
    DataBoxes[i].x1 = data_points(i, 0) - pad_point_x;
    DataBoxes[i].y1 = data_points(i, 1) - pad_point_y;
    DataBoxes[i].x2 = data_points(i, 0) + pad_point_x;
    DataBoxes[i].y2 = data_points(i, 1) + pad_point_y;
    // height over width
    ratios[i] = (Boxes[i].y2 - Boxes[i].y1) / (Boxes[i].x2 - Boxes[i].x1);
    original_centroids[i] = centroid(Boxes[i]);
  }

  std::vector<Point> Points(n);
  for (int i = 0; i < n; i++) {
    Points[i].x = data_points(i, 0);
    Points[i].y = data_points(i, 1);
  }

  double total_force = 0;
  Point f, ci, cj;

  while (any_overlaps && iter < maxiter) {
    iter += 1;
    any_overlaps = false;

    for (int i = 0; i < n; i++) {
      f.x = 0;
      f.y = 0;

      ci = centroid(Boxes[i]);

      for (int j = 0; j < n; j++) {

        cj = centroid(Boxes[j]);

        if (i == j) {
          // Repel the box from its data point.
          // if (point_within_box(Points[i], Boxes[i])) {
          if (overlaps(DataBoxes[i], Boxes[i])) {
            any_overlaps = true;
            f = f + repel_force(ci, Points[i], force);
          }
        } else {
          // Repel the box from overlapping boxes.
          if (overlaps(Boxes[i], Boxes[j])) {
            any_overlaps = true;
            f = f + repel_force(ci, cj, force * 2);
          }
          // Repel the box from other data points.
          // if (point_within_box(Points[j], Boxes[i])) {
          if (overlaps(DataBoxes[j], Boxes[i])) {
            any_overlaps = true;
            f = f + repel_force(ci, Points[j], force);
          }
        }
      }

      // Pull the box toward its original position.
      if (!any_overlaps) {
        f = f + spring_force(original_centroids[i], ci, force * 100);
      }

      // Scale the x force by the ratio of height/width.
      f.x = f.x * ratios[i];

      // Dampen the forces.
      f = f * (1 - 1e-3);

      total_force += fabs(f.x) + fabs(f.y);

      Boxes[i] = Boxes[i] + f;
      Boxes[i] = put_within_bounds(Boxes[i], xbounds, ybounds);
    }

    // If there are no forces, let's break the loop.
    if (total_force == 0) {
      break;
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
    xs[i] = (Boxes[i].x1 + Boxes[i].x2) / 2;
    ys[i] = (Boxes[i].y1 + Boxes[i].y2) / 2;
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("x") = xs,
    Rcpp::Named("y") = ys
  );
}

