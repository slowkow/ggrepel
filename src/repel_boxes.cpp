#include <Rcpp.h>
#include <deque>
using namespace Rcpp;

// Exported convenience functions ---------------------------------------------

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

//' Get the coordinates of the center of a box.
//' @param b A box like \code{c(x1, y1, x2, y2)}
//' @noRd
// [[Rcpp::export]]
NumericVector centroid(NumericVector b) {
  return NumericVector::create((b[0] + b[2]) / 2, (b[1] + b[3]) / 2);
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
  // Sorry for the ugly code :(
  double dy = p2[1] - p1[1];
  double dx = p2[0] - p1[0];

  double slope     = dy / dx;
  double intercept = p2[1] - p2[0] * slope;

  NumericMatrix retval(4, 2);
  std::fill(retval.begin(), retval.end(), -INFINITY);

  double x, y;

  //   +----------+ < b[3]
  //   |          |
  //   |          | < y
  //   |          |
  //   +----------+ < b[1]
  //   ^    ^     ^
  //  b[0]  x    b[2]

  if (dx != 0) {
    // Left boundary
    x = b[0];
    y = dy == 0 ? p1[1] : slope * x + intercept;
    if (b[1] <= y && y <= b[3]) {
      retval(0, _) = NumericVector::create(x, y);
    }

    // Right boundary
    x = b[2];
    y = dy == 0 ? p1[1] : slope * x + intercept;
    if (b[1] <= y && y <= b[3]) {
      retval(1, _) = NumericVector::create(x, y);
    }
  }

  if (dy != 0) {
      // Bottom boundary
      y = b[1];
      x = dx == 0 ? p1[0] : (y - intercept) / slope;
      if (b[0] <= x && x <= b[2]) {
        retval(2, _) = NumericVector::create(x, y);
      }

      // Top boundary
      y = b[3];
      x = dx == 0 ? p1[0] : (y - intercept) / slope;
      if (b[0] <= x && x <= b[2]) {
        retval(3, _) = NumericVector::create(x, y);
      }
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

// Main code for text label placement -----------------------------------------

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
//' @param a A point.
//' @param b A point.
//' @return The distance between two points.
//' @noRd
double euclid(Point a, Point b) {
  Point dist = a - b;
  return sqrt(dist.x * dist.x + dist.y * dist.y);
}

//' Squared Euclidean distance between two points.
//' @param a A point.
//' @param b A point.
//' @return The distance between two points.
//' @noRd
double euclid2(Point a, Point b) {
  Point dist = a - b;
  return dist.x * dist.x + dist.y * dist.y;
}

//' Move a box into the area specificied by x limits and y limits.
//' @param b A box like \code{c(x1, y1, x2, y2)}
//' @param xlim A Point with limits on the x axis like \code{c(xmin, xmax)}
//' @param ylim A Point with limits on the y axis like \code{c(xmin, xmax)}
//' @param force Magnitude of the force (defaults to \code{1e-6})
//' @noRd
Box put_within_bounds(Box b, Point xlim, Point ylim, double force = 1e-5) {
  //double d;
  //if (b.x1 < xlim.x) {
  //  d = std::max(fabs(b.x1 - xlim.x), 0.02);
  //  b.x1 += force / pow(d, 2);
  //  b.x2 += force / pow(d, 2);
  //} else if (b.x2 > xlim.y) {
  //  d = std::max(fabs(b.x2 - xlim.y), 0.02);
  //  b.x1 -= force / pow(d, 2);
  //  b.x2 -= force / pow(d, 2);
  //}
  //if (b.y1 < ylim.x) {
  //  d = std::max(fabs(b.y1 - ylim.x), 0.02);
  //  b.y1 += force / pow(d, 2);
  //  b.y2 += force / pow(d, 2);
  //} else if (b.y2 > ylim.y) {
  //  d = std::max(fabs(b.y2 - ylim.y), 0.02);
  //  b.y1 -= force / pow(d, 2);
  //  b.y2 -= force / pow(d, 2);
  //}
  double width = fabs(b.x1 - b.x2);
  double height = fabs(b.y1 - b.y2);
  if (b.x1 < xlim.x) {
    b.x1 = xlim.x;
    b.x2 = b.x1 + width;
  } else if (b.x2 > xlim.y) {
    b.x2 = xlim.y;
    b.x1 = b.x2 - width;
  }
  if (b.y1 < ylim.x) {
    b.y1 = ylim.x;
    b.y2 = b.y1 + height;
  } else if (b.y2 > ylim.y) {
    b.y2 = ylim.y;
    b.y1 = b.y2 - height;
  }
  return b;
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



Point repel_force_both(
    Point a, Point b, double force = 0.000001
) {
  double dx = fabs(a.x - b.x);
  double dy = fabs(a.y - b.y);
  // Constrain the minimum distance, so it is never 0.
  double d2 = std::max(dx * dx + dy * dy, 0.0004);
  // Compute a unit vector in the direction of the force.
  Point v = (a - b) / sqrt(d2);
  // Divide the force by the squared distance.
  Point f = force * v / d2;
  if (dx > dy) {
    // f.y = f.y * dx / dy;
    f.y = f.y * 2;
  } else {
    // f.x = f.x * dy / dx;
    f.x = f.x * 2;
  }
  return f;
}


Point repel_force_y(
    Point a, Point b, double force = 0.000001
) {
  double dx = fabs(a.x - b.x);
  double dy = fabs(a.y - b.y);
  // Constrain the minimum distance, so it is never 0.
  double d2 = std::max(dx * dx + dy * dy, 0.0004);
  // Compute a unit vector in the direction of the force.
  Point v = {0, (a.y - b.y) / sqrt(d2)};
  // Divide the force by the squared distance.
  Point f = force * v / d2 * 2;
  return f;
}

Point repel_force_x(
    Point a, Point b, double force = 0.000001
) {
  double dx = fabs(a.x - b.x);
  double dy = fabs(a.y - b.y);
  // Constrain the minimum distance, so it is never 0.
  double d2 = std::max(dx * dx + dy * dy, 0.0004);
  // Compute a unit vector in the direction of the force.
  Point v = {(a.x - b.x) / sqrt(d2), 0};
  // Divide the force by the squared distance.
  Point f = force * v / d2 * 2;
  return f;
}

//' Compute the repulsion force upon point \code{a} from point \code{b}.
//'
//' The force decays with the squared distance between the points, similar
//' to the force of repulsion between magnets.
//'
//' @param a A point like \code{c(x, y)}
//' @param b A point like \code{c(x, y)}
//' @param force Magnitude of the force (defaults to \code{1e-6})
//' @param direction direction in which to exert force, either "both", "x", or "y"
//' @noRd
Point repel_force(
    Point a, Point b, double force = 0.000001, std::string direction = "both"
) {
  Point out;
  if (direction == "x"){
    out = repel_force_x(a, b, force);
  } else if (direction == "y"){
    out = repel_force_y(a, b, force);
  } else{
    out = repel_force_both(a, b, force);
  }
  return out;
}



Point spring_force_both(
    Point a, Point b, double force = 0.000001
) {
  double dx = fabs(a.x - b.x);
  double dy = fabs(a.y - b.y);
  double d = sqrt(dx * dx + dy * dy);
  Point f = {0, 0};
  if (d > 0.02) {
    // Compute a unit vector in the direction of the force.
    Point v = (a - b) / d;
    f = force * v * d;
    if (dx < dy) {
      f.y = f.y * 1.5;
      f.x = f.x * 0.5;
    } else {
      f.y = f.y * 0.5;
      f.x = f.x * 1.5;
    }
  }
  return f;
}

Point spring_force_y(
    Point a, Point b, double force = 0.000001
) {
  double dx = fabs(a.x - b.x);
  double dy = fabs(a.y - b.y);
  double d = sqrt(dx * dx + dy * dy);
  Point f = {0, 0};
  if (d > 0.02) {
    // Compute a unit vector in the direction of the force.
    Point v = {0, (a.y - b.y) / d};
    f = force * v * d;
    f.y = f.y * 1.5;
  }
  return f;
}

Point spring_force_x(
    Point a, Point b, double force = 0.000001
) {
  double dx = fabs(a.x - b.x);
  double dy = fabs(a.y - b.y);
  double d = sqrt(dx * dx + dy * dy);
  Point f = {0, 0};
  if (d > 0.02) {
    // Compute a unit vector in the direction of the force.
    Point v = {(a.x - b.x) / d, 0};
    f = force * v * d;
    f.x = f.x * 1.5;
  }
  return f;
}

//' Compute the spring force upon point \code{a} from point \code{b}.
//'
//' The force increases with the distance between the points, similar
//' to Hooke's law for springs.
//'
//' @param a A point like \code{c(x, y)}
//' @param b A point like \code{c(x, y)}
//' @param force Magnitude of the force (defaults to \code{1e-6})
//' @param direction direction in which to exert force, either "both", "x", or "y"
//' @noRd
Point spring_force(
    Point a, Point b, double force = 0.000001, std::string direction = "both"
) {
  Point out;
  if (direction == "x"){
    out = spring_force_x(a, b, force);
  } else if (direction == "y"){
    out = spring_force_y(a, b, force);
  } else{
    out = spring_force_both(a, b, force);
  }
  return out;
}

//' Adjust the layout of a list of potentially overlapping boxes.
//' @param data_points A numeric matrix with rows representing points like
//'   \code{rbind(c(x, y), c(x, y), ...)}
//' @param point_padding_x Padding around each data point on the x axis.
//' @param point_padding_y Padding around each data point on the y axis.
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
    double point_padding_x, double point_padding_y,
    NumericMatrix boxes,
    NumericVector xlim, NumericVector ylim,
    double force = 1e-6, int maxiter = 2000,
    std::string direction = "both"
) {
  int n_points = data_points.nrow();
  int n_texts = boxes.nrow();
  // assert(n_points >= n_texts);
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

  // Each data point gets a bounding box.
  std::vector<Box> DataBoxes(n_points);
  for (int i = 0; i < n_points; i++) {
    DataBoxes[i].x1 = data_points(i, 0) - point_padding_x;
    DataBoxes[i].y1 = data_points(i, 1) - point_padding_y;
    DataBoxes[i].x2 = data_points(i, 0) + point_padding_x;
    DataBoxes[i].y2 = data_points(i, 1) + point_padding_y;
  }

  std::vector<Point> Points(n_points);
  for (int i = 0; i < n_points; i++) {
    Points[i].x = data_points(i, 0);
    Points[i].y = data_points(i, 1);
  }

  // Add a tiny bit of jitter to each text box at the start.
  NumericVector r = rnorm(n_texts, 0, force);
  std::vector<Box> TextBoxes(n_texts);
  std::vector<double> ratios(n_texts);
  std::vector<Point> original_centroids(n_texts);
  for (int i = 0; i < n_texts; i++) {
    TextBoxes[i].x1 = boxes(i, 0);
    TextBoxes[i].x2 = boxes(i, 2);
    TextBoxes[i].y1 = boxes(i, 1);
    TextBoxes[i].y2 = boxes(i, 3);
    // Don't add jitter if the user wants to repel in just one direction.
    if (direction != "y") {
      TextBoxes[i].x1 += r[i];
      TextBoxes[i].x2 += r[i];
    }
    if (direction != "x") {
      TextBoxes[i].y1 += r[i];
      TextBoxes[i].y2 += r[i];
    }
    // height over width
    ratios[i] = (TextBoxes[i].y2 - TextBoxes[i].y1)
      / (TextBoxes[i].x2 - TextBoxes[i].x1);
    original_centroids[i] = centroid(TextBoxes[i]);
  }

  Point f, ci, cj;

  while (any_overlaps && iter < maxiter) {
    iter += 1;
    any_overlaps = false;

    for (int i = 0; i < n_texts; i++) {
      f.x = 0;
      f.y = 0;

      ci = centroid(TextBoxes[i]);

      for (int j = 0; j < n_points; j++) {

        if (i == j) {
          // Skip the data points if the padding is 0.
          if (point_padding_x == 0 && point_padding_y == 0) {
            continue;
          }
          // Repel the box from its data point.
          if (overlaps(DataBoxes[i], TextBoxes[i])) {
            any_overlaps = true;
            f = f + repel_force(ci, Points[i], force, direction);
          }
        } else {
          // Repel the box from overlapping boxes.
          if (j < n_texts && overlaps(TextBoxes[i], TextBoxes[j])) {
            any_overlaps = true;
            cj = centroid(TextBoxes[j]);
            f = f + repel_force(ci, cj, force * 3, direction);
          }
          // Skip the data points if the padding is 0.
          if (point_padding_x == 0 && point_padding_y == 0) {
            continue;
          }
          // Repel the box from other data points.
          if (overlaps(DataBoxes[j], TextBoxes[i])) {
            any_overlaps = true;
            f = f + repel_force(ci, Points[j], force, direction);
          }
        }
      }

      // Pull the box toward its original position.
      if (!any_overlaps) {
        f = f + spring_force(original_centroids[i], ci, force * 2e3, direction);
      }

      // Dampen the forces.
      f = f * (1 - 1e-3);

      TextBoxes[i] = TextBoxes[i] + f;
      TextBoxes[i] = put_within_bounds(TextBoxes[i], xbounds, ybounds);
    }
  }

  NumericVector xs(n_texts);
  NumericVector ys(n_texts);

  for (int i = 0; i < n_texts; i++) {
    xs[i] = (TextBoxes[i].x1 + TextBoxes[i].x2) / 2;
    ys[i] = (TextBoxes[i].y1 + TextBoxes[i].y2) / 2;
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("x") = xs,
    Rcpp::Named("y") = ys
  );
}


