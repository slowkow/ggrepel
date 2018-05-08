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
NumericVector centroid(NumericVector b, double hjust, double vjust) {
  return NumericVector::create(b[0] + (b[2] - b[0]) * hjust, b[1] + (b[3] - b[1]) * vjust);
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
    if (d < dmin) {
      dmin = d;
      imin = i;
    }
  }

  return retval(imin, _);
}


// [[Rcpp::export]]
NumericVector select_line_connection(
    NumericVector p1, NumericVector b
) {

  NumericVector out(2);

  // Find shortest path
  //   +----------+ < b[3]
  //   |          |
  //   |          |
  //   |          |
  //   +----------+ < b[1]
  //   ^          ^
  //  b[0]      b[2]

  bool top = false;
  bool left = false;
  bool right = false;
  bool bottom = false;

  if ((p1[0] >= b[0]) & (p1[0] <= b[2])) {
    out[0] = p1[0];
  } else if (p1[0] > b[2]) {
    out[0] = b[2];
    right = true;
  } else{
    out[0] = b[0];
    left = true;
  }

  if ((p1[1] >= b[1]) & (p1[1] <= b[3])) {
    out[1] = p1[1];
  } else if (p1[1] > b[3]) {
    out[1] = b[3];
    top = true;
  } else{
    out[1] = b[1];
    bottom = true;
  }

  // Nudge to center
  double midx = (b[0] + b[2]) * 0.5;
  double midy = (b[3] + b[1]) * 0.5;
  double d = std::sqrt(
    std::pow(p1[0] - out[0], 2) +
    std::pow(p1[1] - out[1], 2)
  );


  if ((top || bottom) && !(left || right)) {
    // top or bottom
    double altd = std::sqrt(
      std::pow(p1[0] - midx, 2) +
      std::pow(p1[1] - out[1], 2)
    );
    out[0] = out[0] + (midx - out[0]) * d / altd;
  } else if ((left || right) && !(top || bottom)) {
    // left or right
    double altd = std::sqrt(
      std::pow(p1[0] - out[0], 2) +
      std::pow(p1[1] - midy, 2)
    );
    out[1] = out[1] + (midy - out[1]) * d / altd;
  } else if ((left || right) && (top || bottom)) {
    double altd1 = std::sqrt(
      std::pow(p1[0] - midx, 2) +
      std::pow(p1[1] - out[1], 2)
    );
    double altd2 = std::sqrt(
      std::pow(p1[0] - out[0], 2) +
      std::pow(p1[1] - midy, 2)
    );
    if (altd1 < altd2) {
      out[0] = out[0] + (midx - out[0]) * d / altd1;
    } else {
      out[1] = out[1] + (midy - out[1]) * d / altd2;
    }
  }

  return out;
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

// [[Rcpp::export]]
bool approximately_equal(double x1, double x2) {
  return std::abs(x2 - x1) < (std::numeric_limits<double>::epsilon() * 100);
}


bool line_intersect(Point p1, Point q1, Point p2, Point q2) {

  // Special exception, where q1 and q2 are equal (do intersect)
  if (q1.x == q2.x && q1.y == q2.y)
    return false;
  // If line is point
  if (p1.x == q1.x && p1.y == q1.y)
    return false;
  if (p2.x == q2.x && p2.y == q2.y)
    return false;

  double dy1 = q1.y - p1.y;
  double dx1 = q1.x - p1.x;

  double slope1     = dy1 / dx1;
  double intercept1 = q1.y - q1.x * slope1;

  double dy2 = q2.y - p2.y;
  double dx2 = q2.x - p2.x;

  double slope2     = dy2 / dx2;
  double intercept2 = q2.y - q2.x * slope2;

  double x,y;

  // check if lines vertical
  if (approximately_equal(dx1,0.0)) {
    if (approximately_equal(dx2,0.0)) {
      return false;
    } else {
      x = p1.x;
      y = slope2 * x + intercept2;
    }
  } else if (approximately_equal(dx2,0.0)) {
    x = p2.x;
    y = slope1 * x + intercept1;
  } else {
    if (approximately_equal(slope1,slope2)) {
        return false;
    }
    x = (intercept2 - intercept1) / (slope1 - slope2);
    y = slope1 * x + intercept1;
  }

  if (x < p1.x && x < q1.x) {
    return false;
  } else if (x > p1.x && x > q1.x) {
    return false;
  } else if (y < p1.y && y < q1.y) {
    return false;
  } else if (y > p1.y && y > q1.y) {
    return false;
  } else if (x < p2.x && x < q2.x) {
    return false;
  } else if (x > p2.x && x > q2.x) {
    return false;
  } else if (y < p2.y && y < q2.y) {
    return false;
  } else if (y > p2.y && y > q2.y) {
    return false;
  } else{
    return true;
  }
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
Point centroid(Box b, double hjust, double vjust) {
  Point p = {(b.x1 + (b.x2 - b.x1) * hjust), b.y1 + (b.y2 - b.y1) * vjust};
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
  Point v = {0,1};
  if (a.y < b.y) {
    v.y = -1;
  }
  // Divide the force by the distance.
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
  Point v = {1,0};
  if (a.x < b.x) {
    v.x = -1;
  }
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
  if (direction == "x") {
    out = repel_force_x(a, b, force);
  } else if (direction == "y") {
    out = repel_force_y(a, b, force);
  } else{
    out = repel_force_both(a, b, force);
  }
  return out;
}



Point spring_force_both(
    Point a, Point b, double force = 0.000001
) {
  Point f = {0, 0};
  Point v = (a - b) ;
  f = force * v;
  return f;
}

Point spring_force_y(
    Point a, Point b, double force = 0.000001
) {
  Point f = {0, 0};
  Point v = {0, (a.y - b.y)};
  f = force * v;
  return f;
}

Point spring_force_x(
    Point a, Point b, double force = 0.000001
) {
  Point f = {0, 0};
  Point v = {(a.x - b.x), 0};
  f = force * v ;
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
  if (direction == "x") {
    out = spring_force_x(a, b, force);
  } else if (direction == "y") {
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
    NumericVector hjust, NumericVector vjust,
    double force = 1e-6, int maxiter = 2000,
    std::string direction = "both"
) {
  int n_points = data_points.nrow();
  int n_texts = boxes.nrow();
  // assert(n_points >= n_texts);
  int iter = 0;
  bool any_overlaps = true;
  bool i_overlaps = true;

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
    original_centroids[i] = centroid(TextBoxes[i], hjust[i], vjust[i]);
  }

  Point f, ci, cj;

  while (any_overlaps && iter < maxiter) {
    iter += 1;
    any_overlaps = false;

    for (int i = 0; i < n_texts; i++) {
      i_overlaps = false;
      f.x = 0;
      f.y = 0;

      ci = centroid(TextBoxes[i], hjust[i], vjust[i]);

      for (int j = 0; j < n_points; j++) {

        if (i == j) {
          // Skip the data points if the padding is 0.
          if (point_padding_x == 0 && point_padding_y == 0) {
            continue;
          }
          // Repel the box from its data point.
          if (overlaps(DataBoxes[i], TextBoxes[i])) {
            any_overlaps = true;
            i_overlaps = true;
            f = f + repel_force(ci, Points[i], force, direction);
          }
        } else {
          cj = centroid(TextBoxes[j], hjust[j], vjust[j]);
          // Repel the box from overlapping boxes.
          if (j < n_texts && overlaps(TextBoxes[i], TextBoxes[j])) {
            any_overlaps = true;
            i_overlaps = true;
            f = f + repel_force(ci, cj, force * 3, direction);
          }

          // Skip the data points if the padding is 0.
          if (point_padding_x == 0 && point_padding_y == 0) {
            continue;
          }
          // Repel the box from other data points.
          if (overlaps(DataBoxes[j], TextBoxes[i])) {
            any_overlaps = true;
            i_overlaps = true;
            f = f + repel_force(ci, Points[j], force, direction);
          }
        }
      }

      // Pull the box toward its original position.
      if (!i_overlaps) {
        f = f + spring_force(original_centroids[i], ci, force * 2e3, direction);
      }

      TextBoxes[i] = TextBoxes[i] + f;
      // Put boxes within bounds
      TextBoxes[i] = put_within_bounds(TextBoxes[i], xbounds, ybounds);

      // look for line clashes
      if (!any_overlaps || iter % 5 == 0) {
        for (int j = 0; j < n_points; j++) {
          cj = centroid(TextBoxes[j], hjust[j], vjust[j]);
          ci = centroid(TextBoxes[i], hjust[i], vjust[i]);
          // Switch label positions if lines overlap
          if (
            i != j && j < n_texts &&
            line_intersect(ci, Points[i], cj, Points[j])
          ) {
            any_overlaps = true;
            TextBoxes[i] = TextBoxes[i] + spring_force(cj, ci, 1, direction);
            TextBoxes[j] = TextBoxes[j] + spring_force(ci, cj, 1, direction);
            // Check if resolved
            ci = centroid(TextBoxes[i], hjust[i], vjust[i]);
            cj = centroid(TextBoxes[j], hjust[j], vjust[j]);
            if (line_intersect(ci, Points[i], cj, Points[j])) {
              //Rcout << "unresolved overlap in iter " << iter << std::endl;
              TextBoxes[i] = TextBoxes[i] +
                spring_force(cj, ci, 1.25, direction);
              TextBoxes[j] = TextBoxes[j] +
                spring_force(ci, cj, 1.25, direction);
            }
          }
        }
      }

      // Dampen force after each iteration.
      // force = force * 0.9999;

    } // loop through all text labels
  } // while any overlaps exist and we haven't reached max iterations

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


