#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
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
//' @param c A circle like \code{c(x, y, r)}
//' @param r A rectangle like \code{c(x1, y1, x2, y2)}
//' @noRd
// [[Rcpp::export]]
bool intersect_circle_rectangle(NumericVector c, NumericVector r) {
  // Center of the circle.
  double c_x = c[0];
  double c_radius = c[2];
  // Center of the rectangle.
  double r_x = (r[2] + r[0]) / 2;
  double r_halfwidth = std::abs(r[0] - r_x);
  // Distance between centers.
  double cx = std::abs(c_x - r_x);
  double xDist = r_halfwidth + c_radius;
  if (cx > xDist) {
    return false;
  }
  // Center of the circle.
  double c_y = c[1];
  // Center of the rectangle.
  double r_y = (r[3] + r[1]) / 2;
  double r_halfheight = std::abs(r[1] - r_y);
  // Distance between centers.
  double cy = std::abs(c_y - r_y);
  double yDist = r_halfheight + c_radius;
  if (cy > yDist) {
    return false;
  }
  if (cx <= r_halfwidth || cy <= r_halfheight)
    return true;
  double xCornerDist = cx - r_halfwidth;
  double yCornerDist = cy - r_halfheight;
  double xCornerDistSq = xCornerDist * xCornerDist;
  double yCornerDistSq = yCornerDist * yCornerDist;
  double maxCornerDistSq = c_radius * c_radius;
  return xCornerDistSq + yCornerDistSq <= maxCornerDistSq;
}

//' Find the intersection between a line and a circle.
//' @param p1 A point on the line like \code{c(x, y)}
//' @param p2 A point at the circle's center
//' @param r The circle's radius
//' @noRd
// [[Rcpp::export]]
NumericVector intersect_line_circle(
    NumericVector p1, NumericVector p2, double r
) {
  // 0 = x, 1 = y
  double theta = std::atan2(p1[1] - p2[1], p1[0] - p2[0]);
  return NumericVector::create(
      p2[0] + r * std::cos(theta),
      p2[1] + r * std::sin(theta)
  );
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

typedef struct {
  double x, y, r;
} Circle;

Circle operator +(const Circle& b, const Point& p) {
  Circle c = {b.x + p.x, b.y + p.y, b.r};
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

//' Test if a box overlaps another box.
//' @param a A box like \code{c(x1, y1, x2, y2)}
//' @param b A box like \code{c(x1, y1, x2, y2)}
//' @noRd
bool overlaps(Circle c, Box r) {
  // Center of the circle.
  double c_x = c.x;
  double c_radius = c.r;
  // Center of the rectangle.
  double r_x = (r.x1 + r.x2) / 2;
  double r_halfwidth = std::abs(r.x1 - r_x);
  // Distance between centers.
  double cx = std::abs(c_x - r_x);
  double xDist = r_halfwidth + c_radius;
  if (cx > xDist) {
    return false;
  }
  // Center of the circle.
  double c_y = c.y;
  // Center of the rectangle.
  double r_y = (r.y1 + r.y2) / 2;
  double r_halfheight = std::abs(r.y1 - r_y);
  // Distance between centers.
  double cy = std::abs(c_y - r_y);
  double yDist = r_halfheight + c_radius;
  if (cy > yDist) {
    return false;
  }
  if (cx <= r_halfwidth || cy <= r_halfheight) {
    return true;
  }
  double xCornerDist = cx - r_halfwidth;
  double yCornerDist = cy - r_halfheight;
  double xCornerDistSq = xCornerDist * xCornerDist;
  double yCornerDistSq = yCornerDist * yCornerDist;
  double maxCornerDistSq = c_radius * c_radius;
  return xCornerDistSq + yCornerDistSq <= maxCornerDistSq;
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

std::vector<double> rescale(std::vector<double> v) {
  double min_value = *std::min_element(v.begin(), v.end());
  double max_value = *std::max_element(v.begin(), v.end());
  for (long unsigned int i = 0; i < v.size(); i++) {
    v[i] = (v[i] - min_value) / max_value;
  }
  return v;
}

//' Adjust the layout of a list of potentially overlapping boxes.
//' @param data_points A numeric matrix with rows representing points like
//'   \code{rbind(c(x, y), c(x, y), ...)}
//' @param point_size A numeric vector representing the sizes of data points.
//' @param point_padding_x Padding around each data point on the x axis.
//' @param point_padding_y Padding around each data point on the y axis.
//' @param boxes A numeric matrix with rows representing boxes like
//'   \code{rbind(c(x1, y1, x2, y2), c(x1, y1, x2, y2), ...)}
//' @param xlim A numeric vector representing the limits on the x axis like
//'   \code{c(xmin, xmax)}
//' @param ylim A numeric vector representing the limits on the y axis like
//'   \code{c(ymin, ymax)}
//' @param force Magnitude of the force (defaults to \code{1e-6})
//' @param max_time Maximum number of seconds to try to resolve overlaps
//'   (defaults to 0.1)
//' @param max_iter Maximum number of iterations to try to resolve overlaps
//'   (defaults to 2000)
//' @noRd
// [[Rcpp::export]]
DataFrame repel_boxes2(
    NumericMatrix data_points,
    NumericVector point_size,
    double point_padding_x, double point_padding_y,
    NumericMatrix boxes,
    NumericVector xlim, NumericVector ylim,
    NumericVector hjust, NumericVector vjust,
    double force_push = 1e-7,
    double force_pull = 1e-7,
    double max_time = 0.1,
    double max_overlaps = 10,
    int max_iter = 2000,
    std::string direction = "both",
    int verbose = 0
) {
  int n_points = data_points.nrow();
  int n_texts = boxes.nrow();

  // Larger data points push text away with greater force.
  double force_point_size = 100.0;

  if (NumericVector::is_na(force_push)) {
    force_push = 1e-6;
  }
  if (NumericVector::is_na(force_pull)) {
    force_pull = 1e-6;
  }

  // Try to catch errors.
  if (n_texts > n_points) {
    Rcerr << "n_texts is " << n_texts << std::endl;
    Rcerr << "n_points is " << n_points << std::endl;
    stop("n_texts > n_points");
  }

  if (point_size.length() != n_points) {
    Rcerr << "point_size.length() is " << point_size.length() << std::endl;
    Rcerr << "n_points is " << n_points << std::endl;
    stop("point_size.length() != n_points");
  }

  if (hjust.length() < n_texts) {
    Rcerr << "hjust.length() is " << hjust.length() << std::endl;
    Rcerr << "n_texts is " << n_texts << std::endl;
    stop("hjust.length() < n_texts");
  }
  if (vjust.length() < n_texts) {
    Rcerr << "vjust.length() is " << vjust.length() << std::endl;
    Rcerr << "n_texts is " << n_texts << std::endl;
    stop("vjust.length() < n_texts");
  }

  if (xlim.length() != 2) {
    Rcerr << "xlim.length() is " << xlim.length() << std::endl;
    stop("xlim.length() != 2");
  }
  if (ylim.length() != 2) {
    Rcerr << "ylim.length() is " << ylim.length() << std::endl;
    stop("ylim.length() != 2");
  }

  Point xbounds, ybounds;
  xbounds.x = xlim[0];
  xbounds.y = xlim[1];
  ybounds.x = ylim[0];
  ybounds.y = ylim[1];

  // Each data point gets a bounding circle.
  std::vector<Point> Points(n_points);
  std::vector<Circle> DataCircles(n_points);
  for (int i = 0; i < n_points; i++) {
    DataCircles[i].x = data_points(i, 0);
    DataCircles[i].y = data_points(i, 1);
    DataCircles[i].r = point_size[i] + (point_padding_x + point_padding_y) / 4.0;
    Points[i].x = data_points(i, 0);
    Points[i].y = data_points(i, 1);
  }

  // Add a tiny bit of jitter to each text box at the start.
  NumericVector r = rnorm(n_texts, 0, force_push);
  std::vector<Box> TextBoxes(n_texts);
  std::vector<Point> original_centroids(n_texts);
  std::vector<double> TextBoxWidths(n_texts, 0);
  for (int i = 0; i < n_texts; i++) {
    TextBoxes[i].x1 = boxes(i, 0);
    TextBoxes[i].x2 = boxes(i, 2);
    TextBoxes[i].y1 = boxes(i, 1);
    TextBoxes[i].y2 = boxes(i, 3);
    TextBoxWidths[i] = std::abs(TextBoxes[i].x2 - TextBoxes[i].x1);
    // Don't add jitter if the user wants to repel in just one direction.
    if (direction != "y") {
      TextBoxes[i].x1 += r[i];
      TextBoxes[i].x2 += r[i];
    }
    if (direction != "x") {
      TextBoxes[i].y1 += r[i];
      TextBoxes[i].y2 += r[i];
    }
    original_centroids[i] = centroid(TextBoxes[i], hjust[i], vjust[i]);
  }

  // Rescale to be in the range [0,1]
  TextBoxWidths = rescale(TextBoxWidths);
  // for (int i = 0; i < n_texts; i++) {
  //   Rcout << "[" << i << "] = " << TextBoxWidths[i] << "; ";
  // }
  // Rcout << std::endl;

  // Initialize velocities to zero
  std::vector<Point> velocities(n_texts);
  for (int i = 0; i < n_texts; i++) {
    Point v = {0,0};
    velocities[i] = v;
  }
  double velocity_decay = 0.7;

  Point f, ci, cj;

  //Timer timer;
  //timer.step("start");

  nanotime_t start_time = get_nanotime();
  nanotime_t elapsed_time = 0;
  // convert to nanoseconds
  max_time *= 1e9;

  std::vector<double> total_overlaps(n_texts, 0);
  std::vector<bool> too_many_overlaps(n_texts, false);

  int iter = 0;
  int n_overlaps = 1;
  int p_overlaps = 1;
  bool i_overlaps = true;

  while (n_overlaps && iter < max_iter) {
    iter += 1;
    p_overlaps = n_overlaps;
    n_overlaps = 0;

    // Maximum time limit.
    if (iter % 10 == 0) {
      elapsed_time = get_nanotime() - start_time;
      // Stop trying to layout the text after some time.
      if (elapsed_time > max_time) {
        break;
      }
    }

    // The forces get weaker over time.
    force_push *= 0.99999;
    force_pull *= 0.9999;
    // velocity_decay *= 0.999;

    for (int i = 0; i < n_texts; i++) {
      if (iter == 2 && total_overlaps[i] > max_overlaps) {
        too_many_overlaps[i] = true;
      }
      // if (iter == 2) {
      //   // for (int i = 0; i < n_texts; i++) {
      //   Rcout << "total_overlaps[" << i << "] = " << total_overlaps[i] << "; ";
      //   // }
      //   Rcout << std::endl;
      // }
      if (too_many_overlaps[i]) {
        continue;
      }

      // Reset overlaps for the next iteration
      total_overlaps[i] = 0;

      i_overlaps = false;
      f.x = 0;
      f.y = 0;

      ci = centroid(TextBoxes[i], hjust[i], vjust[i]);

      for (int j = 0; j < n_points; j++) {

        if (i == j) {
          // Skip the data points if the size and padding is 0.
          if (point_size[i] == 0 && point_padding_x == 0 && point_padding_y == 0) {
            continue;
          }
          // Repel the box from its data point.
          if (overlaps(DataCircles[i], TextBoxes[i])) {
            n_overlaps += 1;
            i_overlaps = true;
            total_overlaps[i] += 1;
            f = f + repel_force(
              ci, Points[i],
              // force_push,
              point_size[i] * force_point_size * force_push,
              direction
            );
          }
        } else if (j < n_texts && too_many_overlaps[j]) {
          // Skip the data points if the size and padding is 0.
          if (point_size[j] == 0 && point_padding_x == 0 && point_padding_y == 0) {
            continue;
          }
          // Repel the box from other data points.
          if (overlaps(DataCircles[j], TextBoxes[i])) {
            n_overlaps += 1;
            i_overlaps = true;
            total_overlaps[i] += 1;
            f = f + repel_force(
              ci, Points[j],
              // force_push,
              point_size[i] * force_point_size * force_push,
              direction
            );
          }
        } else {
          if (j < n_texts) {
            cj = centroid(TextBoxes[j], hjust[j], vjust[j]);
            // Repel the box from overlapping boxes.
            if (overlaps(TextBoxes[i], TextBoxes[j])) {
              n_overlaps += 1;
              i_overlaps = true;
              total_overlaps[i] += 1;
              f = f + repel_force(ci, cj, force_push, direction);
            }
          }

          // Skip the data points if the size and padding is 0.
          if (point_size[j] == 0 && point_padding_x == 0 && point_padding_y == 0) {
            continue;
          }
          // Repel the box from other data points.
          if (overlaps(DataCircles[j], TextBoxes[i])) {
            n_overlaps += 1;
            i_overlaps = true;
            total_overlaps[i] += 1;
            f = f + repel_force(
              ci, Points[j],
              // force_push,
              point_size[i] * force_point_size * force_push,
              direction
            );
          }
        }
      }

      // Pull the box toward its original position.
      if (!i_overlaps) {
        // force_pull *= 0.999;
        f = f + spring_force(
            original_centroids[i], ci, force_pull, direction);
      }

      double overlap_multiplier = 1.0;
      if (total_overlaps[i] > 10) {
        overlap_multiplier += 0.5;
      } else {
        overlap_multiplier += 0.05 * total_overlaps[i];
      }

      velocities[i] = overlap_multiplier * velocities[i] * (TextBoxWidths[i] + 1e-6) * velocity_decay + f;
      // velocities[i] = velocities[i] * velocity_decay + f;
      TextBoxes[i] = TextBoxes[i] + velocities[i];
      // Put boxes within bounds
      TextBoxes[i] = put_within_bounds(TextBoxes[i], xbounds, ybounds);

      // look for line clashes
      if (n_overlaps == 0 || iter % 5 == 0) {
        for (int j = 0; j < n_texts; j++) {
          cj = centroid(TextBoxes[j], hjust[j], vjust[j]);
          ci = centroid(TextBoxes[i], hjust[i], vjust[i]);
          // Switch label positions if lines overlap
          if (
              i != j && line_intersect(ci, Points[i], cj, Points[j])
          ) {
            n_overlaps += 1;
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
    } // loop through all text labels
  } // while any overlaps exist and we haven't reached max iterations

  if (verbose) {
    if (elapsed_time > max_time) {
      Rprintf(
        "%.2fs elapsed for %d iterations, %d overlaps. Consider increasing 'max.time'.\n",
        max_time / 1e9, iter, p_overlaps
      );
    } else if (iter >= max_iter) {
      Rprintf(
        "%d iterations in %.2fs, %d overlaps. Consider increasing 'max.iter'.\n",
        max_iter, elapsed_time / 1e9, p_overlaps
      );
    } else {
      Rprintf(
        "text repel complete in %d iterations (%.2fs), %d overlaps\n",
        iter, elapsed_time / 1e9, p_overlaps
      );
    }
  }

  //timer.step("end");
  //NumericVector res(timer);
  //for (int i = 0; i < res.size(); i++) {
  //  res[i] = res[i] / 1000000;
  //}
  //Rcpp::Rcout << round(res[1] - res[0]) << " ms" << std::endl;

  NumericVector xs(n_texts);
  NumericVector ys(n_texts);

  for (int i = 0; i < n_texts; i++) {
    xs[i] = (TextBoxes[i].x1 + TextBoxes[i].x2) / 2;
    ys[i] = (TextBoxes[i].y1 + TextBoxes[i].y2) / 2;
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("x") = xs,
    Rcpp::Named("y") = ys,
    Rcpp::Named("too_many_overlaps") = too_many_overlaps
  );
}
