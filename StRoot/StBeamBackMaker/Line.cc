//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University Cyclotron Facility
// 23 September 2005
//

// Local
#include "Line.hh"
using namespace std;
Line::Line(const StThreeVectorD& o, const StThreeVectorD& d)
{
  mOrigin = o;
  mDirection = d.unit();
}

StThreeVectorD Line::at(double pathlength) const
{
  return mOrigin + pathlength * mDirection;
}

double Line::pathlength(const StThreeVectorD& point) const
{
  return mDirection.dot(point - mOrigin);
}

StThreeVectorD Line::perigee(const StThreeVectorD& point) const
{
  return at(pathlength(point));
}

StThreeVectorD Line::dca(const Line& line) const
{
  double cosAngle = mDirection.dot(line.direction());
  double delta = cosAngle * cosAngle - 1;
  StThreeVectorD distance = mOrigin - line.origin();
  double pathlength = (distance.dot(mDirection) - cosAngle * distance.dot(line.direction())) / delta;
  double pathlength2 = (cosAngle * distance.dot(mDirection) - distance.dot(line.direction())) / delta;
  return at(pathlength) - line.at(pathlength2);
}

pair<double, double> Line::pathlengths(const Line& line) const
{
  double cosAngle = mDirection.dot(line.direction());
  double delta = cosAngle * cosAngle - 1;
  double s = pathlength(line.origin());
  double t = line.pathlength(mOrigin);
  return make_pair(-(s + t * cosAngle) / delta, -(s * cosAngle + t) / delta);
}
