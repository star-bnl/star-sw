//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University Cyclotron Facility
// 23 September 2005
//

// Local
#include "StLine.hh"

StLine::StLine(const StThreeVectorD& o, const StThreeVectorD& d)
{
  mOrigin = o;
  mDirection = d.unit();
}

StThreeVectorD StLine::at(double pathlength) const
{
  return mOrigin + pathlength * mDirection;
}

double StLine::pathlength(const StThreeVectorD& point) const
{
  return mDirection.dot(point - mOrigin);
}

StThreeVectorD StLine::perigee(const StThreeVectorD& point) const
{
  return at(pathlength(point));
}

StThreeVectorD StLine::dca(const StLine& line) const
{
  double cosAngle = mDirection.dot(line.direction());
  double delta = cosAngle * cosAngle - 1;
  StThreeVectorD distance = mOrigin - line.origin();
  double pathlength = (distance.dot(mDirection) - cosAngle * distance.dot(line.direction())) / delta;
  double pathlength2 = (cosAngle * distance.dot(mDirection) - distance.dot(line.direction())) / delta;
  return at(pathlength) - line.at(pathlength2);
}

pair<double, double> StLine::pathlengths(const StLine& line) const
{
  double cosAngle = mDirection.dot(line.direction());
  double delta = cosAngle * cosAngle - 1;
  double s = pathlength(line.origin());
  double t = line.pathlength(mOrigin);
  return make_pair(-(s + t * cosAngle) / delta, -(s * cosAngle + t) / delta);
}
