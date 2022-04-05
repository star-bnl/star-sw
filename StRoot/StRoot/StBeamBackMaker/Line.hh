/**
 * @author Pibero Djawotho <pibero@indiana.edu>
 * Indiana University
 * 23 September 2005
 */

#ifndef Line_hh
#define Line_hh
/**
 * @brief C++ STL includes
 */
#include <utility>
using std::pair;

/**
 * @brief STAR includes
 */
#include "StThreeVectorD.hh"

/**
 * @brief 3D lines
 */
class Line {
public:
  Line() {}
  Line(const StThreeVectorD& o, const StThreeVectorD& d);

  /**
   * @brief Origin of the line
   */
  StThreeVectorD origin() const;

  /**
   * @brief Direction vector of the line (normalized to the unit vector)
   */
  StThreeVectorD direction() const;

  /**
   * @brief Point at a given path length along the line
   * @param Pathlength
   * @return Point at pathlength
   */
  StThreeVectorD at(double pathlength) const;

  /**
   * @brief Perigee, i.e. Closest point on the line to a given point
   * @param Any point
   * @return Closest point on the line
   */
  StThreeVectorD perigee(const StThreeVectorD& point) const;

  /**
   * @brief DCA = Distance of Closest Approach to a given line
   * @param Any line
   * @return 3D-vector of the closest distance between the two lines
   */
  StThreeVectorD dca(const Line& line) const;

  /**
   * @brief Pathlength of a given point from the origin of the line
   * @param Any point
   * @return Pathlength from the origin
   */
  double pathlength(const StThreeVectorD& point) const;

  /**
   * @brief Pathlengths of the points of closest approach between the lines.
   * @param Any line
   * @return Pair of double. The first value is the pathlength at this line
   *         of the point of closest approach to that line. The second
   *         value is the pathlength on that lineof the point of closest to
   *         this line.
   */
  pair<double, double> pathlengths(const Line& line) const;

  /**
   * @brief Set the origin of the line to a given point
   * @param Any point
   */
  void setOrigin(const StThreeVectorD& o);

  /**
   * @brief Set the direction of the line to a given vector.
   *        The direction is normalized to the unit vector.
   * @param Any vector
   */
  void setDirection(const StThreeVectorD& d);

private:
  StThreeVectorD mOrigin;
  StThreeVectorD mDirection;
};

inline StThreeVectorD Line::origin() const { return mOrigin; }
inline StThreeVectorD Line::direction() const { return mDirection; }
inline void Line::setOrigin(const StThreeVectorD& o) { mOrigin = o; }
inline void Line::setDirection(const StThreeVectorD& d) { mDirection = d.unit(); }

#endif
