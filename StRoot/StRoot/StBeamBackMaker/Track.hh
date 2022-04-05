/**
 * @author Pibero Djawotho <pibero@iucf.indiana.edu>
 * Indiana University
 * November 17, 2005
 */

#ifndef Track_hh
#define Track_hh

/**
 * @brief C++ STL includes
 */
#include <vector>

/**
 * @brief ROOT includes
 */
#include "TLinearFitter.h"

/**
 * @brief STAR includes
 */
#include "StHit.h"

/**
 * @brief Beam background track
 *
 * Class to hold information about a beam background track. In particular,
 * a Track contains all the hits associated with it and two linear fitters
 * used to fit the track to lines in the zx- and zy-plane.
 */
class Track : public vector<StHit*> {
private:
  /**
   * @brief Comparison between hits using z-coordinate
   * @param hit1 -- first hit
   * @param hit2 -- second hit
   * @return true if z1 < z2, false otherwise
   */
  struct LessHit {
    bool operator()(const StHit* hit1, const StHit* hit2) const
    {
      return hit1->position().z() < hit2->position().z();
    }
  };

public:
  /**
   * @brief First hit
   */
  StHit* firstHit() const;

  /**
   * @brief Last hit
   */
  StHit* lastHit() const;

  /**
   * @brief Merge this track with that track
   */
  void merge(Track* track);

  /**
   * @brief Perform linear fits in zx- and zy-plane
   * @return true if fit succeeded, false otherwise
   */
  bool fit();

  /**
   * @brief chi square of linear fit in zx-plane
   */
  double chi2zx();

  /**
   * @brief chi square of linear fit in zy-plane
   */
  double chi2zy();

  /**
   * @brief Number of degrees of freedom
   */
  int ndf() const;

  /**
   * @brief Good track?
   * @result true if the absolute value of the slopes in the zx- and zy-plane
   *         are both less than MAX_SLOPE (= 0.1), false otherwise.
   */
  bool ok() const;

  /**
   * @brief Is hit close enough to track?
   * @return true if the radial distance in the xy-plane between the hit
   *         and a point on the line at the same z-location as the hit
   *         is less than 5 cm, false otherwise.
   */
  bool accept(StHit* hit) const;

  /**
   * @brief Track length
   * @return Distance between the point of closest approach to the first hit
   *         and the point of closest approach to the last hit on the track.
   */
  double length() const;

  /**
   * @brief x-intercept at z = 0
   */
  double x0() const;

  /**
   * @brief y-intercept at z = 0
   */
  double y0() const;

  /**
   * @brief dx/dz slope
   */
  double dxdz() const;

  /**
   * @brief dy/dz slope
   */
  double dydz() const;

  /**
   * @brief Error on x-intercept
   */
  double x0error() const;

  /**
   * @brief Error on y-intercept
   */
  double y0error() const;

  /**
   * @brief Error on dx/dz slope
   */
  double dxdzError() const;

  /**
   * @brief Error on dy/dz slope
   */
  double dydzError() const;

private:
  static TLinearFitter mXfitter;
  static TLinearFitter mYfitter;

  double mLength;
  double mChiSquareX;
  double mChiSquareY;
  int    mNumberFreeParameters;
  double mX0;
  double mY0;
  double mdxdz;
  double mdydz;
  double mX0error;
  double mY0error;
  double mdxdzError;
  double mdydzError;
};

inline StHit* Track::firstHit() const { return front(); }
inline StHit* Track::lastHit() const { return back(); }
inline double Track::length() const { return mLength; }
inline double Track::chi2zx() { return mChiSquareX; }
inline double Track::chi2zy() { return mChiSquareY; }
inline int    Track::ndf() const { return mNumberFreeParameters; }
inline double Track::x0() const { return mX0; }
inline double Track::y0() const { return mY0; }
inline double Track::dxdz() const { return mdxdz; }
inline double Track::dydz() const { return mdydz; }
inline double Track::x0error() const { return mX0error; }
inline double Track::y0error() const { return mY0error; }
inline double Track::dxdzError() const { return mdxdzError; }
inline double Track::dydzError() const { return mdydzError; }

ostream& operator<<(ostream&, const Track&);

#endif
