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
#include <set>

/**
 * @brief ROOT includes
 */
#include "TLinearFitter.h"

/**
 * @brief STAR includes
 */
#include "StMemoryPool.hh"
#include "StHit.h"

/**
 * @brief Beam background track
 *
 * Class to hold information about a beam background track. In particular,
 * a Track contains all the hits associated with it and two linear fitters
 * used to fit the track to lines in the zx- and zy-plane.
 */
class Track {
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
  typedef multiset<StHit*, LessHit> HitSet;

public:
  /**
   * @brief Constructor initializes the two linear fitters.
   */
  Track();

  /**
   * @brief Memory management functions
   */
  void* operator new(size_t) { return mPool.alloc(); }
  void  operator delete(void* p) { mPool.free(p); }

  typedef HitSet::iterator iterator;
  typedef HitSet::reverse_iterator reverse_iterator;

  /**
   * @brief Number of hits
   */
  int numberOfHits() const;

  /**
   * @brief Forward iterator to first hit
   */
  iterator begin() const;

  /**
   * @brief Forward iterator past last hit
   */
  iterator end() const;

  /**
   * @brief Reverse iterator to last hit
   */
  reverse_iterator rbegin() const;

  /**
   * @brief Reverse iterator past first hit
   */
  reverse_iterator rend() const;

  /**
   * @brief Add hit to track
   * @param Hit to be added
   */
  void addHit(StHit* hit);

  /**
   * @brief Remove hit from track
   * @param Hit iterator
   */
  void removeHit(iterator i);

  /**
   * @brief Remove hit from track
   * @param Hit pointer
   */
  void removeHit(StHit* hit);

  /**
   * @brief Remove hit subsequence from track
   * @param first -- Iterator to first  hit to be removed
   * @param last  -- Iterator past last hit to be removed
   */
  void removeHits(iterator first, iterator last);

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
  // Memory management data structure
  static StMemoryPool mPool;

  static const double MAX_SLOPE;

  // Pool of available hits
  HitSet mHits;

  // Linear fitters in zx- and zy-plane
  TLinearFitter mXfitter;
  TLinearFitter mYfitter;

  // Track length
  double mLength;
};

inline Track::Track() { mXfitter.SetFormula("pol1"); mYfitter.SetFormula("pol1"); }
inline int Track::numberOfHits() const { return mHits.size(); }
inline Track::iterator Track::begin() const { return mHits.begin(); }
inline Track::iterator Track::end() const { return mHits.end(); }
inline Track::reverse_iterator Track::rbegin() const { return mHits.rbegin(); }
inline Track::reverse_iterator Track::rend() const { return mHits.rend(); }
inline void Track::addHit(StHit* hit) { mHits.insert(hit); }
inline void Track::removeHit(iterator i) { mHits.erase(i); }
inline void Track::removeHits(iterator first, iterator last) { mHits.erase(first, last); }
inline StHit* Track::firstHit() const { return *mHits.begin(); }
inline StHit* Track::lastHit() const { return *mHits.rbegin(); }
inline double Track::length() const { return mLength; }
inline double Track::chi2zx() { return mXfitter.GetChisquare(); }
inline double Track::chi2zy() { return mYfitter.GetChisquare(); }
inline int Track::ndf() const { return mXfitter.GetNumberFreeParameters(); }
inline double Track::x0() const { return mXfitter.GetParameter(0); }
inline double Track::y0() const { return mYfitter.GetParameter(0); }
inline double Track::dxdz() const { return mXfitter.GetParameter(1); }
inline double Track::dydz() const { return mYfitter.GetParameter(1); }
inline double Track::x0error() const { return mXfitter.GetParError(0); }
inline double Track::y0error() const { return mYfitter.GetParError(0); }
inline double Track::dxdzError() const { return mXfitter.GetParError(1); }
inline double Track::dydzError() const { return mYfitter.GetParError(1); }

ostream& operator<<(ostream&, const Track&);

#endif
