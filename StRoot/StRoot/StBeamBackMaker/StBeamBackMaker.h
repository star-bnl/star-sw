/** -*- C++ -*-
 *
 * @author Pibero Djawotho <pibero@indiana.edu>
 * Indiana University
 * November 17, 2005
 */

#ifndef StBeamBackMaker_h
#define StBeamBackMaker_h

/**
 * @brief STAR includes
 */
#include "StMaker.h"

/**
 * @brief ROOT forward declarations
 */
class TH1;
class TH2;

/**
 * @brief STAR forward declarations
 */
class StTrack;

/**
 * @brief Local forward declarations
 */
class Track;

/**
 * @brief Beam background tracker in the TPC
 *
 * The beam background tracker in the TPC is a maker that is meant to run
 * in bfc.C (big full chain) after TPC hits have been created and stored in
 * the TPC hit collection of the StEvent structure and after ITTF, the
 * STAR tracking software, has run. The beam background tracker then
 * scavenges leftover TPC Hits to look for beam background tracks
 * parallel to the z-axis that presumably originate upstream of either
 * the blue or yellow beam scrapping against focussing/steering magnet
 * elements in the beamline and spraying particles that shower into the
 * IR (intersection region). Detectors at forward rapidity, such as the
 * Endcap EMC, are most affected by this type of beam background events
 * and this has been confirmed by several physics analyses.
 *
 * The tracker algorithm is:
 *
 * 1. Collect all leftover TPC hits into a C++ multiset with their
 *    z-coordinate used as a key. This ensures the hits are sorted
 *    in ascending z-ccordinate and the insert and delete times are
 *    O(logN).
 * 2. Look for track seeds. Start a track seed with the hit with the
 *    smallest z-coordinate. Keep adding hits to the track seed so long
 *    as the radial distance in xy between the the centroid and the hit
 *    is less than MAX_R_DISTANCE (= 5 cm).
 *
 * 3. Weed out track seeds with less than MIN_TRACK_SEED_HITS (= 100) hits.
 *
 * 4. Perform linear fits in the zx and zy-planes. Save only tracks that
 *    have absolute slopes in both planes less than MAX_SLOPE (= 0.1).
 *    At the same time, add to the track available hits that are within a
 *    radial distance of 5 cm of the fitted line.
 *
 * 5. Merge linear tracks if both end points of the first track
 *    are within 5 cm of the centroid of the track in the xy-plane.
 *
 * 6. Refit the track and remove hits that are not within a radius of 5 cm
 *    in the xy-plane of the refitted line. Pileup tracks are discarded.
 *    A non-pileup track has at least 4 hits in each of the following TPC
 *    zones along the beamline:
 *
 *    a. -200 <= z < -150 cm (far  east)
 *    b.  -50 <= z <    0 cm (near east)
 *    c.    0 <= z <   50 cm (near west)
 *    d.  150 <= z <  200 cm (far  west)
 *
 * 7. Convert all tracks to StTrack for storage into StEvent:
 *
 *    a. The track key is the highest key from existing tracks in StEvent
 *       incremented by 1.
 *    b. The track flag is set to 901 (beam background track in TPC).
 *    c. The track encoded method is kLine3D.
 *    d. The track origin is the point (x-intercept, y-intercept, 0)
 *    e. The track momentum is set to (dx/dz, dy/dz, 1), then the
 *       magnitude is adjusted to 999 GeV/c. This is a simple measure
 *       to prevent unsuspecting folks from accidentally using these
 *       beam background tracks as genuine charged particles from collisions
 *       in the STAR detector in their physics analysis.
 *    f. The first point, last point, hits, number of hits is saved in
 *       StTrackDetectorInfo and StTrackFitTraits.
 *
 * 8. In StEvent, trackNodes() and trackDetectorInfo() are updated.
 *      
 */
class StBeamBackMaker : public StMaker {
public:
  /**
   * @brief Constructor
   * @param Name of the instance of StBeamBackMaker
   */
  StBeamBackMaker(const char* name = "StBeamBackMaker") : StMaker(name) {}

  /**
   * @brief Run the tracker on the current event
   */
  Int_t Make();

private:
  /**
   * @brief Check for pileup
   * @param Track to check for pileup
   * @return true if pileup, false otherwise
   */
  bool pileup(Track* track) const;

  /**
   * @brief Convertion from Track to StTrack
   * @param Track to convert
   * @return Converted StTrack
   */
  StTrack* createStTrack(Track* track);

  /**
   * @brief StMessMgr helper functions
   */
  ostream& info(const Char_t* message = 0);
  ostream& warning(const Char_t* message = 0);

  ClassDef(StBeamBackMaker, 1)
};

#endif
