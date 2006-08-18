/**
 * @author Pibero Djawotho <pibero@iucf.indiana.edu>
 * Indiana University
 * November 3, 2005
 */

#ifndef TopologyMap_hh
#define TopologyMap_hh

#include <iostream>
using std::ostream;

/**
 * @brief Local forward declarations
 */
class Track;

/**
 * @brief Number of hits in diffent zones of the TPC for a given track.
 * 
 * The class TopologyMap holds the number of hits in 4 different zones
 * of the TPC along the beamline:
 *
 *    1. -200 <= z < -150 (far  east)
 *    2.  -50 <= z <    0 (near east)
 *    3.    0 <= z <   50 (near west)
 *    4.  150 <= z <  200 (far  west)
 */
class TopologyMap {
public:
  /**
   * @brief Constructor creates the topology map
   * @param Track to be mapped out
   */
  TopologyMap(Track* track = 0);

  /**
   * @brief Number of hits in far east of TPC (-200 <= z < -150 cm)
   */
  int  farEast() const;

  /**
  * @brief Number of hits in near east of TPC (-50 <= z < 0 cm)
  */
  int nearEast() const;
  /**
   * @brief Number of hits in near west of TPC (0 <= z < 50 cm)
   */
  int nearWest() const;

  /**
   * @brief Number of hits in far west of TPC (150 <= z < 200 cm)
   */
  int  farWest() const;

private:
  // Number of hits in TPC zones along beamline
  int mFarEast;
  int mNearEast;
  int mNearWest;
  int mFarWest;
};

inline int TopologyMap:: farEast() const { return mFarEast;  }
inline int TopologyMap::nearEast() const { return mNearEast; }
inline int TopologyMap::nearWest() const { return mNearWest; }
inline int TopologyMap:: farWest() const { return mFarWest;  }

ostream& operator<<(ostream& os, const TopologyMap& topoMap);

#endif
