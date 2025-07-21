#ifndef __AgMLBbcVolumeId_h__
#define __AgMLBbcVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StarVMC/StarGeometry/StarGeo.h>
#include <StMessMgr.h>

/**
 * @class AgMLBbcVolumeId
 * @brief A volume identifier for the Beam-Beam Counter (BBC).
 *
 * This class calculates a unique integer identifier for each sensitive
 * element in the BBC based on its hierarchical position in the geometry.
 */
class AgMLBbcVolumeId : public AgMLVolumeId {
public:

  AgMLBbcVolumeId(){}
  // Applies to btog.version = 8 with btog.choice =13 (run 13 onwards)
  
  virtual int id( int* numbv ) const { 

    int _id = 1000 * numbv[0] + 100 * numbv[1] + 10 * numbv[2] + numbv[3];

    return _id;

  };
};


#endif
