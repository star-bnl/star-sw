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
    
    int _id;
    if (StarGeometry::HasDetector("BBCMin")){
    // In the old geometry (before the 2018 EPD upgrade), BBC large tiles were included, 
    // so the BBCA volume (Annulus) was placed twice (small and large). 
    // This made it a multi-placed volume, so it was included in the numbv array:
    // numbv[0] = BBCM (West/East: 1 or 2)
    // numbv[1] = BBCA (Annulus Small/Large: 1 or 2)
    // numbv[2] = THXM (Triple Module: 1 to 6)
    // numbv[3] = SHXT (Single Module: 1 to 3)

    // In the new geometry (2018 onwards), BBC large tiles were removed.
    // BBCA is only placed once (small tiles only)
    // The array elements shift left by one:
    // numbv[0] = BBCM (West/East: 1 or 2)
    // numbv[1] = THXM (Triple Module: 1 to 6)
    // numbv[2] = SHXT (Single Module: 1 to 3)
    // numbv[3] = 0 (Empty)
      _id = 1000 * numbv[0] + 100 * 1 + 10 * numbv[1] + numbv[2];
    }
    else {
      _id = 1000 * numbv[0] + 100 * numbv[1] + 10 * numbv[2] + numbv[3];
    }

    return _id;

  };
};


#endif
