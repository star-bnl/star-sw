#ifndef __AgMLVpdVolumeId_h__
#define __AgMLVpdVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StarVMC/StarGeometry/StarGeo.h>
#include <StMessMgr.h>

/**
 * @class AgMLVpdVolumeId
 * @brief A volume identifier for the Vertex Position Detector (VPD).
 *
 * This class calculates a unique integer identifier for each sensitive
 * element in the VPD. It supports different geometry versions by checking
 * for specific detector tags at construction time. The ID is based on
 * the detector side (east/west), inner/outer ring, and sector number.
 */
class AgMLVpdVolumeId : public AgMLVolumeId {
public:
  
  AgMLVpdVolumeId()
  {
    version = 
      StarGeometry::HasDetector( "VPDD07" ) ||
      StarGeometry::HasDetector( "VPDD08" ) 
      ;
  }

  bool version;

    
; // version is hard coded to the dev2021 geometry
  
  virtual int id( int* numbv ) const { 

    int idx = 0;

    int rileft =                numbv[idx++];
    int inout  = version ? numbv[idx++] : 0;
    int sector =                numbv[idx++];

    int _id = 1000 * rileft + 100*inout + sector;

    return _id;

  };
};


#endif

