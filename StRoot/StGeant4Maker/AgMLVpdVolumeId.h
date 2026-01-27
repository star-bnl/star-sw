#ifndef __AgMLVpdVolumeId_h__
#define __AgMLVpdVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StarVMC/StarGeometry/StarGeo.h>
#include <StMessMgr.h>
#include <string>

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
    version = 1;
    if      ( StarGeometry::HasDetector( "VPDD07" ) ) version=7;
    else if ( StarGeometry::HasDetector( "VPDD08" ) ) version=7;
    else if ( StarGeometry::HasDetector( "VPDD02" ) ) version=2;
    else if ( StarGeometry::HasDetector( "VPDD03" ) ) version=3;
    else if ( StarGeometry::HasDetector( "VPDD04" ) ) version=4;
    LOG_INFO << "VPD Volume ID: version=" << version << endm;
      ;
  }

  bool version;

    
; // version is hard coded to the dev2021 geometry
  
  virtual int id( int* numbv ) const { 

    
    int rileft = numbv[0];
    int inout  = 0; // n.b. earlier R&D version appears to have had inner vs outer ring
    int sector = numbv[1];

    int _id = 1000 * rileft + 100*inout + sector;

#if 0
    LOG_INFO << "VPD volume ID : "
	     << std::to_string(numbv[0]) << " " 
	     << std::to_string(numbv[1]) << " "  
	     << std::to_string(numbv[2]) << " " << " | " 
	     << std::to_string(numbv[3]) << " " << " | " 
	     << std::to_string(rileft) << " " 
	     << std::to_string(inout) << " "
	     << std::to_string(sector) << " | " 
	     << _id 
	     << endm;
#endif
    


    return _id;

  };
};


#endif

