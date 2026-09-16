#ifndef __AgMLSsdVolumeId_h__
#define __AgMLSsdVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StarVMC/StarGeometry/StarGeo.h>
#include <StMessMgr.h>


/**
 * @class AgMLSsdVolumeId
 * @brief A volume identifier for the silicon strip detector
 *
 * This class calculates a unique integer identifier for each sensitive
 * element in the SSD based on its hierarchical position in the geometry.
*/


class AgMLSsdVolumeId : public AgMLVolumeId {
public:

  
  AgMLSsdVolumeId(){};
  
  virtual int id( int* numbv ) const { 
    
    int _id;
    int ladder=0, sensor=0, unknown=0;

    #define sisd85a  StarGeometry::HasDetector( "SISD85a" )

    if ( sisd85a ) {

      ladder = numbv[0] - 1;
      sensor = ladder % 16  + 1;
      ladder = ladder / 16   + 1;

    }
    else {
      LOG_WARN << "Undefined ssd ID" << endm;
    }

    _id = 7000 + 100*sensor + ladder;

    return _id;

  };
};


#endif
