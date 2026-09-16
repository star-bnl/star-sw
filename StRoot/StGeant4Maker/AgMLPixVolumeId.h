#ifndef __AgMLPixVolumeId_h__
#define __AgMLPixVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StarVMC/StarGeometry/StarGeo.h>
#include <StMessMgr.h>


/**
 * @class AgMLPixVolumeId
 * @brief A volume identifier for the pixel detector
 *
 * This class calculates a unique integer identifier for each sensitive
 * element in the PIX based on its hierarchical position in the geometry.
*/


class AgMLPixVolumeId : public AgMLVolumeId {
public:

  
  AgMLPixVolumeId(){};
  
  virtual int id( int* numbv ) const { 
    
    int _id;
    int sector=0, ladder=0, sensor=0, unknown=0;

    #define pixl62  StarGeometry::HasDetector( "PIXL62" )
    #define pixl62b StarGeometry::HasDetector( "PIXL62b" )
    #define pixl05  StarGeometry::HasDetector( "PIXL05" )
    #define pixl06  StarGeometry::HasDetector( "PIXL06" )
    #define pixl06b StarGeometry::HasDetector( "PIXL06b" )


    if ( pixl06b || pixl62b ) {

      sensor = numbv[0] - 1;
      sector = sensor / 40 + 1;
      ladder = ( sensor / 10 ) % 4 + 1;
      sensor = ( sensor % 10 ) + 1;

    }
    else {
      LOG_WARN << "Undefined pixl ID" << endm;
    }

    _id = 1000000*sector + 10000*ladder + 100*sensor;

    return _id;

  };
};


#endif
