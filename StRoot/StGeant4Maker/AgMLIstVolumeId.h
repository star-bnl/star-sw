#ifndef __AgMLIstVolumeId_h__
#define __AgMLIstVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StarVMC/StarGeometry/StarGeo.h>
#include <StMessMgr.h>


/**
 * @class AgMLIstVolumeId
 * @brief A volume identifier for the IST
 *
 * This class calculates a unique integer identifier for each sensitive
 * element in the IST based on its hierarchical position in the geometry.
*/


class AgMLIstVolumeId : public AgMLVolumeId {
public:

  
  AgMLIstVolumeId(){};
  
  virtual int id( int* numbv ) const { 
    
    int _id;
    int ladder=0, sensor=0, unknown=0;

#define istd02a  StarGeometry::HasDetector( "ISTD02a" )



    if ( istd02a ) {
      
      sensor = numbv[0] - 1;  //         ! 0 to 143
      ladder = sensor / 6 + 1 + 1;//   ! extra +1 as per legacy below...
      sensor = sensor%6 + 1 ;
    
    }
    else {
      LOG_WARN << "Undefined istl ID" << endm;
    }

    _id = ladder * 1000000  +   
      sensor *   10000  ;


    return _id;

  };
};


#endif
