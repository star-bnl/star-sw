#ifndef __AgMLFstVolumeId_h__
#define __AgMLFstVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StMessMgr.h>

/**
 * @class AgMLFstVolumeId
 * @brief A volume identifier for the Forward Silicon Tracker (FST).
 *
 * This class provides a unique integer identifier for each sensitive sensor
 * in the FST, based on its disk, wedge, and sensor number. It includes
 * a mapping to handle the specific sensor layout.
 */
class AgMLFstVolumeId : public AgMLVolumeId {
public:
  
  AgMLFstVolumeId(){}

  virtual int id( int* numbv ) const { 

    static const int mapping[] = { 2, 3, 1 };

    int disk   = numbv[0];
    int wedge  = numbv[1];
    assert( numbv[2]>0 && numbv[2]<=3 );
    int sensor = mapping[ numbv[2]-1 ];


    return 1000*disk + 10*wedge + sensor;

  };
};


#endif
