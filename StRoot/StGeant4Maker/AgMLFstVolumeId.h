#ifndef __AgMLFstVolumeId_h__
#define __AgMLFstVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StMessMgr.h>

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
