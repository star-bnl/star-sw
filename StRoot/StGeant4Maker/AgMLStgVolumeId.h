#ifndef __AgMLStgVolumeId_h__
#define __AgMLStgVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StMessMgr.h>

class AgMLStgVolumeId : public AgMLVolumeId {

  static const int version = 2;

public:
  
  AgMLStgVolumeId(){}

  virtual int id( int* numbv ) const { 

    int station  = (numbv[0]-1) / 4 + 1;
    int chamber  = (numbv[0]-1) % 4 + 1;

    if ( version == 2 )
      return 10*station + chamber;
    else
      return numbv[0];

  };
};


#endif
