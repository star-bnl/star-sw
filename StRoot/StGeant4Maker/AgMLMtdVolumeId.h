#ifndef __AgMLMtdVolumeId_h__
#define __AgMLMtdVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StMessMgr.h>

class AgMLMtdVolumeId : public AgMLVolumeId {
public:
  
  AgMLMtdVolumeId(){}
  // Applies to btog.version = 8 with btog.choice =13 (run 13 onwards)
  
  virtual int id( int* numbv ) const { 

    int sector = numbv[0];
    int module = numbv[1];
    int layer  = numbv[2];

    return 1000 * sector + 100 * module + layer;

  };
};


#endif
