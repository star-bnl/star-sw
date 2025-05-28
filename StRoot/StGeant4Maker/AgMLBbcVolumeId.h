#ifndef __AgMLBbcVolumeId_h__
#define __AgMLBbcVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StarVMC/StarGeometry/StarGeo.h>
#include <StMessMgr.h>

class AgMLBbcVolumeId : public AgMLVolumeId {
public:

  AgMLBbcVolumeId(){}
  // Applies to btog.version = 8 with btog.choice =13 (run 13 onwards)
  
  virtual int id( int* numbv ) const { 

    int _id = 1000 * numbv[0] + 100 * numbv[1] + 10 * numbv[2] + numbv[3];

    return _id;

  };
};


#endif
