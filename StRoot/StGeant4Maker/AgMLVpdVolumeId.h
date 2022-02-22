#ifndef __AgMLVpdVolumeId_h__
#define __AgMLVpdVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StMessMgr.h>

class AgMLVpdVolumeId : public AgMLVolumeId {
public:
  
  AgMLVpdVolumeId(){}

  static const int version = 2; // version is hard coded to the dev2021 geometry
  
  virtual int id( int* numbv ) const { 

    int idx = 0;

    int rileft =                numbv[idx++];
    int inout  = (1==version) ? numbv[idx++] : 0;
    int sector =                numbv[idx++];

    int _id = 1000 * rileft + 100*inout + sector;

    return _id;

  };
};


#endif
