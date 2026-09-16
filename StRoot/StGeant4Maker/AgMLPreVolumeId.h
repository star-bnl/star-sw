#ifndef __AgMLPreVolumeId_h__
#define __AgMLPreVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StMessMgr.h>

/**
 * @class AgMLPreVolumeId
 * @brief A volume identifier for a preshower detector.
 *
 * This class provides a unique integer identifier for each sensitive slat
 * in a preshower detector, based on its layer and slat number.
 */
class AgMLPreVolumeId : public AgMLVolumeId {
public:
  
  AgMLPreVolumeId(){}

  virtual int id( int* numbv ) const { 

    int layer = numbv[0]; // layer
    int slat  = numbv[1]; // slat

    return 1000*layer + slat;

  };
};


#endif
