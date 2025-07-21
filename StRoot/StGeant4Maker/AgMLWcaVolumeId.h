#ifndef __AgMLWcaVolumeId_h__
#define __AgMLWcaVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StMessMgr.h>

/**
 * @class AgMLWcaVolumeId
 * @brief A volume identifier for the Wcal (forward hadron calorimeter).
 *
 * This class provides a unique integer identifier for each sensitive tower
 * in the Wcal detector, based on its module and tower number.
 */
class AgMLWcaVolumeId : public AgMLVolumeId {
public:
  
  AgMLWcaVolumeId(){}

  virtual int id( int* numbv ) const { 

    int mod = numbv[0]; // module
    int tow = numbv[1]; // tower

    return 1000*mod + tow;

  };
};


#endif
