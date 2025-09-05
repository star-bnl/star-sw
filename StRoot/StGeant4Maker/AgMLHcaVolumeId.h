#ifndef __AgMLHcaVolumeId_h__
#define __AgMLHcaVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StMessMgr.h>

/**
 * @class AgMLHcaVolumeId
 * @brief A volume identifier for the Hcal (forward hadron calorimeter).
 *
 * This class provides a unique integer identifier for each sensitive tower
 * in the Hcal detector, based on its module and tower number.
 */
class AgMLHcaVolumeId : public AgMLVolumeId {
public:
  
  AgMLHcaVolumeId(){}

  virtual int id( int* numbv ) const { 

    int mod = numbv[0]; // module
    int tow = numbv[1]; // tower

    return 1000*mod + tow;

  };
};


#endif
