#ifndef __AgMLMtdVolumeId_h__
#define __AgMLMtdVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StarVMC/StarGeometry/StarGeo.h>
#include <StMessMgr.h>

/**
 * @class AgMLMtdVolumeId
 * @brief A volume identifier for the Muon Telescope Detector (MTD).
 *
 * This class provides a unique integer identifier for each sensitive volume
 * in the MTD, based on its sector, module, and layer number. It is applicable
 * to geometry versions from Run 13 onwards.
 */
class AgMLMtdVolumeId : public AgMLVolumeId {
public:

  AgMLMtdVolumeId(){}
  // Applies to btog.version = 8 with btog.choice =13 (run 13 onwards)
  
  virtual int id( int* numbv ) const { 

    int sector = numbv[0];
    int module = numbv[1];
    int layer  = numbv[2];
      
    int _id = 1000*sector + 100*module + layer;

    return _id;

  };
};


#endif
