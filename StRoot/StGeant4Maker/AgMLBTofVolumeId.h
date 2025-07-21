#ifndef __AgMLBtofVolumeId_h__
#define __AgMLBtofVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StarVMC/StarGeometry/StarGeo.h>
#include <StMessMgr.h>

/**
 * @class AgMLBtofVolumeId
 * @brief A volume identifier for the Barrel Time-of-Flight (BTOF) detector.
 *
 * This class computes a unique integer identifier for each sensitive volume
 * in the BTOF detector. The ID is based on the tray, sector, module, and
 * cell (layer) number. It includes special adjustments for GMT (Gas-Multi-gap
 * Resistive Plate Chamber for the Muon Telescope Detector) modules present
 * in some geometry versions.
 */
class AgMLBtofVolumeId : public AgMLVolumeId {
public:

  AgMLBtofVolumeId(){}
  // Applies to btog.version = 8 with btog.choice =13 (run 13 onwards)
  
  virtual int id( int* numbv ) const { 

    int rileft = numbv[0];
    int sector = numbv[1];
    int module = numbv[2];
    int layer  = numbv[3];

    // Adjust for GMT modules

    if ( StarGeometry::HasDetector("BTOFv8") ) {
      if ( rileft==1 ) {
	if      ( sector== 8 )   module += 4;
	else if ( sector==23 )   module += 4;
      }
      else if ( rileft==2 ) {
	if      ( sector == 33 ) module += 4;
	else if ( sector == 48 ) module += 4;
      }
    }
      
    int _id = layer + 10 * (module + 100 * (sector+100*rileft) );

    return _id;

  };
};


#endif
