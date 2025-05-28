#include <AgMLTpcVolumeId.h>

#include <StarVMC/StarGeometry/StarGeo.h>

AgMLTpcVolumeId::AgMLTpcVolumeId() : 
  AgMLVolumeId(),
  mode(1)
{
  if ( StarGeometry::HasDetector( "TPCEv6"  ) || StarGeometry::HasDetector( "TPCEv61" ) ) {
    LOG_INFO << "TPC volume IDs configured for full iTPC" << endm;
    mode=3;
  }
  else if ( StarGeometry::HasDetector( "TPCE32" ) ) {
    LOG_INFO << "TPC volume IDs configured for testbed iTPC / sector 20 inner will be mapped out of range for tpcrs." << endm;
    mode=2;
  }
  else {
    LOG_INFO << "TPC volume IDs configured for classic inner detector" << endm;
    mode=1;
  }
};

				     

