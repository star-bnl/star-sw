#ifndef __AgMLETofVolumeId_h__
#define __AgMLETofVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>

/**
 * @class AgMLEtofVolumeId
 * @brief A volume identifier for the Endcap Time-of-Flight (ETOF) detector.
 *
 * This class calculates a unique integer identifier for each sensitive cell
 * in the ETOF detector. The ID is constructed from the plane, sector, counter,
 * gas gap, and cell number, following the ETOF numbering scheme.
 */
class AgMLEtofVolumeId : public AgMLVolumeId {
  
public:
  AgMLEtofVolumeId() : AgMLVolumeId(){ /* nada */ }

  virtual int id( int* numbv ) const {

    // cd 'eto'
    int etof_plane    = numbv[0]/100; //         "1 closest to IP, 3 furthest from IP"
    int etof_sector   = numbv[0]%100; //  "matches TPC scheme 13 to 24"
    int etof_counter  = numbv[1];     //         "3 counters per gas volume"
    int etof_gap      = numbv[2];     //         "12 gaps between glass"
    int etof_cell     = numbv[3];     //         "32 cells per gap"
            

    int volume_id = etof_cell + 
      100         * etof_gap     +
      10000       * etof_counter + 
      100000      * etof_sector  +   
      10000000    * etof_plane;

    return volume_id;

  };

};
#endif
