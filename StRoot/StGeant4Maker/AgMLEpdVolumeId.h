#ifndef __AgMLEpdVolumeId_h__
#define __AgMLEpdVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StMessMgr.h>

/**
 * @class AgMLEpdVolumeId
 * @brief A volume identifier for the Event Plane Detector (EPD).
 *
 * This class calculates a unique integer identifier for each sensitive tile
 * in the EPD. The ID is constructed from the detector side (east/west),
 * position (clock-wise), and tile number, following the EPD's specific
 * numbering scheme.
 */
class AgMLEpdVolumeId : public AgMLVolumeId {
public:
  
  AgMLEpdVolumeId(){}

  virtual int id( int* numbv ) const { 

    int epdm = numbv[0]; // 1=east, 2=west
    int epss = numbv[1]; // 1 for PP1, 2 for PP2, PP-postion 1'o,2'o clock etc
    int epdt = numbv[2]; // 1:T1 trap, 2:T1 Triangular, 3:T2 Thin, 4:T3 Thick

    /*

      " EPD volume_id " 
      " 100,000 : east or west "
      "   1,000 : Position clock wise, 1 to 12 "
      "      10 : Tile number 1 to 31, refer EPD Drupal page"
      "       1 : 1 T1 trap or T2 thin; 0 T1 triangular or T2 thick
      
     */

    int _id = 0;
    _id += 100000 * epdm;
    _id +=   1000 * epss;
    _id +=     10 * ( epdt%2 + epdt/2 );
    _id +=      1 * ( epdt%2 );

    return _id;

  };
};


#endif
