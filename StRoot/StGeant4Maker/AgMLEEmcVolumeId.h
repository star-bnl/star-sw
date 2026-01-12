#ifndef __AgMLEEmcVolumeId_h__
#define __AgMLEEmcVolumeId_h__

/**
 * @class AgMLEEmcVolumeId
 * @brief A volume identifier for the Endcap Electromagnetic Calorimeter (EEMC).
 *
 * This class provides a unique integer identifier for each sensitive volume
 * in the EEMC. The ID is calculated based on the detector wheel, section,
 * sub-section, phi bin, and eta bin, mapping these to a standard STAR
 * numbering scheme.
 */
class AgMLEEmcVolumeId : public AgMLVolumeId {

  const int onoff    = 1;
  const int fillmode = 3;
  const int sectormap[2][6] = { 
    { 4, 5, 6, 7, 8, 9}, 
    {10,11,12, 1, 2, 3} 
  };


public:
  
  AgMLEEmcVolumeId() : AgMLVolumeId() { /* nada */ };

  virtual int id( int* numbv ) const { 

    // cd == ESCI
    int rileft = onoff;

    int wheel   = numbv[0];  
    int section = numbv[1];
    int idx     = numbv[2];
    int phi30d  = sectormap[wheel-1][idx-1]; // sector number
    int subsec  = numbv[3]; // subsection (i.e. layer) within each section
    int phi     = numbv[5]; // phibin in sector
    int eta     = numbv[6];

    int depth   = subsec + 3 * ( section - 1 );
    
    int volumeid = 100000 * rileft 
                 +   1000 * ( 5 * ( phi30d-1 ) + phi )
                 +     10 *    eta
                 +           depth;

    return volumeid;

  };
};


#endif
