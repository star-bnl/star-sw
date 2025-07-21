#ifndef __AgMLEEmcVolumeId_h__
#define __AgMLEEmcVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <TGeoNavigator.h>
#include <StMessMgr.h>
#include <TLorentzVector.h>
#include <TVirtualMC.h>
#include <TMath.h>

#include <TString.h>
#include <StMessMgr.h>

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
    int shift  = 0;

    int wheel   = numbv[0];  
    int section = numbv[1];
    int idx     = numbv[2];
    int phi30d  = sectormap[wheel-1][idx-1]; // sector number
    int subsec  = numbv[3]; // subsection (i.e. layer) within each section
    int sublay  = numbv[4]; // ???
    int phi     = numbv[5]; // phibin in sector
    int eta     = numbv[6];

    int depth   = subsec + 3 * ( section - 1 );
    
    //    int  volumeid = 100000*rileft + 1000*(5*(phi30d-1)+phi) + 10*eta + depth;

    int volumeid = 100000 * rileft 
                 +   1000 * ( 5 * ( phi30d-1 ) + phi )
                 +     10 *    eta
                 +           depth;

    // LOG_INFO << Form("AgML EEmc VolumdId = wheel=%i section=%i idx=%i phi30d=%i subsection=%i %i %i %i %i",
    // 		     numbv[0],
    // 		     numbv[1],
    // 		     numbv[2],
    // 		     phi30d,
    // 		     numbv[3],
    // 		     numbv[4],
    // 		     numbv[5], 
    // 		     numbv[6],
    // 		     volumeid ) << endm;    


    return volumeid;

  };
};


#endif
