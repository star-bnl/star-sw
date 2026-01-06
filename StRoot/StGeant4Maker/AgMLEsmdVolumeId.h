#ifndef __AgMLEsmdVolumeId_h__
#define __AgMLEsmdVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <TVirtualMC.h>

class AgMLEsmdVolumeId : public AgMLVolumeId {

  const int onoff    = 1;
  const int fillmode = 3;
  const int sectormap[2][6] = { 
    { 4, 5, 6, 7, 8, 9}, 
    {10,11,12, 1, 2, 3} 
  };

public:
  
  AgMLEsmdVolumeId() : AgMLVolumeId() { };

  virtual int id( int* numbv ) const { 
    
    int rileft = onoff;
    int shift  = 1;
    
    int iWheel    = numbv[0];
    int depth     = numbv[1];
    int phi_30d   = sectormap[iWheel-1][numbv[2]-1];
    int strip     = numbv[3];
    

    int volume_id = 1000000*rileft+10000*phi_30d+1000*depth+strip;

  };
};
/*

fortran has 1 indexing
    rileft    = emcg_onoff
    iWheel = numbv(1+shift) 
    shift  += 1 
    depth     = numbv(1+shift)
    phi_30d   = sector_hash(numbv(2+shift),iWheel)
    strip     = numbv(3+shift) 


	volume_id = 1000000*rileft+10000*phi_30d+1000*depth+strip
*/
#endif