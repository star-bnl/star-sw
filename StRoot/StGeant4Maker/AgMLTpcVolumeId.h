#ifndef __AgMLTpcVolumeId_h__
#define __AgMLTpcVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StMessMgr.h>

class AgMLTpcVolumeId : public AgMLVolumeId {
public:
  
  AgMLTpcVolumeId();

  int tpads[76*2];
  int isdet[76*2];

  virtual int id( int* numbv ) const { 

    int tpgv = numbv[0]; // tpc gas volume 1, 2
    int tpss = numbv[1]; // tpc super sector 1-12
    int tpad = numbv[2]; // tpc padrow

    int sector = tpss + 12 * (tpgv-1); // sector 1-24    

    int det = isdet[tpad-1]; // memcheck flags invalid read of size 4
    int pad = tpads[tpad-1];

    int volumeid = 100000*det + 100*sector + pad;                

    return volumeid;

  };
};

class AgMLTpcVolumeIdTest__ : public AgMLTpcVolumeId {
public:
  
  AgMLTpcVolumeIdTest__() : AgMLTpcVolumeId(){ };

  virtual int id( int* numbv ) const { 

    int tpgv = numbv[0]; // tpc gas volume 1, 2
    assert(tpgv==1 || tpgv==2 );

    int tpss = numbv[1]; // tpc super sector 1-12
    assert( tpss>=1 && tpss <=12 );

    int tpad = numbv[2]; // tpc padrow
    assert( tpad>=1 && tpad <= 76*2 );

    int sector = tpss + 12 * (tpgv-1); // sector 1-24    
    assert( sector >= 1 && sector <= 24 );

    int det = isdet[tpad-1]; // memcheck flags invalid read of size 4
    assert( det>=0 && det<=2 );

    int pad = tpads[tpad-1];
    assert(pad>=1 && pad<=72 );

    int volumeid = 100000*det + 100*sector + pad;                

    return volumeid;

  };
};


#endif
