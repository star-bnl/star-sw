#ifndef __AgMLTpcVolumeId_h__
#define __AgMLTpcVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StMessMgr.h>


/**
 * @class AgMLTpcVolumeId
 * @brief A volume identifier for the Time Projection Chamber (TPC).
 *
 * This class computes a unique integer identifier for each TPC padrow based
 * on its gas volume, super-sector, and raw padrow number. It supports multiple
 * TPC geometries (pre-iTPC, iTPC sector 20 testbed, and full iTPC) by using
 * different internal mappings.
 */
class AgMLTpcVolumeId  : public AgMLVolumeId { 
public:

  const int tpads72[76*2] = { 1,  1,  2,  3,  4,  5,  6,  7,  8,  9,    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,    20, 21, 22, 23, 24, 25, 26, 27, 28, 29,    30, 31, 32, 33, 34, 35, 36, 37, 38, 39,    40, 40, 41, 41, 42, 43, 44, 45, 46, 47, 48, 49,    50, 51, 52, 53, 54, 55, 56, 57, 58, 59,    60, 61, 62, 63, 64, 65, 66, 67, 68, 69,    70, 71, 72, 72,        1,  1,  2,  3,  4,  5,  6,  7,  8,  9,    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,    20, 21, 22, 23, 24, 25, 26, 27, 28, 29,    30, 31, 32, 33, 34, 35, 36, 37, 38, 39,    40, 40, 41, 41, 42, 43, 44, 45, 46, 47, 48, 49,    50, 51, 52, 53, 54, 55, 56, 57, 58, 59,    60, 61, 62, 63, 64, 65, 66, 67, 68, 69,    70, 71, 72, 72  };                     
  const int isdet72[76*2] = { 1,  0,  0,  0,  0,  0,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,      0,  2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,      0,  0,  0,  2,      1,  0,  0,  0,  0,  0,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,      0,  2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,      0,  0,  0,  2 };

  const int tpads45[73*2] = { 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9,10,10,10, 11,11,11,12,12,12,13,13,13,14, 14,15,16,17,18,19,20,21,22,23, 24,25,26,27,28,29,30,31,32,33,     34,35,36,37,38,39,40,41,42,43,     44,45,45};
  const int isdet45[73*2] = { 1, 0, 2, 1, 0, 2, 1, 0, 2, 1, 0, 2, 1, 0, 2, 1, 0, 2, 1, 0, 2, 1, 0, 2, 1, 0, 2, 1, 0, 2, 1, 0, 2, 1, 0, 2, 1, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 2 };
  int mode;

  AgMLTpcVolumeId(); // 3=iTPC era / 2=iTPC sector 20 / 1=pre iTPC

  virtual int id( int* numbv ) const { 

    int tpgv = numbv[0]; // tpc gas volume 1, 2
    int tpss = numbv[1]; // tpc super sector 1-12
    int tpad = numbv[2]; // tpc padrow

    int sector = tpss + 12 * (tpgv-1); // sector 1-24    

    int det = (mode==3)?isdet72[tpad-1]:isdet45[tpad-1]; 
    int pad = (mode==3)?tpads72[tpad-1]:tpads45[tpad-1];

    if ( mode==2 && sector==20 && pad<14 ) {
      sector=99; // erase iTPC from sector 20
    }

    int volumeId = 100000*det + 100*sector + pad;

    return volumeId;


    

  };

};


#if 0
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

#endif
