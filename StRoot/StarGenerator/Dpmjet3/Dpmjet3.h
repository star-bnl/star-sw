#ifndef __Dpmjet3_h__
#define __Dpmjet3_h__

#include "StarCallf77.h"
#include <string>
using namespace std;

#include "TObject.h"
  
void DpmjetInit(int *particle, float* energy);
void DpmjetAAEvent();

//
// Interface to the DTEVT1 common block
//

#define address_of_dtevt1 F77_NAME( address_of_dtect1, ADDRESS_OF_DTEVT1 )
#define address_of_dtglcp F77_NAME( address_of_dtglcp, ADDRESS_OF_DTGLCP )

struct DtEvt1_t {
    int NHKK,NEVHKK;
    int ISTHKK[200000],IDHKK[200000];
    int JMOHKK[2][200000],JDAHKK[2][200000];
    double PHKK[5][200000],VHKK[4][200000],WHKK[4][200000];

    /* Add access methods which mimic fortran arrays */
    int    &st( int i )       {return ISTHKK[i-1];}
    int    &id( int i )       {return IDHKK[i-1];}
    int    &mo( int i, int j ){return JMOHKK[j-1][i-1];}
    int    &da( int i, int j ){return JDAHKK[j-1][i-1];}
    double &p ( int i, int j ){return PHKK[j-1][i-1];}
    double &v ( int i, int j ){return VHKK[j-1][i-1];}
};
extern "C" DtEvt1_t *address_of_dtevt1();

struct DtGlcp_t{
    double RPROJ,RTARG,BIMPAC;
    int NWTSAM,NWASAM,NWBSAM,NWTACC,NWAACC,NWBACC;
};
extern "C" DtGlcp_t *address_of_dtglcp();

#endif
