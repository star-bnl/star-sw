#ifndef CTBRESPONSE_H
#define  CTBRESPONSE_H

#include <TH1.h>
#include "StVertexMaker.h"
#include <Stiostream.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <vector>

const int MxTimeSlot=51;
const float bXingTimeSep=213; //(ns)
const int firstBXing=-25;
const int trigBXing=-firstBXing;

class CtbResponse {

 private:
    unsigned int mMode; // 0 == data, 1 == MC, 2 == Embedding?

 public:
    void SetCTBMode(unsigned int mode){ mMode = mode;};
    unsigned int GetCTBMode(){ return mMode;};
    
    CtbResponse( StVertexMaker *, int* , float *,unsigned int);
    struct Jcyl {float eta,phi,gBXing; int ID;};  // ID=tray+120*slat 
    vector <Jcyl> hits[MxTimeSlot];
    void * GVER[MxTimeSlot];
    void  ctb_get_slat_from_data(int slat, int tray, float & phiDeg, float &eta);
};

#endif

