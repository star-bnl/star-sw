//EemcHit.h
//R. Fatemi (IUCF)
//9/04

//std
#include <string>
#include <iostream>
#include <cmath>
using namespace std;

//root
#include "TVector3.h"
#include "TFile.h"

//star
#include "StChain.h"

//MuDst
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

//Endcap
#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

//local
#include "StJetMaker/StEmcHitMakers/EemcHit.h"

const EEmcDbItem* EemcHit::dbItem(StEEmcDbMaker* eeDb)
{
    return eeDb->getT(mSector, mSubSector-1+'A', mEtaBin);
}
