#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <cmath>

#include "TObjectSet.h"

#include <St_base/StMessMgr.h>
#include "St_db_Maker/St_db_Maker.h"

#include "StGenericVertexMaker/StiPPVertex/BtofHitList.h"

#include "StChain/StMaker.h"
#include "StBTofUtil/StBTofGeometry.h"
#include "StBTofUtil/StBTofTables.h"
#include "StEvent/StBTofCollection.h"

#include "TGeoManager.h"


//==========================================================
//==========================================================
BtofHitList::BtofHitList() :
  // phi, 60 bins
  // eta, 32*2 bins not with the same width, so eta0,deta are really not used
  ScintHitList(0.,M_PI/60,60, -0.9,0.028125,64,"Btof",4,0.75)
{
  myTable = new StBTofTables();
}


//==========================================================
//==========================================================
void
BtofHitList::initRun(St_db_Maker* db_maker) {
  LOG_DEBUG <<Form("BtofHitList::initRun() start")<<endm;
  ScintHitList::initRun();
  // clear old lookup table
  int i,j,k;
  for(i=0;i<mxTray;i++)
    for(j=0;j<mxModule;j++)
      for(k=0;k<mxCell;k++)
	tmc2bin[i][j][k]=-1;

  // .. grab table
  myTable->loadTables(db_maker);

  // Initialize BTOF geometry
  TObjectSet *geom = (TObjectSet *) db_maker->GetDataSet("btofGeometry");
  if (geom)   geometry = (StBTofGeometry *) geom->GetObject();
  if (geometry) {
    LOG_INFO << " Found btofGeometry ... " << endm;
  } else {
    geometry = new StBTofGeometry("btofGeometry","btofGeometry in VertexFinder");
    geom = new TObjectSet("btofGeometry",geometry);
    LOG_INFO << " Create a new btofGeometry ... " << endm;
    db_maker->AddConst(geom);
  } 
  if(geometry && !geometry->IsInitDone()) {
    LOG_INFO << " BTofGeometry initialization ... " << endm;
    TVolume *starHall = gGeoManager ? nullptr : (TVolume *) (db_maker->GetDataSet("HALL"));
    geometry->Init(db_maker, starHall, gGeoManager);
  }


  int nB=0; int nA=0;
  for(int i=0;i<mxTray;i++) {
    for(int j=0;j<mxModule;j++) {
      for(int k=0;k<mxCell;k++) {
        int iBin = cell2bin(i+1,j+1,k+1);
        tmc2bin[i][j][k]=iBin;

        int  status = 1;  //
        status = myTable->status(i+1,j+1,k+1);
        nB++;
        if( status!= 1)  continue; // drop broken channels

        setActive(iBin);
        nA++;
      }
    }
  }

  LOG_INFO <<" BtofHitList::initRun() done,  active="<<nA<<" of "<<nB<<" BTOF channels" <<endm;

}

//==========================================================
//==========================================================
void
BtofHitList::clear(){
  ScintHitList::clear();

}

//==========================================================
//==========================================================
BtofHitList::~BtofHitList(){
  if(myTable) delete myTable;
}

//==========================================================
//==========================================================
void
BtofHitList::build ( StBTofCollection *btofColl){

  if(!btofColl || !btofColl->hitsPresent()) {
    LOG_INFO << " No available BTOF hits for this event ... " << endm;
    return;
  }

  StSPtrVecBTofHit& tofHits = btofColl->tofHits();

  for(size_t i=0;i<tofHits.size();i++) { //loop on hits in modules
    StBTofHit *aHit = tofHits[i];
    if(!aHit) continue;
    int t = aHit->tray();
    int m = aHit->module();
    int c = aHit->cell();
    if(t<=0||t>mxTray||m<=0||m>mxModule||c<=0||c>mxCell) continue;

    int iBin=tmc2bin[t-1][m-1][c-1];
      
    if ( getActive(iBin)<0) continue;
    setFired(iBin);
  }

/*
  // test with Raw Hits
  if(!btofColl || !btofColl->rawHitsPresent()) {
    LOG_INFO << " No available BTOF hits for this event ... " << endm;
    return;
  }

  StSPtrVecBTofRawHit& tofRawHits = btofColl->tofRawHits();

  int chan2mc[24] = {3, 13, 8, 15, 10, 1, 6, 5, 17, 18, 2, 7, 20, 22, 0, 4, 23, 21, 16, 14, 9, 19, 12, 11};
  for(size_t i=0;i<tofRawHits.size();i++) { //loop on hits in modules
    StBTofRawHit *aHit = tofRawHits[i];
    if(!aHit) continue;
    if(!aHit->leadingEdge()) continue;  
    int t = aHit->tray();
    int chan = aHit->channel();
    int tdig = chan/24 + 1;
    int index = chan%24;
    int mc = chan2mc[index];
    int m = (tdig-1)*4 + mc/6 + 1;
    int c = mc%6 + 1;
    if(t<=0||t>mxTray||m<=0||m>mxModule||c<=0||c>mxCell) continue;

    int iBin=tmc2bin[t-1][m-1][c-1];
      
    if ( getActive(iBin)<0) continue;
    setFired(iBin);
  }
*/
};
//==========================================================
//==========================================================
int
BtofHitList::cell2bin(int tray, int module, int cell) {
   int iPhi = (tray-1)%60;
   int iEta = (tray-1)/60*mxModule + (module-1);
   assert( iEta>=0);
   assert( iPhi>=0);
   return iPhiEta2bin(iPhi,iEta);
}
//==========================================================
//==========================================================
int
BtofHitList::addBtofTrack(int tray, int module, int cell) {
  int iBin = cell2bin(tray, module, cell);
  track[iBin]++;
  nTrack++;   // need to be set here rather than ScintList for BTOF
  return iBin;
}
//==========================================================
//==========================================================
//** need to be set here rather than ScintList for BTOF  **//
int
BtofHitList::addBtofMatch(IntVec ibinVec) {
  if(isMatched(ibinVec)) nMatch++;
  return nMatch;
}
//==========================================================
//==========================================================
bool
BtofHitList::isMatched(IntVec ibinVec) {
  bool match = kFALSE;
  for(size_t i=0;i<ibinVec.size();i++) {
    int iBin = ibinVec[i];
    if(getActive(iBin)>0) {
      if(getTrack(iBin)>0&&getFired(iBin)>0) match = kTRUE;
    }
  }
  return match;
}
//==========================================================
//==========================================================
bool
BtofHitList::isVetoed(IntVec ibinVec) {
  int nA = 0;
  bool veto = kTRUE;
  for(size_t i=0;i<ibinVec.size();i++) {
    int iBin = ibinVec[i];
    if(getActive(iBin)>0) {
      nA++;
      if(getTrack(iBin)>0&&getFired(iBin)==0) veto &= kTRUE;
      else veto &= kFALSE;
    }
  }
  if(nA==0) return kFALSE;
  else return veto;
}
//==========================================================
//==========================================================
float
BtofHitList::getWeight(IntVec ibinVec) {
  const float Wdunno=1;
  int nA = 0;
  for(size_t i=0;i<ibinVec.size();i++) {
    int iBin = ibinVec[i];
    if(getActive(iBin)>0) nA++;
  }  
  if(nA==0) return Wdunno;
  if(isMatched(ibinVec)) return Wmatch;
  if(isVetoed(ibinVec)) return Wveto;
  return Wdunno;
}
//==========================================================
//==========================================================
int
BtofHitList::etaBin(float eta){  // not in a real case, be care when using this
  if(fabs(eta)>0.9)  return -1;
  int iEta=(int)((eta-eta0)/dEta); 
  if(iEta<0 || iEta>=nEta) return -1; // out of Eta range
  return iEta;
}

//==========================================================
//==========================================================
float
BtofHitList::bin2EtaLeft(int iEta){ // not in a real case, be care when using this
  assert(iEta>=0);
  assert(iEta<nEta);
  float etaF= eta0+iEta*dEta ;
  if(etaF<-0.9) etaF=0.9;
  return etaF;
}
