//////////////////////////////////////////////////////////////////////
//
// $Id: StEtGrid.cxx,v 1.4 2003/09/02 17:59:01 perev Exp $
// $Log: StEtGrid.cxx,v $
// Revision 1.4  2003/09/02 17:59:01  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.3  2003/04/30 20:38:37  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.2  2002/06/24 13:22:59  akio
// numerous bug fix & updates
//
// Revision 1.1  2002/02/11 20:30:48  akio
// Many updates, including very first version of jet finder.
//
//
// Revision 1.0  2001/06/14 Akio Ogawa
//
//////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include "StEventTypes.h"
#include "StEvent.h"
#include "StSpinMaker/StppTrack.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StEtGrid.h"
#include "math.h"

ClassImp(StEtGrid)

StEtGrid::StEtGrid() {
  nKeys=0;
}

StEtGrid::~StEtGrid() {
  mGrid->Delete();
  if(mGrid) delete mGrid;
  if(etaMinKey) delete [] etaMinKey;
  if(etaMaxKey) delete [] etaMaxKey;
  if(phiMinKey) delete [] phiMinKey;
  if(phiMaxKey) delete [] phiMaxKey;
}

void  StEtGrid::print(){
  int n = mGrid->GetLast() + 1;
  cout << "Number of Cells = " << n << endl;
  if(n>0) for(int i=0; i<n; i++){cell(i)->print();}
}

void StEtGrid::cutAtThreshold(float t){
   int n = mGrid->GetLast() + 1;
   if(n>0) for(int i=0; i<n; i++){cell(i)->removeBelowThreshold(t);}
}

int StEtGrid::add(int key, float et){
  if(key < 0) return 1;
  if(key >= nKeys) return 2;
  for(int i=0; i<=mGrid->GetLast(); i++){
    if(key == cell(i)->key){
      int res = cell(i)->check(key,etaMinKey[key],etaMaxKey[key],phiMinKey[key],phiMaxKey[key]);
      if(res != 0) return res;
      cell(i)->add(et); 
      mState = 3;
      return 0;
    }
  }
  int n = mGrid->GetLast()+1;
  if(n>=nKeys) {cout << "StEtGrid::add  TClonesArray limit exceeded" << endl; return 1;}
  TClonesArray &g = *mGrid;
  new(g[n]) StEtCell(key,etaMinKey[key],etaMaxKey[key],phiMinKey[key],phiMaxKey[key],et);
  mState=3;
  return 0;
}

int StEtGrid::add(float eta, float phi, float et){
  int key = findKey(eta,phi);
  if(key>=0) return add(key, et);
  return 7;
}

int StEtGrid::add(StppTrack *trk){
  if(trk->flag <= 0) return 8;
  return add(trk->eta, trk->psi, trk->pt);
}

int StEtGrid::add(StMuTrack *trk){
  if(trk->flag() <= 0) return 8;
  return add(trk->eta(), trk->phi(), trk->pt());
}

int StEtGrid::add(StTrack *trk){
  if(trk->flag() <= 0) return 8;
  float eta = -::log(tan(trk->geometry()->momentum().theta()/2.));
  float phi = trk->geometry()->momentum().phi();
  float et = trk->geometry()->momentum().mag();
  return add(eta, phi, et);
}

StJet* StEtGrid::findJet(float radius, float minEtSeed, float minEtCell, int id){
  StJet *jet = 0;
  int n = mGrid->GetLast();
  if(n>=0) {
    sort();
    if(cell(n)->et > minEtSeed){
      jet = new StJet();
      jet->add(cell(n)); cell(n)->jetId = id;
      for(int i=n-1; i>=0; i--){
	if(cell(i)->et < minEtCell) break;
	if(cell(n)->distance(cell(i)) < radius){
	  jet->add(cell(i)); cell(i)->jetId = id;
	}
      }
    }
  }
  return jet;
}

