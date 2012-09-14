// $Id: St2011WlumiMaker.cxx,v 1.2 2012/09/14 21:02:29 balewski Exp $
//
//*-- Author : Ross Corliss, MIT

#include "St2011WMaker.h"

#include "St2011WlumiMaker.h"

ClassImp(St2011WlumiMaker)

//_____________________________________________________________________________
//
St2011WlumiMaker::St2011WlumiMaker(const char *name):StMaker(name){
  wMK=0;HList=0;

}


//_____________________________________________________________________________
//
Int_t St2011WlumiMaker::Init(){
  assert(wMK);

  assert(HList);
  initHistos();
  return StMaker::Init();
}


//_____________________________________________________________________________
//
Int_t St2011WlumiMaker::InitRun  (int runumber){
 
  return  kStOK;
}

//_____________________________________________________________________________
//
Int_t St2011WlumiMaker::FinishRun  (int runnumber){


return 0;
}

//_____________________________________________________________________________
//
Int_t 
St2011WlumiMaker::Make(){
 
  bXingSort();
  return kStOK;
}

//_____________________________________________________________________________
//
void 
St2011WlumiMaker::bXingSort(){
  //has access to whole W-algo-maker data via pointer 'wMK'
  
  hA[0]->Fill("inp",1.);
  if(wMK->wEve->vertex.size()<=0) return; 
  hA[0]->Fill("ver",1.);

  uint iv=0;
  WeveVertex &V=wMK->wEve->vertex[iv];
  
  if(V.eleTrack.size()<=0)  return;
  hA[0]->Fill("PT>10",1.);

  int bxStar7=wMK->wEve->bxStar7;
  int bxStar48=wMK->wEve->bxStar48; 
  if(bxStar48!=bxStar7) {
    hA[0]->Fill("badBx48",1.);
    return; // both counters must be in sync
  }
  
  int spin4=wMK->wEve->spin4;  
  hA[2]->Fill(spin4); 
  

  // find max ET of the cluster  matched to track
  float ET=-1;
  for( uint it=0;  it<V.eleTrack.size(); it++) {
    WeveEleTrack &T=V.eleTrack[it];
    if(T.pointTower.id<=0) continue; //skip endcap towers
    if(ET>T.cluster.ET) continue;
    ET=T.cluster.ET;
  }
  if(ET<=1.)  return;
  hA[0]->Fill("B ET>1",1.);
  hA[1]->Fill(ET);

   
  if(ET>5 && ET<10)  hA[3]->Fill(spin4); 
  if(ET>10 && ET<14)  hA[4]->Fill(spin4); 

  
  return;
}

// $Log: St2011WlumiMaker.cxx,v $
// Revision 1.2  2012/09/14 21:02:29  balewski
// *lumi-maker re-written to accumulate alternative rel lumi monitors,
// * added spin sorting to Zs
//
// Revision 1.1  2011/02/10 20:33:23  balewski
// start
//
