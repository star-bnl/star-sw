//////////////////////////////////////////////////////////////////////
//
// $Id: StSpinTagMaker.cxx,v 1.2 2003/01/03 16:40:39 balewski Exp $
// $Log: StSpinTagMaker.cxx,v $
// Revision 1.2  2003/01/03 16:40:39  balewski
// cleanup (2)
//
// Revision 1.1  2001/06/22 19:41:45  balewski
// *** empty log message ***
//
//
// Revision 1.0  2001/06/14 Akio Ogawa
//
//////////////////////////////////////////////////////////////////////
#include <string.h>

#include "StSpinTagMaker.h"
#include "StChain.h"
#include "tables/St_SpinTag_Table.h"
#include "StEventTypes.h"
#include "Stypes.h"
#include "StMessMgr.h"
#include "St_emc_Maker/StEmcHitCollection.h"
#include "tables/St_g2t_ctf_hit_Table.h"


//#include "tables/St_dst_TrgDet_Table.h"
//#include "tables/St_dst_event_summary_Table.h"

ClassImp(StSpinTagMaker)
StSpinTagMaker::StSpinTagMaker(const char *name):StMaker(name){
}

StSpinTagMaker::~StSpinTagMaker(){
}

Int_t StSpinTagMaker::Init(){
   return StMaker::Init();
}

Int_t StSpinTagMaker::Make(){
  float BemcTowerThres=0.15;

  printf("Make: %s ...\n",GetName());

  // Find St_dst_TrgDet
  //St_dst_TrgDet *TrgDet = (St_dst_TrgDet *) GetInputDS("dst/TrgDet");
  //if(!TrgDet) return 0;
  //dst_TrgDet_st *tt = (dst_TrgDet_st *) TrgDet->GetTable();
  // Create a data set and add the table to it.

  St_SpinTag *tagtab= new St_SpinTag("SpinTag",1); m_DataSet->Add(tagtab);

  SpinTag_st row;
  //fill default values for the Spin Tags
  memset(&row,0,sizeof(row));
  row.maxPt=-1;
  tagtab->AddAt(&row,0);  
  //  tagtab->Print(0,2);


  // Find StEvent  
  StEvent* event = (StEvent *)GetInputDS("StEvent");
  if (!event) return kStErr;
  
  // Filling leading pt, and sum(abs(pt))
  StSPtrVecTrackNode& exnode = event->trackNodes();
  Int_t node = exnode.size();
  for(Int_t i=0; i<node; i++){
    UInt_t nprim = exnode[i]->entries(primary);
    if(nprim == 1) {
      row.nPrimary++;
      StTrack *tp = exnode[i]->track(primary);
      float pt = sqrt(tp->geometry()->momentum().x()*tp->geometry()->momentum().x()+
		      tp->geometry()->momentum().y()*tp->geometry()->momentum().y());
      if(pt>row.maxPt) row.maxPt = pt;
      row.sumAbsPt += pt;
    }
  } 
#if 0 // tmp off because B-EMC simu was not in the chain,JB

  // find highest BEMC tower from StEvent
  StEmcCollection *emc=event->emcCollection();  assert(emc);
  StDetectorId id = static_cast<StDetectorId>(kBarrelEmcTowerId);
  StEmcDetector* mDet=emc->detector(id); // getting detector pointer
  int nmodule=mDet->numberOfModules();

  printf(" dump BEMC=%d from StEvent\n",kBarrelEmcTowerId);
  printf("nhit=%d, nmodule=%d\n",mDet->numberOfHits() ,nmodule);

  for(Int_t mod=1; mod <= nmodule; mod++) {// loop in modules
    StSPtrVecEmcRawHit& hits=mDet->module(mod)->hits();
    for(int ih=0; ih<hits.size(); ih++) { //access hits
      float energy=hits[ih]->energy();
      if(energy>BemcTowerThres)row.nBemcTower++;
      if(row.bemcHighTower<energy) row.bemcHighTower=energy;
      //printf("mod=%d, sub=%d, eta=%d,ih=%d, energy=%f\n",mod,hits[ih]->sub(),hits[ih]->eta(),ih,energy);
      //hits[ih]->Dump();      
    }
  }
#endif

  tagtab->AddAt(&row,0);  
  tagtab->Print(0,2);

  return kStOK;  
}



#if 0
  // find highest BEMC tower from DatSet

  printf("\n dump BEMC from DatSet\n");
  St_DataSetIter itr(GetDataSet("emc_hits"));
  StEmcHitCollection* hit=0;
  while((hit = (StEmcHitCollection*)itr()))  {
    if(strcmp(hit->GetName(),"bemc" ) )continue;// tower hits only
    for(Int_t j=0; j<hit->NHit(); j++)
      {
	Float_t energy=hit->HitEnergy(j);
	Int_t hid=hit->HitId(j);
	Int_t module,eta,sub;
	hit->getBin(hid,module,eta,sub);
	printf("BEMC tower j=%d, energy=%f, id=%d, module=%d, eta=%d, sub=%d\n",j,energy,hid,module,eta,sub);
      }
  }

  /* 
     g=chain->GetDataSet("geant") 
     d=g->Find("g2t_emc_hit")
     St_g2t_emc_hit *h=(St_g2t_emc_hit *) d   
     h->Print(0,10)  
  */

  
#endif

