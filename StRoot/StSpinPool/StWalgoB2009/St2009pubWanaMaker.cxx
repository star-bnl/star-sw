// $Id: St2009pubWanaMaker.cxx,v 1.1 2009/11/23 23:00:18 balewski Exp $
//
//*-- Author : Jan Balewski, MIT
// 
#include <StEmcUtil/database/StEmcDecoder.h>

#include "St2009WMaker.h"
#include "St2009pubWanaMaker.h"

ClassImp(St2009pubWanaMaker)

//_____________________________________________________________________________
//
St2009pubWanaMaker::St2009pubWanaMaker(const char *name):StMaker(name){
  wMK=0;HList=0;
  par_highET=20; // GeV
  mMappB=0;
}


//_____________________________________________________________________________
//
Int_t St2009pubWanaMaker::Init(){
  assert(wMK);
  assert(HList);
  initHistos();
  return StMaker::Init();
}

//_____________________________________________________________________________
//
Int_t 
St2009pubWanaMaker::InitRun(int runNo){
  int yyyymmdd = this->GetDBTime().GetDate();
  int hhmmss = this->GetDBTime().GetTime();
  // this is how BTOW mapping is accesible
  //  assert( mMappB==0) ; // do not know how to destroy previous instance,JB
  mMappB = new StEmcDecoder(yyyymmdd,hhmmss);
 return kStOK;
}
//_____________________________________________________________________________
//
Int_t 
St2009pubWanaMaker::Make(){
  //  printf("in %s\n", GetName());
  hA[0]->Fill("inp",1.);

  evalWeleTrackSign();
  scanCrateRate();
  varyCuts4backgStudy();

  return kStOK;
}

//_____________________________________________________________________________
//
void 
St2009pubWanaMaker::evalWeleTrackSign(){
  //has access to whole W-algo-maker data via pointer 'wMK'
 

  // search for  Ws ............
  for(uint iv=0;iv<wMK->wEve.vertex.size();iv++) {
    WeveVertex &V=wMK->wEve.vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.isMatch2Cl==false) continue;
      assert(T.cluster.nTower>0); // internal logical error
      assert(T.nearTotET>0); // internal logical error
      if(T.cluster.ET /T.nearTotET< wMK->par_nearTotEtFrac) continue; // too large nearET
      if(T.awayTotET> wMK-> par_awayTotET) continue; // too large awayET
      
      hA[0]->Fill("acc",1.);
      
      // work with W-track
      float ET=T.cluster.ET;
      const StMuTrack *glTr=T.glMuTrack; assert(glTr);
      const StMuTrack *prTr=T.prMuTrack; assert(prTr);
      float PT=glTr->pt();
      float g_chrg=glTr->charge();
      float p_chrg=prTr->charge();
      //printf("aaa %f %f %f\n",ET,PT,chrg);
      int g_ipn=0, p_ipn=0; // plus
      if( g_chrg<0 ) g_ipn=1;// minus
      if( p_chrg<0 ) p_ipn=1;// minus
      hA[5]->Fill(ET);
      hA[10+g_ipn]->Fill(ET);
      hA[12+p_ipn]->Fill(ET);
      if(g_chrg* p_chrg <-0.5) hA[14+p_ipn]->Fill(ET); // charge flip
      hA[6]->Fill(ET,g_chrg/PT);

      //Change in pT from global to primary //JS
      float primPT=prTr->pt();
      float globPT=glTr->pt();
      hA[28]->Fill(primPT,globPT);
      hA[29]->Fill(globPT-primPT);
      if(fabs(globPT-primPT)>1) hA[30]->Fill(ET);
      if(g_chrg* p_chrg <-0.5) hA[31]->Fill(globPT-primPT);

      // work with prim component
      hA[7]->Fill(ET,p_chrg/prTr->pt());
      if(ET<par_highET) continue;
      hA[0]->Fill("W",1.);
      hA[16+p_ipn]->Fill(prTr->eta());
      

    }
  }

}

//_____________________________________________________________________________
//
void 
St2009pubWanaMaker::scanCrateRate(){
  //has access to whole W-algo-maker data via pointer 'wMK'
  // printf("crateScan: eveID=%d\n",wMK->wEve.id);

  // search for  Ws ............
  for(uint iv=0;iv<wMK->wEve.vertex.size();iv++) {
    WeveVertex &V=wMK->wEve.vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      // T.pointTower.print();
      int softID=T.pointTower.id;
      if(softID<=0) continue;
      if(wMK->wEve.bemc.statTile[kBTow][softID-1]) continue; // skip masked towers

      int RDO, CR,CHAN;
      assert(mMappB->GetDaqIdFromTowerId(softID,RDO)==1);// is good range
      assert(mMappB->GetTowerCrateFromDaqId(RDO,CR,CHAN)==1);

      float adc=wMK->wEve.bemc.adcTile[kBTow][softID-1];
      hA[1]->Fill(adc,CR);
      // printf("soft=%d DRO=%d CR=%d CHAN=%d adc=%.0f\n",softID,RDO,CR,CHAN,adc);
    }
  }
}



//_____________________________________________________________________________
//
void 
St2009pubWanaMaker::varyCuts4backgStudy(){

  for(uint iv=0;iv<wMK->wEve.vertex.size();iv++) {
    WeveVertex &V=wMK->wEve.vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.isMatch2Cl==false) continue;
      assert(T.cluster.nTower>0); // internal logical error
      assert(T.nearTotET>0); // internal logical error

      float nearR=T.cluster.ET /T.nearTotET;
      float awayET=T.awayTotET;
      float ET=	T.cluster.ET;

      // .... logic of histos .....
      if(nearR >0.9) {
	hA[27] -> Fill(ET, awayET);
	if(awayET<8.) {
	  hA[20]->Fill(ET);
	} else {
	  hA[21]->Fill(ET);
	} 
      }

      if(nearR >0.8 && nearR <0.9) {
	if(awayET<8.) {
	  hA[22]->Fill(ET);
	} else {
	  hA[23]->Fill(ET);
	}
      }

      if(nearR >0. && nearR <0.8) {
	if(awayET<8.) {
	  hA[24]->Fill(ET);
	} else {
	  hA[25]->Fill(ET);
	}
      }

      if(awayET<8.) {
	hA[26]->Fill(nearR);
      }

      } // end of loop over tracks
  }
}

// $Log: St2009pubWanaMaker.cxx,v $
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
// Revision 1.1  2009/11/23 21:11:18  balewski
// start
//
