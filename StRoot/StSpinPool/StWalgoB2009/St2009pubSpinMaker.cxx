// $Id: St2009pubSpinMaker.cxx,v 1.5 2010/02/04 03:48:12 balewski Exp $
//
//*-- Author : Jan Balewski, MIT
// 
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StEvent/StEventInfo.h> // just to get time
#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"

#include "St2009WMaker.h"

#include "St2009pubSpinMaker.h"

ClassImp(St2009pubSpinMaker)

//_____________________________________________________________________________
//
St2009pubSpinMaker::St2009pubSpinMaker(const char *name):StMaker(name){
  wMK=0;HList=0;
  core=name;
  par_QPTplus=0.015;
  par_QPTminus=-0.015; 
  par_leptonEta1=-1.; par_leptonEta2=1.;
 }


//_____________________________________________________________________________
//
Int_t 
St2009pubSpinMaker::Init(){
  assert(wMK);
  assert(HList);
  initHistos();
  nRun=0;
  hbxIdeal=0;
  return StMaker::Init();
}


//_____________________________________________________________________________
//
Int_t 
St2009pubSpinMaker::FinishRun  (int runNo){
  char txt[1000];
  sprintf(txt,"events T= %d %d",Tfirst,Tlast);
  printf("Finish run=%d , events time range %s\n",runNo,txt);
  hbxIdeal->GetYaxis()->SetTitle(txt);
  return kStOK;
}

//_____________________________________________________________________________
//
Int_t 
St2009pubSpinMaker::InitRun  (int runNo){
  assert(runNo>= 10081007); // F10407, first pp500 long fill with defined pol pattern
  assert(runNo<=10103046); // F10536, last pp500 fill in run9

  char txt[1000],txt0[100];
  sprintf(txt0,"bxIdeal%d",nRun);
  sprintf(txt,"intended fill pattern  R%d-%d vs. bXing; %s", runNo,nRun,spinDb->getV124comment());
  nRun++;
  Tfirst=int(2e9); Tlast=-Tfirst;
  hbxIdeal=new TH1F(core+txt0,txt,128,-0.5,127.5);
  hbxIdeal->SetFillColor(kYellow);
  HList->Add(hbxIdeal);
  
  spinDb->print(0); // 0=short, 1=huge
  for(int bx=0;bx<120;bx++){
    if(spinDb->isBXfilledUsingInternalBX(bx))  hbxIdeal->Fill(bx);   
  }

  sprintf(txt,"bXing= bx48+off=%d",spinDb->BX48offset());
  hA[3]->GetXaxis()->SetTitle(txt);

  sprintf(txt,"bXing= bx7+off=%d",spinDb->BX7offset());
  hA[4]->GetXaxis()->SetTitle(txt);

  LOG_INFO<<Form("::InitRun(%d) done, W-spin sorting  params: exclude Q/PT in [%.2f, %.2f], leptonEta in[%.1f,%.1f]",
		 par_QPTplus,par_QPTminus,par_leptonEta1, par_leptonEta2
		 )<<endm;	 
  return kStOK;
}

//_____________________________________________________________________________
//
Int_t 
St2009pubSpinMaker::Make(){
  int T=wMK->mMuDstMaker->muDst()->event()->eventInfo().time();
  if(Tlast<T) Tlast=T;
  if(Tfirst>T) Tfirst=T;

  bXingSort();
  return kStOK;
}

//_____________________________________________________________________________
//
void 
St2009pubSpinMaker::bXingSort(){
  //has access to whole W-algo-maker data via pointer 'wMK'
  
  hA[0]->Fill("inp",1.);

  assert(spinDb->isValid());  // all 3 DB records exist 
  assert(spinDb->isPolDirLong());  // you do not want mix Long & Trans by accident
  
  if(wMK->wEve.vertex.size()<=0) return; 
  //......... require: L2W-trig (ET or rnd) & vertex is reasonable .......
  
  int bx48=wMK->wEve.bx48;
  int bx7=wMK->wEve.bx7;
  if(spinDb->offsetBX48minusBX7(bx48,bx7)) {
   printf("BAD bx7=%d bx48=%d del=%d\n",bx7,bx48,spinDb->offsetBX48minusBX7(bx48,bx7));
   hA[0]->Fill("badBx48",1.);
   return;
   // assert(spinDb->offsetBX48minusBX7(bx48,bx7)==0); // both counters must be in sync
  }
 
  hA[1]->Fill(bx48);
  hA[2]->Fill(bx7);

  int bxStar48= spinDb->BXstarUsingBX48(bx48);
  int bxStar7=spinDb->BXstarUsingBX7(bx7);
  hA[3]->Fill(bxStar48);
  hA[4]->Fill(bxStar7);

  int spin4=spinDb->spin4usingBX48(bx48); 
  hA[5]->Fill(bxStar7,spin4);

  float par_maxDsmThr=58;
  float par_myET=25; // monitoring cut
  if( wMK->wEve.l2bitRnd) { // lumi monitor BHT3-random
    StMuEvent* muEve = wMK->mMuDstMaker->muDst()->event();
    int max=0;
    for (int m=0;m<300;m++)  {    // access L0-HT data
      int val=muEve->emcTriggerDetector().highTower(m);
      if(max<val) max=val;
    }
    // avoid too much energy - can be W-events (1/milion :)
    if(max<par_maxDsmThr)  {  hA[6]->Fill(spin4);  hA[0]->Fill("BG1",1.);}
    return;
  }
  
  if( wMK->wEve.l2bitET==0) return; 
  //..... it is guaranteed ..... L2W-ET>13 did fired  ......
  
  
  // search for  Ws ............
  for(uint iv=0;iv<wMK->wEve.vertex.size();iv++) {
    WeveVertex &V=wMK->wEve.vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.pointTower.id==0) continue;

      /* Collect QCD background for lumi monitors */
      float frac24=T.cluster.ET/(T.cl4x4.ET);
      if(iv==0 && it==0 && frac24<wMK->par_clustFrac24) {
	hA[31]->Fill(T.cluster.ET);
	if( T.cluster.ET <20. ) {	hA[7]->Fill(spin4);  hA[0]->Fill("BG2",1.);}
      }

      if(T.isMatch2Cl==false) continue;
      assert(T.cluster.nTower>0); // internal logical error
      assert(T.nearTotET>0); // internal logical error
      
       //put final W cut here
      if(fabs(T.primP.Eta()) > wMK->par_leptonEta) continue;     
      if(T.cluster.ET /T.nearTotET<  wMK->par_nearTotEtFrac) continue; // too large nearET
      if(T.ptBalance.Perp()<wMK->par_ptBalance || T.awayTotET>  wMK->par_awayTotET)  continue;
      hA[0]->Fill("Wcut",1.);

      hA[30]->Fill(T.prMuTrack->eta());
      if(T.prMuTrack->eta()<par_leptonEta1) continue;
      if(T.prMuTrack->eta()>par_leptonEta2) continue;
      hA[0]->Fill("eta",1.);

      //::::::::::::::::::::::::::::::::::::::::::::::::
      //:::::accepted W events for x-section :::::::::::
      //::::::::::::::::::::::::::::::::::::::::::::::::

      float ET=T.cluster.ET;
      if(ET>par_myET) hA[0]->Fill("W25",1.);
      float q2pt=T.prMuTrack->charge()/T.prMuTrack->pt();
      if(ET>par_myET) hA[8]->Fill(q2pt);
      hA[9]->Fill(ET,q2pt);
      if( q2pt> par_QPTminus && q2pt< par_QPTplus) continue;

      const StMuTrack *prTr=T.prMuTrack; assert(prTr);
      float p_Q=prTr->charge();
      if(ET>par_myET) {
	hA[0]->Fill("Q/pT",1.);
	if(p_Q>0) hA[0]->Fill("Q +",1.);
	else  hA[0]->Fill("Q -",1.);
      }

      int iQ=0; // plus
      if( p_Q<0 ) iQ=1;// minus
     
      hA[10+iQ]->Fill(ET);
      if(ET>25 &&ET<50 ) hA[12+iQ]->Fill(spin4);
      if(ET>32 &&ET<44 ) hA[14+iQ]->Fill(spin4);
      if(ET>15 &&ET<20 ) hA[16+iQ]->Fill(spin4);
      hA[18+iQ]->Fill(spin4,ET);	 
     
    } // end of loop over tracks
  }// end of loop ove vertices

}


// $Log: St2009pubSpinMaker.cxx,v $
// Revision 1.5  2010/02/04 03:48:12  balewski
// add ET for lumi monitor
//
// Revision 1.4  2010/01/28 20:10:05  balewski
// added eta dependent spin sorting
//
// Revision 1.3  2010/01/28 03:42:55  balewski
// cleanup
//
// Revision 1.2  2010/01/27 22:12:25  balewski
// spin code matched to x-section code
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
// Revision 1.1  2009/11/23 21:11:18  balewski
// start
//
