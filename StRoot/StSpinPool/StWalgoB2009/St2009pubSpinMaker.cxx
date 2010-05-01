// $Id: St2009pubSpinMaker.cxx,v 1.12 2010/05/01 01:31:45 balewski Exp $
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
  par_QPTlow=0.010;

  par_QPThighET0=25; 
  par_QPThighET1=50; 
  par_QPThighA=0.08; 
  par_QPThighB=0.0013; 
  par_leptonEta1=-1.; par_leptonEta2=1.;
  par_useNoEEMC=0;
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

  LOG_INFO<<Form("::InitRun(%d) done, W-spin sorting  params: exclude |Q/PT| < %.2f OR |Q/PT| above line %.3f*(ET-%.1f)-%6e if ET<%.1f, for AL use leptonEta in[%.1f,%.1f] useNoEEMC=%d", runNo,
		 par_QPTlow,par_QPThighET0, par_QPThighA ,par_QPThighB,par_QPThighET1,par_leptonEta1, par_leptonEta2,par_useNoEEMC
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
   return; // both counters must be in sync
  }

  //remove events tagged as Zs
  if(wMK->wEve.zTag) return;
  hA[0]->Fill("noZ",1.);
 

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
	if( T.cluster.ET <20. ) { hA[7]->Fill(spin4);  hA[0]->Fill("BG2",1.);}
      }

      if(T.isMatch2Cl==false) continue;
      assert(T.cluster.nTower>0); // internal logical error
      assert(T.nearTotET>0); // internal logical error

      int iQ=0; // plus
      float p_Q=T.prMuTrack->charge();
      if( p_Q<0 ) iQ=1;// minus
      float ET=T.cluster.ET;
      

      //put final W cut here
      bool isW= T.cluster.ET /T.nearTotET> wMK->par_nearTotEtFrac;  // near cone
      if(par_useNoEEMC) 
	isW=isW && T.sPtBalance_noEEMC>wMK->par_ptBalance; // awayET
      else
	isW=isW && T.sPtBalance>wMK->par_ptBalance; // awayET
    
      if(!isW) { // AL(QCD)
	if(ET>15 &&ET<20 ) hA[16+iQ]->Fill(spin4);
	continue;
      }

      hA[0]->Fill("Wcut",1.);

      hA[30]->Fill(T.prMuTrack->eta());
      if(fabs(T.primP.Eta()) > wMK->par_leptonEta) continue;     
      // allows further cuts on eta
      if(T.prMuTrack->eta()<par_leptonEta1) continue;
      if(T.prMuTrack->eta()>par_leptonEta2) continue;
      hA[0]->Fill("eta",1.);

      //::::::::::::::::::::::::::::::::::::::::::::::::
      //:::::accepted W events for x-section :::::::::::
      //::::::::::::::::::::::::::::::::::::::::::::::::

      if(ET>par_myET) hA[0]->Fill("W25",1.);
      float q2pt=T.prMuTrack->charge()/T.prMuTrack->pt();
      if(ET>par_myET) hA[8]->Fill(q2pt);
      hA[9]->Fill(ET,q2pt);
      
      // apply cut on reco charge
      if( fabs(q2pt)< par_QPTlow) continue;
      if(ET>par_myET) hA[0]->Fill("Qlow",1.);

      if(par_QPTlow>0) { // abaility to skip all Q/PT cuts
	if( fabs(q2pt)< par_QPTlow) continue;
	float highCut=par_QPThighA - (ET-par_QPThighET0)*par_QPThighB;
	// printf("fff ET=%f q2pr=%f highCut=%f passed=%d\n",ET, q2pt,highCut,fabs(q2pt)<highCut);
	if( ET>par_myET && ET<par_QPThighET1 && fabs(q2pt)>highCut) continue;
      }

      if(ET>par_myET) {
	hA[0]->Fill("Qhigh",1.);
	if(p_Q>0) hA[0]->Fill("Q +",1.);
	else  hA[0]->Fill("Q -",1.);
      }

     
      hA[10+iQ]->Fill(ET);
      if(ET>25 &&ET<50 ) hA[12+iQ]->Fill(spin4);
      if(ET>32 &&ET<44 ) hA[14+iQ]->Fill(spin4);
     
      hA[18+iQ]->Fill(spin4,ET);	 
     
    } // end of loop over tracks
  }// end of loop ove vertices

}


// $Log: St2009pubSpinMaker.cxx,v $
// Revision 1.12  2010/05/01 01:31:45  balewski
// added W->JJ code & JES calibration
//
// Revision 1.11  2010/04/14 20:00:08  balewski
// added AL w/o endcap
//
// Revision 1.10  2010/03/22 17:18:32  balewski
// now the < > sign is right
//
// Revision 1.9  2010/03/22 16:11:42  balewski
// better computation of AL(QCD)
//
// Revision 1.8  2010/03/20 19:19:05  balewski
// added ability to drop Q/PT cut for spin analysis
//
// Revision 1.7  2010/03/15 17:05:46  balewski
// cleanup, used for W AL sort March 15, 2010
//
// Revision 1.6  2010/03/14 22:50:31  balewski
// *** empty log message ***
//
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
