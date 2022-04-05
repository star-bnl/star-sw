// $Id: St2009W_accessMuDst.cxx,v 1.18 2011/09/14 14:23:20 stevens4 Exp $
//
//*-- Author : Jan Balewski, MIT
//*-- Author for Endcap: Justin Stevens, IUCF

//MuDst
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuTriggerIdCollection.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StMuDSTMaker/COMMON/StMuTrack.h>
#include <StMuDSTMaker/COMMON/StMuPrimaryVertex.h>

#include "StEmcRawMaker/defines.h"
#include "StEmcUtil/database/StBemcTables.h"

#include "StEEmcUtil/database/StEEmcDb.h"   
#include "StEEmcUtil/database/EEmcDbItem.h"  

#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

#include "St2009WMaker.h"
//--------------------------------------
//--------------------------------------
int  
St2009WMaker::accessTrig(){ // return non-zero on abort 

  if (isMC){
    /*
      When the trigger emulator is ready, this should hook into that
      instead of the two functions used below.  For now, check that is passes both
      L0 and L2, and set the l2bitET flag to true if so.
    */
    
    if (!passes_L0()) return -1;
    hA[0]->Fill("BHT3Id",1.);
    if(!passes_L2()) return -2;
    hA[0]->Fill("L2wId",1.);

    if(isMC>300){
      StMuEvent* muEve = mMuDstMaker->muDst()->event();
      StL0Trigger *trig=&(muEve->l0Trigger());
      wEve.bx48=trig->bunchCrossingId();
      wEve.bx7=trig->bunchCrossingId7bit(par_inpRunNo);
    }

    wEve.l2bitET=true;
    return 0; // we haven't set everything, but it should be good enough for simu.
  }

  StMuEvent* muEve = mMuDstMaker->muDst()->event();

  //collect info for the luminosity monitor
  int highestT=0;
  int highestM=0;  
  for (int m=0;m<300;m++)
    {
      int myT=muEve->emcTriggerDetector().highTower(m);
      if  (myT>highestT)
        {
          highestT=myT;
          highestM=m;
        }
    }
  int highestPhi, tempPhi, tempEta;
  int awaySum[16];
  int totalSum=0;
  for (int i=0;i<16;i++) awaySum[i]=0;

  patchToEtaPhi(highestM,&tempEta,&highestPhi);
  
  for (int m=0;m<300;m++)
    {
      int myT=muEve->emcTriggerDetector().highTower(m);
      patchToEtaPhi(m,&tempEta,&tempPhi);
      for (int away_width=0;away_width<16;away_width++)
        if ((highestPhi+30-tempPhi)%30>(15-away_width) && (highestPhi+30-tempPhi)%30<(15+away_width))
          {
            //printf("==> adding %d to awaySum",myT);
            awaySum[away_width]+=myT;
          }
      totalSum+=myT;
    }
  for (int i=0;i<16;i++)  wEve.trigAwaySum[i]=awaySum[i];
  wEve.trigTotalSum=totalSum;
  //

  StMuTriggerIdCollection *tic=&(muEve->triggerIdCollection());
  assert(tic);
  const StTriggerId &l1=tic->l1();
  vector<unsigned int> idL=l1.triggerIds();
  
  //  printf("nTrig=%d, trigID: ",idL.size());
  for(unsigned int i=0;i<idL.size(); i++){
    char txt[100];
    sprintf(txt,"%d",idL[i]);
    hA[1]->Fill(txt,1.);
  }
  //  printf("\n");

  if(!tic->nominal().isTrigger(par_bht3TrgID)) return -1;
  hA[0]->Fill("BHT3Id",1.);
  if(!tic->nominal().isTrigger(par_l2wTrgID)) return -2;
  hA[0]->Fill("L2wId",1.);

  TArrayI& l2Array = muEve->L2Result();
  LOG_DEBUG <<Form("AccessL2Decision() from regular muDst: L2Ar-size=%d",l2Array.GetSize())<<endm;
  unsigned int *l2res=(unsigned int *)l2Array.GetArray();
  //printf(" L2-jet online results below:\n");
  const int BEMCW_off=20; // valid only for 2009 run
  //int k;  for (k=0;k<32;k++) printf("k=%2d  val=0x%04x\n",k,l2res[k]);
  wEve.l2algo= ( L2wResult2009 *) &l2res[BEMCW_off];
  // L2wResult2009_print(wEve.l2algo);
  wEve.l2bitET=(wEve.l2algo->trigger&2)>0;  // bit1=ET>thr
  wEve.l2bitRnd=(wEve.l2algo->trigger&1)>0; // bit0=rnd,
  StL0Trigger *trig=&(muEve->l0Trigger());
  wEve.bx48=trig->bunchCrossingId();
  wEve.bx7=trig->bunchCrossingId7bit(mRunNo);

  if( (wEve.l2bitRnd || wEve.l2bitET)==0) return -3; // L2W-algo did not accept this event
  hA[0]->Fill("L2wBits",1.); // confirmation bits were set properly


  if(wEve.l2bitRnd) {
    hA[0]->Fill("L2wRnd",1.);
    hA[61]->Fill(wEve.bx7);
    for (int m=0;m<300;m++){
      int val=muEve->emcTriggerDetector().highTower(m);
      hA[7]->Fill(val);
    }
  }

  if(!wEve.l2bitET)  return -3; // drop L2W-random accepts
  if(wEve.l2bitET) hA[0]->Fill("L2wET",1.);

  //.... only monitor below ....
  hA[2]->Fill(wEve.bx48);
  hA[3]->Fill(wEve.bx7);

  // access L0-HT data
  for (int m=0;m<300;m++)	{
    int val=muEve->emcTriggerDetector().highTower(m);
    if(wEve.l2bitET) hA[6]->Fill(val);
    if(val<par_DsmThres) continue;
    if(wEve.l2bitET) hA[8]->Fill(m);
    //printf("Fired L0 HT m=%d val=%d\n",m,val);
  }
  return 0;
}


//--------------------------------------
//--------------------------------------
int
St2009WMaker::accessVertex(){ // return non-zero on abort 
  int nInpPrimV=mMuDstMaker->muDst()->numberOfPrimaryVertices();
  if(nInpPrimV <par_minPileupVert) return -1;
  hA[0]->Fill("tpcOn",1.);

  int nVer=0;
  for(int iv=0;iv<nInpPrimV;iv++) {
    StMuPrimaryVertex* V= mMuDstMaker->muDst()->primaryVertex(iv);
    assert(V);
    mMuDstMaker->muDst()->setVertexIndex(iv);
    float rank=V->ranking(), funnyR=999;
    if(rank>1e6)  funnyR=log(rank-1e6)+10;
    else if(rank>0)   funnyR=log(rank);
    else   funnyR=log(rank+1e6)-10;
    hA[10]->Fill(funnyR);
    if (rank<=0) continue;
    const StThreeVectorF &r=V->position();
    //   StThreeVectorF &er=V->posError();
    hA[11]->Fill(r.z());

    //add vertex weighting here to get quick turnaround 
    if(isMC>=350)
      hA[190]->Fill(r.z(),hReweight->GetBinContent(hReweight->FindBin(r.z())));

    nVer++; // count valid vertices
    if(fabs(r.z()) > par_vertexZ) continue;
    WeveVertex wv;
    wv.id=iv;
    wv.z=r.z();
    wv.funnyRank=funnyR;
    wEve.vertex.push_back(wv);
  }
  if(nVer<=0) return -2;
  hA[0]->Fill("primVert",1.);
  hA[4]->Fill(wEve.bx48);
  hA[5]->Fill(wEve.bx7);

  // access L0-HT data
  StMuEvent* muEve = mMuDstMaker->muDst()->event();
  for (int m=0;m<300;m++)	{
    int val=muEve->emcTriggerDetector().highTower(m);
    if(val<par_DsmThres) continue;
    if(wEve.l2bitET) hA[9]->Fill(m);
  }

  hA[12]->Fill(wEve.vertex.size());
  if(wEve.vertex.size()<=0) return -3;
  hA[0]->Fill("vertZ",1.);
  return 0;
}



//--------------------------------------
//--------------------------------------
int  
St2009WMaker::accessTracks(){ // return non-zero on abort 
  int nTrOK=0;
  // printf("\n nInp=%d eveID=%d nPVer=%d nAnyV= %d\n",nInpEve,mMuDstMaker->muDst()->event()->eventId(),wEve.vertex.size(),mMuDstMaker->muDst()->numberOfPrimaryVertices());
  for(uint iv=0;iv<wEve.vertex.size(); iv++) {
    uint vertID=wEve.vertex[iv].id;
    assert(vertID<mMuDstMaker->muDst()->numberOfPrimaryVertices());
    assert(vertID>=0);
    StMuPrimaryVertex* V= mMuDstMaker->muDst()->primaryVertex(vertID);
    assert(V);
    //int nTrUse= V->nTracksUsed();
    mMuDstMaker->muDst()->setVertexIndex(vertID);
    float rank=V->ranking();
    assert(rank>0);
    Int_t nPrimTrAll=mMuDstMaker->muDst()->GetNPrimaryTrack();
    for(int itr=0;itr<nPrimTrAll;itr++) {
      StMuTrack *prTr=mMuDstMaker->muDst()->primaryTracks(itr);
      if(prTr->flag()<=0) continue;
      const StMuTrack *glTr=prTr->globalTrack();
      if(glTr==0) continue; // see the reason at the end of this method
      if(prTr->flag()!=301) continue;// TPC+prim vertex tracks
      hA[20]->Fill("101",1.); 
      float pt=prTr->pt();
      if(pt<1.0) continue;
      hA[20]->Fill("pt1",1.);

      // TPC  sector dependent filter
      StThreeVectorF ro=glTr->lastPoint();
      int secID=WtpcFilter::getTpcSec(ro.phi(),ro.pseudoRapidity());
      if ( mTpcFilter[secID-1].accept(prTr)==false) continue;

      if (secID==20) continue; //remove poorly calibrated sector
      
      //accepted ......

      hA[21]->Fill(prTr->nHitsFit());
      float hitFrac=1.*prTr->nHitsFit()/prTr->nHitsPoss();
      hA[22]->Fill(hitFrac);
      hA[23]->Fill(glTr->firstPoint().perp());
      hA[24]->Fill(ro.perp());

      hA[25]->Fill(glTr->p().perp());
      if(glTr->charge()<0) hA[27]->Fill(glTr->p().perp());

      hA[29]->Fill(pt);
      if(prTr->charge()<0)hA[30]->Fill(pt);

      hA[26]->Fill(ro.pseudoRapidity(),ro.phi());
      if(pt>5) //estimate TPC inefficiency in data
        hA[57]->Fill(ro.pseudoRapidity(),ro.phi());

      /* Victor: in reality mChiSqXY is a normal Xi2 for track and
       mChiSqZ is Xi2 of fit to  primary vertex
      */
      float globChi2dof=glTr->chi2();
      hA[35]->Fill(globChi2dof);
      {// monitor chi2 for east/west TPC separately
	StThreeVectorF ri=glTr->firstPoint();
	if(ri.z()>0 && ro.z()>0)  hA[58]->Fill(globChi2dof);
	if(ri.z()<0 && ro.z()<0)  hA[59]->Fill(globChi2dof);
      }

      hA[36]->Fill(globChi2dof,ro.pseudoRapidity());

      float dedx=prTr->dEdx()*1e6;
      //printf("%f %f\n",glTr->p().mag(),dedx); 
      hA[28]->Fill(prTr->p().mag(),dedx);

      if(pt<par_trackPt) continue;
      hA[20]->Fill("ptOK",1.);
      nTrOK++;
      WeveEleTrack wTr;

      wTr.prMuTrack=prTr;
      wTr.glMuTrack=glTr;
      StThreeVectorF prPvect=prTr->p();
      wTr.primP=TVector3(prPvect.x(),prPvect.y(),prPvect.z());

      wEve.vertex[iv].eleTrack.push_back(wTr);
    }// loop over tracks
  }// loop over vertices
  if(nTrOK<=0) return -1;
  hA[0]->Fill("Pt10",1.);
  return 0;
}

/* from Pibero:
   It looks like your global track is null. See this post:

   http://www.star.bnl.gov/HyperNews-star/get/mudst/53.html

   My reading of this hypernews says its just the way ITTF/MuDst
   works. You can get a good primary track, but its global track
   fails the chi2 fit. So the primary track is kept in the MuDst
   but the global track is dropped. I would suggest you skip those
   rare primary tracks that have no global tracks, that way you
   still use most of the tracks in the MuDst. You don't need to
   skip the entire event, just that track. I guess the down side
   is you couldn't make a global DCA cut on those rare tracks, right?
   I guess you could also request S&C to change ITTF/MuDst not to drop
   the global track for every good primary track regardless of chi2.
*/

  

/* $STAR/StRoot/StEvent/StTrack.h
 *  mFlag=zxyy, where  z = 1 for pile up track in TPC (otherwise 0) 
 *                     x indicates the detectors included in the fit and 
 *                    yy indicates the status of the fit. 
 *  Positive mFlag values are good fits, negative values are bad fits. 
 *
 *  The first digit indicates which detectors were used in the refit: 
 *
 *      x=1 -> TPC only 
 *      x=3 -> TPC       + primary vertex 
 *      x=5 -> SVT + TPC 
 *      x=6 -> SVT + TPC + primary vertex 
 *      x=7 -> FTPC only 
 *      x=8 -> FTPC      + primary 
 *      x=9 -> TPC beam background tracks            
 *
 *  The last two digits indicate the status of the refit: 
 *       = +x01 -> good track 
 *
 *      = -x01 -> Bad fit, outlier removal eliminated too many points 
 *      = -x02 -> Bad fit, not enough points to fit 
 *      = -x03 -> Bad fit, too many fit iterations 
 *      = -x04 -> Bad Fit, too many outlier removal iterations 
 *      = -x06 -> Bad fit, outlier could not be identified 
 *      = -x10 -> Bad fit, not enough points to start 
 *
 *      = +x11 -> Short track pointing to EEMC
*/


//________________________________________________
//________________________________________________
int
St2009WMaker::accessBTOW(){
  
  StMuEmcCollection* emc = mMuDstMaker->muDst()->muEmcCollection();
  if (!emc) {
    gMessMgr->Warning() <<"No EMC data for this event"<<endm;    return -4;
  }
    
  int ibp=kBTow; // my index for tower & preshower set to BTOW
  int jBP=BTOW; // official BTOW detector ID
  int n5=0,n0=0,n1=0,n2=0,n3=0,n4=0;
  int maxID=0;
  double maxADC=0,adcSum=0;
  for (int softID=1; softID <=mxBtow ; softID++) {
    float rawAdc= emc->getTowerADC(softID);
    if(rawAdc==0) n0++;

    int statPed,statOfl,statGain;
    mBarrelTables->getStatus(jBP, softID, statPed,"pedestal");
    mBarrelTables->getStatus(jBP, softID, statOfl);
    mBarrelTables->getStatus(jBP, softID, statGain,"calib");
    
    if(statPed!=1) {
      wEve.bemc.statTile[ibp][softID-1]=1;
      n1++; continue;}
    if(statOfl!=1) {
      wEve.bemc.statTile[ibp][softID-1]=2;
      n2++; continue;} 
    if(statGain!=1) {
      wEve.bemc.statTile[ibp][softID-1]=4;
      n3++; continue;} 

    wEve.bemc.statTile[ibp][softID-1]=0 ; 


    float ped,sigPed,gain;
    int capID=0;// just one value for btow
    mBarrelTables->getPedestal(jBP,softID,capID,ped,sigPed); 
    mBarrelTables->getCalib(jBP, softID, 1, gain);
    if (use_gains_file == 1) {
      gain = gains_BTOW[softID];
    }
    //printf("id=%d gain=%f\n",softID,gain);

    //method for shifting energy scale 
    gain=gain*par_btowScale;//(default is par_btowScale=1)

    float adc=rawAdc-ped;
    if(adc>0) n4++;
    if(adc<par_kSigPed*sigPed) continue;
    if(adc<par_AdcThres) continue;
    n5++;
    wEve.bemc.adcTile[ibp][softID-1]=adc;
    wEve.bemc.eneTile[ibp][softID-1]=adc*gain;

    if(maxADC<adc) { maxID=softID; maxADC=adc;}
    adcSum+=adc;
  }

  if(isMC>0 && isMC<20) assert(n1==0);//prevent using real peds for private MC
  //printf("NNN %d %d %d %d %d %d id=%d\n",n0,n1,n2,n3,n4,n5,maxID);
  if(n0==mxBtow) return -1 ;  // BTOW was not present in this events

  wEve.bemc.tileIn[ibp]=1; //tag usable data
  
  if(nInpEve%5000==1) { 
    LOG_INFO << Form("unpackMuBTOW() dataIn=%d, nBbad: ped=%d stat=%d gain=%d ; nAdc: %d>0, %d>thres\n    maxADC=%.0f softID=%d adcSum=%.0f",
		     wEve.bemc.tileIn[ibp],n1,n2,n3,n4,n5,
		     maxADC,maxID,adcSum
		     )<<endm;
  }
  hA[31]->Fill(maxADC);
  hA[32]->Fill(adcSum);

  if(maxADC<par_maxADC)  return -2 ;  // not enough energy
  
  return 0;
}


//________________________________________________
//________________________________________________
int
St2009WMaker::accessETOW(){ 
  
  StMuEmcCollection* emc = mMuDstMaker->muDst()->muEmcCollection();
  if (!emc) {
    LOG_WARN <<"No EMC data for this event"<<endm;    return -4;
  } 
  
  //loop over all towers
  for (int i=0; i< emc->getNEndcapTowerADC(); i++) {
    int sec,eta,sub,rawAdc; //muDst  ranges:sec:1-12, sub:1-5, eta:1-12
    emc->getEndcapTowerADC(i,rawAdc,sec,sub,eta);
    
    const EEmcDbItem *x=mDbE->getTile(sec,'A'+sub-1,eta,'T');
    assert(x); // it should never happened for muDst
    if(x->fail ) continue; // drop not working channels
    int isec=x->sec-1;
    int isub=x->sub-'A';
    int ieta=x->eta-1;
    
    assert(isec>=0 && isec<mxEtowSec); // check input is ok
    assert(isub>=0 && isub<mxEtowSub);
    assert(ieta>=0 && ieta<mxEtowEta);
    
    float adc=rawAdc-x->ped; // ped subtracted ADC
    if(adc<par_kSigPed*x->sigPed) continue;
  
    wEve.etow.adc[isec*mxEtowSub+isub][ieta]=adc;
    
    if(x->gain<=0) continue;// drop channels w/o gains
    float ene=adc/x->gain;
    
    //method for shifting energy scale 
    ene*=par_etowScale;//(default is par_etowScale=1)
    wEve.etow.ene[isec*mxEtowSub+isub][ieta]=ene;
    wEve.etow.stat[isec*mxEtowSub+isub][ieta]=0;
  
  }
  
  return 0;
}

//________________________________________________
//________________________________________________
float
St2009WMaker::sumTpcCone(int vertID, TVector3 refAxis, int flag, int &nTrCnt){ 

  // flag=2 use 2D cut, 1= only delta phi

  // printf("******* sumTpcCone, flag=%d eveId=%d vertID=%d  eta0=%.2f phi0/rad=%.2f  \n",flag,wEve.id,vertID,refAxis.PseudoRapidity() ,refAxis.Phi());

  int nTR=0;
  assert(vertID>=0);
  assert(vertID<(int)mMuDstMaker->muDst()->numberOfPrimaryVertices());
  
  StMuPrimaryVertex* V= mMuDstMaker->muDst()->primaryVertex(vertID);
  assert(V);
  mMuDstMaker->muDst()->setVertexIndex(vertID);
  float rank=V->ranking();
  assert(rank>0);
  double ptSum=0;
  Int_t nPrimTrAll=mMuDstMaker->muDst()->GetNPrimaryTrack();
  for(int itr=0;itr<nPrimTrAll;itr++) {
    StMuTrack *prTr=mMuDstMaker->muDst()->primaryTracks(itr);
    if(prTr->flag()<=0) continue;
    if(prTr->flag()!=301) continue;// TPC-only regular tracks
    float hitFrac=1.*prTr->nHitsFit()/prTr->nHitsPoss();
    if(hitFrac<par_nHitFrac) continue;
    StThreeVectorF prPvect=prTr->p();
    TVector3 primP=TVector3(prPvect.x(),prPvect.y(),prPvect.z());
    // printf(" prTrID=%4d  prTrEta=%.3f prTrPhi/deg=%.1f prPT=%.1f  nFitPts=%d\n", prTr->id(),prTr->eta(),prTr->phi()/3.1416*180.,prTr->pt(),prTr->nHitsFit());
    if(flag==1) {
      float deltaPhi=refAxis.DeltaPhi(primP);
      if(fabs(deltaPhi)> par_awayDeltaPhi) continue;
    }
    if(flag==2 || flag==3) {
      float deltaR=refAxis.DeltaR(primP);
      //printf("delR=%.3f\n",deltaR);
      if(flag==2 && deltaR>par_nearDeltaR) continue;
      if(flag==3 && deltaR>par_smallNearDeltaR) continue;
    }
    float pT=prTr->pt();
    //    printf(" passed pt=%.1f\n",pT);
    if(pT>par_countTrPt) nTR++; //count tracks in "jet"
    if(pT>par_trackPt) ptSum+=par_trackPt;
    else  ptSum+=pT;
  }
  //printf("TPCJet sum: nTR=%d, ptSum=%.1f\n", nTR,ptSum);
  nTrCnt=nTR; 
  return ptSum;
}



//________________________________________________
void
St2009WMaker::accessBSMD(){
  const char cPlane[ mxBSmd]={'E','P'};
  // Access to muDst .......................
  StMuEmcCollection* emc = mMuDstMaker->muDst()->muEmcCollection();
  if (!emc) {
    gMessMgr->Warning() <<"No EMC data for this muDst event"<<endm;    return;
  }

  //....................... B S M D .........................
  for(int iEP=bsmde; iEP<=bsmdp;iEP++) { // official BSMD plane IDs
    int iep=iEP-3; assert(bsmde==3);// what a hack
    int nh= emc->getNSmdHits(iEP);
    //printf("muDst BSMD-%c nHit=%d\n",cPlane[iep],nh);
    int n5=0,n1=0,n2=0,n3=0,n4=0;
    for (int i=0; i < nh; i++) {
      StMuEmcHit *hit=emc->getSmdHit(i,iEP);
      float  adc=hit->getAdc();
      int softID=hit->getId();

      int statPed,statOfl,statGain;
      mBarrelTables->getStatus(iEP, softID, statPed,"pedestal");
      mBarrelTables->getStatus(iEP, softID, statOfl);
      mBarrelTables->getStatus(iEP, softID, statGain,"calib");
      
      if(statPed!=1) {
	wEve.bemc.statBsmd[iep][softID-1]=1;
	n1++; continue;}
      if(statOfl!=1) {
	wEve.bemc.statBsmd[iep][softID-1]=2;
	n2++; continue;} 
      if(statGain<1 || statGain>19) {
	wEve.bemc.statBsmd[iep][softID-1]=4;
	n3++; continue;} 
      
      float pedRes,sigPed,gain;
      int capID=0;// just one value for ped residua in pp500, 2009 run
      mBarrelTables->getPedestal(iEP,softID,capID,pedRes,sigPed); 
      mBarrelTables->getCalib(iEP, softID, 1, gain);
      
      if(isMC) { // overwrite it based on genat DE & private calibration
	float par_bsmdAbsGain=6e6;// tmp arbitrary absolute calib of bsmd, was 3e6
        float  de = hit->getEnergy();// Geant energy deposit (GeV)
        adc=de*par_bsmdAbsGain;
      } else { // correct for pedestal residua
	adc-=pedRes;
	if(adc>0) n4++;
	if(adc<par_kSigPed*sigPed) continue;
	adc*=gain; // use Willie's relative gains for run9 data
      }
      
      n5++;
      assert(softID>=1);      assert(softID<=mxBStrips);
      int id0=softID-1;
      wEve.bemc.adcBsmd[ iep][id0]=adc;
      hA[70+10*iep]->Fill(adc);
      
      //if(nInpEve<3 || i <20 )printf("  i=%d, smd%c id=%d, m=%d adc=%.3f pedRes=%.1f, sigP=%.1f stat: O=%d P=%d G=%d  gain=%.2f\n",i,cPlane[iep],softID,1+id0/150,adc,pedRes,sigPed, statOfl,statPed,statGain, gain);
    }// end of hit list
    if(nTrigEve%5000==1) { 
      LOG_INFO << Form("unpackMuBSMD-%c() nBbad: ped=%d stat=%d gain=%d ; nAdc: %d>0, %d>thres", cPlane[iep],n1,n2,n3,n4,n5)<<endm;
    }
  }// end of E-, P-plane loop

}


//________________________________________________
//________________________________________________
void
St2009WMaker::hadronicRecoil(){ //add up all vector pt outside of 'nearJet' region to get 'hadronic recoil' pt vector 

  for(uint iv=0;iv<wEve.vertex.size();iv++) {
    WeveVertex &V=wEve.vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.isMatch2Cl==false) continue;
      
      TVector3 recoil;
      
      //.... process BTOW hits
      for(int i=0;i< mxBtow;i++) {
	float ene=wEve.bemc.eneTile[kBTow][i];
	if(ene<=0) continue;
	TVector3 primP=positionBtow[i]-TVector3(0,0,V.z);
	primP.SetMag(ene); // it is 3D momentum in the event ref frame
	float deltaR=T.primP.DeltaR(primP);        
	if(deltaR< par_nearDeltaR) continue;
	recoil+=primP;
      }

      //....process ETOW hits
      for(int iphi=0; iphi<mxEtowPhiBin; iphi++){
	for(int ieta=0; ieta<mxEtowEta; ieta++){
	  float ene=wEve.etow.ene[iphi][ieta];
	  if(ene<=0) continue; //skip towers with no energy
	  TVector3 primP=positionEtow[iphi][ieta]-TVector3(0,0,V.z);
	  primP.SetMag(ene); // it is 3D momentum in the event ref frame
	  float deltaR=T.primP.DeltaR(primP);        
	  if(deltaR< par_nearDeltaR) continue;
	  recoil+=primP;
	}
      }    

      //....process TPC tracks
      int vertID=V.id;
      assert(vertID>=0);
      assert(vertID<(int)mMuDstMaker->muDst()->numberOfPrimaryVertices());
      
      StMuPrimaryVertex* V= mMuDstMaker->muDst()->primaryVertex(vertID);
      assert(V);	  
      mMuDstMaker->muDst()->setVertexIndex(vertID);
      float rank=V->ranking();
      assert(rank>0);
      Int_t nPrimTrAll=mMuDstMaker->muDst()->GetNPrimaryTrack();
      for(int itr=0;itr<nPrimTrAll;itr++) {
	StMuTrack *prTr=mMuDstMaker->muDst()->primaryTracks(itr);
	if(prTr->flag()<=0) continue;
	if(prTr->flag()!=301) continue;// TPC-only regular tracks
	float hitFrac=1.*prTr->nHitsFit()/prTr->nHitsPoss();
	if(hitFrac<par_nHitFrac) continue;
	StThreeVectorF prPvect=prTr->p();
	TVector3 primP=TVector3(prPvect.x(),prPvect.y(),prPvect.z());
	float deltaR=T.primP.DeltaR(primP);
	if(deltaR< par_nearDeltaR) continue;
	if(primP.Perp()<0.15) continue; //lower threshold on pT < 150 MeV
	recoil+=primP;	
      }
      
      T.hadronicRecoil=recoil;

    }
  }
}

//$Log: St2009W_accessMuDst.cxx,v $
//Revision 1.18  2011/09/14 14:23:20  stevens4
//update used for cross section PRD paper
//
//Revision 1.17  2010/12/02 18:31:43  rcorliss
//updated lumi code to match the starnote version
//
//Revision 1.16  2010/11/09 23:06:46  balewski
//small change
//
//Revision 1.14  2010/06/30 19:00:03  rcorliss
//passes_L0() now works for simulation, using trigger simu in new macro
//
//Revision 1.13  2010/05/21 19:57:59  stevens4
//remove L0 check for MC until passes_L0() is fixed
//
//Revision 1.12  2010/03/23 15:33:55  seelej
//Edit to files to allow the use of a text file for the gains instead of using the DB.
//
//Revision 1.11  2010/02/18 22:34:50  stevens4
//add tpc effic study and allow energy scaling for data and MC
//
//Revision 1.10  2010/01/29 01:56:01  stevens4
//disable lepton track reco in TPC sector 20
//
//Revision 1.9  2010/01/23 20:07:03  stevens4
//fix use of real btow peds for rcf mc
//
//Revision 1.8  2010/01/23 20:01:00  stevens4
//fix real peds for rcf mc
//
//Revision 1.7  2010/01/23 02:35:38  stevens4
//add ability to scale jet et and use real btow peds for rcf mc
//
//Revision 1.6  2010/01/18 03:26:15  balewski
//expanded TPC track filtering, not finished
//
//Revision 1.5  2010/01/13 03:34:20  stevens4
//give trig emulator access to barrel hits
//
//Revision 1.4  2010/01/06 19:16:47  stevens4
//track cuts now on primary component, cleanup
//
//Revision 1.3  2010/01/05 03:22:55  balewski
//change logic for filling btow status tables, added printout to Z-code
//
//Revision 1.2  2009/12/08 04:48:35  balewski
//*** empty log message ***
//
//Revision 1.1  2009/11/23 23:00:18  balewski
//code moved spin-pool
//
