// $Id: St2011W_acessMuDst.cxx,v 1.17 2016/01/08 02:08:49 jlzhang Exp $
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

#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"

#include "St2011WMaker.h"
//--------------------------------------
//--------------------------------------
int  
St2011WMaker::accessBarrelTrig(){ // return non-zero on abort 

  if (isMC){

	/*
	   When the trigger emulator is ready, this should hook into that
	   instead of the two functions used below.  For now, check that is passes both
	   L0 and L2, and set the l2bitET flag to true if so.
	   */

	//if (!passes_L0()) return -1;
	if(!passes_L2()) return -2;
	hA[0]->Fill("L2bwET",1.);

	wEve->l2bitET=true;
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
  for (int i=0;i<16;i++)  wEve->trigAwaySum[i]=awaySum[i];
  wEve->trigTotalSum=totalSum;

  StMuTriggerIdCollection *tic=&(muEve->triggerIdCollection());
  assert(tic);
  const StTriggerId &l1=tic->l1();
  vector<unsigned int> idL=l1.triggerIds();

  //printf("nTrig=%d, trigID: ",idL.size());
  for(unsigned int i=0;i<idL.size(); i++){
	char txt[100];
	sprintf(txt,"%d",idL[i]);
	//printf("%d, ",idL[i]);
	hA[1]->Fill(txt,1.);
  }
  //printf("\n isTrg=%d trgId=%d\n",tic->nominal().isTrigger(par_l2bwTrgID),par_l2bwTrgID);

  //get bX info
  StL0Trigger *trig=&(muEve->l0Trigger());
  wEve->bx48=trig->bunchCrossingId();
  wEve->bx7=trig->bunchCrossingId7bit(mRunNo);

  // store spin info 
  int bxStar48=-2, bxStar7=-2, spin4=-2;
  if(spinDb && spinDb->isValid() &&  // all 3 DB records exist 
	  spinDb->isPolDirLong()) {  // you do not want mix Long & Trans by accident
	bxStar48= spinDb->BXstarUsingBX48(wEve->bx48);
	bxStar7=spinDb->BXstarUsingBX7(wEve->bx7);
	spin4=spinDb->spin4usingBX48(wEve->bx48); 
  }
  wEve->bxStar48=bxStar48;
  wEve->bxStar7=bxStar7;
  wEve->spin4=spin4;

  //check trigger ID
  if(!tic->nominal().isTrigger(par_l2bwTrgID)) return -2;
  hA[0]->Fill("L2bwId",1.);

  TArrayI& l2Array = muEve->L2Result();
  LOG_DEBUG <<Form("AccessL2Decision() from regular muDst: L2Ar-size=%d",l2Array.GetSize())<<endm;
  unsigned int *l2res=(unsigned int *)l2Array.GetArray();
  const int BEMCW_off=20; // valid only for 2009 & 2011 run
  L2wResult2009 *l2algo= ( L2wResult2009 *) &l2res[BEMCW_off];

  wEve->l2bitET=(l2algo->trigger&2)>0;  // bit1=ET>thr
  wEve->l2bitRnd=(l2algo->trigger&1)>0; // bit0=rnd,

  if( (wEve->l2bitRnd || wEve->l2bitET)==0) return -3; // L2W-algo did not accept this event
  hA[0]->Fill("L2bwBits",1.); // confirmation bits were set properly

  if(wEve->l2bitRnd) {
	hA[0]->Fill("L2bwRnd",1.);
	for (int m=0;m<300;m++){
	  int val=muEve->emcTriggerDetector().highTower(m);
	  hA[7]->Fill(val);
	}
	hA[61]->Fill(wEve->bx7);
  }

  if(!wEve->l2bitET)  return -3; // drop L2W-random accepts
  if(wEve->l2bitET) hA[0]->Fill("L2bwET",1.);

  //.... only monitor below ....
  hA[2]->Fill(wEve->bx48);
  hA[3]->Fill(wEve->bx7);

  // access L0-HT data
  int mxVal=-1;
  for (int m=0;m<300;m++)	{
	int val=muEve->emcTriggerDetector().highTower(m);
	if(mxVal<val) mxVal=val;
	if(wEve->l2bitET) hA[6]->Fill(val);
	if(val<par_DsmThres) continue;
	if(wEve->l2bitET) hA[8]->Fill(m);
	//printf("Fired L0 HT m=%d val=%d\n",m,val);
  }
  wEve->bemc.maxHtDsm=mxVal;
  return 0;
}


//--------------------------------------
//--------------------------------------
int
St2011WMaker::accessVertex(){ // return non-zero on abort 
  int nInpPrimV=mMuDstMaker->muDst()->numberOfPrimaryVertices();

  if(nInpPrimV <par_minPileupVert) return -1;
  //separate histos for barrel and endcap triggers
  if(wEve->l2bitET) hA[0]->Fill("tpcOn",1.);
  if(wEve->l2EbitET) hE[0]->Fill("tpcOn",1.);

  int nVer=0; int nVerR=0;
  for(int iv=0;iv<nInpPrimV;iv++) {
	StMuPrimaryVertex* V= mMuDstMaker->muDst()->primaryVertex(iv);
	assert(V);
	mMuDstMaker->muDst()->setVertexIndex(iv);
	float rank=V->ranking(), funnyR=999;
	if(rank>1e6)  funnyR=log(rank-1e6)+10;
	else if(rank>0)   funnyR=log(rank);
	else   funnyR=log(rank+1e6)-10;
	if(wEve->l2bitET) hA[10]->Fill(funnyR);
	if(wEve->l2EbitET) hE[10]->Fill(funnyR);
	if (rank<=0) continue; 
	const StThreeVectorF &r=V->position();
	//   StThreeVectorF &er=V->posError();
	if(wEve->l2bitET) hA[11]->Fill(r.z());
	if(wEve->l2EbitET) hE[11]->Fill(r.z());
	nVer++; // count valid vertices
	if(fabs(r.z()) > par_vertexZ) continue;
	if(rank>0) nVerR++; //count vertices with rank>0
	WeveVertex wv;
	wv.id=iv;
	wv.z=r.z();
	wv.rank=rank;
	wv.funnyRank=funnyR;
	wv.nEEMCMatch=V->nEEMCMatch();
	wEve->vertex.push_back(wv);
  }
  if(nVer<=0) return -2;
  if(wEve->l2bitET) {
	hA[0]->Fill("primVert",1.);
	hA[4]->Fill(wEve->bx48);
	hA[5]->Fill(wEve->bx7);
  }
  if(wEve->l2EbitET) {
	hE[0]->Fill("primVert",1.);
	hE[4]->Fill(wEve->bx48);
	hE[5]->Fill(wEve->bx7);
  }

  // access L0-HT data
  StMuEvent* muEve = mMuDstMaker->muDst()->event();
  for (int m=0;m<300;m++)	{
	int val=muEve->emcTriggerDetector().highTower(m);
	if(val<par_DsmThres) continue;
	if(wEve->l2bitET && nVerR>0) hA[9]->Fill(m);
  }
  for (int m=0;m<90;m++)	{
	int val=muEve->emcTriggerDetector().highTowerEndcap(m);
	if(val<parE_DsmThres) continue;
	if(wEve->l2EbitET) hE[9]->Fill(m);
  }

  if(wEve->l2bitET) hA[12]->Fill(nVerR);
  if(wEve->l2EbitET) hE[12]->Fill(nVer);

  if(wEve->vertex.size()<=0) return -3;
  if(wEve->l2bitET && nVerR>0) hA[0]->Fill("vertZ",1.);
  if(wEve->l2EbitET) hE[0]->Fill("vertZ",1.);
  return 0;
}



//--------------------------------------
//--------------------------------------
int  
St2011WMaker::accessTracks(){ // return non-zero on abort 
  int nTrOK=0;
  // printf("\n nInp=%d eveID=%d nPVer=%d nAnyV= %d\n",nInpEve,mMuDstMaker->muDst()->event()->eventId(),wEve->vertex.size(),mMuDstMaker->muDst()->numberOfPrimaryVertices());
  for(uint iv=0;iv<wEve->vertex.size(); iv++) {
	uint vertID=wEve->vertex[iv].id;
	assert(vertID<mMuDstMaker->muDst()->numberOfPrimaryVertices());
	assert(vertID>=0);
	StMuPrimaryVertex* V= mMuDstMaker->muDst()->primaryVertex(vertID);
	assert(V);
	mMuDstMaker->muDst()->setVertexIndex(vertID);
	float rank=V->ranking();
	assert(rank>0);
	Int_t nPrimTrAll=mMuDstMaker->muDst()->GetNPrimaryTrack();
	for(int itr=0;itr<nPrimTrAll;itr++) {
	  StMuTrack *prTr=mMuDstMaker->muDst()->primaryTracks(itr);
	  if(prTr->flag()<=0) continue;
	  const StMuTrack *glTr=prTr->globalTrack();
	  if(glTr==0) continue; // see the reason at the end of this method
	  //keep list of all tracks for TPC cone sum in tree ana
	  wEve->vertex[iv].prTrList.push_back(prTr); 
	  StThreeVectorF ro=glTr->lastPoint();

	  // TPC+prim vertex tracks and short EEMC tracks
	  if(prTr->flag()!=301 && prTr->flag()!=311) continue;
	  if(wEve->l2bitET && prTr->flag()==301) 
		hA[20]->Fill("flag",1.); 
	  if(wEve->l2EbitET && ro.pseudoRapidity()>parE_trackEtaMin)
		hE[20]->Fill("flag",1.); 

	  float pt=prTr->pt();
	  if(pt<1.0) continue;
	  if(wEve->l2bitET && prTr->flag()==301) 
		hA[20]->Fill("pt1",1.);
	  if(wEve->l2EbitET && ro.pseudoRapidity()>parE_trackEtaMin)
		hE[20]->Fill("pt1",1.);

	  //accepted tracks......
	  float hitFrac=1.*prTr->nHitsFit()/prTr->nHitsPoss();
	  StThreeVectorF ri=glTr->firstPoint();
	  /* Victor: in reality mChiSqXY is a normal Xi2 for track and mChiSqZ is Xi2 of fit to  primary vertex   */
	  float globChi2dof=glTr->chi2();
	  float dedx=prTr->dEdx()*1e6;

	  //barrel algo track monitors
	  if(wEve->l2bitET && prTr->flag()==301){ 
		hA[21]->Fill(prTr->nHitsFit());
		hA[22]->Fill(hitFrac);
		hA[23]->Fill(ri.perp());
		hA[24]->Fill(ro.perp());

		//TPC sector dependent filter 
		int secID=WtpcFilter::getTpcSec(ro.phi(),ro.pseudoRapidity());
		if (mTpcFilter[secID-1].accept(prTr)==false) continue;
		if (secID==20) continue; //poorly calibrated sector for Run 9+11+12

		hA[25]->Fill(glTr->p().perp());
		if(glTr->charge()<0) hA[27]->Fill(glTr->p().perp());
		hA[29]->Fill(pt);
		if(prTr->charge()<0)hA[30]->Fill(pt);
		hA[26]->Fill(ro.pseudoRapidity(),ro.phi());
		if(pt>5) //estimate TPC inefficiency in data
		  hA[57]->Fill(ro.pseudoRapidity(),ro.phi());
		hA[35]->Fill(globChi2dof);
		// monitor chi2 for east/west TPC separately
		if(ri.z()>0 && ro.z()>0)  hA[58]->Fill(globChi2dof);
		if(ri.z()<0 && ro.z()<0)  hA[59]->Fill(globChi2dof);
		hA[36]->Fill(globChi2dof,ro.pseudoRapidity());
		hA[28]->Fill(prTr->p().mag(),dedx);

		if(pt>10) 
		  hA[197]->Fill(ro.pseudoRapidity(),ro.phi());
		hA[198]->Fill(ro.pseudoRapidity(),prTr->pt());
	  }

	  //endcap algo track monitors
	  if(wEve->l2EbitET && ro.pseudoRapidity()>parE_trackEtaMin){ 
		hE[20]->Fill("#eta>0.7",1.);
		hE[21]->Fill(prTr->nHitsFit());
		hE[22]->Fill(hitFrac);
		hE[23]->Fill(ri.perp());
		hE[24]->Fill(ro.perp());

		// TPC sector dependent filter 
		int secID=WtpcFilter::getTpcSec(ro.phi(),ro.pseudoRapidity());
		if ( mTpcFilterE[secID-1].accept(prTr)==false) continue;

		hE[25]->Fill(glTr->p().perp());
		if(glTr->charge()<0)hE[27]->Fill(glTr->p().perp());
		hE[29]->Fill(pt);
		if(prTr->charge()<0)hE[30]->Fill(pt);

		hE[26]->Fill(ro.pseudoRapidity(),ro.phi());
		if(pt>5) //estimate TPC inefficiency in data
		  hE[57]->Fill(ro.pseudoRapidity(),ro.phi());
		hE[35]->Fill(globChi2dof);
		hE[36]->Fill(globChi2dof,ro.pseudoRapidity());
		hE[28]->Fill(prTr->p().mag(),dedx);
	  }


	  bool barrelTrack=(wEve->l2bitET && prTr->flag()==301 && pt>par_trackPt); 
	  if(barrelTrack) hA[20]->Fill("ptOK",1.);//good barrel candidate
	  bool endcapTrack=(wEve->l2EbitET && ro.pseudoRapidity()>parE_trackEtaMin && pt>parE_trackPt); 
	  if(endcapTrack) hE[20]->Fill("ptOK",1.);//good endcap candidate

	  if(!barrelTrack && !endcapTrack) continue;

	  //keep all tracks in one container
	  nTrOK++;
	  WeveEleTrack wTr;

	  wTr.prMuTrack=prTr;
	  wTr.glMuTrack=glTr;
	  StThreeVectorF prPvect=prTr->p();
	  wTr.primP=TVector3(prPvect.x(),prPvect.y(),prPvect.z());

	  wEve->vertex[iv].eleTrack.push_back(wTr);
	}// loop over tracks
  }// loop over vertices
  if(nTrOK<=0) return -1;
  if(wEve->l2bitET) hA[0]->Fill("Pt10",1.);
  if(wEve->l2EbitET) hE[0]->Fill("Pt10",1.);
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
St2011WMaker::accessBTOW(){

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
	  wEve->bemc.statTile[ibp][softID-1]=1;
	  n1++; continue;
	}
	if(statOfl!=1) {
	  wEve->bemc.statTile[ibp][softID-1]=2;
	  n2++; continue;
	} 
	if(statGain!=1) {
	  wEve->bemc.statTile[ibp][softID-1]=4;
	  n3++; continue;
	} 

	wEve->bemc.statTile[ibp][softID-1]=0 ; 


	float ped,sigPed,gain;
	int capID=0;// just one value for btow
	mBarrelTables->getPedestal(jBP,softID,capID,ped,sigPed); 
	mBarrelTables->getCalib(jBP, softID, 1, gain);
	//printf("id=%d gain=%f\n",softID,gain);

	//method for shifting energy scale 
	gain=gain*par_btowScale;//(default is par_btowScale=1)

	float adc=rawAdc-ped;
	if(adc>0) n4++;
	if(adc<par_kSigPed*sigPed) continue;
	if(adc<par_AdcThres) continue;
	n5++;
	wEve->bemc.adcTile[ibp][softID-1]=adc;
	wEve->bemc.eneTile[ibp][softID-1]=adc*gain;

	if(adc>200){ hA[390]->SetBinContent(softID,adc+hA[390]->GetBinContent(softID));
	  hA[391]->SetBinContent(softID/40+1,softID%40+1,adc+hA[391]->GetBinContent(softID/40+1,softID%40+1));
	}

	if(maxADC<adc) { maxID=softID; maxADC=adc;}
	adcSum+=adc;
  }

  //printf("NNN %d %d %d %d %d %d id=%d\n",n0,n1,n2,n3,n4,n5,maxID);
  if(n0==mxBtow) return -1 ;  // BTOW was not present in this events

  wEve->bemc.tileIn[ibp]=1; //tag usable data

  if(nInpEve%5000==1) { 
	LOG_INFO << Form("unpackMuBTOW() dataIn=%d, nBbad: ped=%d stat=%d gain=%d ; nAdc: %d>0, %d>thres\n    maxADC=%.0f softID=%d adcSum=%.0f",
		wEve->bemc.tileIn[ibp],n1,n2,n3,n4,n5,
		maxADC,maxID,adcSum
		)<<endm;
  }
  hA[31]->Fill(maxADC);
  hA[32]->Fill(adcSum);
  wEve->bemc.maxAdc=maxADC;

  if(maxID<=2400) hA[195]->Fill(maxADC);
  else hA[196]->Fill(maxADC);

  if(maxADC<par_maxADC)  return -2 ;  // not enough energy

  return 0;
}

//________________________________________________
//________________________________________________
float
St2011WMaker::sumTpcCone(int vertID, TVector3 refAxis, int flag, int pointTowId){ 

  // flag=2 use 2D cut, 1= only delta phi

  // printf("******* sumTpcCone, flag=%d eveId=%d vertID=%d  eta0=%.2f phi0/rad=%.2f  \n",flag,wEve->id,vertID,refAxis.PseudoRapidity() ,refAxis.Phi());

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
	if(prTr->flag()!=301 && pointTowId>0) continue;// TPC-only regular tracks for barrel candidate
	if(prTr->flag()!=301 && pointTowId<0) continue;// TPC-only regular tracks for endcap candidate
	float hitFrac=1.*prTr->nHitsFit()/prTr->nHitsPoss();
	if(hitFrac<par_nHitFrac) continue;
	StThreeVectorF prPvect=prTr->p();
	TVector3 primP=TVector3(prPvect.x(),prPvect.y(),prPvect.z());
	// printf(" prTrID=%4d  prTrEta=%.3f prTrPhi/deg=%.1f prPT=%.1f  nFitPts=%d\n", prTr->id(),prTr->eta(),prTr->phi()/3.1416*180.,prTr->pt(),prTr->nHitsFit());
	if(flag==1) {
	  float deltaPhi=refAxis.DeltaPhi(primP);
	  if(fabs(deltaPhi)> par_awayDeltaPhi) continue;
	}
	if(flag==2) {
	  float deltaR=refAxis.DeltaR(primP);
	  if(deltaR>par_nearDeltaR) continue;
	}
	float pT=prTr->pt();
	//    printf(" passed pt=%.1f\n",pT);

	//separate quench for barrel and endcap candidates
	if(pT>par_trackPt && pointTowId>0) ptSum+=par_trackPt;
	else if(pT>parE_trackPt && pointTowId<0) ptSum+=parE_trackPt;
	else  ptSum+=pT;
  }
  return ptSum;
}



//________________________________________________
void
St2011WMaker::accessBSMD(){
  //const char cPlane[ mxBSmd]={'E','P'};
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
		wEve->bemc.statBsmd[iep][softID-1]=1;
		n1++; continue;
	  }
	  if(statOfl!=1) {
		wEve->bemc.statBsmd[iep][softID-1]=2;
		n2++; continue;
	  } 
	  if(statGain<1 || statGain>19) {
		wEve->bemc.statBsmd[iep][softID-1]=4;
		n3++; continue;
	  } 

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
	  }

	  n5++;
	  assert(softID>=1);      assert(softID<=mxBStrips);
	  int id0=softID-1;
	  wEve->bemc.adcBsmd[ iep][id0]=adc;
	  hA[70+10*iep]->Fill(adc);

	  //if(nInpEve<3 || i <20 )printf("  i=%d, smd%c id=%d, m=%d adc=%.3f pedRes=%.1f, sigP=%.1f stat: O=%d P=%d G=%d  gain=%.2f\n",i,cPlane[iep],softID,1+id0/150,adc,pedRes,sigPed, statOfl,statPed,statGain, gain);
	}// end of hit list
  }// end of E-, P-plane loop

}



//$Log: St2011W_acessMuDst.cxx,v $
//Revision 1.17  2016/01/08 02:08:49  jlzhang
//added couples histograms and fixed a small bug
//
//Revision 1.16  2013/09/13 19:33:13  stevens4
//Updates to code for combined 2011+2012 result presented to spin PWG 9.12.13
//
//Revision 1.15  2012/09/18 21:10:06  stevens4
//Include all rank>0 vertex again (new jet format coming next), and remove rank<0 endcap vertices.
//
//Revision 1.14  2012/09/17 03:29:30  stevens4
//Updates to Endcap algo and Q*ET/PT charge separation
//
//Revision 1.13  2012/08/21 18:29:16  stevens4
//Updates to endcap W selection using ESMD strip ratio
//
//Revision 1.12  2012/08/21 17:40:09  stevens4
//Revert to previous version
//
//Revision 1.10  2012/07/24 14:59:24  stevens4
//enable running without spinDb
//
//Revision 1.9  2012/07/13 20:53:16  stevens4
//Add filling of empty events in W tree
//Minor modifications to histograms
//
//Revision 1.8  2012/07/12 20:49:21  balewski
//added spin info(star: bx48, bx7, spin4) and maxHtDSM & BTOW to Wtree
//removed dependence of spinSortingMaker from muDst
//Now Wtree can be spin-sorted w/o DB
//rdMu.C & readWtree.C macros modified
//tested so far on real data run 11
//lot of misc. code shuffling
//
//Revision 1.7  2012/06/18 18:28:01  stevens4
//Updates for Run 9+11+12 AL analysis
//
//Revision 1.6  2011/02/25 06:03:45  stevens4
//addes some histos and enabled running on MC
//
//Revision 1.5  2011/02/17 04:16:20  stevens4
//move sector dependent track QA cuts before track pt>10 cut and lower par_clustET and par_ptBalance thresholds to 14 GeV
//
//Revision 1.4  2011/02/16 15:25:16  stevens4
//remove relative gain correction for BSMD adc
//
//Revision 1.3  2011/02/15 17:39:12  stevens4
//remove accept-all for L2btowW bits
//
//Revision 1.2  2011/02/14 01:36:17  stevens4
//*** empty log message ***
//
//Revision 1.1  2011/02/10 20:33:22  balewski
//start
//
