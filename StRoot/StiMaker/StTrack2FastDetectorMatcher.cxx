/************************************************************
 *
 * $Id: StTrack2FastDetectorMatcher.cxx,v 2.15 2018/04/10 11:32:10 smirnovd Exp $
 *
 * Author: Jan Balewski
 ************************************************************
 *
 * Description:  does not fear any pileup
 *
 ************************************************************/
   
#include <StMessMgr.h>
#include "St_db_Maker/St_db_Maker.h"
#include "StTrack2FastDetectorMatcher.h"
#include "StEvent.h"
#include "StEEmcUtil/database/StEEmcDb.h"
#include "StEEmcUtil/database/EEmcDbItem.h"
#include "StEEmcUtil/database/cstructs/eemcConstDB.hh"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEmcCollection.h"
#include "StBTofCollection.h" // dongx
#include "StBTofUtil/StBTofGeometry.h"
#include "TGeoManager.h"
#include "TMath.h"

//________________________________________________________________________________
StTrack2FastDetectorMatcher::StTrack2FastDetectorMatcher() : mTotEve(0), mMinZBtof(-3.0), mMaxZBtof(3.0), mMinAdcBemc(5), mMinAdcEemc(5),  
							     isMC(kFALSE),
							     btofList(0), 
							     ctbList(0), 
							     bemcList(0), eemcList(0), btofGeom(0),
 							     eeDb(0), geomE(0) {
							     
  LOG_INFO << "StTrack2FastDetectorMatcher::StTrack2FastDetectorMatcher is in use" << endm;
}
//________________________________________________________________________________
void 
StTrack2FastDetectorMatcher::Clear(const Char_t *opt){
  LOG_DEBUG << "StTrack2FastDetectorMatcher::Clear nEve="<<mTotEve<<  endm;
  if (btofList) btofList->clear();  // dongx
  if (ctbList)  ctbList->clear();
  if (bemcList) bemcList->clear();
  if (eemcList) eemcList->clear();
  eveID=-1;
}
//________________________________________________________________________________
StTrack2FastDetectorMatcher::~StTrack2FastDetectorMatcher() {
  //x delete mStiTrack2FastDetector;
  //x delete mVertexData;
  SafeDelete(geomE);
  SafeDelete(btofGeom); // dongx
  SafeDelete(btofList);
  SafeDelete(ctbList);
  SafeDelete(bemcList);
  SafeDelete(eemcList)
}
//________________________________________________________________________________
void StTrack2FastDetectorMatcher::fillArrays(StEvent* event) {
  static Bool_t InitRun = kTRUE;
  if (InitRun) {
    isMC = (! event->triggerData() );
    St_db_Maker* mydb = (St_db_Maker*) StMaker::GetChain()->GetMaker("db");
    Int_t dateY=mydb->GetDateTime().GetYear();
    // access EEMC-DB
    eeDb = (StEEmcDb*)StMaker::GetChain()->GetDataSet("StEEmcDb"); 
    if (eeDb) {
      geomE= new EEmcGeomSimple();
      // choose which 'stat' bits are fatal for mip detection
      UInt_t  killStatEEmc=EEMCSTAT_ONLPED | EEMCSTAT_STKBT|  EEMCSTAT_HOTHT |  EEMCSTAT_HOTJP | EEMCSTAT_JUMPED ;
      eemcList =new StEemcHitList(eeDb, killStatEEmc,geomE); eemcList->initRun();
    }
    if (dateY > 2008) {
      TObjectSet *btofGeom_dataset = (TObjectSet *) mydb->GetDataSet("btofGeometry");
      btofGeom = btofGeom_dataset ? (StBTofGeometry *) btofGeom_dataset->GetObject() : nullptr;
      // If StBTofGeometry object is not found in the list of maker's objects create a new one
      if (! btofGeom) {
	// Build StBTofGeometry from TGeo geometry if available
	TVolume *starHall = gGeoManager ? nullptr : (TVolume *)mydb->GetDataSet("HALL");
	LOG_INFO << " BTofGeometry initialization ... " << endm;
	btofGeom = new StBTofGeometry("btofGeometry","btofGeometry");
	if (isMC) btofGeom->SetMCOn();
	else      btofGeom->SetMCOff();
	btofGeom->Init(mydb, starHall, gGeoManager);
	mydb->AddConst(new TObjectSet("btofGeometry",btofGeom));
      }

      btofList = new StBtofHitList();
      btofList->initRun();

    } else {
      ctbList   = new StCtbHitList; ctbList->initRun(); 
    }
    if(isMC) {
      LOG_INFO << "StTrack2FastDetectorMatcher M-C, Db_date="<<mydb->GetDateTime().AsString()<<endm;
      // simu prior to 2008 
      mMinAdcBemc =7; //ideal BTOW gain 60 GeV ET @ 3500 ADC 
      // in late 2008 Matt uploaded ideal (aka sim) gians matching endcap
      if(dateY>=2008)  mMinAdcBemc =8; //ideal BTOW gain 60 GeV ET @ 4076 ADC
    }
    if(dateY<2006) mMinAdcBemc   = 15;   // BTOW used calibration of maxt Et @ ~27Gev 
    else           mMinAdcBemc   = 8;    // BTOW used calibration of maxt Et @ ~60Gev 
    bemcList  = new StBemcHitList;  bemcList->initRun();
    LOG_INFO 
      <<"\n Min/Max Z position for BTOF hit = "<<mMinZBtof<<" "<<mMaxZBtof   // dongx
      <<"\n MinAdcBemc for MIP ="<<mMinAdcBemc
      <<"\n MinAdcEemc for MIP ="<<mMinAdcEemc
      <<"\n bool    isMC ="<<isMC
      <<"\n bool  useCtb ="<< (ctbList  != 0)
      <<"\n bool useBemc ="<< (bemcList != 0)
      <<"\n bool useEemc ="<< (eemcList != 0)
      <<"\n bool useBtof ="<< (btofList != 0)
      <<endm; 
    InitRun = kFALSE;
  }
  StEvent *mEvent = (StEvent *)  StMaker::GetChain()->GetInputDS("StEvent");
  assert(mEvent); 

  mTotEve++;
  eveID=event->id();
  LOG_INFO << "StTrack2FastDetectorMatcher::fillArrays  START nEve="<<mTotEve<<"  eveID="<<eveID<<  endm;
  // get BTOF info - dongx
  if (btofList) {
    StBTofCollection *btofColl = (StBTofCollection*)mEvent->btofCollection();
    if (btofColl)  btofList->build(btofColl);
  }
  // get CTB info, does not  work for embeding 
  if (ctbList) {
    StTriggerData *trgD=event->triggerData();
    if (trgD) {
      ctbList->buildFromData(trgD); // use real data
    } else {
      if(StMaker::GetChain()->GetDataSet("geant/g2t_ctb_hit")) {
	St_DataSet *gds=StMaker::GetChain()->GetDataSet("geant");
	ctbList->buildFromMC(gds); // use M-C
      }
    }
  }
  StEmcCollection* emcC =(StEmcCollection*)mEvent->emcCollection(); 
  if (emcC) {
    if (bemcList) {
      StEmcDetector* btow = emcC->detector( kBarrelEmcTowerId); 
      if(btow) bemcList->build(btow, mMinAdcBemc);
    }    
    if (eemcList) {
      StEmcDetector* etow = emcC->detector(kEndcapEmcTowerId); 
      if (etow)  eemcList->build(etow, mMinAdcEemc);
    } 
  }
}
//________________________________________________________________________________
void  
StTrack2FastDetectorMatcher::matchTrack2BTOF(const StPhysicalHelixD *hlx,StiTrack2FastDetector *t){
  IntVec idVec;
  DoubleVec pathVec;
  PointVec crossVec;

  IntVec iBinVec;
  if(btofGeom->HelixCrossCellIds(*hlx,idVec,pathVec,crossVec)) {
    for(size_t i=0;i<idVec.size();i++) {
      Int_t tray, module, cell;
      btofGeom->DecodeCellId(idVec[i], cell, module, tray);

      Double_t local[3], global[3];
      for(Int_t j=0;j<3;j++) local[j] = 0;
      global[0] = crossVec[i].x();
      global[1] = crossVec[i].y();
      global[2] = crossVec[i].z();
      StBTofGeomSensor *sensor = btofGeom->GetGeomSensor(module,tray);
      if(!sensor) {
        LOG_WARN << "no sensitive module in this projection??? - weird" << endm;
        continue;
      }
      sensor->Master2Local(&global[0],&local[0]);
      if(local[2]<mMinZBtof||local[2]>mMaxZBtof) continue;
      Int_t iBin = btofList->addBtofTrack(tray, module, cell);
      if (iBin < 0) continue;
      iBinVec.push_back(iBin);
      btofList->addBtofTrack(tray, module, cell);
    }
  }
  if (! iBinVec.size()) return;
  Bool_t  btofMatch=btofList->isMatched(iBinVec);
  Bool_t  btofVeto =btofList->isVetoed(iBinVec);
  Float_t btofW    =btofList->getWeight(iBinVec);
  LOG_DEBUG << " ** BTOF ** match/veto/weight = " << btofMatch << " " << btofVeto << " " << btofW << endm;
  t->updateAnyMatch(btofMatch,btofVeto,t->mBtof);
  t->weight*=btofW;
  t->btofBin= iBinVec.size();
}
//________________________________________________________________________________
void  
StTrack2FastDetectorMatcher::matchTrack2CTB(const StPhysicalHelixD *hlx,StiTrack2FastDetector *t){
  const Double_t Rctb=213.6; // (cm) radius of the CTB 
  StThreeVectorD posCTB;
  Float_t path=-1;
  //alternative helix extrapolation:
  pairD  d2;
  d2 = hlx->pathLength(Rctb);
  path=d2.second;
  if(d2.first>=0 || d2.second<=0) {
    LOG_DEBUG <<Form("WARN MatchTrk , unexpected solution for track crossing CTB\n")<<
      Form(" d2.firts=%f, second=%f, try first", d2.first, d2.second)<<endm;
    path=d2.first;
  }
  posCTB = hlx->at(path);
    // printf(" punch Cylinder x,y,z=%.1f, %.1f, %.1f path.second=%.1f\n",posCTB.x(),posCTB.y(),posCTB.z(),path);


  Float_t phi=TMath::ATan2(posCTB.y(),posCTB.x());
  if(phi < ctbList->MinPhi()) phi+=2*TMath::Pi();// now phi is [0,2Pi] as for CTB slats
  Float_t eta=posCTB.pseudoRapidity();
  //1 cout<<"#e @ctbNode xyz="<<posCTB<<" eta="<<eta<<" phi/deg="<<phi/3.1416*180<<" path/cm="<<path<<endl;

  Int_t iBin=ctbList->addTrack(eta,phi);
  if (iBin < 0) return;
  Bool_t  ctbMatch=ctbList->isMatched(iBin);
  Bool_t  ctbVeto =ctbList->isVetoed(iBin);
  Float_t ctbW    =ctbList->getWeight(iBin);
  
  t->updateAnyMatch(ctbMatch,ctbVeto,t->mCtb);
  t->weight*=ctbW;
  t->ctbBin=iBin;
}
//________________________________________________________________________________
void  
StTrack2FastDetectorMatcher::matchTrack2BEMC(const StPhysicalHelixD *hlx,StiTrack2FastDetector *t, Float_t Rxy){
  StThreeVectorD posCyl;
  Float_t path=-1;
  pairD  d2;
  d2 = hlx->pathLength(Rxy);
  path=d2.second;
  if(d2.first>=0 || d2.second<=0) {
#if 0
    printf("WARN MatchTrk , unexpected solution for track crossing Cyl\n");
    printf(" d2.firts=%f, second=%f, try first\n",
	   d2.first, d2.second);
#endif
    path=d2.first;
  }
  posCyl = hlx->at(path);
  // printf(" punch Cylinder x,y,z=%.1f, %.1f, %.1f path.second=%.1f\n",posCyl.x(),posCyl.y(),posCyl.z(),path);


  Float_t phi=TMath::ATan2(posCyl.y(),posCyl.x());
  if(phi < bemcList->MinPhi()) phi+=2*TMath::Pi();// now phi is [0,2Pi] as for Cyl slats
  Float_t eta=posCyl.pseudoRapidity();
  
  // cout<<"#e @bemcNode xyz="<<posCyl<<" etaDet="<<eta<<" phi/deg="<<phi/3.1416*180<<" path/cm="<<path<<endl;

  Int_t iBin=bemcList->addTrack(eta,phi);
  if (iBin < 0) return;
  Bool_t  bemcMatch=bemcList->isMatched(iBin);
  Bool_t  bemcVeto =bemcList->isVetoed(iBin);
  Float_t bemcW    =bemcList->getWeight(iBin);

  t->updateAnyMatch(bemcMatch,bemcVeto,t->mBemc);
  t->weight*=bemcW;
  t->bemcBin=iBin;

}
//________________________________________________________________________________
void  
StTrack2FastDetectorMatcher::matchTrack2EEMC(const StPhysicalHelixD *hlx,StiTrack2FastDetector *t,Float_t z){
  
  const Float_t maxPath=200 ;// tmp, cut too long extrapolation

  StThreeVectorD rSmd=StThreeVectorD(0,0,z); 
  StThreeVectorD n=StThreeVectorD(0,0,1);
   // path length at intersection with plane
   // Double_t       pathLength(const StThreeVectorD& r,
   //                         const StThreeVectorD& n) const;

  Double_t path = hlx->pathLength(rSmd,n);
  //cout<<" EEMC match: path="<<path<<endl;
  if(path>maxPath) return; // too long extrapolation

  StThreeVectorD r = hlx->at(path);
  Float_t periodL=hlx-> period();
 
  if(periodL<2*path) {
    printf(" Warn, long path fac=%.1f ",path/periodL);
    printf("  punchEEMC1 x,y,z=%.1f, %.1f, %.1f path=%.1f period=%.1f\n",r.x(),r.y(),r.z(),path,periodL); 
  }

  Float_t phi=TMath::ATan2(r.y(),r.x());
  if(phi < eemcList->MinPhi()) phi+=2*TMath::Pi();// now phi is [0,2Pi] as for Cyl slats
  Float_t eta=r.pseudoRapidity();

  Int_t iBin=eemcList->addTrack(eta,phi);
  if (iBin < 0) return;
  Bool_t  eemcMatch=eemcList->isMatched(iBin);
  Bool_t  eemcVeto =eemcList->isVetoed(iBin);
  Float_t eemcW    =eemcList->getWeight(iBin);

  t->updateAnyMatch(eemcMatch,eemcVeto,t->mEemc);
  t->weight*=eemcW;
  t->eemcBin=iBin;

}
//________________________________________________________________________________
void  StTrack2FastDetectorMatcher::matchTrack2FastDetectors(const StPhysicalHelixD *hlx,StiTrack2FastDetector *t) {
  if (btofList && btofList->NoHits()) matchTrack2BTOF(hlx,t);     // matching track to btofGeometry
  if (ctbList  && ctbList->NoHits() ) matchTrack2CTB(hlx,t);
  if (bemcList && bemcList->NoHits()) matchTrack2BEMC(hlx,t,242); // middle of tower in Rxy
  if (eemcList && eemcList->NoHits()) matchTrack2EEMC(hlx,t,288); // middle of tower in Z
}
/**************************************************************************
 * $Log: StTrack2FastDetectorMatcher.cxx,v $
 * Revision 2.15  2018/04/10 11:32:10  smirnovd
 * Minor corrections across multiple files
 *
 * - Remove ClassImp macro
 * - Change white space
 * - Correct windows newlines to unix
 * - Remove unused debugging
 * - Correct StTpcRTSHitMaker header guard
 * - Remove unused preprocessor directives in StiCA
 * - Minor changes in status and debug print out
 * - Remove using std namespace from StiKalmanTrackFinder
 * - Remove includes for unused headers
 *
 * Revision 2.14  2018/02/28 19:40:48  genevb
 * Fix some informational output
 *
 * Revision 2.13  2018/02/26 23:29:50  smirnovd
 * StTrack2FastDetectorMatcher: Build StBTOfGeometry from TGeo geometry when available
 *
 * Revision 2.12  2018/02/26 23:29:42  smirnovd
 * StTrack2FastDetectorMatcher: Don't change debug level for StBTofGeometry (in a weird way)
 *
 * Revision 2.11  2018/02/26 23:29:33  smirnovd
 * StTrack2FastDetectorMatcher: Remove unnecessary test for just created BTof geometry
 *
 * Revision 2.10  2018/01/30 13:23:42  smirnovd
 * Revert previous changes mistakenly committed to trunk instead of a branch
 *
 * Revision 2.9  2018/01/29 19:49:04  smirnovd
 * StTrack2FastDetectorMatcher: Build StBTOfGeometry from TGeo geometry when available
 *
 * Revision 2.8  2018/01/29 19:48:54  smirnovd
 * StTrack2FastDetectorMatcher: Don't change debug level for StBTofGeometry (in a weird way)
 *
 * Revision 2.7  2018/01/29 19:48:40  smirnovd
 * StTrack2FastDetectorMatcher: Remove unnecessary test for just created BTof geometry
 *
 * Revision 2.6  2018/01/03 21:24:14  smirnovd
 * Don't use common name from std:: as local variable
 *
 * Revision 2.5  2013/03/22 23:29:15  genevb
 * Initialize mTotEve to zero
 *
 * Revision 2.4  2013/01/18 15:03:37  fisyak
 * Fix TrackData data name clash with StiPPVertexFinder
 *
 * Revision 2.3  2013/01/17 15:57:26  fisyak
 * Add handles for debugging
 *
 * Revision 2.2  2012/11/06 20:54:09  fisyak
 * Fix a bug with misplacement of TObjectSet
 *
 * Revision 2.1  2012/05/07 14:56:14  fisyak
 * Add StKFVertexMaker
 *
 **************************************************************************/
