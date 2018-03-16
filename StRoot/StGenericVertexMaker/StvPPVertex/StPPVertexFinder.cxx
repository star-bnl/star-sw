/************************************************************
 *
 * $Id: StPPVertexFinder.cxx,v 1.15 2018/03/16 18:38:49 genevb Exp $
 *
 * Author: Jan Balewski
 ************************************************************
 *
 * Description:  does not fear any pileup
 *
 ************************************************************/
   
#include "StMessMgr.h"
#include "TGraphErrors.h"
#include "TF1.h"
#include "TH2.h"
#include "TFile.h"
#include "TLine.h"

#include "tables/St_g2t_vertex_Table.h" // tmp for Dz(vertex)

#include "StPPVertexFinder.h"
#include <StEventTypes.h>
#include "TrackData.h"
#include "VertexData.h" 
#include "Vertex3D.h"
#include "StGenericVertexMaker.h"
#include "St_VertexCutsC.h"

#include "StEventToolkit.h"
#include "StEvent/StGlobalTrack.h"
#include "StEvent/StContainers.h"
#include "StEvent/StEnumerations.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StIOMaker/StIOMaker.h" // to save  local histos 
#include "StBFChain/StBFChain.h"

#include "TGeoManager.h"

#define xL(t)   (t->getX())
#define yL(t)   (t->getY())
#define eyL(t)  sqrt(t->getCyy())
#define zL(t)   (t->getZ())
#define ezL(t)  sqrt(t->getCzz())
#define rxyL(t) sqrt(xL(t)*xL(t) + yL(t)*yL(t)) 
#define xG(t)   (t->x_g())
#define yG(t)   (t->y_g())
#define zG(t)   (t->z_g())
#define rxyG(t) sqrt(xG(t)*xG(t) + yG(t)*yG(t)) 

#include "StEEmcUtil/database/StEEmcDb.h"
#include "StEEmcUtil/database/EEmcDbItem.h"
#include "StEEmcUtil/database/cstructs/eemcConstDB.hh"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

#include "BtofHitList.h"
#include "CtbHitList.h"
#include "BemcHitList.h"
#include "EemcHitList.h"

#include "StEmcCollection.h"
#include "StBTofCollection.h"
#include "StBTofUtil/StBTofGeometry.h"
#include "TObjectSet.h"

enum { kImpImp=0, kZZ = 2 ,kPsiImp=3, kPsiPsi=5 };
enum { RxyMin=59, RxyMax=199, zMax=200,zMembraneDepth=1,nWrongZHitCut=2};

Float_t  mImpImp;
    Float_t  mZImp, mZZ;
    Float_t  mPsiImp, mPsiZ, mPsiPsi;
    Float_t  mPtiImp, mPtiZ, mPtiPsi, mPtiPti;
    Float_t  mTanImp, mTanZ, mTanPsi, mTanPti, mTanTan;
    Char_t   mEnd[1];//!

namespace StEvPPV {
//==========================================================
//==========================================================
StPPVertexFinder::StPPVertexFinder() 
{
  mTotEve              = 0;
  HList=0;
  mToolkit =0;
  memset(hA,0,sizeof(hA));

  UseCTB(false);                      	// default CTB is off
  UseBTOF(false);                      	// default BTOF is off
  setDropPostCrossingTrack(true);    	// default PCT rejection on
  mVertexOrderMethod = orderByRanking; 	// change ordering by ranking

  mAlgoSwitches=0; // default, as for 2008 pp data production

  //........... tune for W-boson reco
  mAlgoSwitches|=kSwitchOneHighPT;
  mCut_oneTrackPT=10; // GeV, used only if coresponding algoSwitch switch is ON.
  mBeamLineTracks=0; // expert only, activation via BFC 

  // special histogram for finding the vertex, not to be saved
  int nb=5000;
  float zRange=250;// (cm)
  hL=new TH1D("ppvL","Vertex likelyhood; Z /cm",nb,-zRange,zRange);
  // needed only for  better errZ calculation
  hM=new TH1D("ppvM","cumulative track multiplicity; Z /cm",nb,-zRange,zRange);
  hW=new TH1D("ppvW","cumulative track weight; Z /cm",nb,-zRange,zRange);

} 


//==========================================================
//==========================================================
void StPPVertexFinder::Init() 
{
  assert(mTotEve==0); // can't be called twice
  LOG_INFO << Form("PPV-algo  switches=0x%0x,  following cuts have been activated:",mAlgoSwitches)<<endm;
  //.. set various params 
  mStoreUnqualifiedVertex=5; // extension requested by Akio, October 2008, set to 0 do disable it
  mFitPossWeighting=false;          // default prior to 2012 

  //get pointer to Sti toolkit
  mToolkit = StEventToolkit::instance();
  assert(mToolkit);          // internal error of Sti
  
  ctbList  = new CtbHitList;
  bemcList = new BemcHitList;
  btofList = new BtofHitList;
  vertex3D = 0; // default
  

  // access EEMC-DB
  eeDb = (StEEmcDb*)StMaker::GetChain()->GetDataSet("StEEmcDb"); 
  assert(eeDb); // eemcDB must be in the chain, fix it,JB
  LOG_INFO << "eeDb done" <<endm;
  geomE= new EEmcGeomSimple();
  // choose which 'stat' bits are fatal for mip detection
  unsigned int killStatEEmc=EEMCSTAT_ONLPED | EEMCSTAT_STKBT|  EEMCSTAT_HOTHT |  EEMCSTAT_HOTJP | EEMCSTAT_JUMPED ;
  eemcList =new EemcHitList(eeDb, killStatEEmc,geomE);
   
  HList=new TObjArray(0);   
  initHisto();
  LOG_INFO << "initiated histos" << endm;
  if (mUseBtof)
    btofList->initHisto( HList);
  ctbList->initHisto( HList);
  bemcList->initHisto( HList);
  eemcList->initHisto( HList);
  LOG_INFO << "Finished Init" << endm;
}

//==========================================================
//==========================================================
void StPPVertexFinder::InitRun(int runnumber)
{
  LOG_INFO << "PPV InitRun() runNo="<<runnumber<<endm;
  St_db_Maker* mydb = (St_db_Maker*) StMaker::GetChain()->GetMaker("db");
  int dateY=mydb->GetDateTime().GetYear();
  
 // Initialize BTOF geometry
  if (mUseBtof){ // only add btof if it is required
    btofGeom = 0;
    TObjectSet *geom = (TObjectSet *) mydb->GetDataSet("btofGeometry");
    if (geom)   btofGeom = (StBTofGeometry *) geom->GetObject();
    if (btofGeom) {
      LOG_INFO << " Found btofGeometry ... " << endm;
    } else {
      btofGeom = new StBTofGeometry("btofGeometry","btofGeometry in VertexFinder");
      geom = new TObjectSet("btofGeometry",btofGeom);
      LOG_INFO << " Create a new btofGeometry ... " << endm;
      mydb->AddConst(geom);
    } 
    if(btofGeom && !btofGeom->IsInitDone()) {
      LOG_INFO << " BTofGeometry initialization ... " << endm;
      TVolume *starHall = gGeoManager ? nullptr : (TVolume *) (mydb->GetDataSet("HALL"));
      btofGeom->Init(mydb, starHall, gGeoManager);
    }
  }

  //.. set various params 
  // It is not clear why one would hard code cuts for any specific run or
  // a period since they can be set in the database. Here we'll assume that for
  // Runs 5 to 12 the PPV cuts are optimized and there is no need to access the
  // values from the database.
  if (runnumber >= 6000000 && runnumber < 13000000) {
    // old defaults, pre-Run12
    // (important if we want to reprocess old data with different cuts!)
    LOG_INFO << "PPV InitRun() using old, hardwired cuts" << endm;
    mMaxTrkDcaRxy = 3.0;  // cm 
    mMinTrkPt     = 0.20; // GeV/c  //was 0.2 in 2005 prod
    mMinFitPfrac  = 0.7;  // nFit /nPossible points on the track
    mMaxZradius   = 3.0;  //+sigTrack, to match tracks to Zvertex
    mMinMatchTr   = 2;    // required to accept vertex
  } else {
    St_VertexCutsC* vtxCuts = St_VertexCutsC::instance();
    mMaxTrkDcaRxy = vtxCuts->RImpactMax();
    mMinTrkPt     = vtxCuts->MinTrackPt();
    mMinFitPfrac  = vtxCuts->MinFracOfPossFitPointsOnTrack();
    mMaxZradius   = vtxCuts->DcaZMax();  //+sigTrack, to match tracks to Zvertex
    mMinMatchTr   = vtxCuts->MinTrack();    // required to accept vertex
    mFitPossWeighting = true;
  }
  mMaxZrange    = 200;  // to accept Z_DCA of a track           
  mDyBtof       = 1.5;  // |dy|<1.5 cm for local position - not used now
  mMinZBtof     = -3.0; //
  mMaxZBtof     = 3.0;  // -3.0<zLocal<3.0
  mMinAdcEemc   = 5;    // chan, MIP @ 6-18 ADC depending on eta

  //assert(dateY<2008); // who knows what 2007 setup will be,  crash it just in case

  if(dateY<2006) {
    mMinAdcBemc   = 15;   // BTOW used calibration of maxt Et @ ~27Gev 
  } else {
    mMinAdcBemc   = 8;    // BTOW used calibration of maxt Et @ ~60Gev 
  }
  if (mUseBtof)
    btofList->initRun();
  ctbList->initRun(); 
  bemcList->initRun();
  eemcList->initRun();
  
  if(mBeamLineTracks){
    assert(vertex3D==0); // crash means initRun was called twice - not foreseen,Jan B.
    vertex3D=new Vertex3D;
    vertex3D->setCuts(0.8,8.0, 3.3,5); // pT1(GeV), pT2, sigY(cm), nTr
    vertex3D->initHisto( HList);
    vertex3D->initRun();
  }

  //gMessMgr->Message("","I") 
  LOG_INFO 
    << "PPV::cuts "
    <<"\n MinNumberOfFitPointsOnTrack = unused"
    <<"\n MinFitPfrac=nFit/nPos  = " << mMinFitPfrac 
    <<"\n MaxTrkDcaRxy/cm= " << mMaxTrkDcaRxy
    <<"\n MinTrkPt GeV/c = " << mMinTrkPt
    <<"\n MinMatchTr of prim tracks = " << mMinMatchTr
    <<"\n MaxZrange (cm)for glob tracks = " << mMaxZrange
    <<"\n MaxZradius (cm) for prim tracks &Likelihood  = " << mMaxZradius
    <<"\n DeltaY (cm) for BTOF local posision = "<< mDyBtof
    <<"\n Min/Max Z position for BTOF hit = " << mMinZBtof<<" "<<mMaxZBtof   
    <<"\n MinAdcBemc for MIP = " << mMinAdcBemc
    <<"\n MinAdcEemc for MIP = " << mMinAdcEemc
    <<"\n bool  useCtb = " << mUseCtb
    <<"\n bool useBtof = " << mUseBtof
    <<"\n bool nFit/nPoss weighting = " << mFitPossWeighting
    <<"\n bool DropPostCrossingTrack = " << mDropPostCrossingTrack
    <<"\n Store # of UnqualifiedVertex = " << mStoreUnqualifiedVertex
    <<"\n Store="<<(mAlgoSwitches & kSwitchOneHighPT) <<
               " oneTrack-vertex if track PT/GeV>"<< mCut_oneTrackPT 
    <<"\n dump tracks for beamLine study = " << mBeamLineTracks
    <<"\n"
    <<endm; 

}


//==========================================================
//==========================================================
void StPPVertexFinder::initHisto() 
{
  assert(HList);
  hA[0]=new TH1F("ppvStat","event types; 1=inp, 2=trg, 3=-, 4=w/trk, 5=anyMch, 6=Bmch 7=Emch 8=anyV, 9=mulV",10,0.5,10.5);
  hA[1]=new TH1F("ch1","chi2/Dof, ppv pool tracks",100,0,10);
  hA[2]=new TH1F("nP","No. of fit points, ppv pool tracks",30,-.5,59.5);
  hA[3]=new TH1F("zV","reconstructed vertices ; Z (cm)",100,-200,200);
  hA[4]=new TH1F("nV","No. of vertices per eve",20,-0.5,19.5);
  
  hA[5]=new TH1F("rxyDca","Rxy to beam @ DCA ; (cm)",40,-0.1,3.9);
  hA[6]=new TH1F("nTpcM","No. tracks: tpcMatch /eve ",60,-.5,59.5);
  hA[7]=new TH1F("nTpcV","No. tracks: tpcVeto /eve ",60,-.5,59.5);

  hA[8]=0; // (TH1F*) new TH2F ("xyE","Y vs. X  of match  tracks in EEMC; X (cm); Y(cm)",200,-250.,250,200,-250,250);

  hA[9]=new TH1F("zDca","Z DCA for all accepted tracks; Z (cm)",100,-200,200);
  
  hA[10]=new TH1F("zCtb","Z @CTB for all accepted tracks; Z (cm)",50,-250,250);  
  hA[11]=new TH1F("zBemc","Z @Bemc for all accepted tracks; Z (cm)",100,-400,400);
  hA[12]=new TH1F("dzVerTr","zVerGeant - zDca of tracks used by any vertex ; (cm)",100,-5,5);
  hA[13]=new TH1F("dzVerVer","zVerGeant - best reco vertex ; (cm)",100,-5,5);

  hA[14]=new TH1F("EzDca","Error of Z DCA for all accepted tracks; Z (cm)",100,-0.,4);
  hA[15]=new TH1F("nTpcT","No. tracks: accepted Dca /eve ",201,-.5,200.5);
  hA[16]=new TH1F("ptTr","pT, ppv pool tracks; pT (GeV/c) ",50,0.,10.);
  hA[17]=new TH1F("vRL","PPV Vertex rank, 'funny' X-axis; X=Log10(rank-1e6)+offset", 150, -11,25);

  hACorr=new TH2F("BTOFvsBEMC","BTOF vs BEMC", 5,-2.5,2.5,5,-2.5,2.5);

  int i;
  for(i=0;i<mxH; i++) if(hA[i]) HList->Add(hA[i]);
  HList->Add(hACorr);
}

//==========================================================
//==========================================================
void StPPVertexFinder::Clear()
{
  LOG_DEBUG << "PPVertex::Clear nEve="<<mTotEve<<  endm;
  StGenericVertexFinder::Clear();
  btofList->clear();
  ctbList->clear();
  bemcList->clear();
  eemcList->clear();
  mTrackData.clear();
  mVertexData.clear();
  eveID=-1;

  // the clear below is not needed but cleans up stale result
  hL->Reset();
  hM->Reset();
  hW->Reset();


}


//==========================================================
//==========================================================
StPPVertexFinder::~StPPVertexFinder() 
{
  //x delete mTrackData;
  //x delete mVertexData;
  delete geomE;
  //yf  if(btofGeom) delete btofGeom;
}

//======================================================
//======================================================
void StPPVertexFinder::printInfo(ostream& os) const
{
  os << "StPPVertexFinder ver=1 - Fit Statistics:" << endl;
  
  os << "StPPVertexFinder::result "<<mVertexData.size()<<" vertices found\n" << endl;

  int nTpcM=0, nTpcV=0;
  unsigned int i;
  int k=0;
  for(i=0;i<mTrackData.size();i++) {
    const TrackData *t=&mTrackData[i];
    if(  t->mTpc>0)   nTpcM++;
    else if (  t->mTpc<0) nTpcV++;
    hA[9]->Fill(t->zDca);
    hA[14]->Fill(t->ezDca);
    if(t->vertexID<=0) continue; // skip not used or pileup vertex 
    k++;
    LOG_DEBUG 
      <<
      Form("%d track@z0=%.2f +/- %.2f gPt=%.3f vertID=%d match:  bin,Fired,Track:\n",
	   k,t->zDca,t->ezDca,t->gPt,t->vertexID) 
      << Form("    Btof %3d,%d,%d",t->btofBin,btofList->getFired(t->btofBin),btofList->getTrack(t->btofBin))
      << Form("    CTB  %3d,%d,%d",t->ctbBin,ctbList->getFired(t->ctbBin),ctbList->getTrack(t->ctbBin))
      << Form("    Bemc %3d,%d,%d",t->bemcBin,bemcList->getFired(t->bemcBin),bemcList->getTrack(t->bemcBin))
      << Form("    Eemc %3d,%d,%d",t->eemcBin,eemcList->getFired(t->eemcBin),bemcList->getTrack(t->bemcBin))
      << Form("    TPC %d",t->mTpc)
      <<endm;
  }
  hA[6]->Fill(nTpcM);
  hA[7]->Fill(nTpcV);
  hA[15]->Fill(mTrackData.size());
  
  LOG_INFO<< Form("PPVend  eveID=%d,  list of found %d vertices from pool of %d tracks\n",eveID,mVertexData.size(),mTrackData.size())<<endm;
  for(i=0;i<mVertexData.size();i++) {
    const VertexData *V=& mVertexData[i];
    V->print(os);
  }
  
  LOG_DEBUG<< Form("---- end of PPVertex Info\n")<<endm;

}


//==========================================================
//==========================================================
int StPPVertexFinder::fit(StEvent* event) {
  LOG_INFO << "***** START FIT" << endm;
  if(mBeamLineTracks) vertex3D->clearEvent();

  hA[0]->Fill(1);
assert(event);
  StEventToolkit::instance()->SetEvent(event);
  mTotEve++;
  eveID=event->id();
  LOG_INFO << "\n   @@@@@@   PPVertex::Fit START nEve="<<mTotEve<<"  eveID="<<eveID<<  endm;

  TString tt="Vertex likelyhood, eveID=";
  tt+=eveID;
  hL->SetTitle(tt);

  hA[0]->Fill(2);

  if(mToolkit==0) {    
   LOG_WARN <<"no Sti tool kit,  PPV is OFF"<<endm;
   return 0;
  }

 // get BTOF info

  if(mUseBtof) {
    StBTofCollection *btofColl = (StBTofCollection*)event->btofCollection();
    if(btofColl==0) {
      LOG_WARN << "no btofCollection , continue THE SAME eve"<<endm;
    } else {
      btofList->build(btofColl);
    }
  }

  // get CTB info, does not  work for embeding 
  if(mUseCtb) {// CTB could be off since 2006 
    StTriggerData *trgD=event->triggerData ();
    ctbList->buildFromData(trgD); // use real data
  }

  
  StEmcCollection* emcC =(StEmcCollection*)event->emcCollection(); 
  if(emcC==0) {
    LOG_WARN <<"no emcCollection , continue THE SAME eve"<<endm;
  } else {
    assert(emcC);
    StEmcDetector* btow = emcC->detector( kBarrelEmcTowerId); 
    if(btow==0) {
      LOG_WARN <<"no BEMC in emcColl , continue THE SAME eve"<<endm;
    } else {
      assert(btow);
      bemcList->build(btow, mMinAdcBemc);
    }
    
    StEmcDetector* etow = emcC->detector(kEndcapEmcTowerId); 
    if(etow==0) {
      LOG_WARN <<"no EEMC in emcColl , continue THE SAME eve"<<endm;
    } else {
      assert(etow);
      eemcList->build(etow, mMinAdcEemc);
    }
  }

  //get the Sti track container...
  const StSPtrVecTrackNode* tracks = mToolkit->getTrackContainer();
   if(tracks==0) {
     LOG_WARN <<"no STi tracks , skip eve"<<endm;
     printInfo();  
     return 0 ;				       
   }

  hA[0]->Fill(4);
  
  //select reasonable tracks and add them to my list
  int k=0;
  int kBtof=0,kCtb=0,kBemc=0, kEemc=0,kTpc=0;
  int nmAny=0;

  int ntrk[7]; for(int i=0; i<7; i++) ntrk[i]=0;

  int nTk = tracks->size();
  for (int it=0; it<nTk; ++it) 
  {
    k++;
    const StGlobalTrack* track = (const StGlobalTrack*)(*tracks)[it]->track(global);
    if (!track->dcaGeometry()) continue;
    TrackData t;

    ntrk[0]++;
    if(track->flag()<0)        		{ntrk[1]++; continue;}

    double myPt = track->dcaGeometry()->pt();
    if(myPt<mMinTrkPt) 			{ntrk[2]++; continue;}
    if(mDropPostCrossingTrack){
      if(isPostCrossingTrack(track))  	{ntrk[3]++; continue;}  // kill if it has hits in wrong z
    }
    if(!examinTrackDca(track,t))      	{ntrk[4]++; continue;}  // drop from DCA		   
    if(!matchTrack2Membrane(track,t)) 	{ntrk[5]++; continue;}  // kill if nFitP too small	   
    ntrk[6]++;

    //cout <<"\n#e itr="<<k<<" gPt="<<track->getPt()<<" gEta="<<track->getPseudoRapidity()<<" nFitP="<<track->getFitPointCount()<<" of "<<track->getMaxPointCount()<<" poolSize="<< mTrackData->size()<<"  myW="<<t.weight<<endl;
    //printf(" t.weight AA=%f\n", t.weight);

    hA[ 1]->Fill(track->fitTraits().chi2());
    hA[ 2]->Fill(track->fitTraits().numberOfFitPoints());
    hA[16]->Fill(myPt);
    //  dumpKalmanNodes(track);
    
    // ......... matcho various detectors ....................
    if(mUseBtof) matchTrack2BTOF(track,t,btofGeom);  // matching track to btofGeometry
    if(mUseCtb)  matchTrack2CTB(track,t);
    matchTrack2BEMC(track,t,242); // middle of tower in Rxy
    matchTrack2EEMC(track,t,288); // middle of tower in Z
    //.... all test done on this track .........
    t.mother=track;
    mTrackData.push_back(t); 

    hA[5]->Fill(t.rxyDca);

    if( t.mBtof>0 ) kBtof++;
    if( t.mCtb>0 )  kCtb++;   
    if( t.mBemc>0)  kBemc++;   
    if( t.mEemc>0)  kEemc++;
    if( t.mTpc>0 )  kTpc++;
 
    if(t.mBtof>0 || t.mCtb>0 || t.mBemc>0 || t.mEemc>0 || t.mTpc>0 ) nmAny++ ;

    hACorr->Fill(t.mBtof, t.mBemc);
    //  t.print();
  }

  LOG_INFO<< Form("PPV:: # of input track          = %d",ntrk[0])<<endm;
  LOG_INFO<< Form("PPV:: dropped due to flag       = %d",ntrk[1])<<endm;
  LOG_INFO<< Form("PPV:: dropped due to pt         = %d",ntrk[2])<<endm;
  LOG_INFO<< Form("PPV:: dropped due to PCT check  = %d",ntrk[3])<<endm;
  LOG_INFO<< Form("PPV:: dropped due to DCA check  = %d",ntrk[4])<<endm;
  LOG_INFO<< Form("PPV:: dropped due to NHit check = %d",ntrk[5])<<endm;
  LOG_INFO<< Form("PPV:: # of track after all cuts = %d",ntrk[6])<<endm;

  if(mUseCtb) {
    ctbList ->print();
    ctbList ->doHisto();
  }

 if(mUseBtof) {
    btofList->print();
    btofList->doHisto();
  }

  bemcList->print();
  eemcList->print();
  LOG_INFO<< Form("PPV::TpcList size=%d nMatched=%d\n\n",mTrackData.size(),kTpc)<<endm;

  bemcList->doHisto();
  eemcList->doHisto();

  LOG_INFO << "PPV::fit() nEve="<<mTotEve<<" , "<<nmAny<<" traks with good DCA, matching: BTOF="<<kBtof<<" CTB="<<kCtb<<" BEMC="<<kBemc<<" EEMC="<<kEemc<<endm;


  if(nmAny<mMinMatchTr &&  mStoreUnqualifiedVertex<=0){
    LOG_INFO << "StPPVertexFinder::fit() nEve="<<mTotEve<<" Quit, to few matched tracks"<<endm;
    printInfo();
    return 0;
  }
  hA[0]->Fill(5);

  if(kBemc)  hA[0]->Fill(6);
  if(kEemc)  hA[0]->Fill(7);

  //............................................................
  // ...................... search for multiple vertices 
  //............................................................

  const float par_rankOffset=1e6; // to separate class of vertices (approximately)

  int nBadVertex=0;
  int vertexID=0;
  while(1) {
    if(! buildLikelihoodZ() ) break;
    VertexData V;
    V.id=++vertexID;
    if(! findVertexZ(V)) break;
  
    bool trigV=evalVertexZ(V);   // V.print();
    //bump up rank of 2+ track all vertices 
    if(V.nAnyMatch>=mMinMatchTr) V.Lmax+=par_rankOffset;
    if(!trigV) {
      if( nBadVertex>=mStoreUnqualifiedVertex)  continue; // drop this vertex
      /*  preserve this unqalified vertex for Akio 
	  and deposit 1 cent on Jan's bank account (optional) 
      */
      nBadVertex++;
      //bump down rank of sub-prime vertices 
      V.Lmax-=par_rankOffset; 
    } 
    
    {// ... more rank QA ...
      float rank=V.Lmax;
      if(rank>1e6)  hA[17]->Fill(log(rank-1e6)+10);
      else if(rank>0)   hA[17]->Fill(log(rank));
      else   hA[17]->Fill(log(rank+1e6)-10);
    }
    
    mVertexData.push_back(V);
    if(trigV && mBeamLineTracks) vertex3D->study(V.r,eveID);
  }

  LOG_INFO << "StPPVertexFinder::fit(totEve="<<mTotEve<<") "<<mVertexData.size()<<" vertices found, nBadVertex=" <<nBadVertex<< endm;
  
  if(mVertexData.size()>0)  hA[0]->Fill(8);
  if(mVertexData.size()>1)  hA[0]->Fill(9);

  exportVertices();
  printInfo();
  
  hA[4]->Fill(mVertexData.size());
  unsigned int i;
  for(i=0;i<mVertexData.size();i++) {
    VertexData *V=&mVertexData[i];
    hA[3]->Fill(V->r.z());
  }
  
  if(mVertexData.size()<=0) {
    return 0; // no vertex
  }
  
  return size();
} 


//==========================================================
//==========================================================
bool StPPVertexFinder::buildLikelihoodZ()
{
  hL->Reset();
  hM->Reset();
  hW->Reset();

  float dzMax2=mMaxZradius*mMaxZradius;

  int nt=mTrackData.size();
  LOG_DEBUG<< Form("PPV::buildLikelihood() pool of nTracks=%d",nt)<<endm;
  if(nt<=0) return false;

  int n1=0;
  int i;

  double *La=hL->GetArray(); // PPV main likelyhood histogram 
  double *Ma=hM->GetArray(); // track multiplicity  histogram 
  double *Wa=hW->GetArray(); // track weight histogram 
  
  for(i=0;i<nt;i++) {
    const TrackData *t=&mTrackData[i];
    if(t->vertexID!=0) continue; // track already used
    if(t->anyMatch) n1++;
    //  t->print();
    float z0=t->zDca;
    float ez=t->ezDca;
    float ez2=ez*ez;
    int j1=hL->FindBin(z0-mMaxZradius-.1);
    int j2=hL->FindBin(z0+mMaxZradius+.1);
    float base=dzMax2/2/ez2;
    float totW=t->weight;
    //  printf("i=%d Z0=%f ez=%f j1=%d j2=%d base=%f gPt/GeV=%.3f ctbW=%.3f\n",i,z0,ez,j1,j2,base,t->gPt,ctbW);

    int j;
    for(j=j1;j<=j2;j++) {
      float z=hL->GetBinCenter(j);
      float dz=z-z0;
      float xx=base-dz*dz/2/ez2;
      if(xx<=0) continue;
      La[j]+=xx*totW;
      Ma[j]+=1.;
      Wa[j]+=totW;
      // printf("z=%f dz=%f  xx=%f\n",z,dz,xx);
    }
    // break; // tmp , to get only one track
  }

 LOG_DEBUG<< Form("PPV::buildLikelihood() %d tracks w/ matched @ Lmax=%f",n1,hL->GetMaximum())<<endm;


  return (n1>=mMinMatchTr) ||  (mStoreUnqualifiedVertex>0 );
}

//==========================================================
//==========================================================
bool StPPVertexFinder::findVertexZ(VertexData &V) {

  if(hL->GetMaximum()<=0) return false; // no more tracks left

  int iMax=hL-> GetMaximumBin();
  float z0=hL-> GetBinCenter(iMax);
  float Lmax=hL-> GetBinContent(iMax);
  float accM=hM-> GetBinContent(iMax);
  float accW=hW-> GetBinContent(iMax);
  assert(accM>0);
  float avrW=accW/accM;

  // search for sigma of the vertex
  float Llow=0.9* Lmax;
  if((Lmax-Llow)<8*avrW )  Llow=Lmax-8*avrW;  // to be at least 4 sigma
  int i;
  double *L=hL->GetArray(); // likelyhood 

  int iLow=-1, iHigh=-1;
  for(i=iMax;i<=hL->GetNbinsX();i++) {
    if(L[i] >Llow) continue;
    iHigh=i;
    break;
  }
  for(i=iMax;i>=1;i--) {
    if(L[i] >Llow) continue;
    iLow=i;
    break;
  }
  
  float zLow=hL-> GetBinCenter(iLow);
  float zHigh=hL-> GetBinCenter(iHigh);

  float kSig= sqrt(2*(Lmax-Llow)/avrW);
  float sigZ= (zHigh-zLow)/2/kSig;
  LOG_DEBUG<< Form("PPV:: iLow/iMax/iHigh=%d/%d/%d\n",iLow,iMax,iHigh)
	  <<Form(" accM=%f  accW=%f  avrW=%f\n",accM,accW,avrW)   
	  <<Form("  Z low/max/high=%f %f %f, kSig=%f, sig=%f\n",zLow,z0,zHigh,kSig,sigZ)
	  <<Form(" found  PPVertex(ID=%d,neve=%d) z0 =%.2f +/- %.2f\n",V.id,mTotEve,z0,sigZ)<<endm;
  if(sigZ<0.1) sigZ=0.1; // tmp, make it not smaller than the bin size

  // take x,y from beam line equation, TMP
  V.r=TVector3(beamX(z0), beamY(z0), z0);
  V.er=TVector3(0.1,0.1,sigZ); //tmp
  V.Lmax=Lmax;

  return true;
}

//==========================================================
//==========================================================
bool StPPVertexFinder::evalVertexZ(VertexData &V) { // and tag used tracks
  // returns true if vertex is accepted accepted
  if(mBeamLineTracks) vertex3D->clearTracks();
  int nt=mTrackData.size();
  LOG_DEBUG << "StPPVertexFinder::evalVertex Vid="<<V.id<<" start ..."<<endm;
  int n1=0, nHiPt=0;
  int i;
  
  for(i=0;i<nt;i++) {
    TrackData *t=&mTrackData[i];
    if(t->vertexID!=0) continue;
    if(! t->matchVertex(V,mMaxZradius)) continue; // track to far
    // this track belongs to this vertex
    n1++;
    t->vertexID=V.id;
    V.gPtSum+=t->gPt;
    if(mBeamLineTracks) vertex3D->addTrack(t);
    if( t->gPt>mCut_oneTrackPT && ( t->mBemc>0|| t->mEemc>0) ) nHiPt++;

    if(  t->mTpc>0)       V.nTpc++;
    else if (  t->mTpc<0) V.nTpcV++;

    if(  t->mBtof>0)       V.nBtof++;
    else if (  t->mBtof<0) V.nBtofV++;

    if(  t->mCtb>0)       V.nCtb++;
    else if (  t->mCtb<0) V.nCtbV++;

    if(  t->mBemc>0)       V.nBemc++;
    else if (  t->mBemc<0) V.nBemcV++;

    if(  t->mEemc>0)       V.nEemc++;
    else if (  t->mEemc<0) V.nEemcV++;

    if( t->anyMatch)     V.nAnyMatch++;
    else if (t->anyVeto) V.nAnyVeto++;
  } 
  V.nUsedTrack=n1;  

  bool validVertex = V.nAnyMatch>=mMinMatchTr;
  if((mAlgoSwitches & kSwitchOneHighPT) && ( nHiPt>0)) {
    validVertex|=1;
  }

  if(!validVertex) { // discrad vertex
    //no match tracks in this vertex, tag vertex ID in tracks differently
    //V.print(cout);
    LOG_DEBUG << "StPPVertexFinder::evalVertex Vid="<<V.id<<" rejected"<<endm;
    for(i=0;i<nt;i++) {
      TrackData *t=&mTrackData[i];
      if(t->vertexID!=V.id) continue;
      t->vertexID=-V.id;
    }
    return false;
  }
  
  LOG_INFO << "StPPVertexFinder::evalVertex Vid="<<V.id<<" accepted, nAnyMatch="<<V.nAnyMatch<<" nAnyVeto="<<V.nAnyVeto<<endm;
  return true;
}

 
//-------------------------------------------------
//-------------------------------------------------
void StPPVertexFinder::exportVertices(){
  if ( ! mVertexConstrain ){
    // code is not ready for reco w/o beamLine
    LOG_FATAL << "StPPVertexFinder code is not ready for reco w/o beamLine" << endm;
    assert(mVertexConstrain); 
  }
  unsigned int i;
  for(i=0;i<mVertexData.size();i++) {
    VertexData *V=&mVertexData[i];
    StThreeVectorD r(V->r.x(),V->r.y(),V->r.z());

    Float_t cov[6];
    memset(cov,0,sizeof(cov)); 
    cov[0]=V->er.x()*V->er.x(); 
    cov[2]=V->er.y()*V->er.y(); 
    cov[5]=V->er.z()*V->er.z();  // [5] is correct,JB 

    StPrimaryVertex primV;
    primV.setPosition(r);
    primV.setCovariantMatrix(cov); 

    if(mUseCtb)  primV.setVertexFinderId(ppvVertexFinder);
    else         primV.setVertexFinderId(ppvNoCtbVertexFinder); 

    primV.setNumTracksUsedInFinder(V->nUsedTrack);
    primV.setNumMatchesWithBTOF(V->nBtof);
    primV.setNumMatchesWithCTB(V->nCtb);
    primV.setNumMatchesWithBEMC(V->nBemc);
    primV.setNumMatchesWithEEMC(V->nEemc);
    primV.setNumTracksCrossingCentralMembrane(V->nTpc);
    primV.setSumOfTrackPt(V->gPtSum);
    primV.setRanking(V->Lmax);
    primV.setFlag(1); //??? is it a right value?
  
    //..... add vertex to the list
    addVertex(primV);
  }
  LOG_DEBUG << "StPPVertexFinder::exportVertices(), size="<<size()<<endm;
}

//-------------------------------------------------
//-------------------------------------------------
void StPPVertexFinder::Finish() 
{

  if(mBeamLineTracks) { // save local histo w/ PPV monitoring
    StIOMaker *ioMk=(StIOMaker*)StMaker::GetChain()->GetMaker("inputStream");
    
    TString tt="ppv";
    if(ioMk) {
      assert(ioMk);
      const char *fname=ioMk->GetFileName();
      tt=strstr(fname,"st_");
      tt.ReplaceAll(".daq",".ppv");
    } else {
      tt= ((StBFChain*)StMaker::GetChain())->GetFileOut() ;
      tt.ReplaceAll(".root",".ppv");
    }
    LOG_INFO << "PPV save local histo="<<tt<<endm;
    saveHisto(tt.Data());
  }

  LOG_INFO << "StPPVertexFinder::Finish() done, seen eve=" <<mTotEve<< endm;
  //  TString fileIn    = ((StBFChain*)StMaker::GetChain())->GetFileIn() ;
  // LOG_INFO << "in="<<fileIn<< endm;
}

//-------------------------------------------------
//-------------------------------------------------
void StPPVertexFinder::saveHisto(TString fname){
  TString outName=fname+".hist.root";
  TFile f( outName,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),outName.Data());
  HList->ls();
  HList->Write();
  f.Close();
}

//==========================================================
//==========================================================
void StPPVertexFinder::dumpKalmanNodes(const StGlobalTrack*)
{
 assert(0 && "dumpKalmanNodes");
}

//==========================================================
//==========================================================
bool StPPVertexFinder::examinTrackDca(const StGlobalTrack *track,TrackData &t)
{

  //1 StGlobalTrackNode* inNode=track->getInnerMostNode();
  //1 cout <<"#e  track->getPseudoRapidity()="<<track->getPseudoRapidity()<<" track->getFitPointCount()="<<track->getFitPointCount()<<endl;
  
  // .......... test DCA to beam .............
  const StDcaGeometry *dcaGeo = track->dcaGeometry();
  if (!dcaGeo) return false;

  double rxy=dcaGeo->impact();

  //1 cout<<"#e @beam global DCA x:"<< bmNode->x_g()<<" y:"<< bmNode->y_g()<<" z:"<< bmNode->z_g()<<" Rxy="<< rxy <<endl;
  if(fabs(rxy)        	> mMaxTrkDcaRxy) 	return false;
  if(fabs(dcaGeo->z())	> mMaxZrange   )	return false ; 

  const float *dcaErr = dcaGeo->errMatrix();
  double impimp = dcaErr[kImpImp];
  double psipsi = dcaErr[kPsiPsi];
  double xyErr = (0.5*(impimp + rxy*rxy*psipsi));
  
  //1 cout<<"#e inBeam |P|="<<bmNode->getP()<<" pT="<<bmNode->getPt()<<" local x="<<xL(bmNode)<<" y="<<yL(bmNode)<<" +/- "<<eyL(bmNode)<<" z="<<zL(bmNode)<<" +/- "<<ezL(bmNode)<<endl;

  t.zDca=dcaGeo->z();
  t.ezDca=sqrt(dcaErr[kZZ]);
  t.rxyDca=rxy;
  t.gPt=dcaGeo->pt();
  		//...... record more detals for 3D vertex reco
  StThreeVectorF  myXYZ(dcaGeo->origin());
  t.dcaTrack.R.SetXYZ(myXYZ.x(),myXYZ.y(),myXYZ.z());
  		// approximation below: use sigX=sigY, I do not want to deal wih rotations in X-Y plane, Jan B.


  t.dcaTrack.sigYloc=sqrt(xyErr);
  t.dcaTrack.sigZ=t.ezDca; 

  StThreeVectorF const globP3=dcaGeo->momentum();
  t.dcaTrack.gP.SetXYZ(globP3.x(),globP3.y(),globP3.z());
//VP  t.dcaTrack.fitErr=bmNode->fitErrs();
  t.dcaTrack.gChi2=track->fitTraits().chi2();
  t.dcaTrack.nFitPoint=track->fitTraits().numberOfFitPoints();
  //  t.dcaTrack.print();

  return true;
}


//==========================================================
//==========================================================
void StPPVertexFinder::matchTrack2BTOF(const StGlobalTrack* track,TrackData &t,StBTofGeometry* geom)
{

  StPhysicalHelixD hlx(track->outerGeometry()->helix());

  IntVec idVec;
  DoubleVec pathVec;
  PointVec crossVec;

  IntVec iBinVec;
  if(geom->HelixCrossCellIds(hlx,idVec,pathVec,crossVec)) {
    for(size_t i=0;i<idVec.size();i++) {
      int itray, imodule, icell;
      geom->DecodeCellId(idVec[i], icell, imodule, itray);

      Double_t local[3], global[3];
      for(int j=0;j<3;j++) local[j] = 0;
      global[0] = crossVec[i].x();
      global[1] = crossVec[i].y();
      global[2] = crossVec[i].z();
      StBTofGeomSensor *sensor = geom->GetGeomSensor(imodule,itray);
      if(!sensor) {
        LOG_WARN << "no sensitive module in this projection??? - weird" << endm;
        continue;
      }
      sensor->Master2Local(&global[0],&local[0]);
//      LOG_INFO << "   Hit the TOF cell " << itray << " " << imodule << " " << icell << endm;
//      LOG_INFO << "   position " << local[0] << " " << local[1] << " " << local[2] << endm;
//      float yCenter = (icell-1-2.5)*3.45;  // cell center position;
//      if(fabs(local[1]-yCenter)>mDyBtof) continue;
//      if(icell==1||icell==6) continue;
      if(local[2]<mMinZBtof||local[2]>mMaxZBtof) continue;
      int iBin = btofList->cell2bin(itray, imodule, icell);
      iBinVec.push_back(iBin);
      btofList->addBtofTrack(itray, imodule, icell);
      LOG_DEBUG << "   !!! Push to the list ...tray/module/cell " << itray << "/" << imodule << "/" << icell << endm;
    }
  }

  bool  btofMatch=btofList->isMatched(iBinVec);
  bool  btofVeto =btofList->isVetoed(iBinVec);
  float btofW    =btofList->getWeight(iBinVec);
  btofList->addBtofMatch(iBinVec);  // update the nMatch statistics

  LOG_DEBUG << " ** BTOF ** match/veto/weight = " << btofMatch << " " << btofVeto << " " << btofW << endm;

  t.updateAnyMatch(btofMatch,btofVeto,t.mBtof);
  t.weight*=btofW;
  t.btofBin= iBinVec.size() ? iBinVec[0] : -1;
}

//==========================================================
//==========================================================
void StPPVertexFinder::matchTrack2CTB(const StGlobalTrack* track,TrackData &t)
{
  const double Rctb=213.6; // (cm) radius of the CTB 
  StPhysicalHelixD hlx(track->outerGeometry()->helix());

  StThreeVectorD posCTB;
  float path=-1;
  pairD  d2;
  d2 = hlx.pathLength(Rctb);
  path=d2.second;
  if(d2.first>=0 || d2.second<=0) {
    LOG_DEBUG <<Form("WARN MatchTrk , unexpected solution for track crossing CTB\n")<<
      Form(" d2.firts=%f, second=%f, try first", d2.first, d2.second)<<endm;
    path=d2.first;
  }
  posCTB = hlx.at(path);
  // printf(" punch Cylinder x,y,z=%.1f, %.1f, %.1f path.second=%.1f\n",posCTB.x(),posCTB.y(),posCTB.z(),path);

  // official Sti node extrapolation

  float phi=posCTB.phi();
  if(phi<0) phi+=2*M_PI;// now phi is [0,2Pi] as for CTB slats
  float eta=posCTB.pseudoRapidity();
  //1 cout<<"#e @ctbNode xyz="<<posCTB<<" eta="<<eta<<" phi/deg="<<phi/3.1416*180<<" path/cm="<<path<<endl;
  if(fabs(eta)<1) hA[10]->Fill(posCTB.z());

  int iBin=ctbList->addTrack(eta,phi);
  
  bool  ctbMatch=ctbList->isMatched(iBin);
  bool  ctbVeto =ctbList->isVetoed(iBin);
  float ctbW    =ctbList->getWeight(iBin);
  
  t.updateAnyMatch(ctbMatch,ctbVeto,t.mCtb);
  t.weight*=ctbW;
  t.ctbBin=iBin;
}

//==========================================================
//==========================================================
void StPPVertexFinder::matchTrack2BEMC(const StGlobalTrack* track,TrackData &t, float Rxy)
{
  
  StPhysicalHelixD hlx(track->outerGeometry()->helix());
  StThreeVectorD posCyl;
  float path=-1;
  pairD  d2;
  d2 = hlx.pathLength(Rxy);
  path=d2.second;
  if(d2.first>=0 || d2.second<=0) {
    LOG_DEBUG <<Form("WARN MatchTrk , unexpected solution for track crossing BEMC Cyl\n")<<
      Form(" d2.firts=%f, second=%f, try first\n", d2.first, d2.second)<<endm;
    path=d2.first;
  }
  posCyl = hlx.at(path);
  // printf(" punch Cylinder x,y,z=%.1f, %.1f, %.1f path.second=%.1f\n",posCyl.x(),posCyl.y(),posCyl.z(),path);


  float phi=posCyl.phi();
  if(phi<0) phi+=2*M_PI;// now phi is [0,2Pi] as for Cyl slats
  float eta=posCyl.pseudoRapidity();
  
  // cout<<"#e @bemcNode xyz="<<posCyl<<" etaDet="<<eta<<" phi/deg="<<phi/3.1416*180<<" path/cm="<<path<<endl;

  if(fabs(eta)<1) hA[11]->Fill(posCyl.z());
  
  int   iBin=bemcList->addTrack(eta,phi);
  bool  bemcMatch=bemcList->isMatched(iBin);
  bool  bemcVeto =bemcList->isVetoed(iBin);
  float bemcW    =bemcList->getWeight(iBin);

  t.updateAnyMatch(bemcMatch,bemcVeto,t.mBemc);
  t.weight*=bemcW;
  t.bemcBin=iBin;

}


//==========================================================
//==========================================================
void StPPVertexFinder::matchTrack2EEMC(const StGlobalTrack* track,TrackData &t,float z)
{
  
  const float minEta=0.7 ;// tmp cut
  const float maxPath=200 ;// tmp, cut too long extrapolation

  const StTrackGeometry* outGeo = track->outerGeometry();
  const StDcaGeometry*   dcaGeo = track->dcaGeometry();
  StPhysicalHelixD hlx(outGeo->helix());

  //direction of extrapolation must be toward West (Z+ axis)
  if(dcaGeo->z() > outGeo->origin().z()) return;
  
  // droop too steep tracks
  if(dcaGeo->momentum().pseudoRapidity() < minEta) return;

  StThreeVectorD rSmd=StThreeVectorD(0,0,z); 
  StThreeVectorD n=StThreeVectorD(0,0,1);

  double path = hlx.pathLength(rSmd,n);
  //cout<<" EEMC match: path="<<path<<endl;
  if(path>maxPath) return; // too long extrapolation

  StThreeVectorD r = hlx.at(path);
  float periodL=hlx. period();
 
  if(periodL<2*path) {
    LOG_DEBUG <<Form(" Warn, long path fac=%.1f ",path/periodL)<<
      Form("  punchEEMC1 x,y,z=%.1f, %.1f, %.1f path=%.1f period=%.1f\n",r.x(),r.y(),r.z(),path,periodL)<<endm; 
  }

  float phi=r.phi();
  if(phi<0) phi+=2*M_PI;// now phi is [0,2Pi] as for Cyl slats
  float eta=r.pseudoRapidity();

  int iBin=eemcList->addTrack(eta,phi);
  bool  eemcMatch=eemcList->isMatched(iBin);
  bool  eemcVeto =eemcList->isVetoed(iBin);
  float eemcW    =eemcList->getWeight(iBin);

  t.updateAnyMatch(eemcMatch,eemcVeto,t.mEemc);
  t.weight*=eemcW;
  t.eemcBin=iBin;

}

//==========================================================
//==========================================================
bool StPPVertexFinder::matchTrack2Membrane(const StGlobalTrack* track,TrackData &t)
{
  //generate bitt pattern for TPC nodes with hits 
  int nPos=0,nFit=0,jz0=0;
  const StDcaGeometry* dcaGeo = track->dcaGeometry();
  if (!dcaGeo)		return 0;
  float rxy=fabs(dcaGeo->impact());
  float z=dcaGeo->z();  
  if(rxy    >RxyMax) 	return 0;
  if(fabs(z)>zMax) 	return 0;

  vector<int> hitPatt; hitPatt.resize(200);
const StPtrVecHit& hits = track->detectorInfo()->hits();
  int nHits = hits.size();
  const StHit *hit = hits[0];
  int zignOld = (hit->position().z()<0)? -1:1;
  int rowMin=999,rowMax=-999;

  for (int it=0;it<nHits;it++) {//loop over hits
    hit = hits[it];
    if (!hit) 				continue;
    if (hit->detector()!=kTpcId)	continue;
    float z = hit->position().z();
    if(fabs(z)>zMax) 			continue;
    if (hit->position().perp()<RxyMin) 	continue;
    if (hit->position().perp()>RxyMax) 	continue;
    if(fabs(z)<zMembraneDepth        ) 	continue;	//ignore hits too close to z=0
    const StTpcHit* tpcHit = (const StTpcHit*)hit;
    int row = tpcHit->padrow();
    if (rowMin>row) rowMin=row;
    if (rowMax<row) rowMax=row;
    hitPatt[row-1]=1; nFit++;
    if (!jz0) 	{// jz0 is not yet defined
      int zignNow = (z<0)? -1:1;
      if (zignNow!=zignOld) jz0 = row;
    }
  }//end loop over hits
  if (rowMax<0) 	return 0;
  
  for (int jl=0,jr=rowMin-1;jr<rowMax;jl++,jr++) { //remove leading & trailing zeros
    hitPatt[jl]=hitPatt[jr];
  }
  jz0-= rowMin-1; nPos = rowMax-rowMin+1;
  hitPatt.resize(nPos);

  if(nFit< mMinFitPfrac * nPos) return false; // too short fragment of a track

  if( mFitPossWeighting)
    t.weight*=double(nFit)/nPos;// introduced in 2012 for pp510 to differentiate between global track quality, together with lowering the overall threshold from 0.7 to 0.51, Jan
  
  t.scanNodes(hitPatt,jz0); // if central membrane is crossed, scale weight inside

  return true;
}

//==========================================================
//==========================================================

bool StPPVertexFinder::isPostCrossingTrack(const StGlobalTrack* track)
{
  int nWrongZHit=0;

  const StPtrVecHit& hits = track->detectorInfo()->hits();
  int nHits =  hits.size();
  for (int it=0; it<nHits;it++) 
  {
    const StTpcHit* hit=(const StTpcHit*)hits[it];
    if(hit->detector()!=kTpcId)	continue;
    float r=hit->position().perp();
    if (r < RxyMin) 		continue;
    if (r > RxyMax) 		continue;
    float z=hit->position().z();
    if (fabs(z) > zMax) 	continue;
    if ((z < -zMembraneDepth && hit->sector() <= 12) ||
	(z >  zMembraneDepth && hit->sector() >  12)) {
	    nWrongZHit++;
	    if(nWrongZHit>=nWrongZHitCut) {return true;}
    }	
  }
  return false;
}

/**************************************************************************
 **************************************************************************
 * $Log: StPPVertexFinder.cxx,v $
 * Revision 1.15  2018/03/16 18:38:49  genevb
 * Use TGeo initializer for BTof geometry
 *
 * Revision 1.14  2017/05/10 23:16:42  smirnovd
 * Some minor refactoring changes:
 *
 * See commits 6fb592df..07da3bdf on master branch
 *
 * StPPVertexFinder: Get rid of a temporary variable
 *
 * StPPVertexFinder: Get rid of extra return
 * Zero vertices returned for unqualified event anyway
 *
 * StGenericVertexFinder: Removed deprecated CalibBeamLine()
 *
 * Revision 1.13  2017/02/14 22:00:41  smirnovd
 * Squashed commit of the following clean-up changes:
 *
 * See master branch for details.
 *
 * - Remove commented code for debugging
 * - Removed extra validation; it is done at construction
 * - No need to include header for apple OS
 * - Removed pointless assert
 * - Use standard portable type name
 * - Remove unused header math_constants.h
 * - StMinuitVertexFinder: Remove abandoned member function
 *
 * Revision 1.12  2017/01/06 21:01:49  smirnovd
 * Use pi constant from standard library, s/C_PI/M_PI/
 *
 * Revision 1.11  2017/01/03 22:17:37  smirnovd
 * [Stylistic] Changed public addVertex() to accept references
 *
 * Avoid unnecessary gymnastics with pointers
 *
 * Revision 1.10  2016/12/12 18:44:21  smirnovd
 * Removed unused local variable
 *
 * Revision 1.9  2016/12/12 17:17:00  smirnovd
 * Removed unused #include "TCanvas.h"
 *
 * Revision 1.8  2016/12/12 16:42:30  smirnovd
 * Removed special treatment for MC data in PPV vertex finder
 *
 * Removed mIsMC flag in order to treat the simulated and real data in the same
 * manner during vertex reconstruction. The flag caused confusion rather than being
 * helpful in any way for the main VF task. As an additional benefit the users will
 * not have to worry about setting the flag when running on simulated data.
 *
 * Revision 1.7  2016/11/07 21:19:28  smirnovd
 * Added and reworded some doxygen and other comments
 *
 * Also cleaned up not-so-useful comments
 *
 * Revision 1.6  2016/11/07 21:19:14  smirnovd
 * Moved log print statements out of constructors
 *
 * Revision 1.5  2016/08/18 17:46:15  smirnovd
 * Squashed commit of the following refactoring changes:
 *
 * Date:   Wed Jul 27 18:31:18 2016 -0400
 *
 *     Removed unused arguments in UseVertexConstraint()
 *
 *     In StiPPVertexFinder and StvPPVertexFinder this method does nothing
 *
 * Date:   Wed Jul 27 16:47:58 2016 -0400
 *
 *     Make old UseVertexConstraint private virtual and call it from its public replacement in the base class
 *
 *     also mark methods as private explicitly
 *
 * Date:   Wed Jul 27 16:52:02 2016 -0400
 *
 *     Removed unused private data member mWeight
 *
 * Date:   Wed Jul 27 16:50:42 2016 -0400
 *
 *     Prefer base class static beamline parameters rather than this class private members
 *
 * Date:   Wed Jul 27 16:21:49 2016 -0400
 *
 *     StPPVertexFinder: Got rid of unused private beamline parameters
 *
 *     The equivalent measurements are available from the base class
 *     StGenericVertexFinder
 *
 * Date:   Wed Jul 27 16:19:19 2016 -0400
 *
 *     StPPVertexFinder: For beamline position use equivalent static methods from parent class
 *
 * Date:   Wed Jul 27 16:05:50 2016 -0400
 *
 *     StGenericVertexMaker: Assigning once is enough
 *
 * Date:   Mon Aug 15 10:43:49 2016 -0400
 *
 *     StGenericVertexFinder: Print out beamline parameters
 *
 *     Print beamline values as extracted from the database before any modification.
 *
 * Date:   Wed Jul 6 15:33:02 2016 -0400
 *
 *     Stylistic changes and minor refactoring
 *
 *     Whitespace and comments for improved readability
 *     s/track/stiKalmanTrack/
 *
 * Date:   Wed Jul 6 15:28:16 2016 -0400
 *
 *     StPPVertexFinder: Switched to cleaner c++11 range loop syntax
 *
 * Date:   Wed Jul 6 15:22:14 2016 -0400
 *
 *     StPPVertexFinder: Minor c++ refactoring
 *
 *     - Removed unused counter
 *     - c-style array to std::array
 *
 * Date:   Wed Jul 6 15:20:11 2016 -0400
 *
 *     Deleted commented out code
 *
 *     Removed unused #include's StMinuitVertexFinder
 *
 * Revision 1.4  2016/03/08 15:54:19  smirnovd
 * Removed pointless remnants of past debugging
 *
 * Revision 1.3  2016/02/29 22:58:23  jwebb
 * Moved include of StEventTypes from header of generic class to implementation files of generic and concrete classes.
 *
 * Revision 1.2  2013/08/19 21:27:32  perev
 * Check for Dca geo added
 *
 * Revision 1.1  2013/08/16 22:19:56  perev
 * PPV with only StEvent dependency
 *
 * Revision 1.44  2013/04/09 22:37:56  genevb
 * Remove boostEfficiency codes: DB usage implemented
 *
 * Revision 1.43  2013/04/09 21:11:58  genevb
 * Use database table for track selection cuts
 *
 * Revision 1.42  2013/04/05 21:00:02  jeromel
 * Implemented and merged back to source the boostEfficiency (i.e. change of
 * nFit /nPossible points on the track fract to consider). No DB imp yet.
 *
 * Fixed boostEfficiency()
 *
 * Changed cout to LOG_INFO
 *
 * Revision 1.41  2012/11/06 20:58:04  fisyak
 * Remove second addition of btofGeom to db maker
 *
 * Revision 1.40  2012/05/25 20:19:40  balewski
 * convert many LOG_INFO to LOG_DEBUG to make PPV more silent. No intencional change of PPV logic.
 *
 * Revision 1.39  2012/05/07 15:55:30  fisyak
 * Proper handing of btofGeometry
 *
 * Revision 1.38  2010/09/16 04:18:55  rjreed
 * Moved intialized of btof from init to initrun and changed it so that the btof class is not utilized if mUseBtof is false
 *
 * Revision 1.37  2010/09/10 21:08:35  rjreed
 * Added function UseBOTF and bool mUseBtof to switch the use of the TOF on and off in vertex finding.  Default value is off (false).
 * Added functions, and variables necessary to use the TOF in PPV for vertex finding.  Includes matching tracks to the TOF and changing the track weight based on its matched status with the TOF.
 *
 * Revision 1.36  2009/11/20 18:54:08  genevb
 * Avoid compiler warning about operator order precedence
 *
 * Revision 1.35  2009/11/05 21:40:08  rjreed
 * Last line of matchTrack2Membrane was deleted between version 1.29 and 1.30.  This line checks
 * tracks to determine whether they've crossed the TPC CM.  This rev reinstates the line.
 *
 * Revision 1.34  2009/07/09 21:29:03  balewski
 * allow export of prim tracks for 3D beam line fit (use VtxSeedCalG option),
 * oneTrack vertex thresholds was lowered form 15 to 10 GeV/c
 *
 * Revision 1.33  2009/02/05 21:43:59  balewski
 * Oleksandr renamed StEEmcDbMaker to StEEmcDb and requested this set of code corrections
 *
 * Revision 1.32  2008/12/02 14:35:05  balewski
 * I forgot to require EMC hit for highPT track, now it is in
 *
 * Revision 1.31  2008/12/01 22:57:39  balewski
 * Added capability to reco 1 high pT track vertices with positive rank. 2+ match vertices will have rank above 1e6. Sub-prime vertices (for Akio) have negative rank. More details is given at:
 * http://drupal.star.bnl.gov/STAR/comp/reco/vf/ppv-vertex/2009-algo-upgrade-1
 *
 * Revision 1.30  2008/10/21 19:23:05  balewski
 * store unqualified vertices on Akio's request
 *
 * Revision 1.29  2008/08/21 22:09:31  balewski
 * - In matchTrack2Membrane()
 *   - Cut on hit max R chanegd from 190 to 199cm
 *   - Fixed logic failure of counting possible hits
 *   - Fixed logic failure of crossing CM for certain pattern of hits
 * - Added a new function bool isPostCrossingTrack()
 *   - it returns true if track have 2 or more hits in wrong z
 * - Use isPostCrossingTrack() in fit()
 * - Added switch setDropPostCrossingTrack(bool), defaulted to true
 * All changes tested & implemented by Akio in preparation for 2008 pp production.
 * The key change (removing PostCrossingTrack) is in response to the change of the TPC cluster finder
 * - now we use the on-line version which allows for longer range of TPC time buckets to be used.
 *
 * Revision 1.28  2008/04/03 16:24:31  fisyak
 * replace sti->getToolkit() by StEventToolkit::instance()
 *
 * Revision 1.27  2008/02/12 17:51:20  jeromel
 * Assert of Year number removed. Assert on beamLine left but added an explaination (so we won't have to rediscover this).
 *
 * Revision 1.26  2007/03/22 08:42:05  balewski
 * extend validity of PPV for 2007 data taking
 *
 * Revision 1.25  2006/10/17 13:38:03  fisyak
 * Remove dependencies from dead classes
 *
 * Revision 1.24  2006/07/31 17:57:54  balewski
 * cleanup before 2006 prod, no changes in algo, CTB stays out all the time
 *
 * Revision 1.23  2006/06/02 20:46:55  perev
 * Accoun DCA node added
 *
 * Revision 1.22  2006/05/04 20:01:31  jeromel
 * Switched to logger
 *
 * Revision 1.21  2006/04/26 15:37:04  jeromel
 * mVertexOrderMethod (To be tested)
 *
 * Revision 1.20  2006/03/12 18:47:29  balewski
 * small corrections of histograms and printouts
 *
 * Revision 1.19  2006/03/12 17:01:01  jeromel
 * Minor change + use ppvNoCtbVertexFinder
 *
 * Revision 1.18  2006/03/11 04:12:49  balewski
 * 2 changes in preparation for 2006 data processing:
 * - CTB matching  ON/OFF switch activated by m_Mode 0x8 or 0x10
 * - vertex enum extension depending on CTB usage - hack in the moment, 
 *   Jerome needs to provide actual new enum
 * - BTOW calibration wil change for 2006+ from maxt eT of ~27 --> 60 GeV
 * NOTE : this new code was NOT executed - it is late, I want to get it in CVS
 * Tomorrow I'll do some tests
 * Jan
 *
 * Revision 1.17  2006/01/24 17:53:39  balewski
 * small fix
 *
 * Revision 1.16  2006/01/24 17:26:06  balewski
 * drop hardcoded mask of BTOW lower East, now it takes BTOW mask & ped+sigPed from DB
 * Watch the DB  time stamp !
 *
 * Revision 1.15  2005/09/03 16:41:53  balewski
 * bug fix: <<endm replaced with <<endl
 *
 * Revision 1.14  2005/08/30 22:08:43  balewski
 * drop '*' from declaration of   mTrackData &  mVertexData
 *
 * Revision 1.13  2005/08/17 15:07:39  balewski
 * cleanup, irrelevant for pp200 production
 *
 * Revision 1.12  2005/08/15 13:04:08  balewski
 * Z-range +/- 250 cm
 *
 * Revision 1.11  2005/08/14 23:49:55  balewski
 * smaller bins for Z-likelihood, limit siZ >=1 mm
 *
 * Revision 1.10  2005/08/12 18:35:27  balewski
 * more accurate calculation of Z-vertex error
 * by accounting for average weight of tracks contributing to the likelihood,
 *  Now errZ is of 0.5-1.5 mm, was ~2x smaller
 *
 * Revision 1.9  2005/07/28 20:57:14  balewski
 * extand zMax range to 200cm, call it PPV-2
 *
 * Revision 1.8  2005/07/27 06:08:19  balewski
 * tuning PPV cuts
 *
 * Revision 1.7  2005/07/26 02:49:08  balewski
 * more flexible for node pathologies
 *
 * Revision 1.6  2005/07/22 21:02:08  balewski
 * bug fix & cleanup
 *
 * Revision 1.5  2005/07/20 05:34:16  balewski
 * cleanup
 *
 * Revision 1.4  2005/07/19 22:01:24  perev
 * MultiVertex
 *
 * Revision 1.3  2005/07/15 20:53:25  balewski
 * cleanup
 *
 * Revision 1.2  2005/07/14 15:39:25  balewski
 * nothing, to force recompilation of this code by Autobuild
 *
 * Revision 1.1  2005/07/11 20:38:12  balewski
 * PPV added for real
 *
 *
 **************************************************************************
 **************************************************************************
 **************************************************************************/

}// end namespace StEvPPV
