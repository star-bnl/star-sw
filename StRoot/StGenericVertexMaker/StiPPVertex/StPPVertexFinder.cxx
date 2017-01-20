/************************************************************
 *
 * $Id: StPPVertexFinder.cxx,v 1.83 2017/01/20 17:48:34 smirnovd Exp $
 *
 * Author: Jan Balewski
 ************************************************************
 *
 * Description:  does not fear any pileup
 *
 ************************************************************/
   
#include <StMessMgr.h>

#include "TFile.h"
#include "TH1D.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TMinuit.h"
#include "TObjArray.h"
#include "TObjectSet.h"

#include <math_constants.h>
#include <tables/St_g2t_vertex_Table.h> // tmp for Dz(vertex)

#include "StPPVertexFinder.h"
#include <StEventTypes.h>
#include "TrackData.h"
#include "VertexData.h" 
#include "Vertex3D.h"
#include "StGenericVertexMaker.h"
#include "St_VertexCutsC.h"

#include <Sti/StiToolkit.h>
#include <Sti/StiKalmanTrack.h>
#include <Sti/StiKalmanTrackNode.h>
#include <Sti/StiTrackContainer.h>
#include "Sti/StiTrack.h"

#include <St_db_Maker/St_db_Maker.h>
#include <StIOMaker/StIOMaker.h> // to save  local histos 
#include <StBFChain/StBFChain.h>

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

#include <StEEmcUtil/database/StEEmcDb.h>
#include <StEEmcUtil/database/EEmcDbItem.h>
#include <StEEmcUtil/database/cstructs/eemcConstDB.hh>
#include <StEEmcUtil/EEmcGeom/EEmcGeomSimple.h>

#include "BtofHitList.h"
#include "CtbHitList.h"
#include "BemcHitList.h"
#include "EemcHitList.h"

#include "StEmcCollection.h"
#include "StBTofCollection.h"
#include "StBTofUtil/StBTofGeometry.h"

//==========================================================
//==========================================================

StPPVertexFinder::StPPVertexFinder(VertexFit_t fitMode) : StGenericVertexFinder(fitMode),
  mDropPostCrossingTrack(true) // default PCT rejection on
{

  mTotEve              = 0;
  HList=0;
  mToolkit =0;
  memset(hA,0,sizeof(hA));

  UseCTB(true);                      // default CTB is in the data stream
  mVertexOrderMethod = orderByRanking; // change ordering by ranking

  mAlgoSwitches=0; // default, as for 2008 pp data production

  //........... tune for W-boson reco
  mAlgoSwitches|=kSwitchOneHighPT;
  mCut_oneTrackPT=10; // GeV, used only if coresponding algoSwitch switch is ON.
  mStudyBeamLineTracks = false; // expert only, activation via BFC

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
void 
StPPVertexFinder::Init() {
  assert(mTotEve==0); // can't be called twice
  LOG_INFO << Form("PPV-algo  switches=0x%0x,  following cuts have been activated:",mAlgoSwitches)<<endm;
  //.. set various params 
  mStoreUnqualifiedVertex=5; // extension requested by Akio, October 2008, set to 0 do disable it
  mFitPossWeighting=false;          // default prior to 2012 

  //get pointer to Sti toolkit
  mToolkit = StiToolkit::instance();
  assert(mToolkit);          // internal error of Sti
  
  ctbList  = new CtbHitList;
  bemcList = new BemcHitList;
  btofList = new BtofHitList;
  vertex3D = 0; // default
  

  // access EEMC-DB
  StEEmcDb *eeDb = (StEEmcDb*)StMaker::GetChain()->GetDataSet("StEEmcDb");
  assert(eeDb); // eemcDB must be in the chain, fix it,JB
  LOG_INFO << "eeDb done" <<endm;
  geomE= new EEmcGeomSimple();
  // choose which 'stat' bits are fatal for mip detection
  uint  killStatEEmc=EEMCSTAT_ONLPED | EEMCSTAT_STKBT|  EEMCSTAT_HOTHT |  EEMCSTAT_HOTJP | EEMCSTAT_JUMPED ;
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
void 
StPPVertexFinder::InitRun(int runnumber){
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
      TVolume *starHall = (TVolume *)mydb->GetDataSet("HALL");
      btofGeom->Init(mydb, starHall);
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
  
  if (mStudyBeamLineTracks) {
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
    <<"\n dump tracks for beamLine study = " << mStudyBeamLineTracks
    <<"\n"
    <<endm; 

}


//==========================================================
//==========================================================
void 
StPPVertexFinder::initHisto() {
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

  for (int i=0; i<mxH; i++) if(hA[i]) HList->Add(hA[i]);

  HList->Add(hACorr);
}


void StPPVertexFinder::findSeeds_TSpectrum()
{
   std::vector<double> vertexZs = StGenericVertexFinder::FindSeeds_TSpectrum();

   // Loop over the seeds and associate tracks with it. Then for each seed
   // create a vertex candidate of VertexData type
   for (double vertexZ : vertexZs)
   {
      VertexData vertex( TVector3(0, 0, vertexZ) );
      // Need to add a findMatchingTracks(vertex, mTrackData) method...
      mVertexData.push_back( vertex );
   }
}


//==========================================================
//==========================================================
void 
StPPVertexFinder::Clear(){
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
StPPVertexFinder::~StPPVertexFinder() {
  delete geomE;
}

//======================================================
//======================================================
void
StPPVertexFinder::printInfo(ostream& os) const
{
  os << "StPPVertexFinder ver=1 - Fit Statistics:" << endl;
  
  os << "StPPVertexFinder::result "<<mVertexData.size()<<" vertices found\n" << endl;

  int nTpcM=0, nTpcV=0;
  int k=0;
  for (const TrackData &t : mTrackData) {
    if(  t.mTpc>0)   nTpcM++;
    else if (  t.mTpc<0) nTpcV++;
    hA[9]->Fill(t.zDca);
    hA[14]->Fill(t.ezDca);
    if(t.vertexID<=0) continue; // skip not used or pileup vertex
    k++;
    LOG_DEBUG 
      <<
      Form("%d track@z0=%.2f +/- %.2f gPt=%.3f vertID=%d match:  bin,Fired,Track:\n",
	   k,t.zDca,t.ezDca,t.gPt,t.vertexID)
      << Form("    Btof %3d,%d,%d",t.btofBin,btofList->getFired(t.btofBin),btofList->getTrack(t.btofBin))
      << Form("    CTB  %3d,%d,%d",t.ctbBin,ctbList->getFired(t.ctbBin),ctbList->getTrack(t.ctbBin))
      << Form("    Bemc %3d,%d,%d",t.bemcBin,bemcList->getFired(t.bemcBin),bemcList->getTrack(t.bemcBin))
      << Form("    Eemc %3d,%d,%d",t.eemcBin,eemcList->getFired(t.eemcBin),eemcList->getTrack(t.eemcBin))
      << Form("    TPC %d",t.mTpc)
      <<endm;
  }
  hA[6]->Fill(nTpcM);
  hA[7]->Fill(nTpcV);
  hA[15]->Fill(mTrackData.size());
  
  LOG_INFO<< Form("PPVend  eveID=%d,  list of found %d vertices from pool of %d tracks\n",eveID,mVertexData.size(),mTrackData.size())<<endm;

  for (const VertexData &v : mVertexData)
    v.print(os);

  LOG_DEBUG<< Form("---- end of PPVertex Info\n")<<endm;

}

//======================================================
//======================================================
void 
StPPVertexFinder::CalibBeamLine(){
  LOG_INFO << "StPPVertexFinder::CalibBeamLine: activated saving high quality prim tracks for 3D fit of the beamLine"<<endm;
  mStudyBeamLineTracks = true;
}


//==========================================================
//==========================================================
int 
StPPVertexFinder::fit(StEvent* event) {
  LOG_INFO << "***** START FIT" << endm;
  if (mStudyBeamLineTracks) vertex3D->clearEvent();

  hA[0]->Fill(1);

  StEvent *mEvent = (StEvent *)  StMaker::GetChain()->GetInputDS("StEvent");
  assert(mEvent); 

  mTotEve++;
  eveID=event->id();
  LOG_INFO << "\n   @@@@@@   PPVertex::Fit START nEve="<<mTotEve<<"  eveID="<<eveID<<  endm;

  hL->SetTitle( "Vertex likelyhood, eveID=" + TString(eveID) );

  hA[0]->Fill(2);

  if(mToolkit==0) {    
   LOG_WARN <<"no Sti tool kit,  PPV is OFF"<<endm;
   return 0;
  }

 // get BTOF info

  if(mUseBtof) {
    StBTofCollection *btofColl = (StBTofCollection*)mEvent->btofCollection();
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

  
  StEmcCollection* emcC =(StEmcCollection*)mEvent->emcCollection(); 
  if(emcC==0) {
    LOG_WARN <<"no emcCollection , continue THE SAME eve"<<endm;
  } else {
    StEmcDetector* btow = emcC->detector( kBarrelEmcTowerId); 
    if(btow==0) {
      LOG_WARN <<"no BEMC in emcColl , continue THE SAME eve"<<endm;
    } else {
      bemcList->build(btow, mMinAdcBemc);
    }
    
    StEmcDetector* etow = emcC->detector(kEndcapEmcTowerId); 
    if(etow==0) {
      LOG_WARN <<"no EEMC in emcColl , continue THE SAME eve"<<endm;
    } else {
      eemcList->build(etow, mMinAdcEemc);
    }
  }

  //get the Sti track container...
  StiTrackContainer* tracks = mToolkit->getTrackContainer();
   if(tracks==0) {
     LOG_WARN <<"no STi tracks , skip eve"<<endm;
     printInfo();  
     return 0 ;				       
   }

  hA[0]->Fill(4);
  
  //select reasonable tracks and add them to my list
  int kBtof=0,kCtb=0,kBemc=0, kEemc=0,kTpc=0;
  int nmAny=0;

  std::array<int, 7> ntrk{};

  for (const StiTrack* stiTrack : *tracks)
  {
    const StiKalmanTrack* stiKalmanTrack = static_cast<const StiKalmanTrack*>(stiTrack);

    TrackData t;

    ntrk[0]++;

    if(stiKalmanTrack->getFlag() != true)        {ntrk[1]++; continue;}
    if(stiKalmanTrack->getPt() < mMinTrkPt)      {ntrk[2]++; continue;}
    if(mDropPostCrossingTrack &&
       isPostCrossingTrack(stiKalmanTrack))      {ntrk[3]++; continue;}  // kill if it has hits in wrong z
    if(!examinTrackDca(stiKalmanTrack, t))       {ntrk[4]++; continue;}  // drop from DCA
    if(!matchTrack2Membrane(stiKalmanTrack, t))  {ntrk[5]++; continue;}  // kill if nFitP too small

    ntrk[6]++;

    //cout << "\n#e gPt="<<stiKalmanTrack->getPt()
    //     << " gEta="<<stiKalmanTrack->getPseudoRapidity()
    //     << " nFitP="<<stiKalmanTrack->getFitPointCount() << " of " << stiKalmanTrack->getMaxPointCount()
    //     << " poolSize="<< mTrackData->size() << "  myW=" << t.weight << endl;
    //printf(" t.weight AA=%f\n", t.weight);

    hA[1]->Fill(stiKalmanTrack->getChi2());
    hA[2]->Fill(stiKalmanTrack->getFitPointCount());
    hA[16]->Fill(stiKalmanTrack->getPt());
    
    // ......... matcho various detectors ....................
    if(mUseBtof) matchTrack2BTOF(stiKalmanTrack, t, btofGeom);  // matching track to btofGeometry
    if(mUseCtb)  matchTrack2CTB(stiKalmanTrack, t);
    matchTrack2BEMC(stiKalmanTrack, t, 242); // middle of tower in Rxy
    matchTrack2EEMC(stiKalmanTrack, t, 288); // middle of tower in Z
    //.... all test done on this track .........
    t.mother = stiKalmanTrack;
    mTrackData.push_back(t); 

    hA[5]->Fill(t.rxyDca);

    if( t.mBtof>0 ) kBtof++;
    if( t.mCtb>0 )  kCtb++;   
    if( t.mBemc>0)  kBemc++;   
    if( t.mEemc>0)  kEemc++;
    if( t.mTpc>0 )  kTpc++;
 
    if(t.mBtof>0 || t.mCtb>0 || t.mBemc>0 || t.mEemc>0 || t.mTpc>0 ) nmAny++ ;

    hACorr->Fill(t.mBtof, t.mBemc);
  }

  LOG_INFO << "\n"
           << Form("PPV:: # of input track          = %d\n", ntrk[0])
           << Form("PPV:: dropped due to flag       = %d\n", ntrk[1])
           << Form("PPV:: dropped due to pt         = %d\n", ntrk[2])
           << Form("PPV:: dropped due to PCT check  = %d\n", ntrk[3])
           << Form("PPV:: dropped due to DCA check  = %d\n", ntrk[4])
           << Form("PPV:: dropped due to NHit check = %d\n", ntrk[5])
           << Form("PPV:: # of track after all cuts = %d",   ntrk[6]) << endm;

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

  LOG_INFO << "PPV::fit() nEve=" << mTotEve << " , "
           << nmAny << " traks with good DCA, matching: BTOF="
           << kBtof << " CTB=" << kCtb << " BEMC=" << kBemc << " EEMC=" << kEemc << endm;


  if(nmAny < mMinMatchTr && mStoreUnqualifiedVertex <= 0) {
    LOG_INFO << "StPPVertexFinder::fit() nEve=" << mTotEve << " Quit, to few matched tracks" << endm;
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
    VertexData V(++vertexID);
    if(! findVertexZ(V)) break;
  
    bool trigV = evalVertexZ(V);   // V.print();

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
      if(rank>1e6)     hA[17]->Fill(log(rank-1e6)+10);
      else if(rank>0)  hA[17]->Fill(log(rank));
      else             hA[17]->Fill(log(rank+1e6)-10);
    }

    if (mVertexFitMode == VertexFit_t::Beamline3D) {
       fitTracksToVertex(V);
    }
    
    mVertexData.push_back(V);
    if(trigV && mStudyBeamLineTracks) vertex3D->study(V.r,eveID);
  }

  LOG_INFO << "StPPVertexFinder::fit(totEve="<<mTotEve<<") "<<mVertexData.size()<<" vertices found, nBadVertex=" <<nBadVertex<< endm;
  
  if(mVertexData.size()>0)  hA[0]->Fill(8);
  if(mVertexData.size()>1)  hA[0]->Fill(9);

  exportVertices();
  printInfo();
  
  hA[4]->Fill(mVertexData.size());

  for (const VertexData &V : mVertexData)
  {
    hA[3]->Fill(V.r.z());
  }
  
  if(mVertexData.size()<=0) {
    return 0; // no vertex
  }
  
  return size();
} 


//==========================================================
//==========================================================
bool  
StPPVertexFinder::buildLikelihoodZ(){
  hL->Reset();
  hM->Reset();
  hW->Reset();

  float dzMax2=mMaxZradius*mMaxZradius;

  int nt=mTrackData.size();
  LOG_DEBUG<< Form("PPV::buildLikelihood() pool of nTracks=%d",nt)<<endm;
  if(nt<=0) return false;

  int n1=0;

  double *La=hL->GetArray(); // PPV main likelyhood histogram 
  double *Ma=hM->GetArray(); // track multiplicity  histogram 
  double *Wa=hW->GetArray(); // track weight histogram 
  
  // Loop over pre-selected tracks only
  for (const TrackData &t : mTrackData) {
    // Skip tracks already assigned to a vertex
    if(t.vertexID!=0) continue;

    if(t.anyMatch) n1++;
    //  t.print();
    float z0   = t.zDca;  // z coordinate at DCA
    float ez   = t.ezDca; // error on z coordinate at DCA
    float ez2  = ez*ez;
    int   j1   = hL->FindBin(z0-mMaxZradius-.1);
    int   j2   = hL->FindBin(z0+mMaxZradius+.1);
    float base = dzMax2/2/ez2;
    float totW = t.weight;
    //  printf("Z0=%f ez=%f j1=%d j2=%d base=%f gPt/GeV=%.3f ctbW=%.3f\n",z0,ez,j1,j2,base,t.gPt,ctbW);

    for (int j=j1; j<=j2; j++) {
      float z  = hL->GetBinCenter(j);
      float dz = z-z0;
      float xx = base - dz*dz/2/ez2;
      if(xx<=0) continue;
      La[j] += xx*totW;
      Ma[j] += 1.;
      Wa[j] += totW;
      // printf("z=%f dz=%f  xx=%f\n",z,dz,xx);
    }
    // break; // tmp , to get only one track
  }

 LOG_DEBUG<< Form("PPV::buildLikelihood() %d tracks w/ matched @ Lmax=%f",n1,hL->GetMaximum())<<endm;

  return (n1>=mMinMatchTr) ||  (mStoreUnqualifiedVertex>0 );
}

//==========================================================
//==========================================================
bool  
StPPVertexFinder::findVertexZ(VertexData &V) {

  if(hL->GetMaximum()<=0) return false; // no more tracks left

  int   iMax = hL->GetMaximumBin();
  float z0   = hL->GetBinCenter(iMax);
  float Lmax = hL->GetBinContent(iMax);
  float accM = hM->GetBinContent(iMax);
  float accW = hW->GetBinContent(iMax);
  assert(accM>0);
  float avrW = accW/accM;

  // search for sigma of the vertex
  float Llow = 0.9* Lmax;
  if ((Lmax-Llow) < 8*avrW )  Llow = Lmax - 8*avrW;  // to be at least 4 sigma

  double *L = hL->GetArray(); // likelyhood 

  int iLow = -1, iHigh = -1;
  for(int i=iMax; i<=hL->GetNbinsX(); i++) {
    if(L[i] > Llow) continue;
    iHigh = i;
    break;
  }
  for(int i=iMax; i>=1; i--) {
    if(L[i] > Llow) continue;
    iLow = i;
    break;
  }
  
  float zLow  = hL->GetBinCenter(iLow);
  float zHigh = hL->GetBinCenter(iHigh);

  float kSig = sqrt(2*(Lmax-Llow)/avrW);
  float sigZ = (zHigh-zLow)/2/kSig;

  LOG_DEBUG << Form("PPV:: iLow/iMax/iHigh=%d/%d/%d\n",iLow,iMax,iHigh)
            << Form(" accM=%f  accW=%f  avrW=%f\n",accM,accW,avrW)   
            << Form("  Z low/max/high=%f %f %f, kSig=%f, sig=%f\n",zLow,z0,zHigh,kSig,sigZ)
            << Form(" found  PPVertex(ID=%d,neve=%d) z0 =%.2f +/- %.2f\n",V.id,mTotEve,z0,sigZ)<<endm;

  if (sigZ < 0.1) sigZ = 0.1; // tmp, make it not smaller than the bin size

  // take x,y from beam line equation, TMP
  V.r  = TVector3(beamX(z0), beamY(z0), z0);
  V.er = TVector3(0.1, 0.1, sigZ); //tmp
  V.Lmax = Lmax;

  return true;
}

//==========================================================
//==========================================================
bool  
StPPVertexFinder::evalVertexZ(VertexData &V) { // and tag used tracks
  // returns true if vertex is accepted accepted
  if (mStudyBeamLineTracks) vertex3D->clearTracks();
  LOG_DEBUG << "StPPVertexFinder::evalVertex Vid="<<V.id<<" start ..."<<endm;
  int n1=0, nHiPt=0;
  
  for (TrackData &t : mTrackData)
  {
    // Skip tracks already matched to a vertex
    if(t.vertexID != 0) continue;

    // Do not match tracks to vertex V if they are too far in Z
    // (i.e. |delta_z| > (mMaxZradius + sigma_z))
    if(! t.matchVertex(V, mMaxZradius)) continue;

    // Otherwise, this track belongs to this vertex
    n1++;
    t.vertexID  = V.id;
    V.gPtSum   += t.gPt;
    if (mStudyBeamLineTracks) vertex3D->addTrack(&t);
    if( t.gPt>mCut_oneTrackPT && ( t.mBemc>0|| t.mEemc>0) ) nHiPt++;

    if(  t.mTpc>0)       V.nTpc++;
    else if (  t.mTpc<0) V.nTpcV++;

    if(  t.mBtof>0)       V.nBtof++;
    else if (  t.mBtof<0) V.nBtofV++;

    if(  t.mCtb>0)       V.nCtb++;
    else if (  t.mCtb<0) V.nCtbV++;

    if(  t.mBemc>0)       V.nBemc++;
    else if (  t.mBemc<0) V.nBemcV++;

    if(  t.mEemc>0)       V.nEemc++;
    else if (  t.mEemc<0) V.nEemcV++;

    if( t.anyMatch)     V.nAnyMatch++;
    else if (t.anyVeto) V.nAnyVeto++;
  } 

  V.nUsedTrack = n1;  

  bool validVertex = (V.nAnyMatch >= mMinMatchTr) || ( (mAlgoSwitches & kSwitchOneHighPT) && nHiPt>0 );

  if (!validVertex) { // discrad vertex
    //no match tracks in this vertex, tag vertex ID in tracks differently
    //V.print(cout);
    LOG_DEBUG << "StPPVertexFinder::evalVertex Vid="<<V.id<<" rejected"<<endm;
    for (TrackData &t : mTrackData) {
      if(t.vertexID!=V.id) continue;
      t.vertexID=-V.id;
    }
    return false;
  }
  
  LOG_INFO << "StPPVertexFinder::evalVertex Vid="<<V.id<<" accepted, nAnyMatch="<<V.nAnyMatch<<" nAnyVeto="<<V.nAnyVeto<<endm;
  return true;
}


/**
 * Creates DCA states for selected tracks (mTrackData) and fills the static
 * container sDCAs. The tracks in mTrackData must be already associated with
 * a corresponding vertex, i.e. we check that track.vertexID == vertex.id
 *
 * \author Dmitri Smirnov, BNL
 * \date April, 2016
 */
void StPPVertexFinder::createTrackDcas(const VertexData &vertex) const
{
   // Fill static array of pointers to StDcaGeometry objects for selected tracks
   // in mTrackData corresponding to this vertex. These will be used in static
   // minimization function
   while (!sDCAs().empty()) delete sDCAs().back(), sDCAs().pop_back();


   for (const TrackData & track : mTrackData)
   {
      if (track.vertexID != vertex.id) continue;
      if (!track.mother) continue;

      // This code is adopted from StiStEventFiller::fillDca()
      StiKalmanTrack tmpTrack = *track.mother;
      StiKalmanTrackNode *tNode = tmpTrack.extrapolateToBeam();

      if (!tNode) continue;

      const StiNodePars &pars = tNode->fitPars();
      const StiNodeErrs &errs = tNode->fitErrs();
      float alfa = tNode->getAlpha();
      Float_t setp[7] = {(float)pars.y(),    (float)pars.z(),    (float)pars.phi()
                        ,(float)pars.ptin(), (float)pars.tanl(), (float)pars.curv(), (float)pars.hz()};
      setp[2]+= alfa;
      Float_t sete[15];
      for (int i=1,li=1,jj=0;i< kNPars;li+=++i) {
        for (int j=1;j<=i;j++) {sete[jj++]=errs.G()[li+j];}}

      StDcaGeometry* dca = new StDcaGeometry();
      dca->set(setp, sete);
      sDCAs().push_back(dca);
   }
}


/**
 * Takes a list of vertex candidates/seeds and updates each vertex position by
 * fitting tracks pointing to it. The fit is performed by minimizing the chi2
 * robust potential. The method uses the base class static container with track
 * DCAs as input.
 *
 * \author Dmitri Smirnov, BNL
 * \date February, 2016
 */
void StPPVertexFinder::fitTracksToVertex(VertexData &vertex) const
{
   createTrackDcas(vertex);

   if (sDCAs().size() == 0) {
      LOG_WARN << "StPPVertexFinder::fitTracksToVertex: At least one track is required. "
               << "This vertex (id = " << vertex.id << ") coordinates will not be updated" << endm;
      return;
   }

   // Recalculate vertex seed coordinates to be used as initial point in the fit
   StThreeVectorD vertexSeed = StGenericVertexFinder::CalcVertexSeed(sDCAs());

   // For fits with beamline force the seed to be on the beamline
   if ( mVertexFitMode == VertexFit_t::Beamline1D ||
        mVertexFitMode == VertexFit_t::Beamline3D )
   {
      vertexSeed.setX( beamX(vertexSeed.z()) );
      vertexSeed.setY( beamY(vertexSeed.z()) );
   }

   static TMinuit minuit(3);

   minuit.SetFCN(&StGenericVertexFinder::fcnCalcChi2DCAsBeamline);
   minuit.SetPrintLevel(-1);
   minuit.SetMaxIterations(1000);

   int minuitStatus;

   minuit.mnexcm("clear", 0, 0, minuitStatus);

   static double step[3] = {0.01, 0.01, 0.01};

   double x_lo = vertexSeed.x() - mMaxTrkDcaRxy;
   double y_lo = vertexSeed.y() - mMaxTrkDcaRxy;
   double z_lo = vertexSeed.z() - mMaxZradius;

   double x_hi = vertexSeed.x() + mMaxTrkDcaRxy;
   double y_hi = vertexSeed.y() + mMaxTrkDcaRxy;
   double z_hi = vertexSeed.z() + mMaxZradius;

   minuit.mnparm(0, "x", vertexSeed.x(), step[0], x_lo, x_hi, minuitStatus);
   minuit.mnparm(1, "y", vertexSeed.y(), step[1], y_lo, y_hi, minuitStatus);
   minuit.mnparm(2, "z", vertexSeed.z(), step[2], z_lo, z_hi, minuitStatus);

   minuit.mnexcm("minimize", 0, 0, minuitStatus);

   // Check fit result
   if (minuitStatus) {
      LOG_WARN << "StPPVertexFinder::fitTracksToVertex: Fit did not converge. "
	       << "Check TMinuit::mnexcm() status flag: " << minuitStatus << ". "
               << "This vertex (id = " << vertex.id << ") coordinates will not be updated" << endm;
      return;
   }

   double chisquare, fedm, errdef;
   int npari, nparx;

   minuit.mnstat(chisquare, fedm, errdef, npari, nparx, minuitStatus);
   minuit.mnhess();

   double emat[9];
   /* 0 1 2
      3 4 5
      6 7 8 */
   minuit.mnemat(emat, 3);

   vertex.r.SetXYZ(minuit.fU[0], minuit.fU[1], minuit.fU[2]);
   vertex.er.SetXYZ( sqrt(emat[0]), sqrt(emat[4]), sqrt(emat[8]) );
}

 
//-------------------------------------------------
//-------------------------------------------------
void 
StPPVertexFinder::exportVertices(){
  if ( mVertexFitMode != VertexFit_t::Beamline1D &&
       mVertexFitMode != VertexFit_t::Beamline3D )
  {
    // code is not ready for reco w/o beamLine
    LOG_FATAL << "StPPVertexFinder code is not ready for reco w/o beamLine" << endm;
    assert(mVertexFitMode == VertexFit_t::Beamline1D ||
           mVertexFitMode == VertexFit_t::Beamline3D);
  }

  for (const VertexData &V : mVertexData)
  {
    StThreeVectorD r(V.r.x(),V.r.y(),V.r.z());

    float cov[6]{};

    cov[0]=V.er.x()*V.er.x();
    cov[2]=V.er.y()*V.er.y();
    cov[5]=V.er.z()*V.er.z();  // [5] is correct,JB

    StPrimaryVertex primV;
    primV.setPosition(r);
    primV.setCovariantMatrix(cov); 
    primV.setVertexFinderId(mUseCtb ? ppvVertexFinder : ppvNoCtbVertexFinder);
    primV.setNumTracksUsedInFinder(V.nUsedTrack);
    primV.setNumMatchesWithBTOF(V.nBtof);
    primV.setNumMatchesWithCTB(V.nCtb);
    primV.setNumMatchesWithBEMC(V.nBemc);
    primV.setNumMatchesWithEEMC(V.nEemc);
    primV.setNumTracksCrossingCentralMembrane(V.nTpc);
    primV.setSumOfTrackPt(V.gPtSum);
    primV.setRanking(V.Lmax);
    primV.setFlag(1); //??? is it a right value?
  
    //..... add vertex to the list
    addVertex(primV);
  }
  LOG_DEBUG << "StPPVertexFinder::exportVertices(), size="<<size()<<endm;
}

//-------------------------------------------------
//-------------------------------------------------
void 
StPPVertexFinder::Finish() {

  if (mStudyBeamLineTracks) { // save local histo w/ PPV monitoring
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
void 
StPPVertexFinder::saveHisto(TString fname){
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
void  
StPPVertexFinder::dumpKalmanNodes(const StiKalmanTrack*track){
   
  //.................... print all nodes ...........
  StiKTNBidirectionalIterator it;
  int in=0,nh=0,nTpc=0;
  float zL=999, zH=-999;
  for (it=track->begin();it!=track->end();it++,in++) {
    StiKalmanTrackNode& ktn = (*it);
    if(!ktn.isValid()) continue;
    if(ktn.getHit() && ktn.getChi2() >1000) continue;
    const StiDetector * det=ktn.getDetector();
    assert(!(ktn.x()) || det);
    float rxy=ktn.getX();
    bool actv= !det || det->isActive(ktn.getY(), ktn.getZ());
    if(!det || (rxy>58 && rxy < 190)){
      float z=ktn.z_g();
      if(zL>z) zL=z;
      if(zH<z) zH=z;
      if(actv) {
	nTpc++;
	if(ktn.getHit()) nh++;
      }
    }
  }
  int nn=in;
  TString tagPlp=" "; if((nTpc-nh)>10) tagPlp=" plp";
  TString tagMemb=" "; if(zH*zL<0) tagMemb=" memb";

  LOG_INFO
    <<"\n#e dumpKalmanNodes nNodes="<<nn<<" actv: nTPC="<<nTpc<<" nHit="<<nh
    <<" zL="<<zL<<" zH="<<zH <<tagPlp<<tagMemb
    <<endm;
 
  // ........................print both ends  ....................
  LOG_INFO << "#e  |P|="<<track->getP()<<" pT="<<track->getPt()<<" eta="<<track->getPseudoRapidity()<<" nFitP="<<track->getFitPointCount() << endm; 
  StiKalmanTrackNode* inNode=track->getInnerMostNode();
  LOG_INFO << "#e @InnerMostNode x:"<< inNode->x_g()<<" y:"<< inNode->y_g()<<" z:"<< inNode->z_g()<<" Eta="<<inNode->getEta()<<" |P|="<<inNode->getP() << endm;
  StiKalmanTrackNode* ouNode=track->getOuterMostNode();
  LOG_INFO << "#e @OuterMostNode g x:"<< ouNode->x_g()<<" y:"<< ouNode->y_g()<<" z:"<< ouNode->z_g()<<" Eta="<<ouNode->getEta()<<" |P|="<<ouNode->getP() << endm;

  in=0;
  for (it=track->begin();it!=track->end();it++,in++) {
    // if(in>=2 && in<nn-5) continue; // print only ends of the track
    StiKalmanTrackNode& ktn = (*it);
    if(!ktn.isValid()) continue;
    if(ktn.getHit() && ktn.getChi2() >1000) continue;
    float sy=sqrt(ktn.getCyy());
    float sz=sqrt(ktn.getCzz());
    const StiDetector * det=ktn.getDetector();
    assert(!(ktn.x()) || det);

    LOG_INFO << "#e in="<<in<<" |P|="<<ktn.getP()<<" Local: x="<<ktn.getX()<<" y="<<ktn.getY()<<" +/- "<<sy<<" z="<<ktn.getZ()<<" +/- "<<sz;

    if(ktn.getHit()) {LOG_INFO <<" hit=1";}
    else             {LOG_INFO <<" hit=0";}

    if(det==0)       {LOG_INFO <<" noDet ";}
    else             {LOG_INFO <<" detActv="<<(!det || det->isActive(ktn.getY(), ktn.getZ()));}
    LOG_INFO << endm;
  }
}

//==========================================================
//==========================================================
bool  
StPPVertexFinder::examinTrackDca(const StiKalmanTrack* track, TrackData &t){

  //1 StiKalmanTrackNode* inNode=track->getInnerMostNode();
  //1 cout <<"#e  track->getPseudoRapidity()="<<track->getPseudoRapidity()<<" track->getFitPointCount()="<<track->getFitPointCount()<<endl;
  
  // .......... test DCA to beam .............
  StiKalmanTrack track1=*track; // clone track
  StiKalmanTrackNode* bmNode=track1.extrapolateToBeam();
  if(bmNode==0)  { 
    //1 cout<<"#a @beam  DCA NULL"<<endl; 
    return false ; 
  }

  float rxy=rxyG(bmNode);

  //1 cout<<"#e @beam global DCA x:"<< bmNode->x_g()<<" y:"<< bmNode->y_g()<<" z:"<< bmNode->z_g()<<" Rxy="<< rxy <<endl;
  if(rxy>mMaxTrkDcaRxy) return false;
  if( fabs(bmNode->z_g())> mMaxZrange )   return false ; 
 
  //1 cout<<"#e inBeam |P|="<<bmNode->getP()<<" pT="<<bmNode->getPt()<<" local x="<<xL(bmNode)<<" y="<<yL(bmNode)<<" +/- "<<eyL(bmNode)<<" z="<<zL(bmNode)<<" +/- "<<ezL(bmNode)<<endl;

  t.zDca   = zL(bmNode);
  t.ezDca  = ezL(bmNode);
  t.rxyDca = rxy;
  t.gPt    = bmNode->getPt();

  //...... record more detals for 3D vertex reco
  t.dcaTrack.R.SetXYZ(xG(bmNode),yG(bmNode),zG(bmNode));
  // approximation below: use sigX=sigY, I do not want to deal wih rotations in X-Y plane, Jan B.
  t.dcaTrack.sigYloc = eyL(bmNode);
  t.dcaTrack.sigZ    = ezL(bmNode);
  StThreeVectorF const globP3 = bmNode->getGlobalMomentumF();
  t.dcaTrack.gP.SetXYZ(globP3.x(),globP3.y(),globP3.z());
  t.dcaTrack.fitErr    = bmNode->fitErrs();
  t.dcaTrack.gChi2     = track1.getChi2();
  t.dcaTrack.nFitPoint = track1.getFitPointCount();

  return true;
}


//==========================================================
//==========================================================
void  
StPPVertexFinder::matchTrack2BTOF(const StiKalmanTrack* track,TrackData &t,StBTofGeometry* geom){

  StiKalmanTrackNode* ouNode=track->getOuterMostNode();

  StThreeVectorD posTOF;
  // helix extrapolation:
  StThreeVectorD ou(ouNode->getX(),ouNode->getY(),ouNode->getZ());
  ou.rotateZ(ouNode->getAlpha());
  StPhysicalHelixD hlx(fabs(ouNode->getCurvature()),
                       ouNode->getDipAngle(),
                       ouNode->getPhase(),
                       ou,
                       ouNode->getHelicity());
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
void  
StPPVertexFinder::matchTrack2CTB(const StiKalmanTrack* track,TrackData &t){
  const double Rctb=213.6; // (cm) radius of the CTB 

  StiKalmanTrackNode* ouNode=track->getOuterMostNode();

  StThreeVectorD posCTB;
  float path=-1;
  //alternative helix extrapolation:
  if(1){
    StiKalmanTrackNode * inNode = ouNode;
    StThreeVectorD in(inNode->getX(),inNode->getY(),inNode->getZ());
    in.rotateZ(inNode->getAlpha());
    StPhysicalHelixD hlx(fabs(inNode->getCurvature()),
			 inNode->getDipAngle(),
			 inNode->getPhase(),
			 in,
			 inNode->getHelicity());
    pairD  d2;
    d2 = hlx.pathLength(Rctb);
    path=d2.second;
    if(d2.first>=0 || d2.second<=0) {
      LOG_DEBUG <<Form("WARN MatchTrk , unexpected solution for track crossing CTB\n")<<
	Form(" d2.firts=%f, second=%f, try first", d2.first, d2.second)<<endm;
      path=d2.first;
    }
    posCTB = hlx.at(path);
  }

  // official Sti node extrapolation
  if(0){
    StiKalmanTrack track2=*track;
    StiKalmanTrackNode* ctbNode=track2.extrapolateToRadius(Rctb);

    if(ctbNode==0)  { 
      LOG_INFO <<"#e @ctbNode NULL"<<endm;
      LOG_INFO <<"#e @track dump"<< *track << endm;
      LOG_INFO <<"#e @OuterMostNode dump"<< *ouNode <<endm;
      return; 
    }
    
    posCTB=StThreeVectorD( ctbNode->x_g(),ctbNode->y_g(),ctbNode->z_g());
  }

  float phi=atan2(posCTB.y(),posCTB.x());
  if(phi<0) phi+=2*M_PI;// now phi is [0,2Pi] as for CTB slats
  float eta=posCTB.pseudoRapidity();
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
void  
StPPVertexFinder::matchTrack2BEMC(const StiKalmanTrack* track,TrackData &t, float Rxy){
  
  StiKalmanTrackNode* ouNode=track->getOuterMostNode();

  //alternative helix extrapolation:
  StThreeVectorD ou(ouNode->getX(),ouNode->getY(),ouNode->getZ());
  ou.rotateZ(ouNode->getAlpha());
  StPhysicalHelixD hlx(fabs(ouNode->getCurvature()),
		       ouNode->getDipAngle(),
		       ouNode->getPhase(),
		       ou,
		       ouNode->getHelicity());
  pairD  d2;
  d2 = hlx.pathLength(Rxy);
  float path = d2.second;

  if(d2.first>=0 || d2.second<=0) {
    LOG_DEBUG <<Form("WARN MatchTrk , unexpected solution for track crossing BEMC Cyl\n")<<
      Form(" d2.firts=%f, second=%f, try first\n", d2.first, d2.second)<<endm;
    path=d2.first;
  }

  StThreeVectorD posCyl = hlx.at(path);


  float phi=atan2(posCyl.y(),posCyl.x());
  if(phi<0) phi+=2*M_PI;// now phi is [0,2Pi] as for Cyl slats
  float eta=posCyl.pseudoRapidity();
  

  if(fabs(eta)<1) hA[11]->Fill(posCyl.z());
  
  int iBin=bemcList->addTrack(eta,phi);
  bool  bemcMatch=bemcList->isMatched(iBin);
  bool  bemcVeto =bemcList->isVetoed(iBin);
  float bemcW    =bemcList->getWeight(iBin);

  t.updateAnyMatch(bemcMatch,bemcVeto,t.mBemc);
  t.weight*=bemcW;
  t.bemcBin=iBin;

}


//==========================================================
//==========================================================
void  
StPPVertexFinder::matchTrack2EEMC(const StiKalmanTrack* track,TrackData &t,float z){
  
  const float minEta=0.7 ;// tmp cut
  const float maxPath=200 ;// tmp, cut too long extrapolation

  StiKalmanTrackNode* ouNode=track->getOuterMostNode();
  StiKalmanTrackNode* inNode=track->getInnerMostNode();

  //direction of extrapolation must be toward West (Z+ axis)
  if(inNode->getZ()> ouNode->getZ()) return;
  
  // droop too steep tracks
  if(track->getPseudoRapidity()<minEta) return;

  StThreeVectorD rSmd=StThreeVectorD(0,0,z); 
  StThreeVectorD n=StThreeVectorD(0,0,1);

  StThreeVectorD ou(ouNode->getX(),ouNode->getY(),ouNode->getZ());
  ou.rotateZ(ouNode->getAlpha());
  StPhysicalHelixD hlx(fabs(ouNode->getCurvature()),
		       ouNode->getDipAngle(),ouNode->getPhase(),
		       ou,ouNode->getHelicity());

   // path length at intersection with plane
   // double       pathLength(const StThreeVectorD& r,
   //                         const StThreeVectorD& n) const;

  double path = hlx.pathLength(rSmd,n);
  if(path>maxPath) return; // too long extrapolation

  StThreeVectorD r = hlx.at(path);
  float periodL=hlx. period();
 
  if(periodL<2*path) {
    LOG_DEBUG <<Form(" Warn, long path fac=%.1f ",path/periodL)<<
      Form("  punchEEMC1 x,y,z=%.1f, %.1f, %.1f path=%.1f period=%.1f\n",r.x(),r.y(),r.z(),path,periodL)<<endm; 
  }

  float phi=atan2(r.y(),r.x());
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
bool  
StPPVertexFinder::matchTrack2Membrane(const StiKalmanTrack* track,TrackData &t){
  const float RxyMin=59, RxyMax=199, zMax=200;
  const float zMembraneDepth=1; // (cm) ignore signe change for nodes so close to membrane

  //generate bitt pattern for TPC nodes with hits 
  std::vector<int> hitPatt;
  int nPos=0,nFit=0;
  int in=0;
  float lastRxy=9999;
  float lastZ=9999;

  int jz0=0;
  StiKTNBidirectionalIterator it;
  for (it=track->begin();it!=track->end();it++) {
    StiKalmanTrackNode* ktnp=& (*it);
    if(!ktnp->isValid()) continue;
    //if(ktnp->getHit() && ktnp->getChi2() >1000) continue; // ---> those track need to be counted as npossiblehit, commented out
    float rxy=rxyG(ktnp); //ktn.getX();
    float z=zG(ktnp);  //ktn.z_g();
    if(rxy<RxyMin) continue;
    if(rxy>RxyMax) continue;
    if(fabs(z)>zMax) continue;
    // .........node is within TPC fiducial volume
    if(lastRxy<=rxy){
      LOG_WARN << "StPPVertexFinder::matchTrack2Membrane() \n the "<<in<<" node of the kalmanTrack below is out of order and is ignorred in (some) of vertex finder analysis"<<"\n  Z="<<z<<" rXY="<<rxy<<" last_rxy="<<lastRxy<<endm;
      continue;
    }
    lastRxy=rxy;
    if(in==0) lastZ=z;
    in++;
    if(fabsf(z)>zMembraneDepth) { //ignore hits too close to z=0
      if(lastZ*z<0) {             // track just crossed Z=0 plane
	if(jz0>0) {
	  LOG_WARN << "StPPVertexFinder::matchTrack2Membrane() \n the "<<in<<" node of the kalmanTrack crosses Z=0 for the 2nd time, this track has a strange list of nodes - continue"<<endm;
	}
	//assert(jz0==0); // only one crosss point is expected
	jz0=hitPatt.size();
      }
      lastZ=z;
    }
    const StiDetector * det=ktnp->getDetector();
    assert(!(ktnp->x()) || det);
    bool active=!det || det->isActive(yL(ktnp), zL(ktnp));
    int hit=ktnp->getHit()?1:0;
    if(active) {
      hitPatt.push_back(hit);
      nPos++;
      if(hit && ktnp->getChi2() <=1000 ) nFit++;
    }
  }

  if(nFit<  mMinFitPfrac  * nPos) return false; // too short fragment of a track

  if( mFitPossWeighting)
    t.weight*=1.*nFit/nPos;// introduced in 2012 for pp510 to differentiate between global track quality, together with lowering the overall threshold from 0.7 to 0.51, Jan
  
  t.scanNodes(hitPatt,jz0); // if central membrane is crossed, scale weight inside


  return true;
}

//==========================================================
//==========================================================

/**
 * Identifies tracks coming from post bunch crossing collisions.
 */
bool StPPVertexFinder::isPostCrossingTrack(const StiKalmanTrack* track){
  const float RxyMin=59, RxyMax=199, zMax=200;
  const float zMembraneDepth=1.0; 
  const int   nWrongZHitCut=2;
  int nWrongZHit=0;
  StiKTNBidirectionalIterator it;
  for (it=track->begin();it!=track->end();it++) {
    StiKalmanTrackNode* ktnp=& (*it);
    if(!ktnp->isValid() || ktnp->getChi2()>1000 ) continue;
    StiHit* stihit=ktnp->getHit();

    if (!stihit) continue;

    StHit* sthit=(StHit*)stihit->stHit();

    if (!sthit) continue;
    if (sthit->detector() != kTpcId) continue;

    StTpcHit* hit=(StTpcHit*) sthit;
    float r=hit->position().perp();
    if (r < RxyMin) continue;
    if (r > RxyMax) continue;

    float z=hit->position().z();
    if (fabs(z) > zMax) continue;

    if ((z < -zMembraneDepth && hit->sector() <= 12) ||
        (z >  zMembraneDepth && hit->sector() >  12))
    {
      nWrongZHit++;
      if (nWrongZHit >= nWrongZHitCut) {return true;}
    }
  }
  return false;
}
