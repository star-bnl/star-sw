/************************************************************
 *
 * $Id: StPPVertexFinder.cxx,v 1.120 2017/10/03 21:25:15 genevb Exp $
 *
 * Author: Jan Balewski
 ************************************************************
 *
 * Description:  does not fear any pileup
 ************************************************************
 *
 * $Log: StPPVertexFinder.cxx,v $
 * Revision 1.120  2017/10/03 21:25:15  genevb
 * Proper reporting/usage of useBTOFmatchOnly, plus LOGGER usage bugs fixed
 *
 * Revision 1.119  2017/08/04 21:14:55  genevb
 * Remove memory leak of StEmcCollection (RT 3303)
 *
 * Revision 1.118  2017/05/30 18:27:19  smirnovd
 * StPPvertexFinder: Removed overlooked reference to a debug histogram
 *
 * See commit 312bb0f6 "StPPVertexFinder: Do not fill debug histograms"
 *
 * Revision 1.117  2017/05/24 05:02:05  genevb
 * Options for number of unqualified verts to store, and using only BTOF-matched tracks
 *
 *
 ************************************************************/
   
#include <St_base/StMessMgr.h>

#include "TFile.h"
#include "TH1D.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TObjArray.h"

#include <tables/St_g2t_vertex_Table.h> // tmp for Dz(vertex)

#include "StGenericVertexMaker/StiPPVertex/StPPVertexFinder.h"
#include "StGenericVertexMaker/StGenericVertexMaker.h"
#include "StGenericVertexMaker/Minuit/St_VertexCutsC.h"
#include "StEvent/StDcaGeometry.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StEnumerations.h"
#include "StEvent/StTrack.h"
#include "StMuDSTMaker/COMMON/StMuBTofHit.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"

#include <Sti/StiToolkit.h>
#include <Sti/StiKalmanTrack.h>
#include <Sti/StiKalmanTrackNode.h>
#include <Sti/StiTrackContainer.h>
#include "Sti/StiTrack.h"

#include <St_db_Maker/St_db_Maker.h>
#include <StIOMaker/StIOMaker.h> // to save  local histos 
#include <StBFChain/StBFChain.h>

#include "StGenericVertexMaker/StiPPVertex/BtofHitList.h"
#include "StGenericVertexMaker/StiPPVertex/CtbHitList.h"
#include "StGenericVertexMaker/StiPPVertex/BemcHitList.h"
#include "StGenericVertexMaker/StiPPVertex/EemcHitList.h"

#include "StEvent/StEmcCollection.h"
#include "StEvent/StBTofCollection.h"
#include "StBTofUtil/StBTofGeometry.h"

//==========================================================
//==========================================================

StPPVertexFinder::StPPVertexFinder(VertexFit_t fitMode) :
  StGenericVertexFinder(SeedFinder_t::PPVLikelihood, fitMode),
  mTrackData(), mVertexData(),
  mTotEve(0), eveID(0), nBadVertex(0),
  mAlgoSwitches(kSwitchOneHighPT),
  hA{}, hACorr(nullptr), hL(nullptr), hM(nullptr), hW(nullptr),
  HList(),
  ntrk{},
  mMinTrkPt(0.2),
  mMaxTrkDcaRxy(3.),
  mMaxZradius(3.),
  mMinMatchTr(5),
  mMaxZrange(200.),
  mMinZBtof(-3.),
  mMaxZBtof(3.),
  mMinAdcBemc(8),
  mMinAdcEemc(5),
  mMinFitPfrac(0.51),
  mFitPossWeighting(false),
  mDropPostCrossingTrack(true), // default PCT rejection on
  mStoreUnqualifiedVertex(5),
  mCut_oneTrackPT(10.),
  mUseBTOFmatchOnly(false),
  mToolkit(nullptr),
  btofList(nullptr),
  ctbList(nullptr),
  bemcList(new BemcHitList()),
  eemcList(new EemcHitList()),
  mStMuDst(nullptr)
{
  mDebugLevel = 1;
  mUseCtb = true;                      // default CTB is in the data stream
  mVertexOrderMethod = orderByRanking; // change ordering by ranking

  // special histogram for finding the vertex, not to be saved
  int   nb = 5000;
  float zRange = 250; // (cm)

  hL = new TH1D("ppvL", "Vertex likelyhood; Z /cm", nb, -zRange, zRange);
  // needed only for  better errZ calculation
  hM = new TH1D("ppvM", "cumulative track multiplicity; Z /cm", nb, -zRange, zRange);
  hW = new TH1D("ppvW", "cumulative track weight; Z /cm", nb, -zRange, zRange);
} 


//==========================================================
//==========================================================
void StPPVertexFinder::Init()
{
  assert(mTotEve==0); // can't be called twice
  LOG_INFO << Form("PPV-algo  switches=0x%0x,  following cuts have been activated:",mAlgoSwitches)<<endm;

  //get pointer to Sti toolkit
  mToolkit = StiToolkit::instance();
  
  // BTOF and/or CTB hits can be requested after the finder is constructed but
  // before the Init() is called. In this case we need to create the
  // corresponding hit lists if they are not available
  if (mUseBtof && !btofList) {
     btofList = new BtofHitList();
  }

  if (mUseCtb && !ctbList)  {
     ctbList  = new CtbHitList();
  }

  LOG_INFO << "Finished Init" << endm;
}

//==========================================================
//==========================================================
void StPPVertexFinder::InitRun(int run_number, const St_db_Maker* db_maker)
{
  StGenericVertexFinder::InitRun(run_number, db_maker);

  int dateY = db_maker->GetDateTime().GetYear();

  //.. set various params 
  // It is not clear why one would hard code cuts for any specific run or
  // a period since they can be set in the database. Here we'll assume that for
  // Runs 5 to 12 the PPV cuts are optimized and there is no need to access the
  // values from the database.
  if (run_number >= 6000000 && run_number < 13000000) {
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

  if(dateY<2006) {
    mMinAdcBemc   = 15;   // BTOW used calibration of maxt Et @ ~27Gev 
  } else {
    mMinAdcBemc   = 8;    // BTOW used calibration of maxt Et @ ~60Gev 
  }

  // Unfortunately, forced to remove const...
  St_db_Maker* st_db_maker = const_cast<St_db_Maker*>(db_maker);

  if (mUseBtof) btofList->initRun(st_db_maker);
  if (mUseCtb)  ctbList->initRun();

  bemcList->initRun(st_db_maker);
  eemcList->initRun(st_db_maker);

  LOG_INFO << "PPV::cuts "
           << "\n MinNumberOfFitPointsOnTrack = unused"
           << "\n MinFitPfrac=nFit/nPos  = " << mMinFitPfrac
           << "\n MaxTrkDcaRxy/cm= " << mMaxTrkDcaRxy
           << "\n MinTrkPt GeV/c = " << mMinTrkPt
           << "\n MinMatchTr of prim tracks = " << mMinMatchTr
           << "\n MaxZrange (cm)for glob tracks = " << mMaxZrange
           << "\n MaxZradius (cm) for prim tracks &Likelihood  = " << mMaxZradius
           << "\n Min/Max Z position for BTOF hit = " << mMinZBtof << " " << mMaxZBtof
           << "\n MinAdcBemc for MIP = " << mMinAdcBemc
           << "\n MinAdcEemc for MIP = " << mMinAdcEemc
           << "\n bool  useCtb = " << mUseCtb
           << "\n bool useBtof = " << mUseBtof
           << "\n bool useBTOFmatchOnly = " << mUseBTOFmatchOnly
           << "\n bool nFit/nPoss weighting = " << mFitPossWeighting
           << "\n bool DropPostCrossingTrack = " << mDropPostCrossingTrack
           << "\n Store # of UnqualifiedVertex = " << mStoreUnqualifiedVertex
           << "\n Store=" << (mAlgoSwitches & kSwitchOneHighPT)
           << " oneTrack-vertex if track PT/GeV>" << mCut_oneTrackPT << endm;
}


//==========================================================
//==========================================================
void StPPVertexFinder::initHisto()
{
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

  for (int i=0; i<mxH; i++) if(hA[i]) HList.Add(hA[i]);

  HList.Add(hACorr);
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


/**
 * Searches for vertex candidates by building a likelihood distribution for
 * track's DCA z values. The likelihood histogram is filled in
 * buildLikelihoodZ() using weighted contributions based on the track DCAs'
 * along the `z` axis. The maximum peak of the likelihood histogram is assumed
 * to correspond to the best found vertex candidate, aka seed. Once the seed is
 * identified the tracks are associated with this seed in evalVertexZ(). The
 * tracks already associated with a vertex are not considered in the next
 * iteration when a new likelihood histogram is built.
 */
void StPPVertexFinder::findSeeds_PPVLikelihood()
{
  const float par_rankOffset = 1e6; // to separate class of vertices (approximately)

  int vertexID=0;

  while(1)
  {
    if ( !buildLikelihoodZ() ) break;

    VertexData vertex(++vertexID);

    if ( !findVertexZ(vertex) ) break;

    bool trigV = evalVertexZ(vertex);   // vertex.print();

    //bump up rank of 2+ track all vertices 
    if (vertex.nAnyMatch >= mMinMatchTr) vertex.Lmax += par_rankOffset;

    if (!trigV) {
      // Ignore this "bad" vertex
      if (nBadVertex >= mStoreUnqualifiedVertex) continue;
      // ... or keep it
      nBadVertex++;
      // ... and bump down rank of sub-prime vertices 
      vertex.Lmax -= par_rankOffset; 
    }

    mVertexData.push_back(vertex);
  }
}


//==========================================================
//==========================================================
void StPPVertexFinder::Clear()
{
  LOG_INFO << "StPPVertexFinder::Clear(): Finished event " << mTotEve
           << ": Found " << mVertexData.size()-nBadVertex << " \"good\" and "
           << nBadVertex << " \"bad\" vertices" << endm;

  StGenericVertexFinder::Clear();

  if (btofList) btofList->clear();
  if (ctbList)  ctbList->clear();
  bemcList->clear();
  eemcList->clear();

  mTrackData.clear();
  mVertexData.clear();
  eveID = -1;
  nBadVertex = 0;

  ntrk.fill(0);

  // the clear below is not needed but cleans up stale result
  hL->Reset();
  hM->Reset();
  hW->Reset();
}


//======================================================
//======================================================
void StPPVertexFinder::printInfo(ostream& os) const
{
  LOG_INFO << "\n"
           << Form("PPV:: # of input track          = %d\n", ntrk[0])
           << Form("PPV:: dropped due to flag/dummy = %d\n", ntrk[1])
           << Form("PPV:: dropped due to pt         = %d\n", ntrk[2])
           << Form("PPV:: dropped due to PCT check  = %d\n", ntrk[3])
           << Form("PPV:: dropped due to DCA check  = %d\n", ntrk[4])
           << Form("PPV:: dropped due to NHit check = %d\n", ntrk[5])
           << Form("PPV:: dropped due to TOF check  = %d\n", ntrk[6])
           << Form("PPV:: # of track after all cuts = %d",   ntrk[7]) << endm;

  if(mUseBtof) btofList->print();
  if(mUseCtb)  ctbList->print();

  bemcList->print();
  eemcList->print();


  os << "StPPVertexFinder ver=1 - Fit Statistics:\n"
     << "StPPVertexFinder::result " << mVertexData.size() << " vertices found" << std::endl;

  int nTpcM=0, nTpcV=0;
  int k=0;
  for (const TrackData &track : mTrackData) {
    if(  track.mTpc>0)   nTpcM++;
    else if (  track.mTpc<0) nTpcV++;
    if(track.vertexID<=0) continue; // skip not used or pileup vertex
    k++;
    LOG_DEBUG 
      << Form("%d track@z0=%.2f +/- %.2f gPt=%.3f vertID=%d match:  bin,Fired,Track:\n",
              k,track.zDca,track.ezDca,track.gPt,track.vertexID);

    if (mUseBtof) { LOG_DEBUG 
      << Form("    Btof %3d,%d,%d",track.btofBin,btofList->getFired(track.btofBin),btofList->getTrack(track.btofBin));
    }

    if (mUseCtb) { LOG_DEBUG 
      << Form("    CTB  %3d,%d,%d",track.ctbBin,ctbList->getFired(track.ctbBin),ctbList->getTrack(track.ctbBin));
    }

    LOG_DEBUG 
      << Form("    Bemc %3d,%d,%d",track.bemcBin,bemcList->getFired(track.bemcBin),bemcList->getTrack(track.bemcBin))
      << Form("    Eemc %3d,%d,%d",track.eemcBin,eemcList->getFired(track.eemcBin),eemcList->getTrack(track.eemcBin))
      << Form("    TPC %d",track.mTpc)
      <<endm;
  }

  LOG_INFO << Form("PPVend  eveID=%d,  list of found %d vertices from pool of %d tracks\n",
                   eveID, mVertexData.size(), mTrackData.size()) << endm;

  for (const VertexData &vertex : mVertexData)
    vertex.print(os);
}


//==========================================================
//==========================================================
int StPPVertexFinder::fit(StEvent* event)
{
  mTotEve++;
  eveID=event->id();

  LOG_INFO << "***** START FIT\n"
           << "   @@@@@@   PPVertex::Fit START nEve=" << mTotEve
           << "  eveID=" << eveID << endm;

  hL->SetTitle("Vertex likelihood, eveID=" + TString(eveID) );

  // get BTOF info
  if(mUseBtof) {
    StBTofCollection *btofColl = (StBTofCollection*)event->btofCollection();
    if(btofColl==0) {
      LOG_WARN << "no btofCollection, continue THE SAME eve" << endm;
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
    LOG_WARN << "No StEmcCollection found, continue with this event" << endm;
  } else {
    StEmcDetector* btow = emcC->detector( kBarrelEmcTowerId); 
    if(btow==0) {
      LOG_WARN << "No BEMC found in StEmcCollection, continue with this event" << endm;
    } else {
      bemcList->build(btow, mMinAdcBemc);
    }

    StEmcDetector* etow = emcC->detector(kEndcapEmcTowerId); 
    if(etow==0) {
      LOG_WARN << "No EEMC found in StEmcCollection, continue with this event" << endm;
    } else {
      eemcList->build(etow, mMinAdcEemc);
    }
  }

  //get the Sti track container...
  assert(mToolkit);          // internal error of Sti
  StiTrackContainer* stiTracks = mToolkit->getTrackContainer();
   if(stiTracks==0) {
     LOG_WARN << "No Sti tracks found, skipping this event" << endm;
     return 0 ;
   }

  //select reasonable tracks and add them to my list
  int kBtof=0,kCtb=0,kBemc=0, kEemc=0,kTpc=0;
  int nTracksMatchingAnyFastDetector=0;

  for (const StiTrack* stiTrack : *stiTracks)
  {
    const StiKalmanTrack* stiKalmanTrack = static_cast<const StiKalmanTrack*>(stiTrack);

    TrackDataT<StiKalmanTrack> track(*stiKalmanTrack);

    ntrk[0]++;

    if (stiKalmanTrack->getFlag() <0)           { ntrk[1]++; continue; }
    if (stiKalmanTrack->getPt() < mMinTrkPt)    { ntrk[2]++; continue; }
    if (mDropPostCrossingTrack &&
        isPostCrossingTrack(stiKalmanTrack))    { ntrk[3]++; continue; }  // kill if it has hits in wrong z
    if (!examinTrackDca(stiKalmanTrack, track)) { ntrk[4]++; continue; }  // drop from DCA
    if (!matchTrack2Membrane(track))            { ntrk[5]++; continue; }  // kill if nFitP too small


    // Match to various detectors
    if (mUseBtof) matchTrack2BTOF(stiKalmanTrack, track);  // matching track to btofGeometry
    if (mUseBTOFmatchOnly && (track.mBtof <= 0)) { ntrk[6]++; continue; }

    if (mUseCtb)  matchTrack2CTB(stiKalmanTrack, track);
    matchTrack2BEMC(track);
    matchTrack2EEMC(track);

    ntrk[7]++;

    // ...all test done on this track
    mTrackData.push_back(track); 


    if (track.mBtof > 0) kBtof++;
    if (track.mCtb  > 0) kCtb++;
    if (track.mBemc > 0) kBemc++;
    if (track.mEemc > 0) kEemc++;
    if (track.mTpc  > 0) kTpc++;

    if (track.mBtof>0 || track.mCtb>0 || track.mBemc>0 || track.mEemc>0 || track.mTpc>0)
       nTracksMatchingAnyFastDetector++;
  }

  LOG_INFO << Form("PPV::TpcList size=%d nMatched=%d\n\n",mTrackData.size(),kTpc)
           << "PPV::fit() nEve=" << mTotEve << " , "
           << nTracksMatchingAnyFastDetector << " traks with good DCA, matching: BTOF="
           << kBtof << " CTB=" << kCtb << " BEMC=" << kBemc << " EEMC=" << kEemc << endm;

  if (nTracksMatchingAnyFastDetector >= mMinMatchTr || mStoreUnqualifiedVertex > 0)
  {
    seed_fit_export();
  } else {
    LOG_INFO << "StPPVertexFinder::fit() nEve=" << mTotEve << " Quit, to few matched tracks" << endm;
  }
  
  return size();
} 


int StPPVertexFinder::fit(const StMuDst& muDst)
{
   mTotEve++;

   mStMuDst = &muDst;

   // Similar to fit() we need to populate bemcList
   StMuEmcCollection *muEmcCollection = muDst.muEmcCollection();

   StMuEmcUtil muEmcUtil;

   StEmcCollection* emcC = muEmcUtil.getEmc(muEmcCollection);

   StEmcDetector* btow = emcC->detector(kBarrelEmcTowerId);
   bemcList->build(btow, mMinAdcBemc);

   StEmcDetector* etow = emcC->detector(kEndcapEmcTowerId);
   eemcList->build(etow, mMinAdcEemc);

   delete emcC; emcC=0;

   // Access btof data from ... branch
   //TClonesArray* muBTofHits = muDst.btofArray(muBTofHit);
   //btofList->build(*muBTofHits);

   // Access array of all StDcaGeometry objects (i.e. tracks)
   TObjArray*    globalTracks  = muDst.globalTracks();
   TClonesArray* covGlobTracks = muDst.covGlobTrack();


   for (const TObject* obj : *globalTracks)
   {
      ntrk[0]++;

      const StMuTrack& stMuTrack = static_cast<const StMuTrack&>(*obj);

      if (stMuTrack.pt() < mMinTrkPt) { ntrk[2]++; continue; }

      // Supposedly equivalent to isPostCrossingTrack()
      if ( (stMuTrack.flagExtension() & kPostXTrack) != 0 ) { ntrk[3]++; continue; }

      // Supposedly equivalent to DCA check with examinTrackDca()
      if (stMuTrack.index2Cov() < 0) { ntrk[4]++; continue; }

      StDcaGeometry* dca = static_cast<StDcaGeometry*>(covGlobTracks->At(stMuTrack.index2Cov()));

      if ( std::fabs(dca->z()) > mMaxZrange ) { ntrk[4]++; continue; }
      if ( std::fabs(dca->impact())  > mMaxTrkDcaRxy) { ntrk[4]++; continue; }

      // Condition similar to one in matchTrack2Membrane
      double fracFit2PossHits = static_cast<double>(stMuTrack.nHitsFit(kTpcId)) / stMuTrack.nHitsPoss(kTpcId);
      if (fracFit2PossHits < mMinFitPfrac) { ntrk[5]++; continue; }  // kill if nFitP too small

      // Test TOF match if required
      if (mUseBTOFmatchOnly && (stMuTrack.tofHit() == 0)) { ntrk[6]++; continue; }

      ntrk[7]++;

      TrackDataT<StMuTrack> track(stMuTrack, dca);

      // Modify track weights
      matchTrack2BEMC(track);
      matchTrack2EEMC(track);
      matchTrack2Membrane(track);

      mTrackData.push_back(track);
   }

   seed_fit_export();

   return size();
}


void StPPVertexFinder::seed_fit_export()
{
   // Select a method to find vertex candidates/seeds. The methods work using the
   // `mTrackData` and `mDCAs` containers as input whereas the reconstructed
   // vertices are put in the private container `mVertexData`
   switch (mSeedFinderType)
   {
   case SeedFinder_t::TSpectrum:
     findSeeds_TSpectrum();
     break;

   case SeedFinder_t::PPVLikelihood:
   default:
     findSeeds_PPVLikelihood();
     break;
   }
  
   // Refit vertex position for all cases (currently NoBeamline, Beamline1D, and
   // Beamline3D) except when the BeamlineNoFit option is specified. This is
   // done to keep backward compatible behavior when by default the vertex was
   // placed on the beamline
   if (mVertexFitMode == VertexFit_t::BeamlineNoFit)
   {
      for (VertexData &vertex : mVertexData) {
         const double& z = vertex.r.Z();
         vertex.r.SetXYZ( beamX(z), beamY(z), z);
      }
   }
   else
   {
      size_t n_seeds = mVertexData.size();

      auto cannot_fit = [this] (VertexData &vertex) { return fitTracksToVertex(vertex) != 0; };
      mVertexData.erase( std::remove_if(mVertexData.begin(), mVertexData.end(), cannot_fit), mVertexData.end() );
      // Update the "bad" vertex counter as some vertices could have been
      // removed in the previous step
      nBadVertex -= n_seeds - mVertexData.size();
   }

   exportVertices();

   if (mDebugLevel) printInfo();
}


//==========================================================
//==========================================================
bool StPPVertexFinder::buildLikelihoodZ()
{
  hL->Reset();
  hM->Reset();
  hW->Reset();

  float dzMax2 = mMaxZradius*mMaxZradius;

  int nt=mTrackData.size();
  LOG_DEBUG<< Form("PPV::buildLikelihood() pool of nTracks=%d",nt)<<endm;
  if(nt<=0) return false;

  int nTracksMatchingAnyFastDetector = 0;

  double *La = hL->GetArray(); // PPV main likelihood histogram 
  double *Ma = hM->GetArray(); // track multiplicity histogram 
  double *Wa = hW->GetArray(); // track weight histogram 
  
  // Loop over pre-selected tracks only
  for (const TrackData &track : mTrackData)
  {
    // Skip tracks already assigned to a vertex
    if (track.vertexID != 0) continue;

    if (track.anyMatch) nTracksMatchingAnyFastDetector++;

    float z0   = track.zDca;  // z coordinate at DCA
    float ez   = track.ezDca; // error on z coordinate at DCA
    float ez2  = ez*ez;
    int   j1   = hL->FindBin(z0-mMaxZradius-.1);
    int   j2   = hL->FindBin(z0+mMaxZradius+.1);
    float base = dzMax2/2/ez2;
    float totW = track.weight;

    for (int j=j1; j<=j2; j++)
    {
      float z  = hL->GetBinCenter(j);
      float dz = z-z0;
      float xx = base - dz*dz/2/ez2;
      if (xx <= 0) continue;
      La[j] += xx*totW;
      Ma[j] += 1.;
      Wa[j] += totW;
    }
  }

  LOG_DEBUG << Form("PPV::buildLikelihood() %d tracks w/ matched @ Lmax=%f", nTracksMatchingAnyFastDetector, hL->GetMaximum()) << endm;

  return (nTracksMatchingAnyFastDetector >= mMinMatchTr) || (mStoreUnqualifiedVertex > 0);
}

//==========================================================
//==========================================================
bool StPPVertexFinder::findVertexZ(VertexData &vertex)
{
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

  float kSig = std::sqrt(2*(Lmax-Llow)/avrW);
  float sigZ = (zHigh-zLow)/2/kSig;

  LOG_DEBUG << Form("PPV:: iLow/iMax/iHigh=%d/%d/%d\n",iLow,iMax,iHigh)
            << Form(" accM=%f  accW=%f  avrW=%f\n",accM,accW,avrW)   
            << Form("  Z low/max/high=%f %f %f, kSig=%f, sig=%f\n",zLow,z0,zHigh,kSig,sigZ)
            << Form(" found  PPVertex(ID=%d,neve=%d) z0 =%.2f +/- %.2f\n",vertex.id,mTotEve,z0,sigZ)<<endm;

  if (sigZ < 0.1) sigZ = 0.1; // tmp, make it not smaller than the bin size

  // For approximate seed position we use (x,y)=(0,0) because the tracks are
  // extrapolated to (0,0) anyway. The x and y coordinates can be updated later
  // in a proper fit.
  vertex.r  = TVector3(0, 0, z0);
  vertex.er = TVector3(0.1, 0.1, sigZ);
  vertex.Lmax = Lmax;

  return true;
}

//==========================================================
//==========================================================
bool  StPPVertexFinder::evalVertexZ(VertexData &vertex) // and tag used tracks
{
  // returns true if vertex is accepted accepted
  int nHiPt=0;
  
  for (TrackData &track : mTrackData)
  {
    // Skip tracks already matched to a vertex
    if (track.vertexID != 0) continue;

    // Do not match tracks to vertex if they are too far in Z
    // (i.e. |delta_z| > (mMaxZradius + sigma_z))
    if ( !track.matchVertex(vertex, mMaxZradius) ) continue;

    // Otherwise, this track belongs to this vertex
    track.vertexID  = vertex.id;
    vertex.gPtSum  += track.gPt;
    vertex.nUsedTrack++;

    if( track.gPt>mCut_oneTrackPT && ( track.mBemc>0|| track.mEemc>0) ) nHiPt++;

    if(  track.mTpc>0)       vertex.nTpc++;
    else if (  track.mTpc<0) vertex.nTpcV++;

    if(  track.mBtof>0)       vertex.nBtof++;
    else if (  track.mBtof<0) vertex.nBtofV++;

    if(  track.mCtb>0)       vertex.nCtb++;
    else if (  track.mCtb<0) vertex.nCtbV++;

    if(  track.mBemc>0)       vertex.nBemc++;
    else if (  track.mBemc<0) vertex.nBemcV++;

    if(  track.mEemc>0)       vertex.nEemc++;
    else if (  track.mEemc<0) vertex.nEemcV++;

    if( track.anyMatch)     vertex.nAnyMatch++;
    else if (track.anyVeto) vertex.nAnyVeto++;
  } 

  vertex.isTriggered = (vertex.nAnyMatch >= mMinMatchTr) || ( (mAlgoSwitches & kSwitchOneHighPT) && nHiPt>0 );
  if (!vertex.isTriggered) { // discrad vertex
    //no match tracks in this vertex, tag vertex ID in tracks differently
    //vertex.print(cout);
    LOG_DEBUG << "StPPVertexFinder::evalVertex Vid="<<vertex.id<<" rejected"<<endm;
    for (TrackData &track : mTrackData) {
      if(track.vertexID!=vertex.id) continue;
      track.vertexID=-vertex.id;
    }
    return false;
  }
  
  LOG_INFO << "StPPVertexFinder::evalVertex Vid=" << vertex.id
           << " accepted, nAnyMatch=" << vertex.nAnyMatch
           << " nAnyVeto=" << vertex.nAnyVeto << endm;

  return true;
}


/**
 * Creates DCA states for selected tracks (mTrackData) and fills the member
 * container mDCAs. The tracks in mTrackData must be already associated with
 * a corresponding vertex, i.e. we check that track.vertexID == vertex.id
 *
 * \author Dmitri Smirnov, BNL
 * \date April, 2016
 */
void StPPVertexFinder::createTrackDcas(const VertexData &vertex)
{
   // Consider muDst case
   if (mStMuDst)
   {
      // Just clean the pointers owned by something else
      mDCAs.clear();
      
      for (const TrackData & track : mTrackData) {
         if ( std::fabs(track.vertexID) != vertex.id) continue;
         mDCAs.push_back(track.dca);
      }

      return;
   }

   // Fill member array of pointers to StDcaGeometry objects for selected tracks
   // in mTrackData corresponding to this vertex. These will be used in static
   // minimization function
   while (!mDCAs.empty()) delete mDCAs.back(), mDCAs.pop_back();


   for (const TrackData & track : mTrackData)
   {
      if ( std::fabs(track.vertexID) != vertex.id) continue;
      if (!track.mother) continue;

      // This code is adopted from StiStEventFiller::fillDca()
      StiKalmanTrack tmpTrack = *track.getMother<StiKalmanTrack>();
      StiKalmanTrackNode *tNode = tmpTrack.extrapolateToBeam();

      if (!tNode) continue;

      const StiNodePars &pars = tNode->fitPars();
      const StiNodeErrs &errs = tNode->fitErrs();
      float alfa = tNode->getAlpha();
      float setp[7] = {(float)pars.y(),    (float)pars.z(),    (float)pars.phi(),
                       (float)pars.ptin(), (float)pars.tanl(), (float)pars.curv(), (float)pars.hz()};
      setp[2] += alfa;
      float sete[15];

      for (int i=1, li=1, jj=0; i<kNPars; li += ++i) {
        for (int j=1;j<=i;j++) {
           sete[jj++] = errs.G()[li+j];
        }
      }

      StDcaGeometry* dca = new StDcaGeometry();
      dca->set(setp, sete);
      mDCAs.push_back(dca);
   }
}


/**
 * Takes a list of vertex candidates/seeds and updates each vertex position by
 * fitting tracks pointing to it. The fit is performed by minimizing the chi2
 * robust potential. The method uses the base class member container with track
 * DCAs as input.
 *
 * \author Dmitri Smirnov, BNL
 * \date February, 2016
 */
int StPPVertexFinder::fitTracksToVertex(VertexData &vertex)
{
   createTrackDcas(vertex);

   bool fitRequiresBeamline = star_vertex::requiresBeamline(mVertexFitMode);

   bool prerequisites = mDCAs.size() > 1 || (mDCAs.size() > 0 && fitRequiresBeamline);

   if ( !prerequisites )
   {
      LOG_WARN << "StPPVertexFinder::fitTracksToVertex: At least two tracks required "
               << "OR one with beam line. This vertex (id = " << vertex.id
               << ") coordinates will not be updated" << endm;
      return 5;
   }

   // Recalculate vertex seed coordinates to be used as initial point in the fit
   StThreeVectorD vertexSeed = CalcVertexSeed(mDCAs);

   // For fits with beamline force the seed to be on the beamline
   if ( fitRequiresBeamline )
   {
      vertexSeed.setX( beamX(vertexSeed.z()) );
      vertexSeed.setY( beamY(vertexSeed.z()) );
   }

   // Make sure the global pointer points to valid object so Minuit uses correct data
   StGenericVertexFinder::sSelf = this;

   int minuitStatus;

   mMinuit->mnexcm("clear", 0, 0, minuitStatus);

   static double step[3] = {0.01, 0.01, 0.01};

   double x_lo = vertexSeed.x() - mMaxTrkDcaRxy;
   double y_lo = vertexSeed.y() - mMaxTrkDcaRxy;
   double z_lo = vertexSeed.z() - mMaxZradius;

   double x_hi = vertexSeed.x() + mMaxTrkDcaRxy;
   double y_hi = vertexSeed.y() + mMaxTrkDcaRxy;
   double z_hi = vertexSeed.z() + mMaxZradius;

   if (mVertexFitMode == VertexFit_t::Beamline1D)
   {
      mMinuit->mnparm(0, "z", vertexSeed.z(), step[2], z_lo, z_hi, minuitStatus);
   } else {
      mMinuit->mnparm(0, "x", vertexSeed.x(), step[0], x_lo, x_hi, minuitStatus);
      mMinuit->mnparm(1, "y", vertexSeed.y(), step[1], y_lo, y_hi, minuitStatus);
      mMinuit->mnparm(2, "z", vertexSeed.z(), step[2], z_lo, z_hi, minuitStatus);
   }

   mMinuit->mnexcm("minimize", 0, 0, minuitStatus);

   // Check fit result
   if (minuitStatus) {
      LOG_WARN << "StPPVertexFinder::fitTracksToVertex: Fit did not converge. "
               << "Check TMinuit::mnexcm() status flag: " << minuitStatus << ". "
               << "This vertex (id = " << vertex.id << ") coordinates will not be updated" << endm;

      // The fit has failed but let's keep the vertex anyway. For cases with
      // beam line we put the vertex on the beam line
      if ( fitRequiresBeamline ) {
         const double& z = vertex.r.Z();
         vertex.r.SetXYZ( beamX(z), beamY(z), z);
         vertex.er.SetXYZ( mBeamline.err_x0, mBeamline.err_y0, vertex.er.Z());
      }
      // Return 0 (=success) in order to keep the vertex
      return 0;
   }

   double chisquare, fedm, errdef;
   int npari, nparx;

   mMinuit->mnstat(chisquare, fedm, errdef, npari, nparx, minuitStatus);
   mMinuit->mnhess();

   // The default dimension 9=3*3 should work for 1D case (npar = 1) as well
   double emat[9];
   /* 0 1 2
      3 4 5
      6 7 8 */
   mMinuit->mnemat(emat, 3);

   if (mVertexFitMode == VertexFit_t::Beamline1D)
   {
      const double& z = mMinuit->fU[0];
      vertex.r.SetXYZ( beamX(z), beamY(z), z);
      vertex.er.SetXYZ( mBeamline.err_x0, mBeamline.err_y0, std::sqrt(emat[0]) );
   } else {
      vertex.r.SetXYZ(mMinuit->fU[0], mMinuit->fU[1], mMinuit->fU[2]);
      vertex.er.SetXYZ( std::sqrt(emat[0]), std::sqrt(emat[4]), std::sqrt(emat[8]) );
   }

   return 0;
}

 
/**
 * Copies vertices from this finder private container to StEvent's one. No
 * rejection criteria is applied during the copy.
 */
void StPPVertexFinder::exportVertices()
{
  for (VertexData &vertex : mVertexData)
  {
    StThreeVectorD r(vertex.r.x(), vertex.r.y(), vertex.r.z());

    float cov[6]{};

    cov[0] = vertex.er.x() * vertex.er.x();
    cov[2] = vertex.er.y() * vertex.er.y();
    cov[5] = vertex.er.z() * vertex.er.z();  // [5] is correct,JB

    StPrimaryVertex primV;
    primV.setPosition(r);
    primV.setCovariantMatrix(cov); 
    primV.setVertexFinderId(mUseCtb ? ppvVertexFinder : ppvNoCtbVertexFinder);
    primV.setNumTracksUsedInFinder(vertex.nUsedTrack);
    primV.setNumMatchesWithBTOF(vertex.nBtof);
    primV.setNumMatchesWithCTB(vertex.nCtb);
    primV.setNumMatchesWithBEMC(vertex.nBemc);
    primV.setNumMatchesWithEEMC(vertex.nEemc);
    primV.setNumTracksCrossingCentralMembrane(vertex.nTpc);
    primV.setSumOfTrackPt(vertex.gPtSum);
    primV.setRanking(vertex.Lmax);
    primV.setFlag(1); //??? is it a right value?

    if (mStMuDst)
    {
       for (TrackData &track : mTrackData)
       {
          StThreeVectorF v_position(vertex.r.x(), vertex.r.y(), vertex.r.z());
          StThreeVectorF dist = v_position - track.dca->origin();

          // Calculate total error as fully correlated between DCA and vertex
          float total_err_perp = std::sqrt( vertex.er.Perp2() + track.dca->errMatrix()[0] ); // fully uncorrelated
          float total_err_z    = std::sqrt( vertex.er.z()*vertex.er.z() + track.dca->errMatrix()[2] );

          bool is_daughter = (track.vertexID == vertex.id ||
                              (std::fabs(dist.perp())/total_err_perp < 3 && std::fabs(dist.z())/total_err_z < 3) );

          if ( !is_daughter ) continue;

          track.vertexID = vertex.id;

          StMuTrack* stMuTrack = const_cast<StMuTrack*>( track.getMother<StMuTrack>() );
          stMuTrack->setType(primary);

          // Create StTrack from StMuTrack so idTruth can be calculated for this vertex
          StTrack* primTrack = StMuDst::createStTrack(stMuTrack);
          primV.addDaughter(primTrack);
       }

       primV.setIdTruth();
       vertex.mIdTruth = primV.idTruth();

       // The daughter StTrack-s are removed at this time because we don't save them
       while ( !primV.daughters().empty() )
          delete primV.daughters().back(), primV.daughters().pop_back();
    }

    //..... add vertex to the list
    addVertex(primV);
  }
}

//-------------------------------------------------
//-------------------------------------------------
void StPPVertexFinder::Finish()
{
  LOG_INFO << "StPPVertexFinder::Finish() done, seen eve=" << mTotEve << endm;
}

//-------------------------------------------------
//-------------------------------------------------
void StPPVertexFinder::saveHisto(TString fname)
{
  TString outName = fname + ".hist.root";
  TFile f(outName, "recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n", HList.GetEntries(), outName.Data());
  HList.Write();
  f.Close();
}

//==========================================================
//==========================================================
void StPPVertexFinder::dumpKalmanNodes(const StiKalmanTrack*track)
{
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
    float sy = std::sqrt(ktn.getCyy());
    float sz = std::sqrt(ktn.getCzz());
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
bool StPPVertexFinder::examinTrackDca(const StiKalmanTrack* stiTrack, TrackData &track)
{
  // .......... test DCA to beam .............
  StiKalmanTrackNode * bmNode = stiTrack->getInnerMostNode();
  if (!bmNode) 		return 0;
  if (!bmNode->isDca()) return 0;

  float rxy = std::sqrt(bmNode->x_g()*bmNode->x_g() + bmNode->y_g()*bmNode->y_g());

  if(rxy>mMaxTrkDcaRxy) return false;
  if( std::fabs(bmNode->z_g())> mMaxZrange )   return false ; 
 
  track.zDca   = bmNode->getZ();
  track.ezDca  = std::sqrt(bmNode->getCzz());
  track.rxyDca = rxy;
  track.gPt    = bmNode->getPt();

  return true;
}


//==========================================================
//==========================================================
void StPPVertexFinder::matchTrack2BTOF(const StiKalmanTrack* stiTrack, TrackData &track)
{
  StBTofGeometry* geom = btofList->Geometry();

  StiKalmanTrackNode* ouNode=stiTrack->getOuterMostNode();

  StThreeVectorD posTOF;
  // helix extrapolation:
  StThreeVectorD ou(ouNode->getX(),ouNode->getY(),ouNode->getZ());
  ou.rotateZ(ouNode->getAlpha());
  StPhysicalHelixD phys_helix(std::fabs(ouNode->getCurvature()),
                       ouNode->getDipAngle(),
                       ouNode->getPhase(),
                       ou,
                       ouNode->getHelicity());
  IntVec idVec;
  DoubleVec pathVec;
  PointVec crossVec;

  IntVec iBinVec;
  if(geom->HelixCrossCellIds(phys_helix,idVec,pathVec,crossVec)) {
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

  track.updateAnyMatch(btofMatch,btofVeto,track.mBtof);
  track.weight*=btofW;
  track.btofBin= iBinVec.size() ? iBinVec[0] : -1;
}

//==========================================================
//==========================================================
void  
StPPVertexFinder::matchTrack2CTB(const StiKalmanTrack* stiTrack,TrackData &track){
  const double Rctb=213.6; // (cm) radius of the CTB 

  StiKalmanTrackNode* ouNode=stiTrack->getOuterMostNode();

  StThreeVectorD posCTB;
  float path=-1;
  //alternative helix extrapolation:
  if(1){
    StiKalmanTrackNode * inNode = ouNode;
    StThreeVectorD in(inNode->getX(),inNode->getY(),inNode->getZ());
    in.rotateZ(inNode->getAlpha());
    StPhysicalHelixD phys_helix(std::fabs(inNode->getCurvature()),
			 inNode->getDipAngle(),
			 inNode->getPhase(),
			 in,
			 inNode->getHelicity());
    pairD  d2;
    d2 = phys_helix.pathLength(Rctb);
    path=d2.second;
    if(d2.first>=0 || d2.second<=0) {
      LOG_DEBUG <<Form("WARN MatchTrk , unexpected solution for track crossing CTB\n")<<
	Form(" d2.firts=%f, second=%f, try first", d2.first, d2.second)<<endm;
      path=d2.first;
    }
    posCTB = phys_helix.at(path);
  }

  // official Sti node extrapolation
  if(0){
    StiKalmanTrack track2=*stiTrack;
    StiKalmanTrackNode* ctbNode=track2.extrapolateToRadius(Rctb);

    if(ctbNode==0)  { 
      LOG_INFO <<"#e @ctbNode NULL"<<endm;
      LOG_INFO <<"#e @track dump"<< *stiTrack << endm;
      LOG_INFO <<"#e @OuterMostNode dump"<< *ouNode <<endm;
      return; 
    }
    
    posCTB=StThreeVectorD( ctbNode->x_g(),ctbNode->y_g(),ctbNode->z_g());
  }

  float phi=atan2(posCTB.y(),posCTB.x());
  if(phi<0) phi+=2*M_PI;// now phi is [0,2Pi] as for CTB slats
  float eta=posCTB.pseudoRapidity();

  int iBin=ctbList->addTrack(eta,phi);
  
  bool  ctbMatch=ctbList->isMatched(iBin);
  bool  ctbVeto =ctbList->isVetoed(iBin);
  float ctbW    =ctbList->getWeight(iBin);
  
  track.updateAnyMatch(ctbMatch,ctbVeto,track.mCtb);
  track.weight*=ctbW;
  track.ctbBin=iBin;
}

//==========================================================
//==========================================================
void StPPVertexFinder::matchTrack2BEMC(TrackDataT<StiKalmanTrack> &track)
{
  const StiKalmanTrack* stiTrack = track.getMother();

  StiKalmanTrackNode* ouNode=stiTrack->getOuterMostNode();

  //alternative helix extrapolation:
  StThreeVectorD ou(ouNode->getX(),ouNode->getY(),ouNode->getZ());
  ou.rotateZ(ouNode->getAlpha());
  StPhysicalHelixD phys_helix(std::fabs(ouNode->getCurvature()),
		       ouNode->getDipAngle(),
		       ouNode->getPhase(),
		       ou,
		       ouNode->getHelicity());

  matchTrack2BEMC(phys_helix, track);
}


void StPPVertexFinder::matchTrack2BEMC(TrackDataT<StMuTrack> &track)
{
   const StMuTrack& muTrack = *track.getMother();
   matchTrack2BEMC(muTrack.outerHelix(), track);
}


void StPPVertexFinder::matchTrack2BEMC(const StPhysicalHelixD& phys_helix, TrackData &track)
{
  const double Rxy = 242.; // middle of tower in Rxy

  pairD  d2;
  d2 = phys_helix.pathLength(Rxy);
  float path = d2.second;

  if(d2.first>=0 || d2.second<=0) {
    LOG_DEBUG <<Form("WARN MatchTrk , unexpected solution for track crossing BEMC Cyl\n")<<
      Form(" d2.firts=%f, second=%f, try first\n", d2.first, d2.second)<<endm;
    path=d2.first;
  }

  StThreeVectorD posCyl = phys_helix.at(path);

  float phi = atan2(posCyl.y(), posCyl.x());
  if (phi < 0) phi += 2*M_PI; // now phi is [0,2Pi] as for Cyl slats
  float eta = posCyl.pseudoRapidity();

  int   iBin      = bemcList->addTrack(eta, phi);
  bool  bemcMatch = bemcList->isMatched(iBin);
  bool  bemcVeto  = bemcList->isVetoed(iBin);
  float bemcW     = bemcList->getWeight(iBin);

  track.updateAnyMatch(bemcMatch, bemcVeto, track.mBemc);
  track.weight *= bemcW;
  track.bemcBin = iBin;
}


//==========================================================
//==========================================================
void StPPVertexFinder::matchTrack2EEMC(TrackDataT<StiKalmanTrack> &track)
{
  const StiKalmanTrack* stiTrack = track.getMother();

  const double minEta=0.7 ;// tmp cut

  StiKalmanTrackNode* ouNode=stiTrack->getOuterMostNode();
  StiKalmanTrackNode* inNode=stiTrack->getInnerMostNode();

  //direction of extrapolation must be toward West (Z+ axis)
  if(inNode->getZ()> ouNode->getZ()) return;
  
  // droop too steep tracks
  if(stiTrack->getPseudoRapidity()<minEta) return;

  StThreeVectorD ou(ouNode->getX(),ouNode->getY(),ouNode->getZ());
  ou.rotateZ(ouNode->getAlpha());
  StPhysicalHelixD phys_helix(std::fabs(ouNode->getCurvature()),
		       ouNode->getDipAngle(),ouNode->getPhase(),
		       ou,ouNode->getHelicity());

   // path length at intersection with plane
   // double       pathLength(const StThreeVectorD& r,
   //                         const StThreeVectorD& n) const;

  matchTrack2EEMC(phys_helix, track);
}


void StPPVertexFinder::matchTrack2EEMC(TrackDataT<StMuTrack> &track)
{
   const StMuTrack& muTrack = *track.getMother();

   const double minEta = 0.7;

   //direction of extrapolation must be toward West (Z+ axis)
   if (muTrack.firstPoint().z() > muTrack.lastPoint().z()) return;
   
   // drop too steep tracks
   if(muTrack.eta() < minEta) return;

   matchTrack2EEMC(muTrack.outerHelix(), track);
}


void StPPVertexFinder::matchTrack2EEMC(const StPhysicalHelixD& phys_helix, TrackData &track)
{
  const double eemc_z_position = 288.; // middle of tower in Z
  const double maxPath=200 ;// tmp, cut too long extrapolation

  StThreeVectorD rSmd=StThreeVectorD(0,0,eemc_z_position);
  StThreeVectorD n=StThreeVectorD(0,0,1);

  double path = phys_helix.pathLength(rSmd,n);
  if(path>maxPath) return; // too long extrapolation

  StThreeVectorD r = phys_helix.at(path);
  double periodL=phys_helix. period();
 
  if(periodL<2*path) {
    LOG_DEBUG <<Form(" Warn, long path fac=%.1f ",path/periodL)<<
      Form("  punchEEMC1 x,y,z=%.1f, %.1f, %.1f path=%.1f period=%.1f\n",r.x(),r.y(),r.z(),path,periodL)<<endm; 
  }

  double phi=atan2(r.y(),r.x());
  if(phi<0) phi+=2*M_PI;// now phi is [0,2Pi] as for Cyl slats
  double eta=r.pseudoRapidity();

  int iBin=eemcList->addTrack(eta,phi);
  bool  eemcMatch=eemcList->isMatched(iBin);
  bool  eemcVeto =eemcList->isVetoed(iBin);
  float eemcW    =eemcList->getWeight(iBin);

  track.updateAnyMatch(eemcMatch,eemcVeto,track.mEemc);
  track.weight*=eemcW;
  track.eemcBin=iBin;

}


//==========================================================
//==========================================================
bool StPPVertexFinder::matchTrack2Membrane(TrackDataT<StiKalmanTrack> &track)
{
  const StiKalmanTrack* stiTrack = track.getMother();

  const double RxyMin=59, RxyMax=199, zMax=200;
  const double zMembraneDepth=1; // (cm) ignore signe change for nodes so close to membrane

  //generate bitt pattern for TPC nodes with hits 
  std::vector<int> hitPatt;
  int nPos=0,nFit=0;
  int in=0;
  double lastRxy=9999;
  double lastZ=9999;

  int jz0=0;
  StiKTNBidirectionalIterator it;
  for (it=stiTrack->begin();it!=stiTrack->end();it++) {
    StiKalmanTrackNode* ktnp=& (*it);
    if(!ktnp->isValid()) continue;
    //if(ktnp->getHit() && ktnp->getChi2() >1000) continue; // ---> those track need to be counted as npossiblehit, commented out
    double rxy = std::sqrt(ktnp->x_g()*ktnp->x_g() + ktnp->y_g()*ktnp->y_g()); //ktn.getX();
    double z=ktnp->z_g();  //ktn.z_g();
    if(rxy<RxyMin) continue;
    if(rxy>RxyMax) continue;
    if(std::fabs(z)>zMax) continue;
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
	jz0 = hitPatt.size();
      }
      lastZ=z;
    }
    const StiDetector * det=ktnp->getDetector();
    assert(!(ktnp->x()) || det);
    bool active = !det || det->isActive(ktnp->getY(), ktnp->getZ());
    int hit = ktnp->getHit() ? 1 : 0;
    if (active) {
      hitPatt.push_back(hit);
      nPos++;
      if(hit && ktnp->getChi2() <=1000 ) nFit++;
    }
  }

  if (nFit < mMinFitPfrac * nPos) return false; // too short fragment of a track

  if( mFitPossWeighting)
    track.weight *= 1.*nFit/nPos; // introduced in 2012 for pp510 to differentiate between global track quality, together with lowering the overall threshold from 0.7 to 0.51, Jan
  
  track.scanNodes(hitPatt, jz0); // if central membrane is crossed, scale weight inside


  return true;
}


void StPPVertexFinder::matchTrack2Membrane(TrackDataT<StMuTrack> &track)
{
   const StMuTrack& muTrack = *track.getMother();

   // Code from matchTrack2Membrane
   if (mFitPossWeighting) { // introduced in 2012 for pp510 to differentiate between global track quality, together with lowering the overall threshold from 0.7 to 0.51
      double fracFit2PossHits = static_cast<double>(muTrack.nHitsFit(kTpcId)) / muTrack.nHitsPoss(kTpcId);
      track.weight *= fracFit2PossHits;
   }

   const StThreeVectorF& firstPoint = muTrack.firstPoint();
   const StThreeVectorF& lastPoint  = muTrack.lastPoint();

   // Require the track to be within TPC volume (approximately)
   const float RxyMin = 59, RxyMax = 199, zMax = 200;

   bool isTrackInside = firstPoint.perp() < RxyMin || lastPoint.perp() < RxyMin ||
                        firstPoint.perp() > RxyMax || lastPoint.perp() > RxyMax ||
                        std::fabs(firstPoint.z()) > zMax ||
                        std::fabs(lastPoint.z())  > zMax;

   // Require start and end to be on different sides of the z=0 plane
   bool crossMembrane = firstPoint.z() * lastPoint.z() < 0;

   if ( !isTrackInside || !crossMembrane) return;

   // Find crossing point of the track with z=0 membrane then identify jz0
   double t = firstPoint.z() / (firstPoint.z() - lastPoint.z());

   double intersect_x = firstPoint.x() + t * (lastPoint.x() - firstPoint.x());
   double intersect_y = firstPoint.y() + t * (lastPoint.y() - firstPoint.y());
   //intersect_z = firstPoint.z() + t * (lastPoint.z() - firstPoint.z()); // == 0 (=z)

   double intersect_r = std::sqrt(intersect_x*intersect_x + intersect_y*intersect_y);

   // TPC padrow radii
   const std::array<double, 45> padrow_radii
   {
      60.0,    64.8,    69.6,    74.4,    79.2,    84.0,    88.8,    93.6,    98.8,    104.0,
      109.2,   114.4,   119.6,   127.195, 129.195, 131.195, 133.195, 135.195, 137.195, 139.195,
      141.195, 143.195, 145.195, 147.195, 149.195, 151.195, 153.195, 155.195, 157.195, 159.195,
      161.195, 163.195, 165.195, 167.195, 169.195, 171.195, 173.195, 175.195, 177.195, 179.195,
      181.195, 183.195, 185.195, 187.195, 189.195
   };

   std::vector<int> hitPatt;
   int jz0 = 0;

   for (auto padrow_r : padrow_radii)
   {
      int curr_padrow_index = hitPatt.size();

      if (intersect_r > padrow_r)
         jz0 = curr_padrow_index;

      int hit = muTrack.topologyMap().hasHitInRow(kTpcId, curr_padrow_index+1) ? 1 : 0;

      hitPatt.push_back(hit);
   }

   track.scanNodes(hitPatt, jz0); // if central membrane is crossed, scale weight inside
}


/**
 * Identifies tracks coming from post bunch crossing collisions.
 */
bool StPPVertexFinder::isPostCrossingTrack(const StiKalmanTrack* stiTrack)
{
  const float RxyMin=59, RxyMax=199, zMax=200;
  const float zMembraneDepth=1.0; 
  const int   nWrongZHitCut=2;
  int nWrongZHit=0;
  StiKTNBidirectionalIterator it;
  for (it=stiTrack->begin();it!=stiTrack->end();it++)
  {
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
    if (std::fabs(z) > zMax) continue;

    if ((z < -zMembraneDepth && hit->sector() <= 12) ||
        (z >  zMembraneDepth && hit->sector() >  12))
    {
      nWrongZHit++;
      if (nWrongZHit >= nWrongZHitCut) {return true;}
    }
  }
  return false;
}
