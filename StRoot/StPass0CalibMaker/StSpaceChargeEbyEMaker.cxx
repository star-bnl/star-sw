//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StSpaceChargeEbyEMaker performs event-by-event determination         //
// of the space charge correction for tracks, and sets it for           //
// the next event.                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TROOT.h"
#include "TSystem.h"
#include "StSpaceChargeEbyEMaker.h"
#include "StDbUtilities/StMagUtilities.h"
#include "StEvent/StEventTypes.h"
#include "StMessMgr.h"
#include "StTpcDb/StTpcDb.h"
#include "StTpcDb/StTpcDbMaker.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StEvent/StDcaGeometry.h"
#include "StEvent/StBTofCollection.h"
#include "StEvent/StBTofHit.h"
#include "StEvent/StBTofHeader.h"
#include "StEvent/StBTofPidTraits.h"
#include "StEvent/StTrackPidTraits.h"
#include "StDetectorDbMaker/St_trigDetSumsC.h"
#include "StDetectorDbMaker/St_tpcGridLeakC.h"
#include "StDetectorDbMaker/St_spaceChargeCorC.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/projection/StEmcPosition.h"

#include "TUnixTime.h"
#include "TFile.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TNtuple.h"
#include "TPad.h"

// Histogram ranges:
const int   SCN1 = 400;
const int   SCN2 = 100;
const float SCL = -0.015;
const float SCH =  0.185;
const float SCB = (SCH-SCL)/SCN1;
const float SCX = SCB/TMath::Sqrt(TMath::TwoPi());

const int   DCN = 125;
const float DCL = -2.5;
const float DCH =  2.5;

const int   PHN = 72;
const float PI2 = TMath::TwoPi();

const int   EVN = 1024;

const int   ZN = 60;
const float ZL = -150.;
const float ZH = 150.;

const int   GN = 150;
const float GL = -0.3;
const float GH = 1.2;
const int   GZN = 17;
const float GZL = -200.;
const float GZH = 225.;

const float ZGGRID = 208.707;

static TF1 ga1("ga1","[0]*exp(-0.5*(x-[1])*(x-[1])/([2]*[2]))");
static TF1 ln1("ln1","[0]+((208.707-abs(x))*[1]/100.0)",-150.,150.);

const unsigned int MAXVTXCANDIDATES = 256;

ClassImp(StSpaceChargeEbyEMaker)
  
//_____________________________________________________________________________
StSpaceChargeEbyEMaker::StSpaceChargeEbyEMaker(const char *name):StMaker(name),
    event(0),runinfo(0),
    Calibmode(kFALSE), PrePassmode(kFALSE), PrePassdone(kFALSE),
    QAmode(kFALSE), TrackInfomode(0), Asymmode(kFALSE),
    doNtuple(kFALSE), doReset(kTRUE), doGaps(kFALSE), doSecGaps(kFALSE),
    inGapRow(0),
    vtxVpdAgree(5.0), vtxPCTs(0), vtxEmcMatch(1), vtxTofMatch(0),
    vtxMinTrks(5), minTpcHits(25),
    reqEmcMatch(kFALSE), reqTofMatch(kFALSE), reqEmcOrTofMatch(kTRUE),
    m_ExB(0), SCcorrection(0), GLcorrection(0), SCEWRatio(0),
    scehist(0), timehist(0), myhist(0), myhistN(0), myhistP(0),
    myhistE(0), myhistW(0), dczhist(0), dcehist(0), dcphist(0),
    dcahist(0), dcahistN(0), dcahistP(0), dcahistE(0), dcahistW(0),
    gapZhist(0), gapZhistneg(0), gapZhistpos(0), cutshist(0), ntup(0) {

  MINTRACKS=6000;
  //SCALER_ERROR = 0.0006; // by eye from hist: SCvsZDCEpW.gif (liberal)
  SCALER_ERROR = 0.0007; // by RMS from hist: SCvsZDCX.gif (liberal)

  // MAXDIFFE is maximum different in sc from last ebye sc
  MAXDIFFE =   SCALER_ERROR;
  // MAXDIFFA is maximum different in sc from last scaler sc
  MAXDIFFA = 2*SCALER_ERROR; // should be about equal to SCALER_ERROR, no?
           // Present uncetainties with scalers demands greater tolerance

  // initializations needed at the start
  runid = 0;
  memset(evts,0,SCHN*sizeof(int));
  memset(times,0,SCHN*sizeof(int));
  memset(evtstbin,0,SCHN*sizeof(float));
  evtsnow = 0;
  firstEvent = -1;

  SetMode(0); // default is mode 0 (no QA, no PrePass)
  //DoQAmode(); // For testing

  schist  = new TH1F("SpCh","Space Charge"      ,SCN1,SCL,SCH);
  schistE = new TH1F("SpChE","Space Charge East",SCN1,SCL,SCH);
  schistW = new TH1F("SpChW","Space Charge West",SCN1,SCL,SCH);
  schist ->SetDirectory(0);
  schistE->SetDirectory(0);
  schistW->SetDirectory(0);
  for (int i=0;i<SCHN;i++){
    schists[i]  = new TH1F(Form("SpCh%d" ,i),"Space Charge"     ,SCN1,SCL,SCH);
    schistsE[i] = new TH1F(Form("SpChE%d",i),"Space Charge East",SCN1,SCL,SCH);
    schistsW[i] = new TH1F(Form("SpChW%d",i),"Space Charge West",SCN1,SCL,SCH);
    schists[i] ->SetDirectory(0);
    schistsE[i]->SetDirectory(0);
    schistsW[i]->SetDirectory(0);
  }
  for (int i=0; i<24; i++) {
    schistS[i] = 0;
    gapZhistS[i] = 0;
    gapZhistnegS[i] = 0;
    gapZhistposS[i] = 0;
  }

  // other initializations for safety (will be set later anyhow)
  evt = 0;
  curhist = 0;
  lasttime = 0;
  memset(scS,0,24*sizeof(float));
  memset(escS,0,24*sizeof(float));
  sc = 0; esc = 0;
  scE = 0; escE = 0;
  scW = 0; escW = 0;
  lastsc = 0;
  lastEWRatio = 0;
  oldevt = 0;
  did_auto = kTRUE;
  memset(ntrks ,0,SCHN*sizeof(float));
  memset(ntrksE,0,SCHN*sizeof(float));
  memset(ntrksW,0,SCHN*sizeof(float));
  memset(ntrksS,0,24*sizeof(float));
  gapZfitslope = 0; gapZfitintercept = 0; gapZdivslope = 0;
  egapZfitslope = 0; egapZfitintercept = 0; egapZdivslope = 0;
  gapZfitslopeneg = 0; gapZfitinterceptneg = 0; gapZdivslopeneg = 0;
  gapZfitslopepos = 0; gapZfitinterceptpos = 0; gapZdivslopepos = 0;
  gapZfitslopeeast = 0; gapZfitintercepteast = 0; gapZdivslopeeast = 0;
  gapZfitslopewest = 0; gapZfitinterceptwest = 0; gapZdivslopewest = 0;
  memset(gapZfitslopeS,0,24*sizeof(float));
  memset(gapZfitinterceptS,0,24*sizeof(float));
  memset(gapZdivslopeS,0,24*sizeof(float));
  memset(gapZfitslopenegS,0,24*sizeof(float));
  memset(gapZfitinterceptnegS,0,24*sizeof(float));
  memset(gapZdivslopenegS,0,24*sizeof(float));
  memset(gapZfitslopeposS,0,24*sizeof(float));
  memset(gapZfitinterceptposS,0,24*sizeof(float));
  memset(gapZdivslopeposS,0,24*sizeof(float));
}
//_____________________________________________________________________________
StSpaceChargeEbyEMaker::~StSpaceChargeEbyEMaker() {
  delete schist;
  delete schistE;
  delete schistW;
  for (int i=0;i<SCHN;i++) {
    delete schists[i];
    delete schistsE[i];
    delete schistsW[i];
  }
}
//_____________________________________________________________________________
Int_t StSpaceChargeEbyEMaker::Finish() {

  if (evt > 0) {
    if (PrePassmode) {
      if (PrePassdone) WriteTableToFile();
      else gMessMgr->Warning("StSpaceChargeEbyEMaker: NO SC => NO OUTPUT");
    }
    if ((!Calibmode)&&(!PrePassdone)) EvalCalib();
    WriteQAHists();
  } else {
    gMessMgr->Warning("StSpaceChargeEbyEMaker: NO EVENTS => NO OUTPUT");
  }

  return kStOk;
}
//_____________________________________________________________________________
Int_t StSpaceChargeEbyEMaker::Init() {

  // Use mode to set switches:
  // Set mode in BFC chain by attribute goptSCE100XXX, where
  //   XXX is the mode number, e.g. goptSCE100050 sets mode 50
  Int_t attrMode = IAttr(".gopt.sce");
  attrMode = (attrMode ? attrMode%1000 : GetMode());
  gMessMgr->Info() << "StSpaceChargeEbyEMaker mode: " << attrMode << endm;
  switch (attrMode) {
    case (1) : DoQAmode(); break;
    case (2) : DoPrePassmode(); break;
    case (3) : DoPrePassmode(); DoQAmode();  break;
    case (4) : DoCalib();  break;
    case (5) : DoCalib(); DoAsym(); break;
    case (6) : DoCalib(); DoSecGaps(); break;
    case (7) : DoCalib(); DoAsym(); DoSecGaps(); break;
    case (10): DoNtuple(); break;
    case (11): DoNtuple(); DontReset(); break;
    case (12): DoNtuple(); DoQAmode(); break;
    case (13): DoNtuple(); DontReset(); DoQAmode(); break;
    case (20): DoNtuple(); DontReset(); DoQAmode(); DoSecGaps(); break;
    case (50): DoTrackInfo(); break; // filter out pile-up tracks
    case (51): DoTrackInfo(2); break; // include pile-up tracks
    case (52): DoTrackInfo(3); break; // include pile-up tracks of any length
    default  : {}
  }

  if (Calibmode) doReset = kFALSE;

  evt=0;
  oldevt=1;
  lastsc=0.;
  lastEWRatio=0.;
  curhist=0;
  lasttime=0;
  did_auto=kTRUE;
  InitQAHists();
  if (QAmode) gMessMgr->Info("StSpaceChargeEbyEMaker: Initializing");
  if (TrackInfomode) gMessMgr->Info("StSpaceChargeEbyEMaker: Track Info mode");
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StSpaceChargeEbyEMaker::Make() {

  if (QAmode) cutshist->Fill(0);

  // On very first event, determine first event timestamp and
  //   set default parameters, unless in Calibmode
  if ((!Calibmode) && (tabname.Length() == 0)) SetTableName();
  
  // Get instance of StMagUtilities
  m_ExB = StMagUtilities::Instance();
  if (!m_ExB) {
#ifdef __NEW_MagUtilities__
    m_ExB = new StMagUtilities(gStTpcDb,(kSpaceChargeR2 | kGridLeak));
#else /* ! __NEW_MagUtilities__ */
    TDataSet *RunLog = GetDataBase("RunLog/MagFactor");
    if (!RunLog) gMessMgr->Warning("StSpaceChargeEbyEMaker: No RunLog/MagFactor found.");
    m_ExB = new StMagUtilities(gStTpcDb,RunLog,(kSpaceChargeR2 | kGridLeak));
#endif /* __NEW_MagUtilities__ */
  }
  m_ExB->UndoDistortion(0,0,0); // initialize for this event in case it wasn't used
  lastsc = m_ExB->CurrentSpaceChargeR2();
  lastEWRatio = m_ExB->CurrentSpaceChargeEWRatio();

  // Get StEvent and related info, determine if things are OK
  event = static_cast<StEvent*>(GetInputDS("StEvent"));
  if (!event) {
    gMessMgr->Warning("StSpaceChargeEbyEMaker: no StEvent; skipping event.");
    return kStWarn;
  }
  if (QAmode) cutshist->Fill(1);
  if (firstEvent<0) firstEvent = event->id();
  // Get runinfo, determine if the magnetic field is nonzero 
  // EbyE maker not currently able to handle zero B field
  runinfo = event->runInfo();
  if ((!runinfo) || (runinfo->magneticField() == 0)) {
    gMessMgr->Error("StSpaceChargeEbyEMaker: cannot run due to zero or unknown mag field!");
    // Look for any errant zero field SC values as a warning
    //   for processing even without EbyE
    if ((lastsc != 0) && (runinfo) && (runinfo->magneticField() == 0))
      gMessMgr->Warning() << "BE AWARE THAT A NONZERO VALUE OF SPACECHARGE\n"
        << "    WAS RETURNED BY DB CALL!\n    (could be a local DB file or"
        << " in the actual database)" << endm;
    return kStFatal;
  }
  if (QAmode) cutshist->Fill(2);

  // Select highest ranked vertex(vertices) + some quality cuts
  StPrimaryVertex* pvtx = 0;
  unsigned int vtxCandidates[MAXVTXCANDIDATES];
  unsigned int numVtxCandidates = 0;
  unsigned int totVertices = event->numberOfPrimaryVertices();
  if (TrackInfomode>1) numVtxCandidates=1;
  else {
    const StBTofCollection* btofColl = event->btofCollection();
    const StBTofHeader* btofHeader = (btofColl ? btofColl->tofHeader() : 0);
    float vpd_zvertex = (btofHeader ? btofHeader->vpdVz() : -999);
    for (unsigned int vtxIdx = 0; vtxIdx < totVertices; vtxIdx++) {
      pvtx = event->primaryVertex(vtxIdx);
      if (QAmode) cutshist->Fill(3);
      if (! (IAttr("EastOff") || IAttr("WestOff"))) {
        // vertex ranking & ordering break for East/West off
        StVertexFinderId vtxFindID = pvtx->vertexFinderId();
        float min_rank = -1e6;
        switch (vtxFindID) {
          case minuitVertexFinder   : min_rank = -5; break;
          case ppvVertexFinder      :
          case ppvNoCtbVertexFinder : min_rank = 0; break;
          default                   : break;
        }
        // only one chance for MinuitVF
        if (vtxFindID == minuitVertexFinder) totVertices = 1;
        // vertices are rank ordered, so once it fails, we're done
        if (pvtx->ranking() < min_rank) break;
      }
      if (QAmode) cutshist->Fill(4);
      if (pvtx->numberOfDaughters()  < vtxMinTrks) continue;
      if (QAmode) cutshist->Fill(5);
      if (pvtx->numMatchesWithBEMC() < vtxEmcMatch) continue;
      if (QAmode) cutshist->Fill(6);
      if (pvtx->numMatchesWithBTOF() < vtxTofMatch) continue;
      if (QAmode) cutshist->Fill(7);
      if (pvtx->numPostXTracks() > vtxPCTs) continue;
      if (QAmode) cutshist->Fill(8);
      if (vtxVpdAgree > 0 && // set vtxVpdAgree negative to skip this cut
          TMath::Abs(pvtx->position().z() - vpd_zvertex) > vtxVpdAgree) continue;
      if (QAmode) cutshist->Fill(9);
      vtxCandidates[numVtxCandidates] = vtxIdx;
      numVtxCandidates++;
      if (numVtxCandidates == MAXVTXCANDIDATES) break;
    }
  }
  if (!numVtxCandidates) return kStOk;
  if (QAmode) cutshist->Fill(10);
  
  StSPtrVecTrackNode& theNodes = event->trackNodes();
  unsigned int nnodes = theNodes.size();
  if (!nnodes) return kStOk;
  if (QAmode) cutshist->Fill(11);

  // Store and setup event-wise info
  evt++;
  int thistime = event->time();
  if (lasttime) {
    timehist->SetBinContent(evt,thistime-lasttime);
  } else {
    runid = event->runId();
  }
  if (doReset) {
    if (evt>1) curhist = imodHN(curhist+1);
    schists[curhist] ->Reset();
    schistsE[curhist]->Reset();
    schistsW[curhist]->Reset();
    if (doGaps) {
      gapZhist->Reset();
      gapZhistpos->Reset();
      gapZhistneg->Reset();
      if (doSecGaps) for (int i=0; i<24; i++) {
        schistS[i]->Reset();
        gapZhistS[i]->Reset();
        gapZhistposS[i]->Reset();
        gapZhistnegS[i]->Reset();
      }
    }
  } else {
    // Do not reset ntuple in calib mode
    if (doNtuple && !Calibmode) ntup->Reset();
  }

  // Keep time and event number
  times[curhist] = thistime;
  evts[curhist]=evt;

  // Keep calibrations used
  if (!SCcorrection) {
    SCcorrection = new TNamed("SCcorrection",
      (St_spaceChargeCorR2C::instance()->getSpaceChargeString()).Data());
    SCEWRatio = new TNamed("SCEWRatio",Form("%f",
      St_spaceChargeCorR2C::instance()->getEWRatio()));
    GLcorrection = new TNamed("GLcorrection",Form("%f",
      St_tpcGridLeakC::instance()->MiddlGLStrength()));
    gMessMgr->Info() << "Using the following corrections:"  << endm;
    gMessMgr->Info() << "sc = " << SCcorrection->GetTitle() << endm;
    gMessMgr->Info() << "E/W= " << SCEWRatio->GetTitle()    << endm;
    gMessMgr->Info() << "GL = " << GLcorrection->GetTitle() << endm;
  }

  // Keep track of # of events in the same time bin
  if (thistime == lasttime) evtsnow++;
  else evtsnow = 1;
  evtstbin[curhist] = evtsnow;

  if (QAmode) {
    gMessMgr->Info()
      << "used (for this event) SpaceCharge = "
      << lastsc << " (" << thistime << ")" << endm;
    gMessMgr->Info()
      << "zdc west+east = "
      << runinfo->zdcWestRate()+runinfo->zdcEastRate() << endm;
    gMessMgr->Info()
      << "zdc coincidence = "
      << runinfo->zdcCoincidenceRate() << endm;
  }

  // Fill the StEvent information for the SpaceCharge used in this event
  runinfo->setSpaceCharge(lastsc);
  runinfo->setSpaceChargeCorrectionMode(m_ExB->GetSpaceChargeMode());
  if (!inGapRow) {
    if (runinfo->runId() > 10000000) inGapRow = 13; // Run 9+
    else if (runinfo->runId() > 0) inGapRow = 12; // Run exists
    // else undefined
  }
 
  // Track loop
  unsigned int i,j,k,v;

  // Prepare for EMC match
  StEmcDetector* bemcDet = 0;
  Double_t emcRadius = 0;
  static StEmcPosition* emcPosition = 0;
  static StEmcGeom* emcGeom = 0;
  if (reqEmcMatch || reqEmcOrTofMatch) {
    bemcDet = event->emcCollection()->detector(kBarrelEmcTowerId);
    if (!emcPosition) emcPosition = new StEmcPosition();
    if (!emcGeom) emcGeom = StEmcGeom::instance("bemc");
    emcRadius = emcGeom->Radius() + 30; // use exit radius, 30cm beyond face
  }

  St_tpcPadConfigC* pads = St_tpcPadConfigC::instance();
  StThreeVectorD vtxPos(0,0,0),vtxPosErr(0,0,0);
  for (v=0; v<numVtxCandidates; v++) {
    if (TrackInfomode<2) {
      pvtx = event->primaryVertex(vtxCandidates[v]);
      if (! pvtx) continue;
      vtxPos = pvtx->position();
      vtxPosErr = pvtx->positionError();
    }

    for (i=0; i<nnodes; i++) {
      for (j=0; j<theNodes[i]->entries(global); j++) {
        if (QAmode) cutshist->Fill(16);
        StTrack* tri = theNodes[i]->track(global,j);
        if (!tri) continue;
        if (QAmode) cutshist->Fill(17);

          const StTrackTopologyMap& map = tri->topologyMap();
          //if (! map.trackTpcOnly()) continue;
          if (! map.hasHitInDetector(kTpcId)) continue;
          if (QAmode) cutshist->Fill(18);
          // Multiple silicon hits destroy sDCA <-> SpaceCharge correlation,
          // and single hit in SVT is unreliable. Only good config is NO SVT!
          // Updated for HFT era to exclude anything with PXL or IST hits
          if ((map.hasHitInDetector(kSvtId) ||
               map.hasHitInDetector(kPxlId) ||
               map.hasHitInDetector(kIstId)) && TrackInfomode < 1) continue;
          if (QAmode) cutshist->Fill(19);
          if (map.numberOfHits(kTpcId) < minTpcHits) continue;
          if (QAmode) cutshist->Fill(20);

          // *** TOF MATCHING ***
          Bool_t tofMatch = kFALSE;
          if (reqTofMatch || reqEmcOrTofMatch) {
            const StPtrVecTrackPidTraits& theTofPidTraits = tri->pidTraits(kTofId);
            if (theTofPidTraits.size()) {
              if (QAmode) cutshist->Fill(21);
              StTrackPidTraits* theSelectedTrait = theTofPidTraits[theTofPidTraits.size()-1];
              if (theSelectedTrait) {
                if (QAmode) cutshist->Fill(22);
                StBTofPidTraits *pidTof = dynamic_cast<StBTofPidTraits *>(theSelectedTrait);
                if (pidTof) {
                  if (QAmode) cutshist->Fill(23);
                  int Mflag=pidTof->matchFlag();
                  //  0: no matching
                  //  1: 1-1 matching
                  //  2: 1-2 matching, pick up the one with higher ToT vaule (<25ns) 
                  tofMatch = (Mflag > 0);
                }
              }
            }
            // Don't require match if reqEmcOrTofMatch
            if (reqTofMatch && !tofMatch) continue;
          }
          if (QAmode) cutshist->Fill(24);

          // *** EMC MATCHING ***
          Bool_t emcMatch = kFALSE;
          if (reqEmcMatch || (reqEmcOrTofMatch&&(!tofMatch))) {
            Double_t mEmcThresh = 0.15;
            Double_t energyBEMC = -100.0;
            UInt_t tower_eta,tower_mod = 0;
            Int_t tower_sub = 0;
            StThreeVectorD emcTrkMomentum,emcTrkPosition;
            if ((emcPosition->trackOnEmc(&emcTrkPosition,&emcTrkMomentum,
                                 tri,runinfo->magneticField()/10.,emcRadius))) {
              if (QAmode) cutshist->Fill(25);
              Float_t emcEta = emcTrkPosition.pseudoRapidity();
              Float_t emcPhi = emcTrkPosition.phi();
              Int_t m = 0 ,e = 0,s = 0,id = 0;
              emcGeom->getBin(emcPhi,emcEta,m,e,s);
              if (emcGeom->getId(m,e,s,id) == 0) {
                tower_mod = m;
                tower_eta = e;
                tower_sub = s;
              }
              if (tower_mod >= 1 && tower_mod <= 120) {
                if (QAmode) cutshist->Fill(26);
                if (event->emcCollection()) {
                  StEmcModule* emcMod = bemcDet->module(tower_mod);
                  StSPtrVecEmcRawHit& emcHits = emcMod->hits();
                  for (UInt_t emcHit=0; emcHit<emcHits.size(); emcHit++) {
                    if ((emcHits[emcHit]) && (emcHits[emcHit]->eta() == tower_eta)
                                          && (emcHits[emcHit]->sub() == tower_sub)) {
                        energyBEMC = emcHits[emcHit]->energy();
                        break; // only one hit
                    }
                  }
                  emcMatch = (energyBEMC >= mEmcThresh);
                }
              }
            }
            // Require match for both reqEmcMatch or reqEmcOrTofMatch if here
            if (!emcMatch) continue; 
          }
          if (QAmode) cutshist->Fill(27);

          StTrackGeometry* triGeom = tri->geometry();

          StThreeVectorF xvec = triGeom->origin();
          if (!(xvec.x() || xvec.y() || xvec.z())) continue;
          if (QAmode) cutshist->Fill(28);
          StThreeVectorF pvec = triGeom->momentum();
          if (!(pvec.x() || pvec.y())) continue;
          if (QAmode) cutshist->Fill(29);

          float oldPt = pvec.perp();
          if (oldPt < 0.0001) continue;
          if (QAmode) cutshist->Fill(30);

          int e_or_w = 0; // east is -1, west is +1
          if (pvec.z() * xvec.z() > 0) e_or_w = ( (xvec.z() > 0) ? 1 : -1 );

          StPhysicalHelixD hh = triGeom->helix();

          Float_t eta=pvec.pseudoRapidity();
          Float_t phi=0;
          Double_t pathlen = 0;
          //Float_t DCA=hh.geometricSignedDistance(0,0); // for testing only
          Float_t DCA3=-999;
          Float_t DCA2=-999;
          Double_t DCAerr = 0.;
          StDcaGeometry* triDcaGeom = ((StGlobalTrack*) tri)->dcaGeometry();
          if (triDcaGeom) {
            StPhysicalHelixD dcahh = triDcaGeom->helix();
            DCA3 = dcahh.distance(vtxPos,kFALSE);
            DCA2 = dcahh.geometricSignedDistance(vtxPos.x(),vtxPos.y());
            // helix() gets the sign of DCA2, thelix() gets the error
            THelixTrack thelix = triDcaGeom->thelix();
            thelix.Dca(vtxPos.x(),vtxPos.y(),&DCAerr);
            phi = TMath::ATan2(dcahh.cy(pathlen),dcahh.cx(pathlen));
            if (TrackInfomode>1) {
              vtxPos.setZ(dcahh.z(pathlen));
            }
          } else {
            DCA3 = hh.distance(vtxPos,kFALSE);
            DCA2 = hh.geometricSignedDistance(vtxPos.x(),vtxPos.y());
            pathlen = hh.pathLength(vtxPos.x(),vtxPos.y());
            phi = TMath::ATan2(hh.cy(pathlen),hh.cx(pathlen));
            if (TrackInfomode>1) {
              vtxPos.setZ(hh.z(pathlen));
              vtxPosErr.setY(1); // flag the non-DcaGeom tracks
            }
          }
          if (TrackInfomode>1) {
            if (TMath::Abs(DCA2) > 2) continue; // cut out tracks not near (0,0)
          } else {
            if (DCA3 > 4) continue; // cut out pileup tracks!
          }
          if (QAmode) cutshist->Fill(31);
          Int_t ch = (int) triGeom->charge();

          Int_t PCT = 0;
          Int_t sec,sector = -1;
          Int_t prow = 0;
          Int_t prowmask = 0x0;
          Double_t rs[128];
          Double_t rerrors[128];
          Double_t rphierrors[128];
          memset(rerrors,0,64*sizeof(Float_t));
          memset(rphierrors,0,64*sizeof(Float_t));
          StPtrVecHit& hits = tri->detectorInfo()->hits();
          for (k=0;k<hits.size();k++) {
            StHit* hit = hits[k];
            if (hit->detector() == kTpcId) {
              sec  = ((StTpcHit*) hit)->sector();
              prow = ((StTpcHit*) hit)->padrow();
              if ((hit->position().z() > 1 && sec > 12) ||
                  (hit->position().z() <-1 && sec < 13)) PCT++;
              int lastInner = pads->innerPadRows(sec);
              if (prow >= lastInner-2 && prow <= lastInner+3) {
                sector = ( sec == sector || sector < 0 ? sec : 0 ); // 0 if crossing sectors
                // Require 4 hits: one on prow closest to gap for both inner and outer,
                // and another on either of next two prows away from gap for both inner and outer
                // (exception for inGapRow==12 to require prow 11 & 12 for inner)
                int shifter = prow-(lastInner-1);
                if (shifter<0) shifter += (inGapRow==12 ? 2 : 1);
                else if (shifter>3) shifter--;
                prowmask |= (0x1 << shifter);
              }
            }
            rs[k] = hit->position().perp();
            Float_t herr = hit->positionError().perp();
            rerrors[k] = herr;
            rphierrors[k] = herr;
          }
          if (PCT && TrackInfomode<2) continue; // Track has post-crossing hits
          bool good4gap = (sector > 0) && (prowmask == 0xF);
          if (QAmode) cutshist->Fill(32);

          if (TrackInfomode) {
            LOG_INFO << Form("GOODTRACK %d %d %6.2f %9.4f %8.3f %8.4f %8.4f %6.4f %6.4f %d",
              runid,event->id(),vtxPos.z(),ch/oldPt,eta,phi,DCA2,DCAerr,
              vtxPosErr.perp(),hits.size()) << endm;
            continue;
          }

          Float_t space = 10000.;
          Int_t predictFailed = m_ExB->PredictSpaceChargeDistortion(hits.size(),ch,oldPt,vtxPos.z(),
            eta,phi,DCA2,rs,rerrors,rphierrors,space);
          if (predictFailed) {
            if (QAmode) cutshist->Fill(40+predictFailed);
            continue;
          }
          if (QAmode) cutshist->Fill(33);

          TH1F** aschists = (e_or_w > 0 ? schistsW :
                            (e_or_w < 0 ? schistsE : 0));
          Bool_t doSecSCs = (doSecGaps && sector>0);
          sector--;

          Double_t spaceErr = TMath::Abs(space*DCAerr/DCA2);
          Float_t spaceEW = space + lastsc * (e_or_w < 0 ? lastEWRatio : 1.0);
          space += lastsc;  // Assumes additive linearity of space charge!
          if (spaceErr > 0) {
            // Fill SpaceCharge accounting for prediction error
            ga1.SetParameters(SCX/spaceErr,space,spaceErr);
            for (Int_t si=1;si<=SCN1;si++) {
              Double_t sx = schists[curhist]->GetBinCenter(si);
              if (TMath::Abs((sx-space)/spaceErr) < 4.5) {
                // only within +/-4.5 sigma
                schists[curhist]->Fill(sx,ga1.Eval(sx));
              }
            }
            // Now for east/west
            if (aschists || doSecSCs) {
              ga1.SetParameters(SCX/spaceErr,spaceEW,spaceErr);
              for (Int_t si=1;si<=SCN1;si++) {
                Double_t sx = schists[curhist]->GetBinCenter(si);
                if (TMath::Abs((sx-spaceEW)/spaceErr) < 4.5) {
                  // only within +/-4.5 sigma
                  if (aschists) aschists[curhist]->Fill(sx,ga1.Eval(sx));
                  if (doSecSCs) schistS [sector ]->Fill(sx,ga1.Eval(sx));
                }
              }
            }
          } else {
            schists[curhist]->Fill(space);
            if (aschists) aschists[curhist]->Fill(spaceEW);
            if (doSecSCs) schistS [sector ]->Fill(spaceEW);
          }
          FillQAHists(DCA2,space,spaceEW,ch,hh,e_or_w);


          if (doGaps && good4gap &&
              (e_or_w!=0) && (TMath::Abs(ch)==1) && (oldPt>0.3))
            FillGapHists(tri,hh,e_or_w,ch);

      } // loop over j tracks
    } // loop over i Nodes
  } // loop over v vertices
  if (QAmode) cutshist->Fill(9);


  ntrks[curhist]  = schists[curhist] ->Integral();
  ntrksE[curhist] = schistsE[curhist]->Integral();
  ntrksW[curhist] = schistsW[curhist]->Integral();
  if (doSecGaps) for (i=0; i<24; i++) ntrksS[i] = schistS[i]->Integral();

  // Wrap it up and make a decision
  int result = DecideSpaceCharge(thistime);

  if (doGaps) DetermineGaps();
  if (doNtuple) {
      static float X[123];
      static float ntent = 0.0;
      static float nttrk = 0.0;

      if (ntent == 0.0) memset(X,0,51*sizeof(float));
      ntent++;  // # entries since last reset, including this one
      float last_nttrk = nttrk;
      nttrk = ntrks[curhist];  // # tracks since last reset, including these
      float s0 = ( nttrk ? last_nttrk / nttrk : 0 );
      float s1 = 1.0 - s0; // fraction of tracks from current event

      if (QAmode) {
        gMessMgr->Info() << "reset = " << doReset << endm;
        gMessMgr->Info() << "nevts = " << ntent << endm;
        gMessMgr->Info() << "ntrks = " << nttrk << endm;
        if (doSecGaps) for (i=0; i<24; i++) {
          gMessMgr->Info() << "ntrks(" << i+1 << ") = " << ntrksS[i] << endm;
        }
      }

      float ee;
      int fbin = evt + 1 - ((int) ntent);

      X[0]  = sc;
      X[1]  = FindPeak(dcehist->ProjectionY("_dy",fbin,evt),ee);
      X[2]  = s0*X[2] + s1*runinfo->zdcCoincidenceRate();
      X[3]  = s0*X[3] + s1*runinfo->zdcWestRate();
      X[4]  = s0*X[4] + s1*runinfo->zdcEastRate();
      X[5]  = s0*X[5] + s1*runinfo->bbcCoincidenceRate();
      X[6]  = s0*X[6] + s1*runinfo->bbcWestRate();
      X[7]  = s0*X[7] + s1*runinfo->bbcEastRate();
      X[8]  = s0*X[8] + s1*runinfo->bbcBlueBackgroundRate();
      X[9]  = s0*X[9] + s1*runinfo->bbcYellowBackgroundRate();
      X[10] = s0*X[10] + s1*runinfo->initialBeamIntensity(blue);   // west-bound
      X[11] = s0*X[11] + s1*runinfo->initialBeamIntensity(yellow); // east-bound
      X[12] = runinfo->beamFillNumber(blue);
      X[13] = runinfo->magneticField();
      X[14] = event->runId();
      X[15] = firstEvent;
      if ((QAmode) && (evt <= EVN)) {
        X[16] = FindPeak(dcahistN->ProjectionZ("_dnz",fbin,evt,1,PHN),ee);
        X[17] = FindPeak(dcahistP->ProjectionZ("_dpz",fbin,evt,1,PHN),ee);
        X[18] = FindPeak(dcahistE->ProjectionZ("_dez",fbin,evt,1,PHN),ee);
        X[19] = FindPeak(dcahistW->ProjectionZ("_dwz",fbin,evt,1,PHN),ee);
      }
      X[20] = gapZfitslope;
      X[21] = gapZfitintercept;
      X[22] = gapZdivslope;
      X[23] = gapZfitslopeneg;
      X[24] = gapZfitinterceptneg;
      X[25] = gapZdivslopeneg;
      X[26] = gapZfitslopepos;
      X[27] = gapZfitinterceptpos;
      X[28] = gapZdivslopepos;
      X[29] = gapZfitslopeeast;
      X[30] = gapZfitintercepteast;
      X[31] = gapZdivslopeeast;
      X[32] = gapZfitslopewest;
      X[33] = gapZfitinterceptwest;
      X[34] = gapZdivslopewest;
      X[35] = s0*X[35] + s1*runinfo->spaceCharge();
      X[36] = s0*X[36] + s1*((float) (runinfo->spaceChargeCorrectionMode()));
      X[37] = s0*X[37] + s1*St_tpcGridLeakC::instance()->MiddlGLStrength();
      X[38] = s0*X[38] + s1*St_trigDetSumsC::Nc(runinfo->zdcCoincidenceRate(),
                                 runinfo->zdcEastRate(),runinfo->zdcWestRate());
      X[39] = s0*X[39] + s1*St_trigDetSumsC::Nc(runinfo->bbcCoincidenceRate(),
                                 runinfo->bbcEastRate(),runinfo->bbcWestRate());

      // VPD data:
      // StRunInfo's backgroundRate is filled in StEventMaker from 'mult' of trigDetSums
      // trigDetSums fills 'mult' from rich scaler rs11 in the DAQ stream
      //   (but in the offline database, it is migrated from rs16)
      // rs11 stores VPD coincidence rate as of 2007-12-19
      // rs16 stores MTD rate as of Run 9
      // VPD east and west are rs8 and 9, and are not stored in StEvent
      X[40] = s0*X[40] + s1*runinfo->backgroundRate();
      X[41] = s0*X[41] + s1*St_trigDetSumsC::instance()->getPVPDWest();
      X[42] = s0*X[42] + s1*St_trigDetSumsC::instance()->getPVPDEast();

      // NoKiller data:
      // Stored in rich scalers rs12,13,15 for 2011+ data, and available
      //   for the DAQ stream via otherwise empty CTB members of
      //   trigDetSums starting with SL13b
      X[43] = s0*X[43] + s1*St_trigDetSumsC::instance()->getCTBOrTOFp(); // ZDCXNoKiller
      X[44] = s0*X[44] + s1*St_trigDetSumsC::instance()->getCTBWest();   // ZDCWestNoKiller
      X[45] = s0*X[45] + s1*St_trigDetSumsC::instance()->getCTBEast();   // ZDCEastNoKiller

      // StMagUtilities distortion correction parameters
      X[46] = s0*X[46] + s1*m_ExB->GetConst_0();
      X[47] = s0*X[47] + s1*m_ExB->GetConst_1();

      // SpaceCharge east/west asymmetry
      X[48] = scE;
      X[49] = scW;
      X[50] = s0*X[50] + s1*lastEWRatio*runinfo->spaceCharge();

      for (i=0;i<24;i++) {
        X[51+3*i] = scS[i];
        X[52+3*i] = gapZfitslopeS[i];
        X[53+3*i] = gapZfitinterceptS[i];
      }

      // In calib mode, only fill when doReset (we found an sc)
      if (doReset || !Calibmode) ntup->Fill(X);

      if (doReset) {ntent = 0.0; nttrk = 0.0; }

  }
          
  lasttime = thistime;

  return result;
}
//_____________________________________________________________________________
Int_t StSpaceChargeEbyEMaker::DecideSpaceCharge(int time) {

  // curhist has only this event
  // curhist-1 has past two events...
  // curhist-x has past (x+1) events
  // curhist-(SCHN-1) == curhist+1 has past SCHN events

  Bool_t QAout = QAmode || PrePassmode;
  Bool_t do_auto = kTRUE;
  Bool_t few_stats = kTRUE;
  Bool_t large_err = kTRUE;
  Bool_t large_dif = kTRUE;

  // Cuts on difference from previous sc measure:
  // If last event was auto, use MAXDIFFA, else use between MAXDIFFE & MAXDIFFA
  //   scaled by oldness of previous sc measure (curhist-1)
  float maxdiff,dif=0;
  if (did_auto)
    maxdiff = MAXDIFFA;
  else
    maxdiff = MAXDIFFA - (MAXDIFFA-MAXDIFFE)*oldness(imodHN(curhist-1));

  // More than 30 seconds since last used event? Forget it...
  int timedif = time-lasttime;
  if (QAout) {
    gMessMgr->Info() << "time since last event = "
      << timedif << endm;
    gMessMgr->Info() << "curhist = "
      << curhist << endm;
  }
  float ntrkstot  = 0; // running sum using oldness scale factor
  float ntrkstotE = 0;
  float ntrkstotW = 0;
  Bool_t decideFromData = ((PrePassmode) || (Calibmode) || (lasttime==0) || (timedif < 30));
  if (decideFromData) {
  
    int isc;
    static int iscMax = 1;  // use only one hist for calib mode, and...
    if (!Calibmode && iscMax<SCHN) iscMax = curhist+1; // don't use uninitialized
    for (isc=0; isc<iscMax; isc++) {
      int i = imodHN(curhist-isc);
      ntrkstot  += ntrks[i]  * oldness(i);
      ntrkstotE += ntrksE[i] * oldness(i);
      ntrkstotW += ntrksW[i] * oldness(i);
      if (QAout) {
        if (!isc) gMessMgr->Info("Building with: i, ni, oi, nt:");
        gMessMgr->Info() << "Building with: " << i << ", "
          << ntrks[i]  << ", " << oldness(i) << ", " << ntrkstot << endm;
        gMessMgr->Info() << "....east with: " << i << ", "
          << ntrksE[i] << ", " << oldness(i) << ", " << ntrkstotE << endm;
        gMessMgr->Info() << "....west with: " << i << ", "
          << ntrksW[i] << ", " << oldness(i) << ", " << ntrkstotW << endm;
      }

      // Too little data collected? Keep trying...
      few_stats = (IAttr("EastOff") ?
                   (ntrkstotW < MINTRACKS) :   // west-only
                   (IAttr("WestOff") ? 
                    (ntrkstotE < MINTRACKS) :  // east-only
                    (Asymmode ?
                     (ntrkstotE < MINTRACKS || ntrkstotW < MINTRACKS) : // all, Asymmode
                     (ntrkstot < MINTRACKS) ) ) ); // all, not Asymmode
      if (!few_stats) {
        BuildHist(i,schist ,schists );
        BuildHist(i,schistE,schistsE);
        BuildHist(i,schistW,schistsW);
        FindSpaceCharge(schist ,sc ,esc );
        FindSpaceCharge(schistE,scE,escE);
        FindSpaceCharge(schistW,scW,escW);
        if (doSecGaps) for (int j=0;j<24;j++) FindSpaceCharge(schistS[j],scS[j],escS[j]);
        if (QAout) {
          gMessMgr->Info() << "sc  = " << sc  << " +/- " << esc  << endm;
          gMessMgr->Info() << "scE = " << scE << " +/- " << escE << endm;
          gMessMgr->Info() << "scW = " << scW << " +/- " << escW << endm;
          if (doSecGaps) for (int j=0;j<24;j++) {
            gMessMgr->Info() << "sc" << Form("%2d",j+1) << " = " << scS[j] << " +/- " << escS[j] << endm;
          }
        }
        large_err = (esc == 0) || (esc > SCALER_ERROR);
        if (!large_err) {
          if (PrePassmode) { do_auto=kFALSE; break; }
          // otherwise, check for big jumps
          dif = TMath::Abs(sc-lastsc);
          large_dif = dif > maxdiff;
          if (!large_dif || Calibmode) {
            oldevt = evts[i];
            do_auto=kFALSE;
            break;
          }
        }
      }

      // shouldn't need to go past oldest event previously used?
      // tough to know - allow for now as of 11 Jan 2008
      // if (evts[i] <= oldevt)  break;
    }
    if (QAout && (isc == SCHN)) gMessMgr->Info()
      << "STORED DATA EXHAUSTED: "
      << SCHN << " events" << endm;
  }

  did_auto = do_auto;

  // In normal  mode, do_auto decides whether to use automatic SC from DB
  // In PrePass mode, do_auto decides when we're ready to stop
  // In Calib   mode, do_auto decides when to save entries and reset

  if (do_auto) {
    if (QAout && decideFromData) {
      if (few_stats) gMessMgr->Info()
        << "(RECENT) STATS TOO FEW: "
        << ntrkstot << " / " << ntrkstotE << " / " << ntrkstotW
        << " (" << MINTRACKS << ")" << endm;
      else if (large_err) gMessMgr->Info()
        << "FIT ERROR TOO LARGE: "
        << esc << " (" << SCALER_ERROR << ")" << endm;
      else if (large_dif) gMessMgr->Info()
        << "DIFFERENCE TOO LARGE: "
        << dif << " (" << maxdiff << ")" << endm;
    }
    gMessMgr->Info("using auto SpaceCharge");
    if (Calibmode) doReset = kFALSE;
    else m_ExB->AutoSpaceChargeR2();
  } else {
    gMessMgr->Info() << "using SpaceCharge = "
      << sc << " +/- " << esc << " (" << time << ")" << endm;
    scehist->SetBinContent(evt,sc);
    scehist->SetBinError(evt,esc);
    if (PrePassmode) {
      PrePassdone = kTRUE;
      return kStStop; // We're happy! Let's stop!
    }
    if (Calibmode) {
      doReset = kTRUE;
      return kStStop; // We're happy? Let's stop!
    }
    else m_ExB->ManualSpaceChargeR2(sc,lastEWRatio);
  }
  return kStOk;
}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::FindSpaceCharge(TH1F* aschist, float& asc, float& aesc) {
  aesc = 0.;
  double res = FindPeak(aschist,aesc);
  asc = (res > -500. ? res : 0.0);
}
//_____________________________________________________________________________
double StSpaceChargeEbyEMaker::FindPeak(TH1* hist,float& pkwidth) {

  if (!hist) return -996.;
  pkwidth = 0.;
  if (hist->Integral() < 100.0) return -997.;
  double mn = hist->GetMean();
  double rms = TMath::Abs(hist->GetRMS());
  double range = hist->GetXaxis()->GetXmax() - hist->GetXaxis()->GetXmin();
  mn *= 1.0 + (rms/range); rms *= 1.5;
  double lr = mn-rms;
  double ur = mn+rms;
  double pmax = TMath::Max(hist->GetMaximum(),0.);
  double lp = pmax*0.001;
  double up = pmax*10.0;
  double lw = rms*0.001;
  double uw = rms*10.0;
  ga1.SetParameters(pmax,mn,rms*0.5);
  ga1.SetRange(lr,ur);
  ga1.SetParLimits(0,lp,up); // Loglikelihood only works with positive functions
  ga1.SetParLimits(2,lw,uw); // To help the fit
  hist->Sumw2();
  int fitResult = hist->Fit(&ga1,
    (gROOT->GetVersionInt() >= 53000 ? "WLRB0Q" : "LLRB0Q")); // Loglikelihood options changed!
  hist->Sumw2(kFALSE);
  ga1.ReleaseParameter(0);
  ga1.ReleaseParameter(2);
  if (fitResult != 0) return -999.;
  double rp = ga1.GetParameter(0);
  if (rp == lp || rp == up) return -998;
  pkwidth = TMath::Abs(ga1.GetParError(1));
  return ga1.GetParameter(1);

}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::InitQAHists() {

  scehist  = new TH1F("SpcChgEvt","SpaceCharge fit vs. Event",
                      EVN,0.,EVN);
  timehist = new TH1F("EvtTime","Event Times",
                      EVN,0.,EVN);
  dcehist  = new TH2F("DcaEve","psDCA vs. Event",
                      EVN,0.,EVN,DCN,DCL,DCH);
  dcphist  = new TH3F("DcaPhi","psDCA vs. Phi",
                      PHN,0,PI2,DCN,DCL,DCH,(QAmode ? 3 : 1),-1.5,1.5);

  AddHist(scehist);
  AddHist(timehist);
  AddHist(dcehist);
  AddHist(dcphist);

  if (QAmode) {
    myhist   = new TH3F("SpcEvt","SpaceCharge vs. Phi vs. Event",
                        EVN,0.,EVN,PHN,0,PI2,SCN2,SCL,SCH);
    dcahist  = new TH3F("DcaEvt","psDCA vs. Phi vs. Event",
                        EVN,0.,EVN,PHN,0,PI2,DCN,DCL,DCH);
    dczhist  = new TH2F("DcaZ","psDCA vs. Z",
                        //80,-200,200,250,-5.0,5.0);
                        ZN,ZL,ZH,DCN,DCL,DCH);
    myhistN  = new TH3F("SpcEvtN","SpaceCharge vs. Phi vs. Event Neg",
                        EVN,0.,EVN,PHN,0,PI2,SCN2,SCL,SCH);
    myhistP  = new TH3F("SpcEvtP","SpaceCharge vs. Phi vs. Event Pos",
                        EVN,0.,EVN,PHN,0,PI2,SCN2,SCL,SCH);
    myhistE  = new TH3F("SpcEvtE","SpaceCharge vs. Phi vs. Event East",
                        EVN,0.,EVN,PHN,0,PI2,SCN2,SCL,SCH);
    myhistW  = new TH3F("SpcEvtW","SpaceCharge vs. Phi vs. Event West",
                        EVN,0.,EVN,PHN,0,PI2,SCN2,SCL,SCH);
    dcahistN = new TH3F("DcaEvtN","psDCA vs. Phi vs. Event Neg",
                        EVN,0.,EVN,PHN,0,PI2,DCN,DCL,DCH);
    dcahistP = new TH3F("DcaEvtP","psDCA vs. Phi vs. Event Pos",
                        EVN,0.,EVN,PHN,0,PI2,DCN,DCL,DCH);
    dcahistE = new TH3F("DcaEvtE","psDCA vs. Phi vs. Event East",
                        EVN,0.,EVN,PHN,0,PI2,DCN,DCL,DCH);
    dcahistW = new TH3F("DcaEvtW","psDCA vs. Phi vs. Event West",
                        EVN,0.,EVN,PHN,0,PI2,DCN,DCL,DCH);
    cutshist = new TH1I("CutsHist","Step at which tracks and events are cut",
                        64,-0.5,63.5); // Use < 16 for event-wise cuts
    AddHist(myhist);
    AddHist(dcahist);
    AddHist(dczhist);
    AddHist(myhistN);
    AddHist(myhistP);
    AddHist(myhistE);
    AddHist(myhistW);
    AddHist(dcahistN);
    AddHist(dcahistP);
    AddHist(dcahistE);
    AddHist(dcahistW);
    AddHist(cutshist);
  }

  if (doNtuple) ntup = new TNtuple("SC","Space Charge",
    "sc:dca:zdcx:zdcw:zdce:bbcx:bbcw:bbce:bbcbb:bbcyb:intb:inty:fill:mag:run:event:dcan:dcap:dcae:dcaw:gapf:gapi:gapd:gapfn:gapin:gapdn:gapfp:gapip:gapdp:gapfe:gapie:gapde:gapfw:gapiw:gapdw:usc:uscmode:ugl:zdcc:bbcc:vpdx:vpdw:vpde:zdcxnk:zdcwnk:zdcenk:const0:const1:sce:scw:usce:sc1:gapf1:gapi1:sc2:gapf2:gapi2:sc3:gapf3:gapi3:sc4:gapf4:gapi4:sc5:gapf5:gapi5:sc6:gapf6:gapi6:sc7:gapf7:gapi7:sc8:gapf8:gapi8:sc9:gapf9:gapi9:sc10:gapf10:gapi10:sc11:gapf11:gapi11:sc12:gapf12:gapi12:sc13:gapf13:gapi13:sc14:gapf14:gapi14:sc15:gapf15:gapi15:sc16:gapf16:gapi16:sc17:gapf17:gapi17:sc18:gapf18:gapi18:sc19:gapf19:gapi19:sc20:gapf20:gapi20:sc21:gapf21:gapi21:sc22:gapf22:gapi22:sc23:gapf23:gapi23:sc24:gapf24:gapi24");

  if (doGaps) {
    gapZhist = new TH2F("Gaps","Gaps",GZN,GZL,GZH,GN,GL,GH);
    gapZhistneg = new TH2F("Gapsneg","Gaps Neg",GZN,GZL,GZH,GN,GL,GH);
    gapZhistpos = new TH2F("Gapspos","Gaps Pos",GZN,GZL,GZH,GN,GL,GH);
    if (doSecGaps) for (int i=0; i<24; i++) {
      int j = i+1;
      schistS[i]  = new TH1F(Form("SpCh%02d",j),Form("Space Charge%02d",j),SCN1,SCL,SCH);
      gapZhistS[i] = new TH2F(Form("Gaps%02d",j),Form("Gaps%02d",j),GZN,GZL,GZH,GN,GL,GH);
      gapZhistnegS[i] = new TH2F(Form("Gapsneg%02d",j),Form("Gaps Neg%02d",j),GZN,GZL,GZH,GN,GL,GH);
      gapZhistposS[i] = new TH2F(Form("Gapspos%02d",j),Form("Gaps Pos%02d",j),GZN,GZL,GZH,GN,GL,GH);
    }
  }

}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::WriteQAHists() {
// Only if QAmode or doNtuple or doGaps

  if (!(QAmode || doNtuple || doGaps)) return;

  if (runid == 0) {
    gMessMgr->Error("StSpaceChargeEbyEMaker: No runid => No output");
    return;
  }

  const char* f1 = GetName();
  runid -= 1000000*(runid/1000000); // Remove the year, optional
  runid *= 100; // limits one run to 100 files!
  TString fname = "./hists";
  if (PrePassmode) fname.Append("Pre");
  gSystem->cd(fname.Data());
  while (f1) {
    fname = Form("Hist%d.root",runid);
    f1 = gSystem->Which(".",fname.Data());
    runid++;
    delete [] f1;
  }

  TFile ff(fname.Data(),"RECREATE");
  if (QAmode) {
    myhist->Write();
    dcehist->Write();
    dcphist->Write();
    dcahist->Write();
    dczhist->Write();
    myhistN->Write();
    dcahistN->Write();
    myhistP->Write();
    dcahistP->Write();
    myhistE->Write();
    dcahistE->Write();
    myhistW->Write();
    dcahistW->Write();
    scehist->Write();
    timehist->Write();
    cutshist->Write();
  }
  if (doGaps) {
    gapZhist->Write();
    gapZhistneg->Write();
    gapZhistpos->Write();
    if (doSecGaps) for (int i=0; i<24; i++) {
      schistS[i]->Write();
      gapZhistS[i]->Write();
      gapZhistnegS[i]->Write();
      gapZhistposS[i]->Write();
    }
  }
  if (doNtuple) ntup->Write();
  if (SCcorrection) {
    SCcorrection->Write();
    SCEWRatio->Write();
    GLcorrection->Write();
  }
  ff.Close();

  gMessMgr->Info() << "QA hists file: " << fname.Data() << endm;

  gSystem->cd("..");

}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::FillQAHists(float DCA, float space, float spaceEW,
                                         int ch, StPhysicalHelixD& hh, int e_or_w) {
  // Find a "Phi" for the track
  pairD pls = hh.pathLength(97.0);
  double pl = pls.first;
  if (TMath::Abs(pls.second) < TMath::Abs(pls.first)) pl = pls.second;
  StThreeVectorD hh_at_pl = hh.at(pl);
  float Phi = hh_at_pl.phi();
  while (Phi < 0) Phi += PI2;
  while (Phi >= TMath::TwoPi()) Phi -= PI2;
  
  // To pile all sectors atop each other:
  // while (Phi >= TMath::Pi()/6.) Phi -= TMath::Pi()/6.;

  float evtn = ((float) evt) - 0.5;
  dcehist->Fill(evtn,DCA);
  dcphist->Fill(Phi,DCA,(float) e_or_w);

  if (QAmode) {
    myhist->Fill(evtn,Phi,space);
    dcahist->Fill(evtn,Phi,DCA);
    if (ch > 0) {
      myhistP->Fill(evtn,Phi,space);
      dcahistP->Fill(evtn,Phi,DCA);
    } else {
      myhistN->Fill(evtn,Phi,space);
      dcahistN->Fill(evtn,Phi,DCA);
    }
    if (e_or_w > 0) {
      myhistW->Fill(evtn,Phi,spaceEW);
      dcahistW->Fill(evtn,Phi,DCA);
    } else if (e_or_w < 0) {
      myhistE->Fill(evtn,Phi,spaceEW);
      dcahistE->Fill(evtn,Phi,DCA);
    }
    if ((e_or_w != 0) && (TMath::Abs(hh.dipAngle()) < 0.05)) dczhist->Fill(hh_at_pl.z(),DCA);
  }
}
//_____________________________________________________________________________
int StSpaceChargeEbyEMaker::imodHN(int i) {
  // Keep index in bounds of circular queue
  return ( i >= SCHN ? imodHN(i-SCHN) : (i < 0 ? imodHN(i+SCHN) : i) );
}
//_____________________________________________________________________________
float StSpaceChargeEbyEMaker::oldness(int i, int j) {
  // Deterime how to treat relative "age" of event
  // In PrePassmode, earliest events are most important!
  float s = 1.0;
  if (!PrePassmode) { // Weight newest the most (or evenly for PrePass)
    if (j<0) j = curhist;

    // Weight in sub-second intervals by # of events because
    // times have only 1 second granularity (best we can do).
    // Method assumes time-ordering of events, which is violated
    // perhaps only occasionally from DAQ.
    int k = i;
    while (k!=j) {
      k = imodHN(k+1);
      if (times[k] != times[i]) { k = imodHN(k-1); break; }
    }
    // # seconds + fraction of a second:
    float time_factor = (times[j]-times[i]) + (1.-(evtstbin[i]/evtstbin[k]));
    //float time_factor = (times[j]-times[i]);
    float decay_const = -0.12;
    // float decay_const = -0.15;
    s = exp( decay_const * time_factor );
  }
  return s;
}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::BuildHist(int i, TH1F* aschist, TH1F** aschists) {
  // Build up one histogram from several events
  aschist->Reset();
  int isc = curhist;
  aschist->Add(aschists[isc],1.0);
  while (isc != i) {
    isc = imodHN(isc-1);
    aschist->Add(aschists[isc],oldness(isc));
  }
}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::SetTableName() {
  // Problem caused if first event comes later in time than other events.
  // Solution: subtract 10 seconds...
  TUnixTime ux(GetDateTime(),1);
  ux+=-10;
  int date,time;
  ux.GetGTime(date,time);
  gMessMgr->Info() << "first event date = " << date << endm;
  gMessMgr->Info() << "first event time = " << time << endm;
  tabname = Form("./StarDb/Calibrations/rich/spaceChargeCorR2.%08d.%06d.C",date,time);

  // Set default parameters based on data time
  //   for non-calib modes
  if (TrackInfomode) {
    if (TrackInfomode<2) return; // use standard defaults
    // ...else show pile-up tracks too
    if (TrackInfomode>2) setMinTpcHits(0);
    setVtxEmcMatch(0);
    setVtxTofMatch(0);
    setVtxMinTrks(0);
    setReqEmcOrTofMatch(kFALSE);
  } else if (date < 20071000) {
    setVtxEmcMatch(0);
    setVtxTofMatch(0);
    setVtxMinTrks(10);
    setReqEmcOrTofMatch(kFALSE);
    MINTRACKS = 1500;
    setVtxVpdAgree(-5);
  } else if (date < 20090000) {
    setVtxEmcMatch(1);
    setVtxTofMatch(0);
    setVtxMinTrks(5);
    setReqEmcOrTofMatch(kFALSE);
    MINTRACKS = 1500;
    setVtxVpdAgree(-5);
  } else if (date < 20100000) {
    setVtxVpdAgree(-5);
  }
}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::WriteTableToFile(){
  gMessMgr->Info() << "Writing new table to:\n  "
    << tabname.Data() << endm;
  TString dirname = gSystem->DirName(tabname.Data());
  TString estr = dirname;
  estr.Prepend("mkdir -p ");
  gSystem->Exec(estr.Data());
  if (gSystem->OpenDirectory(dirname.Data())==0) {
    if (gSystem->mkdir(dirname.Data())) {
      gMessMgr->Warning() << "Directory creation failed for:\n  " << dirname
        << "\n  Putting table file in current directory" << endm;
      tabname.Remove(0,tabname.Last('/')).Prepend(".");
    }
  }
  ofstream *out = new ofstream(tabname.Data());
  SCTable()->SavePrimitive(*out,"");
  return;
}
//_____________________________________________________________________________
St_spaceChargeCor* StSpaceChargeEbyEMaker::SCTable() {
  St_spaceChargeCor* table = new St_spaceChargeCor("spaceChargeCorR2",1);
  spaceChargeCor_st* row = table->GetTable();
  row->fullFieldB = sc;
  row->halfFieldB = sc;
  row->zeroField  = (float) evt;
  row->halfFieldA = sc;
  row->fullFieldA = sc;
  row->satRate = 1.0;
  row->factor = 1.0;
  row->detector = 3;
  row->offset = 0;
  row->ewratio = m_ExB->CurrentSpaceChargeEWRatio();
  table->SetNRows(1);
  return table;
}
//_____________________________________________________________________________
float StSpaceChargeEbyEMaker::FakeAutoSpaceCharge() {
  // Use this to mimic what is done with the scalers, using your own code
  float zdcsum = runinfo->zdcWestRate()+runinfo->zdcEastRate();
  float sc = 6e-8 * zdcsum;
  
  return sc;
}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::FillGapHists(StTrack* tri, StPhysicalHelixD& hh,
                                          int e_or_w, int ch) {
  St_tpcPadConfigC* pads = St_tpcPadConfigC::instance();
  float fsign = ( event->runInfo()->magneticField() < 0 ? -1 : 1 );
  StPtrVecHit hts = tri->detectorInfo()->hits(kTpcId);
  float gap = 0.; float zgap = 0.; int ct=0;
  float GAPRADIUS = 121.8; // messy to get from the database (see StMagUtilities)
  Int_t sector = -1;
  for (UInt_t ht=0; ht<hts.size(); ht++) {
    StTpcHit* hit = (StTpcHit*) hts[ht];
    Int_t prow = hit->padrow();
    Int_t sec = hit->sector();
    int lastInner = pads->innerPadRows(sec);
    if ((prow != lastInner) && (prow != lastInner+1)) continue;
    float gsign = ( prow > lastInner ? -1 : 1 );
    const StThreeVectorF& hp = hit->position();

    // Avoid sector edges
    float hphi = hp.phi() + TMath::TwoPi();
    while (hphi > TMath::Pi()/12.) hphi -= TMath::Pi()/6.;
    if (TMath::Abs(hphi) > 0.75*TMath::Pi()/12.) break;

    float distToOuterRow = pads->radialDistanceAtRow(sec,lastInner+1) - GAPRADIUS;
    float distToInnerRow = GAPRADIUS - pads->radialDistanceAtRow(sec,lastInner + inGapRow - 13);
    zgap += (hp.z() / (distToInnerRow+distToOuterRow)) *
            ( prow == lastInner ? distToOuterRow : distToInnerRow ); // ~z at gap

    // Measurement method described at:
    // http://drupal.star.bnl.gov/STAR/blog/genevb/2010/feb/21/gridleak-update-using-residuals-along-padrows
    Double_t residual = hh.geometricSignedDistance(hp.x(),hp.y());

    sector = ( sec == sector || sector < 0 ? sec : 0 ); // 0 if crossing sectors
    if (sector == 0) return; // don't use sector-crossing tracks!
    Double_t sector_angle = (TMath::Pi()/6.) * (sec < 13 ? 3 - sec : sec - 21);
    Double_t pathlen = hh.pathLength(hp.x(),hp.y());
    Double_t theta = TMath::ATan2(hh.cy(pathlen),hh.cx(pathlen)) - sector_angle;
    Double_t phi = hp.phi() - sector_angle;

    Double_t Eff1 = TMath::Cos(theta);
    Double_t Eff2 = 1;
    Double_t Eff3 = 0;

    Float_t x1[3],x2[3];
    x1[0] = hp.perp()*TMath::Sin(phi);
    x1[1] = hp.perp()*TMath::Cos(phi);
    x1[2] = hp.z();
    m_ExB->UndoGridLeakDistortion(x1,x2,sec);
    Double_t dX = x2[0]-x1[0];
    if (TMath::Abs(dX) > 1e-20) {
      // warning: no available GridLeak calculation may lead
      //   to slightly different results
      Eff3 = gsign * ((x2[1]-x1[1])/dX) * TMath::Sin(theta);
      x1[0] = 0;
      m_ExB->UndoGridLeakDistortion(x1,x2,sec);
      Eff2 = (x2[0]-x1[0])/dX;
    }

    Double_t DistortionX = Eff2 * residual / (Eff1 + Eff3);
 
    gap += fsign * gsign * DistortionX;
    ct++;
  }

  sector = (doSecGaps ? sector - 1 : -1);
  float abs_zgap = TMath::Abs(zgap);
  if ((ct==2) && (abs_zgap<200.0) && (abs_zgap>10.0)) {
     gapZhist->Fill(zgap,gap);
     if (ch==1) gapZhistpos->Fill(zgap,gap);
     else gapZhistneg->Fill(zgap,gap);
     if (sector >= 0) {
       gapZhistS[sector]->Fill(zgap,gap);
       if (ch==1) gapZhistposS[sector]->Fill(zgap,gap);
       else gapZhistnegS[sector]->Fill(zgap,gap);
     }

     if (abs_zgap<150 && abs_zgap>25) { // Restrict the Z range further
       // normalize at z=100cm from ggrid
       float gap_scaled = (gap * 100.0) / (ZGGRID - abs_zgap);
       //float gap_scaled = (gap * 100.0) / (350.0 - abs_zgap);
       float z_beyond = ZGGRID+1.0;
       gapZhist->Fill(z_beyond,gap_scaled);
       if (ch==1) gapZhistpos->Fill(z_beyond,gap_scaled);
       else gapZhistneg->Fill(z_beyond,gap_scaled);
       if (sector >= 0) {
         gapZhistS[sector]->Fill(z_beyond,gap_scaled);
         if (ch==1) gapZhistposS[sector]->Fill(z_beyond,gap_scaled);
         else gapZhistnegS[sector]->Fill(z_beyond,gap_scaled);
       }
     }
   }
}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::DetermineGaps() {
  TString GapStr = "Gap measurements: fit slope & intercept, div slope";

  if (doSecGaps) {
    GapStr += "\nneg (by sector):";
    for (int i=0; i<24; i++) {
      GapStr += Form("\n%2d : ",i+1);
      GapStr += DetermineGapHelper(gapZhistnegS[i],gapZfitslopenegS[i],gapZfitinterceptnegS[i],gapZdivslopenegS[i]);
    }
    GapStr += "\npos (by sector):";
    for (int i=0; i<24; i++) {
      GapStr += Form("\n%2d : ",i+1);
      GapStr += DetermineGapHelper(gapZhistposS[i],gapZfitslopeposS[i],gapZfitinterceptposS[i],gapZdivslopeposS[i]);
    }
    GapStr += "\nall (by sector):";
    for (int i=0; i<24; i++) {
      GapStr += Form("\n%2d : ",i+1);
      GapStr += DetermineGapHelper(gapZhistS[i],gapZfitslopeS[i],gapZfitinterceptS[i],gapZdivslopeS[i]);
    }
    GapStr += "\nAll sectors:";
  }
    
  GapStr += "\nneg: ";
  GapStr += DetermineGapHelper(gapZhistneg,gapZfitslopeneg,gapZfitinterceptneg,gapZdivslopeneg);
  GapStr += "\npos: ";
  GapStr += DetermineGapHelper(gapZhistpos,gapZfitslopepos,gapZfitinterceptpos,gapZdivslopepos);
  GapStr += "\nall: ";
  GapStr += DetermineGapHelper(gapZhist,gapZfitslope,gapZfitintercept,gapZdivslope);

  LOG_INFO << GapStr << endm;
}
//_____________________________________________________________________________
TString StSpaceChargeEbyEMaker::DetermineGapHelper(TH2F* gh,
      float& fitslope, float& fitintercept, float& divslope) {

  TString result = "n/a";
  if (gh->GetEntries() < 10) { // don't waste time fitting ~empty histograms
    fitslope = 0; fitintercept = 0; divslope = 0;
    egapZfitslope = 0; egapZfitintercept = 0; egapZdivslope = 0;
    return result;
  }
  ga1.SetParameters(gh->GetEntries()/(16.*2.*10.),0.,0.1);
  ga1.SetParLimits(0,0.001,10.0*gh->GetEntries()); // Loglikelihood only works with positive functions
  gh->FitSlicesY(&ga1,1,0,20,"LB0Q"); // gapZhist bin contents are integers
  ga1.ReleaseParameter(0);
  const char* hn = gh->GetName();
  TH1D* GapsChi2 = (TH1D*) gDirectory->Get(Form("%s_chi2",hn));
  TH1D* GapsAmp  = (TH1D*) gDirectory->Get(Form("%s_0",hn));
  TH1D* GapsMean = (TH1D*) gDirectory->Get(Form("%s_1",hn));
  TH1D* GapsRMS  = (TH1D*) gDirectory->Get(Form("%s_2",hn));

  divslope = GapsMean->GetBinContent(GZN);
  egapZdivslope = TMath::Abs(GapsMean->GetBinError(GZN));

  // Fit only from |Z| = 25-150cm
  // Zero bins -25 thru 25cm (depends upon GZN,GZL,GZH definitions)
  GapsMean->SetBinContent(8,0); GapsMean->SetBinError(8,0);
  GapsMean->SetBinContent(9,0); GapsMean->SetBinError(9,0);

  ln1.SetRange(-150.,150.);
  GapsMean->Fit(&ln1,"R0Q");
  fitslope = ln1.GetParameter(1);
  fitintercept = ln1.GetParameter(0);

  // remaining variables save the last histogram fit: "Gaps" for now

  egapZfitslope = TMath::Abs(ln1.GetParError(1));
  egapZfitintercept = TMath::Abs(ln1.GetParError(0));

  ln1.SetRange(-150.,0.);
  GapsMean->Fit(&ln1,"R0Q");
  gapZfitslopeeast = ln1.GetParameter(1);
  gapZfitintercepteast = ln1.GetParameter(0);

  ln1.SetRange(0.,150.);
  GapsMean->Fit(&ln1,"R0Q");
  gapZfitslopewest = ln1.GetParameter(1);
  gapZfitinterceptwest = ln1.GetParameter(0);

  delete GapsChi2;
  delete GapsAmp;
  delete GapsMean;
  delete GapsRMS;

  result = Form("%7.4f+/-%7.4f , %7.4f+/-%7.4f , %7.4f+/-%7.4f",
                fitslope,egapZfitslope,fitintercept,egapZfitintercept,
                divslope,egapZdivslope);
  return result;
}
//_____________________________________________________________________________
float StSpaceChargeEbyEMaker::EvalCalib(TDirectory* hdir) {

  if (hdir) {
    dcehist = static_cast<TH2F*>(hdir->Get("DcaEve"));
    timehist = static_cast<TH1F*>(hdir->Get("EvtTime"));
    scehist = static_cast<TH1F*>(hdir->Get("SpcChgEvt"));
    if (!(dcehist && timehist && scehist)) {
      LOG_ERROR << "Problems finding SC histograms" << endm;
      return 999.;
    }
  }

  // Other counts
  float spc = (float) (scehist->GetEntries());
  float dcc = (float) (dcehist->GetEntries());
  float evc = (float) (timehist->GetEntries());

  float hm=0,hw=0,gm=0,gw=0,gm1=0,gw1=0,gme=0,gwe=0,pm=0,pw=0,epsec=0,frac=0,wid=9.99;
  TF1* pl0 = 0;

  if (dcc>0) {
    TH1D* dcaproj = dcehist->ProjectionY();

    // Initial fits to DCA distribution
    ga1.SetParameters(1.,0.,1.);
    dcaproj->Fit(&ga1,"Q");
    hm = ga1.GetParameter(1);
    hw = TMath::Abs(ga1.GetParameter(2));
    float hd = 0.6*hw;
    ga1.SetRange(hm-hd,hm+hd);
    dcaproj->Fit(&ga1,"RQ");
    gm = ga1.GetParameter(1);
    gw = TMath::Abs(ga1.GetParameter(2));
    gm1 = gm;
    gw1 = gw;

    // Iterate fit to get best answer
    ga1.SetRange(gm-0.9*gw,gm+0.9*gw);
    dcaproj->Fit(&ga1,"RQ");
    gm = ga1.GetParameter(1);
    gw = TMath::Abs(ga1.GetParameter(2));
    ga1.SetRange(gm-0.8*gw,gm+0.8*gw);
    dcaproj->Fit(&ga1,"RQ");
    gm = ga1.GetParameter(1);
    gw = TMath::Abs(ga1.GetParameter(2));
    ga1.SetRange(gm-0.7*gw,gm+0.7*gw);
    dcaproj->Fit(&ga1,"RQ");
    gm = ga1.GetParameter(1);
    gw = TMath::Abs(ga1.GetParameter(2));
    gme = TMath::Abs(ga1.GetParError(1));
    gwe = TMath::Abs(ga1.GetParError(2));

    // Quality measures
    wid = gw1*gw1+gw*gw;
    if (wid>0) wid = TMath::Min(10.,TMath::Log10(wid));
  }

  if (spc>0) {
    // Average SC
    scehist->Fit("pol0","Q");
    pl0 = scehist->GetFunction("pol0");
    if (pl0) {
      pm = pl0->GetParameter(0);
      pw = pl0->GetParError(0);
    }
  }

  if (evc) {
    timehist->GetXaxis()->SetRange(1,(int) evc);
    timehist->Fit("pol0","LQ");
    pl0 = timehist->GetFunction("pol0");
    if (pl0) epsec = pl0->GetParameter(0); // events per second

    // Quality measures
    frac = spc/evc; // fraction of events for which SC was found
  }


  float code=0;
  if (frac<0.2) code = 1. + frac;               // code = 1.x
  else if (wid>0) code = 2. + 0.1*wid;          // code = 2.x

  LOG_INFO << Form("SCeval: %f %f %f %f %f %f %f %f %f %f %f %f %f %f\n",
    hm,hw,gm,gw,pm,pw,gm1,gw1,spc,dcc,evc,epsec,gme,gwe) << endm;

  if (code>0) {
    LOG_ERROR << "CheckFail: Break of SpaceCharge performance! code = " << code << endm;
  }

  return code;
}
//_____________________________________________________________________________
// $Id: StSpaceChargeEbyEMaker.cxx,v 1.72 2018/10/19 21:06:17 genevb Exp $
// $Log: StSpaceChargeEbyEMaker.cxx,v $
// Revision 1.72  2018/10/19 21:06:17  genevb
// Clean up after Y. Fisyak modifications (which were for iTPC, not dE/dx), and use new PredictSpaceCharge() using real hit radii
//
// Revision 1.71  2018/10/17 20:45:27  fisyak
// Restore update for Run XVIII dE/dx calibration removed by Gene on 08/07/2018
//
// Revision 1.70  2018/09/21 18:22:47  genevb
// Bug fix for wrong vertex daughter PCTs function
//
// Revision 1.69  2018/06/08 16:39:59  genevb
// Needs explicity include of TROOT.h
//
// Revision 1.68  2018/06/08 15:35:06  genevb
// Needs explicity include of TSystem.h
//
// Revision 1.67  2018/02/15 03:25:29  genevb
// Restore prepass settings for old data
//
// Revision 1.66  2017/04/12 19:47:25  genevb
// Use generic GridLeak function
//
// Revision 1.65  2017/02/14 23:38:38  fisyak
// Adjustment to structure changes in StTpcdEdxCorrection
//
// Revision 1.64  2015/06/30 21:44:31  genevb
// Use an initialization call for StMagUtilities for each event
//
// Revision 1.63  2015/05/23 04:26:07  genevb
// More vertex selection criteria: PCT daughters, and VPD z agreement
//
// Revision 1.62  2015/05/19 19:36:09  genevb
// Code cleanup in preparation for C++11
//
// Revision 1.61  2015/05/15 14:34:45  genevb
// Fix incorrect memset usage (RT 3093)
//
// Revision 1.60  2015/03/11 21:38:52  genevb
// HFT era: no tracks with PXL or IST hits in calibration
//
// Revision 1.59  2014/11/17 20:49:09  genevb
// Store east and west gapf in the ntuple
//
// Revision 1.58  2014/10/23 21:07:23  genevb
// Add GridLeak-by-sector codes, East/WestOff handling, and some code reformatting
//
// Revision 1.57  2014/07/23 17:58:46  genevb
// Machinery for sector-by-sector Gaps (GridLeak) measurements
//
// Revision 1.56  2014/06/26 22:06:26  fisyak
// New Tpc Alignment, v632
//
// Revision 1.55  2014/05/15 17:14:03  genevb
// Minor tweaks for TrackInfo mode
//
// Revision 1.54  2014/05/02 02:38:07  genevb
// TrackInfo mode with pile-up tracks too
//
// Revision 1.53  2014/01/02 20:54:28  genevb
// TrackInfomode, and Basic E/W asymmetry functionality
//
// Revision 1.52  2013/09/25 20:55:51  genevb
// Allow use of multiple PPVF vertices, introduce EmcOrTofMatch, keep track of Predict...() cuts
//
// Revision 1.51  2013/04/26 20:00:54  genevb
// Protection against 0 entry histos for EvalCalib()
//
// Revision 1.50  2013/03/11 20:04:31  genevb
// make ugl and average over data
//
// Revision 1.49  2013/03/09 23:37:35  genevb
// Add NoKiller ZDC data to ntuple
//
// Revision 1.48  2013/03/07 23:12:30  genevb
// Improve FindPeak(), particularly for sc hists at high lumi, and add StMagUtil:const0,1 to ntuple
//
// Revision 1.47  2013/02/19 21:07:58  genevb
// Print out the sc and GL correction formulas
//
// Revision 1.46  2012/12/28 22:04:25  genevb
// Improve chances of fits succeeding
//
// Revision 1.45  2012/12/15 03:13:50  genevb
// Store used calibrations in histogram files
//
// Revision 1.44  2012/10/01 17:50:07  genevb
// Reduce some overhead DB queries by being more specific about needed tables
//
// Revision 1.43  2012/09/13 20:58:56  fisyak
// Corrections for iTpx
//
// Revision 1.42  2012/08/18 02:11:59  genevb
// Expand SC hist ranges, add VPD to ntuple
//
// Revision 1.41  2012/04/25 19:23:55  genevb
// Pointing angle near vertex needed for improved PredictSpaceCharge
//
// Revision 1.40  2012/01/14 00:21:04  genevb
// Add code for EMC match, set default to required
//
// Revision 1.39  2011/10/27 23:11:03  genevb
// Account for pointing error in sDCA and predicted SpaceCharge
//
// Revision 1.38  2011/10/26 15:33:49  genevb
// Avoid floating point exception in Loglikelihood fits of gaussian peaks
//
// Revision 1.37  2011/04/18 17:36:52  genevb
// Expanded range for sc hists
//
// Revision 1.36  2011/02/10 18:31:45  genevb
// Restore corrected coincidence rates, add QA histogram of where events/tracks are cut
//
// Revision 1.35  2011/02/09 21:56:50  genevb
// Version which can work in SL10k
//
// Revision 1.34  2011/02/09 21:11:36  genevb
// Parameters need to be available in normal event-by-event mode too
//
// Revision 1.33  2011/02/09 16:24:18  genevb
// Allow for historical operating parameters in Prepass mode
//
// Revision 1.32  2010/11/17 17:23:33  genevb
// Include corrected coincidence rates in ntuple
//
// Revision 1.31  2010/07/09 19:01:54  genevb
// Add TOF matching
//
// Revision 1.30  2010/06/09 20:24:53  genevb
// Modify interface to allow EMC and TOF matching requirements (needs implementation)
//
// Revision 1.29  2010/02/25 21:50:18  genevb
// Pass sector number to StMagUtilities, correct unsigned int usage
//
// Revision 1.28  2010/02/24 22:58:21  genevb
// Even the highest ranked vertex might need to be above a minimum ranking
//
// Revision 1.27  2010/02/23 23:59:41  genevb
// Reduce positional biases in GridLeak measurements
//
// Revision 1.26  2010/01/27 15:11:00  fisyak
// eliminate access to StTpcDbMaker, use directly gStTpcDb
//
// Revision 1.25  2010/01/27 14:42:33  fisyak
// Use  StMagUtilities::Instance instead of asking tpcHitMoverMaker
//
// Revision 1.24  2009/11/16 22:02:19  genevb
// Loosen nDaughters cut, add BEMCmatch cut, PCT hits cut, enable padrow 13 for Run 9+
//
// Revision 1.23  2009/11/10 20:54:13  fisyak
// pams Cleanup
//
// Revision 1.22  2008/07/24 19:17:55  genevb
// SpaceChargeEWRatio must be written to prepass output table
//
// Revision 1.21  2008/07/21 21:17:10  genevb
// No call to EvalCalib() if Prepass runs successfully
//
// Revision 1.20  2008/07/17 23:35:33  jeromel
// Small format change
//
// Revision 1.19  2008/07/16 03:42:15  genevb
// Use CheckFail in failed performance check error message
//
// Revision 1.18  2008/07/15 22:30:38  genevb
// Added evaluation of calibration performance
//
// Revision 1.17  2008/04/30 14:52:15  genevb
// Reduce pileup contributions
//
// Revision 1.16  2008/01/14 19:22:49  genevb
// Fine tuning of parameters, removal of excess text in messages
//
// Revision 1.15  2007/01/25 19:04:04  perev
// GMT fix
//
// Revision 1.14  2007/01/24 21:42:22  perev
// GMT conversion fixed
//
// Revision 1.13  2006/12/16 01:00:58  genevb
// Better handling of zero magnetic field
//
// Revision 1.12  2006/08/15 23:40:59  genevb
// Averaging was done improperly in DontReset mode
//
// Revision 1.11  2006/07/17 20:13:08  genevb
// Disallow SVT points on tracks
//
// Revision 1.10  2006/07/02 23:22:36  genevb
// Allow for SVT/SSD hits on tracks (necessary for ITTF)
//
// Revision 1.9  2006/06/01 17:27:11  genevb
// Bug fix: gapd and gapf backwards; Improvements: gap fit intercepts, hist and fit ranges
//
// Revision 1.8  2006/01/05 19:12:53  genevb
// Added calib mode
//
// Revision 1.7  2005/12/07 19:45:46  perev
// Histos diconnected from directory. EndCrashFix
//
// Revision 1.6  2005/04/21 19:38:20  genevb
// Additional code for studying SpaceCharge
//
// Revision 1.5  2005/02/16 17:18:06  genevb
// Fill StEvent info on SpaceCharge
//
// Revision 1.4  2004/08/13 20:49:12  genevb
// Improve upon keeping method locked on for each event, and timestamp change
//
// Revision 1.3  2004/08/02 01:19:27  genevb
// minor fixes for getting directories correct
//
// Revision 1.2  2004/07/01 01:46:04  genevb
// Slightly larger margin for full vs half field differences
//
// Revision 1.1  2004/06/30 23:16:00  genevb
// Introduction of StSpaceChargeEbyEMaker
//
//
