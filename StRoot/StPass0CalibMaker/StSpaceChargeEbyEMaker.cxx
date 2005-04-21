//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StSpaceChargeEbyEMaker performs event-by-event determination         //
// of the space charge correction for tracks, and sets it for           //
// the next event.                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StSpaceChargeEbyEMaker.h"
#include "StDbUtilities/StMagUtilities.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StTpcDb/StTpcDb.h"
#include "StTpcDb/StTpcDbMaker.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StTpcHitMoverMaker/StTpcHitMoverMaker.h"
#include "tables/St_spaceChargeCor_Table.h"

#include "TFile.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TNtuple.h"

// Histogram ranges:
const int   SCN = 80;
const float SCL = -0.015;
const float SCH =  0.025;

const int   DCN = 75;
const float DCL = -1.5;
const float DCH =  1.5;

const int   PHN = 72;
const float PI2 = TMath::TwoPi();

const int   EVN = 1024;

const int   ZN = 60;
const float ZL = -150.;
const float ZH = 150.;

static TF1 ga1("ga1","[0]*exp(-0.5*(x-[1])*(x-[1])/([2]*[2]))");
static TF1 ln1("ln1","[0]+((208.707-abs(x))*[1]/100.0)",-150.,150.);

ClassImp(StSpaceChargeEbyEMaker)
  
//_____________________________________________________________________________
StSpaceChargeEbyEMaker::StSpaceChargeEbyEMaker(const char *name):StMaker(name),
    event(0),
    PrePassmode(kFALSE), PrePassdone(kFALSE), QAmode(kFALSE), doNtuple(kFALSE),
    doReset(kTRUE), doGaps(kFALSE),
    m_ExB(0), tpcDbMaker(0), tpcHitMoverMaker(0),
    scehist(0), timehist(0), myhist(0), myhistN(0), myhistP(0),
    myhistE(0), myhistW(0), dczhist(0), dcehist(0), dcphist(0),
    dcahist(0), dcahistN(0), dcahistP(0), dcahistE(0), dcahistW(0),
    gapZhist(0), gapZhistneg(0), gapZhistpos(0),
    gapZhisteast(0), gapZhistwest(0), ntup(0) {

  HN=96;  // max events used, cannot exceed 96 used in header file
  MINTRACKS=1500;
  SCALER_ERROR = 0.0006; // by eye from hist: SCvsZDCEpW.gif (liberal)

  // MAXDIFFE is maximum different in sc from last ebye sc
  MAXDIFFE =   SCALER_ERROR;
  // MAXDIFFA is maximum different in sc from last scaler sc
  MAXDIFFA = 2*SCALER_ERROR; // should be about equal to SCALER_ERROR, no?
           // Present uncetainties with scalers demands greater tolerance

  runid = 0;
  memset(evts,0,HN*sizeof(int));
  memset(times,0,HN*sizeof(int));
  memset(evtstbin,0,HN*sizeof(float));
  evtsnow = 0;

  SetMode(0); // default is mode 0 (no QA, no PrePass)
  //DoQAmode(); // For testing

  schist = new TH1F("SpCh","Space Charge",80,SCL,SCH);
  for (int i=0;i<HN;i++)
    schists[i] = new TH1F(Form("SpCh%d",i),"Space Charge",80,SCL,SCH);

}
//_____________________________________________________________________________
StSpaceChargeEbyEMaker::~StSpaceChargeEbyEMaker() {
  delete schist;
  for (int i=0;i<HN;i++) delete schists[i];
}
//_____________________________________________________________________________
Int_t StSpaceChargeEbyEMaker::Finish() {

  if (evt > 0) {
    if (PrePassmode) {
      if (PrePassdone) WriteTableToFile();
      else gMessMgr->Warning("StSpaceChargeEbyEMaker: NO SC => NO OUTPUT");
    }
    WriteQAHists();
  } else {
    gMessMgr->Warning("StSpaceChargeEbyEMaker: NO EVENTS => NO OUTPUT");
  }

  return kStOk;
}
//_____________________________________________________________________________
Int_t StSpaceChargeEbyEMaker::Init() {

  // Use mode to set switches:
  switch (GetMode()) {
    case (1) : DoQAmode(); break;
    case (2) : DoPrePassmode(); break;
    case (3) : DoPrePassmode(); DoQAmode();  break;
    case (10): DoNtuple(); break;
    case (11): DoNtuple(); DontReset(); break;
    case (12): DoNtuple(); DoQAmode(); break;
    case (13): DoNtuple(); DontReset(); DoQAmode(); break;
    default  : {}
  }

  evt=0;
  oldevt=1;
  lastsc=0.;
  curhist=0;
  lasttime=0;
  ntrkssum=0;
  did_auto=kTRUE;
  InitQAHists();
  if (QAmode) gMessMgr->Info("StSpaceChargeEbyEMaker: Initializing");
    
  // Find StTpcHitMoverMaker
  tpcHitMoverMaker = GetMaker("tpc_hit_mover");
  
  // Find StTpcDbMaker if no StTpcHitMoverMaker
  if (!tpcHitMoverMaker) {
    StMakerIter iter(GetParentChain());
    StMaker* mkr;
    while ((mkr = iter.NextMaker())) {
      if (mkr->IsA() == StTpcDbMaker::Class()) {
        tpcDbMaker = mkr;
        break;
      }
    }
    if (!tpcDbMaker)
      gMessMgr->Warning("StSpaceChargeEbyEMaker: No StTpcDbMaker found.");
  }
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StSpaceChargeEbyEMaker::Make() {

  // On very first event
  if (PrePassmode && (tabname.Length() == 0)) SetTableName();
  
  // Get instance of StMagUtilities
  if (tpcHitMoverMaker) {
    m_ExB = ((StTpcHitMover*) tpcHitMoverMaker)->getExB();
  } else if (!m_ExB) {
    TDataSet *RunLog = GetDataBase("RunLog");
    if (!RunLog) gMessMgr->Warning("StSpaceChargeEbyEMaker: No RunLog found.");
    m_ExB = new StMagUtilities(
      ((StTpcDbMaker*) tpcDbMaker)->tpcDbInterface(),RunLog,(kSpaceChargeR2 | kGridLeak));
  }

  // Get StEvent and related info, determine if things are OK
  event = (StEvent*) GetInputDS("StEvent");
  if (!event) {
    gMessMgr->Warning("StSpaceChargeEbyEMaker: no StEvent; skipping event.");
    return kStWarn;
  }
  StPrimaryVertex* pvtx = event->primaryVertex();
  if (!pvtx) return kStOk;
  StSPtrVecTrackNode& theNodes = event->trackNodes();
  unsigned int nnodes = theNodes.size();
  if (!nnodes) return kStOk;
  runinfo = event->runInfo();
  if ((!runinfo) || (runinfo->magneticField() == 0)) {
    if (PrePassmode) {
      gMessMgr->Warning("StSpaceChargeEbyEMaker: No PrePass for zero or unknown mag field");
      return kStFatal;
    }
    return kStOk;
  }

  // Store and setup event-wise info
  evt++;
  int thistime = event->time();
  if (lasttime) {
    timehist->SetBinContent(evt,thistime-lasttime);
  } else {
    runid = event->runId();
    ntrkssum -= ntrks[curhist];
  }
  if (doReset) {
    curhist = imodHN(curhist+1);
    schists[curhist]->Reset();
    if (doGaps) {
      gapZhist->Reset();
      gapZhisteast->Reset();
      gapZhistwest->Reset();
      gapZhistpos->Reset();
      gapZhistneg->Reset();
    }
  } else {
    if (doNtuple) ntup->Reset();
  }

  // Keep time and event number
  times[curhist] = thistime;
  evts[curhist]=evt;

  // Keep track of # of events in the same time bin
  if (thistime == lasttime) evtsnow++;
  else evtsnow = 1;
  evtstbin[curhist] = evtsnow;

  lastsc = m_ExB->CurrentSpaceChargeR2();
  if (QAmode) {
    gMessMgr->Info()
      << "StSpaceChargeEbyEMaker: used (for this event) SpaceCharge = "
      << lastsc << " (" << thistime << ")" << endm;
    gMessMgr->Info()
      << "StSpaceChargeEbyEMaker: zdc west+east = "
      << runinfo->zdcWestRate()+runinfo->zdcEastRate() << endm;
    gMessMgr->Info()
      << "StSpaceChargeEbyEMaker: zdc coincidence = "
      << runinfo->zdcCoincidenceRate() << endm;
  }

  // Fill the StEvent information for the SpaceCharge used in this event
  runinfo->setSpaceCharge(lastsc);
  runinfo->setSpaceChargeCorrectionMode(m_ExB->GetSpaceChargeMode());

  // Track loop
  unsigned int i,j;
  StThreeVectorD ooo = pvtx->position();

  for (i=0; i<nnodes; i++) {
      for (j=0; j<theNodes[i]->entries(global); j++) {
        StTrack* tri = theNodes[i]->track(global,j);
        if (!tri) continue;

          const StTrackTopologyMap& map = tri->topologyMap();
          //if (! map.trackTpcOnly()) continue;
          if (! map.hasHitInDetector(kTpcId)) continue;
          if (map.numberOfHits(kTpcId) < 25) continue;
          StTrackGeometry* triGeom = tri->geometry();

          StThreeVectorF xvec = triGeom->origin();
          if (!(xvec.x() || xvec.y() || xvec.z())) continue;
          StThreeVectorF pvec = triGeom->momentum();
          if (!(pvec.x() || pvec.y())) continue;

          float oldPt = pvec.perp();
          if (oldPt < 0.0001) continue;

	  int e_or_w = 0; // east is -1, west is +1
	  if (pvec.z() * xvec.z() > 0) e_or_w = ( (xvec.z() > 0) ? 1 : -1 );

          StPhysicalHelixD hh = triGeom->helix();

          Float_t eta=pvec.pseudoRapidity();
          //Float_t DCA=hh.geometricSignedDistance(0,0); // for testing only
          Float_t DCA=hh.geometricSignedDistance(ooo.x(),ooo.y());
          Int_t ch = (int) triGeom->charge();
          Float_t space = 10000.;
          if (!(m_ExB->PredictSpaceChargeDistortion(ch,oldPt,ooo.z(),
	     eta,DCA,map.data(0),map.data(1),space))) continue;

	  space += lastsc;  // Assumes additive linearity of space charge!
	  schists[curhist]->Fill(space);
          FillQAHists(DCA,space,ch,hh,e_or_w);


          if ((doGaps) &&
              (map.hasHitInRow(kTpcId,12))&&(map.hasHitInRow(kTpcId,14)) &&
              (map.hasHitInRow(kTpcId,11))&&(map.hasHitInRow(kTpcId,15)) &&
              (e_or_w!=0) && (TMath::Abs(ch)==1) && (oldPt>0.3))
            FillGapHists(tri,hh,e_or_w,ch);

      } // loop over j tracks
  } // loop over i Nodes

  
  ntrks[curhist] = schists[curhist]->Integral();
  ntrkssum += ntrks[curhist];

  // Wrap it up and make a decision
  int result = DecideSpaceCharge(thistime);

  if (doGaps) DetermineGaps();
  if (doNtuple) {
      static float X[32];
      static float ntent = 0.0;

      if (ntent == 0.0) for (i=0; i<32; i++) X[i] = 0.0;
      ntent++;
      float s1 = ( doReset ? 1.0 : 1.0/ntent );
      float s0 = 1.0 - s1;

      float ee;
      int fbin = ( doReset ? evt : 1 );

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
      X[15] = event->id();
      //X[16] = sceast;
      //X[17] = scwest;
      if ((QAmode) && (evt <= EVN)) {
        X[16] = FindPeak(dcahistN->ProjectionZ("_dnz",fbin,evt,1,PHN),ee);
        X[17] = FindPeak(dcahistP->ProjectionZ("_dpz",fbin,evt,1,PHN),ee);
        X[18] = FindPeak(dcahistE->ProjectionZ("_dez",fbin,evt,1,PHN),ee);
        X[19] = FindPeak(dcahistW->ProjectionZ("_dwz",fbin,evt,1,PHN),ee);
      }
      X[20] = gapZfitslope;
      X[21] = gapZdivslope;
      X[22] = gapZfitslopeneg;
      X[23] = gapZdivslopeneg;
      X[24] = gapZfitslopepos;
      X[25] = gapZdivslopepos;
      X[26] = gapZfitslopeeast;
      X[27] = gapZdivslopeeast;
      X[28] = gapZfitslopewest;
      X[29] = gapZdivslopewest;
      X[30] = s0*X[30] + s1*runinfo->spaceCharge();
      X[31] = (float) (runinfo->spaceChargeCorrectionMode());
      ntup->Fill(X);
  }
  
  lasttime = thistime;
  
  return result;
}
//_____________________________________________________________________________
Int_t StSpaceChargeEbyEMaker::DecideSpaceCharge(int time) {

  // curhist has only this event
  // curhist-1 has past two events...
  // curhist-x has past (x+1) events
  // curhist-(HN-1) == curhist+1 has past HN events

  Bool_t QAout = QAmode || PrePassmode;
  Bool_t do_auto = kTRUE;
  Bool_t few_stats = kTRUE;
  Bool_t large_err = kTRUE;
  Bool_t large_dif = kTRUE;

  // If last event was auto, use MAXDIFFA, else use between MAXDIFFE & MAXDIFFA
  //   scaled by oldness of previous sc measure (curhist-1)
  float maxdiff;
  if (did_auto)
    maxdiff = MAXDIFFA;
  else
    maxdiff = MAXDIFFA - (MAXDIFFA-MAXDIFFE)*oldness(imodHN(curhist-1));

  // More than 30 seconds since last used event? Forget it...
  int timedif = time-lasttime;
  if (QAout) gMessMgr->Info() <<
    "StSpaceChargeEbyEMaker: time since last event = " <<
    timedif << endm;
  if ((lasttime==0) || (timedif < 30)) {
  
    int isc;
    float ntrkstot = 0; // running sum using oldness scale factor
    float dif=0; // difference from lastsc
    for (isc=0; isc<HN; isc++) {
      int i = imodHN(curhist-isc);
      ntrkstot += ntrks[i] * oldness(i);
      if (QAout) {
        if (!isc) gMessMgr->Info("StSpaceChargeEbyEMaker: i, ni, oi, nt:");
        gMessMgr->Info() << "StSpaceChargeEbyEMaker: " << i << ", "
          << ntrks[i] << ", " << oldness(i) << ", " << ntrkstot << endm;
      }

      // Too little data collected? Keep trying...
      few_stats = ntrkstot < MINTRACKS;
      if (!few_stats) {
	BuildHist(i);
        FindSpaceCharge();
        if (QAout) gMessMgr->Info()
          << "StSpaceChargeEbyEMaker: sc = " << sc
          << " +/- " << esc << endm;
        large_err = (esc == 0) || (esc > SCALER_ERROR);
        if (!large_err) {
          if (PrePassmode) { do_auto=kFALSE; break; }
          dif = TMath::Abs(sc-lastsc);
          large_dif = dif > maxdiff;
          if (!large_dif) {
            oldevt = evts[i];
            do_auto=kFALSE;
            break;
          }
        }
      }

      // shouldn't need to go past oldest event previously used
      if (evts[i] <= oldevt) {
        if (QAout) {
          if (few_stats) gMessMgr->Info()
            << "StSpaceChargeEbyEMaker: (RECENT) STATS TOO FEW: "
            << ntrkstot << " (" << MINTRACKS << ")" << endm;
          else if (large_err) gMessMgr->Info()
	    << "StSpaceChargeEbyEMaker: FIT ERROR TOO LARGE: "
            << esc << " (" << SCALER_ERROR << ")" << endm;
	  else if (large_dif) gMessMgr->Info()
	    << "StSpaceChargeEbyEMaker: DIFFERENCE TOO LARGE: "
            << dif << " (" << maxdiff << ")" << endm;
        }
	break;
      }
    }
    if (QAout && (isc == HN)) gMessMgr->Info()
      << "StSpaceChargeEbyEMaker: STORED DATA EXHAUSTED: "
      << HN << " events" << endm;
  }

  did_auto = do_auto;

  if (do_auto) {
    gMessMgr->Info("StSpaceChargeEbyEMaker: using auto SpaceCharge");
    m_ExB->AutoSpaceChargeR2();
  } else {
    gMessMgr->Info() << "StSpaceChargeEbyEMaker: using SpaceCharge = "
      << sc << " +/- " << esc << " (" << time << ")" << endm;
    scehist->SetBinContent(evt,sc);
    scehist->SetBinError(evt,esc);
    if (PrePassmode) {
      PrePassdone = kTRUE;
      return kStStop; // We're happy! Let's stop!
    }
    m_ExB->ManualSpaceChargeR2(sc);
  }
  return kStOk;
}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::FindSpaceCharge() {
  esc = 0.;
  double res = FindPeak(schist,esc);
  if (res > -500.) sc = res;
}
//_____________________________________________________________________________
double StSpaceChargeEbyEMaker::FindPeak(TH1* hist,float& pkwidth) {

  pkwidth = 0.;
  if (hist->Integral() < 100.0) return -998.;
  double mn = hist->GetMean();
  double rms = TMath::Abs(hist->GetRMS());
  mn *= 1.1; rms *= 1.5;
  double lr = mn-rms;
  double ur = mn+rms;
  double pmax = hist->GetMaximum();
  ga1.SetParameters(pmax,mn,rms*0.5);
  ga1.SetRange(lr,ur);
  int fitResult = hist->Fit(&ga1,"LLR0Q");
  if (fitResult != 0) return -999.;
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
		        EVN,0.,EVN,PHN,0,PI2,SCN,SCL,SCH);
    dcahist  = new TH3F("DcaEvt","psDCA vs. Phi vs. Event",
		        EVN,0.,EVN,PHN,0,PI2,DCN,DCL,DCH);
    dczhist  = new TH2F("DcaZ","psDCA vs. Z",
		        ZN,ZL,ZH,DCN,DCL,DCH);
    myhistN  = new TH3F("SpcEvtN","SpaceCharge vs. Phi vs. Event Neg",
			EVN,0.,EVN,PHN,0,PI2,SCN,SCL,SCH);
    myhistP  = new TH3F("SpcEvtP","SpaceCharge vs. Phi vs. Event Pos",
			EVN,0.,EVN,PHN,0,PI2,SCN,SCL,SCH);
    myhistE  = new TH3F("SpcEvtE","SpaceCharge vs. Phi vs. Event East",
			EVN,0.,EVN,PHN,0,PI2,SCN,SCL,SCH);
    myhistW  = new TH3F("SpcEvtW","SpaceCharge vs. Phi vs. Event West",
			EVN,0.,EVN,PHN,0,PI2,SCN,SCL,SCH);
    dcahistN = new TH3F("DcaEvtN","psDCA vs. Phi vs. Event Neg",
			EVN,0.,EVN,PHN,0,PI2,DCN,DCL,DCH);
    dcahistP = new TH3F("DcaEvtP","psDCA vs. Phi vs. Event Pos",
			EVN,0.,EVN,PHN,0,PI2,DCN,DCL,DCH);
    dcahistE = new TH3F("DcaEvtE","psDCA vs. Phi vs. Event East",
			EVN,0.,EVN,PHN,0,PI2,DCN,DCL,DCH);
    dcahistW = new TH3F("DcaEvtW","psDCA vs. Phi vs. Event West",
			EVN,0.,EVN,PHN,0,PI2,DCN,DCL,DCH);
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
  }

  if (doNtuple) ntup = new TNtuple("SC","Space Charge",
    "sc:dca:zdcx:zdcw:zdce:bbcx:bbcw:bbce:bbcbb:bbcyb:intb:inty:fill:mag:run:event:dcan:dcap:dcae:dcaw:gapf:gapd:gapfn:gapdn:gapfp:gapdp:gapfe:gapde:gapfw:gapdw:usc:uscmode");
    //"sc:dca:zdcx:zdcw:zdce:bbcx:bbcw:bbce:bbcbb:bbcyb:intb:inty:fill:mag:run:event:sce:scw:dcae:dcaw:gapf:gapd:gapfn:gapdn:gapfp:gapdp:gapfe:gapde:gapfw:gapdw:usc:uscmode");

  if (doGaps) {
    gapZhist = new TH2F("Gaps","Gaps",110,-0.3,0.8,17,-200.,225.);
    gapZhistneg = new TH2F("Gapsneg","Gaps Neg",110,-0.3,0.8,17,-200.,225.);
    gapZhistpos = new TH2F("Gapspos","Gaps Pos",110,-0.3,0.8,17,-200.,225.);
    gapZhisteast = new TH2F("Gapseast","Gaps East",110,-0.3,0.8,17,-200.,225.);
    gapZhistwest = new TH2F("Gapswest","Gaps West",110,-0.3,0.8,17,-200.,225.);
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
  }
  if (doGaps) {
    gapZhist->Write();
    gapZhistneg->Write();
    gapZhistpos->Write();
    gapZhisteast->Write();
    gapZhistwest->Write();
  }
  if (doNtuple) ntup->Write();
  ff.Close();

  gMessMgr->Info() << "StSpaceChargeEbyEMaker: QA hists file: "
                   << fname.Data() << endm;

  gSystem->cd("..");

}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::FillQAHists(float DCA, float space, int ch,
                                         StPhysicalHelixD& hh, int e_or_w) {
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
      myhistW->Fill(evtn,Phi,space);
      dcahistW->Fill(evtn,Phi,DCA);
    } else if (e_or_w < 0) {
      myhistE->Fill(evtn,Phi,space);
      dcahistE->Fill(evtn,Phi,DCA);
    }
    if ((e_or_w != 0) && (TMath::Abs(hh.dipAngle()) < 0.05)) dczhist->Fill(hh_at_pl.z(),DCA);
  }
}
//_____________________________________________________________________________
int StSpaceChargeEbyEMaker::imodHN(int i) {
  // Keep index in bounds of circular queue
  return ( i >= HN ? i-HN : (i < 0 ? i+HN : i) );
}
//_____________________________________________________________________________
float StSpaceChargeEbyEMaker::oldness(int i, int j) {
  // Deterime how to treat relative "age" of event
  // In PrePassmode, earliest events are most important!
  static float decay_const = -0.12;
  //static float decay_const = -0.15;
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
    s = exp( decay_const * time_factor );
  }
  return s;
}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::BuildHist(int i) {
  // Build up one histogram from several events
  schist->Reset();
  int isc = curhist;
  schist->Add(schists[isc],1.0);
  while (isc != i) {
    isc = imodHN(isc-1);
    schist->Add(schists[isc],oldness(isc));
  }
}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::SetTableName() {
  // Problem caused if first event comes later in time than other events.
  // Solution: subtract 10 seconds...
  TDatime firsttime(GetDateTime().Convert()-10);
  int date = firsttime.GetDate();
  int time = firsttime.GetTime();
  gMessMgr->Info() << "StSpaceChargeEbyEMaker: first event date = " << date << endm;
  gMessMgr->Info() << "StSpaceChargeEbyEMaker: first event time = " << time << endm;
  tabname = Form("./StarDb/Calibrations/rich/spaceChargeCorR2.%08d.%06d.C",date,time);
}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::WriteTableToFile(){
  gMessMgr->Info() << "StSpaceChargeEbyEMaker: Writing new table to:\n  "
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
  float fsign = ( event->runInfo()->magneticField() < 0 ? -1 : 1 );
  StPtrVecHit hts = tri->detectorInfo()->hits(kTpcId);
  float gap = 0.; float zgap = 0.; int ct=0;
  for (UInt_t ht=0; ht<hts.size(); ht++) {
    StTpcHit* hit = (StTpcHit*) hts[ht];
    UInt_t prow = hit->padrow();
    if ((prow != 12) && (prow != 14)) continue;
    float gsign = ( prow == 14 ? -1 : 1 );
    const StThreeVectorF& hp = hit->position();

    // Avoid sector edges
    float hphi = hp.phi() + TMath::TwoPi();
    while (hphi > TMath::Pi()/12.) hphi -= TMath::Pi()/6.;
    if (TMath::Abs(hphi) > 0.75*TMath::Pi()/12.) break;

    gap += fsign * gsign * hh.geometricSignedDistance(hp.x(),hp.y());
    zgap += (hp.z() / 12.795) * ( prow == 14 ? 7.4 : 5.395 ); // ~z at gap
    ct++;
  }

  if ((ct==2) && (TMath::Abs(zgap)<200.0) && (TMath::Abs(zgap)>10.0)) {
     gapZhist->Fill(gap,zgap);
     if (e_or_w==1) gapZhistwest->Fill(gap,zgap);
     else gapZhisteast->Fill(gap,zgap);
     if (ch==1) gapZhistpos->Fill(gap,zgap);
     else gapZhistneg->Fill(gap,zgap);
     float znew = 208.707 - TMath::Abs(zgap);
     if (znew > 50) {
       gapZhist->Fill(gap*100.0/znew,210.); // normalize at z=100cm from end
       if (e_or_w==1) gapZhistwest->Fill(gap*100.0/znew,210.);
       else gapZhisteast->Fill(gap*100.0/znew,210.);
       if (ch==1) gapZhistpos->Fill(gap*100.0/znew,210.);
       else gapZhistneg->Fill(gap*100.0/znew,210.);
     }
   }
}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::DetermineGaps() {
  DetermineGapHelper(gapZhistneg,gapZfitslopeneg,gapZdivslopeneg);
  DetermineGapHelper(gapZhistpos,gapZfitslopepos,gapZdivslopepos);
  DetermineGapHelper(gapZhisteast,gapZfitslopeeast,gapZdivslopeeast);
  DetermineGapHelper(gapZhistwest,gapZfitslopewest,gapZdivslopewest);
  DetermineGapHelper(gapZhist,gapZfitslope,gapZdivslope);
}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::DetermineGapHelper(TH2F* hh,
      float& divslope, float& fitslope) {

  ga1.SetParameters(hh->GetEntries()/(16.*2.*10.),0.,0.1);
  hh->FitSlicesX(&ga1,1,0,0,"LL0Q");
  const char* hn = hh->GetName();
  TH1D* GapsChi2 = (TH1D*) gDirectory->Get(Form("%s_chi2",hn));
  TH1D* GapsAmp  = (TH1D*) gDirectory->Get(Form("%s_0",hn));
  TH1D* GapsMean = (TH1D*) gDirectory->Get(Form("%s_1",hn));
  TH1D* GapsRMS  = (TH1D*) gDirectory->Get(Form("%s_2",hn));

  divslope = GapsMean->GetBinContent(17);
  egapZdivslope = TMath::Abs(GapsMean->GetBinError(17));

  GapsMean->Fit(&ln1,"R0Q");
  fitslope = ln1.GetParameter(1);
  egapZfitslope = TMath::Abs(ln1.GetParError(1));

  delete GapsChi2;
  delete GapsAmp;
  delete GapsMean;
  delete GapsRMS;
}
//_____________________________________________________________________________
// $Id: StSpaceChargeEbyEMaker.cxx,v 1.6 2005/04/21 19:38:20 genevb Exp $
// $Log: StSpaceChargeEbyEMaker.cxx,v $
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
