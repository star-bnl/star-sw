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

#include "TH3.h"
#include "TFile.h"
#include "TF1.h"

// Histogram ranges:
const int   SCN = 80;
const float SCL = -0.015;
const float SCH =  0.025;

const int   DCN = 75;
const float DCL = -1.5;
const float DCH =  1.5;

const int   PHN = 72;
const float PI2 = TMath::TwoPi();

const int   EVN = 500;


ClassImp(StSpaceChargeEbyEMaker)
  
//_____________________________________________________________________________
StSpaceChargeEbyEMaker::StSpaceChargeEbyEMaker(const char *name):StMaker(name),
    PrePassmode(kFALSE), PrePassdone(kFALSE), QAmode(kFALSE),
    m_ExB(0), tpcDbMaker(0), tpcHitMoverMaker(0) {

  HN=32;  // max events used, cannot exceed 32 used in header file
  MINTRACKS=1500;
  SCALER_ERROR = 0.0004; // by eye from hist: SCvsZDCEpW.gif

  // MAXDIFFE is maximum different in sc from last ebye sc
  MAXDIFFE =   SCALER_ERROR;
  // MAXDIFFA is maximum different in sc from last scaler sc
  MAXDIFFA = 2*SCALER_ERROR; // should be about equal to SCALER_ERROR, no?
           // Present uncetainties with scalers demands greater tolerance

  runid = 0;
  memset(evts,0,HN*sizeof(int));

  SetMode(0); // default is mode 0 (no QA, no PrePass)
  //QAmode=kTRUE; // For testing

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
    if (QAmode) WriteQAHists();
  } else {
    gMessMgr->Warning("StSpaceChargeEbyEMaker: NO EVENTS => NO OUTPUT");
  }

  return kStOk;
}
//_____________________________________________________________________________
Int_t StSpaceChargeEbyEMaker::Init() {

  switch (GetMode()) {
    case (3) : PrePassmode = kTRUE; QAmode = kTRUE;  break;
    case (2) : PrePassmode = kTRUE; break;
    case (1) : QAmode = kTRUE; break;
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
  } else {
    TDataSet *RunLog = GetDataBase("RunLog");
    if (!RunLog) gMessMgr->Warning("StSpaceChargeEbyEMaker: No RunLog found.");
    m_ExB = new StMagUtilities(
      ((StTpcDbMaker*) tpcDbMaker)->tpcDbInterface(),RunLog,0);
  }

  // Get StEvent and related info, determine if things are OK
  StEvent* event = (StEvent*) GetInputDS("StEvent");
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
  curhist = imodHN(curhist+1);
  schists[curhist]->Reset();
  times[curhist] = thistime;
  evts[curhist]=evt;
  lastsc = m_ExB->CurrentSpaceChargeR2();
  if (QAmode)  gMessMgr->Info()
    << "StSpaceChargeEbyEMaker: used (for this event) SpaceCharge = "
    << lastsc << " (" << thistime << ")" << endm;


  // Track loop
  unsigned int i,j;
  StThreeVectorD ooo = pvtx->position();

  printf("GGGGGGGGGGG Makin3 %f\n",(float) (runinfo->zdcWestRate()+runinfo->zdcEastRate()));

  for (i=0; i<nnodes; i++) {
      for (j=0; j<theNodes[i]->entries(global); j++) {
        StTrack* tri = theNodes[i]->track(global,j);
        if (!tri) continue;

          const StTrackTopologyMap& map = tri->topologyMap();
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
          Float_t DCA=hh.geometricSignedDistance(ooo.x(),ooo.y());
          Int_t ch = (int) triGeom->charge();
          Float_t space = 10000.;
          if (!(m_ExB->PredictSpaceChargeDistortion(ch,oldPt,ooo.z(),
	     eta,DCA,map.data(0),map.data(1),space))) continue;

	  space += lastsc;
	  schists[curhist]->Fill(space);
          FillQAHists(DCA,space,ch,hh,e_or_w);

      } // loop over j tracks
  } // loop over i Nodes

  
  ntrks[curhist] = schists[curhist]->Integral();
  ntrkssum += ntrks[curhist];
  
  // Wrap it up and make a decision
  int result = DecideSpaceCharge(thistime);

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
  if (QAout) gMessMgr->Info() <<
    "StSpaceChargeEbyEMaker: time since last event = " <<
    time-lasttime << endm;
  if ((lasttime==0) || (time-lasttime < 30)) {
  
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

  if (do_auto) m_ExB->AutoSpaceChargeR2();
  else {
    gMessMgr->Info() << "StSpaceChargeEbyEMaker: using SpaceCharge = "
      << sc << " +/- " << esc << " (" << time << ")" << endm;
    scehist->SetBinContent(evt,sc);
    scehist->SetBinError(evt,esc);
    if (PrePassmode) {
      PrePassdone = kTRUE;
      return kStFatal; // We're happy! Let's stop!
    }
    m_ExB->ManualSpaceChargeR2(sc);
  }
  return kStOk;
}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::FindSpaceCharge() {

  static TF1 ga1("ga1","[0]*exp(-0.5*(x-[1])*(x-[1])/([2]*[2]))");
  esc = 0.;
  if (schist->GetEntries() < 100) return;
  double mn = schist->GetMean();
  double rms = TMath::Abs(schist->GetRMS());
  mn *= 1.1; rms *= 1.5;
  double lr = mn-rms;
  double ur = mn+rms;
  double pmax = schist->GetMaximum();
  ga1.SetParameters(pmax,mn,rms*0.5);
  ga1.SetRange(lr,ur);
  int fitResult = schist->Fit(&ga1,"LLR0Q");
  if (fitResult != 0) return;
  sc = ga1.GetParameter(1);
  esc = TMath::Abs(ga1.GetParError(1));

}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::InitQAHists() {

  scehist  = new TH1F("SpcChgEvt","SpaceCharge fit vs. Event",
		      EVN,0.,EVN);
  timehist = new TH1F("EvtTime","Event Times",
		      EVN,0.,EVN);
  myhist   = new TH3F("SpcEvt","SpaceCharge vs. Phi vs. Event",
		      EVN,0.,EVN,PHN,0,PI2,SCN,SCL,SCH);
  dcahist  = new TH3F("DcaEvt","psDCA vs. Phi vs. Event",
		      EVN,0.,EVN,PHN,0,PI2,DCN,DCL,DCH);
  AddHist(scehist);
  AddHist(timehist);
  AddHist(myhist);
  AddHist(dcahist);

  if (QAmode) {
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
    AddHist(myhistN);
    AddHist(myhistP);
    AddHist(myhistE);
    AddHist(myhistW);
    AddHist(dcahistN);
    AddHist(dcahistP);
    AddHist(dcahistE);
    AddHist(dcahistW);
  }

}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::WriteQAHists() {
// Only if QAmode

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
  myhist->Write();
  dcahist->Write();
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
  float Phi = hh.at(pl).phi();
  while (Phi < 0) Phi += PI2;
  while (Phi >= TMath::TwoPi()) Phi -= PI2;
  
  // To pile all sectors atop each other:
  // while (Phi >= TMath::Pi()/6.) Phi -= TMath::Pi()/6.;

  float evtn = (float) evt - 1;
  myhist->Fill(evtn,Phi,space);
  dcahist->Fill(evtn,Phi,DCA);
  if (QAmode) {
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
  float s = 1.0;
  if (!PrePassmode) { // Weight newest the most (or evenly for PrePass)
    if (j<0) j = curhist;
    s = exp(-0.15*(times[j]-times[i]));
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
  int date = GetDate();
  int time = GetTime();
  gMessMgr->Info() << "StSpaceChargeEbyEMaker: first event date = " << date << endm;
  gMessMgr->Info() << "StSpaceChargeEbyEMaker: first event time = " << time << endm;
  tabname = Form("./StarDb/Calibrations/rich/spaceChargeCorR2.%08d.%06d.C",date,time);
}
//_____________________________________________________________________________
void StSpaceChargeEbyEMaker::WriteTableToFile(){
  gMessMgr->Info() << "StSpaceChargeEbyEMaker: Writing new table to:\n  "
    << tabname.Data() << endm;
  TString dirname = gSystem->DirName(tabname.Data());
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
  float sc = 6e-8 *zdcsum;
  
  return sc;
}
//_____________________________________________________________________________
// $Id: StSpaceChargeEbyEMaker.cxx,v 1.2 2004/07/01 01:46:04 genevb Exp $
// $Log: StSpaceChargeEbyEMaker.cxx,v $
// Revision 1.2  2004/07/01 01:46:04  genevb
// Slightly larger margin for full vs half field differences
//
// Revision 1.1  2004/06/30 23:16:00  genevb
// Introduction of StSpaceChargeEbyEMaker
//
//
