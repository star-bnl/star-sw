//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEbyET0 redoes the space charge correction                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StEvent/StEvent.h"
#include "StEvent/StTpcHit.h"
#include "StEvent/StTpcHitCollection.h"
#include "StEvent/StTriggerData.h"
#include "StDetectorDbMaker/St_EbyET0C.h"
/*
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
#include "StDetectorDbMaker/St_tpcTimeBucketCorC.h"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StTpcDb/StTpcDb.h"
*/
#include "TFile.h"
#include "TNtupleD.h"
#include "StEbyET0.h"

StEbyET0* StEbyET0::mInstance = 0;

ClassImp(StEbyET0)
  
//_____________________________________________________________________________
StEbyET0::StEbyET0() : mRunId(0), mEventId(0), mTime(0.), mTree(0) {}
//_____________________________________________________________________________
StEbyET0::~StEbyET0() {
  finishCalib();
  mInstance=0;
}
//_____________________________________________________________________________
void StEbyET0::finishCalib() {
  if (!mTree) return;
  mTree->Write();
  delete (mTree->GetDirectory()->GetFile());
}
//_____________________________________________________________________________
double StEbyET0::getTime(StEvent* event, int mode) {

  // check for same event
  if (event->runId() == mRunId && event->id() == mEventId) return mTime;
  
  mRunId = event->runId();
  mEventId = event->id();

  // check for calibration mode
  if (mode == 1) fillTree(event);

  // determine event time
  mTime = 0.; // default should always be zero, not correcting for some other global T0
  double info[12];
  St_EbyET0C* ebyeTable = St_EbyET0C::instance();
  for (int row = 0; row < ebyeTable->GetNRows(); row++) {
    int detector = ebyeTable->detector(row);
    if (detector < 0) break; // end of active rows in the table

    double coordinate = -9e23;

    // determine which time coordinate to use
    switch (detector) {
      case  0 : // first 4 use average of trigger detectors
      case  1 : 
      case  2 : 
      case  3 : getTriggerInfo(event,static_cast<trigDetType> (detector),info);
                if (info[0] > 0 && info[1] > 0)
                  coordinate = 0.5*(info[0]+info[1]);
                break;
      case  5 : // next 4 use east trigger detectors
      case  6 :
      case  7 :
      case  8 : getTriggerInfo(event,static_cast<trigDetType> (detector-4),info);
                if (info[0] > 0)
                  coordinate = info[0];
                break;
      case  9 : // next 4 use west trigger detectors
      case 10 :
      case 11 :
      case 12 : getTriggerInfo(event,static_cast<trigDetType> (detector-8),info);
                if (info[1] > 0)
                  coordinate = info[1];
                break;
    }

    // determine if coordinate is in acceptable range
    if (coordinate < -8e23 ||
        (ebyeTable->min(row) < ebyeTable->max(row) &&
         (ebyeTable->min(row) > coordinate ||
          ebyeTable->max(row) < coordinate))) continue;

    // use only this coordinate to determine a time correction
    mTime = ebyeTable->time(row,coordinate);
    break;
  }

  return mTime;
}
//_____________________________________________________________________________
void StEbyET0::fillTree(StEvent* event) {

  if (!mTree) {
    new TFile(Form("EbyET0.%d.%d.root",mRunId,mEventId),"RECREATE");
    mTree = new TNtupleD("ebyeT0tree","Event by Event T0 tree",
                        "run:event:time"
                        ":vpde:vpdw:epde:epdw:bbce:bbcw:zdce:zdcw"
                        ":tpctie:tpcnie:tpcrie"
                        ":tpctiw:tpcniw:tpcriw"
                        ":tpctoe:tpcnoe:tpcroe"
                        ":tpctow:tpcnow:tpcrow");
  }

  // reduce AutoSave frequency for larger trees
  int treeEntries = mTree->GetEntriesFast();
  switch (treeEntries) {
    case    0 : mTree->SetAutoSave(  5); break;
    case   25 : mTree->SetAutoSave( 20); break;
    case  250 : mTree->SetAutoSave(100); break;
    case 1250 : mTree->SetAutoSave(250); break;
  }

  double info[23];
  info[0] = (double) mRunId;
  info[1] = (double) mEventId;
  info[2] = (double) (event->time());
  getTriggerInfo(event,kVPD,&(info[3]));
  getTriggerInfo(event,kEPD,&(info[5]));
  getTriggerInfo(event,kBBC,&(info[7]));
  getTriggerInfo(event,kZDC,&(info[9]));
  getTpcInfo(event,&(info[11]));
  mTree->Fill(info);

  return;
}
//_____________________________________________________________________________
void StEbyET0::getTriggerInfo(StEvent* event, trigDetType trigDet, double* info) {

  StTriggerData* trigdata = event->triggerData();
  switch (trigDet) {
    case kVPD : info[0] = (double) (trigdata->vpdEarliestTDC(StBeamDirection::east,0));
                info[1] = (double) (trigdata->vpdEarliestTDC(StBeamDirection::west,0));
                break;
    case kEPD : info[0] = (double) (trigdata->epdEarliestTDC(StBeamDirection::east,0));
                info[1] = (double) (trigdata->epdEarliestTDC(StBeamDirection::west,0));
                break;
    case kBBC : info[0] = (double) (trigdata->bbcEarliestTDC(StBeamDirection::east,0));
                info[1] = (double) (trigdata->bbcEarliestTDC(StBeamDirection::west,0));
                break;
    case kZDC : info[0] = (double) (trigdata->zdcEarliestTDC(StBeamDirection::east,0));
                info[1] = (double) (trigdata->zdcEarliestTDC(StBeamDirection::west,0));
                break;
    default   : info[0] = -999.;
                info[1] = -999.;
  }

  return;
}
//_____________________________________________________________________________
void StEbyET0::getTpcInfo(StEvent* event, double* info) {

  // counters
  double tie = 0.; // hit times sum
  double tiw = 0.;
  double toe = 0.;
  double tow = 0.;
  double nie = 0.; // hit counts
  double niw = 0.;
  double noe = 0.;
  double now = 0.;
  double rie = 0.; // hit rows sum
  double riw = 0.;
  double roe = 0.;
  double row = 0.;

/*
  // parameters: prompt hit windows
  static const double innerMinTB = 7.7;
  static const double innerMaxTB = 10.5;
  static const double outerMinTB = 2.0;
  static const double outerMaxTB = 5.0;

  // coordinate transform to get time of hit
  static StTpcCoordinateTransform* mTpcTransForm = 0;
  if (! mTpcTransForm) mTpcTransForm = new StTpcCoordinateTransform(gStTpcDb);
  StTpcCoordinateTransform &transform = *mTpcTransForm;

  StTpcHitCollection* tpcHits = event->tpcHitCollection();

  if (tpcHits) {
    for (unsigned int i=0; i<tpcHits->numberOfSectors(); i++) {
      StTpcSectorHitCollection* tpcHitsSector = tpcHits->sector(i);
      for (unsigned int j=0; j<tpcHitsSector->numberOfPadrows(); j++) {
        StSPtrVecTpcHit& tpcHitsVec = tpcHitsSector->padrow(j)->hits();
        for (unsigned int k=0; k<tpcHitsVec.size(); k++) {
          StTpcHit* hit = tpcHitsVec[k];
          if (!hit || hit->flag() != 0) continue;
          unsigned int sector = hit->sector();
          unsigned int padrow = hit->padrow();
          int pad = TMath::Nint(hit->pad());
          bool innerRow = St_tpcPadConfigC::instance()->IsRowInner(sector,padrow);
          double tb = hit->timeBucket();

          // copied from StTpcHitMoverMaker
          if (St_tpcTimeBucketCorC::instance()->getNumRows()) {
            Int_t io = (innerRow ? 1 : 0);
            Double_t noTmbks = hit->maxTmbk() - hit->minTmbk() + 1;
            tb += St_tpcTimeBucketCorC::instance()->CalcCorrection(io, noTmbks);// units are tb
          }
          StTpcPadCoordinate padcoord(sector, padrow, pad, tb);
          StTpcLocalSectorCoordinate  coorS;
          transform(padcoord,coorS,kFALSE);
          // transform converts tb => time => z, so we have to step back to time;
          // ignoring adding a zoffset = StTpcDb::instance->Dimenstions()->zInner/OuterOffset()
          // because constant shifts aren't relavant for the EbyET0 calibration
          double time = coorS.position().z() / (StTpcDb::instance()->DriftVelocity()*1e-6);

          if (innerRow) {
            if (tb < innerMaxTB && tb > innerMinTB) { // use only prompt hit candidates
              if (sector <= 12) {
                niw += 1.0; tiw += time; riw += padrow;
              } else {
                nie += 1.0; tie += time; rie += padrow;
              }
            }
          } else {
            // exclude some known problematic rows
            if ((sector == 5 || sector == 19) &&
              abs((float) (St_tpcPadConfigC::instance()->padRows(sector) - padrow) - 7.5) < 8.0)
              continue;
            if (tb < outerMaxTB && tb > outerMinTB) { // use only prompt hit candidates
              if (sector <= 12) {
                now += 1.0; tow += time; row += padrow;
              } else {
                noe += 1.0; toe += time; roe += padrow;
              }
            }
          } // innerRow
        } // hits-in-padrow for-loop
      } // padrow for-loop
    } // sector for-loop
  } // tpcHits
*/

  // pass back the average time, number of hits, and average padrow
  info[ 0] = (nie > 0 ? tie/nie : 0); info[ 1] = nie; info[ 2] = (nie > 0 ? rie/nie : 0);
  info[ 3] = (niw > 0 ? tiw/niw : 0); info[ 4] = niw; info[ 5] = (niw > 0 ? riw/niw : 0);
  info[ 6] = (noe > 0 ? toe/noe : 0); info[ 7] = noe; info[ 8] = (noe > 0 ? roe/noe : 0);
  info[ 9] = (now > 0 ? tow/now : 0); info[10] = now; info[11] = (now > 0 ? row/now : 0);

  return;
}
//_____________________________________________________________________________
// $Id: StEbyET0.cxx,v 1.2 2021/03/20 02:38:13 genevb Exp $
// $Log: StEbyET0.cxx,v $
// Revision 1.2  2021/03/20 02:38:13  genevb
// Remove StTpcDb dependence for StEventUtilities
//
// Revision 1.1  2021/03/19 01:44:47  genevb
// Introduce Event-by-Event T0 corrections
//
//
