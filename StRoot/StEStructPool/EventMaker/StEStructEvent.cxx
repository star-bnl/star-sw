/**********************************************************************
 *
 * $Id: StEStructEvent.cxx,v 1.15 2013/02/08 19:32:52 prindle Exp $
 *
 * Author: Jeff Porter as rewrite of Ebye code by Jeff Reid
 *
 **********************************************************************
 *
 * Description:  Event quantities + list of (primary) tracks
 *               Depending on option may use global tracks
 *
 **********************************************************************/

#include "StEStructEvent.h"
#include "StEStructTrack.h"
#include "StEStructCentrality.h"
#include <math.h>
#include "PhysicalConstants.h"
#include "TVector2.h"
#include "TH1.h"
#include "TFile.h"

ClassImp(StEStructEvent)

//-------------------------------------------------------
StEStructEvent::StEStructEvent() {
  
  fTracks = new TClonesArray("StEStructTrack", 1200);  // 1200 is not the max size, just an initial suggestion for ROOT
  mNtrack = 0;

  mTrackCollectionM = new StEStructTrackCollection();
  mTrackCollectionP = new StEStructTrackCollection();

  mPsi = 0.;			// Initialize event plane angle
}

//-------------------------------------------------------
StEStructEvent::StEStructEvent(StEStructEvent& e){

  mRunID          = e.RunID();
  mEventTime      = e.EventTime();
  mVx             = e.Vx();
  mVy             = e.Vy();
  mVz             = e.Vz();
  mBField         = e.BField();
  mZDCe           = e.ZDCe();
  mZDCw           = e.ZDCw();
  mZDCCoincidence = e.ZDCCoincidence();

  StEStructTrack::BField = e.BField();
  mNtrack=0;
  fTracks = new TClonesArray("StEStructTrack", 1200);
  for(int i=0;i<e.Ntrack();i++){
    StEStructTrack* t=(StEStructTrack*)e.Tracks()->UncheckedAt(i);
    AddTrack(t);
  }

  mTrackCollectionM = new StEStructTrackCollection();
  mTrackCollectionP = new StEStructTrackCollection();
  FillChargeCollections();

  mPsi = 0.;			// Initialize event plane angle
};


//-------------------------------------------------------
StEStructEvent::~StEStructEvent(){

  Clear();
  delete fTracks;
  //  delete mTrackCollectionM;
  //  delete mTrackCollectionP;

};  

//-------------------------------------------------------
void StEStructEvent::AddTrack(StEStructTrack* inputTrack) {

  // Add a new track to the list of tracks for this StEStructEvent.
  // To avoid calling the very time consuming operator new for each track,
  // the standard but not well know C++ operator "new with placement"
  // is called. If tracks[i] is 0, a new Track object will be created
  // otherwise the previous Track[i] will be overwritten.
  //  [Note: new with placement is now the only way to fill a TClonesArray,
  //  see the ROOT class documentation for details] 

  TClonesArray &tracks = *fTracks;  // this is a reference:  tracks = *fTracks
  new(tracks[mNtrack++]) StEStructTrack(inputTrack);  
}


//-------------------------------------------------------
void StEStructEvent::Clear(Option_t *option) {

  mTrackCollectionP->Clear();
  mTrackCollectionM->Clear();
  fTracks->Clear(option);
  mNtrack=0;
}


//-------------------------------------------------------
void StEStructEvent::FillChargeCollections(){

  int num=Ntrack();
  if(num<=0) return;

  int id = mRunID;
  int time = mEventTime;
  double vx = mVx;
  double vy = mVy;
  double vz = mVz;
  double b = mBField;

  for(int i=0;i<num;i++){
    StEStructTrack* aTrack=(StEStructTrack*)Tracks()->UncheckedAt(i);
    if(!aTrack->isComplete()){
      aTrack->FillTransientData();
      aTrack->FillTpcReferencePoints();
      aTrack->SetComplete();
    }

    if(aTrack->Charge()==-1){
      TrackCollectionM()->push_back(aTrack);
    } else if(aTrack->Charge()==1){
      TrackCollectionP()->push_back(aTrack);
    } else {
      cout<<" Track Charge = "<<aTrack->Charge()<<endl;
    }
  }

}


//-------------------------------------------------------
StEStructTrackCollection * StEStructEvent::TrackCollectionM() const { return mTrackCollectionM;}; 
StEStructTrackCollection * StEStructEvent::TrackCollectionP() const { return mTrackCollectionP;};

//-------------------------------------------------------
TVector2 StEStructEvent::Q() {
  // Event plane vector

  TVector2 mQ;
  Float_t mQx=0., mQy=0.;

/*  if (mUseZDCSMD) { // pFlowSelect is disabled; only 1st order Q generated
    Float_t eXsum=0., eYsum=0., wXsum=0., wYsum=0.;
    Float_t eXWgt=0., eYWgt=0., wXWgt=0., wYWgt=0.;
    for (int v=1; v<8; v++) {
      eXsum += ZDCSMD_GetPosition(0,0,v)*ZDCSMD(0,0,v);
      wXsum += ZDCSMD_GetPosition(1,0,v)*ZDCSMD(1,0,v);
      eXWgt += ZDCSMD(0,0,v);
      wXWgt += ZDCSMD(1,0,v);
    } //v
    for (int h=1;h<9;h++) {
      eYsum += ZDCSMD_GetPosition(0,1,h)*ZDCSMD(0,1,h);
      wYsum += ZDCSMD_GetPosition(1,1,h)*ZDCSMD(1,1,h);
      eYWgt += ZDCSMD(0,1,h);
      wYWgt += ZDCSMD(1,1,h);
    }
    mQx= (eXWgt>0. && wXWgt>0. && eYWgt>0. && wYWgt>0.) ?
      eXsum/eXWgt - wXsum/wXWgt:0.;
    mQy= (eXWgt>0. && wXWgt>0. && eYWgt>0. && wYWgt>0.) ?
      eYsum/eYWgt - wYsum/wYWgt:0.;
  } //if
  else { */
    //int    selN  = pFlowSelect->Sel();
    //int    harN  = pFlowSelect->Har();
    int harN = 1;
    double order = (double)(harN + 1);

    StEStructTrackIterator itr;
    for (itr = TrackCollectionM()->begin();
         itr != TrackCollectionM()->end(); itr++) {
      StEStructTrack* pFlowTrack = *itr;
      //if (pFlowSelect->Select(pFlowTrack)) {
        //double phiWgt = PhiWeight(selN, harN, pFlowTrack);
        double phiWgt = 1.;
        Float_t phi = pFlowTrack->Phi();
        //Float_t phiWgt = mPhiWgt->GetBinContent(mPhiWgt->FindBin(phi));
        mQx += phiWgt * cos(phi * order);
        mQy += phiWgt * sin(phi * order);
      //}
    }
    for (itr = TrackCollectionP()->begin();
         itr != TrackCollectionP()->end(); itr++) {
      StEStructTrack* pFlowTrack = *itr;
      double phiWgt = 1.;
      Float_t phi = pFlowTrack->Phi();
      //Float_t phiWgt = mPhiWgt->GetBinContent(mPhiWgt->FindBin(phi));
      mQx += phiWgt * cos(phi * order);
      mQy += phiWgt * sin(phi * order);
    }
//    itr.~StEStructTrackIterator(); djp
//  } //else

  mQ.Set(mQx, mQy);
  return mQ;
}

//-------------------------------------------------------
void StEStructEvent::CalculatePsi() {
  // Event plane angle

  //int    harN = pFlowSelect->Har();
  int harN = 1;
  float order = (float)(harN + 1);
  Float_t psi = 0.;

  TVector2 mQ = Q();

  if (mQ.Mod()) {
    psi= mQ.Phi() / order;
    if (psi < 0.) { psi += twopi / order; }
  }

//  return psi;
  mPsi = psi;
//  mQ.~TVector2();   djp
}

//-------------------------------------------------------
Float_t StEStructEvent::Psi() {
	return mPsi;
}

//-------------------------------------------------------
void StEStructEvent::ShiftPhi() {
    // First, need to calculate the event plane angle
    CalculatePsi();

    StEStructTrackIterator itr;
    for (itr = TrackCollectionM()->begin();
         itr != TrackCollectionM()->end(); itr++) {
      StEStructTrack* pTrack = *itr;
      Float_t phi = pTrack->Phi();
      phi -= mPsi;
      if(phi<-pi) {phi += twopi;}
      pTrack->SetPhi(phi);
    }
    for (itr = TrackCollectionP()->begin();
         itr != TrackCollectionP()->end(); itr++) {
      StEStructTrack* pTrack = *itr;
      Float_t phi = pTrack->Phi();
      phi -= mPsi;
      if(phi<-pi) {phi += twopi;}
      pTrack->SetPhi(phi);
    }
//    itr.~StEStructTrackIterator(); djp
}

//-------------------------------------------------------
void StEStructEvent::SetPhiWgt(const char* weightFile) {
  TFile* tf=new TFile(weightFile);
  TString hname("PhiWgt");
  mPhiWgt = (TH1F *) tf->Get(hname.Data());

  // delete phiWgt;
//  hname.~TString(); djp
//  tf->~TFile(); djp
  delete tf;
}

/**********************************************************************
 *
 * $Log: StEStructEvent.cxx,v $
 * Revision 1.15  2013/02/08 19:32:52  prindle
 * Added "Triggered" histograms in StEStruct2ptCorrelations.
 * Protected against using tracks cuts in StEStruct2ptCorrelations when reading EStruct format events.
 * Added comment in EventMaker/StEStructTrack.cxx pointing out need to set BField correctly
 * when reading EStruct format events. (This should be read from file somehow, but...)
 *
 * Revision 1.14  2012/11/16 21:24:37  prindle
 * Changes to support reading/writing of EStructEvent. Fill helix as transient and
 * get BField from file (?).
 *
 * Revision 1.13  2011/08/02 20:36:57  prindle
 *   Event: modifications for ZDCCoincidence
 *   Track: big changes in evalPID. These should be superseded when TOF-dEdx
 *          space is understood better.
 *
 * Revision 1.12  2008/05/01 23:41:44  prindle
 *   Just different comments.
 *
 * Revision 1.11  2007/02/05 17:20:09  msd
 * Added include statement
 *
 * Revision 1.10  2006/10/02 22:24:02  prindle
 * Removed a few destructors that I think caused crashes.
 *
 * Revision 1.9  2006/04/26 18:49:55  dkettler
 *
 * Added reaction plane determination for the analysis
 *
 * Added reaction plane angle calculation
 *
 * Revision 1.8  2006/04/06 01:06:18  prindle
 *
 *   Rationalization of centrality binning, as described in AnalysisMaker checkin.
 *
 * Revision 1.7  2006/04/04 22:12:30  porter
 * Set up StEtructCentrality for use in event cut selection - includes impact para for generators
 *
 * Revision 1.6  2006/02/22 22:06:05  prindle
 * Removed all references to multRef (?)
 *
 * Revision 1.5  2005/09/14 17:21:14  msd
 * Simplified helix fitting by taking helix from mudst instead of calculating from scratch
 *
 * Revision 1.4  2004/06/25 03:13:41  porter
 * updated centrality methods and put a test in StEStructEvent.cxx
 *
 * Revision 1.3  2004/06/09 22:39:09  prindle
 * Expanded centrality class.
 * Call to set centrality from event reader.
 *
 *
 * CVS :nded ----------------------------------------------------------------------
 *
 * Revision 1.2  2004/02/27 02:28:04  prindle
 *
 * Small modification to StEStructCentrality in EventMaker branch.
 * Many modifications to Fluctuations branch, although that branch is not
 * stable yet.
 *
 * Revision 1.1  2003/10/15 18:20:51  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
