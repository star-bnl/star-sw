/**********************************************************************
 *
 * $Id: StEStructEvent.h,v 1.11 2012/11/16 21:24:37 prindle Exp $
 *
 * Author: Jeff Porter as rewrite of Ebye code by Jeff Reid
 *
 **********************************************************************
 *
 * Description:  Event quantities + list of (primary) tracks
 *               Depending on option may use global tracks
 *
 **********************************************************************/

#ifndef _StEStructEvent
#define _StEStructEvent

#include "Stiostream.h"
#include "TObject.h"
#include "TClonesArray.h"
#include "StEStructTrackCollection.h"
#include "TVector2.h"

class TH1F;

class StEStructEvent : public TObject {

 private:

  Int_t mNtrack;                     // track number

  Int_t mEventID;                    // event ID
  Int_t mRunID;                      // run ID
  Int_t mEventTime;                  // event (unix) timestamp
  Float_t mVx;                       // primary vertex position
  Float_t mVy;
  Float_t mVz;
  Float_t mBField;                   // magnetic field (kilogauss as MuDst)

  //  Int_t mCTBarray[32];               // coarse CTB array

  Float_t mZDCe;                     // ZDC East
  Float_t mZDCw;                     // ZDC West
  Float_t mZDCCoincidence;           // ZDC Coincidence rate

  unsigned short mRefMult;            // not used for determining centrality, stored for comparison
  double mctbMult;                    
  int mNumPrim;                       // from StEventSummary::numberOfGoodPrimaryTracks()

  TClonesArray *fTracks; //->

  // non-persistent data to merge old event and 2ptevent classes

  Double_t mCentrality;               // centrality measure (depends on analysis)
  StEStructTrackCollection *mTrackCollectionM; //! negative charge list
  StEStructTrackCollection *mTrackCollectionP; //! positive charge list

  Float_t mPsi;				//! Event plane angle
  TH1F* mPhiWgt;			//! Phi weights
     
 public:
  StEStructEvent();
  StEStructEvent(StEStructEvent& e);
  virtual ~StEStructEvent();


  void Clear(Option_t *option ="");

  Int_t EventID() const { return mEventID; }; 
  Int_t RunID() const { return mRunID; };
  Int_t EventTime() const { return mEventTime; }

  Double_t Centrality() const { return mCentrality; };

  Float_t Vx() const { return mVx; }
  Float_t Vy() const { return mVy; }
  Float_t Vz() const { return mVz; }
  Float_t VertexZ() const { return mVz; } 
  Float_t BField() const { return mBField; };

  Float_t ZDCe() const { return mZDCe; }
  Float_t ZDCw() const { return mZDCw; }
  Float_t ZDCCoincidence() const { return mZDCCoincidence; }
  
  unsigned short RefMult() const { return mRefMult; }
  double ctbMult() const { return mctbMult; }
  int NumPrim() const { return mNumPrim; }

  // Reaction-plane related functions
  TVector2 Q();			// Calculates Q and returns it
  void CalculatePsi();		// Calculates Psi and stores it in mPsi
  Float_t Psi();		// Returns mPsi
  void ShiftPhi();		// Loops over tracks, modifies Phi values by Psi
  void SetPhiWgt(const char* weightFile);	// Sets the phi weights
  
  void AddTrack(StEStructTrack* pEStructTrack);

  void SetEventID(const Int_t id) { mEventID = id; }
  void SetRunID(const Int_t id) { mRunID = id; }

  void SetVx(const Float_t vx) { mVx = vx; }
  void SetVy(const Float_t vy) { mVy = vy; }
  void SetVz(const Float_t vz) { mVz = vz; }

  void SetVertex(const Float_t vx, const Float_t vy, const Float_t vz) {
    mVx = vx; mVy = vy; mVz = vz; }
  void SetBField(const float bfield){ mBField=bfield; };

  void SetZDCe(const Float_t zdce) { mZDCe = zdce; }
  void SetZDCw(const Float_t zdcw) { mZDCw = zdcw; }
  void SetZDCCoincidence(const Float_t zdccoincidence) { mZDCCoincidence = zdccoincidence; }

  void SetRefMult(const unsigned short mult) { mRefMult = mult; }
  void SetctbMult(const double mult) { mctbMult = mult; }
  void SetNumPrim(const int mult) { mNumPrim = mult; }

  Int_t Ntrack() { return mNtrack; }
  Int_t Npos() { return (Int_t)mTrackCollectionP->size(); }
  Int_t Nneg() { return (Int_t)mTrackCollectionM->size(); }

  TClonesArray *Tracks() { return fTracks; }

  virtual StEStructTrackCollection * TrackCollectionM() const; 
  virtual StEStructTrackCollection * TrackCollectionP() const;
  void SetCentrality(const Double_t N) { mCentrality = N; }

  virtual void FillChargeCollections();

  ClassDef(StEStructEvent,1)
};



#endif


/**********************************************************************
 *
 * $Log: StEStructEvent.h,v $
 * Revision 1.11  2012/11/16 21:24:37  prindle
 * Changes to support reading/writing of EStructEvent. Fill helix as transient and
 * get BField from file (?).
 *
 * Revision 1.10  2011/08/02 20:36:57  prindle
 *   Event: modifications for ZDCCoincidence
 *   Track: big changes in evalPID. These should be superseded when TOF-dEdx
 *          space is understood better.
 *
 * Revision 1.9  2008/05/01 23:41:45  prindle
 *   Just different comments.
 *
 * Revision 1.8  2007/05/27 22:45:18  msd
 * Added Npos() and Nneg().
 *
 * Revision 1.7  2006/04/26 18:49:56  dkettler
 *
 * Added reaction plane determination for the analysis
 *
 * Added reaction plane angle calculation
 *
 * Revision 1.6  2006/04/06 01:06:20  prindle
 *
 *   Rationalization of centrality binning, as described in AnalysisMaker checkin.
 *
 * Revision 1.5  2006/04/04 22:12:30  porter
 * Set up StEtructCentrality for use in event cut selection - includes impact para for generators
 *
 * Revision 1.4  2006/02/22 22:06:06  prindle
 * Removed all references to multRef (?)
 *
 * Revision 1.3  2004/06/09 22:39:10  prindle
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
