//////////////////////////////////////////////////////////////////////////
// 
// $Id: StFlowNanoEvent.h,v 1.8 2000/08/09 21:38:23 snelling Exp $
//
// Author: Sergei Voloshin and Raimond Snellings, March 2000
//
// Description:  A persistent Flow nano DST
// 
//////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowNanoEvent.h,v $
// Revision 1.8  2000/08/09 21:38:23  snelling
// PID added
//
// Revision 1.7  2000/05/26 21:29:30  posk
// Protected Track data members from overflow.
//
// Revision 1.6  2000/05/20 00:55:18  posk
// Condensed flownanoevent.root somewhat.
//
// Revision 1.5  2000/05/16 20:59:34  posk
// Voloshin's flownanoevent.root added.
//
// Revision 1.2  2000/03/08 15:10:50  posk
//
//
//////////////////////////////////////////////////////////////////////////

#ifndef StFlowNanoEvent__h
#define StFlowNanoEvent__h
#include <iostream.h>
#include "TObject.h"
#include "TClonesArray.h"
#include "StFlowTrack.h"
#include "StThreeVectorF.hh"
#include "TVector2.h"
#include "SystemOfUnits.h"

class StFlowNanoEvent : public TObject {
  
 public:

                 StFlowNanoEvent();
  virtual        ~StFlowNanoEvent() { Clear(); }

  void           Clear(Option_t *option ="");
  Int_t          EventID() const; 
  UInt_t         OrigMult() const;
  UInt_t         Centrality() const;
  StThreeVectorF VertexPos() const;
  UInt_t         MultEta1() const;
  UInt_t         MultEta2() const;
  Float_t        CTB() const;
  Float_t        ZDCe() const;
  Float_t        ZDCw() const;
  
  void AddTrack(StFlowTrack* pFlowTrack);
  void SetEventID(const Int_t&); 
  void SetOrigMult(const UInt_t&);
  void SetCentrality(const UInt_t&);
  void SetVertexPos(const StThreeVectorF&);
  void SetMultEta1(const UInt_t&);
  void SetMultEta2(const UInt_t&);
  void SetCTB(const Float_t ctb);
  void SetZDCe(const Float_t zdce);
  void SetZDCw(const Float_t zdcw);
  
  Int_t         GetNtrack() const { return mNtrack; }
  TClonesArray *GetTracks() const { return fTracks; }
  
 private:

  Int_t           mNtrack;                     // track number
  Int_t           mEventID;                    // event ID
  UInt_t          mOrigMult;                   // number of StEvent tracks
  UInt_t          mCentrality;                 // centrality bin
  StThreeVectorF  mVertexPos;                  // primary vertex position
  UInt_t          mMultEta1; // number of tracks with positive flag in 1 unit of eta
  UInt_t          mMultEta2; // number of tracks with positive flag in 2 units of eta
  Float_t         mCTB;                                 // CTB value sum
  Float_t         mZDCe;                                // ZDC east
  Float_t         mZDCw;                                // ZDC west
  
  TClonesArray        *fTracks;
  static TClonesArray *fgTracks;
  
  ClassDef(StFlowNanoEvent,1)
};

inline Int_t StFlowNanoEvent::EventID() const { return mEventID; }

inline UInt_t StFlowNanoEvent::OrigMult() const { return mOrigMult; }

inline UInt_t StFlowNanoEvent::Centrality() const { return mCentrality; }

inline StThreeVectorF StFlowNanoEvent::VertexPos() const { return mVertexPos; }

inline void StFlowNanoEvent::SetEventID(const Int_t& id) {
  mEventID = id; }

inline void StFlowNanoEvent::SetOrigMult(const UInt_t& tracks) {
  mOrigMult = tracks; }

inline void StFlowNanoEvent::SetCentrality(const UInt_t& cent) {
  mCentrality = cent; }

inline void StFlowNanoEvent::SetVertexPos(const StThreeVectorF& vertexPos) {
  mVertexPos = vertexPos; }

inline void StFlowNanoEvent::SetMultEta1(const UInt_t& goodtracks1) {
  mMultEta1 = goodtracks1; }

inline void StFlowNanoEvent::SetMultEta2(const UInt_t& goodtracks2) {
  mMultEta2 = goodtracks2; }

inline void  StFlowNanoEvent::SetCTB(const Float_t ctb)  {mCTB = ctb; }

inline void  StFlowNanoEvent::SetZDCe(const Float_t zdce)  {mZDCe = zdce; }

inline void  StFlowNanoEvent::SetZDCw(const Float_t zdcw)  {mZDCw = zdcw; }

inline UInt_t StFlowNanoEvent::MultEta1() const { return mMultEta1; }

inline UInt_t StFlowNanoEvent::MultEta2() const { return mMultEta2; }

inline Float_t  StFlowNanoEvent::CTB()         const { return mCTB; }

inline Float_t  StFlowNanoEvent::ZDCe()        const { return mZDCe; }

inline Float_t  StFlowNanoEvent::ZDCw()        const { return mZDCw; }

#endif



