//////////////////////////////////////////////////////////////////////////
// 
// $Id: StFlowNanoEvent.h,v 1.5 2000/05/16 20:59:34 posk Exp $
//
// Author: Sergei Voloshin and Raimond Snellings, March 2000
//
// Description:  A persistent Flow nano DST
// 
//////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowNanoEvent.h,v $
// Revision 1.5  2000/05/16 20:59:34  posk
// Voloshin's flownanoevent.root added.
//
// Revision 1.2  2000/03/08 15:10:50  posk
// Added $Id: StFlowNanoEvent.h,v 1.5 2000/05/16 20:59:34 posk Exp $ and $Log: StFlowNanoEvent.h,v $
// Added $Id$ and Revision 1.5  2000/05/16 20:59:34  posk
// Added $Id$ and Voloshin's flownanoevent.root added.
// Added $Id$ and.
//
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
  UInt_t         EventNumber() const;
  UInt_t         OrigMult() const;
  UInt_t         Centrality() const;
  StThreeVectorF VertexPos() const;
  Long_t         EventID() const; 
  
  void AddTrack(StFlowTrack* pFlowTrack);
  void SetEventID(const Long_t&); 
  void SetEventNumber(const UInt_t&);        
  void SetOrigMult(const UInt_t&);
  void SetCentrality(const UInt_t&);
  void SetVertexPos(const StThreeVectorF&);
  
  Int_t         GetNtrack() const { return mNtrack; }
  TClonesArray *GetTracks() const { return fTracks; }
  
 private:
  Int_t           mNtrack;                     // track number
  Long_t          mEventID;                    // event ID
  //  UInt_t          mEventNumber;                // number of the event
  UInt_t          mOrigMult;                   // number of StEvent tracks
  UInt_t          mCentrality;                 // centrality bin
  StThreeVectorF  mVertexPos;                  // primary vertex position
  
  TClonesArray        *fTracks;
  static TClonesArray *fgTracks;
  
  ClassDef(StFlowNanoEvent,1)
};

//inline UInt_t StFlowNanoEvent::EventNumber() const { return mEventNumber; }
inline Long_t StFlowNanoEvent::EventID() const { return mEventID; }
inline UInt_t StFlowNanoEvent::OrigMult() const { return mOrigMult; }
inline UInt_t StFlowNanoEvent::Centrality() const { return mCentrality; }
inline StThreeVectorF StFlowNanoEvent::VertexPos() const { return mVertexPos; }
//inline void StFlowNanoEvent::SetEventNumber(const UInt_t& event) {
//  mEventNumber = event; }
inline void StFlowNanoEvent::SetEventID(const Long_t& id) {
  mEventID = id; }
inline void StFlowNanoEvent::SetOrigMult(const UInt_t& tracks) {
  mOrigMult = tracks; }
inline void StFlowNanoEvent::SetCentrality(const UInt_t& centr) {
  mCentrality = centr; }
inline void StFlowNanoEvent::SetVertexPos(const StThreeVectorF& vertexPos) {
  mVertexPos = vertexPos; }

#endif



