//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowEvent.hh,v 1.1 1999/11/11 23:08:56 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//////////////////////////////////////////////////////////////////////
//
// Description: A subset of StEvent 
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowEvent.hh,v $
// Revision 1.1  1999/11/11 23:08:56  posk
// Rearrangement of files.
//
// Revision 1.1  1999/11/04 19:02:06  snelling
// First check in of StFlowMaker. It contains the common code from
// StFlowTagMaker and StFlowAnalysisMaker.
//
//
//////////////////////////////////////////////////////////////////////

#ifndef StFlowEvent_hh
#define StFlowEvent_hh
//#include "StFlowMaker.hh"
#include "StFlowTrackCollection.hh"

class StFlowEvent{

public:

                  StFlowEvent();
  virtual         ~StFlowEvent();

  unsigned short  EventNumber() const;
  unsigned short  NumberOfTracks() const;
  StFlowTrackCollection* TrackCollection() const;

  void SetEventNumber(const unsigned short&);
  void SetNumberOfTracks(const unsigned short&);

  // For I/O of this object -- functions defined in StHbtIO.cc
  friend ostream& operator<<(ostream& out, StFlowEvent& ev);
  friend istream& operator>>(istream& in,  StFlowEvent& ev);

private:

  unsigned short  mEventNumber;                    // number of the event
  unsigned short  mNumberOfTracks;                 // number of tracks
  StFlowTrackCollection* mTrackCollection;         //!

};

inline void StFlowEvent::SetEventNumber(const unsigned short& event){
  mEventNumber = event;}
inline void StFlowEvent::SetNumberOfTracks(const unsigned short& tracks){
  mNumberOfTracks = tracks;}

inline  unsigned short StFlowEvent::EventNumber() const {return mEventNumber;}
inline  unsigned short StFlowEvent::NumberOfTracks() const {
  return mNumberOfTracks;}
inline StFlowTrackCollection* StFlowEvent::TrackCollection() const {
  return mTrackCollection;}

#endif
