/***************************************************************************
 *
 * $Id: FlowEvent.hh,v 1.1 1999/11/04 19:02:06 snelling Exp $
 *
 * Author: Raimond Snellings and Art Poskanzer
 ***************************************************************************
 *
 * Description: part of Flow Framework: 
 *
 ***************************************************************************
 *
 * $Log: FlowEvent.hh,v $
 * Revision 1.1  1999/11/04 19:02:06  snelling
 * First check in of StFlowMaker. It contains the common code from
 * StFlowTagMaker and StFlowAnalysisMaker.
 *
 *
 **************************************************************************/

#ifndef FlowEvent_hh
#define FlowEvent_hh

#include "FlowTrackCollection.hh"

class FlowEvent{
public:
  FlowEvent();
  ~FlowEvent();

  unsigned short EventNumber() const;
  unsigned short NumberOfTracks() const;
  unsigned short NumberOfGoodTracks() const;
  float ReactionPlane() const;
  float ReactionPlaneError() const;
  FlowTrackCollection* TrackCollection() const;

  void SetEventNumber(const unsigned short&);
  void SetNumberOfTracks(const unsigned short&);
  void SetNumberOfGoodTracks(const unsigned short&);
  void SetReactionPlane(const float&);
  void SetReactionPlaneError(const float&);

  // For I/O of this object -- functions defined in StHbtIO.cc
  friend ostream& operator<<(ostream& out, FlowEvent& ev);
  friend istream& operator>>(istream& in,  FlowEvent& ev);

private:
  unsigned short mEventNumber;           //
  unsigned short mNumberOfTracks;     // total number of TPC tracks
  unsigned short mNumberOfGoodTracks; // number of "good" tracks
  float mReactionPlane[2]; //reaction plane/error  //   
  FlowTrackCollection* mTrackCollection;

};

inline void FlowEvent::SetEventNumber(const unsigned short& event){
mEventNumber = event;}
inline void FlowEvent::SetNumberOfTracks(const unsigned short& tracks){
mNumberOfTracks = tracks;}
inline void FlowEvent::SetNumberOfGoodTracks(const unsigned short& tracks){
mNumberOfGoodTracks = tracks;}
inline void FlowEvent::SetReactionPlane(const float& rp){
mReactionPlane[0] = rp ;}
inline void FlowEvent::SetReactionPlaneError(const float& rp ){
mReactionPlane[1]=rp;}

inline  unsigned short FlowEvent::EventNumber() const {return mEventNumber;}
inline  unsigned short FlowEvent::NumberOfTracks() const {
return mNumberOfTracks;}
inline  unsigned short FlowEvent::NumberOfGoodTracks() const {
return mNumberOfGoodTracks;}
inline  float          FlowEvent::ReactionPlane() const {
return mReactionPlane[0];}
inline  float          FlowEvent::ReactionPlaneError() const {
return mReactionPlane[1];}
inline FlowTrackCollection* FlowEvent::TrackCollection() const {return mTrackCollection;}

#endif
