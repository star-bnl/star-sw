/***************************************************************************
 *
 * $Id: StHbtEvent.hh,v 1.9 2000/05/25 21:54:16 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   HbtEvent is the "transient microDST"  Objects of this class are
 *   generated from the input data by a Reader, and then presented to
 *   the Cuts of the various active Analyses.
 *
 ***************************************************************************
 *
 * $Log: StHbtEvent.hh,v $
 * Revision 1.9  2000/05/25 21:54:16  laue
 * RotateZ implemented. Rotates momentum and helix around the z axis
 *
 * Revision 1.7  2000/02/18 21:32:23  laue
 * franksTrackCut changed. If mCharge is set to '0' there will be no cut
 * on charge. This is important for front-loaded cuts.
 *
 * copy constructor implemented for StHbtEvent, StHbtTrack and StHbtV0.
 *
 * franks1HistoD.cxx franks1HistoD.h franks2HistoD.cxx franks2HistoD.h
 * removed. We can now (CC5 on Solaris) use the versions (no D)
 *
 * Revision 1.6  1999/09/16 18:47:59  lisa
 * replace placeholder HbtV0Track stuff with Helens StHbtV0 classes
 *
 * Revision 1.5  1999/09/03 22:39:15  lisa
 * Readers now MUST have Report() methods and MAY have WriteHbtEvent() methods
 *
 * Revision 1.4  1999/07/19 14:24:06  hardtke
 * modifications to implement uDST
 *
 * Revision 1.3  1999/07/06 22:33:22  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.2  1999/06/29 17:50:27  fisyak
 * formal changes to account new StEvent, does not complie yet
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtEvent_hh
#define StHbtEvent_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtV0Collection.hh"

class StHbtTrackCut;
class StHbtV0Cut;

class StHbtEvent{
public:
  StHbtEvent();
  StHbtEvent(const StHbtEvent&, StHbtTrackCut* =0, StHbtV0Cut* =0); // copy constructor with track and v0 cuts
  ~StHbtEvent();
  void RotateZ(const double);

  unsigned short EventNumber() const;
  unsigned short CtbMult() const;
  unsigned short ZdcAdcEast() const;
  unsigned short ZdcAdcWest() const;
  int NumberOfTpcHits() const;
  unsigned short NumberOfTracks() const;
  unsigned short NumberOfGoodTracks() const;
  float ReactionPlane() const;
  float ReactionPlaneError() const;
  StHbtThreeVector PrimVertPos() const;
  StHbtTrackCollection* TrackCollection() const;
  StHbtV0Collection* V0Collection() const;

  void SetEventNumber(const unsigned short&);
  void SetCtbMult(const unsigned short&);
  void SetZdcAdcEast(const unsigned short&);
  void SetZdcAdcWest(const unsigned short&);
  void SetNumberOfTpcHits(const int&);
  void SetNumberOfTracks(const unsigned short&);
  void SetNumberOfGoodTracks(const unsigned short&);
  void SetReactionPlane(const float&);
  void SetReactionPlaneError(const float&);
  void SetPrimVertPos(const StHbtThreeVector&);

  // For I/O of this object -- functions defined in StHbtIO.cc
  friend ostream& operator<<(ostream& out, StHbtEvent& ev);
  friend istream& operator>>(istream& in,  StHbtEvent& ev);

private:
  unsigned short mEventNumber;           //
  unsigned short mCtbMultiplicity;       // Central Trigger Barrel
  unsigned short mZdcAdc[2];       // Zero-degree calorimeter 
                                         //values east/west
  int mTpcNhits;                         // number of TPC hits
  unsigned short mNumberOfTracks;     // total number of TPC tracks
  unsigned short mNumberOfGoodTracks; // number of "good" tracks
  float mReactionPlane[2]; //reaction plane/error  //   
  StHbtThreeVector mPrimVertPos;
  StHbtTrackCollection* mTrackCollection;
  StHbtV0Collection* mV0Collection;

  friend class StHbtIOBinary;
};

inline void StHbtEvent::SetEventNumber(const unsigned short& event){
mEventNumber = event;}
inline void StHbtEvent::SetCtbMult(const unsigned short& mult){
mCtbMultiplicity = mult;}
inline void StHbtEvent::SetZdcAdcEast(const unsigned short& adc){
mZdcAdc[0]= adc;}
inline void StHbtEvent::SetZdcAdcWest(const unsigned short& adc){
mZdcAdc[1]=adc;}
inline void StHbtEvent::SetNumberOfTpcHits(const int& nhits){
mTpcNhits = nhits;}
inline void StHbtEvent::SetNumberOfTracks(const unsigned short& tracks){
mNumberOfTracks = tracks;}
inline void StHbtEvent::SetNumberOfGoodTracks(const unsigned short& tracks){
mNumberOfGoodTracks = tracks;}
inline void StHbtEvent::SetReactionPlane(const float& rp){
mReactionPlane[0] = rp ;}
inline void StHbtEvent::SetReactionPlaneError(const float& rp ){
mReactionPlane[1]=rp;}
inline void StHbtEvent::SetPrimVertPos(const StHbtThreeVector& vp){
mPrimVertPos = vp;}

inline  unsigned short StHbtEvent::EventNumber() const {return mEventNumber;}
inline  unsigned short StHbtEvent::CtbMult() const {return mCtbMultiplicity;}
inline  unsigned short StHbtEvent::ZdcAdcEast() const {return mZdcAdc[0];}
inline  unsigned short StHbtEvent::ZdcAdcWest() const {return mZdcAdc[1];}
inline  int            StHbtEvent::NumberOfTpcHits() const {return mTpcNhits;}
inline  unsigned short StHbtEvent::NumberOfTracks() const {
return mNumberOfTracks;}
inline  unsigned short StHbtEvent::NumberOfGoodTracks() const {
return mNumberOfGoodTracks;}
inline  float          StHbtEvent::ReactionPlane() const {
return mReactionPlane[0];}
inline  float          StHbtEvent::ReactionPlaneError() const {
return mReactionPlane[1];}
inline StHbtTrackCollection* StHbtEvent::TrackCollection() const {return mTrackCollection;}
inline StHbtV0Collection* StHbtEvent::V0Collection() const 
{return mV0Collection;}
inline StHbtThreeVector StHbtEvent::PrimVertPos() const {return mPrimVertPos;}


#endif
