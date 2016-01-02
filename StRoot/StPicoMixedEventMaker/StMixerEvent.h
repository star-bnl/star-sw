#ifndef StMixerEvent_hh
#define StMixerEvent_hh

#include <vector>
#include "StarClassLibrary/StThreeVectorF.hh"

#include "StMixerTrack.h"
/* **************************************************
 *
 * Event class used for mixed event buffer, stripped down 
 * to minimum information neede to reconstruct the helix
 * and basic track information. Currently include:
 * 1) primVtx
 * 2) B-Field
 * 3) MixerTrack array
 * 4) Arrays of indices pointing to pions and kaons in mixertrack
 *
 * **************************************************
 *
 *  Initial Authors:  
 *         ** Michael Lomnitz (mrlomnitz@lbl.gov)
 *            Mustafa Mustafa (mmustafa@lbl.gov)
 *
 *  ** Code Maintainer
 *
 * **************************************************
 */

class StMixerTrack;

class StMixerEvent{
 public:
  StMixerEvent();
  StMixerEvent(StMixerEvent *);
  StMixerEvent(StThreeVectorF, float);
  ~StMixerEvent(){;};
  void addPion(int);
  void addKaon(int);
  void addTrack(StMixerTrack);
  void setPos( float const, float const, float const);
  void setField( float const );
  int getNoTracks();
  int getNoKaons();
  int getNoPions();
  int pionId(int counter);
  int kaonId(int counter);
  StMixerTrack pionAt(int const); 
  StMixerTrack kaonAt(int const); 
  StThreeVectorF const & vertex() const;
  double const field() const;
 private:
  StThreeVectorF mVtx;
  float mBField;
  std::vector <StMixerTrack  > mTracks;
  std::vector <int  > mEventKaons;
  std::vector <int  > mEventPions;
};
inline void StMixerEvent::setPos( float const vx, float const vy, float const vz){
  mVtx = StThreeVectorF(vx, vy, vz);
}
inline void StMixerEvent::setField( float const field ){ mBField = field; }
inline int StMixerEvent::getNoPions(){ return mEventPions.size(); }
inline int StMixerEvent::getNoKaons(){ return mEventKaons.size(); }
inline int StMixerEvent::getNoTracks(){ return mTracks.size();}
inline int StMixerEvent::pionId(int counter){ return mEventPions.at(counter); }
inline int StMixerEvent::kaonId(int counter){ return mEventKaons.at(counter); }
inline StMixerTrack StMixerEvent::pionAt(int const counter) { return( mTracks.at(mEventPions.at(counter)) );} 
inline StMixerTrack StMixerEvent::kaonAt(int const counter) { return( mTracks.at(mEventKaons.at(counter)) );} 
inline StThreeVectorF const & StMixerEvent::vertex() const { return mVtx; }
inline double const StMixerEvent::field() const {return mBField; }
#endif
