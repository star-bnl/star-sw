/// \File StiLocalTrackSeedFinder.h
/// \author M.L. Miller (Yale Software) 10/01
#ifndef StiLocalTrackSeedFinder_HH
#define StiLocalTrackSeedFinder_HH
#include "Stiostream.h"
using std::ostream;
#include <vector>
using std::vector;
#include "StiSortedHitIterator.h"
#include "StDetectorDbMaker/StiLocalTrackSeedFinderParameters.h"
#include "StDetectorDbMaker/StiLocalTrackSeedFinderParameters.h"
#include "TStopwatch.h"
class StiDetector;

/// \class StiLocalTrackSeedFinder
/// StiLocalTrackSeedFinder is a concrete implementation of StiKalmanTrackFinder.
/// It is built from a collection of StiDetectorObjects, which it
/// stores in a vector and orders properly, then uses each detector
/// as a layer from which a one-point seed can be generated.  It then proceeds
/// to step inwards, iteratively making a local decision at each step.
/// \author M.L. Miller (Yale Software) 10/01
/// \author Claude A Pruneau (Wayne State) Jan 2003
class StiLocalTrackSeedFinder : public TNamed {
public:
  StiLocalTrackSeedFinder(const Char_t * name,
			  const Char_t * description) : TNamed(name,description), _Reset(kTRUE),  fRxyMin(0) {}
  virtual ~StiLocalTrackSeedFinder() {}
  StiKalmanTrack* findTrack(double rMin=0);
  virtual void Clear(const Option_t *opt=""){Reset();};
  virtual void Initialize(){};
  virtual void print() const;
  virtual void Reset() {  _Reset = kTRUE;  cout <<"StiLocalTrackSeedFinder::Reset() -I- Done"<<endl;}

  virtual void Unset(){;}
  Bool_t isReset() {Bool_t state = _Reset; if (_Reset) _Reset = kFALSE; return state;}

  void   findTracks(){}; 
  Bool_t find(StiKalmanTrack *track, int direction,double rmin=0){return kFALSE;};
  void   ExtendTracksToVertex(StiHit* vertex){};

  friend ostream& operator<<(ostream& os, const StiLocalTrackSeedFinder & f);

protected:
  StiKalmanTrack*makeTrack(StiHit* hit);
  ///Extend hit looking for closest neighbor in z
  Bool_t ExtendHit(StiHit & hit);
  ///Extrapolate to next layer using straight line, add hit closest in z
  Bool_t Extrapolate();
  StiKalmanTrack* InitializeTrack();
  Bool_t   _Reset;
  StiSortedHitIterator   _hitIter;
  vector<StiHit*>        _seedHits;
  double fRxyMin;
 private:
  //The following are not implemented, as they are non-trivial
  //and the default compiler generated versions will be wrong.
  StiLocalTrackSeedFinder();
  StiLocalTrackSeedFinder(const StiLocalTrackSeedFinder&);
  StiLocalTrackSeedFinder operator=(const StiLocalTrackSeedFinder&);
  //Count how many hits we've skipped in extrapolation
  int    _skipped;
  
  Double_t    chi2;
  TStopwatch *mTimg[3]; //seeds,traks,prims
  static Int_t   _debug;
  ClassDef(StiLocalTrackSeedFinder,0)
};
#endif
