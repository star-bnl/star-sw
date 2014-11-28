 /***************************************************************************
 *
 * $Id: 
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 * a particle cut that uses NSigma cut below certain P threshhold
 # and a PID probability above it
 *
 ***************************************************************************
 *
 * $Log:
 *
 **************************************************************************/

#ifndef adamsTrackCut_hh
#define adamsTrackCut_hh


//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "Stsstream.h"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Cut/franksTrackCut.h"

class adamsTrackCut : public franksTrackCut
{

 public:

  adamsTrackCut();
  adamsTrackCut(adamsTrackCut& );
  ~adamsTrackCut();
  
  virtual bool Pass(const StHbtTrack*);

  virtual StHbtString Report();

  void SetPIDPThreshold(const float&);

  adamsTrackCut* Clone();

  ostrstream* finalReport() const;

 private:   // here are the quantities I want to cut on...
  float             mPIDPThreshold;

 protected:
  long              mNTracksPassed;
  long              mNTracksFailed;

#ifdef __ROOT__
  ClassDef(adamsTrackCut, 1)
#endif
};

inline void adamsTrackCut::SetPIDPThreshold(const float& pidpt){mPIDPThreshold = pidpt;}
inline adamsTrackCut* adamsTrackCut::Clone() { adamsTrackCut* c = new adamsTrackCut(*this); return c;}

#endif
