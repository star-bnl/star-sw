#ifndef STRARETRACKCUT_HH
#define STRARETRACKCUT_HH
#include <TObject.h>
class StPrimaryTrack;
class StRareTrack;
class StRareTrackCut : public TObject {

 public:
  virtual int Accept(StPrimaryTrack* track) = 0;
  virtual void Report() = 0;  

  ClassDef(StRareTrackCut,0)
};
#endif
