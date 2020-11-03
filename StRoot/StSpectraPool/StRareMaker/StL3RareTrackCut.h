#ifndef STL3RARETRACKCUT_HH
#define STL3RARETRACKCUT_HH

#include <TObject.h>

class StGlobalTrack;

class StL3RareTrackCut : public TObject {

 public:
  virtual int Accept(StGlobalTrack* track) = 0;
  virtual void Report() = 0;  

  ClassDef(StL3RareTrackCut,0)
};
#endif
