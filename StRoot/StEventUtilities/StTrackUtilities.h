#ifndef __StTrackUtilities_h__
#define __StTrackUtilities_h__
#include "TObject.h"
class StPrimaryTrack;
class StTrack;

class StTrackUtilities : public TObject {
 public:
  static StTrackUtilities* 	instance();
  virtual ~StTrackUtilities() {fgInstance = 0;}
  static void FillPrimaryTracks();
  static void FillPrimaryTrack(StPrimaryTrack *pTrack);
  static void FillFlags(StTrack* gTrack);
  static Bool_t StFixTopoMap(StTrack* gTrack);
 private:
  StTrackUtilities() {}
  static StTrackUtilities* fgInstance;
  ClassDef(StTrackUtilities,1) //C++ TChair for TpcInnerSectorPosition
};
#endif /* __StTrackUtilities_h__ */
