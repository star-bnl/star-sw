#ifndef StPicoCut_h
#define StPicoCut_h

#include "TObject.h"

class StMuEvent;
class StMuTrack;
class StPicoV0;
class StPicoTrack;

class StPicoCut : public TObject {
 public:
  StPicoCut();
  ~StPicoCut();
  
  bool passEvent( StMuEvent * );
  bool passTrack( StMuTrack * );
  /*
  bool passV0Daughter( StPicoTrack * );
  bool passV0( StPicoV0 *, StMuEvent * );
  bool passKs( StPicoV0 * );
  bool passLambda( StPicoV0 * );
  bool passLbar( StPicoV0 * );
  int  flowFlag( StMuTrack * );
  */
  ClassDef(StPicoCut,1)
};

#endif
