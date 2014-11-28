#ifndef __agconvert__
#define __agconvert__
#include "Rtypes.h"

class TVolumeView;
class TVolume;
class agconvert 
{ 
  protected:
  TVolume     * fTop; 
  TVolumeView * fView;
  public:
   agconvert():fTop(0),fView(0) {}; // fTop,fView initialised to 0
  virtual  ~agconvert()         {};

  TVolume     * ag2root ();
  TVolume     * convert ();
  TVolumeView * ag2view (TVolume *tree=0, Int_t first=3, Int_t last=5);

  ClassDef(agconvert,0)
};
extern agconvert &Ag;        // expose <Ag> object global pointer to CINT
#endif
