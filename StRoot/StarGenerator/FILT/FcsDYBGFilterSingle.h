/*! \class FcsDYBGFilterSingle

  This class is just used to filter non DrellYan events
  The important bits are:
    *change it so that it inherits from StarFilterMaker, not StMCFilter
    *replace RejectGE() with Filter()
    *fix the filter function so that it takes a StarGenEvent rather than a StarGenParticleMaster
*/

#ifndef STAR_FcsDYBGFilterSingle
#define STAR_FcsDYBGFilterSingle

#include <vector>
#include <string>
#include "StarGenerator/FILT/StarFilterMaker.h"
#include "StarGenerator/EVENT/StarGenEvent.h"

class StarGenParticleMaster;
class StarGenParticle;
class StarGenEvent;

class FcsDYBGFilterSingle : public StarFilterMaker
{
 public:
  FcsDYBGFilterSingle(); ///constructor
  FcsDYBGFilterSingle(Int_t dy, Int_t check, Int_t swap); ///constructor
  virtual ~FcsDYBGFilterSingle(){;};///destructor
  Int_t Filter( StarGenEvent *mEvent );

  Int_t mDYmode=0;
  Int_t mCheckmode=0;
  Int_t mSwap=0;

  ClassDef(FcsDYBGFilterSingle,1);  
};

#endif
