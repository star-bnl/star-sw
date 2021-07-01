/*! \class FcsDYBGFilter

  This class is just used to filter non DrellYan events
  The important bits are:
    *change it so that it inherits from StarFilterMaker, not StMCFilter
    *replace RejectGE() with Filter()
    *fix the filter function so that it takes a StarGenEvent rather than a StarGenParticleMaster
*/

#ifndef STAR_FcsDYBGFilter
#define STAR_FcsDYBGFilter

#include <vector>
#include <string>
#include "StarGenerator/FILT/StarFilterMaker.h"
#include "StarGenerator/EVENT/StarGenEvent.h"

class StarGenParticleMaster;
class StarGenParticle;
class StarGenEvent;

class FcsDYBGFilter : public StarFilterMaker
{
 public:
  FcsDYBGFilter(); ///constructor
  FcsDYBGFilter(Int_t dy, Int_t check, Int_t swap); ///constructor
  virtual ~FcsDYBGFilter(){;};///destructor
  Int_t Filter( StarGenEvent *mEvent );

  Int_t mDYmode=0;
  Int_t mCheckmode=0;
  Int_t mSwap=0;

  ClassDef(FcsDYBGFilter,1);  
};

#endif
