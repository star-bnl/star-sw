/*! \class FcsDYFilter

  This class is just used to filter non DrellYan events
  The important bits are:
    *change it so that it inherits from StarFilterMaker, not StMCFilter
    *replace RejectGE() with Filter()
    *fix the filter function so that it takes a StarGenEvent rather than a StarGenParticleMaster
*/

#ifndef STAR_FcsDYFilter
#define STAR_FcsDYFilter

#include <vector>
#include <string>
#include "StarGenerator/FILT/StarFilterMaker.h"
#include "StarGenerator/EVENT/StarGenEvent.h"

class StarGenParticleMaster;
class StarGenParticle;
class StarGenEvent;

class FcsDYFilter : public StarFilterMaker
{
 public:
  FcsDYFilter(); ///constructor
  virtual ~FcsDYFilter(){;};///destructor
  Int_t Filter( StarGenEvent *mEvent );

  ClassDef(FcsDYFilter,1);  
};

#endif
