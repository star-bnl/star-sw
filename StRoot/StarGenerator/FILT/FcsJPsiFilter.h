/*! \class FcsJPsiFilter

  This class is just used to filter non DrellYan events
  The important bits are:
    *change it so that it inherits from StarFilterMaker, not StMCFilter
    *replace RejectGE() with Filter()
    *fix the filter function so that it takes a StarGenEvent rather than a StarGenParticleMaster
*/

#ifndef STAR_FcsJPsiFilter
#define STAR_FcsJPsiFilter

#include <vector>
#include <string>
#include "StarGenerator/FILT/StarFilterMaker.h"
#include "StarGenerator/EVENT/StarGenEvent.h"

class StarGenParticleMaster;
class StarGenParticle;
class StarGenEvent;

class FcsJPsiFilter : public StarFilterMaker
{
 public:
  FcsJPsiFilter(); ///constructor
  virtual ~FcsJPsiFilter(){;};///destructor
  Int_t Filter( StarGenEvent *mEvent );

  ClassDef(FcsJPsiFilter,1);  
};

#endif
