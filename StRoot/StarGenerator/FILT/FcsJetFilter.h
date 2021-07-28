/*! \class FcsJetFilter

  This class is just used to filter non Jet events
  The important bits are:
    *change it so that it inherits from StarFilterMaker, not StMCFilter
    *replace RejectGE() with Filter()
    *fix the filter function so that it takes a StarGenEvent rather than a StarGenParticleMaster
*/

#ifndef STAR_FcsJetFilter
#define STAR_FcsJetFilter

#include <vector>
#include <string>
#include "StarGenerator/FILT/StarFilterMaker.h"
#include "StarGenerator/EVENT/StarGenEvent.h"

class StarGenParticleMaster;
class StarGenParticle;
class StarGenEvent;

class FcsJetFilter : public StarFilterMaker
{
 public:
  FcsJetFilter(); ///constructor
  virtual ~FcsJetFilter(){;};///destructor
  Int_t Filter( StarGenEvent *mEvent );

  ClassDef(FcsJetFilter,1);  
};

#endif
