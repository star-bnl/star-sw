#ifndef __StarMuEventReader_h__
#define __StarMuEventReader_h__

#include "StarGenerator/BASE/StarGenerator.h"
#include "StarGenerator/EVENT/StarGenEvent.h"
#include "TTreeIter.h"

class StarMuEventReader : public StarGenerator
{
public:
  StarMuEventReader( const Char_t *name="MuReader" ) 
    : StarGenerator(name)
  { 
    mEvent = new StarGenEvent("primaryEvent");
  };
 ~StarMuEventReader(){ /* nada */ };

  Int_t Init();
  Int_t Generate();
  Int_t Skip(Int_t Nskip);
  StarGenParticle *AddParticle();
  StarGenParticle *AddParticle( const Char_t *type );
private:
protected:
  TTreeIter *fMuDstIter;
  ClassDef( StarMuEventReader, 1 );
};

#endif
