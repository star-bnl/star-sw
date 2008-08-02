// -*- mode: c++;-*-
// $Id: StjMCParticleMaker.h,v 1.2 2008/08/02 19:22:26 tai Exp $
#ifndef STJETMCPARTICLEMAKER_HH
#define STJETMCPARTICLEMAKER_HH

#include "StMaker.h"
#include <Rtypes.h>

class StjMCParticleListWriter;

class TDirectory;
class TTree;

class StMaker;

namespace StSpinJet {
  class StjMC;
  class StjMCParticleListCut;
}

class StjMCParticleMaker : public StMaker {

public:

  StjMCParticleMaker(const Char_t *name, TDirectory* file, StMaker* uDstMaker);
  virtual ~StjMCParticleMaker() { }

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjMCParticleMaker.h,v 1.2 2008/08/02 19:22:26 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  TDirectory* _file;

  StMaker* _uDstMaker;

  StSpinJet::StjMC* _mc;
  StSpinJet::StjMCParticleListCut* _mcCut;

  StjMCParticleListWriter* _writer;

  ClassDef(StjMCParticleMaker, 0)

};

#endif // STJETMCPARTICLEMAKER_HH
