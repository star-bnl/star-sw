// -*- mode: c++;-*-
// $Id: StjMCParticleMaker.h,v 1.1 2008/08/02 04:04:49 tai Exp $
#ifndef STJETMCPARTICLEMAKER_HH
#define STJETMCPARTICLEMAKER_HH

#include "StMaker.h"
#include <Rtypes.h>

class StJetMCParticleListWriter;

class TDirectory;
class TTree;

class StMaker;

namespace StSpinJet {
  class StJetMC;
  class StJetMCParticleListCut;
}

class StJetMCParticleMaker : public StMaker {

public:

  StJetMCParticleMaker(const Char_t *name, TDirectory* file, StMaker* uDstMaker);
  virtual ~StJetMCParticleMaker() { }

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjMCParticleMaker.h,v 1.1 2008/08/02 04:04:49 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  TDirectory* _file;

  StMaker* _uDstMaker;

  StSpinJet::StJetMC* _mc;
  StSpinJet::StJetMCParticleListCut* _mcCut;

  StJetMCParticleListWriter* _writer;

  ClassDef(StJetMCParticleMaker, 0)

};

#endif // STJETMCPARTICLEMAKER_HH
