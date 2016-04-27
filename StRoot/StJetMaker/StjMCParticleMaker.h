// -*- mode: c++;-*-
// $Id: StjMCParticleMaker.h,v 1.4.6.1 2016/04/27 15:22:49 zchang Exp $
#ifndef STJMCPARTICLEMAKER_H
#define STJMCPARTICLEMAKER_H

#include "StMaker.h"
#include <Rtypes.h>

class StjMCParticleListWriter;

class TDirectory;
class TTree;

class StMaker;

class StjMC;
class StjMCParticleListCut;

class StjMCParticleMaker : public StMaker {

public:

  StjMCParticleMaker(const Char_t *name, TDirectory* file, StMaker* uDstMaker);
  virtual ~StjMCParticleMaker() { }

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjMCParticleMaker.h,v 1.4.6.1 2016/04/27 15:22:49 zchang Exp $ built " __DATE__ " " __TIME__; return cvs;}

private:

  TDirectory* _file;

  StMaker* _uDstMaker;

  StjMC* _mc;
  StjMCParticleListCut* _mcCut;

  StjMCParticleListWriter* _writer;

  ClassDef(StjMCParticleMaker, 0)

};

#endif // STJMCPARTICLEMAKER_H
