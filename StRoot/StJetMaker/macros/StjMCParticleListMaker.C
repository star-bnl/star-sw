#include "StMaker.h"

#include "StjMCParticleCut.h"
#include "StjMCParticleCutEta.h"
#include "StjMCParticleCutEtaForStatus.h"
#include "StjMCParticleCutStatus.h"

#include "StjMCParticleListWriter.h"

#include "StjMCMuDst.h"

#include "StjMCParticleListCut.h"

#include <TDirectory.h>

class StjMCParticleListMaker : public StMaker {

public:

  StjMCParticleListMaker(const Char_t *name, TDirectory* file, StMaker* uDstMaker)
  : StMaker(name), _file(file), _uDstMaker(uDstMaker) { }
  virtual ~StjMCParticleListMaker() { }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjMCParticleListMaker.C,v 1.4 2014/08/06 11:43:24 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}

private:

  TDirectory* _file;

  StMaker* _uDstMaker;

  StjMC* _mc;
  StjMCParticleListCut _mcCut;

  StjMCParticleListWriter* _writer;

public:

  Int_t Init()
  {
    _mc = new StjMCMuDst(_uDstMaker);

    //    int goodStatus[] = {1, 3};
    //    _mcCut.addCut(new StjMCParticleCutStatus(2, goodStatus));
    _mcCut.addCut(new StjMCParticleCutEtaForStatus(-3.0, 3.0, 2));
    _mcCut.addCut(new StjMCParticleCutEtaForStatus(-3.0, 3.0, 1));

    _writer = new StjMCParticleListWriter("mcParticles", _file);
    
    return kStOk;
  }

  Int_t Make()
  {
    StjMCParticleList theList = _mc->getMCParticleList();

    theList = _mcCut(theList);

    _writer->Fill(theList);

    return kStOk;
  }

  Int_t Finish()
  {
    _writer->Finish();

    return kStOk;
  }

  ClassDef(StjMCParticleListMaker, 0)

};
