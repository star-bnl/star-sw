#include "StMaker.h"

#include "StjMCKinMuDst.h"

#include "StjMCKinWriter.h"

#include <TDirectory.h>

class StjMCKinMaker : public StMaker {

public:

  StjMCKinMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker)
  : StMaker(name), _file(file), _uDstMaker(uDstMaker)
  { }

  virtual ~StjMCKinMaker() { }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjMCKinMaker.C,v 1.2 2014/08/06 11:43:24 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}

private:

  TDirectory* _file;

  StjMCKin* _mckin;

  StjMCKinWriter* _writer;

  StMuDstMaker* _uDstMaker;
public:

  Int_t Init()
  {
    _mckin = new StjMCKinMuDst(_uDstMaker);

    _writer = new StjMCKinWriter("mckin", "mckin", _file, _mckin);

    _writer->Init();

    return kStOk;
  }

  Int_t Make()
  {
    _writer->Make();

    return kStOk;
  }

  Int_t Finish()
  {
    _writer->Finish();

    return kStOk;
  }

  ClassDef(StjMCKinMaker, 0)

};
