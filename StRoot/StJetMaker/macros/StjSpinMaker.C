#include "StMaker.h"

#include "StjSpinMuDst.h"

#include "StjSpinWriter.h"

#include <TDirectory.h>

class StjSpinMaker : public StMaker {

public:

  StjSpinMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker)
  : StMaker(name), _file(file), _uDstMaker(uDstMaker)
  { }

  virtual ~StjSpinMaker() { }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjSpinMaker.C,v 1.3 2014/08/06 11:43:24 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}

private:

  TDirectory* _file;

  StjSpin* _spin;

  StjSpinWriter* _writer;

  StMuDstMaker* _uDstMaker;
public:

  Int_t Init()
  {
    _spin = new StjSpinMuDst(_uDstMaker);
    _writer = new StjSpinWriter("spin", "spin", _file, _spin);
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
  ClassDef(StjSpinMaker, 0)
};
