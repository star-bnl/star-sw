#include "StMaker.h"

#include "StjVertexMuDst.h"

#include "StjVertexWriter.h"

#include <TDirectory.h>

class StjVertexMaker : public StMaker {

public:

  StjVertexMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker)
  : StMaker(name), _file(file), _uDstMaker(uDstMaker)
  { }

  virtual ~StjVertexMaker() { }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjVertexMaker.C,v 1.2 2014/08/06 11:43:24 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}

private:

  TDirectory* _file;

  StjVertex* _vertex;

  StjVertexWriter* _writer;

  StMuDstMaker* _uDstMaker;
public:

  Int_t Init()
  {
    _vertex = new StjVertexMuDst(_uDstMaker);

    _writer = new StjVertexWriter("vertex", "vertex", _file, _vertex);

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

  ClassDef(StjVertexMaker, 0)

};
