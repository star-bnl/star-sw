#include "StMaker.h"

#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

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
  {static const char cvs[]="Tag $Name:  $ $Id: StjSpinMaker.C,v 1.1 2008/11/05 05:48:16 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

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

    int runNumber = _uDstMaker->muDst()->event()->runId();
    int eventId = _uDstMaker->muDst()->event()->eventId();

    int bx7 = _uDstMaker->muDst()->event()->l0Trigger().bunchCrossingId7bit( runNumber );
    int bx48 =  _uDstMaker->muDst()->event()->l0Trigger().bunchCrossingId();

    int spin4 = _uDstMaker->muDst()->event()->l0Trigger().spinBits( runNumber );

    int bbcTimebin = _uDstMaker->muDst()->event()->bbcTriggerDetector().onlineTimeDifference()/32;

    double vertexZ = _uDstMaker->muDst()->event()->primaryVertexPosition().z();

    cout
      << runNumber << " "
      << eventId << " "
      << bx7 << " "
      << bx48 << " "
      << spin4 << " "
      << bbcTimebin << " "
      << vertexZ << " "
      << endl;

    return kStOk;
  }

  Int_t Finish()
  {
    _writer->Finish();

    return kStOk;
  }

  ClassDef(StjSpinMaker, 0)

};
