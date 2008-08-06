#include "StMaker.h"

#include "StjTrgPassCondition.h"

#include "StjTrg.h"

#include "StjTrgJPWriter.h"
#include "StjTrgHTWriter.h"
#include "StjTrgMBWriter.h"

#include "StjTrg.h"

#include "StjTrgSoftwareEmcTriggerMakerFactory.h"
#include "StjTrgSoftwareTriggerSimuMakerFactory.h"

class StjTrigger2005DataMaker : public StMaker {

public:

  StjTrigger2005DataMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StEmcTriggerMaker* emcTrigMaker)
  : StMaker(name), _file(file), _uDstMaker(uDstMaker)
  , _softTrgFactory(new StjTrgSoftwareEmcTriggerMakerFactory(emcTrigMaker))
  { }

  StjTrigger2005DataMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StTriggerSimuMaker* simuTrig)
  : StMaker(name), _file(file), _uDstMaker(uDstMaker)
  , _softTrgFactory(new StjTrgSoftwareTriggerSimuMakerFactory(simuTrig))
  { }

  virtual ~StjTrigger2005DataMaker() { }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjTrigger2005DataMaker.C,v 1.1 2008/08/06 05:49:52 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  TDirectory* _file;

  StMuDstMaker* _uDstMaker;
  StjTrgSoftwareFactory* _softTrgFactory;

  StjTrgWriter* _minbWriter;
  StjTrgWriter* _bht1Writer;
  StjTrgWriter* _bht2Writer;
  StjTrgWriter* _bjp1Writer;
  StjTrgWriter* _bjp2Writer;

public:

  Int_t Init()
  {
    StjTrg* minbTrg = new StjTrg(96011, _uDstMaker, _softTrgFactory->create());
    StjTrgPassCondition* minbFillCondition = new StjTrgPassConditionHardOnly(minbTrg);
    StjTrgPassCondition* minbPassCondition = new StjTrgPassConditionHardOnly(minbTrg);
    _minbWriter = new StjTrgMBWriter("trgMINB", "trgMINB", _file, minbTrg, minbFillCondition, minbPassCondition);

    StjTrg* bht1Trg = new StjTrg(96201, _uDstMaker, _softTrgFactory->create());
    StjTrgPassCondition* bht1FillCondition = new StjTrgPassConditionHardOrSoft(bht1Trg);
    StjTrgPassCondition* bht1PassCondition = new StjTrgPassConditionHardAndSoft(bht1Trg);
    _bht1Writer = new StjTrgHTWriter("trgBHT1", "trgBHT1", _file, bht1Trg, bht1FillCondition, bht1PassCondition);

    StjTrg* bht2Trg = new StjTrg(96211, _uDstMaker, _softTrgFactory->create());
    StjTrgPassCondition* bht2FillCondition = new StjTrgPassConditionHardOrSoft(bht2Trg);
    StjTrgPassCondition* bht2PassCondition = new StjTrgPassConditionHardAndSoft(bht2Trg);
    _bht2Writer = new StjTrgHTWriter("trgBHT2", "trgBHT2", _file, bht2Trg, bht2FillCondition, bht2PassCondition);

    StjTrg* bjp1Trg = new StjTrg(96221, _uDstMaker, _softTrgFactory->create());
    StjTrgPassCondition* bjp1FillCondition = new StjTrgPassConditionHardOrSoft(bjp1Trg);
    StjTrgPassCondition* bjp1PassCondition = new StjTrgPassConditionHardAndSoft(bjp1Trg);
    _bjp1Writer = new StjTrgJPWriter("trgBJP1", "trgBJP1", _file, bjp1Trg, bjp1FillCondition, bjp1PassCondition);

    StjTrg* bjp2Trg = new StjTrg(96233, _uDstMaker, _softTrgFactory->create());
    StjTrgPassCondition* bjp2FillCondition = new StjTrgPassConditionHardOrSoft(bjp2Trg);
    StjTrgPassCondition* bjp2PassCondition = new StjTrgPassConditionHardAndSoft(bjp2Trg);
    _bjp2Writer = new StjTrgJPWriter("trgBJP2", "trgBJP2", _file, bjp2Trg, bjp2FillCondition, bjp2PassCondition);

    _minbWriter->Init();
    _bht1Writer->Init();
    _bht2Writer->Init();
    _bjp1Writer->Init();
    _bjp2Writer->Init();

    return kStOk;
  }

  Int_t Make()
  {
    _minbWriter->Make();
    _bht1Writer->Make();
    _bht2Writer->Make();
    _bjp1Writer->Make();
    _bjp2Writer->Make();

    return kStOk;
  }

  Int_t Finish()
  {
    _minbWriter->Finish();
    _bht1Writer->Finish();
    _bht2Writer->Finish();
    _bjp1Writer->Finish();
    _bjp2Writer->Finish();

    return kStOk;
  }

  ClassDef(StjTrigger2005DataMaker, 0)

};
