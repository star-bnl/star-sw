#include "StMaker.h"

#include "StjTrgPassCondition.h"

#include "StjTrgMuDst.h"

#include "StjTrgJPWriter.h"
#include "StjTrgHTWriter.h"
#include "StjTrgMBWriter.h"
#include "StjTrgBEMCJetPatchTowerIdMap2006.h"

#include "StjTrgMuDst.h"

#include "StjTrgSoftMuDstEmcTriggerMakerFactory.h"
#include "StjTrgSoftMuDstTriggerSimuMakerFactory.h"

class StjTrigger2006DataMaker : public StMaker {

public:

  StjTrigger2006DataMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StEmcTriggerMaker* emcTrigMaker)
  : StMaker(name), _file(file), _uDstMaker(uDstMaker)
  , _softTrgFactory(new StjTrgSoftMuDstEmcTriggerMakerFactory(emcTrigMaker, uDstMaker, new StjTrgBEMCJetPatchTowerIdMap2006()))
  { }

  StjTrigger2006DataMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StTriggerSimuMaker* simuTrig)
  : StMaker(name), _file(file), _uDstMaker(uDstMaker)
  , _softTrgFactory(new StjTrgSoftMuDstTriggerSimuMakerFactory(simuTrig, uDstMaker, new StjTrgBEMCJetPatchTowerIdMap2006()))
  { }

  virtual ~StjTrigger2006DataMaker() { }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjTrigger2006DataMaker.C,v 1.3 2014/08/06 11:43:24 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}

private:

  TDirectory* _file;

  StMuDstMaker* _uDstMaker;
  StjTrgSoftFactory* _softTrgFactory;

  StjTrgWriter* _minbWriter;
  StjTrgWriter* _bht2Writer;
  StjTrgWriter* _bjp1_1Writer;
  StjTrgWriter* _bjp1_2Writer;

public:

  Int_t Init()
  {
    StjTrgPassCondition* minbPassCondition = new StjTrgPassConditionHardOnly;
    StjTrg* minbTrg = new StjTrgMuDst(117001, minbPassCondition, _uDstMaker, _softTrgFactory->create());
    StjTrgPassCondition* minbFillCondition = new StjTrgPassConditionHardOnly;
    _minbWriter = new StjTrgMBWriter("trg117001", "trg117001", _file, minbTrg, minbFillCondition);

    StjTrgPassCondition* bht2PassCondition = new StjTrgPassConditionHardAndSoft;
    StjTrg* bht2Trg = new StjTrgMuDst(137213, bht2PassCondition, _uDstMaker, _softTrgFactory->create());
    StjTrgPassCondition* bht2FillCondition = new StjTrgPassConditionHardOrSoft;
    _bht2Writer = new StjTrgHTWriter("trg137213", "trg137213", _file, bht2Trg, bht2FillCondition);

    StjTrgPassCondition* bjp1_1PassCondition = new StjTrgPassConditionHardAndSoft;
    StjTrg* bjp1_1Trg = new StjTrgMuDst(137221, bjp1_1PassCondition, _uDstMaker, _softTrgFactory->create());
    StjTrgPassCondition* bjp1_1FillCondition = new StjTrgPassConditionHardOrSoft;
    _bjp1_1Writer = new StjTrgJPWriter("trg137221", "trg137221", _file, bjp1_1Trg, bjp1_1FillCondition);

    StjTrgPassCondition* bjp1_2PassCondition = new StjTrgPassConditionHardAndSoft;
    StjTrg* bjp1_2Trg = new StjTrgMuDst(137222, bjp1_2PassCondition, _uDstMaker, _softTrgFactory->create());
    StjTrgPassCondition* bjp1_2FillCondition = new StjTrgPassConditionHardOrSoft;
    _bjp1_2Writer = new StjTrgJPWriter("trg137222", "trg137222", _file, bjp1_2Trg, bjp1_2FillCondition);

    _minbWriter->Init();
    _bht2Writer->Init();
    _bjp1_1Writer->Init();
    _bjp1_2Writer->Init();

    return kStOk;
  }

  Int_t Make()
  {
    _minbWriter->Make();
    _bht2Writer->Make();
    _bjp1_1Writer->Make();
    _bjp1_2Writer->Make();

    return kStOk;
  }

  Int_t Finish()
  {
    _minbWriter->Finish();
    _bht2Writer->Finish();
    _bjp1_1Writer->Finish();
    _bjp1_2Writer->Finish();

    return kStOk;
  }

  ClassDef(StjTrigger2006DataMaker, 0)

};
