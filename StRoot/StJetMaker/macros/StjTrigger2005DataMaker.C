#include "StMaker.h"

#include "StjTrgPassCondition.h"

#include "StjTrgMuDst.h"

#include "StjTrgJPWriter.h"
#include "StjTrgHTWriter.h"
#include "StjTrgMBWriter.h"
#include "StjTrgBEMCJetPatchTowerIdMap2005.h"

#include "StjTrgMuDst.h"

#include "StjTrgSoftMuDstEmcTriggerMakerFactory.h"
#include "StjTrgSoftMuDstTriggerSimuMakerFactory.h"

class StjTrigger2005DataMaker : public StMaker {

public:

  StjTrigger2005DataMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StEmcTriggerMaker* emcTrigMaker)
  : StMaker(name), _file(file), _uDstMaker(uDstMaker)
  , _softTrgFactory(new StjTrgSoftMuDstEmcTriggerMakerFactory(emcTrigMaker, uDstMaker, new StjTrgBEMCJetPatchTowerIdMap2005()))
  { }

  StjTrigger2005DataMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StTriggerSimuMaker* simuTrig)
  : StMaker(name), _file(file), _uDstMaker(uDstMaker)
  , _softTrgFactory(new StjTrgSoftMuDstTriggerSimuMakerFactory(simuTrig, uDstMaker, new StjTrgBEMCJetPatchTowerIdMap2005()))
  { }

  virtual ~StjTrigger2005DataMaker() { }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjTrigger2005DataMaker.C,v 1.9 2014/08/06 11:43:24 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}

private:

  TDirectory* _file;

  StMuDstMaker* _uDstMaker;
  StjTrgSoftFactory* _softTrgFactory;

  StjTrgWriter* _minbWriter;
  StjTrgWriter* _bht1Writer;
  StjTrgWriter* _bht2Writer;
  StjTrgWriter* _bjp1Writer;
  StjTrgWriter* _bjp2Writer;

public:

  Int_t Init()
  {
    StjTrgPassCondition* minbPassCondition = new StjTrgPassConditionHardOnly;
    StjTrg* minbTrg = new StjTrgMuDst(96011, minbPassCondition, _uDstMaker, _softTrgFactory->create());
    StjTrgPassCondition* minbFillCondition = new StjTrgPassConditionHardOnly;
    _minbWriter = new StjTrgMBWriter("trgMINB", "trgMINB", _file, minbTrg, minbFillCondition);

    StjTrgPassCondition* bht1PassCondition = new StjTrgPassConditionHardAndSoft;
    StjTrg* bht1Trg = new StjTrgMuDst(96201, bht1PassCondition, _uDstMaker, _softTrgFactory->create());
    StjTrgPassCondition* bht1FillCondition = new StjTrgPassConditionHardOrSoft;
    _bht1Writer = new StjTrgHTWriter("trgBHT1", "trgBHT1", _file, bht1Trg, bht1FillCondition);

    StjTrgPassCondition* bht2PassCondition = new StjTrgPassConditionHardAndSoft;
    StjTrg* bht2Trg = new StjTrgMuDst(96211, bht2PassCondition, _uDstMaker, _softTrgFactory->create());
    StjTrgPassCondition* bht2FillCondition = new StjTrgPassConditionHardOrSoft;
    _bht2Writer = new StjTrgHTWriter("trgBHT2", "trgBHT2", _file, bht2Trg, bht2FillCondition);

    StjTrgPassCondition* bjp1PassCondition = new StjTrgPassConditionHardAndSoft;
    StjTrg* bjp1Trg = new StjTrgMuDst(96221, bjp1PassCondition, _uDstMaker, _softTrgFactory->create());
    StjTrgPassCondition* bjp1FillCondition = new StjTrgPassConditionHardOrSoft;
    _bjp1Writer = new StjTrgJPWriter("trgBJP1", "trgBJP1", _file, bjp1Trg, bjp1FillCondition);

    StjTrgPassCondition* bjp2PassCondition = new StjTrgPassConditionHardAndSoft;
    StjTrg* bjp2Trg = new StjTrgMuDst(96233, bjp2PassCondition, _uDstMaker, _softTrgFactory->create());
    StjTrgPassCondition* bjp2FillCondition = new StjTrgPassConditionHardOrSoft;
    _bjp2Writer = new StjTrgJPWriter("trgBJP2", "trgBJP2", _file, bjp2Trg, bjp2FillCondition);

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
