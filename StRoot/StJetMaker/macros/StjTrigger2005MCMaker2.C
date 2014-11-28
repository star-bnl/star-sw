#include "StMaker.h"

#include "StjTrgPassCondition.h"

#include "StjTrgMuDst.h"

#include "StjBEMCMuDst.h"

#include "StjTrgJPWriter.h"
#include "StjTrgHTWriter.h"
#include "StjTrgMBWriter.h"
#include "StjTrgBEMCJetPatchTowerIdMap2005.h"

#include "StjTrgSoftPass.h"
#include "StjTrgSoftEtThresholdBHT.h"
#include "StjTrgSoftEtThresholdBJP.h"

#include "StjTrgMuDst.h"

class StjTrigger2005MCMaker2 : public StMaker {

public:

  StjTrigger2005MCMaker2(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker)
  : StMaker(name), _file(file), _uDstMaker(uDstMaker)
  { }

  virtual ~StjTrigger2005MCMaker2() { }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjTrigger2005MCMaker2.C,v 1.3 2014/08/06 11:43:24 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}

private:

  TDirectory* _file;

  StMuDstMaker* _uDstMaker;

  StjTrgWriter* _minbWriter;
  StjTrgWriter* _bht1Writer;
  StjTrgWriter* _bht2Writer;
  StjTrgWriter* _bjp1Writer;
  StjTrgWriter* _bjp2Writer;

public:

  Int_t Init()
  {
    StjBEMC* bemc = new StjBEMCMuDst(_uDstMaker, true);

    StjTrgPassCondition* minbPassCondition = new StjTrgPassConditionSoftOnly;
    StjTrg* minbTrg = new StjTrgMuDst(96011, minbPassCondition, _uDstMaker, new StjTrgSoftPass());
    StjTrgPassCondition* minbFillCondition = new StjTrgPassConditionSoftOnly;
    _minbWriter = new StjTrgMBWriter("trgMINB", "trgMINB", _file, minbTrg, minbFillCondition);

    StjTrgPassCondition* bht1PassCondition = new StjTrgPassConditionSoftOnly;
    StjTrgSoft* softbht1 = new StjTrgSoftEtThresholdBHT(bemc, 2.0);
    StjTrg* bht1Trg = new StjTrgMuDst(96201, bht1PassCondition, _uDstMaker, softbht1);
    StjTrgPassCondition* bht1FillCondition = new StjTrgPassConditionSoftOnly;
    _bht1Writer = new StjTrgHTWriter("trgBHT1", "trgBHT1", _file, bht1Trg, bht1FillCondition);

    StjTrgPassCondition* bht2PassCondition = new StjTrgPassConditionSoftOnly;
    StjTrgSoft* softbht2 = new StjTrgSoftEtThresholdBHT(bemc, 3.0);
    StjTrg* bht2Trg = new StjTrgMuDst(96211, bht2PassCondition, _uDstMaker, softbht2);
    StjTrgPassCondition* bht2FillCondition = new StjTrgPassConditionSoftOnly;
    _bht2Writer = new StjTrgHTWriter("trgBHT2", "trgBHT2", _file, bht2Trg, bht2FillCondition);

    StjTrgBEMCJetPatchTowerIdMap* bemcJpTowerMap = new StjTrgBEMCJetPatchTowerIdMap2005();

    StjTrgPassCondition* bjp1PassCondition = new StjTrgPassConditionSoftOnly;
    StjTrgSoft* softbjp1 = new StjTrgSoftEtThresholdBJP(bemc, bemcJpTowerMap, 4.0);
    StjTrg* bjp1Trg = new StjTrgMuDst(96221, bjp1PassCondition, _uDstMaker, softbjp1);
    StjTrgPassCondition* bjp1FillCondition = new StjTrgPassConditionSoftOnly;
    _bjp1Writer = new StjTrgJPWriter("trgBJP1", "trgBJP1", _file, bjp1Trg, bjp1FillCondition);

    StjTrgPassCondition* bjp2PassCondition = new StjTrgPassConditionSoftOnly;
    StjTrgSoft* softbjp2 = new StjTrgSoftEtThresholdBJP(bemc, bemcJpTowerMap, 5.0);
    StjTrg* bjp2Trg = new StjTrgMuDst(96233, bjp2PassCondition, _uDstMaker, softbjp2);
    StjTrgPassCondition* bjp2FillCondition = new StjTrgPassConditionSoftOnly;
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

  ClassDef(StjTrigger2005MCMaker2, 0)

};
