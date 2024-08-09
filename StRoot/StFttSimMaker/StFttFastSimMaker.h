
#ifndef ST_FTT_FASTER_SIM_MAKER_H
#define ST_FTT_FASTER_SIM_MAKER_H

#include "StChain/StMaker.h"
#include <vector>

#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TNtuple.h"


class StFttFastSimMaker : public StMaker {
  public:
    StFttFastSimMaker(const Char_t *name = "fttSim");
    ~StFttFastSimMaker() {}
    Int_t Make();
    int Init();
    int Finish() {
        return kStOk;
    }

    ClassDef(StFttFastSimMaker, 0);
};


#endif