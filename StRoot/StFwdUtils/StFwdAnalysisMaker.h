#ifndef ST_FWD_ANALYSIS_MAKER_H
#define ST_FWD_ANALYSIS_MAKER_H

#include "StChain/StMaker.h"
#include "TVector3.h"
// ROOT includes
#include "TNtuple.h"
#include "TTree.h"
// STL includes
#include <vector>
#include <memory>

class StFwdTrack;


class StFwdAnalysisMaker : public StMaker {

    ClassDef(StFwdAnalysisMaker, 0);

  public:
    StFwdAnalysisMaker();
    ~StFwdAnalysisMaker(){/* nada */};

    int Init();
    int Finish();
    int Make();
    void Clear(const Option_t *opts = "");
    void ProcessFwdTracks();
    void ProcessFwdMuTracks();

  private:
  protected:
};

#endif
