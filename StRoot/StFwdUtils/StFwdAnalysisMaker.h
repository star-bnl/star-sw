#ifndef ST_FWD_ANALYSIS_MAKER_H
#define ST_FWD_ANALYSIS_MAKER_H

#include <map>

#include "StChain/StMaker.h"


class StFwdAnalysisMaker : public StMaker
{
  public:
    StFwdAnalysisMaker();
    ~StFwdAnalysisMaker(){/* nada */};

    int Init();
    int Finish();
    int Make();
    void Clear(const Option_t *opts = "");
    void ProcessFwdTracks();
    void ProcessFwdMuTracks();

    // StEvent analyzed by default
    // call this to analyze the MuDst instead
    void setMuDstInput() { mAnalyzeMuDst = true; }

  protected:

    std::map<TString, TH1*> mHists;
    TH1 * addHist( TH1 * h1){
      mHists[h1->GetName()] = h1;
    }
    TH1* getHist( TString n ){
      if (mHists.count(n))
        return mHists[n];
      LOG_ERROR << "Attempting to access non-existing histogram" << endm;
      return new TH1F( "NULL", "NULL", 1, 0, 1 ); // returning nullptr can lead to seg fault, this fails softly
    }
    bool mAnalyzeMuDst = false;

  ClassDef(StFwdAnalysisMaker, 0);
};

#endif
