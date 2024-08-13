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
    void setLocalOutputFile( TString f ) { mLocalOutputFile = f; }

  protected:

    /**
     * @brief Map of <name (TString), histogram>
     * 
     */
    std::map<TString, TH1*> mHists;

    /**
     * @brief Get the Hist object from the map
     *  - Additional check and safety for missing histograms
     * @param n Histogram name
     * @return TH1* histogram if found, otherwise a 'nil' histogram with one bin
     */
    TH1* getHist( TString n ){
      if (mHists.count(n))
        return mHists[n];
      LOG_ERROR << "Attempting to access non-existing histogram" << endm;
      return new TH1F( "NULL", "NULL", 1, 0, 1 ); // returning nullptr can lead to seg fault, this fails softly
    }

    /**
     * @brief Control whether the analysis uses StEvent (default) or MuDst as input
     * 
     */
    bool mAnalyzeMuDst = false;
    TString mLocalOutputFile;

  ClassDef(StFwdAnalysisMaker, 0);
};

#endif
