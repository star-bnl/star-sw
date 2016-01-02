#ifndef StPicoD0AnaMaker_h
#define StPicoD0AnaMaker_h

/* **************************************************
 *  A Maker to read a StPicoEvent and StPicoD0Event
 *  simultaneously and do analysis. 
 *
 *  Please write your analysis in the ::Make() function.
 *
 *  Authors:  **Mustafa Mustafa (mmustafa@lbl.gov)
 *
 *  **Code Maintainer
 *
 * **************************************************
 */

#include "TChain.h"
#include "StMaker.h"

class TString;
class TFile;
class TNtuple;
class StPicoD0Event;
class StKaonPion;
class StPicoDstMaker;
class StHFCuts;


class StPicoD0AnaMaker : public StMaker
{
  public:
    StPicoD0AnaMaker(char const * name, char const * inputFilesList, 
        char const * outName,StPicoDstMaker* picoDstMaker);
    virtual ~StPicoD0AnaMaker();

    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    int getEntries() const;

    void setHFCuts(StHFCuts* cuts);    

  private:
    StPicoD0AnaMaker() {}
    void readNextEvent();

    bool isGoodPair(StKaonPion const*) const;

    StPicoDstMaker* mPicoDstMaker;
    StPicoD0Event* mPicoD0Event;

    TString mOutFileName;
    TString mInputFileList;
    TFile* mOutputFile;
    TChain* mChain;
    int mEventCounter;

    StHFCuts* mHFCuts;

    // -------------- USER variables -------------------------
    // add your member variables here. 
    // Remember that ntuples size can be really big, use histograms where appropriate

    ClassDef(StPicoD0AnaMaker, 0)
};

inline int StPicoD0AnaMaker::getEntries() const 
{
  return mChain? mChain->GetEntries() : 0;
}

inline void StPicoD0AnaMaker::readNextEvent()
{
  mChain->GetEntry(mEventCounter++);
}

inline void StPicoD0AnaMaker::setHFCuts(StHFCuts* cuts)   
{ 
  mHFCuts = cuts; 
}

#endif
