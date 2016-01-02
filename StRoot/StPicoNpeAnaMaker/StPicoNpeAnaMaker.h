#ifndef StPicoNpeAnaMaker_h
#define StPicoNpeAnaMaker_h

/* **************************************************
 *  A Maker to read a StPicoEvent and StPicoNpeEvent
 *  simultaneously and do analysis. 
 *
 *  Please write your analysis in the ::Make() function.
 *
 *  Authors:  Xin Dong        (xdong@lbl.gov)
 *            Michael Lomnitz (mrlomnitz@lbl.gov)
 *            Mustafa Mustafa (mmustafa@lbl.gov)
 *            Jochen Thaeder  (jmthader@lbl.gov)   
 *
 * **************************************************
 */
 

#include "TChain.h"
#include "StMaker.h"
#include "THnSparse.h"
#include "TH2F.h"

class TString;
class TFile;
class TNtuple;
class StPicoNpeEvent;
class StElectronTrack;
class StElectronPair;
class StPicoDstMaker;
class StPicoTrack;

class StPicoNpeAnaMaker : public StMaker
{
  public:
    StPicoNpeAnaMaker(char const * name, char const * inputFilesList, 
        char const * outName,StPicoDstMaker* picoDstMaker);
    virtual ~StPicoNpeAnaMaker();

    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    int getEntries() const;

    
  private:
    StPicoNpeAnaMaker() {}
    void readNextEvent();

    bool isGoodPair(StElectronPair const*) const;
    
    StPicoDstMaker* mPicoDstMaker;
    StPicoNpeEvent* mPicoNpeEvent;

    TString mOutFileName;
    TString mInputFileList;
    TFile* mOutputFile;
    TChain* mChain;
    int mEventCounter;

    
    // -------------- USER variables -------------------------
    // add your member variables here. 
    // Remember that ntuples size can be really big, use histograms where appropriate
    
    
    
    
    ClassDef(StPicoNpeAnaMaker, 0)
};

inline int StPicoNpeAnaMaker::getEntries() const 
{
  return mChain? mChain->GetEntries() : 0;
}

inline void StPicoNpeAnaMaker::readNextEvent()
{
  mChain->GetEntry(mEventCounter++);
}

#endif
