/***************************************************************************
 *
 * $Id: StEbye2ptMaker.h,v 1.1.1.1 2000/02/05 03:15:21 jgreid Exp $
 *
 * Author: Jeff Reid, UW
 *         with design advice from Thomas Ullrich, Yale
 *
 ***************************************************************************
 *
 * Description:  This is a maker to generate 2-point correlation
 *                spaces (App) from p_t spectra
 *
 ***************************************************************************
 *
 * $Log: StEbye2ptMaker.h,v $
 * Revision 1.1.1.1  2000/02/05 03:15:21  jgreid
 * Two particle correlationspace generation package
 *
 *
 **************************************************************************/
#ifndef StEbye2ptMaker_HH
#define StEbye2ptMaker_HH
#include "StMaker.h"
#include "TH2.h"
#include "TFile.h"

class TFile;
class TH2F;
class StEvent;
class StRun;

class StEbye2ptMaker : public StMaker {
public:

    StEbye2ptMaker(const Char_t *name="EbyE 2-pt");
    virtual ~StEbye2ptMaker();
    
    void Clear(Option_t *option="");
    Int_t Init();
    Int_t Make();
    Int_t Finish();
    
    virtual const char *GetCVS() const
    {static const char cvs[]="$Id: StEbye2ptMaker.h,v 1.1.1.1 2000/02/05 03:15:21 jgreid Exp $ built "__DATE__" "__TIME__ ; return cvs;}
    
private:

    // arrays for p_t values that pass track cuts
    Double_t *mThisEventPlus,*mThisEventMinus;
    Double_t *mPreviousEventPlus,*mPreviousEventMinus;

    // histograms for sibling and mixed pairs
    //   (+.+, +.-, -.+, -.-)
    TH2F *mSibPP,*mSibPM,*mSibMP,*mSibMM;
    TH2F *mMixPP,*mMixPM,*mMixMP,*mMixMM;

    Int_t mixEvents();
    Int_t processEvent(StEvent &event);
  
    ClassDef(StEbye2ptMaker,1)

};
#endif
