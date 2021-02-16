// \class StFcsEventDisplay
// \author Akio Ogawa
//
//   This is FCS event display.
// 
//  $Id: StFcsEventDisplay.h,v 1.6 2021/02/13 21:37:31 akio Exp $
//  $Log: StFcsEventDisplay.h,v $
//  Revision 1.6  2021/02/13 21:37:31  akio
//  #ifdef ___USESTGC___
//
//  Revision 1.5  2020/05/29 18:54:29  akio
//  Adding EPD as PRES
//
//  Revision 1.4  2019/08/01 18:38:22  akio
//  Added STGC
//
//  Revision 1.3  2019/07/02 14:43:16  akio
//  fixed TColor id ovewrite problem, fixed pdf file creation
//
//  Revision 1.2  2019/06/21 18:53:25  akio
//  added run19 version
//
//  Revision 1.1  2018/11/14 16:50:14  akio
//  FCS codes in offline/upgrade/akio
//

#ifndef STAR_StFcsEventDisplay_HH
#define STAR_StFcsEventDisplay_HH

#include "StMaker.h"
#include "StEnumerations.h"

class TCanvas;
class TApplication;
class StFcsDbMaker;
class StFcsCollection;
#ifdef ___USESTGC___
class StStgcDbMaker;
class StStgcCollection;
#endif
class StEpdGeom;

class StFcsEventDisplay : public StMaker{
public: 
    StFcsEventDisplay(const Char_t* name="FcsED");
    ~StFcsEventDisplay();
    Int_t Init();
    Int_t Make();
    Int_t Finish();

    void setFilter(int v){mFilter=v;}
    void setMaxEvents(int v){mMaxEvents=v;}         
    void setFileName(char* file){mFilename=file;} 
    void setRun19(int v) {mRun19=v;}
    void setDebug(int v) {mDebug=1;}

private:
    StFcsDbMaker* mFcsDbMaker=0;
    StFcsCollection* mFcsColl=0;
#ifdef ___USESTGC___
    StStgcDbMaker* mStgcDbMaker=0;
    StStgcCollection* mStgcColl=0;
#endif
    StEpdGeom* mEpdgeo=0;

    TApplication* mApplication=0;
    TCanvas* mCanvas=0;    
    int mDebug=0;
    int mFilter=0;
    int mNEvents=-1;
    int mNAccepted=0;
    int mMaxEvents=20;
    char* mFilename=0;
    int mRun19=0;    
    TFile* mFile=0;
    
    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFcsEventDisplay.h,v 1.6 2021/02/13 21:37:31 akio Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StFcsEventDisplay,0);
};

#endif
