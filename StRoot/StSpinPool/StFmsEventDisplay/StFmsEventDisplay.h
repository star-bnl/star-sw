// \class StFmsEventDisplay
// \author Akio Ogawa
//
//   This is FMS-FPS event display.
// 
//  $Id: StFmsEventDisplay.h,v 1.3 2016/01/20 19:56:39 akio Exp $
//  $Log: StFmsEventDisplay.h,v $
//  Revision 1.3  2016/01/20 19:56:39  akio
//  *** empty log message ***
//
//  Revision 1.1  2015/10/20 19:55:51  akio
//  Initial version of FMS event display
//
//

#ifndef STAR_StFmsEventDisplay_HH
#define STAR_StFmsEventDisplay_HH

#include "StMaker.h"
#include "StEnumerations.h"

class TCanvas;
class TApplication;
class StFmsDbMaker;
class StFmsCollection;

class StFmsEventDisplay : public StMaker{
public: 
    StFmsEventDisplay(const Char_t* name="FmsED");
    ~StFmsEventDisplay();
    Int_t Init();
    Int_t Make();
    Int_t Finish();

    void setFilter(int v){mFilter=v;}
    void setMaxEvents(int v){mMaxEvents=v;}         
    void setFileName(char* file){mFilename=file;} 
    
private:
    StFmsDbMaker* mFmsDbMaker;
    StFmsCollection* mFmsColl;
    TApplication* mApplication;
    TCanvas* mCanvas;    
    int mFilter;
    int mNEvents;
    int mNAccepted;
    int mMaxEvents;
    char* mFilename;
    TFile* mFile;
    
    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFmsEventDisplay.h,v 1.3 2016/01/20 19:56:39 akio Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StFmsEventDisplay,0);
};

#endif
