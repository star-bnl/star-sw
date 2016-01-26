// \class StFmsFpsMaker
// \author Akio Ogawa
//
//   This is analysis for FMS-FPS correlations.
// 
//  $Id: StFmsFpsMaker.h,v 1.6 2016/01/26 19:53:14 akio Exp $
//  $Log: StFmsFpsMaker.h,v $
//  Revision 1.6  2016/01/26 19:53:14  akio
//  Drop QA/alignment histograms and just do the analysis/filling StEvent structure
//  Offline QA and Alignment histograms will be moved to StRoot/StSpinPool/StFmsOfflineQaMaker
//
//  Revision 1.5  2015/12/08 17:00:03  akio
//  updates
//
//  Revision 1.4  2015/10/29 21:22:01  akio
//  more cuts for QA
//
//  Revision 1.3  2015/10/21 15:51:01  akio
//  Add more QA for debugging FMS reconstruciton code
//
//  Revision 1.2  2015/09/18 18:50:06  akio
//  cleaning up and adding QA histos
//
//  Revision 1.1  2015/09/02 14:56:12  akio
//  Initial version of FMS-FPS correation analysis
//

#ifndef STAR_StFmsFpsMaker_HH
#define STAR_StFmsFpsMaker_HH

#include "StMaker.h"
#include "StEnumerations.h"

class StFmsDbMaker;
class StFmsCollection;

class StFmsFpsMaker : public StMaker{
public: 
    StFmsFpsMaker(const Char_t* name="FmsFps");
    ~StFmsFpsMaker();
    Int_t Init();
    Int_t Make();
    // Int_t Finish();
    
    //Read MuDST hits if available, and update FPS hits in StEvent using current DB values
    //if this is set to 0 (default), use hits as is (new DB values will NOT be applied)
    void setReadMuDST(int v=1){mReadMuDST=v;}         

    //max distance to associate FPS slat to FMS point
    void setMaxDistanceToAssociate(float v) {mMaxDistanceToAssociate=v;}

    //0=take closest slat, 1=take sum of all slat associated
    void setPidMethod(int v) {mPidMethod=v;}
    
private:
    StFmsDbMaker* mFmsDbMaker;
    StFmsCollection* mFmsColl;
    
    int mReadMuDST; 
    void readMuDST();
    
    int mMaxDistanceToAssociate;
    int mPidMethod;
	
    void corrFmsFps();
    void pid(int opt=0);
    void isolationCone();
    
    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFmsFpsMaker.h,v 1.6 2016/01/26 19:53:14 akio Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StFmsFpsMaker,0);
};

#endif
