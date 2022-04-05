// \class StFmsFpsMaker
// \author Akio Ogawa
//
//   This is analysis for FMS-FPS correlations.
// 
//  $Id: StFmsFpsMaker.h,v 1.7 2016/06/07 17:58:06 akio Exp $
//  $Log: StFmsFpsMaker.h,v $
//  Revision 1.7  2016/06/07 17:58:06  akio
//  Added more comments on setReadMuDST()
//  Added setMeanVertexZ(float v)
//  Added protection for setting >2.0 for setMaxDistanceToAssociate()
//  Added c++ style data member initializations
//  Added project() and distance() as private member function (from global scope)
//
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
    
    //Read MuDST hits if available, and update FPS hits in StEvent using current DB values
    //If this is set to 0 (default), use hits as is (new DB values will NOT be applied)
    //You only need this when you want to apply new calibration from DB to overwrite
    //old calibration which was in place when the Mudst is produced. This is the case
    //typically when you are running on FastOffline Mudst to calibrate FPS.
    void setReadMuDST(int v=1){mReadMuDST=v;}         

    //mean z vertex to be used for interpration between FMS and vertex
    void setMeanVertexZ(float v) {mMeanVertexZ=v;}

    //max distance to associate FPS slat to FMS point
    void setMaxDistanceToAssociate(float v) {if(v>0.0 && v<2.0) mMaxDistanceToAssociate=v;}

    //0=take closest slat, 1=take sum of all slat associated
    void setPidMethod(int v) {mPidMethod=v;}
    
private:
    StFmsDbMaker* mFmsDbMaker=0;
    StFmsCollection* mFmsColl=0;
    
    int mReadMuDST=0; 
    void readMuDST();
    
    float mMeanVertexZ=0.0; //[cm]
    float mMaxDistanceToAssociate=2.0; //[cm] making this >2cm causes more than 4 slats to associate with FMS hit!
    int mPidMethod=1;
    
    void corrFmsFps();
    void pid(int opt=0);
    void isolationCone();

    Float_t project(float x, float z, float zp, float vz);
    Float_t distance(float x, float y, float x0, float y0, float xw, float yw);    

    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFmsFpsMaker.h,v 1.7 2016/06/07 17:58:06 akio Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StFmsFpsMaker,0);
};

#endif
