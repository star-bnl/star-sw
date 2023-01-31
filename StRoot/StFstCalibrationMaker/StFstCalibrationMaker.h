/***************************************************************************
* $Id: StFstCalibrationMaker.h$
*
* Author: Shenghui Zhang,Oct. 2021
****************************************************************************
* Description: 
* Calculates mean pedestal, rms noise and cm noise by histogram methods,
* and produce two data files and several QA plots.
****************************************************************************
****************************************************************************
* StFstCalibrationMaker.h,v 1.0
* Revision 1.0 2021/10/15 Shenghui Zhang
* Initial version
****************************************************************************/

#ifndef StFstCalibrationMaker_h
#define StFstCalibrationMaker_h

#include <string>
#include "StMaker.h"
#include "StEvent/StFstConsts.h"

class StFstDb;
class TH2S;

class StFstCalibrationMaker : public StMaker {
public:
    // constructors
    StFstCalibrationMaker( const char* name = "fst_calib" );
    // deconstructor
    ~StFstCalibrationMaker();

    Int_t Init();
    Int_t InitRun(Int_t runNumber);
    Int_t Make();
    Int_t Finish();

    // modifiers
    void setTimeBinMask( short mask = 0xFF );
    void setRunHist(bool flag =false);
    void setPedCutFlag(bool flag =false);
    void setPedCut(float pedCut = 3.0);

    // ped, rms, ran, gain
    struct pedNoiseData_t {
        Int_t n;
        Float_t ped;
        Float_t rms;
        Float_t ran;

        pedNoiseData_t( int nIn=0, float p=0, float r=0, float d=0 ) : n(nIn), ped(p), rms(r), ran{d} { /* */ };
    };
    typedef std::vector< pedNoiseData_t > pedNoiseDataVec_t;
    pedNoiseDataVec_t mPedVec;
    // cmn
    struct cmNoiseData_t {
        Int_t n;
        Float_t cmn;

        cmNoiseData_t( int nIn=0, float c=0 ) : n(nIn), cmn(c) { /* */ };
    };
    typedef std::vector< cmNoiseData_t > cmNoiseDataVec_t;
    cmNoiseDataVec_t mCmnVec; 

protected:
    // mask for which time bins to save
    Short_t mTimeBinMask;
    // whether or not to run Histogram method
    Bool_t mRunHist;
    // whether or not put constraints 
    Bool_t  mDoPedCut;
    Float_t mPedCut;
    //containers for histogram calculation method
    std::vector< TH1S* > mHistPedVec;
    std::vector< TH1F* > mHistCmnVec;
    std::vector< TH1F* > mHistRanVec;

    //containers for 1st loop's pedestal/rms values
    std::vector< float > mPedVec1stLoop;
    std::vector< float > mRmsVec1stLoop;
    std::vector< float > mRanVec1stLoop;

    //containers for mathematical calculation mathod
    std::vector< double > mMathPedVec;
    std::vector< double > mMathRmsVec;   
    std::vector< int > mMathCouVec;

    std::vector< double > mMathRanVec;   
    std::vector< double > mMathPedRanVec;
    std::vector< double > mMathRmsRanVec;   
    std::vector< int > mMathCouRanVec;

    Int_t evtIdx; 
   
    // functions that actually do the saving
    Int_t saveToFile();

    // has finished
    Bool_t mHasFinished;

    // Db
    StFstDb *mFstDb;
    // mapping
    typedef std::vector< int > MappingVec_t; //Channel elec. index, geometry ID
    MappingVec_t mMappingVec;
    
    // for saving to file
    Bool_t mDoOutput;
    TH1F *hist_meanPed[kFstNumTimeBins];     // mean pedestal = pedestal histogram -> GetMean()
    TH1F *hist_rmsPed[kFstNumTimeBins];      // standard deveriation = pedestal histogram -> GetRMS()
    TH1F *hist_cmNoise[kFstNumTimeBins];     // common mode noise per APV chip
    TH1F *hist_ranNoise[kFstNumTimeBins];    // standard deveriation = pedestal histogram -> GetRMS()
    TH1F *hist_sumPed[kFstNumTimeBins];      // summary pedestal over all channels
    TH1F *hist_sumRms[kFstNumTimeBins];      // summary rms noise over all channels
    TH1F *hist_sumCmn[kFstNumTimeBins];      // summary common mode noise over all channels
    TH1F *hist_sumRan[kFstNumTimeBins];      // summary common mode noise over all channels
    TH2S *hist_adcSpectrum[kFstNumTimeBins]; // ADC spectrum over all channels
    static const string sectionLabel[72];

private:
    
   ClassDef(StFstCalibrationMaker,0);
}; 

// modifiers
inline void StFstCalibrationMaker::setTimeBinMask( short mask ) { mTimeBinMask = mask; };
inline void StFstCalibrationMaker::setRunHist(bool flag)        { mRunHist = flag; };
inline void StFstCalibrationMaker::setPedCutFlag(bool flag)     { mDoPedCut = flag; };
inline void StFstCalibrationMaker::setPedCut(float pedCut)      { mPedCut = pedCut; };
/*inline const char *StFstCalibrationMaker::GetCVS() const {
   static const char cvs[] = "Tag $Name:  $ $Id: StFstCalibrationMaker.h,v 1.8 2014/07/29 20:13:30 ypwang Exp $ built "__DATE__" "__TIME__ ;
   return cvs;
}*/
#endif
