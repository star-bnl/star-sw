// $Id: StFcsWaveformFitMaker.h,v 1.2 2021/05/30 21:26:53 akio Exp $
// $Log: StFcsWaveformFitMaker.h,v $
// Revision 1.2  2021/05/30 21:26:53  akio
// Added mFitDrawOn=2 for resetting mHitIdx for end of page, instead of each event
// Increased accepted tb range as hit, need further tuning
//
// Revision 1.1  2021/03/30 13:40:13  akio
// FCS code after peer review and moved from $CVSROOT/offline/upgrades/akio
//
// Revision 1.13  2021/02/25 21:56:28  akio
// Int_t -> int
//
// Revision 1.12  2021/02/25 19:24:33  akio
// Modified for STAR code review (Hongwei)
//
// Revision 1.11  2021/02/13 13:43:09  akio
// Some cleanups
//
// Revision 1.10  2021/02/13 03:11:44  akio
// Added TGraphAsymmErrorsWithReset and separate resetGraph() and simplify getGraph(idx=-1)
//
// Revision 1.9  2021/02/10 04:11:24  akio
// Code from David on improving TGraphArray housekeeping
//
// Revision 1.8  2021/01/26 18:43:14  akio
// Include David's fitter & holding of TGA for the event. Separate drawFit().
//
// Revision 1.7  2021/01/11 14:46:18  akio
// Adding another tail functional form, adding fit drawing to PDF
//
// Revision 1.6  2021/01/02 21:38:03  akio
// Fix some minor issues
//
// Revision 1.5  2021/01/02 21:04:54  akio
// added 11 for ped sub
//
// Revision 1.4  2020/09/17 19:01:52  akio
// fix bugs David reported
//
// Revision 1.3  2020/09/16 14:52:46  akio
// Use of TGraphAsymmErrors to deal with adc saturation
// Impelment gausFit() for gaussian fit
//
// Revision 1.2  2020/09/03 20:15:49  akio
// Adding res array for peak height/position/sigma and chi2
//
// Revision 1.1  2020/08/19 19:51:08  akio
// Initial version
//

#ifndef STROOT_STFCSWAVEFORMFITMAKER_STFCSWAVEFORMFITMAKER_H_
#define STROOT_STFCSWAVEFORMFITMAKER_STFCSWAVEFORMFITMAKER_H_

#include "StMaker.h"
#include "StFcsPulseFit.h"

class StFcsCollection;
class StFcsHit;
class StFcsDb;
class TGraphAsymmErrors;
class TGraph;
class TCanvas;

class StFcsWaveformFitMaker : public StMaker {
public:
    
    StFcsWaveformFitMaker(const char* name = "StFcsWaveformFitMaker");
    ~StFcsWaveformFitMaker();
    int InitRun(int runNumber);
    int Make();
    int Finish(); 
    void Clear(Option_t* option = "");
    
    void setDebug(int v=1)        {SetDebug(v);}
    void setEnergySelect(int v)   {mEnergySelect=v;}
    void setCenterTimeBins(int v, int min=0, int max=512) {mCenterTB=v; mMinTB=min; mMaxTB=max;}
    void setAdcSaturation(int v)  {mAdcSaturation=(double)v;}
    void setError(double v)       {mError=v;}
    void setErrorSaturated(int v) {mErrorSaturated=v;}
    void setMinAdc(int v)         {mMinAdc=v;}
    void setTail(int v)           {mTail=v;}
    void setMaxPeak(int v)        {mMaxPeak=v;}
    void setPedTimeBins(int min, int max) {mPedMin=min; mPedMax=max;}

    // Create or Reset TGraphAsymmErrors (at mHitIdx)
    TGraphAsymmErrors* resetGraph();

    // Return TGraphAsymmErrors at idx
    // if idx<0 (default) it returns "current" which is idx=mHitIdx-1
    // makeTGraphAsymmErrors() makes all at idx=mHitIdx=0 except when 
    // mFitDrawOn=1, then keep incrementing mHitIdx to hold them for a event
    // mFitDrawOn=2, then keep incrementing mHitIdx to hold them until new page
    TGraphAsymmErrors* getGraph(int idx=-1);

    //measuring fit time
    void setMeasureTime(char* file) {mMeasureTime=file;}
    TH1F* mTime;

    //create makeTGraphAsymmErrors from given timebin and adc data
    //with asymmetroc errors when adc is saturated
    //Places graph at index 0 of internal TClonesArray and will be deleted in destructor
    TGraphAsymmErrors* makeTGraphAsymmErrors(int n, double* t, double* adc);    
    TGraphAsymmErrors* makeTGraphAsymmErrors(TGraph* g);
    TGraphAsymmErrors* makeTGraphAsymmErrors(StFcsHit* hit);//Names graphs by detid and chan id
    TGraphAsymmErrors* makeTGraphAsymmErrors(TH1* hist);

    // Perfom the analysis using selected method
    // res[0] pulse integral [ch]
    // res[1] pulse peak height [ch]
    // res[2] pulse peak position [timebin]
    // res[3] pulse peak sigma [timebin]
    // res[4] pulse fit chi2
    // res[5] # of peaks
    float analyzeWaveform(int select, TGraphAsymmErrors* g, float* res, TF1*& f, float ped=0.0); 
    float analyzeWaveform(int select, int n, double* t, double* adc, float* res, TF1*& f, float ped=0.0); 
    float analyzeWaveform(int select, TGraph* g, float* res, TF1*& f,float ped=0.0); 
    float analyzeWaveform(int select, StFcsHit* hit, float* res, TF1*& f,float ped=0.0); 

    //actual analysis methods
    float sum8    (TGraphAsymmErrors* g, float *res);         //! mEnergySelect=1
    float sum16   (TGraphAsymmErrors* g, float *res);         //! mEnergySelect=2
    float highest (TGraphAsymmErrors* g, float *res);         //! mEnergySelect=3
    float highest3(TGraphAsymmErrors* g, float *res);         //! mEnergySelect=4
    float gausFit (TGraphAsymmErrors* g, float *res, TF1*& f, float ped=0.0); //! mEnergySelect=10
    float gausFitWithPed (TGraphAsymmErrors* g, float *res, TF1*& f);         //! mEnergySelect=11
    float PulseFit(TGraphAsymmErrors* g, float* res, TF1*& f);                //! mEnergySelect=12

    //Pedestal Analysis methods
    //res[0] pedestal Value
    //res[1] pedestal Error
    //res[2] pedestal Chi2/NDF
    float PedFit(TGraphAsymmErrors* g, float* res, TF1*& f );//! mEnergySelect=21
   
    //LED Analysis methods
    // res[1] pulse peak height [ch]
    // res[2] pulse peak position [timebin]
    // res[3] pulse peak sigma [timebin]
    // res[4] pulse fit chi2
    // res[5] pedestal Value
    // res[6] pedestal Sigma
    // res[7] pedestal Chi2/NDF
    float LedFit( TGraphAsymmErrors* g, float* res, TF1*& f);//! mEnergySelect=31 (TF1 is for LED pulse fit)

    //pulse shape functions
    double pulseShape(double* x, double* p);
    double multiPulseShape(double* x, double* p);

    //Draw fits    
    void setMaxPage(int v){mMaxPage=v;}         
    void setSkip(int v){mSkip=v;}         
    void setFileName(char* file, int maxpage=25, int skip=5){mFilename=file; mMaxPage=maxpage; mSkip=skip;} 
    void setFitDrawOn(int v=1) {mFitDrawOn=v;}  //=1 to keep for a event, =2 for a page
    void setFitFilter(char* filter) {mFilter=filter; mFitDrawOn=2;}

    //Draw from David
    StFcsPulseFit* davidFitter(){return mPulseFit;}
    static int GenericPadPos(int value, int Nvals, int PadNums );
    static int PadNum4x4(int det, int col, int row);
    void drawRegion(int det, int col_low, int row_low, int col_high, int row_high, int event=0);
    void drawEvent(int det, int event=0);
    void printArray() const;

 protected:
    TClonesArray mChWaveData;  //Contains all graph data
    void drawFit(TGraphAsymmErrors* g, TF1* func);
    StFcsPulseFit* mPulseFit;

 private:
    StFcsDb* mDb=0;                    //! pointer to fcsDb
    StFcsCollection* mFcsCollection=0; //! pointer to fcsCollection

    unsigned int mHitIdx = 0; //running index for the TClonesArray

    //Figures out error to set on the TGraphAsymmErrors for a given point and adc
    void setTGraphAsymmErrors(TGraphAsymmErrors* gae, const int &i, const double &adc);

    char *mMeasureTime=0;                //! output file for measuring fitting time

    int mEnergySelect=0;                 //! 0=MC (straight from dE), >0 see above
    int mCenterTB=50;                    //! center timebin for triggered crossing
    int mMinTB=0;                        //! center timebin for triggered crossing
    int mMaxTB=512;                      //! center timebin for triggered crossing

    double mError=1.0;                   //! error to be used for none saturated timebin 
    double mErrorSaturated=1000.0;       //! upper error to be used for saturated timebin 
    double mAdcSaturation=4000.0;        //! above this, upper error will be enlarged
 
    int mPedMin=-1;                      //! min TB for ped
    int mPedMax=-1;                      //! max TB for ped

    //gausFit
    int mMinAdc=5;                       //! minimum adc to be a peak
    int mTail=2;                         //! pulse tail shape (0=none, 1=summer2020) 
    int mMaxPeak=5;                      //! max # of peak for trying to fit

    //Drawing fits
    TCanvas* mCanvas=0; 
    int mPage=0;
    int mPad=0;
    int mMaxPage=25;
    int mSkip=2;
    char* mFilename=0;
    char* mFilter=0;
    char mDetName[100];
    int mFitDrawOn=0;   //! If set, it will also create a new TGraphAsymmErrors for each hit

    virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag " __DATE__ " " __TIME__ ; return cvs;}

    ClassDef(StFcsWaveformFitMaker, 3)
};
#endif  // STROOT_STFCSWAVEFORMFITMAKER_STFCSWAVEFORMFITMAKER_H_
