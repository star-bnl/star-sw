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
#include "StFcsPulseAna.h"

#include "TH2.h"

class StFcsCollection;
class StFcsHit;
class StFcsDb;
class StFcsDbPulse;
class TGraphAsymmErrors;
class TGraph;
class TCanvas;

class StFcsWaveformFitMaker : public StMaker {
public:
    
    StFcsWaveformFitMaker(const char* name = "StFcsWaveformFitMaker");
    ~StFcsWaveformFitMaker();
    virtual int Init();
    virtual int InitRun(int runNumber);
    virtual int Make();
    virtual int Finish();
    virtual void Clear(Option_t* option = "");
    
    void setDebug(int v=1)        {SetDebug(v);}
    void setTest(int v);           //!< Set test level. Intended to be used for single files. Output file name can be changed with #writeFile()
    void setEnergySelect(int ecal=10, int hcal=10, int pres=1) {mEnergySelect[0]=ecal; mEnergySelect[1]=hcal; mEnergySelect[2]=pres;}
    void setCenterTimeBins(int v, int min=0, int max=512) {mCenterTB=v; mMinTB=min; mMaxTB=max;}
    void setAdcSaturation(int v)  {mAdcSaturation=(double)v;}
    void setError(double v)       {mError=v;}
    void setErrorSaturated(int v) {mErrorSaturated=v;}
    void setMinAdc(int v)         {mMinAdc=v;}
    void setTail(int v);
    void setMaxPeak(int v)        {mMaxPeak=v;}
    void setPedTimeBins(int min, int max) {mPedMin=min; mPedMax=max;}

    TGraphAsymmErrors* resetGraph();    //! Create or Reset TGraphAsymmErrors (at mHitIdx)
  
    // Return TGraphAsymmErrors at idx
    // if idx<0 (default) it returns "current" which is idx=mHitIdx-1
    // makeTGraphAsymmErrors() makes all at idx=mHitIdx=0 except when 
    // mFitDrawOn=1, then keep incrementing mHitIdx to hold them for a event
    // mFitDrawOn=2, then keep incrementing mHitIdx to hold them until new page
    TGraphAsymmErrors* getGraph(int idx=-1);
    TGraphAsymmErrors* getGraph(int det, int id);

    //measuring fit time
    void setMeasureTime(char* file) {mMeasureTime=file;}
    TH1F* mTime = 0;

    //stage0 peak algo study
    TH2F* mTimeIntg[4];

    //create makeTGraphAsymmErrors from given timebin and adc data
    //with asymmetric errors when adc is saturated
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
    float PulseFit1(TGraphAsymmErrors* g, float* res, TF1*& f);                //! mEnergySelect=12
    float PulseFit2(TGraphAsymmErrors* g, float* res, TF1*& f);                //! mEnergySelect=13

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
  
    //Draw fits    
    void setMaxPage(int v){mMaxPage=v;}         
    void setSkip(int v){mSkip=v;}         
    void setFileName(char* file, int maxpage=25, int skip=5){mFilename=file; mMaxPage=maxpage; mSkip=skip;} 
    void writeFile(std::string filename);       //!< Use to change the name of the file where test histograms will be saved
    void setFitDrawOn(int v=1) {mFitDrawOn=v;}  //=1 to keep for a event, =2 for a page
    void setFitFilter(char* filter) {mFilter=filter; mFitDrawOn=2;}

    //Draw from David
    int centerTB()const{return mCenterTB;}
    void setDavidFitter(StFcsPulseAna* v){if(mPulseFit==0){mPulseFit=v;}}
    StFcsPulseAna* davidFitter(){return mPulseFit;}
    StFcsPulseAna* InitFitter(Double_t ped=0);     //! Sets up basic values needed by #StFcsPulseAna
    static int GenericPadPos(int value, int Nvals, int PadNums );
    static int PadNum4x4(int det, int col, int row);
    void drawRegion(int det, int col_low, int row_low, int col_high, int row_high, int event=0);
    void drawEvent(int det, int event=0);
    void printArray() const;
    void drawFitter(Option_t* opt){ if(mPulseFit!=0){mPulseFit->Draw(opt);} }
    void drawCh(UInt_t detid, UInt_t ch) const;

 protected:
    TClonesArray mChWaveData;  //Contains all graph data
    void drawFit(TGraphAsymmErrors* g, TF1* func);
    StFcsPulseAna* mPulseFit;

    /**@brief Variable to use when testing StFcsWaveformFitMaker algorithms

       Self contained analysis for testing various components/functions of #StFcsWaveformFitMaker
       
       - 0 = no testing
       - 1 = test DEP algorithm
       - 2 = test PeakAna vs. gausFit
       - 3 = test PulseFit1 picking sum method
       - 4 = test PulseFit1 all data with peaks
       - 5 = test timing of gausFit() vs. PulseFit1()
       - 6 = like test==3 but for PulseFit2()
       - 7 = test PulseFit2() for overall quality doesn't include preshower
    */
    int mTest = 0;

    TFile* mOutFile;//Root output file for testing
    //For testing Dep0 algo (mTest==1)
    TH2F* mH2_Dep0DepMod[3];
    TH2F* mH2_Sum8Dep0[3];
    TH2F* mH2_Sum8DepMod[3];
    //For testing number of peaks finding (mTest==2||mTest==3||mTest==6)
    TH1F* mH1_NPeaksAkio = 0;                       //Number of peaks found by gausFit
    TH1F* mH1_NPeaksFilteredAkio = 0;               //Number of peaks found by gausFit for signals that had a triggered crossing
    TH2F* mH2F_AdcTbAkio[6];     //Adc vs. Tb for different number of peaks Akio method
    TH2F* mH2F_AdcTbMine[6];   //Adc vs. Tb for different number of peaks my method
    TH2F* mH2F_AdcTbValidPeak[7];//Adc vs. Tb from my algorithm that had a peak at centerTb (Need an extra one for non-valid peaks)
    TH2F* mH2F_SumFitvSumWin[6]; //Sum from Akio's Fit function vs. Sum from my found peak window for different number of peaks
    TH2F* mH2F_APeakvMPeak[6];   //PeakLocations from Akio vs. Mine
    TH1F* mH1F_PeakStart[6];     //PeakWindow Starting x-values
    TH1F* mH1F_PeakEnd[6];       //PeakWindow Ending x-values
    TH1F* mH1_PeakTiming = 0;                       //Timing for just peak finding.

    TH1F* mH1F_NPeaks[7];           //Number of peaks found by peak finder
    TH1F* mH1F_NPeaksFiltered[7];   //Number of peaks for cases where a valid peak was found
    TH2F* mH2_NPeakvsPeakXdiff = 0; //Number of peaks vs. Peak X diff
    TH2F* mH2_NPeakvsPeakYratio = 0;//Number of peaks vs. Peak Y ratio
    TH1F* mH1_VOverlap = 0;         //Value of overlap
    TH2F* mH2_NOvsNPeaks = 0;       //NO (Number of overlaps) vs. Number of peaks
    TH2F* mH2_VvsNOverlap = 0;      //Compare value for that peak comparison vs. Overlap index
    TH2F* mH2F_NOvsId[6];        //NO (number of overlaps) vs channel id for the 6 detector ids
    TH1F* mH1F_Res0[7];          //Histogram of all res[0] regardless of method
    TH1F* mH1F_Res0Zoom[7];      //Histogram of all res[0] with finer bining at low end
    TH1F* mH1F_Sum8Res0[7];      //Histogram of "res[0]" using sum8 regardless of method called
    TH1F* mH1F_Sum8Res0Zoom[7];  //same as above histogram with finer bining at low end
    TH1F* mH1F_FitRes0[7];       //Histogram of "res[0]" from fit regardless of method called
    TH1F* mH1F_FitRes0Zoom[7];   //same as above histogram with finer bining at low end
    TH2F* mH2F_Sum8vFit[7];      //Histograms of Fit res[0] vs. sum8 res[0]     
    TH1F* mH1_TimeFitPulse = 0;  //Histogram to time how long just the fitting takes in PulseFit()

    //For testing peak height vs. sigma
    TH2F* mH2_HeightvsSigma = 0;                //Histogram of all fitted peak heights vs. their sigma
    TH2F* mH2_HeightvsSigmaTrig = 0;            //Histogram of height of fitted peaks in triggered crossing vs. their sigma
    TH1F* mH1_ChiNdf = 0;                       //Histogram of chi^2/ndf for all fits
    TH2F* mH2_HeightvsChiNdf = 0;               //Histogram of height vs. chi^2/ndf for all fits
    TH2F* mH2_MeanvsChiNdf = 0;                 //Histogram of height vs. chi^2/ndf for all fits
    TH2F* mH2_SigmavsChiNdf = 0;                //Histogram of chi^2/ndf for all fits

    TH1F* mH1_PeakTimingGaus = 0;               //Histogram to test timing of gausFit()
    TH1F* mH1_PeakTimingPuls = 0;               //Histogram to test timing of PulseFit()
    TH2F* mH2_PeakTimingCompare = 0;            //Histogram to test timing between gausFit() and PulseFit()

    void SetupDavidFitterMay2022(Double_t ped=0);    //! This special function is used to set all the parameters for #StFcsPulseAna based on cosmic and Run 22 data. It is intended to be used only for Run 22 data
    int PeakCompare(const PeakWindow& pwin1, const PeakWindow& pwin2 ); //Compare if two peaks overlap and return a bit vector of tests passed/failed for comparing pwin1 to pwin2. 0 means all tests passed and pwin1 does not overlap with pwin2
    int NPeaksPre2Post1(int& trigidx, Double_t& xmin, Double_t& xmax) const;//xmin and xmax will be the range of the pre-crossing -2 and post-crossing +1 peaks. trigidx is needed to pick up the triggered crossing in the new number of peaks

 private:
    StFcsDb* mDb=0;                    //! pointer to fcsDb
    StFcsDbPulse* mDbPulse = 0;        //! pointer to fcsPulse
    StFcsCollection* mFcsCollection=0; //! pointer to fcsCollection

    unsigned int mHitIdx = 0; //running index for the TClonesArray

    //Figures out error to set on the TGraphAsymmErrors for a given point and adc
    //void setTGraphAsymmErrors(TGraphAsymmErrors* gae, const int &i, const double &adc);
    char *mMeasureTime=0;                //! output file for measuring fitting time

    int mEnergySelect[3];                //! 0=MC (straight from dE), >0 see above
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
    int mFitDrawOn=0;   //! If set to 1 it will create a new TGraphAsymmErrors for each hit, if set to 2 will create new TGraphAsmmErrors for each hit up to mMaxPage then reset

    virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag " __DATE__ " " __TIME__ ; return cvs;}

    ClassDef(StFcsWaveformFitMaker, 4)
};
#endif  // STROOT_STFCSWAVEFORMFITMAKER_STFCSWAVEFORMFITMAKER_H_
