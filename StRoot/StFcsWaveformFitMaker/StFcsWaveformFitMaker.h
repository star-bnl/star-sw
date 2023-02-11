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
    void setTest(int v);           //!< Set test level. Intended to be used for single files. Output file name can be changed with #writeFile(), see #mTest for meaning of values
    void setEnergySelect(int ecal=10, int hcal=10, int pres=1) {mEnergySelect[0]=ecal; mEnergySelect[1]=hcal; mEnergySelect[2]=pres;}
    void setCenterTimeBins(int v, int min=0, int max=512) {mCenterTB=v; mMinTB=min; mMaxTB=max;}
    void setAdcSaturation(int v)  {mAdcSaturation=(double)v;}
    void setError(double v)       {mError=v;}
    void setErrorSaturated(int v) {mErrorSaturated=v;}
    void setMinAdc(int v)         {mMinAdc=v;}
    void setTail(int v);                            //!< Set tail parameters to use for #mDbPulse, see #StFcsDbPulse::setTail()
    void setMaxPeak(int v)        {mMaxPeak=v;}     //!< Set maximum number of peaks to fit, see #mMaxPeak
    void setPedTimeBins(int min, int max) {mPedMin=min; mPedMax=max;} //!< Set the timebin region where the pedestal occurs, for non-pedestal subtracted data
    void setAnaWaveform(bool value=true){ mAnaWaveform=value; }   //!< Set whether to compute integral of waveform (true); or just recompute energy (false); see #mAnaWaveform

    TGraphAsymmErrors* resetGraph();    //!< Create or Reset TGraphAsymmErrors (at #mHitIdx)
  
    /**@brief Return TGraphAsymmErrors at idx
       
       @param idx index of #mChWaveData to get graph, if idx<0 (default) it returns "current" which is idx=#mHitIdx-1
       @return TGraphAsymmErrors for channel data, 0 if idx does not exist in #mChWaveData
    */
    TGraphAsymmErrors* getGraph(int idx=-1);
    TGraphAsymmErrors* getGraph(int det, int id);   //!< Get graph of a particular detector and channel id if it exists in #mChWaveData

    //measuring fit time
    void setMeasureTime(char* file) {mMeasureTime=file;}
    TH1F* mTime = 0;

    //stage0 peak algo study
    TH2F* mTimeIntg[4];

    /**@brief Create TGraphAsymmErrors from given timebin and adc data

       This function is used populate a TGraphAsymmErrors object from raw data with asymmetric errors when adc is saturated at #mAdcSaturation. Uses #StFcsDbPulse::setTGraphAsymmErrors() to determine the errors.

       Places graph at #mHitIdx=0 of #mChWaveData and will be deleted in destructor except when #mFitDrawOn is set, see #setFitDrawOn().
       
       @param n size of array/data
       @param t array of timebin data
       @param adc array of ADC data
       @return ADC saturated TGraphAsymmErrors object
    */
    TGraphAsymmErrors* makeTGraphAsymmErrors(int n, double* t, double* adc);    
    TGraphAsymmErrors* makeTGraphAsymmErrors(TGraph* g);                 //!< Same as #makeTGraphAsymmErrors(int,double*,double*) except using a TGraph object
    TGraphAsymmErrors* makeTGraphAsymmErrors(StFcsHit* hit);             //!< Same as #makeTGraphAsymmErrors(int,double*,double*) except using #StFcsHit object, also names graphs by detid and chan id
    TGraphAsymmErrors* makeTGraphAsymmErrors(TH1* hist);                 //!< Same as #makeTGraphAsymmErrors(int,double*,double*) except using 1D histogram where histogram bin is timebin value and ADC is bin content

    /**@brief Perfom the analysis using selected method

       Main analysis function that will determine the ADC sum used to compute the energy

       @param select analysis method to use
       - 1 = Use sum 8 method, #sum8()  
       - 2 = Use sum 16 method, #sum16()  
       - 3 = Use highest data point, #highest()  
       - 4 = Use +-1 of triggered crossing, #highest3()  
       - 10 = Find peaks and do Gaussian fit, #gausFit()  
       - 11 = Figure out pedestal and do a  Gaussian fit, #gausFitWithPed()  
       - 12 = Use #StFcsPulseAna to find peaks and fit to Gaussian except those peaks from #PeakCompare=0, #PulseFit1()  
       - 13 = Use #StFcsPulseAna to find peaks and fit to Gaussian those peaks from #NPeaksPrePost, #PulseFit2()  
       - 14 = Use #StFcsPulseAna to find peaks and fit all found peaks to Gaussian, #PulseFitAll()  
	      
       @param g TGraphAsymmErrors data to use in the analysis method
       @param res array of determined values from analysis method, max is 8
       - res[0] = pulse integral [ch]
       - res[1] = pulse peak height [ch]
       - res[2] = pulse peak position [timebin]
       - res[3] = pulse peak sigma [timebin]
       - res[4] = pulse fit chi2/NDF
       - res[5] = number of peaks
       - res[6] = pedestal Value
       - res[7] = pedestal Standard Deviation
	      
       @param f TF1 function to use for fitting if needed, gets deleted on function call so analysis method must create a new one each time
       @param ped pedestal value to use in the analysis method
    */
    float analyzeWaveform(int select, TGraphAsymmErrors* g, float* res, TF1*& f, float ped=0.0); 
    float analyzeWaveform(int select, int n, double* t, double* adc, float* res, TF1*& f, float ped=0.0); //!< same as #analyzeWaveform(int, TGraphAsymmErrors*, float*, TF1*&, float) except will generate TGraphAsymmErrors object from timebin (t) and adc array data
    float analyzeWaveform(int select, TGraph* g, float* res, TF1*& f,float ped=0.0);                      //!< same as #analyzeWaveform(int, TGraphAsymmErrors*, float*, TF1*&, float) except will generate TGraphAsymmErrors object from timebin and adc data in a TGraph
    float analyzeWaveform(int select, StFcsHit* hit, float* res, TF1*& f,float ped=0.0);                  //!< same as #analyzeWaveform(int, TGraphAsymmErrors*, float*, TF1*&, float) except will generate TGraphAsymmErrors object from #StFcsHit

    //Pedestal analysis methods
    float AnaPed( TGraphAsymmErrors* g, float& ped, float& pedstd );
    float AnaPed( StFcsHit* hit, float& ped, float& pedstd );
    //actual analysis methods
    float sum8    (TGraphAsymmErrors* g, float *res);                             //!< mEnergySelect=1
    float sum16   (TGraphAsymmErrors* g, float *res);                             //!< mEnergySelect=2
    float highest (TGraphAsymmErrors* g, float *res);                             //!< mEnergySelect=3
    float highest3(TGraphAsymmErrors* g, float *res);                             //!< mEnergySelect=4
    float gausFit (TGraphAsymmErrors* g, float *res, TF1*& f, float ped=0.0);     //!< mEnergySelect=10
    float gausFitWithPed (TGraphAsymmErrors* g, float *res, TF1*& f);             //!< mEnergySelect=11
    float PulseFit1(TGraphAsymmErrors* g, float* res, TF1*& f, float ped=0.0);    //!< mEnergySelect=12, find and fit selected peaks around triggered crossing using #PeakCompare()
    float PulseFit2(TGraphAsymmErrors* g, float* res, TF1*& f, float ped=0.0);    //!< mEnergySelect=13, (default) find and fit selected peaks around triggered crossing using #NPeaksPrePost()
    float PulseFitAll(TGraphAsymmErrors* g, float* res, TF1*& f, float ped=0.0);  //!< mEnergySelect=14, fit all peaks less than mMaxPeak
    float PulseFit2WithPed( TGraphAsymmErrors* g, float* res, TF1*& f);           //!< mEnergySelect=15, first find pedestal using #AnaPed(), then call #PulseFit2()
    float PulseFitAllWithPed( TGraphAsymmErrors* g, float* res, TF1*& f);         //!< mEnergySelect=16, first find pedestal using #AnaPed(), then call #PulseFitAll()
    float PedFitPulseFit( TGraphAsymmErrors* g, float* res, TF1*& f);             //!< mEnergySelect=17, first find pedestal using #StFcsPulseAna::AnalyzePedestal() then call #PulseFitAll()
  
    //Draw fits    
    void setMaxPage(int v){mMaxPage=v;}         //!< Max pages to draw
    void setSkip(int v){mSkip=v;}               //!< Number of channels to skip before drawing
    /** @brief Set event drawing options
	
	This function can be used to set the file name the channel drawing canvas will save to, the number of pages for the canvas to save, and how may channels to skip when drawing channels

	@param file name of file to save drawing canvas to, exclude file extension in the name since all saved files will be pdf format
	@param maxpage maximum number of pages to draw
	@param skip number of channels to skip before drawing
     */
    void setFileName(char* file, int maxpage=25, int skip=5){mFilename=file; mMaxPage=maxpage; mSkip=skip;} 
    void writeFile(std::string filename);       //!< Use to change the name of the file where test histograms will be saved
    /** @brief Sets how much channel data will saved in #mChWaveData

	This flag is used to tell #StFcsWaveformFitMaker how much channel data to store for each event.

	@param v flag level
	- 0 = only store 1 channel data for an event, i.e. don't increment #mHitIdx
	- 1 = keep all channel data for an event, i.e. increment #mHitIdx until event ends
	- 2 = keep channel data up to MAXPAD=16 in #drawFit(), i.e. increment #mHitIdx until #mHitIdx=MAXPAD
     */
    void setFitDrawOn(int v=1) {mFitDrawOn=v;}
    void setFitFilter(char* filter) {mFilter=filter; mFitDrawOn=2;}

    //Draw from David
    int centerTB()const{return mCenterTB;}                                //!< @return  #mCenterTB
    void setDavidFitter(StFcsPulseAna* v){if(mPulseFit==0){mPulseFit=v;}} //!< Override #mPulseFit, will be deleted
    StFcsPulseAna* davidFitter(){return mPulseFit;}                       //!< @return #mPulseFit
    StFcsPulseAna* InitFitter(Double_t ped=0);     //!< Sets up basic values needed by #StFcsPulseAna @param ped the pedestal value to use

    /**@brief Helper function for #PadNum4x4

       This function can be used to get the pad number needed when you have Nvals of stuff to draw in a single column/row

       @param value column/row of Ecal or Hcal channel to draw
       @param Nvals number of channels in a column/row to draw on the same pad
       @param PadNums number of pads in a column/row
       @return column/row number in a TCanvas that was split
     */
    static int GenericPadPos(int value, int Nvals, int PadNums );
    /**@brief Helper function for #drawRegion()

       Get the pad number to use when drawing Ecal and Hcal channels on a TCanvas split into 4x4 pads. It assumes each pad will contain a 2x3 (column x row) grouping of Ecal channels and a 2x2 grouping of Hcal channels.
       
       @param det detector id to get pad for
       @param col column of Ecal/Hcal channel to get pad number for
       @param row row of Ecal/Hcal channel to get pad number for
       @return pad number to draw channel on for a 4x4 split TCanvas
    */
    static int PadNum4x4(int det, int col, int row);
    /**@brief draw a region of the Ecal or Hcal

       This function can be used to draw the timebin and adc data in the Ecal or Hcal for a specified region in terms of column and rows. It draws on the internal canvas #mCanvas. Utilizes #PadNum4x4(), and #GenericPadPos() to get the correct pad number. By default, it will draw on the same pad, a 2x3 (column x row) grouping of channels for Ecal and a 2x2 grouping of channels in the Hcal.

       Needs #mFitDrawOn>0 to draw all channels

       @param det detector id to draw, only Ecal and Hcal (det<4)
       @param col_low lowest column of Ecal/Hcal to draw
       @param row_low lowest row of Ecal/Hcal to draw
       @param col_high highest column of Ecal/Hcal to draw
       @param row_high highest row of Ecal/Hcal to draw
       @param event event number that is drawn, only used in file name when saving, this way multiple events can be saved without overwrite
    */
    void drawRegion(int det, int col_low, int row_low, int col_high, int row_high, int event=0);
    void drawEvent(int det, int event=0);    //!< Utilizes #drawRegion() to draw all channels in an event
    void drawFitter(Option_t* opt){ if(mPulseFit!=0){mPulseFit->Draw(opt);} }  //!< Call #StFcsPulseAna::Draw()
    /**@brief Draw a single channel on the current canvas/pad

       Draw a single channel's adc vs. tb data. If #mPulseFit!=0 then also draw found peaks from #StFcsPulseAna.
       
       Needs #mFitDrawOn=1 otherwise channel may not exist in #mChWaveData
       
       @param detid detector id to draw
       @param ch channel to draw
    */
    void drawCh(UInt_t detid, UInt_t ch) const;
    /**@brief Special draw function, to compare fits for a single channel

       This function will draw a single channel's adc vs. timebin on the current canvas/pad and also the fits from the two methods #PulseFit2() and #gausFit(). It is intended to be used for comparison and testing only.

       Needs #mFitDrawOn=1 otherwise channel may not exist in #mChWaveData

       @param detid detector id to draw
       @param ch channel to draw
     */
    void drawDualFit(UInt_t detid, UInt_t ch);

    void printArray() const;                 //!< Print contents of #mChWaveData, excluding timebin and adc information

 protected:
    TClonesArray mChWaveData;  //!< Contains all graph data
    void drawFit(TGraphAsymmErrors* g, TF1* func); //!< Draw a single TGraph
    StFcsPulseAna* mPulseFit;   //!< Pointer to peak finder used by some analysis methods

    /**@brief Variable to use when testing #StFcsWaveformFitMaker algorithms

       Self contained analysis for testing various components/functions of #StFcsWaveformFitMaker
       
       - 0 = no testing
       - 1 = test DEP algorithm #StFcsPulseAna::SumDep0() to #StFcsPulseAna::SumDep0Mod()
       - 2 = test #PeakAna vs. #gausFit()
       - 3 = test #PulseFit1() picking sum method
       - 4 = test #PulseFit1() all data with peaks
       - 5 = test timing of #gausFit() vs. #PulseFit1()
       - 6 = like test==3 but for #PulseFit2()
       - 7 = test #PulseFit2() for overall quality doesn't include preshower
       - 8 = test #PulseFit2() sum to #gausFit() sum when fitting even single peak cases
    */
    int mTest = 0;

    TFile* mOutFile;  //!< Root output file for testing
    //For testing Dep0 algo (mTest==1)
    TH2F* mH2_Dep0DepMod[3];
    TH2F* mH2_Sum8Dep0[3];
    TH2F* mH2_Sum8DepMod[3];
    //For testing number of peaks finding (mTest==2||mTest==3||mTest==6||mTest==8)
    TH1F* mH1_NPeaksAkio = 0;                       //!< Number of peaks found by #gausFit()
    TH1F* mH1_NPeaksFilteredAkio = 0;               //!< Number of peaks found by #gausFit() for signals that had a triggered crossing
    TH2F* mH2F_AdcTbAkio[6];                        //!< Adc vs. Tb for different number of peaks Akio method
    TH2F* mH2F_AdcTbMine[6];                        //!< Adc vs. Tb for different number of peaks my method
    TH2F* mH2F_AdcTbValidPeak[7];                   //!< Adc vs. Tb from my algorithm that had a peak at #mCenterTb (Need an extra one for non-valid peaks)
    TH2F* mH2F_SumFitvSumWin[6];                    //!< Sum from Akio's Fit function vs. Sum from my found peak window for different number of peaks
    TH2F* mH2F_APeakvMPeak[6];                      //!< PeakLocations from Akio vs. Mine
    TH1F* mH1F_PeakStart[6];                        //!< #PeakWindow Starting x-values
    TH1F* mH1F_PeakEnd[6];                          //!< #PeakWindow Ending x-values
    TH1F* mH1_PeakTiming = 0;                       //!< Timing for just peak finding.

    TH1F* mH1F_NPeaks[7];                           //!< Number of peaks found by peak finder #StFcsPulseAna
    TH1F* mH1F_NPeaksFiltered[7];                   //!< Number of peaks for cases where a peak around #mCenterTB was found
    TH2F* mH2_NPeakvsPeakXdiff = 0;                 //!< Number of peaks vs. Peak X diff
    TH2F* mH2_NPeakvsPeakYratio = 0;                //!< Number of peaks vs. Peak Y ratio
    TH1F* mH1_VOverlap = 0;                         //!< Value of overlap
    TH2F* mH2_NOvsNPeaks = 0;                       //!< NO (Number of overlaps) vs. Number of peaks
    TH2F* mH2_VvsNOverlap = 0;                      //!< Compare value for that peak comparison vs. Overlap index
    TH2F* mH2F_NOvsId[6];                           //!< NO (number of overlaps) vs channel id for the 6 detector ids
    TH1F* mH1F_Res0[7];                             //!< Histogram of all res[0] regardless of method
    TH1F* mH1F_Res0Zoom[7];                         //!< Histogram of all res[0] with finer bining at low end
    TH1F* mH1F_Sum8Res0[7];                         //!< Histogram of "res[0]" using sum8 regardless of method called
    TH1F* mH1F_Sum8Res0Zoom[7];                     //!< same as #mH1F_Sum8Res0 with finer bining at low end
    TH1F* mH1F_FitRes0[7];                          //!< Histogram of "res[0]" from fit regardless of method called
    TH1F* mH1F_FitRes0Zoom[7];                      //!< same as #mH1F_FitRes0 with finer bining at low end
    TH2F* mH2F_Sum8vFit[7];                         //!< Histograms of Fit res[0] vs. sum8 res[0]     
    TH1F* mH1_TimeFitPulse = 0;                     //!< Histogram to time how long just the fitting takes in #PulseFit1() and #PulseFit2()

    //For testing peak height vs. sigma
    TH2F* mH2_HeightvsSigma = 0;                    //!< Histogram of all fitted peak heights vs. their sigma
    TH2F* mH2_HeightvsSigmaTrig = 0;                //!< Histogram of height of fitted peaks in triggered crossing vs. their sigma
    TH1F* mH1_ChiNdf = 0;                           //!< Histogram of chi^2/ndf for all fits
    TH2F* mH2_HeightvsChiNdf = 0;                   //!< Histogram of height vs. chi^2/ndf for all fits
    TH2F* mH2_MeanvsChiNdf = 0;                     //!< Histogram of height vs. chi^2/ndf for all fits
    TH2F* mH2_SigmavsChiNdf = 0;                    //!< Histogram of chi^2/ndf for all fits

    TH1F* mH1_PeakTimingGaus = 0;                   //!< Histogram to test timing of #gausFit()
    TH1F* mH1_PeakTimingPuls = 0;                   //!< Histogram to test timing of #PulseFit1()
    TH2F* mH2_PeakTimingCompare = 0;                //!< Histogram to test timing between #PulseFit1() vs. #gausFit()

    void SetupDavidFitterMay2022(Double_t ped=0);   //!< This special function is used to set all the parameters for #StFcsPulseAna based on cosmic and Run 22 data. It is intended to be used only for Run 22 data
  /**@brief Compare if two peaks overlap and return a bit vector of tests passed/failed.

     This function can be used to compare two #PeakWindow objects pwin1 and pwin2 for overlap. It will check x-difference between the peak locations and the ratio of the peak heights. If the x-difference is <10.0 it will turn on bit 0. If peak ratio of pwin1 to pwin2 is <2 it will turn on bit 1. If returned bit vector is 0 it means pwin1 does not overlap with pwin2
     @param pwin1 first #PeakWindow to compare
     @param pwin2 second #PeakWindow to compare
     @return bit vector of tests passed, 0 means no overlap
   */
    int PeakCompare(const PeakWindow& pwin1, const PeakWindow& pwin2 );
    /**@brief Finds out how many peaks are within a fixed number of pre-crossings and post-crossings

     This function is used to find out how many peaks are within 3 RHIC pre-crossings and 2 RHIC post-crossings. The index in #PeakAna::mPeaks that those peaks correspond to are saved in a vector and returned with trigidx being the index in the returned vector where the triggered crossing occurs in #PeakAna::mPeaks. It also will find the x-range for the returned peak index vector (xmin, xmax).
     @param trigidx index in the returned vector of where the triggered crossing occurs in #PeakAna::mPeaks
     @param xmin minimum x-value for the found peaks
     @param xmax maximum x-value for the found peaks
     @return vector of indexes for #PeakAna::mPeaks which correspond to peaks within the search crieteria
    */
    std::vector<int> NPeaksPrePost(int& trigidx, Double_t& xmin, Double_t& xmax) const;

 private:
    StFcsDb* mDb=0;                    //!< pointer to fcsDb
    StFcsDbPulse* mDbPulse = 0;        //!< pointer to fcsPulse
    StFcsCollection* mFcsCollection=0; //!< pointer to fcsCollection

    unsigned int mHitIdx = 0; //!< running index for #mChWaveData, needed to keep saved channel information in the array as small as needed

    //Figures out error to set on the TGraphAsymmErrors for a given point and adc
    char *mMeasureTime=0;                //!< output file for measuring fitting time

    int mEnergySelect[3];                //!< 0=MC (straight from dE), >0 see #analyzeWaveform()
    bool mAnaWaveform;                   //!< if true (default) call #anlayzeWaveform() to integrate the waveform data, if false read integral from StFcsHit and only recompute energy using gain, and gain correction factors
    int mCenterTB=50;                    //!< center timebin for triggered crossing
    int mMinTB=0;                        //!< center timebin for triggered crossing
    int mMaxTB=512;                      //!< center timebin for triggered crossing

    double mError=1.0;                   //!< error to be used for none saturated timebin 
    double mErrorSaturated=1000.0;       //!< upper error to be used for saturated timebin 
    double mAdcSaturation=4000.0;        //!< above this, upper error will be enlarged
 
    int mPedMin=-1;                      //!< min TB for ped
    int mPedMax=-1;                      //!< max TB for ped

    //gausFit
    int mMinAdc=5;                       //!< minimum adc to be a peak
    int mTail=2;                         //!< pulse tail shape (0=none, 1=summer2020) 
    int mMaxPeak=5;                      //!< max number of peak for trying to fit in #gausFit() and #PulseFit2(), if number of peaks >= #mMaxPeak do #sum8()

    //Drawing fits
    TCanvas* mCanvas=0;                  //!< Canvas to draw channels on when drawing events
    int mPage=0;                         //!< Counter for canvas page that will be drawn
    int mPad=0;                          //!< Counter for canvas pad that will be drawn
    int mMaxPage=25;                     //!< Max pages to draw for #mCanvas
    int mSkip=2;                         //!< Number of channels to skip when drawing channels on canvas
    char* mFilename=0;                   //!< Name of file to save canvas on, exclude file extension, pdf by default
    char* mFilter=0;
    char mDetName[100];                  //!< Persistant array that will be used to set the detector name of the TGraph object
    int mFitDrawOn=0;                    //!< Flag to indicate how many channels data to save in #mChWaveData, 0=none, 1=all, 2=MAXPAD

    virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag " __DATE__ " " __TIME__ ; return cvs;}

    ClassDef(StFcsWaveformFitMaker, 4)
};
#endif  // STROOT_STFCSWAVEFORMFITMAKER_STFCSWAVEFORMFITMAKER_H_
