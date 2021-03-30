//Author:David Kapukchyan
//October 13, 2020
//Changed search for T0s to exclude any peaks not within the search window.  Also added 'Peak' to 'SigWindow' to help with searches.  'LocalMax' in 'GetPossibleT0' replaces with this "Peak" variable.

//October 01, 2020
//Added extra functions for baseline determination.

//Sep 15, 2020
//Changed to StFcsPulseFit which only keeps the relevant peak finding algorithm

//Sep 11, 2020
//Need to fix case where possible T0s exist but no valid search window was found.  Currently this returns the first index of the possible T0 vector

//Sep 04, 2020
//Moved back into specific FCS loacation since I realized many of the checks are specific to FCS and seperating and making all of these as "set" variables will make such a generic class too complicated to use.  Also, the algorithm assumes certain things about the signal itself which is embedded into the finding and would be too cumbersome to generalize

//Aug 01, 2020
//Moved this to a more generic location: 'MyTools'
//For this reason the methods related to 'StFcsHit' have been commented out

//Oct 16, 2019
//Class that will do signal finding and fitting for the FCS

//Nov 1, 2019
//Need to rethink how and when baselne will be computed
//Add Mean and RMS functions/variables for signal?

#ifndef __STFCSPULSEFIT_HH_
#define __STFCSPULSEFIT_HH_

//Custom Tools
//#include "Rtools.h"
//#include "DrawTools.h"

//Fcs Headers
//#include "StFcsHit.h"

//ROOT Headers
#include "TLegend.h"
#include "TLegendEntry.h"
#include "TF1.h"
#include "TGraphAsymmErrors.h"
#include "TMultiGraph.h"
#include "TH1.h"
//#include "TH2.h"
//#include "TH2I.h"
//#include "TProfile.h"
#include "TLine.h"
#include "TPaveText.h"
#include "TPaveStats.h"
//#include "TColor.h"
//#include "TGaxis.h"
//#include "TFrame.h"

class StFcsPulseFit : public TObject
{
 public:
  StFcsPulseFit();
  StFcsPulseFit( std::string name );
  explicit StFcsPulseFit( TGraphAsymmErrors* Sig, std::string name = "StFcsPulseFit");//Construct with a known TGraph
  //explicit StFcsPulseFit( int ntb, double* tb, double* adc, std::string name = "StFcsPulseFit");//Array of timebins and ADC values with known size (ntb)
  virtual ~StFcsPulseFit();

  std::string Name(){return mName;}
  //void LoadHit(int det, StFcsHit* hit);
  
  void ResetFinder();//Resets all histograms and values
  void ResetBaseline();//Resets baseline values
  void ResetT0();//Only resets variables related to T0
  void ResetSum();//Only resets variables related to finding the sum
  
  void ForceInternal(){ mInternalSignal=true; }//Call this is to force this class to delete the TGraphAsymmErrors in the destructor
  void SetDebug(){ mDEBUG=true; }
  //void SetParameters( Int_t start, Double_t baseline, Double_t baselinesigma=0.75, Double_t sigmascale=4.0);//This will be used by the fitter in place of the defaults
  //void SetSearchWindow();
  //void SetBaselineSearchParam();//if baseline search is set to zero will not find baseline
  Int_t Sum(Int_t Start, Int_t End);//Generic sum funcition to add ADC within a given range and subtract baseline
  Int_t SumWindow();//Sum the data inside the found window
  Double_t SumAdcFit(Int_t Start=0,Int_t End=0);//Rename future?

  void SetSignal(TGraphAsymmErrors* SigGraph);
  //void SetSignal(int ntb, double* tb, double* adc);
  void SetBaseline(Double_t value, Double_t sigma=0.75);//This is zs-pedestal subtracted data where baseline doesn't need to be determined. Must be >=0.  The sigma set will determine the threshold ADC.  ThresholdADC=sigma*4.  The 4 is default but can be changed - See "AnalyzeForT0".  This means default ADC threshold is 3 for zs-pedestal subtracted data
  //void SetBaselineRange(Int_t Nbins, Double_t Xmin, Double_t Xmax);//Need to implment?
  void SetWindow(const Int_t &start, const Int_t &end ){mSignalT0.SetWindow(start,end);mT0Computed=true;}//Set signal window to user defined values.
  void SetSearchWindow(Int_t peak, Int_t width){ mSearch.SetWindow(peak,width); }//Set start time to search for and the width of the start time
  void SetZS(){SetBaseline(0,0.6);}//Call this for ZS data which uses thresholds that are relevant for that. (like 0 baseline and 0.5 sigma so thereshold is at 2 since ZS is pedestal subtracted. Maybe even 0.3 so it is above one.

  void AnalyzeForPedestal(); //Analyze graph data to determine baseline internally
  
  void AnalyzeForT0(Float_t Sigma=4.0);//Signal cutoff is 4(sigma)*BaselineSigma above baseline
  Int_t SignalStart(){if( !mT0Computed ){AnalyzeForT0();} return mSignalT0.Start;}//Found Signal Start time
  Int_t SignalEnd(){  if( !mT0Computed ){AnalyzeForT0();} return mSignalT0.End;}//Found Signal End time
  Double_t SignalPeakTb();//Timebin of found signal peak
  Double_t SignalPeakAdc();//ADC of found signal peak
  void SignalPeak(Double_t &tb, Double_t &adc);

  Double_t Baseline(){ if( FindBaseline() ){return mBaseline;}else{return -5.0;} }//If baseline is found will only return the value
  Double_t BaselineSigma(){ if( FindBaseline() ){return fabs(mBaselineSigma);}else{return 0.75;} }//fabs in case the found sigma is negative
  //Need checks for fitting future?
  //Double_t SignalFitMean(){return mF1_SignalFit->GetParameter(1);}
  //Double_t SignalFitSigma(){return fabs(mF1_SignalFit->GetParameter(2));}

  void GetXYMax(Double_t xmin=-5, Double_t xmax=2000);
  Double_t MaxAdc(){if( mMaxAdc<0 ){GetXYMax();} return mMaxAdc; }
  Double_t MaxTb(){if( mMaxTb<0 ){GetXYMax(); } return mMaxTb; }

  bool GoodWindow();

  TH1F* BaselineHist(){return mH1_Baseline;}
  TF1* SignalFit(const char option ='n');//Replace with a function that actually calls "fit" and then returns the function?Arguments can be values and range?A little more complicated since hard to tell when user would want to call a fit or just get the function itself?Maybe no arguments means give me function, arguments means do a refit?
  TF1* BaselineFit(){return mF1_BaselineFit;}//see above "^"?

  //void ReFitBaseline(){mG_Signal->Fit(mF1_BaselineFit,"R");}

  virtual void PrintInfo() const;

 protected:

  bool mDEBUG;
  virtual void Initialize(); //Sets everything to zero except signal and background histograms
  
  bool mT0Computed;  //This will keep from having to determine T0 every time
  //bool mFindBase;  //This will be used to know whether baseline needs to be found or not

  //This struct is for holding the signal window
  struct SigWindow
  {
    Int_t Start; Int_t End;
    Int_t P_Peak;//Point Number of peak (P for point)
    //Default values are two extremes (Start>End otherwise algorithm won't work properly)
    SigWindow(){Start=-5;End=2000;P_Peak=-5;}//2000 since largest timebin is 1024
    void SetWindow(Int_t s, Int_t e){Start=s; End=e;}
    void GetWindow(Int_t &s, Int_t &e){s=Start;e=End;}
    void Reset(){Start=-5.0; End=2000;P_Peak=-5.0;}
    void Print()const{std::cout << "|Start:"<<Start << "|End:"<<End<<"|Peak:"<<P_Peak;}
  };

  SigWindow mSignalT0;//This is the signal start time and hold the value of the histogram bin that corresponds to this signal start time that is why it is an 'Int_t'
  Double_t mBaseline;//This is the basline for the signal (For physics data in Zero Suppression bank should be zero)
  Double_t mBaselineSigma;//This is the standard deviation (sigma) around the baseline for ADCs (Also used as a threshold for the hit)
  
  //Main objects to hold raw infomration
  TGraphAsymmErrors* mGAE_Signal;
  //Pros of graph:Accurately represents data, no errors in fitting assumed
  //cons of graph: delete and recreate arrays constantly, drawing will produce a histogram object anyway
  //pros of histogram: Reset calls "memset" not "delete" won't be deleting memory constantly
  //cons of histogram: Need to fit without errors, histogram is not representing the kind of data we have which may cause issues in fitting in future
  TH1F* mH1_Baseline;//This histogram holds projection info from mG_Signal
  //Main fit functions
  TF1*  mF1_SignalFit;
  TF1*  mF1_BaselineFit;

  SigWindow mSearch;//In this struct the 'start' represents the start time to look for and the 'end' represents the +- window from 'start' to look for

  bool FindBaseline();//Does Gaussian fitting to mH1_Background to determine baseline
  std::vector<SigWindow> GetPossibleT0(Float_t SigmaScale);//Finds all possible T0s in signal with some criteria
  UShort_t SearchForT0(const std::vector<SigWindow> &PossibleT0s);//Finds signal start time by checking which start time fits inside the trigger window

 private:
  
  std::string mName;

  bool mInternalSignal;
  bool mWindowSum;
  bool mFitSum;
    
  //Variables to hold sum values.  This is to avoid tedious recomputation
  Int_t mSumAdc;
  Double_t mSumAdcFit;

  Double_t mMaxAdc;//Maximum value for Adc
  Double_t mMaxTb;//Timebin where maximum value of Adc occurs
  

  ClassDef(StFcsPulseFit,1)

};

#endif
