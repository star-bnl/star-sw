//Author:David Kapukchyan
/*
@[June 25, 2022]
 > Added Refinements to drawing and copying. Also made some changes to how the tunnel probability is used to merge the peaks.

@[March 3, 2022]
 > Copied StFcsPulseFit so that I can inherit from and use the PeakAna class methods from MyTools
*/

#ifndef STROOT_STFCSWAVEFORMFITMAKER_STFCSPULSEANA_H_
#define STROOT_STFCSWAVEFORMFITMAKER_STFCSPULSEANA_H_

//ROOT Headers
#include "TMath.h"
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

#include "StFcsDbMaker/StFcsDbPulse.h"
#include "PeakAna.h"

class StFcsPulseAna : public PeakAna
{
 public:
  StFcsPulseAna();
  StFcsPulseAna( std::string name );
  explicit StFcsPulseAna( TGraph* Sig, std::string name = "StFcsPulseAna");//Construct with a known TGraph
  StFcsPulseAna(const StFcsPulseAna& old, const char* post_name="_copy", TGraph* graph=0);//Copy Constructor
  StFcsPulseAna& operator=(const StFcsPulseAna& rhs);//Assignment operator
  //explicit StFcsPulseAna( int ntb, double* tb, double* adc, std::string name = "StFcsPulseAna");//Array of timebins and ADC values with known size (ntb)
  virtual ~StFcsPulseAna();

  virtual void Copy(TObject& obj) const;
  virtual TObject* Clone(const char* newname) const;

  virtual Int_t AnalyzeForPeak();

  static double MaxwellBoltzmannDist(double* x, double* p);//Maxwell Boltzmann Distribution function
  static void GetMBPars(const double& xpeak, const double& xrise, const double& yh, const double& ped, double& height, double& scale );//Get parameters for a Maxwell Boltzmann distribution from above based on the 4 const parameters 

  virtual TGraphAsymmErrors* GetData()const{return (TGraphAsymmErrors*)mG_Data;}

  const char* GetName() const {return mName.c_str();}
  std::string& Name() {return mName;}
  const std::string& Name() const {return mName;}
  //void LoadHit(int det, StFcsHit* hit);

  void setDbPulse(StFcsDbPulse* p){mDbPulse = p;}
  
  void ResetFinder();//Resets all histograms and values
  void ResetBaseline();//Resets baseline values
  void ResetSum();//Only resets variables related to finding the sum
  
  //void SetBaselineSearchParam();//if baseline search is set to zero will not find baseline
  Int_t Sum(Int_t Start, Int_t End);//Generic sum funcition to add ADC within a given range and subtract baseline
  Int_t SumWindow();//Sum the data inside the found window
  Double_t GausFit(Int_t Start=0,Int_t End=0);
  void SignalMBPars(double& height, double& scale);
  double SumMB();
  Double_t MBFit(Int_t Start=0,Int_t End=0);
  Double_t PulseFit(Int_t Start=0, Int_t End=0);
  //TF1* PulseFit(float* res);
  void SetFitPars(TF1* func);

  static void FillAdc(TGraphAsymmErrors* g, unsigned short& counter, int Start, unsigned short* adcdata);//Needed for SumDep0, et al. It basically fills in tb vs. adc sequentially so all timebins from a given start value have an adc.
  static int SumDep0(TGraphAsymmErrors* gdata, int Start, int ped=0);
  static int SumDep0Mod(TGraphAsymmErrors* gdata, int Start, int ped=0);

  //void SetSignal(TGraphAsymmErrors* SigGraph);//replaced with setdata
  void SetFitSignal(TF1* func){mF1_SignalFit=func;}
  void SetBaselineFit(TF1* func){mF1_BaselineFit=func;}

  //void SetBaselineRange(Int_t Nbins, Double_t Xmin, Double_t Xmax);//Need to implment?
  void SetZS(){SetBaseline(0,0.6);}//Call this for ZS data which uses thresholds that are relevant for that. (like 0 baseline and 0.5 sigma so thereshold is at 2 since ZS is pedestal subtracted. Maybe even 0.3 so it is above one.

  void AnalyzeForPedestal(); //Analyze graph data to determine baseline internally
  
  TH1F* BaselineHist()const{return mH1_Baseline;}
  TF1* SignalFit(const char option ='n');//Replace with a function that actually calls "fit" and then returns the function?Arguments can be values and range?A little more complicated since hard to tell when user would want to call a fit or just get the function itself?Maybe no arguments means give me function, arguments means do a refit?
  TF1* SignalFit() const {return mF1_SignalFit;}
  TF1* BaselineFit()const{return mF1_BaselineFit;}//see above "^"?

  //void ReFitBaseline(){mG_Signal->Fit(mF1_BaselineFit,"R");}

  virtual StFcsPulseAna* DrawCopy(Option_t* opt="",const char* name_postfix = "_copy", TGraph* graph=0) const;

  virtual void Print(Option_t* opt="") const;

  virtual void MergeByProbability(std::vector<PeakWindow>& merged);
  //void FillInMissingData();

 protected:
  void Init(); //Sets everything to zero except signal and background histograms

  StFcsDbPulse* mDbPulse;

  TH1F* mH1_Baseline;//This histogram holds projection info from mG_Signal
  //Main fit functions
  TF1*  mF1_SignalFit;
  TF1*  mF1_BaselineFit;

  bool FindBaseline();//Does Gaussian fitting to mH1_Background to determine baseline

 private:
  
  std::string mName;

  bool mWindowSum;
  bool mFitSum;
    
  //Variables to hold sum values.  This is to avoid tedious recomputation
  Int_t mSumAdc;
  Double_t mSumAdcFit;
  
  //These two variables are in PeakAna as mMaxY and mMaxX respectively
  //Double_t mMaxAdc;//Maximum value for Adc
  //Double_t mMaxTb;//Timebin where maximum value of Adc occurs
  

  ClassDef(StFcsPulseAna,1)

};

#endif
