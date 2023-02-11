//Author:David Kapukchyan
/*
@[December 17, 2022]
> SetFitPars can now set a function parameters for a range of in npeaks as opposed to all peaks

@[June 25, 2022]
 > Added Refinements to drawing and copying. Also made some changes to how the tunnel probability is used to merge the peaks.

@[March 3, 2022]
 > Copied StFcsPulseFit so that I can inherit from and use the PeakAna class methods from MyTools
*/

/*!
  This class is an extension of #PeakAna to analyze specifically STAR data from the DEP boards. The DEP boards are used in the Forward Calorimeter System (FCS) and comprises of an electromagnetic, and hadronic calorimeters as well as a preshower.
*/

#ifndef STFCSPULSEANA_H
#define STFCSPULSEANA_H

//ROOT Headers
#include "TMath.h"
#include "TLegend.h"
#include "TLegendEntry.h"
#include "TF1.h"
#include "TGraphAsymmErrors.h"
#include "TMultiGraph.h"
#include "TH1.h"

#include "TLine.h"
#include "TPaveText.h"
#include "TPaveStats.h"

#include "StFcsDbMaker/StFcsDbPulse.h"
#include "PeakAna.h"

class StFcsPulseAna : public PeakAna
{
 public:
  StFcsPulseAna();                    //!< Default constructor
  StFcsPulseAna( std::string name );  //!< Constructor with name
  explicit StFcsPulseAna( TGraph* Sig, std::string name = "StFcsPulseAna"); //!< Construct with a known TGraph
  StFcsPulseAna(const StFcsPulseAna& old, const char* post_name="_copy", TGraph* graph=0); //!< Copy Constructor can be called with a new graph and post-fix to the name
  StFcsPulseAna& operator=(const StFcsPulseAna& rhs);   //!< Assignment operator

  virtual ~StFcsPulseAna();  //!< Destructor

  virtual void Copy(TObject& obj) const;              //!< Internal method use Clone instead.
  virtual TObject* Clone(const char* newname) const;  //!< Clones internal graph as opposed to just copying the pointer

  virtual Int_t AnalyzeForPeak();  //!< Overwritten from #PeakAna to process peak tunneling after finding all peaks
  virtual Int_t AnalyzeForPeak(Double_t peak, Double_t width){ return PeakAna::AnalyzeForPeak(peak,width); }//<! same as #AnalyzeForPeak() but also sets search parameters for peak
  virtual Int_t AnalyzeForNoisyPeak(){ return PeakAna::AnalyzeForNoisyPeak(); }//<! First calls #ConvertToAna() function and then #AnalyzeForPeak() on the converted data

  static double MaxwellBoltzmannDist(double* x, double* p);//!< Maxwell Boltzmann Distribution function
  static void GetMBPars(const double& xpeak, const double& xrise, const double& yh, const double& ped, double& height, double& scale );//!< Get parameters for a Maxwell Boltzmann distribution from above based on the 4 const parameters 

  virtual TGraphAsymmErrors* GetData()const{return (TGraphAsymmErrors*)mG_Data;} //!< Overwrite from #PeakAna to type convert for #StFcsWaveformFitMaker

  const char* GetName() const {return mName.c_str();} //!< @return #mName as const char*
  std::string& Name() {return mName;}                 //!< @return #mName
  const std::string& Name() const {return mName;}     //!< @return #mName

  void setDbPulse(StFcsDbPulse* p){mDbPulse = p;}     //!< @param p sets #mDbPulse
  
  void ResetFinder();   //!< Resets all histograms and values
  void ResetBaseline(); //!< Resets baseline values
  void ResetSum();      //!< Only resets variables related to finding the sum
  
  Int_t Sum(Int_t Start, Int_t End);                //!< Add raw ADC within a given range and subtract the baseline
  Int_t SumWindow();                                //!< Sum the ADC in the peak defined by #mFoundPeak and subtract the baseline
  Double_t GausFit(Int_t Start=0,Int_t End=0);      //!< Do a Gaussian fit on #mFoundPeak and return the integral subtracted by the baseline
  void SignalMBPars(double& height, double& scale); //!< Figure out the height and scale of a Maxwell-Boltzmann distribution that approximates the signal 
  double SumMB();                                   //!< Integral of an approximated Maxwell-Boltzmann distribution minus the baseline
  Double_t MBFit(Int_t Start=0,Int_t End=0);        //!< Fit a Maxwell-Boltzmann distribution to #mFoundPeak and return the integral minus the baseline
  Double_t PulseFit(Int_t Start=0, Int_t End=0);    //!< Fit the pulse shape defined in #StFcsDbPulse::multiPulseShape() to all peaks and return the integral of the found peak minus the baseline

  void SetFitPars(TF1*& func, int start=-1, int end=-1);   //!< Set the parameters of an external TF1 function that has the form of #StFcsDbPulse::multiPulseShape(), optionally only set fit paramaters for peaks from index start up to and including end

  static void FillAdc(TGraphAsymmErrors* g, unsigned short& counter, int Start, unsigned short* adcdata);//!< Needed for SumDep0, et al. It basically fills in tb vs. adc sequentially so all timebins from a given start value have an adc.
  static int SumDep0(TGraphAsymmErrors* gdata, int Start, int ped=0);     //!< Test of sum method in DEP board
  static int SumDep0Mod(TGraphAsymmErrors* gdata, int Start, int ped=0);  //!< Test my modified sum method to DEP board

  void SetFitSignal(TF1* func){mF1_SignalFit=func;}      //!< @param func sets #mF1_SignalFit
  void SetBaselineFit(TF1* func){mF1_BaselineFit=func;}  //!< @param func sets #mF1_BaselineFit

  void SetZS(){SetBaseline(0,0.39);} //!< Call this for ZS data which uses thresholds that are relevant for that. (like 0 baseline and 0.5 sigma so thereshold is at 2 since ZS is pedestal subtracted. Maybe even 0.3 so it is above one.

  void AnalyzeForPedestal(); //!< Analyze graph data to determine baseline internally
  
  TH1F* BaselineHist()const{return mH1_Baseline;} //!< @return mH1_Baseline
  TF1* SignalFit(const char option ='n');//Replace with a function that actually calls "fit" and then returns the function?Arguments can be values and range?A little more complicated since hard to tell when user would want to call a fit or just get the function itself?Maybe no arguments means give me function, arguments means do a refit?
  TF1* SignalFit() const {return mF1_SignalFit;}    //!< @return mF1_SignalFit
  TF1* BaselineFit()const{return mF1_BaselineFit;}  //!< @return mF1_BaselineFit

  virtual StFcsPulseAna* DrawCopy(Option_t* opt="",const char* name_postfix = "_copy", TGraph* graph=0) const; //!< Clone and draw, see #PeakAna::Draw() for options

  virtual void Print(Option_t* opt="") const; //!< Print class variables @param opt "ana" means call #PeakAna::Print(), "debug" print extra information

  virtual void MergeByProbability(std::vector<PeakWindow>& merged) const; //!< Overwritten from #PeakAna::MergeByProbability() to change merge criteria

 protected:
  void Init(); //!< Initialize everything to zero except signal and background histograms

  StFcsDbPulse* mDbPulse; //!< Pointer to #StFcsDbPulse

  TH1F* mH1_Baseline;     //!< Histogram that holds projection of mG_Data for baseline determination
  //Main fit functions
  TF1*  mF1_SignalFit;    //!< Function to fit to the data
  TF1*  mF1_BaselineFit;  //!< Gaussian function to fit to #mH1_Baseline to determine baseline

  bool FindBaseline();    //!< Does Gaussian fitting to #mH1_Baseline to determine baseline

 private:
  
  std::string mName;      //!< Name of class

  bool mWindowSum;        //!< true if a sum using #SumWindow() was called
  bool mFitSum;           //!< true if any sum method that uses fitting was called
    
  //Variables to hold sum values.  This is to avoid tedious recomputation
  Int_t mSumAdc;          //!< holds the value from #SumWindow() when it is called
  Double_t mSumAdcFit;    //!< holds the value from a fitting sum method when one is called

  ClassDef(StFcsPulseAna,1)

};

#endif
