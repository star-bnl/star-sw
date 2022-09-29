/*
Author: David Kapukchyan
@[August 15, 2022]
> Small fix to GetPossiblePeaks() so that it does not stop at slope==0 on the decreasing part of the peak

@[June 27, 2022]
> Correctly sets x value of the end point of the found peak. This can result in overlaps when drawing the peak lines.

@[June 26, 2022]
> Changed GetPossiblePeaks() to check if dealing with last point. If there is an active start time and loop reaches last graph point, last graph point becomes the end of that peak window.

@[June 23, 2022]
> Added MergeByProbability() which can be used to merge peaks by probability after GetPossiblePeaks() has been called.

@[June 12, 2022]
> Implemented TObject's "Clone" and "Copy" functions so that "DrawClone" will correctly create a clone of this object for drawing purposes. Also removed setting the found peak line color from 'AnalyzeForPeak()' to *PeakAnaPainter*

@[June 8, 2022]
> Implemented a Gaussian Filter.

@[May 10, 2022]
> Implemented the ability to color and style the graph and peak lines. Changed 'GetPossiblePeaks' to use swap rather than return a vector. 'GetPeak' returns a reference to the PeakWindow since not returning a reference actually returned a copy

@[May 9, 2022]
> Implemented the "Mean" filter from Rtools into this class, and made "filter" variables. Also got rid of 'scale' in 'AnalyzeForPeak' and made it its own variable 'BaselineSigmaScale'

@[March 9, 2022]
> Moved the draw functions to their own separate painter class

@[February 25, 2022]
> Added methods for using the peak "chirality" from peak window

@[February 18, 2022]
> Moved *PeakWindow* to it's own implementation file

@[February 7, 2022]
> Added ConvertToGraph/Ana functions that is useful for super noisy data where each PeakWindow can be thought of as a point in some graph. This graph essentially captures the shape of the data without the noise.

@[February 2, 2022]
> Changed the probability function for peak window from Exp[-x]*Erfc[y] to 1/(x^2+1)*Erfc[y]. This was done as Exp[-x] was going to zero too quickly and thus made it difficult to find good parameters with reasonable probabilites. Also made some modifications to how things are drawn

@[January 29, 2022]
> Changes to how drawing functions work and a compare for PeakWindow

@[January 28, 2022]
> Fixes to peak tunnel method includng adding a tunnel sigma variable since using baseline sigma did not work as expected

@[January 24, 2022]
> Added draw methods

@[January 23, 2022]
> Added the peak tunneling method for skipping over noise data

@[January 19, 2022]
> First instance of this class that is adopted from work on *StFcsPulseFit*

The purpose of this class is to find peaks in a sample data and the range of those peaks; i.e. where the first derivative is zero and second derivate changes sign from positive to negative and back to positive as this indicates a local max. 

There is a helper data class *PeakWindow* which holds the data and does not inherit from TObject

@[January 21, 2022]>Inherit PeakAna from TGraph??


*/

#ifndef STROOT_STFCSWAVEFORMFITMAKER_PEAKANA_H_
#define STROOT_STFCSWAVEFORMFITMAKER_PEAKANA_H_

//C++ Headers
#include <iostream>
#include <vector>
#include <utility> //std::move (Needs C++ 11) and std::pair
#include <iterator>

//ROOT Headers
#include "TObject.h"
#include "TAttLine.h"
#include "TKey.h"
#include "TH1.h"
#include "TPaveText.h"

//Custom Headers
#include "PeakWindow.h"
//#include "PeakAnaVirtualPainter.h"
class PeakAnaPainter;

class PeakAna : public TObject
{
public:
  PeakAna();
  PeakAna( int size, double* xvals, double* yvals );//Array of x and y values with known size
  PeakAna( TGraph* Sig );//Construct with a known TGraph
  PeakAna( TH1* hist);
  PeakAna(const PeakAna &OldAna,TGraph* graph=0); //Copy Constructor
  PeakAna& operator=(const PeakAna& rhs);//Assignment operator
  virtual ~PeakAna();
  
  virtual void Copy(TObject& obj) const;//Clone graph as opposed to just copying the pointer
  virtual TObject* Clone(const char* newname) const;
  
  virtual void AddPeakStats(TPaveText* pave, const char* opt="");
  //Peak finding methods are independent of the class
  static TGraph* ConvertHistToGraph(TH1* graph, UInt_t numavgs=1);//The second argument can be used to do a running average over 'numavgs' number of bins (1 means no average)
  static TGraph* ConvertPeaksToGraph(const std::vector<PeakWindow> &Peaks);//The purpose of this function is to help with really noisy data where the "peaks" are really just noise. Essentially the idea is that each "peak" should actually be a point on a kind of "running average" in this way you capture the overall shape of the data without noise
  void ConvertPeaksToGraph();//same as above but creates new graph that replaces old one
  static PeakAna* ConvertPeaksToAna(const PeakAna &Ana);//same as above but returns new PeakAna with same settings without modifying old one
  PeakAna* ConvertPeaksToAna();//same as above but returns new PeakAna with same settings without modifying current one
  
  virtual void GetPossiblePeaks();//Finds all possible Peaks in signal with some criteria
  virtual Int_t SearchForPeak(const std::vector<PeakWindow> &PossiblePeaks );
  Int_t SearchForPeak(const std::vector<PeakWindow> &PossiblePeaks, const PeakWindow& search);//Returns index for vector where a peak that matches search criteria
  Int_t SearchForPeak(const std::vector<PeakWindow> &PossiblePeaks, Double_t peak, Double_t width);//Returns index for vector where a peak that matches search criteria
  
  //Class methods can be used to store data and preserve found peaks for fitting
  virtual Int_t AnalyzeForPeak();//check for Peaks above Baseline+BaselineSigma*BaselineSigmaScale. returns index of found peak
  virtual Int_t AnalyzeForPeak(Double_t peak, Double_t width);//set search parameters along with finding peak
  virtual Int_t AnalyzeForNoisyPeak();//Analyze using the ConvertToAna functions
  
  virtual void MergeByProbability(std::vector<PeakWindow>& newpeaks) const;//Merges peaks in mPeaks using tunnel parameters (Doesn't call "GetPossiblePeaks")
  virtual void MergeByChirality(std::vector<PeakWindow>& newpeaks) const;
  virtual short MergeLeftOrRight(UInt_t index) const;
    
  Double_t Baseline()const{return mBaseline;}//Look for peaks above the baseline
  Double_t BaselineSigma()const{return fabs(mBaselineSigma); }//fabs in case sigma is negative
  Double_t BaselineSigmaScale()const{return mBaselineSigmaScale;}
  Double_t MinX()const{ return mXRangeMin; }
  Double_t MinY()const{ return mYRangeMin; }
  Double_t MaxX()const{ return mXRangeMax; }
  Double_t MaxY()const{ return mYRangeMax; }
  Double_t SearchPeak()const{ return mSearch.mStartX; }
  Double_t SearchWidth()const{ return mSearch.mEndX; }
  Double_t TunnelScale()const{ return mTunnelScale; }
  Double_t TunnelSigma()const{ return mTunnelSigma; }
  Double_t TunnelThreshold()const{ return mTunnelThreshold; }
  
  virtual void Draw(Option_t *opt="");//Options are semicolon seperated list with graph options first then the peak qa drawing options and finally the stats box drawing option
  //peak qa drawing options(case insensitive): *R* for range,
  //                         *B* for baselines,
  //                         *F* found peak qa,
  //                         *P* for full peak qa,
  //                         *A* for all which combines "P" and "B"
  //stats drawing options(case insensitive): *S* is to show just the found peak,
  //                       *A* is to show for all peaks,
  //                       *D* is whether to show detailed printout or not (works with option *S* or *A*)
  virtual void Paint(Option_t *opt="");
  PeakAnaPainter* GetPainter(Option_t *opt="");
  
  void ForceInternal(){ mInternalSignal=true; }//Call this to force this class to delete the internal TGraph object
  PeakAna* GausFilter(Int_t sizeavgs=0, bool copy=true);//If 'copy'=true return a new "PeakAna" that has Gaussian Filtered data.
  bool GoodWindow(); //Check if found peak is inside x range
  virtual TGraph* GetData()const{return mG_Data;}
  UInt_t GetDebug()const{return mDEBUG;}//debug level 0 is none, 1 is info, 2 is verbose, 3 is deep
  const PeakWindow& GetPeak(UInt_t peakidx)const{return mPeaks.at(peakidx);}
  PeakWindow& GetPeak(UInt_t peakidx){return mPeaks.at(peakidx);}
  UInt_t GetFilter()const{ return mFilter; }
  Int_t GetFilterScale()const{ return mFilterScale; }
  //Double_t GetFilterSigma()const{ return mFilterSigma; }
  Int_t FoundPeakIndex()const{return mComputedIndex;}
  virtual void GetXYMax(Double_t xmin, Double_t xmax);
  Double_t MaxXval(){if( mMaxX<mXRangeMax ){GetXYMax(mXRangeMin,mXRangeMax); } return mMaxX; }
  Double_t MaxYval(){if( mMaxY<mYRangeMin ){GetXYMax(mXRangeMin,mXRangeMax);} return mMaxY; }
  PeakAna* MeanFilter(Int_t sizeavgs=0, bool copy=true);//If 'copy'=true return a new "PeakAna" that has MeanFiltered data. if 'copy=false' modify internal graph and return itself
  int NPeaks()const{return mPeaks.size();}
  bool ValidPeakIdx() const;
  
  Double_t PeakStart(){if( mComputedIndex<0 ){this->AnalyzeForPeak();} return mFoundPeak.mStartX;}//Found Signal Start time
  Double_t PeakEnd(){  if( mComputedIndex<0 ){this->AnalyzeForPeak();} return mFoundPeak.mEndX;}//Found Signal End time
  Double_t PeakX();//x-value of found signal peak
  Double_t PeakY();//y-value of found signal peak
  void PeakXY(Double_t &xval, Double_t &yval);
  bool PeakTunnel(const PeakWindow &window) const;
  Double_t PeakProb(const PeakWindow& windw, Double_t scale, Double_t sigma ) const;
  
  virtual void Print(Option_t* opt="") const;
  void ResetPeak();//Resets values associated with peak finding
  
  void SetDebug(UInt_t level){ mDEBUG=level; }
  virtual void SetData(TGraph* graph);//The second argument can be used to do a running average over 'numavgs' number of bins (1 means no average)
  virtual void SetData(TH1* hist, UInt_t numavgs=1);//Note:The histogram will not be deleted but the internal graph object created from the histogram will be deleted
  
  void SetBaseline(Double_t base, Double_t sigma);//The sigma will determine the threshold value above baseline('value') i.e. the resolution of the finder
  void SetBaselineSigmaScale(Double_t scale);//This scale will determine how much to multiply BaselineSigma to get a y-value threshold
  void SetContinuity(Double_t val){ mDeltaX = val; }
  void SetFilter( UInt_t filter, Int_t scale, Double_t sigma=0 );//Set the filter to use when peak finding.
  void SetRange( Double_t xmin, Double_t ymin, Double_t xmax, Double_t ymax);//This needs to be set based on the data set used otherwise algorithm won't work properly)
  void SetSearchWindow(Double_t peak, Double_t width);//Set x-value to search for and the +- range around that value to search for
  void SetTunnelScale(Double_t value);
  void SetTunnelSigma(Double_t value);
  void SetTunnelPars(Double_t scale, Double_t sigma);
  void SetTunnelThreshold(Double_t value);
  
  Double_t ChiralityPeakScale() const { return mChiralityPeakScale; }
  Double_t ChiralityScale() const { return mChiralityScale; }
  Double_t ChiralityProbScale() const { return mChiralityProbScale; }
  Double_t ChiralityThreshold() const { return mChiralityThreshold; }
  void SetChiralityPeakScale(Double_t v) { mChiralityPeakScale=v; }
  void SetChiralityScale(Double_t v) { mChiralityScale=v;}
  void SetChiralityProbScale(Double_t v){ mChiralityProbScale=v;}
  void SetChiralityThreshold(Double_t v){ if(v<1){mChiralityThreshold=v;} }
  
  void SetPeak(const Int_t peakpoint, const Double_t peakx );//Use known values to set the peak WARNING doesn't set start and end values nor overwrite 'mComputedIndex'
  void SetWindow(const Int_t start, const Int_t end );//Set peak window to custom values. WARNING doesn't set peak values nor overwrite 'mComputedIndex'
  void SetWindow(PeakWindow window);
  
  //Styling for graph
  Color_t GetLineColor() const;
  Style_t GetLineStyle() const;
  Width_t GetLineWidth() const;
  
  Color_t GetFillColor() const;
  Style_t GetFillStyle() const;
  
  Color_t GetMarkerColor() const;
  Size_t GetMarkerSize() const;
  Style_t GetMarkerStyle() const;
  
  void SetLineColor(Color_t color);
  void SetLineColorAlpha(Color_t color, Float_t alpha);
  void SetLineStyle(Style_t style);
  void SetLineWidth(Width_t width);
  
  void SetFillColor(Color_t color);
  void SetFillColorAlpha(Color_t color, Float_t alpha);
  void SetFillStyle(Style_t style);
  
  void SetMarkerColor(Color_t color=1);
  void SetMarkerColorAlpha(Color_t color, Float_t alpha);
  void SetMarkerSize(Size_t size=1);
  void SetMarkerStyle(Style_t style=1);
  
  //Styling for peak painter
  Color_t GetBaseLineColor() const;
  Style_t GetBaseLineStyle() const;
  Width_t GetBaseLineWidth() const;
  
  Color_t GetHitLineColor() const;
  Style_t GetHitLineStyle() const;
  Width_t GetHitLineWidth() const;
  
  void SetBaseLineColor(Color_t color);
  void SetBaseLineColorAlpha(Color_t color,Float_t alpha);
  void SetBaseLineStyle(Style_t style);
  void SetBaseLineWidth(Width_t width);
  
  void SetHitLineColor(Color_t color);
  void SetHitLineColorAlpha(Color_t color,Float_t alpha);
  void SetHitLineStyle(Style_t style);
  void SetHitLineWidth(Width_t width);
  
  Color_t GetPeakLineColorS(UInt_t peakidx) const;
  Color_t GetPeakLineColorE(UInt_t peakidx) const;
  Style_t GetPeakStyleS(UInt_t peakidx) const;
  Style_t GetPeakStyleE(UInt_t peakidx) const;
  Width_t GetPeakWidthS(UInt_t peakidx) const;
  Width_t GetPeakWidthE(UInt_t peakidx) const;
  
  Color_t GetFoundPeakLineColorS() const;
  Color_t GetFoundPeakLineColorE() const;
  Style_t GetFoundPeakStyleS() const;
  Style_t GetFoundPeakStyleE() const;
  Width_t GetFoundPeakWidthS() const;
  Width_t GetFoundPeakWidthE() const;
  
  void SetFoundPeakLineColorS(Color_t s_color);
  void SetFoundPeakLineColorE(Color_t e_color);
  void SetFoundPeakLineColor(Color_t s_color, Color_t e_color);
  void SetFoundPeakStyleS(Style_t s_style);
  void SetFoundPeakStyleE(Style_t e_style);
  void SetFoundPeakStyle(Style_t s_style,Style_t e_style);
  void SetFoundPeakWidthS(Width_t s_width);
  void SetFoundPeakWidthE(Width_t e_width);
  void SetFoundPeakWidth(Width_t s_width, Width_t e_width);
  
  void SetPeakLineColorS(UInt_t peakidx, Color_t s_color);
  void SetPeakLineColorE(UInt_t peakidx, Color_t e_color);
  void SetPeakLineColor(UInt_t peakidx, Color_t s_color, Color_t e_color);
  void SetPeakLineColorAlphaS(UInt_t peakidx, Color_t s_color,Float_t s_alpha);
  void SetPeakLineColorAlphaE(UInt_t peakidx, Color_t e_color,Float_t e_alpha);
  void SetPeakLineColorAlpha(UInt_t peakidx, Color_t s_color,Float_t s_alpha, Color_t e_color, Float_t e_alpha);
  void SetPeakLineStyleS(UInt_t peakidx, Style_t s_style);
  void SetPeakLineStyleE(UInt_t peakidx, Style_t e_style);
  void SetPeakLineStyle(UInt_t peakidx, Style_t s_style, Style_t e_style);
  void SetPeakLineWidthS(UInt_t peakidx, Width_t s_width);
  void SetPeakLineWidthE(UInt_t peakidx, Width_t e_width);
  void SetPeakLineWidth(UInt_t peakidx, Width_t s_width, Width_t e_width);
  
  void SetAllPeakLineColor(Color_t s_color, Color_t e_color);
  void SetAllPeakLineStyle(Style_t s_style, Color_t e_style);
  void SetAllPeakLineWidth(Width_t s_width, Width_t e_width);
  
protected:
  void Init();//Not virtual since called in constructor
    
  Int_t mComputedIndex;  //Index of found peak and also will be used to know if 'AnalyzeForPeaks' was called; this will keep from having to determine peaks every time. It will be negative if 'AnalyzeForPeaks' was not called and one more than the size of the peaks vector if no peak was found.
  //bool mFindBase;  //This will be used to know whether baseline needs to be found or not
  //int mNPeaks;  //Number of peaks found
  
  std::vector<PeakWindow> mPeaks;
  PeakWindow mFoundPeak;//This is the signal start time and hold the value of the histogram bin that corresponds to this signal start time that is why it is an 'Int_t'. This is needed in addtion to the index to prevent unnecessary copies if a function returns the found peak at the found index
  PeakWindow mSearch;//In this struct the 'start' represents the start time to look for and the 'end' represents the +- window from 'start' to look for
  Double_t mBaseline;//This is the basline for the signal (For physics data in Zero Suppression bank should be zero)
  Double_t mBaselineSigma;//This is the standard deviation (sigma) around the baseline for ADCs (Also used as a threshold for the hit)
  Double_t mBaselineSigmaScale;//Scale on BaselineSigma on which to start looking for peaks. e.g. y-value>=baseline+baselinesigmascale*baselinesigma (default 4.0)
  Double_t mMaxX;//x-value where global maximum occurs
  Double_t mMaxY;//Global maximum
  Double_t mDeltaX;//Graph known delta-x, for graphs that may not be continous i.e. each adjacent point is not a fixed a x-distance. <=0 means don't check continuity
  
  //Main object that holds raw infomration
  TGraph* mG_Data; //TGraph's easier to process but may want to change to dynamic arrays for speed??
  
  //Main fit functions
  //TF1*  mF1_SignalFit; ??In the future make a class that inherits from this class that will do fitting too (Functionalities:Fit all peaks to same function, do global fit)??
  Double_t mXRangeMin;
  Double_t mXRangeMax;
  //A bit redundant but used for internal checks
  Double_t mYRangeMin;
  Double_t mYRangeMax;
  
  Double_t mTunnelScale;//Scale on exponential for determining tunneling probability (default is 1)
  Double_t mTunnelSigma;//Sigma for Gaussian for determining tunneling probability (default is 1)
  Double_t mTunnelThreshold;//If threshold less than 0 (default) then skip peak tunnel method
  
  Double_t mChiralityPeakScale;//how much to scale the slope of the line formed by the start and end points of the window
  Double_t mChiralityScale;//modify whether peak position should be near start or end of peak window (see PeakWindow)
  Double_t mChiralityProbScale;//Scale for probablity 
  Double_t mChiralityThreshold;//Threshold chirality for peaks (if abs(chirality)>mChiralityThreshold merge +,- chirality)
  
  UInt_t mFilter;//Select filter to use in AnalyzeForPeak. Note that filters should not change the size of the data
  //- 0 = No Filter (default)
  //- 1 = Mean Filter
  //- 2 = Gauss Filter/Blur
  Double_t* mFilterWeights;//weight array to use in filtering, size is 2*mFilterScale+1
  Int_t mFilterScale;//Filter scale, i.e. how many points to group together when applying filter
  //Double_t mFilterSigma;//Filter sigma, i.e. sigma to use to use for Gaussian Filter (0 means sigma=mFilterScale/2)
  
  PeakAnaPainter* mPainter;
  
  static double* GaussianMatrix2D(int rx,  double sx=0, int ry=0, double sy=0, bool kNorm=true);
  
  virtual bool MergeLeft(Double_t leftchir, Double_t thischir, Double_t rightchir) const;
  virtual std::vector< std::pair<int,int> > MergeIndices(std::vector<short>& vec) const;
  
  TString mOption;
  
private:
  UInt_t mDEBUG;
  bool mInternalSignal;
  
  ClassDef(PeakAna,3);
};

#endif
