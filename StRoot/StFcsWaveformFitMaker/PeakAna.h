/*
Author: David Kapukchyan
@[September 29, 2022]
> Got rid of the virtual painter as it is no longer needed

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

@[January 21, 2022]>Inherit PeakAna from TGraph??
*/
/*!
  The purpose of this class is to find peaks in a sample data and the range of those peaks; i.e. where the first derivative is zero and second derivate changes sign from positive to negative and back to positive as this indicates a local max. 

There is a helper data class #PeakWindow which holds the found peak data.
*/


#ifndef PEAKANA_H
#define PEAKANA_H

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

#include "St_base/StMessMgr.h"

//Custom Headers
#include "PeakWindow.h"
//#include "PeakAnaVirtualPainter.h"
class PeakAnaPainter;

class PeakAna : public TObject
{
public:
  PeakAna();//!< Default constructor that always creates a new TGraph
  PeakAna( int size, double* xvals, double* yvals ); //!< Constructor using array of x and y values with known size
  PeakAna( TGraph* Sig ); //!< Constructor with a known TGraph
  PeakAna( TH1* hist);    //!< Constructor with a histogram, histogram gets converted to graph object
  PeakAna(const PeakAna &OldAna,TGraph* graph=0); //!< Copy constructor can be called with a new graph
  PeakAna& operator=(const PeakAna& rhs);         //!< Assignment operator doesn't clone graph
  virtual ~PeakAna();//!< Destructor
  
  virtual void Copy(TObject& obj) const;              //!< Internal method use Clone instead.
  virtual TObject* Clone(const char* newname) const;  //!< Clones internal graph as opposed to just copying the pointer

  /**@brief Add peak information to a "statistics" box

     "statistics" box can be a TPaveText or a TPaveStats since the latter inherits from the former.
     @param pave TPaveText to write peak information
     @param opt options for what information to add to stats box\n 
       "" (No option) means just show found peak
       "a" means show all found peaks
       "d" means show all detail information for the peak
  */
  virtual void AddPeakStats(TPaveText* pave, const char* opt="");
  
  /** @brief Converts a histogram to a graph

      Treats each bin in the histogram as a point in a TGraph and the bin content as the y-value. Also, can be used to do a running average over "numavgs" bins

      @param graph histogram to be converted to a graph
      @param numavgs number of bins to average over when creating the graph (running average), 1 means no running average
      @return TGraph with bin data as points
  */
  static TGraph* ConvertHistToGraph(TH1* graph, UInt_t numavgs=1);
  
  /**@brief Convert each peak position into a TGraph
     
     The purpose of this function is to help with really noisy data where the "peaks" are really just noise. Essentially the idea is that each "peak" should actually be a point on a kind of "running average" in this way you capture the overall shape of the data without noise.
     @param Peaks vector of PeakWindows to convert to a TGraph
     @return TGraph with each peak in "Peaks" as a point
  */
  static TGraph* ConvertPeaksToGraph(const std::vector<PeakWindow> &Peaks);
  void ConvertPeaksToGraph();  //!< same as #ConvertPeaksToGraph() but creates a new graph that replaces old one
  static PeakAna ConvertPeaksToAna(const PeakAna &Ana);  //!< same as #ConvertPeaksToGraph() but returns a new PeakAna with same settings without modifying old one
  PeakAna ConvertPeaksToAna();//!< same as #ConvertPeaksToAna() but returns new PeakAna with same settings without modifying current one

  /**@brief Finds all possible Peaks in signal with some criteria

     Uses a second derivative test to find all peaks in the data. Found peaks must be above the baseline, and the slope must change by #mBaseline+#mBaselineSigma*#mBaselineSigmaScale. These values can be set with the appropriate function calls. Also, checks each peak's probability and merges peaks below some threshold if set.\n 
     Called by #AnalyzeForPeak();
   */
  virtual void GetPossiblePeaks();
  
  /**@brief searches a vector of #PeakWindow's for a specific peak based on input search criteria

     Search criteria is stored in #mSearch where the #mStartX is the peak location to search for and #mEndX is the tolerance (width) around which the peak should be found.\n
     Called by #AnalyzeForPeak() on #mPeaks

     @param PossiblePeaks vector of peaks to search
     @return index in #mPeaks of found peak that matched search criteria
   */
  virtual Int_t SearchForPeak(const std::vector<PeakWindow> &PossiblePeaks );
  Int_t SearchForPeak(const std::vector<PeakWindow> &PossiblePeaks, const PeakWindow& search);//!< same as #SearchForPeak() but and set search criteria using #PeakWindow
  Int_t SearchForPeak(const std::vector<PeakWindow> &PossiblePeaks, Double_t peak, Double_t width);//!< same as #SearchForPeak() and set search window by peak location and width
  
  //Class methods can be used to store data and preserve found peaks for fitting
  /**@brief Main analysis method for finding peaks

     First filters the data if the #mFilter is set, then calls #GetPossiblePeaks(), then calls #SearchForPeak().
     @return index of found peak, if no found peak then returns size of #mPeaks
  */
  virtual Int_t AnalyzeForPeak();
  virtual Int_t AnalyzeForPeak(Double_t peak, Double_t width);//<! same as #AnalyzeForPeak() but also sets search parameters for peak
  virtual Int_t AnalyzeForNoisyPeak();//<! First calls #ConvertToAna() function and then #AnalyzeForPeak() on the converted data

  /**@brief Merges peaks in #mPeaks using peak probability parameters

     It is intended to be used after calling #AnalyzeForPeak() since it doesn't do this by itself.
     @param newpeaks vector to store merged peaks
  */
  virtual void MergeByProbability(std::vector<PeakWindow>& newpeaks) const;
  virtual void MergeByChirality(std::vector<PeakWindow>& newpeaks) const;
  virtual short MergeLeftOrRight(UInt_t index) const;
    
  Double_t Baseline()const{return mBaseline;}                     //!< @return #mBaseline
  Double_t BaselineSigma()const{return fabs(mBaselineSigma); }    //!< @return fabs(#mBaselineSigma)  
  Double_t BaselineSigmaScale()const{return mBaselineSigmaScale;} //!< @return #mBaselineSigmaScale
  Double_t MinX()const{ return mXRangeMin; }                      //!< @return #mXRangeMin
  Double_t MinY()const{ return mYRangeMin; }                      //!< @return #mYRangeMin
  Double_t MaxX()const{ return mXRangeMax; }                      //!< @return #mXRangeMax
  Double_t MaxY()const{ return mYRangeMax; }                      //!< @return #mYRangeMax
  Double_t SearchPeak()const{ return mSearch.mStartX; }           //!< @return expected peak location for searching
  Double_t SearchWidth()const{ return mSearch.mEndX; }            //!< @return width from expected peak location to search
  Double_t TunnelScale()const{ return mTunnelScale; }             //!< @return #mTunnelScale
  Double_t TunnelSigma()const{ return mTunnelSigma; }             //!< @return #mTunnelSigma
  Double_t TunnelThreshold()const{ return mTunnelThreshold; }     //!< @return #mTunnelThreshold

  /**@brief Draw method for #PeakAna

     Options are semicolon seperated list with graph options first then the peak qa drawing options and finally the stats box drawing option.\n 
     peak qa drawing options(case insensitive):\n 
       - "R" for range\n 
       - "B" for baselines\n 
       - "F" found peak qa\n 
       - "P" for full peak qa\n 
       - "A" for all, which combines "P" and "B"\n 
     stats drawing options(case insensitive):\n
       - "S" is to show just the found peak\n 
       - "A" is to show for all peaks\n 
       - "D" is whether to show detailed printout or not (works with option "S" or "A")

       Example1: "APL;FB;S" means draw graph using option "APL", draw only found peak qa and baselines, and create a stats box showing only basic information about the found peak.
       Example2: ";A;AD" means don't draw graph but draw all peaks and put detailed information of all peaks in a stats box
       Example3: "PL;A" means draw graph with option "PL" and draw all peaks but don't draw a stats box
  */
  virtual void Draw(Option_t *opt="");
  virtual void Paint(Option_t *opt=""); //<! see #Draw() for options
  PeakAnaPainter* GetPainter(Option_t *opt=""); //<! Creates a #PeakAnaPainter if one doesn't exist @return #mPainter
  
  void ForceInternal(){ mInternalSignal=true; } //<! Call this when you need to tell this class to delete the internal TGraph object
  /**@brief Apply a Gaussian filter to the data
     
     Applies a Gaussian filter by sampling "sizeavgs" and Gaussian weights stored in #mFilterWeights.
     @param sizeavgs number of points to sample during filtering
     @param copy if copy is true returns a new #PeakAna with Gaussian filtered data, if false modifies "this" object
     @return new #PeakAna object if copy=true or "this" object if false
  */
  PeakAna* GausFilter(Int_t sizeavgs=0, bool copy=true);
  bool GoodWindow(); //!< Check if found peak is inside #mXRangeMin, #mYRangeMin, #mXRangeMax, #mYRangeMax and has logical values
  virtual TGraph* GetData()const{return mG_Data;}//!< @return #mG_Data
  UInt_t GetDebug()const{return mDEBUG;}         //!< @return #mDEBUG
  const PeakWindow& GetPeak(UInt_t peakidx)const{return mPeaks.at(peakidx);} //!< @return peak at index peakidx 
  PeakWindow& GetPeak(UInt_t peakidx){return mPeaks.at(peakidx);}            //!< @return peak at index peakidx
  UInt_t GetFilter()const{ return mFilter; }                                 //!< @return #mFilter
  Int_t GetFilterScale()const{ return mFilterScale; }                        //!< @return #mFilterScale

  Int_t FoundPeakIndex()const{return mComputedIndex;}                        //!< @return #mComputedIndex
  virtual void GetXYMax(Double_t xmin, Double_t xmax);                       //!< Finds and sets #mMaxX and #mMaxY
  Double_t MaxXval(){if( mMaxX<mXRangeMax ){GetXYMax(mXRangeMin,mXRangeMax); } return mMaxX; } //!< @return #mMaxX
  Double_t MaxYval(){if( mMaxY<mYRangeMin ){GetXYMax(mXRangeMin,mXRangeMax);} return mMaxY; }  //!< @return #mMaxY
  
  /**@brief Apply a Mean filter to the data
     
     Applies a Mean filter by sampling "sizeavgs"
     @param sizeavgs number of points to sample during filtering
     @param copy if copy is true returns a new #PeakAna with Mean filtered data, if false modifies "this" object
     @return new #PeakAna object if copy=true or "this" object if false
  */
  PeakAna* MeanFilter(Int_t sizeavgs=0, bool copy=true);
  int NPeaks()const{return mPeaks.size();}  //!< @return size of #mPeaks
  bool ValidPeakIdx() const;                //!< @return true if 0<= #mComputedIndex < mPeaks.size(), false otherwise
  
  Double_t PeakStart(){if( mComputedIndex<0 ){this->AnalyzeForPeak();} return mFoundPeak.mStartX;}//!< Found Signal starting x-value
  Double_t PeakEnd(){  if( mComputedIndex<0 ){this->AnalyzeForPeak();} return mFoundPeak.mEndX;}  //!< Found Signal ending x-value
  Double_t PeakX();  //!< x-value of found signal peak
  Double_t PeakY();  //!< y-value of found signal peak
  void PeakXY(Double_t &xval, Double_t &yval);  //!< get peak x, and y value directly by reference
  /**@brief test whether a given peak satisifies peak tunnel parameters

     Evaluates a #PeakWindow's probability with function parameters stored in this class, and the internal graph object.
     @param window #PeakWindow to be evaluated
     @return true if peak probability>#mTunnelThreshold, false otherwise
   */
  bool PeakTunnel(const PeakWindow &window) const;
  
  /**@brief compute a given #PeakWindow probability using internal graph

     Evaluates a given #PeakWindow's prbability using internal graph and external parameters for the probability function
     @param window #PeakWindow to compute probability
     @param scale x-value scale for probability, see #mTunnelScale
     @param sigma y-value scale for probability, see #mTunnelSigma
     @return computed probability
   */
  Double_t PeakProb(const PeakWindow& window, Double_t scale, Double_t sigma ) const;
  
  virtual void Print(Option_t* opt="") const; //!< Print peak information
  void ResetPeak();                           //!< Resets values associated with peak finding
  
  void SetDebug(UInt_t level){ mDEBUG=level; }   //!< @param level sets debug level #mDEBUG
  /**@brief sets new data for #PeakAna

     This will change #mG_Data to the graph object passed in and calls #ResetPeak(). Since graph is passed as external parameter this means this object should not delete it in the future so #mInternalSignal is set to false. If you want to change this behavior so this class deletes #mG_Data call #ForceInternal()
     @param graph sets #mG_Data
  */
  virtual void SetData(TGraph* graph);

  /**@brief sets new data for #PeakAna using histogram object

     Works like #SetData() but first calls #ConvertHistToGraph() to convert a histogram to a graph. Since it creates a new graph #mInternalSignal is set to true so that the new graph gets deleted properly. The second argument can be used to do a running average over 'numavgs' number of bins.
     @param hist histogram to use as graph data
     @param numavgs controls how many bins you want to average over when creating graph (1 means no average)
   */
  virtual void SetData(TH1* hist, UInt_t numavgs=1);
  
  void SetBaseline(Double_t base, Double_t sigma);   //!< @param base sets #mBaseline @param sigma sets #mBaselineSigma
  void SetBaselineSigmaScale(Double_t scale);        //!< @param scale sets #mBaselineSigmaScale
  void SetContinuity(Double_t val){ mDeltaX = val; } //!< @param val sets #mDeltaX
  
  /**@brief Set the filter to use when peak finding

     There are two filters that can be used: a Mean filter which just averages over small samples of the data at a time, and a Gaussian filter which does a weighted average over the data samples, where the weights resemble a Gaussian function see #GaussianMatrix2D(). The data sample averaging size is determined by #mFilterScale. Note that a filter does not alter the size of the data.
     @param filter sets #mFilter (0=none, 1=Mean, 2=Gaussian)
     @param scale sets #mFilterScale (number of points to group together when filtering)
     @param sigma sigma of Gaussian to use to compute #mFilterWeights (0 means sigma=mFilterScale/2)
   */
  void SetFilter( UInt_t filter, Int_t scale, Double_t sigma=0 );
  
  /**@brief Sets the absolute range of the data
     
     This sets the varaibles that tell the finder the range of x,y values possible for the data. This kind of range is needed for the algorithm to work properly since it only checks for peaks inside that range.
     @param xmin sets #mXRangeMin
     @param ymin sets #mYRangeMin
     @param xmax sets #mXRangeMax
     @param ymax sets #mYRangeMax
  */
  void SetRange( Double_t xmin, Double_t ymin, Double_t xmax, Double_t ymax);
  
  /**@brief Sets peak search parameters

     This function is needed if you want to pick/check out a particular peak in the vector of #PeakWindows. It allows you to set the x-value of where you expect or want a peak and how far from that ideal position (width) you would like to check for a peak.
     @param peak x-location where you expect or want a peak
     @param width how far in x you expect or want a peak from the ideal position of "peak"
  */
  void SetSearchWindow(Double_t peak, Double_t width);
  
  void SetTunnelScale(Double_t value);                 //!< @param value sets #mTunnelScale
  void SetTunnelSigma(Double_t value);                 //!< @param value sets #mTunnelSigma
  void SetTunnelPars(Double_t scale, Double_t sigma);  //!< @param scale sets #mTunnelScale @param sigma sets #mTunnelSigma
  void SetTunnelThreshold(Double_t value);             //!< @param value sets #mTunnelThreshold
  
  Double_t ChiralityPeakScale() const { return mChiralityPeakScale; }
  Double_t ChiralityScale() const { return mChiralityScale; }
  Double_t ChiralityProbScale() const { return mChiralityProbScale; }
  Double_t ChiralityThreshold() const { return mChiralityThreshold; }
  void SetChiralityPeakScale(Double_t v) { mChiralityPeakScale=v; }
  void SetChiralityScale(Double_t v) { mChiralityScale=v;}
  void SetChiralityProbScale(Double_t v){ mChiralityProbScale=v;}
  void SetChiralityThreshold(Double_t v){ if(v<1){mChiralityThreshold=v;} }
  
  void SetPeak(const Int_t peakpoint, const Double_t peakx );//!< Overwrite peak location in #mFoundpeak. WARNING doesn't set start and end values nor overwrite #mComputedIndex
  void SetWindow(const Int_t start, const Int_t end );//!< Overwrite peak start and end values in #mFoundPeak. WARNING doesn't set peak values nor overwrite #mComputedIndex
  void SetWindow(PeakWindow window); //!< Overwrite #mFoundPeak with a different #PeakWindow. WARNING doesn't overwrite #mComputedIndex
  
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
  void Init();//!< Initialize internal varaibles

  /**@brief Index in #mPeaks where a peak was found

     This variables is negative if #AnalyzeForPeak() was not called.\n 
     It is equal to size of #mPeaks if #AnalyzeForPeak() was called but no peak was found.\n 
     All other times it equals the index in #mPeaks where the peak was found. The found peak is also stored in #mFoundPeak.\n 
     This makes it easy to tell whether #AnalyzeForPeak() was called and whether or not a peak was found.
  */
  Int_t mComputedIndex;
  
  std::vector<PeakWindow> mPeaks; //!< vector that stores all the found peaks in the data
  
  /**@brief Copy of found peak in #mPeaks
     
     This gets set after #SearchForPeak() is called and finds a peak with given search parameters. It is separate for ease of access and so it can be overwritten for testing and debugging purposes
  */
  PeakWindow mFoundPeak;
  
  /**@brief Variable that defines peak search parameters

     The values are stored in a #PeakWindow object but only #PeakWindow::mStartX is used for the peak location and #PeakWindow::mEndX is used for the width. The width represents the +- window from #PeakWindow::mStartX to look for a peak. This is used by #SearchForPeak(). Default is peak at 0 with a width of 1.
   */
  PeakWindow mSearch;

  /**@brief Minimum threshold to search for a peak

     Threshold for peak start and end values meaning y-value must exceed this variable to even check if it contains a peak. y-values below this variable will be ignored. This defines the noise level
  */
  Double_t mBaseline;
  
  /**@brief Error on #PeakAna::mBaseline

     This is used to characterize how much noise there is. It will be used with #PeakAna::mBaseline to determine the threshold y-value for the data. It effectively sets the resolution of the finder.
     This is the standard deviation (sigma) around the baseline for ADCs (Also used as a threshold for the hit) error in baseline (fabs in case sigma is negative), //The sigma will determine the threshold value above baseline('value') i.e. the resolution of the finder
  */
  Double_t mBaselineSigma;
  
    /**@brief scale on #PeakAna::mBaselineSigma to determine final threshold

       This is a multiplicative factor on #PeakAna::mBaselineSigma that is used to determine the final thresholds on the y-value and slope to start the second derivative test in #GetPossiblePeaks(). This "hit" threshold is #PeakAna::mBaseline + #PeakAna::mBaselineSigma * #PeakAna::mBaselineSigmaScale. The default is 4. This threshold also applies to the first positive slope to prevent finding data with small changes as potential peaks.
   */
  Double_t mBaselineSigmaScale;
  
  Double_t mMaxX;//!< x-value where global maximum occurs
  Double_t mMaxY;//!< y-value of global maximum

  /**@brief graph known delta-x
     
     Set this value if you know how far apart each x point in the  data/graph needs to be. This way the finder can handle discontinuities (i.e. each adjacent point is not a fixed x-distance) in the actual data. Default is -1, which means don't check continuity.
   */
  Double_t mDeltaX;
  
  /**@brief TGraph that stores the x,y data
     
     This graph object will not be deleted by this class unless #PeakAna::mInternalSiganl=true
     A TGraph is used because it is easier to process but may want to change to dynamic arrays for speed??
  */
  TGraph* mG_Data;
  
  //A bit redundant but used for internal checks
  Double_t mXRangeMin;  //!< Absolute possible x-value minimum of data
  Double_t mXRangeMax;  //!< Absolute possible x-value maximum of data
  Double_t mYRangeMin;  //!< Absolute possible y-value minimum of data
  Double_t mYRangeMax;  //!< Absolute possible y-value maximum of data
  
  Double_t mTunnelScale;      //!< Scale on exponential for determining tunneling probability (default is 1) see #PeakWindow::PeakTunelProb()
  Double_t mTunnelSigma;      //!< Sigma for Gaussian for determining tunneling probability (default is 1) see #PeakWindow::PeakTunelProb()
  Double_t mTunnelThreshold;  //!< Cutoff probability for peak tunneling method. If threshold less than 0 (default) then skip peak tunnel method, see #PeakTunnel()
  
  Double_t mChiralityPeakScale; //how much to scale the slope of the line formed by the start and end points of the window
  Double_t mChiralityScale;     //modify whether peak position should be near start or end of peak window (see PeakWindow)
  Double_t mChiralityProbScale; //Scale for probablity 
  Double_t mChiralityThreshold; //Threshold chirality for peaks (if abs(chirality)>mChiralityThreshold merge +,- chirality)
  
  /**@brief Filter to use in #AnalyzeForPeak()
     
     see #SetFilter(). Note that filters do not change the size of the data\n 
     - 0 = No Filter (default)\n 
     - 1 = Mean Filter\n 
     - 2 = Gauss Filter/Blur\n 
  */
  UInt_t mFilter;
  Double_t* mFilterWeights;  //!< Array of weights to use in filtering, size is 2*mFilterScale+1
  Int_t mFilterScale;        //!< How many points to group together when applying filter
  
  PeakAnaPainter* mPainter;  //!< Painter for this class

  /**@brief Generates up to a 2D matrix of Gaussian weights

     Generates Gaussian weights to use in Gaussian filtering in a flattened 2D matrix.
     @param rx number of columns to generate weights
     @param sx sigma of Gaussian to use in column weights
     @param ry number of rows to generate weights
     @param sy sigma of Gaussian to use in row weights
     @param kNorm if true use a normalized Gaussian i.e. sum of all weights=1
     @return 1D array containing weights
   */
  static double* GaussianMatrix2D(int rx,  double sx=0, int ry=0, double sy=0, bool kNorm=true);
  
  virtual bool MergeLeft(Double_t leftchir, Double_t thischir, Double_t rightchir) const;
  virtual std::vector< std::pair<int,int> > MergeIndices(std::vector<short>& vec) const;
  
  TString mOption;      //!< Drawing option
  
private:
  UInt_t mDEBUG;        //!< debug level: 0 is none, 1 is info, 2 is verbose, 3 is deep
  bool mInternalSignal; //!< If true will call delete on #mG_Data
  
  ClassDef(PeakAna,3);
};

#endif

