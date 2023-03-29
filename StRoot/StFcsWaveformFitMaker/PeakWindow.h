/*
Author: David Kapukchyan
@[October 20, 2022]
> Added doxygen style comments

@[June 27, 2022]
> Added a TMarker for the peak position in drawing PeakWindow objects. Also added options to the Draw() and Paint() methods for this object.

@[June 26, 2022]
> Fixed SetPeak to work for the extreme cases where the peak point is the first or last point

@[June 12, 2022]
> Implemented TObject's "Clone" and "Copy" methods in line with *PeakAna* for ease of copying these objects.

@[May 10, 2022]
> Added TLine objects here for drawing. As well they're accessor functions and styling options.

@[February 28, 2022]
> Added Sinh(*mse*) into chirality function as both a multiplicative factor and addition to the "hyperbola" as described in Feb. 25, 2022. It did not work as expected since the "hyperbola" form goes to zero but so does the Sinh(*mse*) and so as a multiplicative factor that means whether the slope is zero or the "hyperbola" is zero you get low chirality since it should not be a one or the either case but a mixture. The sum also did not work as this meant the probablity function maxes at 0.5 instead of 1. The bigger issue is that the "hyperbola" term was dominating the chirality and thus the Sinh(*mse*) was not contributing at all to the chirality. The main issue that arises when one takes away the "hyperbola" part, is that the slope is still not sufficient as noisy data can easily cause the slope to be zero since the start and end points will match up. This is a "false" matching in the sense that the overall behavior of the curve could still be increasing but a large noise level causes the next point to be as low as the start point. In order to solve this one may need to take into account the noise level (e.g. "sigma" in tunnel scale) and treat it as some kind of error on the start and end points and then propagate that to the uncertainty in slope and use that as a part of the chirality ??

@[February 25, 2022]
> Testing of "peak" chirality function of the form (mpe - mse)^2 - (mps - mse)^2/(scale)^2; essentially a hyperbola where *mpe* is slope of line containing points (mPeakX,mPeakY) and (mEndX,mEndY), *mps* is slope of line containing points (mPeakX,mPeakY) and (mStartX,mStartY), *mse* is slope of line containing (mEndY,mEndX) and (mStartX,mStartY); in all cases order of points mattter; only one scale is needed since scale>1 shifts to the "start" and 0<scale<1 shifts to the "end". This form was okay but only measures how far away the peak position is from the center of the window. In future want to also take into account *mse* into the chirality ??

@[February 22, 2022]
> Added a "peak" chirality function and functions to check if a given PeakWindow should be merged based on chirality

@[February 18, 2022]
> Moved *PeakWindow* to it's implementation file as it's become a more composite structure
*/
/*!
  Data class to hold properties of a found peak like the peak position and its start and end points. Mostly used as a helper class for #PeakAna.

  A "peak window" consists of 3 points: where a peak starts (first positive slope after negative slopes leading up to peak), where it plateaus (slope is zero i.e. the peak position), and where the peak ends (last negative slope before changing to positive slopes)
*/

#ifndef PEAKWINDOW_H
#define PEAKWINDOW_H

//C++ Headers
#include <iostream>
#include <vector>
#include <utility> //std::move (Needs C++ 11)

//ROOT Headers
#include "TObject.h"
#include "TKey.h"
#include "TH1.h"
#include "TMarker.h"
#include "TGraph.h"
#include "TLine.h"
#include "TMath.h"

#include "St_base/StMessMgr.h"

class PeakWindow : public TObject{
public:
  /**@brief Default Constructor

     Start and end points of peak are set to a range of 0 to 1 and #mP_Peak, #mPeakX, and #mPeakY equal to -1 (impossible point)
   */
  PeakWindow();
  PeakWindow(Double_t start, Double_t end);     //!< Construct with known start and end points, peak gets set to imposible values
  PeakWindow(const PeakWindow& oldpeak);        //!< Copy Constructor
  PeakWindow& operator=(const PeakWindow& rhs); //!< Assignment operator
  virtual ~PeakWindow();                        //!< Destructor
  
  virtual void Copy(TObject& obj) const;                //!< Only copies variables, to copy TLines use #Clone()
  virtual TObject* Clone(const char* newname="") const; //!< Clone whole object, name is irrelevant
  
  //To ease computation time I store the x,y values of the peak window/range rather than the graph point
  Double_t mStartX; //!< x value for start of the peak window
  Double_t mEndX;   //!< x value for end of the peak window
  Double_t mStartY; //!< y value associated with #mStartX
  Double_t mEndY;   //!< y value associated with #mEndX
  Int_t mP_Peak;    //!< Point Number of peak in a TGraph object (P for point), point is such that slope with previous point will be positive and next point will be negative
  Double_t mPeakX;  //!< x-value of peak position as determined by #SetPeak()
  Double_t mPeakY;  //!< y-value at #mP_Peak
  
  void SetWindow(Double_t s, Double_t e);          //!< @param s set x-value for start of peak @param e set x-value for end of peak
  void GetWindow(Double_t &s, Double_t &e) const;  //!< @param s get x-value for start of peak @param e get x-value for end of peak
  /**@brief sets #mPeakX based on #mP_Peak using line of slopes from points (#mP_Peak-1,#mP_Peak) and (#mP_Peak,#mP_Peak+1)

     This function is used to set #mPeakX to a value from the left and right slopes of #mP_Peak to correct for any discretization coming from points on a graph. Requires #mP_Peak has been set correctly
     @param gdata TGraph where data is stored
  */
  void SetPeak(TGraph* gdata);

  /**@brief combine two #PeakWindow objects

     @param leftpeak one of the peaks to be combined and assumed to have the lower x-value
     @param rightpeak one of the peaks to be combined and assumed to have the higher x-value
     @param keepleft boolean to determine which peak position should be kept in the combined peak, true means "leftpeak", false means "rightpeak"
     @return combined #PeakWindow object is returned
  */
  static PeakWindow Combine(const PeakWindow &leftpeak, const PeakWindow &rightpeak, bool keepleft=true);
  /**@brief merges one #PeakWindow into another

     Changes "this" peak to include the "other" peak.
     @param other #PeakWindow to merge with this one
     @param keepthis true means keep "this" peak's position, false means keep "other" peak's position
   */
  virtual void Combine( const PeakWindow &other, bool keepthis=true );
  
  /**@brief compare two #PeakWindow objects

     Compares two #PeakWindow's and returns a value based on how different they are
     @return comparison flag\n 
       0 = different #PeakWindow's\n 
       1 = same #mStartX and #mStartY only\n 
       2 = "1" + same #mP_Peak only\n 
       3 = "1" + "2" + same #mStartY and #mEndY only\n 
       4 = "1" + "2" + "3" + same #mPeakX only\n 
       5 = same #PeakWindow's ("1"+"2"+"3"+"4"+same #mPeakY)
   */
  virtual UShort_t CompareTo( const PeakWindow& other ) const;
  Double_t StartEndLineSlope() const;     //!< Computes the slope of the line formed by the points (#mStartX,#mStartY) and (#mEndX,#mEndY)
  Double_t StartEndSlopeUncertainty(Double_t sigma) const; //!< Uncertainty int the slope of the line formed by the points (#mStartX,#mStartY) and (#mEndX,#mEndY)
  Double_t StartEndLineYint() const;      //!< Computes the y-intercept of the line formed by the points (#mStartX,#mStartY) and (#mEndX,#mEndY)

  /**@brief Computes the the line formed by the points (#mStartX,#mStartY) and (#mEndX,#mEndY) and evaluates that line at #mPeakX

     The function is mostly needed in computing the probablity for peak tunneling since the difference of #MidPoint() and #mPeakY is used in the probability formula.
     @param graph if graph given use that graph's x-value at #mP_Peak rather than #mPeakX
     @return y-value evaluated at #mPeakX of line formed by points (#mStartX,#mStartY) and (#mEndX,#mEndY)
   */
  Double_t MidPoint(TGraph* graph=0) const;
  virtual Double_t SlopeChirality(Double_t scale) const;//function to compute chirality factor for startendslope
  virtual Double_t PeakChirality(Double_t slopescale, Double_t peakscale) const;//for peakscale 1 means peak is centered in window, peakscale>1 peak center shifted towards start, peakscale<1 peak center shifted towards end (peakscale>0)
  virtual Double_t PeakChiralityProb(Double_t probscale, Double_t chirality) const;
  virtual Double_t PeakChiralityProb(Double_t probscale, Double_t peakscale, Double_t chirscale) const;//Returns probabilty not to merge or probabilty is a "real" peak

  /**@brief Compute probablity that a given #PeakWindow is a real peak
     
     \f{equation}{ Probability = 1/(scale*StartEndDiff^2+1)*Erfc(PeakHeightDiff/(sqrt(2)*sigma)) \f} or 1 if PeakHeightDiff<=0\n
     Erfc = complimentary error function\n 
     StartEndDiff = #mEndX-#mStartX\n 
     PeakHeightDiff = #mPeakY - #MidPoint()\n

     This function will work even if #SetPeak() is not called since it requires a TGraph and will read the values from there

     @param graph TGraph of data points
     @param scale StartEndDiff scale in formula of probability
     @param sigma sigma of Erfc to use in formula of probability 
  */
  virtual Double_t PeakTunnelProb(TGraph* graph, Double_t scale=1.0, Double_t sigma=1.0 ) const;
  //bool ChirMerge(const Rtools::PeakWindow& other, Double_t scalechir=1) const;//returns true if "this" should be merged with "other" PeakWindow according to PeakChirality
  //bool ChirMergeLeft(const Rtools::PeakWindow& left, const Rtools::PeakWindow& right, Double_t scalechir=1) const;//based on chirality - returns true if should be merged with "left" PeakWindow; false if should be merged with "right" PeakWindow
  virtual void Reset(Double_t start, Double_t end);  //!< Reset #PeakWindow to constructor state
  virtual void Print(Option_t* opt="") const;        //!< Prints information about PeakWindow
  
  /**@brief Draw the PeakWindow
     
     @param opt options for drawing:
       - "" (empty string) means draw start line, peak marker and end line.\n 
       - "s" means draw start line\n 
       - "p" means draw peak marker\n 
       - "e" means draw end line
  */
  virtual void Draw(Option_t* opt="");
  virtual void Paint(Option_t* opt=""); //!< paint method see #Draw() for options

  //Options for drawing
  TLine* GetStartLine(Double_t ymin=0, Double_t ymax=0); //!< Create and return a TLine for the start of the peak window
  Color_t GetStartLineColor() const;
  Style_t GetStartLineStyle() const;
  Width_t GetStartLineWidth() const;
  void SetStartLineColor(Color_t color);
  void SetStartLineColorAlpha(Color_t color,Float_t alpha);
  void SetStartLineStyle(Style_t style);
  void SetStartLineWidth(Width_t width);
  
  TMarker* GetPeakMarker();  //!< Create and return a TMarker to mark the location of the peak
  Color_t GetPeakMarkerColor() const;
  Style_t GetPeakMarkerStyle() const;
  Size_t GetPeakMarkerSize() const;
  void SetPeakMarkerColor(Color_t color);
  void SetPeakMarkerColorAlpha(Color_t color, Float_t alpha);
  void SetPeakMarkerStyle(Style_t style);
  void SetPeakMarkerSize(Size_t size);
  
  TLine* GetEndLine(Double_t ymin=0, Double_t ymax=0); //!< Create and return a TLine for the end of the peak window
  Color_t GetEndLineColor() const;
  Style_t GetEndLineStyle() const;
  Width_t GetEndLineWidth() const;
  void SetEndLineColor(Color_t color);
  void SetEndLineColorAlpha(Color_t color,Float_t alpha);
  void SetEndLineStyle(Style_t style);
  void SetEndLineWidth(Width_t width);
  
protected:
  TLine* mStartLine;     //!< TLine for drawing the start of the peak window
  TMarker* mPeakMarker;  //!< TMarker for drawing the peak location
  TLine* mEndLine;       //!< TLine for drawing the end of the peak window
  
  ClassDef(PeakWindow,3);
};

#endif
