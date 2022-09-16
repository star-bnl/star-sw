/*
Author: David Kapukchyan
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

Helper class for PeakAna
*/

#ifndef STROOT_STFCSWAVEFORMFITMAKER_PEAKWINDOW_H_
#define STROOT_STFCSWAVEFORMFITMAKER_PEAKWINDOW_H_

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

//This class is for holding the peak position information
class PeakWindow : public TObject{
public:
  PeakWindow();//Default values are range 0 to 1 with P_Peak and Peak equal to -1 (i.e. impossible point)
  PeakWindow(Double_t start, Double_t end);//point and peak gets set to imposible values
  PeakWindow(const PeakWindow& oldpeak); //Copy Constructor
  PeakWindow& operator=(const PeakWindow& rhs);//Assignment operator
  virtual ~PeakWindow();
  
  virtual void Copy(TObject& obj) const;
  virtual TObject* Clone(const char* newname="") const;//Not named objects
  
  //To ease computation time I store the x,y values of the peak range rather than the graph point
  Double_t mStartX; Double_t mEndX;//x values of where the peak is located
  Double_t mStartY; Double_t mEndY;//y values of the peak
  Int_t mP_Peak;//Point Number of peak (P for point), value contains point such that slope with previous point will be positive and next point will be negative
  Double_t mPeakX;//Actual Peak position determined by making line of slopes from (P_Peak,P_Peak+1) and (P_Peak+1,P_Peak+2)
  Double_t mPeakY;//y value at mP_Peak
  
  void SetWindow(Double_t s, Double_t e);
  void GetWindow(Double_t &s, Double_t &e) const;
  void SetPeak(TGraph* gdata);//Requires P_Peak has been set correctly
  
  static PeakWindow Combine(const PeakWindow &leftpeak, const PeakWindow &rightpeak, bool keepleft=true);
  virtual void Combine( const PeakWindow &other, bool keepthis=true );
  virtual UShort_t CompareTo( const PeakWindow& other ) const;
  Double_t StartEndLineSlope() const;
  Double_t StartEndSlopeUncertainty(Double_t sigma) const;
  Double_t StartEndLineYint() const;
  //void StartEndLine(Double_t& x, Double_t& y) const;
  Double_t MidPoint(TGraph* graph=0) const;
  virtual Double_t SlopeChirality(Double_t scale) const;//function to compute chirality factor for startendslope
  virtual Double_t PeakChirality(Double_t slopescale, Double_t peakscale) const;//for peakscale 1 means peak is centered in window, peakscale>1 peak center shifted towards start, peakscale<1 peak center shifted towards end (peakscale>0)
  virtual Double_t PeakChiralityProb(Double_t probscale, Double_t chirality) const;
  virtual Double_t PeakChiralityProb(Double_t probscale, Double_t peakscale, Double_t chirscale) const;//Returns probabilty not to merge or probabilty is a "real" peak
  virtual Double_t PeakTunnelProb(TGraph* graph, Double_t scale=1.0, Double_t sigma=1.0 ) const;//Probablity is Exp(-scale*StartEndDiff)*Erfc(PeakHeightDiff/(sqrt(2)*s)) or 1 if peakheightdiff<=0
  //bool ChirMerge(const Rtools::PeakWindow& other, Double_t scalechir=1) const;//returns true if "this" should be merged with "other" PeakWindow according to PeakChirality
  //bool ChirMergeLeft(const Rtools::PeakWindow& left, const Rtools::PeakWindow& right, Double_t scalechir=1) const;//based on chirality - returns true if should be merged with "left" PeakWindow; false if should be merged with "right" PeakWindow
  virtual void Reset(Double_t start, Double_t end);
  virtual void Print(Option_t* opt="") const;
  
  //Functions for TLine and drawing
  //Options include:
  // - "" (empty string) means draw start line, peak marker and end line.
  // - "s" means draw start line
  // - "p" means draw peak marker
  // - "e" means draw end line
  virtual void Draw(Option_t* opt="");
  virtual void Paint(Option_t* opt="");
  
  TLine* GetStartLine(Double_t ymin=0, Double_t ymax=0);
  Color_t GetStartLineColor() const;
  Style_t GetStartLineStyle() const;
  Width_t GetStartLineWidth() const;
  void SetStartLineColor(Color_t color);
  void SetStartLineColorAlpha(Color_t color,Float_t alpha);
  void SetStartLineStyle(Style_t style);
  void SetStartLineWidth(Width_t width);
  
  TMarker* GetPeakMarker();
  Color_t GetPeakMarkerColor() const;
  Style_t GetPeakMarkerStyle() const;
  Size_t GetPeakMarkerSize() const;
  void SetPeakMarkerColor(Color_t color);
  void SetPeakMarkerColorAlpha(Color_t color, Float_t alpha);
  void SetPeakMarkerStyle(Style_t style);
  void SetPeakMarkerSize(Size_t size);
  
  TLine* GetEndLine(Double_t ymin=0, Double_t ymax=0);
  Color_t GetEndLineColor() const;
  Style_t GetEndLineStyle() const;
  Width_t GetEndLineWidth() const;
  void SetEndLineColor(Color_t color);
  void SetEndLineColorAlpha(Color_t color,Float_t alpha);
  void SetEndLineStyle(Style_t style);
  void SetEndLineWidth(Width_t width);
  
protected:
  TLine* mStartLine;
  TMarker* mPeakMarker;
  TLine* mEndLine;
  
  ClassDef(PeakWindow,3);
};

#endif
