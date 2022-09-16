/*
Author: David Kapukchyan
@[June 12, 2022]
> Painter now sets the found peak line color

@[May 10, 2022]
> Changed how options work. Graph options now supported. Use semicolon separated list for graph, peak and stat options respectively. Added style options for the various lines. Moved lines for the peak windows to the 'PeakWindow' class.

@[March 23, 2022]
> Found a bug where calling "Draw" on ROOT objects is bad because it adds the TLine objects created by this class to the pad. This means the pad can delete those objects which causes seg fault errors. The proper method to call is "Paint". This is because "Draw" for most ROOT objects only adds objects to the TPad's list so that later the pad can call the "Paint" method of the added class. This means when "Draw" is called on a *PeakAna* object it gets added to the pad so that "Paint" can be called. The "Paint" method will now draw all the lines needed. However, if "Draw" is called the lines are also added to the TPad's list. This is not needed since it is sufficient that only the *PeakAna* class be in the list so that the lines are not deleted by TPad.

 If that paint method involves a "Draw" call then it only adds objects to the pad without calling their paint method

@[March 14, 2022]
> Separated the peak stats box painting method and added options for it

@[March 9, 2022]
> This class will be used to draw PeakAna. It is a first try after reading ROOT code and and seeing that by having a separate painter class is better due to how TCanvas objects work and drawing in ROOT in general works. Also after splitting up the various classes I realized that it really is easier to have a separate painter and analysis class. The painter class essentially assumes a canvas/pad exists and only draws on it the things it needs independent of what else is on the canvas/pad. This is the philosophy to keep in mind when writing painter classes.

Painter class for PeakAna
*/

#ifndef STROOT_STFCSWAVEFORMFITMAKER_PEAKANAPAINTER_H_
#define STROOT_STFCSWAVEFORMFITMAKER_PEAKANAPAINTER_H_

//C++ Headers
#include <vector>
#include <utility> //std::move (Needs C++ 11) and std::pair

//ROOT Headers
#include "TObject.h"
#include "TKey.h"
#include "TH1.h"

//Custom Headers
#include "PeakWindow.h"
#include "PeakAnaVirtualPainter.h"
#include "PeakAna.h"

class PeakAnaPainter : public PeakAnaVirtualPainter
{
public:
  PeakAnaPainter();
  virtual ~PeakAnaPainter();
  
  virtual void Paint(Option_t *opt="");
  virtual void PaintRawData();//Raw data with no modifications
  virtual void PaintFoundPeak();//Raw data inside zoomed in on found signal region
  virtual void PaintFoundPeakQa();//Draw signal and found signal window
  virtual void PaintPeakQa();//Show all found signal windows and signal
  virtual void PaintBaselines();//Just draw the baseline and hitlines
  virtual void PaintFoundRange();//Just draw the found peak on the current pad
  virtual void PaintPeakRanges();//Draw all found peaks on the current pad
  virtual void PaintStats();//Draw Stats box for peak finding
  
  //virtual void Print(Option_t* opt="") const;
  
  virtual void CleanPainter();
  
  virtual void SetPeakAna(PeakAna* ana);
  //virtual const char* GetStatsOption(){ return mStatsOption.Data(); };
  //virtual void SetStatsOption(const char* opt="");
  
  virtual Color_t GetBaseLineColor();
  virtual Style_t GetBaseLineStyle();
  virtual Width_t GetBaseLineWidth();
  
  virtual Color_t GetHitLineColor();
  virtual Style_t GetHitLineStyle();
  virtual Width_t GetHitLineWidth();
  
  virtual void SetBaseLineColor(Color_t color);
  virtual void SetBaseLineColorAlpha(Color_t color,Float_t alpha);
  virtual void SetBaseLineStyle(Style_t style);
  virtual void SetBaseLineWidth(Width_t width);
  
  virtual void SetHitLineColor(Color_t color);
  virtual void SetHitLineColorAlpha(Color_t color,Float_t alpha);
  virtual void SetHitLineStyle(Style_t style);
  virtual void SetHitLineWidth(Width_t width);
  
  bool ValidGraph();
  
protected:
  void Init();//Not virtual since called in constructor
  
  TLine* mTheBaseLine;    //Found baseline value or for ZS data the default in 'AnalyzeForT0'
  TLine* mTheHitLine;     //The hit threshold which is 4\sigma over baseline by default
  
  virtual TPaveText* MakePaveText(Double_t xmin=0.7,Double_t ymin=0.5, Double_t xmax=1.0, Double_t ymax=1.0);
  
  PeakAna* mPA;//PA=PeakAna
  TPaveText* mPaveT_PA;//for custom stats box
  TString mGraphOption;
  TString mPeakOption;
  TString mStatsOption;
  
  //[March 23, 2022]>Need a variable to know if something needs to repainted or not this will prevent multiple calls to delete??
  
private:
  UInt_t mDEBUG;
  
  ClassDef(PeakAnaPainter,2);
};

#endif
