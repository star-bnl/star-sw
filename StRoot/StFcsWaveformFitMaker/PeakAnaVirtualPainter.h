/*
Author: David Kapukchyan
@[March 9, 2022]
> Virtual interface for PeakAnaPainter. This is needed since the non-virtual class runs into "cross-dependency" issues during compilation. The idea is that a virtual interface will get rid of this problem.
> The design is inspired/copied from ROOT's TVirtualHistPainter.

PeakAnaPainter abstract base class
*/

#ifndef STROOT_STFCSWAVEFORMFITMAKER_PEAKANAVIRTUALPAINTER_H_
#define STROOT_STFCSWAVEFORMFITMAKER_PEAKANAVIRTUALPAINTER_H_

//C++ Headers
#include <vector>
#include <utility> //std::move (Needs C++ 11) and std::pair

//ROOT Headers
#include "TROOT.h"
#include "TObject.h"
#include "TPluginManager.h"
#include "TClass.h"
#include "TKey.h"
#include "TH1.h"

//Custom Headers
#include "PeakWindow.h"
//#include "PeakAna.h"

class PeakAna;

class PeakAnaVirtualPainter : public TObject
{
public:
  PeakAnaVirtualPainter();
  virtual ~PeakAnaVirtualPainter();
  
  virtual void Paint(Option_t* opt="") = 0;
  virtual void SetPeakAna(PeakAna* ana) = 0;
  //virtual void Print(Option_t* opt="") const;
  
  virtual void CleanPainter() = 0;
  
  virtual Color_t GetBaseLineColor() = 0;
  virtual Style_t GetBaseLineStyle() = 0;
  virtual Width_t GetBaseLineWidth() = 0;
  
  virtual Color_t GetHitLineColor() = 0;
  virtual Style_t GetHitLineStyle() = 0;
  virtual Width_t GetHitLineWidth() = 0;
  
  virtual void SetBaseLineColor(Color_t color) = 0;
  virtual void SetBaseLineColorAlpha(Color_t color,Float_t alpha) = 0;
  virtual void SetBaseLineStyle(Style_t style) = 0;
  virtual void SetBaseLineWidth(Width_t width) = 0;
  
  virtual void SetHitLineColor(Color_t color) = 0;
  virtual void SetHitLineColorAlpha(Color_t color,Float_t alpha) = 0;
  virtual void SetHitLineStyle(Style_t style) = 0;
  virtual void SetHitLineWidth(Width_t width) = 0;
  
  static PeakAnaVirtualPainter* MakePainter(PeakAna* ana);//@[March 9, 2022]> In future copy from THistPainter not TGraphPainter since TGraphPainter is a singleton.
  static void SetPainter(const char* painter);
  
private:
  static TClass* mpa_Painter;
  
  ClassDef(PeakAnaVirtualPainter,2);
};

#endif
