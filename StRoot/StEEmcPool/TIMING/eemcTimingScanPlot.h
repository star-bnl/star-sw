/*
 * eemcTimingScanPlot 
 *
 * \author Dave Relyea 
 *
 * $Log: eemcTimingScanPlot.h,v $
 * Revision 1.3  2005/02/01 23:35:46  jwebb
 * Added channel/crate ID's to the plots.
 *
 *
 */

#ifndef eemcTimingScanPlot_H
#define eemcTimingScanPlot_H

#include "StEEmcUtil/EEfeeRaw/EEdims.h"

#include "TObject.h"
#include "TH2.h"

#include <map>
#include <set>
#include <vector>
#include <string>

class eemcTimingScanPlot : public TObject {
  public:

  eemcTimingScanPlot();
  ~eemcTimingScanPlot(){ /* nada */ };
  
  /// Produce timing scan plots from timing scan files
  /// located in the specified directory
  Int_t scan(TString directory);

  /// Set the range of delays to be displayed on the 
  /// x-axis
  void setAxisRange(Float_t min, Float_t max);
  
  /// Normalize timing scan plots for each channel
  /// to a common maximum value.
  /// Pre/postshower boxes are not normalized.
  void normalize();
  /// Normalize pre/postshower boxes to a common
  /// maximum value.
  void normalizePreshower();
  /// Add a TLegend to the plots
  void legend();
  /// Suppress zeros
  void suppressZeros();

  /// Show errorbars (not yet implemented)
  void errors();
  
  private:

  Float_t mAxisMin;
  Float_t mAxisMax;

  Bool_t mNormalize;
  Bool_t mNormalizePreshower;
  Bool_t mErrors;
  Bool_t mLegend;
  Bool_t mSuppressZeros;


  
  ClassDef(eemcTimingScanPlot,0)
};

inline void eemcTimingScanPlot::setAxisRange(Float_t min,Float_t max){ mAxisMin=min; mAxisMax=max; }
inline void eemcTimingScanPlot::normalize() { mNormalize=true; }
inline void eemcTimingScanPlot::normalizePreshower() { mNormalizePreshower = true; }
inline void eemcTimingScanPlot::errors() { mErrors=0; Warning("errors","errorbars not yet implemented"); }
inline void eemcTimingScanPlot::legend() { mLegend=1; }
inline void eemcTimingScanPlot::suppressZeros() { mSuppressZeros = 1; }

#endif
