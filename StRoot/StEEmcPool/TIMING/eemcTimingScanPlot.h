/*
 * eemcTimingScanPlot 
 *
 * \author Dave Relyea
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

  /// Show errorbars (not yet implemented)
  void errors();
  
  private:

  Float_t mAxisMin;
  Float_t mAxisMax;

  Bool_t mNormalize;
  Bool_t mNormalizePreshower;
  Bool_t mErrors;

  
  ClassDef(eemcTimingScanPlot,0)
};

inline void eemcTimingScanPlot::setAxisRange(Float_t min,Float_t max){ mAxisMin=min; mAxisMax=max; }
inline void eemcTimingScanPlot::normalize() { mNormalize=true; }
inline void eemcTimingScanPlot::normalizePreshower() { mNormalizePreshower = true; }
inline void eemcTimingScanPlot::errors() { mErrors=0; Warning("errors","errorbars not yet implemented"); }

#endif
