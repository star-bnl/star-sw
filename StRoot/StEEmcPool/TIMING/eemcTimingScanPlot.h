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
  
  // analysis functions
  Int_t scan(TString directory);

  void setAxisRange(Float_t min, Float_t max);
  
  private:

  Float_t mAxisMin;
  Float_t mAxisMax;

  
  ClassDef(eemcTimingScanPlot,0)
};

inline void eemcTimingScanPlot::setAxisRange(Float_t min,Float_t max){ mAxisMin=min; mAxisMax=max; }

#endif
