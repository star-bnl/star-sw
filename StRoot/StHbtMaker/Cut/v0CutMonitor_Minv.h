#ifndef v0CutMonitor_Minv_hh
#define v0CutMonitor_Minv_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtV0.hh"
#include "StHbtMaker/Base/StHbtCutMonitor.hh"

class v0CutMonitor_Minv : public StHbtCutMonitor{

private:
  StHbt1DHisto*  mHistoK0Short; 
  StHbt1DHisto*  mHistoLambda; 
  StHbt1DHisto*  mHistoAntiLambda; 

public:
  v0CutMonitor_Minv();
  v0CutMonitor_Minv(const char* TitCutMoni, const char* title, int nbins ,double min, double max);
  virtual ~v0CutMonitor_Minv();


  virtual StHbtString Report(); 
  virtual void Fill(const StHbtV0* v0);
  virtual void Finish();
  StHbt1DHisto* K0Short()           {return mHistoK0Short;}
  StHbt1DHisto* Lambda()       {return mHistoLambda;}
  StHbt1DHisto* AntiLambda() {return mHistoAntiLambda;}
  
#ifdef __ROOT__ 
 ClassDef(v0CutMonitor_Minv, 1)
#endif
};

#endif
