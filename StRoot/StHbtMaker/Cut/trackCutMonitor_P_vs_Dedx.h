#ifndef trackCutMonitor_P_vs_Dedx_hh
#define trackCutMonitor_P_vs_Dedx_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Base/StHbtCutMonitor.hh"

class trackCutMonitor_P_vs_Dedx : public StHbtCutMonitor {

private:
  StHbt2DHisto* mHisto;
  int mCharge;

public:
  trackCutMonitor_P_vs_Dedx();   // default constructor
  trackCutMonitor_P_vs_Dedx(const trackCutMonitor_P_vs_Dedx&);   // copy constructor
  trackCutMonitor_P_vs_Dedx(const trackCutMonitor_P_vs_Dedx&, int charge);   // copy constructor with charge setting
  trackCutMonitor_P_vs_Dedx(int charge);
  trackCutMonitor_P_vs_Dedx(int charge, const char* TitCutMoni, const char* title, 
			   int nbins1 ,double min1, double max1,
			   int nbins2 ,double min2, double max2);
  virtual ~trackCutMonitor_P_vs_Dedx();

  virtual void Fill(const StHbtTrack* track);
  StHbt2DHisto* Histo() {return mHisto;}  
#ifdef __ROOT__  
 ClassDef(trackCutMonitor_P_vs_Dedx, 1)
#endif
};

#endif
