#ifndef trackCutMonitor_Nhits_vs_NhitsDedx_hh
#define trackCutMonitor_Nhits_vs_NhitsDedx_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Base/StHbtCutMonitor.hh"

class trackCutMonitor_Nhits_vs_NhitsDedx : public StHbtCutMonitor {

private:
  StHbt2DHisto* mHisto;
  int mCharge;

public:
  trackCutMonitor_Nhits_vs_NhitsDedx();   // default constructor
  trackCutMonitor_Nhits_vs_NhitsDedx(const trackCutMonitor_Nhits_vs_NhitsDedx&);   // copy constructor
  trackCutMonitor_Nhits_vs_NhitsDedx(const char* TitCutMoni, const char* title);
  virtual ~trackCutMonitor_Nhits_vs_NhitsDedx();

  virtual void Fill(const StHbtTrack* track);
  StHbt2DHisto* Histo() {return mHisto;}  
#ifdef __ROOT__  
 ClassDef(trackCutMonitor_Nhits_vs_NhitsDedx, 1)
#endif
};

#endif
