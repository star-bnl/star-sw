#ifndef trackCutMonitor_Pt_hh
#define trackCutMonitor_Pt_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Base/StHbtCutMonitor.hh"

class trackCutMonitor_Pt : public StHbtCutMonitor{

private:
  StHbt1DHisto*  mHisto; 

public:
  trackCutMonitor_Pt();
  trackCutMonitor_Pt(const char* TitCutMoni, const char* title, int nbins ,double min, double max);
  virtual ~trackCutMonitor_Pt();

  virtual void Fill(const StHbtTrack* track);
  virtual StHbt1DHisto* Histo() {return mHisto;}

#ifdef __ROOT__ 
 ClassDef(trackCutMonitor_Pt, 1)
#endif
};

#endif
