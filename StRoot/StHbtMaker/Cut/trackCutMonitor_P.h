#ifndef trackCutMonitor_P_hh
#define trackCutMonitor_P_hh

//#include<string>
#include <typeinfo>
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Base/StHbtCutMonitor.hh"

class trackCutMonitor_P : public StHbtCutMonitor{

private:
  StHbt1DHisto*  mHisto; 

public:
  trackCutMonitor_P();
  trackCutMonitor_P(const char* TitCutMoni, const char* title, int nbins ,double min, double max);
  virtual ~trackCutMonitor_P();


  virtual StHbtString Report(); 
  virtual void Fill(const StHbtTrack* track);
  virtual void Finish();
  StHbt1DHisto* Histo() {return mHisto;}
  
#ifdef __ROOT__ 
 ClassDef(trackCutMonitor_P, 1)
#endif
};

#endif
