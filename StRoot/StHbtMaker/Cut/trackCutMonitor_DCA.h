#ifndef trackCutMonitor_DCA_hh
#define trackCutMonitor_DCA_hh

//#include<string>
#include <typeinfo>
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Base/StHbtCutMonitor.hh"

class trackCutMonitor_DCA : public StHbtCutMonitor{

private:
  StHbt1DHisto*  mHisto; 

public:
  trackCutMonitor_DCA();
  trackCutMonitor_DCA(const char* TitCutMoni, const char* title, 
		     int nbins ,double min, double max);
  virtual ~trackCutMonitor_DCA();

  virtual void Fill(const StHbtTrack* track);
  StHbt1DHisto* Histo() {return mHisto;}

#ifdef __ROOT__ 
 ClassDef(trackCutMonitor_DCA, 1)
#endif
};

#endif
