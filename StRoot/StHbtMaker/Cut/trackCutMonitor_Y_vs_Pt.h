#ifndef trackCutMonitor_Y_vs_Pt_hh
#define trackCutMonitor_Y_vs_Pt_hh

//#include<string>
#include <typeinfo>
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Base/StHbtCutMonitor.hh"

class trackCutMonitor_Y_vs_Pt : public StHbtCutMonitor{

private:
  StHbt2DHisto*  mHisto; 

public:
  trackCutMonitor_Y_vs_Pt();
  trackCutMonitor_Y_vs_Pt(const char* TitCutMoni, const char* title, 
			 int nbins1 ,double min1, double max1,
			 int nbins2 ,double min2, double max2);
  virtual ~trackCutMonitor_Y_vs_Pt();

  virtual void Fill(const StHbtTrack* track);
  StHbt2DHisto* Histo() {return mHisto;}

 ClassDef(trackCutMonitor_Y_vs_Pt
, 1)
};

#endif
