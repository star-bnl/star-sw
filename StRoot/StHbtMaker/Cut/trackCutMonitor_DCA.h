#ifndef trackCutMonitor_DCA_hh
#define trackCutMonitor_DCA_hh

//#include<string>
//#include <typeinfo>
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

  // These dummy Fill() functions were introduced to remove a compiler
  //   warning related to overloaded base-class Fill() functions being 
  //   hidden by a single version of Fill() in this derived class
  void Fill(const StHbtParticleCollection* d) {;}
  void Fill(const StHbtEvent *d1, const StHbtParticleCollection* d2) {;}
  void Fill(const StHbtPair* d) {;}
  void Fill(const StHbtKink* d) {;}
  void Fill(const StHbtV0* d) {;}
  void Fill(const StHbtEvent* d) {;}

#ifdef __ROOT__ 
 ClassDef(trackCutMonitor_DCA, 1)
#endif
};

#endif
