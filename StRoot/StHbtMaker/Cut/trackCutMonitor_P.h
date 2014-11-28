#ifndef trackCutMonitor_P_hh
#define trackCutMonitor_P_hh

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
 ClassDef(trackCutMonitor_P, 1)
#endif
};

#endif
