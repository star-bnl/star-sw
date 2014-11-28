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
 ClassDef(trackCutMonitor_Nhits_vs_NhitsDedx, 1)
#endif
};

#endif
