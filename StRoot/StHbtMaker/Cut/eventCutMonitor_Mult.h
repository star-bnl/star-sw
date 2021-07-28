#ifndef eventCutMonitor_Mult_hh
#define eventCutMonitor_Mult_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Base/StHbtCutMonitor.hh"

class eventCutMonitor_Mult : public StHbtCutMonitor{

private:
  StHbt1DHisto*  mHisto; 

public:
  eventCutMonitor_Mult();
  eventCutMonitor_Mult(const char* TitCutMoni, const char* title, int nbins ,double min, double max);
  virtual ~eventCutMonitor_Mult();


  virtual StHbtString Report(); 
  void Fill(const StHbtEvent* event);
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
  void Fill(const StHbtTrack* d) {;}

#ifdef __ROOT__ 
 ClassDef(eventCutMonitor_Mult, 1)
#endif
};

#endif
