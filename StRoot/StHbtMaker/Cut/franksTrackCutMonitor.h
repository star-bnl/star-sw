#ifndef franksTrackCutMonitor_hh
#define franksTrackCutMonitor_hh

class StHbtTrack;

#include "StHbtMaker/Infrastructure/StHbtHisto.hh"
#include "StHbtMaker/Base/StHbtCutMonitor.hh"

class franksTrackCutMonitor : public StHbtCutMonitor {

private:
  StHbt1DHisto* mDCAxy;
  StHbt1DHisto* mDCAxyGlobal;
  StHbt2DHisto* mPvsDedx;

public:
  franksTrackCutMonitor(const char*);   // default constructor
  franksTrackCutMonitor( const  franksTrackCutMonitor& cutMoni);
  virtual ~franksTrackCutMonitor();

  virtual void Fill(const StHbtTrack* track);

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
 ClassDef(franksTrackCutMonitor, 1)
#endif
};

#endif
