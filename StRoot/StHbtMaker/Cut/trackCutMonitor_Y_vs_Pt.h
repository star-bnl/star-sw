#ifndef trackCutMonitor_Y_vs_Pt_hh
#define trackCutMonitor_Y_vs_Pt_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Base/StHbtCutMonitor.hh"

class trackCutMonitor_Y_vs_Pt : public StHbtCutMonitor{

private:
  StHbt2DHisto*  mHisto; 
  double mMass;

public:
  trackCutMonitor_Y_vs_Pt();
  trackCutMonitor_Y_vs_Pt(double aMass, 
			  const char* TitCutMoni, const char* title, 
			 int nbins1 ,double min1, double max1,
			 int nbins2 ,double min2, double max2);
  trackCutMonitor_Y_vs_Pt(const char* TitCutMoni, const char* title, 
			 int nbins1 ,double min1, double max1,
			 int nbins2 ,double min2, double max2);
  virtual ~trackCutMonitor_Y_vs_Pt();

  void Fill(const StHbtTrack* track);
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
 ClassDef(trackCutMonitor_Y_vs_Pt, 1)
#endif
};

#endif
