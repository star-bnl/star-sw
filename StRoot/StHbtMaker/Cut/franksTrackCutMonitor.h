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
#ifdef __ROOT__  
 ClassDef(franksTrackCutMonitor, 1)
#endif
};

#endif
