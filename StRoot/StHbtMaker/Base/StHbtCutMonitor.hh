#ifndef StHbtCutMonitor_hh
#define StHbtCutMonitor_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Infrastructure/StHbtV0.hh"

class StHbtCutMonitor{
  
private:
  
public:
  StHbtCutMonitor(){/* no-op */};
  virtual ~StHbtCutMonitor(){/* no-op */};
  virtual StHbtString Report(){ 
    string Stemp = "*** no user defined Fill(const StHbtEvent*), take from base class"; 
    StHbtString returnThis = Stemp;
    return returnThis; 
  }
  virtual void Fill(const StHbtEvent*) { cout << " *** no user defined Fill(const StHbtEvent*), take from base class" << endl; }
  virtual void Fill(const StHbtTrack*) { cout << " *** no user defined Fill(const StHbtTrack*), take from base class" << endl; }
  virtual void Fill(const StHbtV0*) { cout << " *** no user defined Fill(const StHbtV0Track*), take from base class" << endl; }
  virtual void Finish() { cout << " *** no user defined Finish(), take from base class" << endl;}
  virtual void Init() { cout << " *** no user defined Init(), take from base class" << endl;}
};

#endif
