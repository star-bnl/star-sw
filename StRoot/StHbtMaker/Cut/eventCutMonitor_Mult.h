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
  virtual void Fill(const StHbtEvent* event);
  virtual void Finish();
  StHbt1DHisto* Histo() {return mHisto;}
  
#ifdef __ROOT__ 
 ClassDef(eventCutMonitor_Mult, 1)
#endif
};

#endif
