#ifndef _STAR_JEVP_FMS_BUILDER_
#define _STAR_JEVP_FMS_BUILDER_

#include <list>
#include <map>
#include <math.h>

#include "JevpBuilder.h"

class TH1;
class daqReader;
class JevpPlot;

/**
 Builder creating and filling plots for FMS in Jevp.
 */
class fmsBuilder : public JevpBuilder {

public:

  /**
   Constructor
   */
  fmsBuilder(JevpServer* parent=NULL);
  
  /**
   Destructor
   */
  virtual ~fmsBuilder();
  
  /**
   Called once at the beginning of the programme
   */
  void initialize(int argc, char* argv[]);
  
  /**
   Called at the start of a run
   */
  void startrun(daqReader*);
  
  /**
   Called at the end of a run
   */
  void stoprun(daqReader*);
  
  /**
   Called once per event
   */
  void event(daqReader*);
  
  static void main(int argc, char* argv[]);
  
protected:

  // The collection of JevpPlots to fill.
  typedef std::list<JevpPlot*> JevpPlotPtrList;
  JevpPlotPtrList mPlots;
  
  // ADC vs. channel histograms keyed by QT crate number.
  // Histograms are owned by a corresponding JevpPlot in mPlots.
  typedef std::map<int, TH1*> TH1PtrMap;
  TH1PtrMap mHists;

  ClassDef(fmsBuilder, 1);
};

#endif // _STAR_JEVP_FMS_BUILDER_
