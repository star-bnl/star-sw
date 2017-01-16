#ifndef _STAR_JEVP_FMS_BUILDER_
#define _STAR_JEVP_FMS_BUILDER_

#include <list>
#include <map>
#include <math.h>
#include <stdio.h>
#include <memory>
#include <sstream>
#include <utility>

#include "StEvent/StTriggerData.h"

#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"

#include <TH1F.h>
#include <TH2F.h>

#include "Jevp/StJevpPlot/JevpPlot.h"
#include "JevpBuilder.h"

//#include "StRoot/StFmsDbMaker/StFmsDbMaker.h" // disabled



class TH1;
class daqReader;
class JevpPlot;
//class StFmsDbMaker; // disabled

const Int_t N_QTCR = 5;
const Int_t N_QTIDX = 353;

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

  // fms cell --> map coordinates -- DEPRECATED
  //Float_t xPos(Int_t nstb_,Int_t row_,Int_t col_);
  //Float_t yPos(Int_t nstb_,Int_t row_,Int_t col_);
  
  void initMaps();


protected:

  // The collection of JevpPlots to fill.
  typedef std::list<JevpPlot*> JevpPlotPtrList;
  JevpPlotPtrList mPlots;
  
  // ADC vs. channel histograms keyed by QT crate number.
  // Histograms are owned by a corresponding JevpPlot in mPlots.
  typedef std::map<int, TH1*> TH1PtrMap;
  TH1PtrMap mHists;

  // channel maps
  Int_t nstb_map[N_QTCR][N_QTIDX];
  Int_t chan_map[N_QTCR][N_QTIDX];
  Int_t row_map[N_QTCR][N_QTIDX];
  Int_t col_map[N_QTCR][N_QTIDX];


  // hit map
  //StFmsDbMaker * fmsdb; // disabled
  TH2D * hitmap[2][2]; // [kLarge/kSmall] [kADC/kLED]
  //TH2D * fmschan[2]; // deprecated
  int crate;
  int index;
  unsigned short adc;
  Int_t detid,nstb,cellchan;
  Int_t row,col;

  ClassDef(fmsBuilder, 1);
};

#endif // _STAR_JEVP_FMS_BUILDER_
