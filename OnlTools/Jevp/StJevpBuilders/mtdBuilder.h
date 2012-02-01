#ifndef _MTDBUILDER_H
#define _MTDBUILDER_H

#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

#define nMTDtrays 30
#define nMTDtrig 20

class mtdBuilder : public JevpPlotSet {
 public:

  //RunStatus status;
  int run;

  mtdBuilder(JevpServer *parent=NULL); 
  ~mtdBuilder();
  
  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:

  union {
    TH1 *array[];
    struct {
      // MTDhits histogram group
      TH1*** hMTD_hitmap;

      // MTDtriggerinfo histogram group
      TH1** hMTD_trig;
    };
  } contents;

  // MTDhits support
  int tdcchan2globalstrip(int,int,int);
  int tdig2slot(int, int);
  int istray3bl(int);
 
  double numberforsort;
  vector<double> leadinghits;
  vector<double> trailinghits;

  JevpPlot **plots;
  ClassDef(mtdBuilder, 1);
};

#endif