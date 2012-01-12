#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>
#include <TRandom.h>
#include "DAQ_READER/daq_dta.h"
#include <string>
#include <math.h>

class fgtBuilder : public JevpPlotSet {
public:
  int run;

  fgtBuilder(JevpServer *parent=NULL); 
  ~fgtBuilder();
  

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:
  TRandom tRnd;
  int evtCt;
  int t_2min;
  int t_10min;
  int t_120min;



  static const string Gid2Label[19];
  //the gid encodes rdo etc. but since this is consecutive, we have to have another mapping
  static const int Indx2Gid[19];


  //*** Histogram Declarations...
  //*** Use the union to be able to treat in bulk
  //*** As well as by name...
  union {
    TH2 *array[];
    struct {
      TH2* q1;
      TH2* q2;
      TH2* q3;
      TH2* q4;
      TH2* q5;
      TH2* q6;
      TH2* q7;
      TH2* q8;
      TH2* q9;
      TH2* q10;
      TH2* q11;
      TH2* q12;
      TH2* q13;
      TH2* q14;
      TH2* q15;
      TH2* q16;
      TH2* q17;
      TH2* q18;
      TH2* q19;
    };
  } contents;

  union {
    TH1 *hArray[];
    struct {
      TH1* h1;
      TH1* h2;
      //other possible plots:
      //hits over pedestal for each timebin, shows in time, collision condition etc.

    };
  } hContents;

  static const int maxC;
  static const int maxA;
  static const int maxPedVal;
  static const int maxRMSVal;
  static const int minPedVal;
  static const int minRMSVal;

  int np;
  int hNp;
  JevpPlot** plots;
  daq_dta *dd;
  //*** End Histogram Declarations...
  float meanVals[19*1400];
  int aVals[19*1400];
  int numVals[19*1400];
  int rmsVals[19*1400];
  bool isChannelBad[19*1400];
  JLatex* errorMsg;

  ClassDef(fgtBuilder, 1);
};
