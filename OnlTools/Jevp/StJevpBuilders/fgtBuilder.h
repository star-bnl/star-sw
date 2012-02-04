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

  void fillSumHistos();
  TH1D* projX;
  TRandom tRnd;
  int evtCt;
  int t_2min;
  int t_10min;
  int t_120min;



  static const string Gid2Label[19];
  //the gid encodes rdo etc. but since this is consecutive, we have to have another mapping
  static const int Indx2Gid[19];
  static const int Gid2Indx[19];


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
      TH1* hSumBad;
      TH1* hApvCorpt;
      //other possible plots:
      //hits over pedestal for each timebin, shows in time, collision condition etc.
    };
  } hContents;

  union {
    TH2 *sumHArray[];
    struct{
      TH2* hSumPed;
      TH2* hSumSig;
      TH2* hSumFrac;

    };
  }hSumContents;

  static const int maxC;
  static const int maxA;
  static const int maxPedVal;
  static const int maxRMSVal;
  static const int minPedVal;
  static const int minRMSVal;
  static const int numRDO;
  static const int numAPV;
  static const int numARM;
  static const int goodChCut;

  int np;
  int hNp;
  int hSNp;

  JevpPlot** plots;
  daq_dta *dd;
  //*** End Histogram Declarations...
  float meanVals[19*1400];
  float oldStdDevs[19*1400];

  int aVals[19*1400];
  int numVals[19*1400];
  //  int rmsVals[19*1400];


  int numOverOneSig[19*1400];
  bool isChannelBad[19*1400];


  double runningAvg[19*1400];
  double runningStdDevSq[19*1400];
  //num RDOs, ARM, APV, keep track of channel count per apv
  int chCntDaq[2][6][24];

  int sumHistogramsFilled;


  JLatex* errorMsg;

  ClassDef(fgtBuilder, 1);
};
