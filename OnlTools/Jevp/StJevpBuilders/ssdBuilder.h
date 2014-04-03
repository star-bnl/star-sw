#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>
#include "TH2.h"
#include <TRandom.h>
#include "DAQ_READER/daq_dta.h"
#include <string>
#include <math.h>

class ssdBuilder : public JevpPlotSet {
 public:
  int run;
  
  ssdBuilder(JevpServer *parent=NULL); 
  ~ssdBuilder();
  
  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  static void main(int argc, char *argv[]);
  
 private:
  void FindLadderSide(int RDO,int channel,int &ladder,int &side);
  void FindStripNumber(int &strip);  
  void fillSumHistos();
  int evtCt; //all event number 
  //SSD paraneters
  static const int nSide           = 2;  
  static const int nRdo            = 5; 
  static const int nLadderPerSide  = 20;   
  static const int nWaferPerLadder = 16; 
  static const int nStripPerWafer  = 768;
  static const int nChipPerWafer   = 6;   
  static const int nChPerLadder    = 12288;//n channel per ladder 

  //--book histograms---
  TH2I *hAdcStrip[nSide][nLadderPerSide]; 
  TH2I *hAdcEvent[nSide][nLadderPerSide];
  TH2I *hLadderWafer[nSide];
  //-------------------
  int mSector;
  int mRDO;
  int mSide;
  int mFiber;
  int mLadder;
  int mWafer;
  int mStrip;
  int mAdc;
  int mAdcLength;
  int mPed;
  int mRms;
  JevpPlot **plots;
  daq_dta *dd;

  JLatex* errorMsg;

  ClassDef(ssdBuilder, 1);
};


  //RDO to Ladder 
const int RDO2LADDER[5][8] = { {1,2,3,4,5,6,7,8},
                           {9,10,11,12,13,14,15,16},
                           {17,18,19,20,1,2,3,4},
                           {5,6,7,8,9,10,11,12},
                           {13,14,15,16,17,18,19,20}
};//Ladder cards number in each RDO channel .

  //readout to strip number 
const int ReadOutMap[128] ={
  97,96,98,95,99,94,100,93,
  101,92,102,91,103,90,104,89,
  105,88,106,87,107,86,108,85,
  109,84,110,83,111,82,112,81,
  113,80,114,79,115,78,116,77,
  117,76,118,75,119,74,120,73,
  121,72,122,71,123,70,124,69,
  125,68,126,67,127,66,128,65,
  1,64,2,63,3,62,4,61,
  5,60,6,59,7,58,8,57,
  9,56,10,55,11,54,12,53,
  13,52,14,51,15,50,16,49,
  17,48,18,47,19,46,20,45,
  21,44,22,43,23,42,24,41,
  25,40,26,39,27,38,28,37,
  29,36,30,35,31,34,32,33
};//silicon strip number ordered by ALICE128 readout order      
