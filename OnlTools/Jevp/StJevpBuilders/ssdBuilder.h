#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>
#include "TH2.h"
#include <TRandom.h>
#include "DAQ_READER/daq_dta.h"
#include <string>
#include <math.h>
#include "TTree.h"
class ssdBuilder : public JevpBuilder {
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
  void UpdateRatio(Float_t &ratio,Int_t Nevent,Int_t value);
  Int_t Mid(UInt_t start, UInt_t end, UInt_t input);
  void globleStyle();
  void setstyle();
  int evtCt; //all event number 
  //SSD paraneters
  static const int nSector         = 2;
  static const int nSide           = 2;  
  static const int nRdo            = 5; 
  static const int nLadderPerSide  = 20;   
  static const int nWaferPerLadder = 16; 
  static const int nStripPerWafer  = 768;
  static const int nChipPerWafer   = 6;   
  static const int nChPerLadder    = 12288;//n channel per ladder 
  //--book histograms---                     // Size
  TH2I *hRawAdcStrip[nSide][nLadderPerSide]; //	2*20
  TH2I *hZSAdcStrip[nSide][nLadderPerSide];  //	2*20
  TH1I *hPedStrip[nSide][nLadderPerSide];    //	2*20
  TH1I *hRmsStrip[nSide][nLadderPerSide];    //	2*20
  TH2I *hRawLadderWafer[nSide];  	     //	2
  TH2I *hZSLadderWafer[nSide];   	     //	2
  TH2I *hFailedLadderChip[nSide];	     //	2          
 
  TH2I *hErrorRdo;        		     // 1
  TH2I *hErrorFiber[nRdo];		     // 5                
  TH1I *hModeCounter;                        // 1 
  /* TH1I *hErrorRatioRdo[nRdo];//5 */
  /* TH1I *hErrorRatioFiber[nRdo];//5 */

  //-------------------
  UInt_t mDataMode;
  int mSector;
  int mRDO;
  int mSide;
  int mFiber;
  int mLadder;
  int mWafer;
  int mChip;
  int mStrip;
  int mAdc;
  int mAdcLength;
  int mPed;
  int mRms;
  
  TTree *mTree;
  Int_t mOutPutTree;
  JevpPlot **plots;

  daq_dta *dd;

  JLatex* errorMsg;

  //DAQ File parameters(please look at the SSD data formata document. )
   static const UInt_t  HEADER_LENGTH       = 8;
   static const UInt_t  FIBER_HEADER_LENGTH = 10;
   static const UInt_t  HEADER_TOKEN        = 0xAAAAAAAA;
   static const UInt_t  END_TOKEN           = 0xBBBBBBBB;
   static const UInt_t  TCD_TOKEN           = 0xCCCCCCCC;
   static const UInt_t  FIBER_LINK_TOKEN    = 0xDDDDDDDD;
   static const UInt_t  TCD_END_TOKEN       = 0xEEEEEEEE;
   static const UInt_t  RDO_START           = 24;//RDO Number Start bit
   static const UInt_t  RDO_END             = 28;//RDO Number End bit
   static const UInt_t  TRIG_START          = 0;//TCD trigger word start
   static const UInt_t  TRIG_END            = 20;//TCD trigger word end
   static const UInt_t  FIBER_START         = 28;//Fiber Input Start bit
   static const UInt_t  FIBER_END           = 31;//Fiber Input End bit
   static const UInt_t  HYBRID_ONE_START    = 0;//Hyirid 0 start bit
   static const UInt_t  HYBRID_ONE_END      = 10;//Hyirid 0 end bit
   static const UInt_t  HYBRID_TWO_START    = 10;
   static const UInt_t  HYBRID_TWO_END      = 20;
   static const UInt_t  HYBRID_THREE_START  = 20;
   static const UInt_t  HYBRID_THREE_END    = 30;
   static const UInt_t  COM_ADC_START       = 0;
   static const UInt_t  COM_ADC_END         = 10;
   static const UInt_t  HYBRID_START        = 10;
   static const UInt_t  HYBRID_END          = 14;
   static const UInt_t  STRIP_START         = 14;
   static const UInt_t  STRIP_END           = 24;
   static const UInt_t  ERROR_START         = 24;//CMN Err Code Start Bit
   static const UInt_t  ERROR_END           = 32;//CMN Err Code End Bit
   static const UInt_t  ADC_START           = 4;//adc length start bit
   static const UInt_t  ADC_END             = 20;//adc length end bit
   static const UInt_t  DATAMODE_START      = 0;//adc mode start bit
   static const UInt_t  DATAMODE_END        = 4;//adc mode end bit
   static const UInt_t  RAWMODE             = 0x0;//0x0 Raw data mode
   static const UInt_t  COMPRESSEDMODE      = 0x1;//0x1 ZS mode
   static const UInt_t  CMNSMODE            = 0x3;//0x3 CMN suppressed mode
   static const UInt_t  FLAG_START          = 20;//flag start bit
   static const UInt_t  FLAG_END            = 32;//flag end bit
   static const UInt_t  NORMAL              = 0x0;//0x000  //normal flag
   static const UInt_t  NODATA              = 0x1;//0x001  //no data
   static const UInt_t  OVERFLOWFLAG        = 0x2;//0x002  //over flow
   static const UInt_t  EARLY_ABORT         = 0x3;//0x003  //early abort
   static const UInt_t  WRONG_PIPE_MODE     = 0x4;//0x004  //wrong pipe mode
   static const UInt_t  ADC_LENGTH_LIMIT    = 4106;//adc length limit,include fiber header
   static const UInt_t  CMNERRORCODE        = 0xB;//CMN algorithm failed error code
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
