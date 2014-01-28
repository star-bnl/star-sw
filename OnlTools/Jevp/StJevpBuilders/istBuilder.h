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

class istBuilder : public JevpPlotSet {
 public:
  int run;
  
  istBuilder(JevpServer *parent=NULL); 
  ~istBuilder();
  
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
 
  //constants declarations...
  static const int totSec       = 72;    // 24 ladders * 3 sections
  static const int totSensor    = 144;   // 24 ladders * 6 sensors
  static const int ChPerSec     = 1536;  // channels/section - 128 channels * 12 APV chips
  static const int ChPerSensor  = 768;   // 128 channels * 6 APV chips
  static const int ApvPerSec    = 12;
  static const int ChPerLadder  = 4608;  // channels/section - 128 channels * 12 APV chips
  static const int ApvPerLadder = 36;
  static const int ChPerApv     = 128;
  static const int totCh        = 110592;  // Total channels in IST - totSec*ChPerSec
  static const int totAPV       = 864;     // Total APVs in IST - totSec*12
  static const int numRDO       = 6;
  static const int numARM       = 6;
  static const int numAPV       = 24;
  static const int numLadder    = 24;
  static const int numSensor    = 6;
  static const int numColumn    = 12;
  static const int numRow       = 64;

  static const int numTimeBin   = 9;//update from 7 to 9: YPWANG 25/1/2014
  static const int goodChCut    = 64;
  static const int minPedVal    = 200;
  static const int maxPedVal    = 1500;
  static const int minRMSVal    = 10;
  static const int maxRMSVal    = 100;

  static const float cmnCut	= 3.0;
  static const float hitCut	= 5.0;
  static const float rmsMin     = 12.0;
 
  //IST mapping
  int istMapping[totCh]; //IST channel mapping (electronics ID & geometry ID transform)

  //*** Histogram Declarations...
  union {
    TH2 *adcArray[]; //ADC value of each ladder's channels (ADC value vs. channel index)
    struct {
      TH2* ADC1;
      TH2* ADC2;
      TH2* ADC3;
      TH2* ADC4;
      TH2* ADC5;
      TH2* ADC6;
      TH2* ADC7;
      TH2* ADC8;
      TH2* ADC9;
      TH2* ADC10;
      TH2* ADC11;
      TH2* ADC12;
      TH2* ADC13;
      TH2* ADC14;
      TH2* ADC15;
      TH2* ADC16;
      TH2* ADC17;
      TH2* ADC18;
      TH2* ADC19;
      TH2* ADC20;
      TH2* ADC21;
      TH2* ADC22;
      TH2* ADC23;
      TH2* ADC24;
    };
  } hAdcContents;

  union {
    TH1 *multArray[]; //hit multiplicity of each ladder per event
    struct {
      TH1* hitMult1;
      TH1* hitMult2;
      TH1* hitMult3;
      TH1* hitMult4;
      TH1* hitMult5;
      TH1* hitMult6;
      TH1* hitMult7;
      TH1* hitMult8;
      TH1* hitMult9;
      TH1* hitMult10;
      TH1* hitMult11;
      TH1* hitMult12;
      TH1* hitMult13;
      TH1* hitMult14;
      TH1* hitMult15;
      TH1* hitMult16;
      TH1* hitMult17;
      TH1* hitMult18;
      TH1* hitMult19;
      TH1* hitMult20;
      TH1* hitMult21;
      TH1* hitMult22;
      TH1* hitMult23;
      TH1* hitMult24;
    };
  } hMultContents;

  union {
    TH2 *hitMapArray[]; //hit map for each ladder (phi vs. Z --- 64 rows vs. 72 columns)
    struct {
      TH2* hitMap1;
      TH2* hitMap2;
      TH2* hitMap3;
      TH2* hitMap4;
      TH2* hitMap5;
      TH2* hitMap6;
      TH2* hitMap7;
      TH2* hitMap8;
      TH2* hitMap9;
      TH2* hitMap10;
      TH2* hitMap11;
      TH2* hitMap12;
      TH2* hitMap13;
      TH2* hitMap14;
      TH2* hitMap15;
      TH2* hitMap16;
      TH2* hitMap17;
      TH2* hitMap18;
      TH2* hitMap19;
      TH2* hitMap20;
      TH2* hitMap21;
      TH2* hitMap22;
      TH2* hitMap23;
      TH2* hitMap24;
    };
  } hHitMapContents;

  union {
    TH2 *tbVsAdcArray[];  // Time bin vs. ADC value
    struct{
      TH2* tbVsAdc1;
      TH2* tbVsAdc2;
      TH2* tbVsAdc3;
      TH2* tbVsAdc4;
      TH2* tbVsAdc5;
      TH2* tbVsAdc6;
      TH2* tbVsAdc7;
      TH2* tbVsAdc8;
      TH2* tbVsAdc9;
      TH2* tbVsAdc10;
      TH2* tbVsAdc11;
      TH2* tbVsAdc12;
      TH2* tbVsAdc13;
      TH2* tbVsAdc14;
      TH2* tbVsAdc15;
      TH2* tbVsAdc16;
      TH2* tbVsAdc17;
      TH2* tbVsAdc18;
      TH2* tbVsAdc19;
      TH2* tbVsAdc20;
      TH2* tbVsAdc21;
      TH2* tbVsAdc22;
      TH2* tbVsAdc23;
      TH2* tbVsAdc24;
    };
  } hTbVsAdcContents;

  union {
    TH1 *eventSumArray[];
    struct {
      TH1* hMeanPed;//mean pedestal of all channels
      TH1* hMeanRMS;//mean rms of all channels
      TH1* hSumTB;  //number of time bin per event
      TH1* hMaxTimeBin;  //max ADC time bin index
      TH1* hSumBad; //number of good channels per APV
      TH1* hApvCorpt;//frequency of visible APVs per event
      TH1* hEventSize;//IST event size
      TH1* hMIPvsSensor; //IST MPV of MIP per sensor
    };
  } hEventSumContents;
  
  union {
    TH1 *mipArray[]; //MIP signal distribution per sensor
    struct {
      TH1* hMip1;
      TH1* hMip2;
      TH1* hMip3;
      TH1* hMip4;
      TH1* hMip5;
      TH1* hMip6;
      TH1* hMip7;
      TH1* hMip8;
      TH1* hMip9;
      TH1* hMip10;
      TH1* hMip11;
      TH1* hMip12;
      TH1* hMip13;
      TH1* hMip14;
      TH1* hMip15;
      TH1* hMip16;
      TH1* hMip17;
      TH1* hMip18;
      TH1* hMip19;
      TH1* hMip20;
      TH1* hMip21;
      TH1* hMip22;
      TH1* hMip23;
      TH1* hMip24;
      TH1* hMip25;
      TH1* hMip26;
      TH1* hMip27;
      TH1* hMip28;
      TH1* hMip29;
      TH1* hMip30;
      TH1* hMip31;
      TH1* hMip32;
      TH1* hMip33;
      TH1* hMip34;
      TH1* hMip35;
      TH1* hMip36;
      TH1* hMip37;
      TH1* hMip38;
      TH1* hMip39;
      TH1* hMip40;
      TH1* hMip41;
      TH1* hMip42;
      TH1* hMip43;
      TH1* hMip44;
      TH1* hMip45;
      TH1* hMip46;
      TH1* hMip47;
      TH1* hMip48;
      TH1* hMip49;
      TH1* hMip50;
      TH1* hMip51;
      TH1* hMip52;
      TH1* hMip53;
      TH1* hMip54;
      TH1* hMip55;
      TH1* hMip56;
      TH1* hMip57;
      TH1* hMip58;
      TH1* hMip59;
      TH1* hMip60;
      TH1* hMip61;
      TH1* hMip62;
      TH1* hMip63;
      TH1* hMip64;
      TH1* hMip65;
      TH1* hMip66;
      TH1* hMip67;
      TH1* hMip68;
      TH1* hMip69;
      TH1* hMip70;
      TH1* hMip71;
      TH1* hMip72;
      TH1* hMip73;
      TH1* hMip74;
      TH1* hMip75;
      TH1* hMip76;
      TH1* hMip77;
      TH1* hMip78;
      TH1* hMip79;
      TH1* hMip80;
      TH1* hMip81;
      TH1* hMip82;
      TH1* hMip83;
      TH1* hMip84;
      TH1* hMip85;
      TH1* hMip86;
      TH1* hMip87;
      TH1* hMip88;
      TH1* hMip89;
      TH1* hMip90;
      TH1* hMip91;
      TH1* hMip92;
      TH1* hMip93;
      TH1* hMip94;
      TH1* hMip95;
      TH1* hMip96;
      TH1* hMip97;
      TH1* hMip98;
      TH1* hMip99;
      TH1* hMip100;
      TH1* hMip101;
      TH1* hMip102;
      TH1* hMip103;
      TH1* hMip104;
      TH1* hMip105;
      TH1* hMip106;
      TH1* hMip107;
      TH1* hMip108;
      TH1* hMip109;
      TH1* hMip110;
      TH1* hMip111;
      TH1* hMip112;
      TH1* hMip113;
      TH1* hMip114;
      TH1* hMip115;
      TH1* hMip116;
      TH1* hMip117;
      TH1* hMip118;
      TH1* hMip119;
      TH1* hMip120;
      TH1* hMip121;
      TH1* hMip122;
      TH1* hMip123;
      TH1* hMip124;
      TH1* hMip125;
      TH1* hMip126;
      TH1* hMip127;
      TH1* hMip128;
      TH1* hMip129;
      TH1* hMip130;
      TH1* hMip131;
      TH1* hMip132;
      TH1* hMip133;
      TH1* hMip134;
      TH1* hMip135;
      TH1* hMip136;
      TH1* hMip137;
      TH1* hMip138;
      TH1* hMip139;
      TH1* hMip140;
      TH1* hMip141;
      TH1* hMip142;
      TH1* hMip143;
      TH1* hMip144;
    };
  } hMipContents;

  union {
    TH2 *sumArray[];
    struct{
      TH2* hHitMap;       //hit density (phi vs. z -- 64*24 vs. 12*6)
      TH2* hMultVsLadder; //total number of hits per event vs. ladder
      TH2* hSumPed;	  //pedestal per channel (ADC vs. channel index)
      TH2* hSumSig;	  //pedestal RMS per channel (RMS vs. channel index)
      TH2* hCommonModeNoise; //common mode noise per chip (CM noise vs.chip index)
    };
  } hSumContents;
  //*** End Histogram Declarations...

  //The below histogram array defined for dynamical common mode noise distribution per chip
  //They are not wrotten to QA file
  union {
      TH1* hCmnPerChip[totAPV];
  } hCmnTemp;

  int mAdcHist;
  int mMultHist;
  int mHitMapHist;
  int mTbVsAdcHist;
  int mEventSumHist;
  int mMipHist;
  int mSumHist;

  JevpPlot** plots;
  daq_dta *dd;

  int numVals[totCh];
  int aVals[totCh];
  int numOverOneSig[totCh];
  int maxAdc[totCh];
  int maxTimeBin[totCh];
  double runningAvg[totCh];
  double runningStdDevSq[totCh];

  float oldStdDevs[totCh];
  float meanVals[totCh];

  float cmNoise[totAPV];
  bool isChannelBad[totCh];
  //int rmsVals[totCh];

  //num RDOs, ARM, APV, keep track of channel count per apv
  int chCntDaq[6][6][24];

  int sumHistogramsFilled;

  JLatex* errorMsg;

  ClassDef(istBuilder, 1);
};
