#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
// #include "DAQ_READER/daqReader.h"
class daqReader;
#include <TH1F.h>
#include <TH2F.h>
#include <TH1S.h>
#include <TH2S.h>
#include <TRandom.h>
//#include "DAQ_READER/daq_dta.h"
struct daq_dta;

#include <string>
#include <math.h>

//#if __STDC_VERSION__ < 201112L
//#define constexpr
//#endif


class fstBuilder : public JevpBuilder {
 public:
  int run;
  
  fstBuilder(JevpServer *parent=NULL); 
  ~fstBuilder();
  
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
  int evtCt_nonZS;
  int evtCt_ZS;
  int t_2min;
  int t_10min;
  int t_120min;
 
  //constants declarations...
  static const int totDisk      = 3;     // 1 disk per crate
  static const int totRdo       = 6;     // 2 rdos * 3 crates
  static const int totArm       = 18;    // 3 arms * 2 rdos * 3 crates
  static const int totMod       = 36;    // 12 modules * 3 disks (6 modules * 6 rdos)
  static const int totSec       = 72;    // 2 sections * 12 moudles * 3 disks
  static const int totSensor    = 144;   // 2 sensors (inner: 0-1 & outer: 2-3) * 2 sections * 12 moudles * 3 disks
  static const int totAPV       = 288;   // 8 APV chips * 12 modules * 3 disks
  static const int totCh        = 36864; // 128 channels * 288 APV chips

  static const int RdoPerDisk   = 2;     // per disk
  static const int ArmPerDisk   = 6;     // per disk
  static const int ModPerDisk   = 12;    // per disk
  static const int SecPerDisk   = 24;    // 2 (SecPerMod) * 2 (PortPerArm) * 3 (ArmPerRdo) * 2 (RdoPerDisk)
  static const int ArmPerRdo    = 3;     // per RDO
  static const int ModPerRdo    = 6;     // per RDO
  static const int SecPerRdo    = 12;    // 2 (SecPerMod) * 2 (PortPerArm) * 3 (ArmPerRdo)
  static const int PortPerArm   = 2;     // per ARM
  static const int ModPerArm    = 2;     // per ARM
  static const int SecPerArm    = 4;     // 2 (SecPerMod) * 2 (PortPerArm)
  static const int SecPerMod    = 2;     // per module
  static const int SensorPerMod = 4;     // per module
  static const int RstripPerMod = 8;     // per module: 0-3 for inner section | 4-7 for outer section
  static const int PhiSegPerMod = 128;   // per module

  static const int ChPerApv     = 128;
  static const int ApvPerSensor = 2;     // sensor0: APV0&1 | sensor1: APV2&3 | sensor2: APV4&5 | sensor3: APV6&7
  static const int ChPerSensor  = 256;   // 128 channels * 2 APV chips
  static const int ApvPerSec    = 4;
  static const int ChPerSec     = 512;   // 128 channels * 4 APV chips
  static const int ApvPerMod    = 8;
  static const int ChPerMod     = 1024;  // 128 channels * 8 APV chips
  static const int ApvPerPort   = 8;
  static const int ChPerPort    = 1024;  // 128 channels * 8 APV chips
  static const int ApvPerArm    = 16;    // 8 APV chips * 2 modules | APV number: port0: 0-7 & port1: 12-19
  static const int ChPerArm     = 2048;  // 128 channels * 16 APV chips
  static const int ApvPerRdo    = 48;    // 8 APV chips * 6 modules
  static const int ChPerRdo     = 6144;  // 128 channels * 48 APV chips
  static const int ApvPerDisk   = 96;    // 8 APV chips * 12 modules
  static const int ChPerDisk    = 12288; // 128 channels * 96 APV chips

  static const int ApvNumOffset = 12;    // APV RO number same as IST | used for APV number convertion
  static const int ApvRoPerPort = 12;    // APV RO number same as IST
  static const int ApvRoPerArm  = 24;    // APV RO number same as IST

  static const int numTimeBin   = 3;     // to be decided
  static const int goodChCut    = 64;    // to be decided
  static const int minPedVal    = 200;   // to be decided
  static const int maxPedVal    = 3000;  // to be decided
  static const int minRMSVal    = 10;    // to be decided
  static const int maxRMSVal    = 60;    // to be decided
  static const int minRanVal    = 8;    // to be decided
  static const int maxRanVal    = 60;    // to be decided
  //alarm threshold
  static const float minMipMpv_ZS;
  static const float minMipMpv_nonZS;
  static const float maxMipMpv;
  static const float minMipSigma_ZS;
  static const float minMipSigma_nonZS;
  static const float maxMipSigma;
  static const float maxTbFracOK;
  //landau fitting range
  static const float landauFit_dn;
  static const float landauFit_up;
  //cut setting
  static const float cmnCut;
  static const float hitCut;
  static const float zsCut;
  static const float noiseChipCut;
  static const int   hitOccupancyCut;
  static const int   defTb;

  // constant used for FST Geometry Hit Map
  // all values are defined by inner direction
  static const int zFilp[totDisk];
  static const int zDirct[ModPerDisk];
  static const float phiStart[ModPerDisk];
  static const float phiStop[ModPerDisk];
  static const float phiDelta;
  static const float rStart[RstripPerMod];
  static const float rStop[RstripPerMod];
  static const float rDelta;

  //FST mapping
  int fstGeomMapping[totCh]; //FST channel mapping (electronics ID to geometry ID transform)
  int fstElecMapping[totCh]; //FST channel mapping (geometry ID & electronics ID transform)
  //FST pedestal/rms
  bool  tableFound;
  float fstPedestal[numTimeBin][totCh];
  float fstRmsNoise[numTimeBin][totCh];
  float fstRanNoise[numTimeBin][totCh];

  //*** Histogram Declarations...
  union {
    TH2 *adcArray[1]; //ADC value of each module's channels (ADC value vs. channel index)
    struct { // 3 disks * 12 modules
      TH2* hADCDisk1Mod1;
      TH2* hADCDisk1Mod2;
      TH2* hADCDisk1Mod3;
      TH2* hADCDisk1Mod4;
      TH2* hADCDisk1Mod5;
      TH2* hADCDisk1Mod6;
      TH2* hADCDisk1Mod7;
      TH2* hADCDisk1Mod8;
      TH2* hADCDisk1Mod9;
      TH2* hADCDisk1Mod10;
      TH2* hADCDisk1Mod11;
      TH2* hADCDisk1Mod12;
      TH2* hADCDisk2Mod1;
      TH2* hADCDisk2Mod2;
      TH2* hADCDisk2Mod3;
      TH2* hADCDisk2Mod4;
      TH2* hADCDisk2Mod5;
      TH2* hADCDisk2Mod6;
      TH2* hADCDisk2Mod7;
      TH2* hADCDisk2Mod8;
      TH2* hADCDisk2Mod9;
      TH2* hADCDisk2Mod10;
      TH2* hADCDisk2Mod11;
      TH2* hADCDisk2Mod12;
      TH2* hADCDisk3Mod1;
      TH2* hADCDisk3Mod2;
      TH2* hADCDisk3Mod3;
      TH2* hADCDisk3Mod4;
      TH2* hADCDisk3Mod5;
      TH2* hADCDisk3Mod6;
      TH2* hADCDisk3Mod7;
      TH2* hADCDisk3Mod8;
      TH2* hADCDisk3Mod9;
      TH2* hADCDisk3Mod10;
      TH2* hADCDisk3Mod11;
      TH2* hADCDisk3Mod12;
    };
  } hAdcContents;

  union {
    TH1 *multArray[1]; //hit multiplicity of each module per event
    struct { // 3 disks * 12 modules
      TH1* hitMultDisk1Mod1;
      TH1* hitMultDisk1Mod2;
      TH1* hitMultDisk1Mod3;
      TH1* hitMultDisk1Mod4;
      TH1* hitMultDisk1Mod5;
      TH1* hitMultDisk1Mod6;
      TH1* hitMultDisk1Mod7;
      TH1* hitMultDisk1Mod8;
      TH1* hitMultDisk1Mod9;
      TH1* hitMultDisk1Mod10;
      TH1* hitMultDisk1Mod11;
      TH1* hitMultDisk1Mod12;
      TH1* hitMultDisk2Mod1;
      TH1* hitMultDisk2Mod2;
      TH1* hitMultDisk2Mod3;
      TH1* hitMultDisk2Mod4;
      TH1* hitMultDisk2Mod5;
      TH1* hitMultDisk2Mod6;
      TH1* hitMultDisk2Mod7;
      TH1* hitMultDisk2Mod8;
      TH1* hitMultDisk2Mod9;
      TH1* hitMultDisk2Mod10;
      TH1* hitMultDisk2Mod11;
      TH1* hitMultDisk2Mod12;
      TH1* hitMultDisk3Mod1;
      TH1* hitMultDisk3Mod2;
      TH1* hitMultDisk3Mod3;
      TH1* hitMultDisk3Mod4;
      TH1* hitMultDisk3Mod5;
      TH1* hitMultDisk3Mod6;
      TH1* hitMultDisk3Mod7;
      TH1* hitMultDisk3Mod8;
      TH1* hitMultDisk3Mod9;
      TH1* hitMultDisk3Mod10;
      TH1* hitMultDisk3Mod11;
      TH1* hitMultDisk3Mod12;
    };
  } hMultContents;

  union {
    TH2 *hitMapArray[1]; //hit map for each module (phi vs. R --- 128 phi vs. 8 R)
    struct { // 3 disks * 12 modules
      TH2* hitMapDisk1Mod1;
      TH2* hitMapDisk1Mod2;
      TH2* hitMapDisk1Mod3;
      TH2* hitMapDisk1Mod4;
      TH2* hitMapDisk1Mod5;
      TH2* hitMapDisk1Mod6;
      TH2* hitMapDisk1Mod7;
      TH2* hitMapDisk1Mod8;
      TH2* hitMapDisk1Mod9;
      TH2* hitMapDisk1Mod10;
      TH2* hitMapDisk1Mod11;
      TH2* hitMapDisk1Mod12;
      TH2* hitMapDisk2Mod1;
      TH2* hitMapDisk2Mod2;
      TH2* hitMapDisk2Mod3;
      TH2* hitMapDisk2Mod4;
      TH2* hitMapDisk2Mod5;
      TH2* hitMapDisk2Mod6;
      TH2* hitMapDisk2Mod7;
      TH2* hitMapDisk2Mod8;
      TH2* hitMapDisk2Mod9;
      TH2* hitMapDisk2Mod10;
      TH2* hitMapDisk2Mod11;
      TH2* hitMapDisk2Mod12;
      TH2* hitMapDisk3Mod1;
      TH2* hitMapDisk3Mod2;
      TH2* hitMapDisk3Mod3;
      TH2* hitMapDisk3Mod4;
      TH2* hitMapDisk3Mod5;
      TH2* hitMapDisk3Mod6;
      TH2* hitMapDisk3Mod7;
      TH2* hitMapDisk3Mod8;
      TH2* hitMapDisk3Mod9;
      TH2* hitMapDisk3Mod10;
      TH2* hitMapDisk3Mod11;
      TH2* hitMapDisk3Mod12;
    };
  } hHitMapContents;

  union {
    TH2 *tbVsAdcArray[1];  // Time bin vs. ADC value per section
    struct{ // 3 disks * 12 modules * 2 sections
      TH2* tbVsAdcDisk1Sec0;
      TH2* tbVsAdcDisk1Sec1;
      TH2* tbVsAdcDisk1Sec2;
      TH2* tbVsAdcDisk1Sec3;
      TH2* tbVsAdcDisk1Sec4;
      TH2* tbVsAdcDisk1Sec5;
      TH2* tbVsAdcDisk1Sec6;
      TH2* tbVsAdcDisk1Sec7;
      TH2* tbVsAdcDisk1Sec8;
      TH2* tbVsAdcDisk1Sec9;
      TH2* tbVsAdcDisk1Sec10;
      TH2* tbVsAdcDisk1Sec11;
      TH2* tbVsAdcDisk1Sec12;
      TH2* tbVsAdcDisk1Sec13;
      TH2* tbVsAdcDisk1Sec14;
      TH2* tbVsAdcDisk1Sec15;
      TH2* tbVsAdcDisk1Sec16;
      TH2* tbVsAdcDisk1Sec17;
      TH2* tbVsAdcDisk1Sec18;
      TH2* tbVsAdcDisk1Sec19;
      TH2* tbVsAdcDisk1Sec20;
      TH2* tbVsAdcDisk1Sec21;
      TH2* tbVsAdcDisk1Sec22;
      TH2* tbVsAdcDisk1Sec23;
      TH2* tbVsAdcDisk2Sec0;
      TH2* tbVsAdcDisk2Sec1;
      TH2* tbVsAdcDisk2Sec2;
      TH2* tbVsAdcDisk2Sec3;
      TH2* tbVsAdcDisk2Sec4;
      TH2* tbVsAdcDisk2Sec5;
      TH2* tbVsAdcDisk2Sec6;
      TH2* tbVsAdcDisk2Sec7;
      TH2* tbVsAdcDisk2Sec8;
      TH2* tbVsAdcDisk2Sec9;
      TH2* tbVsAdcDisk2Sec10;
      TH2* tbVsAdcDisk2Sec11;
      TH2* tbVsAdcDisk2Sec12;
      TH2* tbVsAdcDisk2Sec13;
      TH2* tbVsAdcDisk2Sec14;
      TH2* tbVsAdcDisk2Sec15;
      TH2* tbVsAdcDisk2Sec16;
      TH2* tbVsAdcDisk2Sec17;
      TH2* tbVsAdcDisk2Sec18;
      TH2* tbVsAdcDisk2Sec19;
      TH2* tbVsAdcDisk2Sec20;
      TH2* tbVsAdcDisk2Sec21;
      TH2* tbVsAdcDisk2Sec22;
      TH2* tbVsAdcDisk2Sec23;
      TH2* tbVsAdcDisk3Sec0;
      TH2* tbVsAdcDisk3Sec1;
      TH2* tbVsAdcDisk3Sec2;
      TH2* tbVsAdcDisk3Sec3;
      TH2* tbVsAdcDisk3Sec4;
      TH2* tbVsAdcDisk3Sec5;
      TH2* tbVsAdcDisk3Sec6;
      TH2* tbVsAdcDisk3Sec7;
      TH2* tbVsAdcDisk3Sec8;
      TH2* tbVsAdcDisk3Sec9;
      TH2* tbVsAdcDisk3Sec10;
      TH2* tbVsAdcDisk3Sec11;
      TH2* tbVsAdcDisk3Sec12;
      TH2* tbVsAdcDisk3Sec13;
      TH2* tbVsAdcDisk3Sec14;
      TH2* tbVsAdcDisk3Sec15;
      TH2* tbVsAdcDisk3Sec16;
      TH2* tbVsAdcDisk3Sec17;
      TH2* tbVsAdcDisk3Sec18;
      TH2* tbVsAdcDisk3Sec19;
      TH2* tbVsAdcDisk3Sec20;
      TH2* tbVsAdcDisk3Sec21;
      TH2* tbVsAdcDisk3Sec22;
      TH2* tbVsAdcDisk3Sec23;
    };
  } hTbVsAdcContents;

  union {
    TH1 *eventSumArray[1];
    struct {
      TH1* hMeanPed;//mean pedestal of all channels
      TH1* hMeanRMS;//mean total rms of all channels
      TH1* hMeanRan;//mean random rms of all channels
      TH1* hSumTB;  //number of time bin per event
      TH1* hMaxTimeBin;  //max ADC time bin index
      TH1* hMaxTimeBin_ZS;  //max ADC time bin index
      TH1* hSumBad; //number of good channels per APV
      TH1* hApvCorpt;//frequency of visible APVs per event
      TH1* hEventSize;//FST event size
      TH1* hMipMPVvsSection; //FST MPV of MIP per section (non-ZS) => per module?
      TH1* hMipMPVvsSection_ZS; //FST MPV of MIP per section (ZS) => per module?
      TH1* hMipSIGMAvsSection; //FST Sigma of MIP per section (non-ZS) => per module?
      TH1* hMipSIGMAvsSection_ZS; //FST Sigma of MIP per section (ZS) => per module?
      TH1* hMaxTBfractionVsSection_ZS; //max time bin fraction in 1,2,3 over all time bins vs section ID
      TH1* hMaxAdc; //max ADC
      TH1* hMaxAdc_ZS; //max ADC (ZS)
    };
  } hEventSumContents;
  
  union {
    TH1 *mipArray[1]; //MIP signal distribution per section
    struct {  // 3 disks * 12 modules * 2 sections
      TH1* hMipDisk1Sec1;
      TH1* hMipDisk1Sec2;
      TH1* hMipDisk1Sec3;
      TH1* hMipDisk1Sec4;
      TH1* hMipDisk1Sec5;
      TH1* hMipDisk1Sec6;
      TH1* hMipDisk1Sec7;
      TH1* hMipDisk1Sec8;
      TH1* hMipDisk1Sec9;
      TH1* hMipDisk1Sec10;
      TH1* hMipDisk1Sec11;
      TH1* hMipDisk1Sec12;
      TH1* hMipDisk1Sec13;
      TH1* hMipDisk1Sec14;
      TH1* hMipDisk1Sec15;
      TH1* hMipDisk1Sec16;
      TH1* hMipDisk1Sec17;
      TH1* hMipDisk1Sec18;
      TH1* hMipDisk1Sec19;
      TH1* hMipDisk1Sec20;
      TH1* hMipDisk1Sec21;
      TH1* hMipDisk1Sec22;
      TH1* hMipDisk1Sec23;
      TH1* hMipDisk1Sec24;
      TH1* hMipDisk2Sec1;
      TH1* hMipDisk2Sec2;
      TH1* hMipDisk2Sec3;
      TH1* hMipDisk2Sec4;
      TH1* hMipDisk2Sec5;
      TH1* hMipDisk2Sec6;
      TH1* hMipDisk2Sec7;
      TH1* hMipDisk2Sec8;
      TH1* hMipDisk2Sec9;
      TH1* hMipDisk2Sec10;
      TH1* hMipDisk2Sec11;
      TH1* hMipDisk2Sec12;
      TH1* hMipDisk2Sec13;
      TH1* hMipDisk2Sec14;
      TH1* hMipDisk2Sec15;
      TH1* hMipDisk2Sec16;
      TH1* hMipDisk2Sec17;
      TH1* hMipDisk2Sec18;
      TH1* hMipDisk2Sec19;
      TH1* hMipDisk2Sec20;
      TH1* hMipDisk2Sec21;
      TH1* hMipDisk2Sec22;
      TH1* hMipDisk2Sec23;
      TH1* hMipDisk2Sec24;
      TH1* hMipDisk3Sec1;
      TH1* hMipDisk3Sec2;
      TH1* hMipDisk3Sec3;
      TH1* hMipDisk3Sec4;
      TH1* hMipDisk3Sec5;
      TH1* hMipDisk3Sec6;
      TH1* hMipDisk3Sec7;
      TH1* hMipDisk3Sec8;
      TH1* hMipDisk3Sec9;
      TH1* hMipDisk3Sec10;
      TH1* hMipDisk3Sec11;
      TH1* hMipDisk3Sec12;
      TH1* hMipDisk3Sec13;
      TH1* hMipDisk3Sec14;
      TH1* hMipDisk3Sec15;
      TH1* hMipDisk3Sec16;
      TH1* hMipDisk3Sec17;
      TH1* hMipDisk3Sec18;
      TH1* hMipDisk3Sec19;
      TH1* hMipDisk3Sec20;
      TH1* hMipDisk3Sec21;
      TH1* hMipDisk3Sec22;
      TH1* hMipDisk3Sec23;
      TH1* hMipDisk3Sec24;
      TH1* hMipDisk1Sec1_ZS;
      TH1* hMipDisk1Sec2_ZS;
      TH1* hMipDisk1Sec3_ZS;
      TH1* hMipDisk1Sec4_ZS;
      TH1* hMipDisk1Sec5_ZS;
      TH1* hMipDisk1Sec6_ZS;
      TH1* hMipDisk1Sec7_ZS;
      TH1* hMipDisk1Sec8_ZS;
      TH1* hMipDisk1Sec9_ZS;
      TH1* hMipDisk1Sec10_ZS;
      TH1* hMipDisk1Sec11_ZS;
      TH1* hMipDisk1Sec12_ZS;
      TH1* hMipDisk1Sec13_ZS;
      TH1* hMipDisk1Sec14_ZS;
      TH1* hMipDisk1Sec15_ZS;
      TH1* hMipDisk1Sec16_ZS;
      TH1* hMipDisk1Sec17_ZS;
      TH1* hMipDisk1Sec18_ZS;
      TH1* hMipDisk1Sec19_ZS;
      TH1* hMipDisk1Sec20_ZS;
      TH1* hMipDisk1Sec21_ZS;
      TH1* hMipDisk1Sec22_ZS;
      TH1* hMipDisk1Sec23_ZS;
      TH1* hMipDisk1Sec24_ZS;
      TH1* hMipDisk2Sec1_ZS;
      TH1* hMipDisk2Sec2_ZS;
      TH1* hMipDisk2Sec3_ZS;
      TH1* hMipDisk2Sec4_ZS;
      TH1* hMipDisk2Sec5_ZS;
      TH1* hMipDisk2Sec6_ZS;
      TH1* hMipDisk2Sec7_ZS;
      TH1* hMipDisk2Sec8_ZS;
      TH1* hMipDisk2Sec9_ZS;
      TH1* hMipDisk2Sec10_ZS;
      TH1* hMipDisk2Sec11_ZS;
      TH1* hMipDisk2Sec12_ZS;
      TH1* hMipDisk2Sec13_ZS;
      TH1* hMipDisk2Sec14_ZS;
      TH1* hMipDisk2Sec15_ZS;
      TH1* hMipDisk2Sec16_ZS;
      TH1* hMipDisk2Sec17_ZS;
      TH1* hMipDisk2Sec18_ZS;
      TH1* hMipDisk2Sec19_ZS;
      TH1* hMipDisk2Sec20_ZS;
      TH1* hMipDisk2Sec21_ZS;
      TH1* hMipDisk2Sec22_ZS;
      TH1* hMipDisk2Sec23_ZS;
      TH1* hMipDisk2Sec24_ZS;
      TH1* hMipDisk3Sec1_ZS;
      TH1* hMipDisk3Sec2_ZS;
      TH1* hMipDisk3Sec3_ZS;
      TH1* hMipDisk3Sec4_ZS;
      TH1* hMipDisk3Sec5_ZS;
      TH1* hMipDisk3Sec6_ZS;
      TH1* hMipDisk3Sec7_ZS;
      TH1* hMipDisk3Sec8_ZS;
      TH1* hMipDisk3Sec9_ZS;
      TH1* hMipDisk3Sec10_ZS;
      TH1* hMipDisk3Sec11_ZS;
      TH1* hMipDisk3Sec12_ZS;
      TH1* hMipDisk3Sec13_ZS;
      TH1* hMipDisk3Sec14_ZS;
      TH1* hMipDisk3Sec15_ZS;
      TH1* hMipDisk3Sec16_ZS;
      TH1* hMipDisk3Sec17_ZS;
      TH1* hMipDisk3Sec18_ZS;
      TH1* hMipDisk3Sec19_ZS;
      TH1* hMipDisk3Sec20_ZS;
      TH1* hMipDisk3Sec21_ZS;
      TH1* hMipDisk3Sec22_ZS;
      TH1* hMipDisk3Sec23_ZS;
      TH1* hMipDisk3Sec24_ZS;
    };
  } hMipContents;

  union {
    TH1 *maxTimeBinArray[1]; //MaxTimeBin per section
    struct {  // 3 disks * 12 modules * 2 sections
      TH1* hMaxTBDisk1Sec1;
      TH1* hMaxTBDisk1Sec2;
      TH1* hMaxTBDisk1Sec3;
      TH1* hMaxTBDisk1Sec4;
      TH1* hMaxTBDisk1Sec5;
      TH1* hMaxTBDisk1Sec6;
      TH1* hMaxTBDisk1Sec7;
      TH1* hMaxTBDisk1Sec8;
      TH1* hMaxTBDisk1Sec9;
      TH1* hMaxTBDisk1Sec10;
      TH1* hMaxTBDisk1Sec11;
      TH1* hMaxTBDisk1Sec12;
      TH1* hMaxTBDisk1Sec13;
      TH1* hMaxTBDisk1Sec14;
      TH1* hMaxTBDisk1Sec15;
      TH1* hMaxTBDisk1Sec16;
      TH1* hMaxTBDisk1Sec17;
      TH1* hMaxTBDisk1Sec18;
      TH1* hMaxTBDisk1Sec19;
      TH1* hMaxTBDisk1Sec20;
      TH1* hMaxTBDisk1Sec21;
      TH1* hMaxTBDisk1Sec22;
      TH1* hMaxTBDisk1Sec23;
      TH1* hMaxTBDisk1Sec24;
      TH1* hMaxTBDisk2Sec1;
      TH1* hMaxTBDisk2Sec2;
      TH1* hMaxTBDisk2Sec3;
      TH1* hMaxTBDisk2Sec4;
      TH1* hMaxTBDisk2Sec5;
      TH1* hMaxTBDisk2Sec6;
      TH1* hMaxTBDisk2Sec7;
      TH1* hMaxTBDisk2Sec8;
      TH1* hMaxTBDisk2Sec9;
      TH1* hMaxTBDisk2Sec10;
      TH1* hMaxTBDisk2Sec11;
      TH1* hMaxTBDisk2Sec12;
      TH1* hMaxTBDisk2Sec13;
      TH1* hMaxTBDisk2Sec14;
      TH1* hMaxTBDisk2Sec15;
      TH1* hMaxTBDisk2Sec16;
      TH1* hMaxTBDisk2Sec17;
      TH1* hMaxTBDisk2Sec18;
      TH1* hMaxTBDisk2Sec19;
      TH1* hMaxTBDisk2Sec20;
      TH1* hMaxTBDisk2Sec21;
      TH1* hMaxTBDisk2Sec22;
      TH1* hMaxTBDisk2Sec23;
      TH1* hMaxTBDisk2Sec24;
      TH1* hMaxTBDisk3Sec1;
      TH1* hMaxTBDisk3Sec2;
      TH1* hMaxTBDisk3Sec3;
      TH1* hMaxTBDisk3Sec4;
      TH1* hMaxTBDisk3Sec5;
      TH1* hMaxTBDisk3Sec6;
      TH1* hMaxTBDisk3Sec7;
      TH1* hMaxTBDisk3Sec8;
      TH1* hMaxTBDisk3Sec9;
      TH1* hMaxTBDisk3Sec10;
      TH1* hMaxTBDisk3Sec11;
      TH1* hMaxTBDisk3Sec12;
      TH1* hMaxTBDisk3Sec13;
      TH1* hMaxTBDisk3Sec14;
      TH1* hMaxTBDisk3Sec15;
      TH1* hMaxTBDisk3Sec16;
      TH1* hMaxTBDisk3Sec17;
      TH1* hMaxTBDisk3Sec18;
      TH1* hMaxTBDisk3Sec19;
      TH1* hMaxTBDisk3Sec20;
      TH1* hMaxTBDisk3Sec21;
      TH1* hMaxTBDisk3Sec22;
      TH1* hMaxTBDisk3Sec23;
      TH1* hMaxTBDisk3Sec24;
    };
  } hMaxTimeBinContents;

  union {
    TH2 *sumArray[1];
    struct{
      TH2* hSumPed[totDisk];  	         //pedestal from pedestal run (ADC vs. channel index)
      TH2* hSumSig[totDisk];	         //pedestal RMS from pedestal run (totRMS vs. channel index)
      TH2* hSumRan[totDisk];	         //random RMS from pedestal run (ranRMS vs. channel index)
      TH2* hSumCmn[totDisk];	         //cmn RMS from pedestal run (cmnRMS vs. channel index)
      TH2* hSignal[totDisk];             //signal (non-ZS) updates every event (adc-pedestal vs.chip index)
      TH2* hRanNoise[totDisk];           //random noise (non-ZS) updates every 5k events (random noise vs.chip index)
      TH2* hCommonModeNoise[totDisk];    //common mode noise (non-ZS) updates every 5k events (CM noise vs.chip index)
      TH2* hVisibleApv[totDisk];         //visible APVs per modules per event for each disk
      TH2* hHitMap[totDisk];             //hit density for each disk (phi bin vs. r bin -- 128*12 vs. 8)
      TH2* hDummyPolyHitMap[totDisk];    //hit density for each disk (phi val vs. r val -- 128*12 vs. 8)
      TH2* hPolyHitMap[totDisk];         //hit density for each disk (phi val vs. r val -- 128*12 vs. 8)
      TH2* hHitMapVsAPV[totDisk];        //hit map on APV for each disk (APV geometry ID vs. module geometry ID)
      TH2* hMultVsModule[totDisk];       //total number of hits (non-ZS) per event vs. module for each disk
      TH2* hSignal_zs[totDisk];          //signal (ZS) updates every ZS event (adc-pedestal-CMN vs.chip index)
      TH2* hHitMap_ZS[totDisk];          //hit density for each disk (phi vs. r -- 128*12 vs. 8)
      TH2* hDummyPolyHitMap_ZS[totDisk]; //hit density for each disk (phi val vs. r val -- 128*12 vs. 8)
      TH2* hPolyHitMap_ZS[totDisk];      //hit density for each disk (phi val vs. r val -- 128*12 vs. 8)
      TH2* hHitMapVsAPV_ZS[totDisk];     //hit map on APV for each disk (APV geometry ID vs. module geometry ID)
      TH2* hMultVsModule_zs[totDisk];    //total number of hits (ZS) per event vs. module for each disk
    };
  } hSumContents;
  //*** End Histogram Declarations...

  //The below histogram array defined for dynamical common mode noise distribution per chip
  //They are not wrotten to QA file
  union {
    TH1* hCmnPerChip[totAPV][4]; // each APV has 4 groups of CMN
  } hCmnTemp;

  int mAdcHist;
  int mMultHist;
  int mHitMapHist;
  int mTbVsAdcHist;
  int mEventSumHist;
  int mMipHist;
  int mMaxTimeBinHist;
  int mSumHist;

  JevpPlot** plots;
  daq_dta *dd;

  int numVals[totCh];
  int aVals[totCh];
  int numOverOneSig[totCh];
  short maxAdc[totCh];
  char  maxTimeBin[totCh];
  short maxAdc_zs[totCh];
  char  maxTimeBin_zs[totCh];
  float runningAvg[totCh];
  float runningStdDevSq[totCh];
  float sumAdc[totCh];
  float sum2Adc[totCh];
  int   couAdc[totCh];

  float oldStdDevs[totCh];
  float ranStdDevs[totCh];

  float cmNoise[totAPV][4]; // each APV has 4 groups of CMN
  bool isChannelBad[totCh];
  bool isNoisyApv[totAPV];
  //int rmsVals[totCh];

  int chCntDaq[totRdo][ArmPerRdo][ApvPerArm]; //num RDOs, ARM, APV, keep track of channel count per apv
  int apvCntDaq[totSec]; // sectors
  int nExpectedChip_Sec[totSec];

  int sumHistogramsFilled;
  int numTb;

  JLatex* errorMsg;

  ClassDef(fstBuilder, 1);
};
