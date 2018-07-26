// -*- mode: c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M
// 13 July 2012
//
// FMS trigger simulator
//

#ifndef ST_FMS_TRIGGER_MAKER_H
#define ST_FMS_TRIGGER_MAKER_H

class TH2F;

#include <cstring>
#if !defined(__CINT__) && !defined(__CLING__)
#include "Board.hh"
#include "Crate.hh"
#include "bits.hh"
#else
class Board;
class Crate;
#endif
#include "TDatime.h"
#include "StMaker.h"

class StFmsTriggerMaker : public StMaker {
public:
  StFmsTriggerMaker(const char* name = "fmstrigger");

  void Clear(Option_t* option = "");
  int Init();
  int InitRun(int runNumber);
  int Make();
  int Finish();

  // Input mode
  void useTrgData() { mUseTrgData = 1;}
  void useMuDst() { mUseMuDst = 1;}
  void useStEvent() { mUseStEvent = 1; }
  
  // Some controls
  void overwriteThr(char* name, int value);
  void forceRunNumber(int run) {mForceRun=run;}
  void useDsmData() {mUseDsmData=1;}
  void useQTSim()   {mUseDsmData=0;}

  //
  // QT and DSM algorithms and cabling for 2011:
  //
  // http://www.star.bnl.gov/public/trg/TSL/Software/FMS_2011.pdf
  // http://www.star.bnl.gov/public/trg/TSL/Schematics/FMS_Crate_Cable_Map.pdf
  // http://www.star.bnl.gov/public/trg/TSL/Schematics/FEQ_Crate_Cable_Map.pdf
  //
  int FP201output(int t=MAXPP) const { return fp201.output[t]; }
  int FM0xxoutput(int number, int t=MAXPP) const;
  int FM1xxoutput(int number, int t=MAXPP) const;

  int FP201input(int ch, int t=MAXPP) const; 
  int FM0xxinput(int number, int ch, int t=MAXPP) const;
  int FM1xxinput(int number, int ch, int t=MAXPP) const;

  int FP201data(int ch) const; 
  int FM0xxdata(int number, int ch, int t=MAXPP) const;
  int FM1xxdata(int number, int ch, int t=MAXPP) const;

  int FP201userdata(int ch, int t=MAXPP) const;
  int FM0xxuserdata(int number, int ch, int t=MAXPP) const;
  int FM1xxuserdata(int number, int ch, int t=MAXPP) const;

  int FmsHighTowerTh0(int t=MAXPP) const { return btest(fp201.output[t],0); } // Small cells
  int FmsHighTowerTh1(int t=MAXPP) const { return btest(fp201.output[t],1); } // Large cells
  int FmsSmallClusterTh0(int t=MAXPP) const { return btest(fp201.output[t],2); }
  int FmsSmallClusterTh1(int t=MAXPP) const { return btest(fp201.output[t],3); }
  int FmsSmallClusterTh2(int t=MAXPP) const { return btest(fp201.output[t],4); }
  int FmsLargeClusterTh0(int t=MAXPP) const { return btest(fp201.output[t],5); }
  int FmsLargeClusterTh1(int t=MAXPP) const { return btest(fp201.output[t],6); }
  int FmsLargeClusterTh2(int t=MAXPP) const { return btest(fp201.output[t],7); }
  int FmsJetPatchTh0(int t=MAXPP) const { return btest(fp201.output[t],8); }
  int FmsJetPatchTh1(int t=MAXPP) const { return btest(fp201.output[t],9); }
  int FmsJetPatchTh2(int t=MAXPP) const { return btest(fp201.output[t],10); }
  int FmsDijet(int t=MAXPP) const { return btest(fp201.output[t],11); }
  int FPE(int t=MAXPP) const { return btest(fp201.output[t],14); }
  
  //
  // Additions for 2012:
  //
  // http://www.star.bnl.gov/public/trg/TSL/Software/FMS.pdf
  //
  int FmsFPEcombo1(int t=MAXPP) const { return btest(fp201.output[t],12); }
  int FmsFPEcombo2(int t=MAXPP) const { return btest(fp201.output[t],13); }

private:
  int loadRegisters(int runNumber);
  int MakeMuDst();
  int MakeStEvent();
  int MakeTrgData();

  Crate& crateAt(int i) { return crates[i-1]; }
  const Crate& crateAt(int i) const { return crates[i-1]; }

  void runFpeQtLayer(int t=MAXPP);
  void runFmsQtLayer(int t=MAXPP);
  void runFmsLayer0(int t=MAXPP);
  void runFpeLayer1(int t=MAXPP);
  void runFmsLayer1(int t=MAXPP);
  void runFpdLayer2(int t=MAXPP);

  template<class T>
  void writeQtCrate(const T* hit, int t=MAXPP);
  void writeQtCrate(int crate, int slot, int ch, int adc, int t=MAXPP);
  void writeDsmData(int t=MAXPP);
  
  void writeFpeQtLayerToFpeLayer1(Crate& sim, int t=MAXPP);
  void writeFmsQtLayerToFmsLayer0(Crate& sim, int t=MAXPP);
  void writeFmsLayer0ToFmsLayer1(Crate& sim, int t=MAXPP);
  void writeFpeLayer1ToFpdLayer2(Crate& sim, int t=MAXPP);
  void writeFmsLayer1ToFpdLayer2(Crate& sim, int t=MAXPP);
  void writeFpdLayer2ToFpdLayer3(Crate& sim, int t=MAXPP);

  void fillQtHistogram(const Crate& qtcrate, TH2F* hqtadc, int t=MAXPP);
  void fillQtHistograms(int t=MAXPP);

  // DB time stamp
  TDatime mDBTime;

  // Input mode
  int mUseTrgData;
  int mUseMuDst;
  int mUseStEvent;

  // Crates
  enum { NCRATES = 14 };
#if !defined(__CINT__) && !defined(__CLING__)

  Crate crates[NCRATES];

  Crate& l1;
  Crate& fms;
  Crate& mix;
  Crate& feq;
  Crate& qt1;
  Crate& qt2;
  Crate& qt3;
  Crate& qt4;

  // L1 crate
  Board& fp201;

  // FMS crate
  Board& fm001;
  Board& fm002;
  Board& fm003;
  Board& fm004;
  Board& fm005;
  Board& fm006;
  Board& fm007;
  Board& fm008;
  Board& fm009;
  Board& fm010;
  Board& fm011;
  Board& fm012;
  Board& fm101;
  Board& fm102;
  Board& fm103;
  Board& fm104;

  // MIX crate
  Board& fe101;

  // FEQ crate
  Board& fe001;
  Board& fe002;
  Board& fe003;
  Board& fe004;
  Board& fs001;
  Board& fs002;
  Board& fs003;
  Board& fs004;
  Board& fs005;
  Board& fs006;
#else
  Crate *crates;

  Crate* l1;
  Crate* fms;
  Crate* mix;
  Crate* feq;
  Crate* qt1;
  Crate* qt2;
  Crate* qt3;
  Crate* qt4;

  // L1 crate
  Board* fp201;

  // FMS crate
  Board* fm001;
  Board* fm002;
  Board* fm003;
  Board* fm004;
  Board* fm005;
  Board* fm006;
  Board* fm007;
  Board* fm008;
  Board* fm009;
  Board* fm010;
  Board* fm011;
  Board* fm012;
  Board* fm101;
  Board* fm102;
  Board* fm103;
  Board* fm104;

  // MIX crate
  Board* fe101;

  // FEQ crate
  Board* fe001;
  Board* fe002;
  Board* fe003;
  Board* fe004;
  Board* fs001;
  Board* fs002;
  Board* fs003;
  Board* fs004;
  Board* fs005;
  Board* fs006;

#endif
  // QA histograms
  TH2F* hqt1adc;
  TH2F* hqt2adc;
  TH2F* hqt3adc;
  TH2F* hqt4adc;
  TH2F* hfeqadc;

  //run# overwrites
  Int_t mForceRun;

  //thresholds overwrites
  static const int MAX=100;
  Int_t mNThrOW;
  TString mThrOWName[MAX];
  Int_t mThrOWValue[MAX];
  
  //0 = Use simulatied QT data 1=use DSM input from data
  Int_t mUseDsmData;

  // # of pre/post from data
  Int_t mNPre;
  Int_t mNPost;

  //number of ADC=0xFFF
  Int_t mNFFF;

  ClassDef(StFmsTriggerMaker,0);
};

#endif // ST_FMS_TRIGGER_SIMU_H

