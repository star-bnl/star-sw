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
#include "Board.hh"
#include "Crate.hh"
#include "bits.hh"
#include "StMaker.h"

class StFmsTriggerMaker : public StMaker {
public:
  StFmsTriggerMaker(const char* name = "fmstrigger");

  void Clear(Option_t* option = "");
  int Init();
  int InitRun(int runNumber);
  int Make();

  // Input mode
  void useMuDst() { mUseMuDst = 1;}
  void useStEvent() { mUseStEvent = 1; }

  //
  // QT and DSM algorithms and cabling:
  //
  // http://www.star.bnl.gov/public/trg/TSL/Software/FMS_2011.pdf
  // http://www.star.bnl.gov/public/trg/TSL/Schematics/FMS_Crate_Cable_Map.pdf
  // http://www.star.bnl.gov/public/trg/TSL/Schematics/FEQ_Crate_Cable_Map.pdf
  //
  int FP201output() const { return fp201.output; }
  int FmsHighTowerTh0() const { return btest(fp201.output,0); } // Small cells
  int FmsHighTowerTh1() const { return btest(fp201.output,1); } // Large cells
  int FmsSmallClusterTh0() const { return btest(fp201.output,2); }
  int FmsSmallClusterTh1() const { return btest(fp201.output,3); }
  int FmsSmallClusterTh2() const { return btest(fp201.output,4); }
  int FmsLargeClusterTh0() const { return btest(fp201.output,5); }
  int FmsLargeClusterTh1() const { return btest(fp201.output,6); }
  int FmsLargeClusterTh2() const { return btest(fp201.output,7); }
  int FmsJetPatchTh0() const { return btest(fp201.output,8); }
  int FmsJetPatchTh1() const { return btest(fp201.output,9); }
  int FmsJetPatchTh2() const { return btest(fp201.output,10); }
  int FmsDijet() const { return btest(fp201.output,11); }
  int FPE() const { return btest(fp201.output,14); }

private:
  int loadRegisters(int runNumber);
  int MakeMuDst();
  int MakeStEvent();

  Crate& crateAt(int i) { return crates[i-1]; }
  const Crate& crateAt(int i) const { return crates[i-1]; }

  void runFpeQtLayer();
  void runFmsQtLayer();
  void runFmsLayer0();
  void runFpeLayer1();
  void runFmsLayer1();
  void runFpdLayer2();

  template<class T>
  void writeQtCrate(const T* hit);
  void writeFpeQtLayerToFpeLayer1(Crate& sim);
  void writeFmsQtLayerToFmsLayer0(Crate& sim);
  void writeFmsLayer0ToFmsLayer1(Crate& sim);
  void writeFpeLayer1ToFpdLayer2(Crate& sim);
  void writeFmsLayer1ToFpdLayer2(Crate& sim);
  void writeFpdLayer2ToFpdLayer3(Crate& sim);

  void fillQtHistogram(const Crate& qtcrate, TH2F* hqtadc);
  void fillQtHistograms();

  // Input mode
  int mUseMuDst;
  int mUseStEvent;

  // Crates
  enum { NCRATES = 14 };

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
  Board& fm101;
  Board& fm005;
  Board& fm006;
  Board& fm007;
  Board& fm008;
  Board& fm102;
  Board& fm009;
  Board& fm010;
  Board& fm011;
  Board& fm012;
  Board& fm103;

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

  // QA histograms
  TH2F* hqt1adc;
  TH2F* hqt2adc;
  TH2F* hqt3adc;
  TH2F* hqt4adc;
  TH2F* hfeqadc;

  ClassDef(StFmsTriggerMaker,0);
};

#endif // ST_FMS_TRIGGER_SIMU_H

