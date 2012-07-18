//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M
// 13 July 2012
//

#include <algorithm>
#include <cstring>
#include "TH2.h"
#include <mysql/mysql.h>
#include "StEvent/StEventTypes.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "Board.hh"
#include "Crate.hh"
#include "qt32b_fpe_2009_a.hh"
#include "qt32b_fms_2009_a.hh"
#include "fms_fm001_2011_a.hh"
#include "fms_fm005_2011_a.hh"
#include "fms_fm006_2011_a.hh"
#include "mix_fe101_2009_a.hh"
#include "fms_fm101_2011_a.hh"
#include "fms_fm102_2011_a.hh"
#include "l1_fp201_2011_a.hh"
#include "fms_fm001_2012_a.hh"
#include "fms_fm101_2012_a.hh"
#include "fms_fm102_2012_a.hh"
#include "l1_fp201_2012_b.hh"
#include "StFmsTriggerMaker.h"

using namespace std;

ClassImp(StFmsTriggerMaker);

StFmsTriggerMaker::StFmsTriggerMaker(const char* name)
  : StMaker(name)

  // Crates
  , l1(crateAt(L1_CONF_NUM))
  , fms(crateAt(FMS_CONF_NUM))
  , mix(crateAt(MIX_CONF_NUM))
  , feq(crateAt(FEQ_CONF_NUM))
  , qt1(crateAt(QT1_CONF_NUM))
  , qt2(crateAt(QT2_CONF_NUM))
  , qt3(crateAt(QT3_CONF_NUM))
  , qt4(crateAt(QT4_CONF_NUM))

  // L1 crate
  , fp201(l1.boardAt(FP201_BASE_ADDRESS))

  // FMS crate
  , fm001(fms.boardAt(FM001_BASE_ADDRESS))
  , fm002(fms.boardAt(FM002_BASE_ADDRESS))
  , fm003(fms.boardAt(FM003_BASE_ADDRESS))
  , fm004(fms.boardAt(FM004_BASE_ADDRESS))
  , fm101(fms.boardAt(FM101_BASE_ADDRESS))
  , fm005(fms.boardAt(FM005_BASE_ADDRESS))
  , fm006(fms.boardAt(FM006_BASE_ADDRESS))
  , fm007(fms.boardAt(FM007_BASE_ADDRESS))
  , fm008(fms.boardAt(FM008_BASE_ADDRESS))
  , fm102(fms.boardAt(FM102_BASE_ADDRESS))
  , fm009(fms.boardAt(FM009_BASE_ADDRESS))
  , fm010(fms.boardAt(FM010_BASE_ADDRESS))
  , fm011(fms.boardAt(FM011_BASE_ADDRESS))
  , fm012(fms.boardAt(FM012_BASE_ADDRESS))
  , fm103(fms.boardAt(FM103_BASE_ADDRESS))

  // MIX crate
  , fe101(mix.boardAt(FE101_BASE_ADDRESS))

  // FEQ crate
  , fe001(feq.boardAt(FE001_BASE_ADDRESS))
  , fe002(feq.boardAt(FE002_BASE_ADDRESS))
  , fe003(feq.boardAt(FE003_BASE_ADDRESS))
  , fe004(feq.boardAt(FE004_BASE_ADDRESS))
  , fs001(feq.boardAt(FS001_BASE_ADDRESS))
  , fs002(feq.boardAt(FS002_BASE_ADDRESS))
  , fs003(feq.boardAt(FS003_BASE_ADDRESS))
  , fs004(feq.boardAt(FS004_BASE_ADDRESS))
  , fs005(feq.boardAt(FS005_BASE_ADDRESS))
  , fs006(feq.boardAt(FS006_BASE_ADDRESS))

{
  // L1 crate
  fp201.setName("FP201");

  // FMS crate
  fm001.setName("FM001");
  fm002.setName("FM002");
  fm003.setName("FM003");
  fm004.setName("FM004");
  fm101.setName("FM101");
  fm005.setName("FM005");
  fm006.setName("FM006");
  fm007.setName("FM007");
  fm008.setName("FM008");
  fm102.setName("FM102");
  fm009.setName("FM009");
  fm010.setName("FM010");
  fm011.setName("FM011");
  fm012.setName("FM012");
  fm103.setName("FM103");

  // MIX crate
  fe101.setName("FE101");

  // FEQ crate
  fe001.setName("FE001");
  fe002.setName("FE002");
  fe003.setName("FE003");
  fe004.setName("FE004");
  fs001.setName("FS001");
  fs002.setName("FS002");
  fs003.setName("FS003");
  fs004.setName("FS004");
  fs005.setName("FS005");
  fs006.setName("FS006");

  fe001.bitmask = 0x01010101;
  fe002.bitmask = 0xff010101;
  fe003.bitmask = 0x01010101;
  fe004.bitmask = 0xff010101;

  // Input mode
  mUseMuDst = 0;
  mUseStEvent = 0;
}

void StFmsTriggerMaker::Clear(Option_t* option)
{
  qt1.clear();
  qt2.clear();
  qt3.clear();
  qt4.clear();
  feq.clear();
  fms.clear();
  l1.clear();
}

int StFmsTriggerMaker::Init()
{
  hqt1adc = new TH2F("hqt1adc","QT1 crate;QT channel;QT ADC",512,0,512,1024,0,1024);
  hqt2adc = new TH2F("hqt2adc","QT2 crate;QT channel;QT ADC",512,0,512,1024,0,1024);
  hqt3adc = new TH2F("hqt3adc","QT3 crate;QT channel;QT ADC",512,0,512,1024,0,1024);
  hqt4adc = new TH2F("hqt4adc","QT4 crate;QT channel;QT ADC",512,0,512,1024,0,1024);
  hfeqadc = new TH2F("hfeqadc","FEQ crate;QT channel;QT ADC",512,0,512,1024,0,1024);
  return kStOk;
}

int StFmsTriggerMaker::InitRun(int runNumber)
{
  mDBTime = GetDBTime();
  return loadRegisters(runNumber);
}

int StFmsTriggerMaker::Make()
{
  if (mUseMuDst) MakeMuDst();
  if (mUseStEvent) MakeStEvent();
  if (Debug()) fillQtHistograms();
  runFpeQtLayer();
  runFmsQtLayer();
  writeFpeQtLayerToFpeLayer1(mix);
  writeFmsQtLayerToFmsLayer0(fms);
  runFmsLayer0();
  writeFmsLayer0ToFmsLayer1(fms);
  runFpeLayer1();
  runFmsLayer1();
  writeFpeLayer1ToFpdLayer2(l1);
  writeFmsLayer1ToFpdLayer2(l1);
  runFpdLayer2();
  LOG_INFO << Form("FP201: Fms-HT-th0=%d Fms-HT-th1=%d FmsSml-Cluster-th0=%d FmsSml-Cluster-th1=%d FmsSml-Cluster-th2=%d FmsLrg-Cluster-th0=%d FmsLrg-Cluster-th1=%d FmsLrg-Cluster-th2=%d Fms-JP-th0=%d Fms-JP-th1=%d Fms-JP-th2=%d Fms-dijet=%d FPE=%d",FmsHighTowerTh0(),FmsHighTowerTh1(),FmsSmallClusterTh0(),FmsSmallClusterTh1(),FmsSmallClusterTh2(),FmsLargeClusterTh0(),FmsLargeClusterTh1(),FmsLargeClusterTh2(),FmsJetPatchTh0(),FmsJetPatchTh1(),FmsJetPatchTh2(),FmsDijet(),FPE()) << endm;
    //LOG_INFO << Form("FP201: 0x%04x (data) / 0x%04x (simu)",StMuDst::event()->l0Trigger().lastDsmArray(5),FP201output()) << endm;
  return kStOk;
}

int StFmsTriggerMaker::MakeMuDst()
{
  StMuFmsCollection* fms = StMuDst::muFmsCollection();
  if (fms) {
    for (size_t i = 0; i < fms->numberOfHits(); ++i) writeQtCrate(fms->getHit(i));
    return kStOk;
  }
  return kStErr;
}

int StFmsTriggerMaker::MakeStEvent()
{
  StEvent* event = (StEvent*)GetDataSet("StEvent");
  if (event && event->fmsCollection()) {
    const StSPtrVecFmsHit& hits = event->fmsCollection()->hits();
    for (size_t i = 0; i < hits.size(); ++i) writeQtCrate(hits[i]);
    return kStOk;
  }
  return kStErr;
}

template<class T>
void StFmsTriggerMaker::writeQtCrate(const T* hit)
{
  switch (hit->detectorId()) {
  case 0: // fpd north
    {
      int qtdaughter = (hit->channel()-1)/7;
      int qtchannel  = (hit->channel()-1)%7;
      switch (qtdaughter/4) {
      case 0: fe001.channels[(qtdaughter%4)*8+qtchannel] = hit->adc(); break;
      case 1: fe002.channels[(qtdaughter%4)*8+qtchannel] = hit->adc(); break;
      }
    }
    break;
  case 1: // fpd south
    {
      int qtdaughter = (hit->channel()-1)/7;
      int qtchannel  = (hit->channel()-1)%7;
      switch (qtdaughter/4) {
      case 0: fe003.channels[(qtdaughter%4)*8+qtchannel] = hit->adc(); break;
      case 1: fe004.channels[(qtdaughter%4)*8+qtchannel] = hit->adc(); break;
      }
    }
    break;
  case 2: // fpd preshower north
    fe002.channels[24+hit->channel()-1] = hit->adc();
    break;
  case 3: // fpd preshower south
    fe004.channels[24+hit->channel()-1] = hit->adc();
    break;
  case 4: // fpd north smdv
  case 5: // fpd south smdv
  case 6: // fpd north smdh
  case 7: // fpd south smdh
    break;
  case 8: // fms north large
  case 9: // fms south large
  case 10: // fms north small
  case 11: // fms south small
    switch (hit->qtCrate()) {
    case 1: qt1.boards[hit->qtSlot()-1].channels[hit->qtChannel()] = hit->adc(); break;
    case 2: qt2.boards[hit->qtSlot()-1].channels[hit->qtChannel()] = hit->adc(); break;
    case 3: qt3.boards[hit->qtSlot()-1].channels[hit->qtChannel()] = hit->adc(); break;
    case 4: qt4.boards[hit->qtSlot()-1].channels[hit->qtChannel()] = hit->adc(); break;
    }
    break;
  case 12: // fhc north
  case 13: // fhc south
    break;
  }
}

void StFmsTriggerMaker::fillQtHistogram(const Crate& qtcrate, TH2F* hqtadc)
{
  for (int slot = 0; slot < 16; ++slot) {
    for (int channel = 0; channel < 32; ++channel) {
      int idx = slot*32+channel;
      int adc = qtcrate.boards[slot].channels[channel];
      hqtadc->Fill(idx,adc);
    }
  }
}

void StFmsTriggerMaker::fillQtHistograms()
{
  fillQtHistogram(qt1,hqt1adc);
  fillQtHistogram(qt2,hqt2adc);
  fillQtHistogram(qt3,hqt3adc);
  fillQtHistogram(qt4,hqt4adc);
  fillQtHistogram(feq,hfeqadc);
}

void StFmsTriggerMaker::runFpeQtLayer()
{
  qt32b_fpe_2009_a(fe001);
  qt32b_fpe_2009_a(fe002);
  qt32b_fpe_2009_a(fe003);
  qt32b_fpe_2009_a(fe004);
}

void StFmsTriggerMaker::runFmsQtLayer()
{
  for_each(qt1.boards,qt1.boards+12,qt32b_fms_2009_a);
  for_each(qt2.boards,qt2.boards+12,qt32b_fms_2009_a);
  for_each(qt3.boards,qt3.boards+12,qt32b_fms_2009_a);
  for_each(qt4.boards,qt4.boards+12,qt32b_fms_2009_a);
}

void StFmsTriggerMaker::runFmsLayer0()
{
  switch (mDBTime.GetYear()) {
  case 2011:
    fms_fm001_2011_a(fm001);
    fms_fm001_2011_a(fm002);
    fms_fm001_2011_a(fm003);
    fms_fm001_2011_a(fm004);
    break;
  case 2012:
    fms_fm001_2012_a(fm001);
    fms_fm001_2012_a(fm002);
    fms_fm001_2012_a(fm003);
    fms_fm001_2012_a(fm004);
    break;
  }

  fms_fm005_2011_a(fm005);
  fms_fm005_2011_a(fm007);
  fms_fm005_2011_a(fm009);
  fms_fm005_2011_a(fm011);

  fms_fm006_2011_a(fm006);
  fms_fm006_2011_a(fm008);
  fms_fm006_2011_a(fm010);
  fms_fm006_2011_a(fm012);
}

void StFmsTriggerMaker::runFpeLayer1()
{
  mix_fe101_2009_a(fe101);
}

void StFmsTriggerMaker::runFmsLayer1()
{
  switch (mDBTime.GetYear()) {
  case 2011:
    fms_fm101_2011_a(fm101);
    fms_fm102_2011_a(fm102);
    fms_fm102_2011_a(fm103);
    break;
  case 2012:
    fms_fm101_2012_a(fm101);
    fms_fm102_2012_a(fm102);
    fms_fm102_2012_a(fm103);
    break;
  }
}

void StFmsTriggerMaker::runFpdLayer2()
{
  switch (mDBTime.GetYear()) {
  case 2011: l1_fp201_2011_a(fp201); break;
  case 2012: l1_fp201_2012_b(fp201); break;
  }
}

void StFmsTriggerMaker::writeFpeQtLayerToFpeLayer1(Crate& sim)
{
  Board& fe101sim = sim.boardAt(FE101_BASE_ADDRESS);
  int* fe101simchannels = (int*)fe101sim.channels;
  fe101simchannels[0] = fe001.output;
  fe101simchannels[1] = fe002.output;
  fe101simchannels[2] = fe003.output;
  fe101simchannels[3] = fe004.output;
}

void StFmsTriggerMaker::writeFmsQtLayerToFmsLayer0(Crate& sim)
{
  Board& fm001sim = sim.boardAt(FM001_BASE_ADDRESS);
  Board& fm002sim = sim.boardAt(FM002_BASE_ADDRESS);
  Board& fm003sim = sim.boardAt(FM003_BASE_ADDRESS);
  Board& fm004sim = sim.boardAt(FM004_BASE_ADDRESS);

  Board& fm005sim = sim.boardAt(FM005_BASE_ADDRESS);
  Board& fm006sim = sim.boardAt(FM006_BASE_ADDRESS);
  Board& fm007sim = sim.boardAt(FM007_BASE_ADDRESS);
  Board& fm008sim = sim.boardAt(FM008_BASE_ADDRESS);

  Board& fm009sim = sim.boardAt(FM009_BASE_ADDRESS);
  Board& fm010sim = sim.boardAt(FM010_BASE_ADDRESS);
  Board& fm011sim = sim.boardAt(FM011_BASE_ADDRESS);
  Board& fm012sim = sim.boardAt(FM012_BASE_ADDRESS);

  for (int ch = 0; ch < 4; ++ch) {
    ((int*)fm001sim.channels)[ch] = qt1.boards[3-ch].output;
    ((int*)fm002sim.channels)[ch] = qt2.boards[3-ch].output;
    ((int*)fm003sim.channels)[ch] = qt3.boards[3-ch].output;
    ((int*)fm004sim.channels)[ch] = qt4.boards[3-ch].output;
    ((int*)fm005sim.channels)[ch] = qt1.boards[7-ch].output;
    ((int*)fm007sim.channels)[ch] = qt2.boards[7-ch].output;
    ((int*)fm009sim.channels)[ch] = qt3.boards[7-ch].output;
    ((int*)fm011sim.channels)[ch] = qt4.boards[7-ch].output;
  }

  for (int ch = 0; ch < 2; ++ch) {
    ((int*)fm006sim.channels)[ch] = qt1.boards[9-ch].output;
    ((int*)fm008sim.channels)[ch] = qt2.boards[9-ch].output;
    ((int*)fm010sim.channels)[ch] = qt3.boards[9-ch].output;
    ((int*)fm012sim.channels)[ch] = qt4.boards[9-ch].output;
  }

  for (int ch = 2; ch < 4; ++ch) {
    ((int*)fm006sim.channels)[ch] = 0xffffffff;
    ((int*)fm008sim.channels)[ch] = 0xffffffff;
    ((int*)fm010sim.channels)[ch] = 0xffffffff;
    ((int*)fm012sim.channels)[ch] = 0xffffffff;
  }
}

void StFmsTriggerMaker::writeFmsLayer0ToFmsLayer1(Crate& sim)
{
  Board& fm101sim = sim.boardAt(FM101_BASE_ADDRESS);
  Board& fm102sim = sim.boardAt(FM102_BASE_ADDRESS);
  Board& fm103sim = sim.boardAt(FM103_BASE_ADDRESS);

  for (int ch = 0; ch < 4; ++ch) {
    ((int*)fm101sim.channels)[ch] = fms.boards[ch   ].output;
    ((int*)fm102sim.channels)[ch] = fms.boards[ch+5 ].output;
    ((int*)fm103sim.channels)[ch] = fms.boards[ch+10].output;
  }
}

void StFmsTriggerMaker::writeFpeLayer1ToFpdLayer2(Crate& sim)
{
  Board& fp201sim = sim.boardAt(FP201_BASE_ADDRESS);
  fp201sim.channels[6] = 0xffff; // unused
  fp201sim.channels[7] = fe101.output;
}

void StFmsTriggerMaker::writeFmsLayer1ToFpdLayer2(Crate& sim)
{
  Board& fp201sim = sim.boardAt(FP201_BASE_ADDRESS);
  int* fp201simchannels = (int*)fp201sim.channels;

  fp201simchannels[0] = fm101.output;
  fp201simchannels[1] = fm102.output;
  fp201simchannels[2] = fm103.output;
}

int StFmsTriggerMaker::loadRegisters(int runNumber)
{
  MYSQL mysql;
  const char* host = "dbbak.starp.bnl.gov";
  const char* user = "";
  const char* pass = "";
  // See http://drupal.star.bnl.gov/STAR/comp/db/onlinedb/online-sever-port-map
  unsigned int port = 3400+mDBTime.GetYear()%100-1;
  const char* database = "Conditions_rts";
  const char* unix_socket = NULL;
  unsigned long client_flag = 0;
  char query[1024];

  LOG_INFO << Form("host=%s user=\"%s\" pass=\"%s\" port=%d database=%s",host,user,pass,port,database) << endm;

  mysql_init(&mysql);

  // Open connection to online database
  if (!mysql_real_connect(&mysql,host,user,pass,database,port,unix_socket,client_flag)) {
    LOG_ERROR << "Can't connect to database: " << mysql_error(&mysql) << endm;
    return kStErr;
  }

  // Get run number associated with DB time stamp
  sprintf(query,"select max(idx_rn) from triggers where beginTime <= '%s'",mDBTime.AsSQLString());
  LOG_INFO << query << endm;
  mysql_query(&mysql,query);
  if (MYSQL_RES* result = mysql_store_result(&mysql)) {
    while (MYSQL_ROW row = mysql_fetch_row(result)) {
      runNumber = atoi(row[0]);
    }
    mysql_free_result(result);
  }

  // Get register values
  // object=DSM crate, idx=DSM board
  sprintf(query,"select object,idx,reg,label,value,defaultvalue from dict where hash=(select dicthash from run where idx_rn = %d)",runNumber);
  LOG_INFO << query << endm;
  mysql_query(&mysql,query);
  if (MYSQL_RES* result = mysql_store_result(&mysql)) {
    LOG_INFO << setw(10) << "object" << setw(10) << "idx" << setw(10) << "reg" << setw(30) << "label" << setw(10) << "value" << setw(15) << "defaultvalue" << endm;
    while (MYSQL_ROW row = mysql_fetch_row(result)) {
      int object = atoi(row[0]);
      int idx = atoi(row[1]);
      int reg = atoi(row[2]);
      TString label = row[3];
      int value = atoi(row[4]);
      int defaultvalue = atoi(row[5]);
      LOG_INFO << setw(10) << object << setw(10) << idx << setw(10) << reg << setw(30) << label << setw(10) << value << setw(15) << defaultvalue << endm;
      if (object >= 1 && object <= NCRATES && idx >= 0x10) {
	LOG_INFO << object << '\t' << idx << '\t' << reg << '\t' << label << '\t' << value << '\t' << defaultvalue << endm;
	crateAt(object).boardAt(idx<<24).registerAt(reg<<24) = (value == -1) ? defaultvalue : value;
      }
    }
    mysql_free_result(result);
  }

  // Close connection to online database
  mysql_close(&mysql);

  return kStOk;
}
