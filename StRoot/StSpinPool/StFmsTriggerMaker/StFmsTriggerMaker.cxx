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
#include "qt32b_fms_2015_a.hh"
#include "fms_fm001_2015_a.hh"
#include "fms_fm005_2015_a.hh"
#include "fms_fm006_2015_a.hh"
#include "fms_fm101_2015_a.hh"
#include "fms_fm101_2015_b.hh"
#include "fms_fm103_2015_a.hh"
#include "l1_fp201_2015_a.hh"
#include "l1_fp201_2015_b.hh"
#include "fms_fm001_2017_a.hh"
#include "fms_fm005_2017_a.hh"
#include "fms_fm006_2017_a.hh"
#include "fms_fm101_2017_a.hh"
#include "fms_fm101_2017_a.hh"
#include "fms_fm103_2017_a.hh"
#include "l1_fp201_2017_a.hh"
#include "l1_fp201_2017_a.hh"
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
  , fm005(fms.boardAt(FM005_BASE_ADDRESS))
  , fm006(fms.boardAt(FM006_BASE_ADDRESS))
  , fm007(fms.boardAt(FM007_BASE_ADDRESS))
  , fm008(fms.boardAt(FM008_BASE_ADDRESS))
  , fm009(fms.boardAt(FM009_BASE_ADDRESS))
  , fm010(fms.boardAt(FM010_BASE_ADDRESS))
  , fm011(fms.boardAt(FM011_BASE_ADDRESS))
  , fm012(fms.boardAt(FM012_BASE_ADDRESS))
  , fm101(fms.boardAt(FM101_BASE_ADDRESS))
  , fm102(fms.boardAt(FM102_BASE_ADDRESS))
  , fm103(fms.boardAt(FM103_BASE_ADDRESS))  
  , fm104(fms.boardAt(FM104_BASE_ADDRESS))  
  
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

  , mForceRun(0)
  , mNThrOW(0)
  , mUseDsmData(0)
  , mNPre(0)
  , mNPost(0)
{
  // L1 crate
  fp201.setName("FP201");

  // FMS crate
  fm001.setName("FM001");
  fm002.setName("FM002");
  fm003.setName("FM003");
  fm004.setName("FM004");
  fm005.setName("FM005");
  fm006.setName("FM006");
  fm007.setName("FM007");
  fm008.setName("FM008");
  fm009.setName("FM009");
  fm010.setName("FM010");
  fm011.setName("FM011");
  fm012.setName("FM012");
  fm101.setName("FM101");
  fm102.setName("FM102");
  fm103.setName("FM103");
  fm104.setName("FM104");

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

  //QT1-4
  for(int i=0; i<16; i++){
    qt1.boards[i].setName(Form("QT1.0x%02x",i+0x10));
    qt2.boards[i].setName(Form("QT2.0x%02x",i+0x10));
    qt3.boards[i].setName(Form("QT3.0x%02x",i+0x10));
    qt4.boards[i].setName(Form("QT4.0x%02x",i+0x10));
  }

  // Input mode
  mUseTrgData = 0;
  mUseMuDst = 0;
  mUseStEvent = 0;

  //ADC=0xFFF counter
  mNFFF=0;
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

int StFmsTriggerMaker::InitRun(int runNumber){  
  //mDBTime = GetDBTime();
  //mDBTime = TDatime();
  return loadRegisters(runNumber);
}

int StFmsTriggerMaker::Finish(){  
  printf("%12d      Number of ADC=0xFFF\n",mNFFF);
  return kStOK;
}

int StFmsTriggerMaker::Make(){
  if (mUseTrgData) MakeTrgData();
  if (mUseMuDst) MakeMuDst();
  if (mUseStEvent) MakeStEvent();
  if (Debug()) fillQtHistograms();
  for(int t=0; t<MAXT; t++){
    int x=t-MAXPP;
    if(-x>mNPre || x>mNPost) continue;
    writeDsmData(t);
    if(mDBTime.GetYear()<2015){
      runFpeQtLayer(t);
      writeFpeQtLayerToFpeLayer1(mix,t);
      runFpeLayer1(t);
      writeFpeLayer1ToFpdLayer2(l1,t);
    }
    runFmsQtLayer(t);
    writeFmsQtLayerToFmsLayer0(fms,t);
    runFmsLayer0(t);
    writeFmsLayer0ToFmsLayer1(fms,t);
    runFmsLayer1(t);
    writeFmsLayer1ToFpdLayer2(l1,t);
    runFpdLayer2(t);
  }
  //LOG_INFO << Form("FP201: Fms-HT-th0=%d Fms-HT-th1=%d FmsSml-Cluster-th0=%d FmsSml-Cluster-th1=%d FmsSml-Cluster-th2=%d FmsLrg-Cluster-th0=%d FmsLrg-Cluster-th1=%d FmsLrg-Cluster-th2=%d Fms-JP-th0=%d Fms-JP-th1=%d Fms-JP-th2=%d Fms-dijet=%d FPE=%d",FmsHighTowerTh0(),FmsHighTowerTh1(),FmsSmallClusterTh0(),FmsSmallClusterTh1(),FmsSmallClusterTh2(),FmsLargeClusterTh0(),FmsLargeClusterTh1(),FmsLargeClusterTh2(),FmsJetPatchTh0(),FmsJetPatchTh1(),FmsJetPatchTh2(),FmsDijet(),FPE()) << endm;
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
  // printf("Getting StEvent and fmsCollection %x %x\n",event,event->fmsCollection());  
  if (event && event->fmsCollection()) {    
    //printf("Found StEvent and fmsCollection\n");
    const StSPtrVecFmsHit& hits = event->fmsCollection()->hits();
    for (size_t i = 0; i < hits.size(); ++i) writeQtCrate(hits[i]);
    return kStOk;
  }
  return kStErr;
}

int StFmsTriggerMaker::MakeTrgData(){
  StTriggerData* trgd=(StTriggerData*)GetDataSet("StTriggerData")->GetObject();
  if(!trgd) {printf("MakeTrgData found no trigger data\n"); return kStErr;}
  mNPre=trgd->numberOfPreXing();
  mNPost=trgd->numberOfPostXing();
  //printf("StFmsTriggerMaker::MakeTrgData found npre=%d npost=%d\n",mNPre,mNPost);
  int n=0;
  for(int t=0; t<MAXT; t++){
    int x=t-MAXPP;
    if(-x>mNPre || x>mNPost) continue;
    for(int crt=1; crt<=4; crt++){
      for(int adr=0; adr<12; adr++){
	for(int ch=0; ch<32; ch++){
	  int adc=trgd->fmsADC(crt,adr,ch,x);
	  if(adc>0) {
	    writeQtCrate(crt,adr,ch,adc,t); 
	    n++;
	    if(adc==0xFFF) {
	      printf("0xFFF problem : Crt=%2d Adr=%2d ch=%2d ADC=%4d\n",crt,adr,ch,adc);
	      mNFFF++;	      
	    }
	    //	    if(crt==2 && adr==0) printf("Crt=%2d Adr=%2d ch=%2d ADC=%4d\n",crt,adr,ch,adc);
	  }
	}
      }
    }
  }
  //printf("StFmsTriggerMaker::MakeTrgData found %d hits\n",n);
  //hack FAKE stuck bits -akio
  /*
  for(int t=0; t<MAXT; t++){
    int x=t-MAXPP;
    if(-x>mNPre|| x>mNPost) continue;
    writeQtCrate(3,2,16,0x115,t);
    writeQtCrate(4,5, 0,0x040,t);
  }
  */
  return kStOk;
}

unsigned char* dFMS;
unsigned short dFP201[8];  
inline unsigned int getDSMdata(int slot, int ch){
  static const int chadd[4]={7,3,15,11};
  static const int chadd2[4]={3,1,7,5};
  if(slot<16){
    int add=slot*16+chadd[ch];
    return dFMS[add] + (dFMS[add-1]<<8) + (dFMS[add-2]<<16) + (dFMS[add-3]<<24);
  }else if(slot==16){
    int add=chadd2[ch];
    return dFP201[add] + (dFP201[add-1]<<16);
  }
  return 0;
}  

void StFmsTriggerMaker::writeDsmData(int t){
  StTriggerData* trgd=(StTriggerData*)GetDataSet("StTriggerData")->GetObject();  
  if(!trgd) {printf("writeDsmData found no trigger data\n"); return; }
  int x=t-MAXPP;
  dFMS=trgd->getDsm_FMS(x);
  if(x==0) for(int i=0; i<8; i++) dFP201[i]=trgd->fpdLayer2DSMRaw(i);
  for(int ch=0; ch<4; ch++){
    ((int*)fm001.dsmdata[t])[ch] = getDSMdata(0,ch);
    ((int*)fm002.dsmdata[t])[ch] = getDSMdata(1,ch);
    ((int*)fm003.dsmdata[t])[ch] = getDSMdata(3,ch);
    ((int*)fm004.dsmdata[t])[ch] = getDSMdata(4,ch);
    ((int*)fm005.dsmdata[t])[ch] = getDSMdata(6,ch);
    ((int*)fm006.dsmdata[t])[ch] = getDSMdata(7,ch);
    ((int*)fm007.dsmdata[t])[ch] = getDSMdata(8,ch);
    ((int*)fm008.dsmdata[t])[ch] = getDSMdata(9,ch);
    ((int*)fm009.dsmdata[t])[ch] = getDSMdata(11,ch);
    ((int*)fm010.dsmdata[t])[ch] = getDSMdata(12,ch);
    ((int*)fm011.dsmdata[t])[ch] = getDSMdata(13,ch);
    ((int*)fm012.dsmdata[t])[ch] = getDSMdata(14,ch);
    ((int*)fm101.dsmdata[t])[ch] = getDSMdata(2,ch);
    ((int*)fm102.dsmdata[t])[ch] = getDSMdata(5,ch);
    ((int*)fm103.dsmdata[t])[ch] = getDSMdata(10,ch);
    ((int*)fm104.dsmdata[t])[ch] = getDSMdata(15,ch);
    if(x==0) ((int*)fp201.dsmdata[t])[ch] = getDSMdata(16,ch);
  }

  //FAKE!!! FM006 4th int should be 0xFFFFFFFF since not connected... but shows some activities fake increasing mismatch%. Masking out -akio
  ((int*)fm006.dsmdata[t])[3] = 0xFFFFFFFF;
  //FAKE!!! FM010 3rd int should be 0xFFFFFFFF since not connected... but shows some activities fake increasing mismatch%. Masking out -akio
  ((int*)fm010.dsmdata[t])[2] = 0xFFFFFFFF;
  //FAKE!!! FP201 4th int bit0 is stuck high and not in use after 2015b algo is in. Masking out to get real mismatch% -akio
  if(mForceRun>=16056024)
    ((int*)fp201.dsmdata[t])[3] = (((int*)fp201.dsmdata[t])[3]) & 0xfffffffe;

  //printf("StFmsTriggerMaker::writeDsmData x=%d\n",x);
}

template<class T>
void StFmsTriggerMaker::writeQtCrate(const T* hit, int t){
  switch (hit->detectorId()) {
  case 0: // fpd north
    {
      int qtdaughter = (hit->channel()-1)/7;
      int qtchannel  = (hit->channel()-1)%7;
      switch (qtdaughter/4) {
      case 0: fe001.channels[t][(qtdaughter%4)*8+qtchannel] = hit->adc(); break;
      case 1: fe002.channels[t][(qtdaughter%4)*8+qtchannel] = hit->adc(); break;
      }
    }
    break;
  case 1: // fpd south
    {
      int qtdaughter = (hit->channel()-1)/7;
      int qtchannel  = (hit->channel()-1)%7;
      switch (qtdaughter/4) {
      case 0: fe003.channels[t][(qtdaughter%4)*8+qtchannel] = hit->adc(); break;
      case 1: fe004.channels[t][(qtdaughter%4)*8+qtchannel] = hit->adc(); break;
      }
    }
    break;
  case 2: // fpd preshower north
    fe002.channels[t][24+hit->channel()-1] = hit->adc();
    break;
  case 3: // fpd preshower south
    fe004.channels[t][24+hit->channel()-1] = hit->adc();
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
    case 1: qt1.boards[hit->qtSlot()-1].channels[t][hit->qtChannel()] = hit->adc(); break;
    case 2: qt2.boards[hit->qtSlot()-1].channels[t][hit->qtChannel()] = hit->adc(); break;
    case 3: qt3.boards[hit->qtSlot()-1].channels[t][hit->qtChannel()] = hit->adc(); break;
    case 4: qt4.boards[hit->qtSlot()-1].channels[t][hit->qtChannel()] = hit->adc(); break;
    }
    break;
  case 12: // fhc north
  case 13: // fhc south
    break;
  }
}

void StFmsTriggerMaker::writeQtCrate(int crate, int slot, int ch, int adc, int t){
  switch (crate) {
  case 1: qt1.boards[slot].channels[t][ch] = adc; break;
  case 2: qt2.boards[slot].channels[t][ch] = adc; break;
  case 3: qt3.boards[slot].channels[t][ch] = adc; break;
  case 4: qt4.boards[slot].channels[t][ch] = adc; break;
  }
}

void StFmsTriggerMaker::fillQtHistogram(const Crate& qtcrate, TH2F* hqtadc, int t){
  for (int slot = 0; slot < 16; ++slot) {
    for (int channel = 0; channel < 32; ++channel) {
      int idx = slot*32+channel;
      int adc = qtcrate.boards[slot].channels[t][channel];
      hqtadc->Fill(idx,adc);
    }
  }
}

void StFmsTriggerMaker::fillQtHistograms(int t){
  fillQtHistogram(qt1,hqt1adc,t);
  fillQtHistogram(qt2,hqt2adc,t);
  fillQtHistogram(qt3,hqt3adc,t);
  fillQtHistogram(qt4,hqt4adc,t);
  fillQtHistogram(feq,hfeqadc,t);
}

void StFmsTriggerMaker::runFpeQtLayer(int t){
  qt32b_fpe_2009_a(fe001,t);
  qt32b_fpe_2009_a(fe002,t);
  qt32b_fpe_2009_a(fe003,t);
  qt32b_fpe_2009_a(fe004,t);
}

void StFmsTriggerMaker::runFmsQtLayer(int t){  
  for(int i=0; i<12; i++){
    if(mDBTime.GetYear()<2015){
      qt32b_fms_2009_a(qt1.boards[i],t);
      qt32b_fms_2009_a(qt2.boards[i],t);
      qt32b_fms_2009_a(qt3.boards[i],t);
      qt32b_fms_2009_a(qt4.boards[i],t);
    }else{
      qt32b_fms_2015_a(qt1.boards[i],t);
      qt32b_fms_2015_a(qt2.boards[i],t);
      qt32b_fms_2015_a(qt3.boards[i],t);
      qt32b_fms_2015_a(qt4.boards[i],t);
    }
  }
}

void StFmsTriggerMaker::runFmsLayer0(int t){
  switch (mDBTime.GetYear()) {
  case 2011:
    fms_fm001_2011_a(fm001,t);
    fms_fm001_2011_a(fm002,t);
    fms_fm001_2011_a(fm003,t);
    fms_fm001_2011_a(fm004,t);
    fms_fm005_2011_a(fm005,t);
    fms_fm005_2011_a(fm007,t);
    fms_fm005_2011_a(fm009,t);
    fms_fm005_2011_a(fm011,t);    
    fms_fm006_2011_a(fm006,t);
    fms_fm006_2011_a(fm008,t);
    fms_fm006_2011_a(fm010,t);
    fms_fm006_2011_a(fm012,t);
    break;
  case 2012: case 2013:
    fms_fm001_2012_a(fm001,t);
    fms_fm001_2012_a(fm002,t);
    fms_fm001_2012_a(fm003,t);
    fms_fm001_2012_a(fm004,t);
    fms_fm005_2011_a(fm005,t);
    fms_fm005_2011_a(fm007,t);
    fms_fm005_2011_a(fm009,t);
    fms_fm005_2011_a(fm011,t);    
    fms_fm006_2011_a(fm006,t);
    fms_fm006_2011_a(fm008,t);
    fms_fm006_2011_a(fm010,t);
    fms_fm006_2011_a(fm012,t);
    break;
  case 2015:
    fms_fm001_2015_a(fm001,t,mUseDsmData);
    fms_fm001_2015_a(fm002,t,mUseDsmData);
    fms_fm001_2015_a(fm003,t,mUseDsmData);
    fms_fm001_2015_a(fm004,t,mUseDsmData);
    fms_fm005_2015_a(fm005,t,mUseDsmData);
    fms_fm006_2015_a(fm006,t,mUseDsmData);
    fms_fm005_2015_a(fm007,t,mUseDsmData);
    fms_fm006_2015_a(fm008,t,mUseDsmData);
    fms_fm005_2015_a(fm009,t,mUseDsmData);
    fms_fm006_2015_a(fm010,t,mUseDsmData);
    fms_fm005_2015_a(fm011,t,mUseDsmData);
    fms_fm006_2015_a(fm012,t,mUseDsmData);
    break;
  case 2017:
  default:
    fms_fm001_2017_a(fm001,t,mUseDsmData);
    fms_fm001_2017_a(fm002,t,mUseDsmData);
    fms_fm001_2017_a(fm003,t,mUseDsmData);
    fms_fm001_2017_a(fm004,t,mUseDsmData);
    fms_fm005_2017_a(fm005,t,mUseDsmData);
    fms_fm006_2017_a(fm006,t,mUseDsmData);
    fms_fm005_2017_a(fm007,t,mUseDsmData);
    fms_fm006_2017_a(fm008,t,mUseDsmData);
    fms_fm005_2017_a(fm009,t,mUseDsmData);
    fms_fm006_2017_a(fm010,t,mUseDsmData);
    fms_fm005_2017_a(fm011,t,mUseDsmData);
    fms_fm006_2017_a(fm012,t,mUseDsmData);
    break;
  }
}

void StFmsTriggerMaker::runFpeLayer1(int t){
  mix_fe101_2009_a(fe101,t);
}

void StFmsTriggerMaker::runFmsLayer1(int t){
  switch (mDBTime.GetYear()) {
  case 2011:
    fms_fm101_2011_a(fm101,t);
    fms_fm102_2011_a(fm102,t);
    fms_fm102_2011_a(fm103,t);
    break;
  case 2012: case 2013:
    fms_fm101_2012_a(fm101,t);
    fms_fm102_2012_a(fm102,t);
    fms_fm102_2012_a(fm103,t);
    break;
  case 2015:
    if(mForceRun<16056024){ 
      fms_fm101_2015_a(fm101,t,mUseDsmData);
      fms_fm101_2015_a(fm102,t,mUseDsmData);
    }else{                
      fms_fm101_2015_b(fm101,t,mUseDsmData);
      fms_fm101_2015_b(fm102,t,mUseDsmData);
    }
    fms_fm103_2015_a(fm103,t,mUseDsmData);
    fms_fm103_2015_a(fm104,t,mUseDsmData);
    break;
  case 2017:
  default:
    fms_fm101_2017_a(fm101,t,mUseDsmData);
    fms_fm101_2017_a(fm102,t,mUseDsmData);
    fms_fm103_2017_a(fm103,t,mUseDsmData);
    fms_fm103_2017_a(fm104,t,mUseDsmData);
    break;
  }
}

void StFmsTriggerMaker::runFpdLayer2(int t){
  switch (mDBTime.GetYear()) {
  case 2011: l1_fp201_2011_a(fp201,t); break;
  case 2012: l1_fp201_2012_b(fp201,t); break;
  case 2013: l1_fp201_2012_b(fp201,t); break;
  case 2015: 
    if(mForceRun<16056024)  {l1_fp201_2015_a(fp201,t,mUseDsmData);} 
    else                    {l1_fp201_2015_b(fp201,t,mUseDsmData);}
    break;
  case 2017: 
  default: l1_fp201_2017_a(fp201,t,mUseDsmData);
    break;
  }
}

void StFmsTriggerMaker::writeFpeQtLayerToFpeLayer1(Crate& sim, int t){
  Board& fe101sim = sim.boardAt(FE101_BASE_ADDRESS);
  int* fe101simchannels = (int*)fe101sim.channels[t];
  fe101simchannels[0] = fe001.output[t];
  fe101simchannels[1] = fe002.output[t];
  fe101simchannels[2] = fe003.output[t];
  fe101simchannels[3] = fe004.output[t];
}

void StFmsTriggerMaker::writeFmsQtLayerToFmsLayer0(Crate& sim, int t){
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
    ((int*)fm001sim.channels[t])[ch] = qt1.boards[3-ch].output[t];
    ((int*)fm002sim.channels[t])[ch] = qt2.boards[3-ch].output[t];
    ((int*)fm003sim.channels[t])[ch] = qt3.boards[3-ch].output[t];
    ((int*)fm004sim.channels[t])[ch] = qt4.boards[3-ch].output[t];
    if(mDBTime.GetYear()<2015){
      ((int*)fm005sim.channels[t])[ch] = qt1.boards[7-ch].output[t];
      ((int*)fm007sim.channels[t])[ch] = qt2.boards[7-ch].output[t];
      ((int*)fm009sim.channels[t])[ch] = qt3.boards[7-ch].output[t];
      ((int*)fm011sim.channels[t])[ch] = qt4.boards[7-ch].output[t];
    }else{
      ((int*)fm005sim.channels[t])[ch] = qt1.boards[9-ch].output[t];
      ((int*)fm007sim.channels[t])[ch] = qt2.boards[9-ch].output[t];
      ((int*)fm009sim.channels[t])[ch] = qt3.boards[9-ch].output[t];
      ((int*)fm011sim.channels[t])[ch] = qt4.boards[9-ch].output[t];
    }
  }

  for (int ch = 0; ch < 2; ++ch) {
    if(mDBTime.GetYear()<2015){
      ((int*)fm006sim.channels[t])[ch] = qt1.boards[9-ch].output[t];
      ((int*)fm008sim.channels[t])[ch] = qt2.boards[9-ch].output[t];
      ((int*)fm010sim.channels[t])[ch] = qt3.boards[9-ch].output[t];
      ((int*)fm012sim.channels[t])[ch] = qt4.boards[9-ch].output[t];
    }else{
      ((int*)fm006sim.channels[t])[ch] = qt1.boards[5-ch].output[t];
      ((int*)fm008sim.channels[t])[ch] = qt2.boards[5-ch].output[t];
      ((int*)fm010sim.channels[t])[ch] = qt3.boards[5-ch].output[t];
      ((int*)fm012sim.channels[t])[ch] = qt4.boards[5-ch].output[t];
    }
  }

  //ZERO-ing out QT3A->FM003 lower 16 bits FAKE - akio
  //((int*)fm003sim.channels[t])[3] = 0;
  //wrong map!!!
  /*
  ((int*)fm005sim.channels[t])[1] = qt1.boards[4].output[t];
  ((int*)fm005sim.channels[t])[0] = qt1.boards[5].output[t];
  ((int*)fm006sim.channels[t])[1] = qt1.boards[6].output[t];
  ((int*)fm006sim.channels[t])[0] = qt1.boards[7].output[t];
  ((int*)fm005sim.channels[t])[3] = qt1.boards[8].output[t];
  ((int*)fm005sim.channels[t])[2] = qt1.boards[9].output[t];
  ((int*)fm007sim.channels[t])[1] = qt2.boards[4].output[t];
  ((int*)fm007sim.channels[t])[0] = qt2.boards[5].output[t];
  ((int*)fm008sim.channels[t])[1] = qt2.boards[6].output[t];
  ((int*)fm008sim.channels[t])[0] = qt2.boards[7].output[t];
  ((int*)fm007sim.channels[t])[3] = qt2.boards[8].output[t];
  ((int*)fm007sim.channels[t])[2] = qt2.boards[9].output[t];
  ((int*)fm009sim.channels[t])[1] = qt3.boards[4].output[t];
  ((int*)fm009sim.channels[t])[0] = qt3.boards[5].output[t];
  ((int*)fm010sim.channels[t])[1] = qt3.boards[6].output[t];
  ((int*)fm010sim.channels[t])[0] = qt3.boards[7].output[t];
  ((int*)fm009sim.channels[t])[3] = qt3.boards[9].output[t];
  ((int*)fm009sim.channels[t])[2] = qt3.boards[10].output[t];
  ((int*)fm011sim.channels[t])[1] = qt4.boards[4].output[t];
  ((int*)fm011sim.channels[t])[0] = qt4.boards[5].output[t];
  ((int*)fm012sim.channels[t])[1] = qt4.boards[6].output[t];
  ((int*)fm012sim.channels[t])[0] = qt4.boards[7].output[t];
  ((int*)fm011sim.channels[t])[3] = qt4.boards[9].output[t];
  ((int*)fm011sim.channels[t])[2] = qt4.boards[10].output[t];
  */

  for (int ch = 2; ch < 4; ++ch) {
    ((int*)fm006sim.channels[t])[ch] = 0xffffffff;
    ((int*)fm008sim.channels[t])[ch] = 0xffffffff;
    ((int*)fm010sim.channels[t])[ch] = 0xffffffff;
    ((int*)fm012sim.channels[t])[ch] = 0xffffffff;
  }
}

void StFmsTriggerMaker::writeFmsLayer0ToFmsLayer1(Crate& sim, int t)
{
  Board& fm101sim = sim.boardAt(FM101_BASE_ADDRESS);
  Board& fm102sim = sim.boardAt(FM102_BASE_ADDRESS);
  Board& fm103sim = sim.boardAt(FM103_BASE_ADDRESS);
  Board& fm104sim = sim.boardAt(FM104_BASE_ADDRESS);

  if(mDBTime.GetYear()<2015){
    for (int ch = 0; ch < 4; ++ch) {
    ((int*)fm101sim.channels[t])[ch] = fms.boards[ch   ].output[t];
    ((int*)fm102sim.channels[t])[ch] = fms.boards[ch+5 ].output[t];
    ((int*)fm103sim.channels[t])[ch] = fms.boards[ch+10].output[t];
    }
  }else{
    ((int*)fm101sim.channels[t])[0] = fms.boards[17].output[t];
    ((int*)fm101sim.channels[t])[1] = fms.boards[0].output[t];
    ((int*)fm102sim.channels[t])[0] = fms.boards[2].output[t];
    ((int*)fm102sim.channels[t])[1] = fms.boards[3].output[t];
    ((int*)fm103sim.channels[t])[0] = fms.boards[5].output[t];
    ((int*)fm103sim.channels[t])[1] = fms.boards[6].output[t];
    ((int*)fm103sim.channels[t])[2] = fms.boards[7].output[t];
    ((int*)fm103sim.channels[t])[3] = fms.boards[8].output[t];
    ((int*)fm104sim.channels[t])[0] = fms.boards[10].output[t];
    ((int*)fm104sim.channels[t])[1] = fms.boards[11].output[t];
    ((int*)fm104sim.channels[t])[2] = fms.boards[12].output[t];
    ((int*)fm104sim.channels[t])[3] = fms.boards[13].output[t];   
    /*
    for (int ch = 0; ch < 4; ++ch) {
      if(ch<2){
	((int*)fm101sim.channels[t])[ch] = fms.boards[ch  ].output[t];
	((int*)fm102sim.channels[t])[ch] = fms.boards[ch+2].output[t];
      }
      ((int*)fm103sim.channels[t])[ch] = fms.boards[ch+5].output[t];
      ((int*)fm104sim.channels[t])[ch] = fms.boards[ch+10].output[t];
    }
    */
  }
}

void StFmsTriggerMaker::writeFpeLayer1ToFpdLayer2(Crate& sim, int t)
{
  Board& fp201sim = sim.boardAt(FP201_BASE_ADDRESS);
  fp201sim.channels[t][6] = 0xffff; // unused
  fp201sim.channels[t][7] = fe101.output[t];
}

void StFmsTriggerMaker::writeFmsLayer1ToFpdLayer2(Crate& sim, int t)
{
  Board& fp201sim = sim.boardAt(FP201_BASE_ADDRESS);
  int* fp201simchannels= (int*)fp201sim.channels[t];

  if(mDBTime.GetYear()<2015){
    fp201simchannels[0] = fm101.output[t];
    fp201simchannels[1] = fm102.output[t];
    fp201simchannels[2] = fm103.output[t];
  }else{
    fp201simchannels[0] = fm102.output[t];
    fp201simchannels[1] = fm103.output[t];
    fp201simchannels[2] = fm104.output[t];
    fp201simchannels[3] = fm101.output[t];    
  }
}

int StFmsTriggerMaker::loadRegisters(int runNumber)
{
  MYSQL mysql;
  char* host1 = "db04.star.bnl.gov"; //offline DB which has online DB copies from past year (a hack! don't tell Dmitry!)
  char* host2 = "onldb2.starp.bnl.gov";  //this is online DB for current run, cannot access from ofl/rcas
  const char* user = "";
  const char* pass = "";
  // See http://drupal.star.bnl.gov/STAR/comp/db/onlinedb/online-sever-port-map
  char* host = host1;
  int year;
  if(mForceRun==0){ 
    mDBTime=GetDBTime(); 
    year=mDBTime.GetYear(); 
  }else{
    int date=(mForceRun%1000000)/1000;   
    year=mForceRun/1000000+1999;
    if(date>250) year++;
    LOG_INFO << Form("DB Year=%d forceYear=%d\n",mDBTime.GetYear(),year) <<endm;
  }
  unsigned int port = 3400+year%100-1; 
  if(year==2018) {port=3501; host=host2;}  //only works at online machines
  if(year>2018) {printf("NO RUN19 DB yet.... Skip for now...\n"); return kStOK; }
  
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
  if(mForceRun==0) {
    sprintf(query,"select max(idx_rn) from triggers where beginTime <= '%s'",mDBTime.AsSQLString());
    LOG_INFO << query << endm;
    mysql_query(&mysql,query);
    if (MYSQL_RES* result = mysql_store_result(&mysql)) {
      while (MYSQL_ROW row = mysql_fetch_row(result)) {
	runNumber = atoi(row[0]);
      }
      mysql_free_result(result);
    }
  }else{ 
    //if mForceRun is set, get date/time for the run, and overwrite run# and DBdate/time.
    sprintf(query,"select beginTime from run where idx_rn=%d",mForceRun);
    LOG_INFO << query << endm;
    mysql_query(&mysql,query);
    TString newdt;
    if (MYSQL_RES* result = mysql_store_result(&mysql)) {
      while (MYSQL_ROW row = mysql_fetch_row(result)) {
	newdt=row[0];
      }
      mysql_free_result(result);
    }
    LOG_INFO << "StFmsTriggerMaker Ignoring run# from DB timestamp and Forcing run#="<<mForceRun<<endm;
    //LOG_INFO << " from "<<mDBTime.GetDate()<< " " <<mDBTime.GetTime();
    mDBTime.Set(newdt.Data());
    LOG_INFO << "StFmsTriggerMaker DB timestamp is taken from forced run begin time = "<<mDBTime.GetDate()<< " " <<mDBTime.GetTime()<< endm;
    runNumber = mForceRun;
  }

  // Get register values
  // object=DSM crate, idx=DSM board
  sprintf(query,"select object,idx,reg,label,value,defaultvalue from dict where hash=(select dicthash from run where idx_rn = %d)",runNumber);
  LOG_DEBUG << query << endm;
  mysql_query(&mysql,query);
  if (MYSQL_RES* result = mysql_store_result(&mysql)) {
    LOG_DEBUG<< setw(10) << "object" << setw(10) << "idx" << setw(10) << "reg" << setw(30) << "label" << setw(10) << "value" << setw(15) << "defaultvalue" << endm;
    while (MYSQL_ROW row = mysql_fetch_row(result)) {
      int object = atoi(row[0]);
      int idx = atoi(row[1]);
      int reg = atoi(row[2]);
      TString label = row[3];
      int value = atoi(row[4]);
      int defaultvalue = atoi(row[5]);
      if(mNThrOW>0){
	for(int i=0; i<mNThrOW; i++){
	  if(label.Contains(mThrOWName[i])) {
	    LOG_INFO << "Overwriting Thr="<<mThrOWName[i].Data()<<" from " <<value<< " to "<<mThrOWValue[i]<<endm;
	    value=mThrOWValue[i];
	  }
	}
      }
      LOG_DEBUG << setw(10) << object << setw(10) << idx << setw(10) << reg << setw(30) << label << setw(10) << value << setw(15) << defaultvalue << endm;
      if (object >= 1 && object <= NCRATES && idx >= 0x10) {
	LOG_DEBUG << object << '\t' << idx << '\t' << reg << '\t' << label << '\t' << value << '\t' << defaultvalue << endm;
	crateAt(object).boardAt(idx<<24).registerAt(reg<<24) = (value == -1) ? defaultvalue : value;
      }
    }
    mysql_free_result(result);
  }

  // Close connection to online database
  mysql_close(&mysql);

  return kStOk;
}

void StFmsTriggerMaker::overwriteThr(char* name, int value){
  mThrOWName[mNThrOW]=name;
  mThrOWValue[mNThrOW]=value;
  LOG_INFO << "Set overwriting Thr="<<mThrOWName[mNThrOW].Data()<<" to "<<mThrOWValue[mNThrOW]<<endm;
  mNThrOW++;
}

int StFmsTriggerMaker::FM0xxoutput(int number, int t) const{
  switch(number){
  case 1:  return fm001.output[t];
  case 2:  return fm002.output[t];
  case 3:  return fm003.output[t];
  case 4:  return fm004.output[t];
  case 5:  return fm005.output[t];
  case 6:  return fm006.output[t];
  case 7:  return fm007.output[t];
  case 8:  return fm008.output[t];
  case 9:  return fm009.output[t];
  case 10: return fm010.output[t];
  case 11: return fm011.output[t];
  case 12: return fm012.output[t];
  }
  return 0;
}

int StFmsTriggerMaker::FM1xxoutput(int number, int t) const{
  switch(number){
  case 1:  return fm101.output[t];
  case 2:  return fm102.output[t];
  case 3:  return fm103.output[t];
  case 4:  return fm104.output[t];
  }
  return 0;
}

int StFmsTriggerMaker::FP201input(int ch, int t) const{
  return ((int*)fp201.channels[t])[ch];
}

int StFmsTriggerMaker::FM0xxinput(int number, int ch, int t) const{
  switch(number){
  case 1:  return ((int*)fm001.channels[t])[ch];
  case 2:  return ((int*)fm002.channels[t])[ch];
  case 3:  return ((int*)fm003.channels[t])[ch];
  case 4:  return ((int*)fm004.channels[t])[ch];
  case 5:  return ((int*)fm005.channels[t])[ch];
  case 6:  return ((int*)fm006.channels[t])[ch];
  case 7:  return ((int*)fm007.channels[t])[ch];
  case 8:  return ((int*)fm008.channels[t])[ch];
  case 9:  return ((int*)fm009.channels[t])[ch];
  case 10: return ((int*)fm010.channels[t])[ch];
  case 11: return ((int*)fm011.channels[t])[ch];
  case 12: return ((int*)fm012.channels[t])[ch];
  }
  return 0;
}

int StFmsTriggerMaker::FM1xxinput(int number, int ch, int t) const{
  switch(number){
  case 1:  return ((int*)fm101.channels[t])[ch];
  case 2:  return ((int*)fm102.channels[t])[ch];
  case 3:  return ((int*)fm103.channels[t])[ch];
  case 4:  return ((int*)fm104.channels[t])[ch];
  }
  return 0;
}

int StFmsTriggerMaker::FP201data(int ch) const{
  return ((int*)fp201.dsmdata[MAXPP])[ch];
}

int StFmsTriggerMaker::FM0xxdata(int number, int ch, int t) const{
  switch(number){
  case 1:  return ((int*)fm001.dsmdata[t])[ch];
  case 2:  return ((int*)fm002.dsmdata[t])[ch];
  case 3:  return ((int*)fm003.dsmdata[t])[ch];
  case 4:  return ((int*)fm004.dsmdata[t])[ch];
  case 5:  return ((int*)fm005.dsmdata[t])[ch];
  case 6:  return ((int*)fm006.dsmdata[t])[ch];
  case 7:  return ((int*)fm007.dsmdata[t])[ch];
  case 8:  return ((int*)fm008.dsmdata[t])[ch];
  case 9:  return ((int*)fm009.dsmdata[t])[ch];
  case 10: return ((int*)fm010.dsmdata[t])[ch];
  case 11: return ((int*)fm011.dsmdata[t])[ch];
  case 12: return ((int*)fm012.dsmdata[t])[ch];
  }
  return 0;
}

int StFmsTriggerMaker::FM1xxdata(int number, int ch, int t) const{
  switch(number){
  case 1:  return ((int*)fm101.dsmdata[t])[ch];
  case 2:  return ((int*)fm102.dsmdata[t])[ch];
  case 3:  return ((int*)fm103.dsmdata[t])[ch];
  case 4:  return ((int*)fm104.dsmdata[t])[ch];
  }
  return 0;
}

int StFmsTriggerMaker::FM0xxuserdata(int number, int ch, int t) const{
  switch(number){
  case 1:  return ((int*)fm001.userdata[t])[ch];
  case 2:  return ((int*)fm002.userdata[t])[ch];
  case 3:  return ((int*)fm003.userdata[t])[ch];
  case 4:  return ((int*)fm004.userdata[t])[ch];
  case 5:  return ((int*)fm005.userdata[t])[ch];
  case 6:  return ((int*)fm006.userdata[t])[ch];
  case 7:  return ((int*)fm007.userdata[t])[ch];
  case 8:  return ((int*)fm008.userdata[t])[ch];
  case 9:  return ((int*)fm009.userdata[t])[ch];
  case 10: return ((int*)fm010.userdata[t])[ch];
  case 11: return ((int*)fm011.userdata[t])[ch];
  case 12: return ((int*)fm012.userdata[t])[ch];
  }
  return 0;
}

int StFmsTriggerMaker::FM1xxuserdata(int number, int ch, int t) const{
  switch(number){
  case 1:  return ((int*)fm101.userdata[t])[ch];
  case 2:  return ((int*)fm102.userdata[t])[ch];
  case 3:  return ((int*)fm103.userdata[t])[ch];
  case 4:  return ((int*)fm104.userdata[t])[ch];
  }
  return 0;
}

int StFmsTriggerMaker::FP201userdata(int ch, int t) const{
  return fp201.userdata[MAXPP][ch];
}


