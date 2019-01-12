//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#ifndef TRG_UTIL_2009_HH
#define TRG_UTIL_2009_HH

#include <cstdio>
#include <cassert>
#include <byteswap.h>
#include <algorithm>

using namespace std;

#ifdef __ROOT__
#include "RTS/trg/include/trgDataDefs_46.h"
#include "RTS/trg/include/trgConfNum.h"
#else
#include "trgDataDefs_46.h"
#include "trgConfNum.h"
#endif

inline unsigned short bswap16(unsigned short x) { return bswap_16(x); }
inline unsigned int   bswap32(unsigned int   x) { return bswap_32(x); }

inline void swapTrgOfflen(TrgOfflen& trgOfflen)
{
  trgOfflen.offset = bswap32(trgOfflen.offset);
  trgOfflen.length = bswap32(trgOfflen.length);
}

inline void swapEvtDescData(EvtDescData& evtDesc)
{
  evtDesc.length = bswap32(evtDesc.length);
  evtDesc.bunchXing_hi = bswap32(evtDesc.bunchXing_hi);
  evtDesc.bunchXing_lo = bswap32(evtDesc.bunchXing_lo);
  evtDesc.actionWdDetectorBitMask = bswap16(evtDesc.actionWdDetectorBitMask);
  evtDesc.TrgToken = bswap16(evtDesc.TrgToken);
  evtDesc.addBits = bswap16(evtDesc.addBits);
  evtDesc.DSMInput = bswap16(evtDesc.DSMInput);
  evtDesc.externalBusy = bswap16(evtDesc.externalBusy);
  evtDesc.internalBusy = bswap16(evtDesc.internalBusy);
  evtDesc.physicsWord = bswap16(evtDesc.physicsWord);
  evtDesc.TriggerWord = bswap16(evtDesc.TriggerWord);
  evtDesc.DSMAddress = bswap16(evtDesc.DSMAddress);
  evtDesc.TCU_Mark = bswap16(evtDesc.TCU_Mark);
  evtDesc.npre = bswap16(evtDesc.npre);
  evtDesc.npost = bswap16(evtDesc.npost);
}

inline void swapL1_DSM_Data(L1_DSM_Data& L1data)
{
  L1data.length = bswap32(L1data.length);
  transform(L1data.TOF,L1data.TOF+8,L1data.TOF,bswap16);
  transform(L1data.VTX,L1data.VTX+8,L1data.VTX,bswap16);
  transform(L1data.EMC,L1data.EMC+8,L1data.EMC,bswap16);
  transform(L1data.BCdata,L1data.BCdata+16,L1data.BCdata,bswap16);
  transform(L1data.specialTriggers,L1data.specialTriggers+8,L1data.specialTriggers,bswap16);
  transform(L1data.FPD,L1data.FPD+8,L1data.FPD,bswap16);
  transform(L1data.lastDSM,L1data.lastDSM+8,L1data.lastDSM,bswap16);
}

inline void swapTrgSumData(TrgSumData& trgSum)
{
  trgSum.length = bswap32(trgSum.length);
  transform(trgSum.L1Sum,trgSum.L1Sum+2,trgSum.L1Sum,bswap32);
  transform(trgSum.L2Sum,trgSum.L2Sum+2,trgSum.L2Sum,bswap32);
  transform(trgSum.L1Result,trgSum.L1Result+32,trgSum.L1Result,bswap32);
  transform(trgSum.L2Result,trgSum.L2Result+64,trgSum.L2Result,bswap32);
  transform(trgSum.C2Result,trgSum.C2Result+64,trgSum.C2Result,bswap32);
}

inline void swapDataBlock(DataBlock& data)
{
  data.length = bswap32(data.length);
}

inline void swapBELayerBlock(BELayerBlock& bc1)
{
  bc1.length = bswap32(bc1.length);
  transform(bc1.BEMClayer1,bc1.BEMClayer1+48,bc1.BEMClayer1,bswap16);
  transform(bc1.EEMClayer1,bc1.EEMClayer1+16,bc1.EEMClayer1,bswap16);
}

inline void swapMIXBlock(MIXBlock& mix)
{
  mix.length = bswap32(mix.length);
  transform(mix.FPDEastNSLayer1,mix.FPDEastNSLayer1+8,mix.FPDEastNSLayer1,bswap16);
  transform(mix.TOFLayer1,mix.TOFLayer1+8,mix.TOFLayer1,bswap16);
  transform(mix.TOF,mix.TOF+48,mix.TOF,bswap16);
}

inline void swapBWestBlock(BWestBlock& bcw)
{
  bcw.length = bswap32(bcw.length);
}

inline void swapBEastBlock(BEastBlock& bce)
{
  bce.length = bswap32(bce.length);
}

inline void swapBBCBlock(BBCBlock& bbc)
{
  bbc.length = bswap32(bbc.length);
  transform(bbc.BBClayer1,bbc.BBClayer1+16,bbc.BBClayer1,bswap16);
  transform(bbc.ZDClayer1,bbc.ZDClayer1+8,bbc.ZDClayer1,bswap16);
  transform(bbc.VPD,bbc.VPD+8,bbc.VPD,bswap16);
}

inline void swapFMSBlock(FMSBlock& fms)
{
  fms.length = bswap32(fms.length);
}

inline void swapQTBlock(QTBlock& qt)
{
  qt.length = bswap32(qt.length);
  qt.dataLoss = bswap32(qt.dataLoss);
}

inline void swapDataBlockAtCrate(DataBlock& data, int crate)
{
  switch (crate) {
  case BC1_CONF_NUM:
    {
      BELayerBlock* bc1 = (BELayerBlock*)&data;
      assert(strncmp(bc1->name, "BC1", 3) == 0);
      swapBELayerBlock(*bc1);
      break;
    }
  case MIX_CONF_NUM:
    {
      MIXBlock* mix = (MIXBlock*)&data;
      assert(strncmp(mix->name, "MIX", 3) == 0);
      swapMIXBlock(*mix);
      break;
    }
  case BCW_CONF_NUM:
    {
      BWestBlock* bcw = (BWestBlock*)&data;
      assert(strncmp(bcw->name, "BCW", 3) == 0);
      swapBWestBlock(*bcw);
      break;
    }
  case BCE_CONF_NUM:
    {
      BEastBlock* bce = (BEastBlock*)&data;
      assert(strncmp(bce->name, "BCE", 3) == 0);
      swapBEastBlock(*bce);
      break;
    }
  case BBC_CONF_NUM:
    {
      BBCBlock* bbc = (BBCBlock*)&data;
      assert(strncmp(bbc->name, "BBC", 3) == 0);
      swapBBCBlock(*bbc);
      break;
    }
  case FMS_CONF_NUM:
    {
      FMSBlock* fms = (FMSBlock*)&data;
      assert(strncmp(fms->name, "FMS", 3) == 0);
      swapFMSBlock(*fms);
      break;
    }
  case QT1_CONF_NUM:
  case QT2_CONF_NUM:
  case QT3_CONF_NUM:
  case QT4_CONF_NUM:
    {
      QTBlock* qt = (QTBlock*)&data;
      assert(strncmp(qt->name, "QT1", 3) == 0 ||
	     strncmp(qt->name, "QT2", 3) == 0 ||
	     strncmp(qt->name, "QT3", 3) == 0 ||
	     strncmp(qt->name, "QT4", 3) == 0);
      swapQTBlock(*qt);
      break;
    }
  default:
    {
      swapDataBlock(data);
      break;
    }
  }
}

inline void swapTriggerDataBlk(TriggerDataBlk& trgData)
{
  // Swap format version, total trigger length and event number

  trgData.FormatVersion = bswap32(trgData.FormatVersion);
  //assert(trgData.FormatVersion == FORMAT_VERSION);
  trgData.totalTriggerLength = bswap32(trgData.totalTriggerLength);
  trgData.eventNumber = bswap32(trgData.eventNumber);

  // Swap event descriptor

  swapTrgOfflen(trgData.EventDesc_ofl);
  EvtDescData* evtDesc = (EvtDescData*)((int)&trgData+trgData.EventDesc_ofl.offset);
  assert(strncmp(evtDesc->name, "EVD", 3) == 0 || strncmp(evtDesc->name, "EVT", 3) == 0);
  swapEvtDescData(*evtDesc);

  // Swap L1 DSM data

  swapTrgOfflen(trgData.L1_DSM_ofl);
  L1_DSM_Data* L1data = (L1_DSM_Data*)((int)&trgData+trgData.L1_DSM_ofl.offset);
  assert(strncmp(L1data->name, "L1DS", 4) == 0);
  swapL1_DSM_Data(*L1data);

  // Swap trigger summary

  swapTrgOfflen(trgData.Summary_ofl);
  TrgSumData* trgSum = (TrgSumData*)((int)&trgData+trgData.Summary_ofl.offset);
  assert(strncmp(trgSum->name, "TSUM", 4) == 0);
  swapTrgSumData(*trgSum);

  // Swap data in crates for main bXing

  for (int crate = 0; crate < MAX_OFFLEN; ++crate) {
    swapTrgOfflen(trgData.MainX[crate]);
    if (trgData.MainX[crate].offset && trgData.MainX[crate].length) {
      DataBlock* data = (DataBlock*)((int)&trgData+trgData.MainX[crate].offset);
      swapDataBlockAtCrate(*data, crate);
    }
  }

  // Swap data in crates for pre/post bXing

  const int nentries = evtDesc->npre + evtDesc->npost;

  for (int i = 0; i < nentries; ++i) {
    trgData.PrePostList[i] = bswap32(trgData.PrePostList[i]);
    TrgOfflen* PrePostX = (TrgOfflen*)((int)&trgData+trgData.PrePostList[i]);
    for (int crate = 0; crate < MAX_OFFLEN; ++crate) {
      swapTrgOfflen(PrePostX[crate]);
      if (PrePostX[crate].offset && PrePostX[crate].length) {
	DataBlock* data = (DataBlock*)((int)&trgData+PrePostX[crate].offset);
	swapDataBlockAtCrate(*data, crate);
      }
    }
  }
}

inline void printName(const char* name)
{
  printf("name: ");
  for (int i = 0; i < 4; ++i) putchar(name[i]);
  printf("\n");
}

inline void printEvtDescData(const EvtDescData& evtDesc)
{
  printName(evtDesc.name);
  printf("TrgDataFmtVer: 0x%x\n", evtDesc.TrgDataFmtVer);
  printf("length: %d\n", evtDesc.length);
  printf("bunchXing_hi: %d\n", evtDesc.bunchXing_hi);
  printf("bunchXing_lo: %d\n", evtDesc.bunchXing_lo);
  printf("actionWdDetectorBitMask: %d\n", evtDesc.actionWdDetectorBitMask);
  printf("actionWdTrgCommand: %d\n", evtDesc.actionWdTrgCommand);
  printf("actionWdDaqCommand: %d\n", evtDesc.actionWdDaqCommand);
  printf("TrgToken: %d\n", evtDesc.TrgToken);
  printf("addBits: %d\n", evtDesc.addBits);
  printf("DSMInput: %d\n", evtDesc.DSMInput);
  printf("externalBusy: %d\n", evtDesc.externalBusy);
  printf("internalBusy: %d\n", evtDesc.internalBusy);
  printf("physicsWord: %d\n", evtDesc.physicsWord);
  printf("TriggerWord: %d\n", evtDesc.TriggerWord);
  printf("DSMAddress: %d\n", evtDesc.DSMAddress);
  printf("TCU_Mark: %d\n", evtDesc.TCU_Mark);
  printf("npre: %d\n", evtDesc.npre);
  printf("npost: %d\n", evtDesc.npost);
}

inline void printL1_DSM_Data(const L1_DSM_Data& L1data)
{
  printName(L1data.name);
  printf("length: %d\n", L1data.length);
  printf("TOF: ");
  for (int i = 0; i < 8; ++i) printf("%04x ", L1data.TOF[i]);
  printf("\n");
  printf("VTX: ");
  for (int i = 0; i < 8; ++i) printf("%04x ", L1data.VTX[i]);
  printf("\n");
  printf("EMC: ");
  for (int i = 0; i < 8; ++i) printf("%04x ", L1data.EMC[i]);
  printf("\n");
  printf("BCdata:\n");
  for (int i = 0; i < 16; ++i) {
    printf("%04x ", L1data.BCdata[i]);
    if (i % 8 == 7) printf("\n");
  }
  printf("specialTriggers: ");
  for (int i = 0; i < 8; ++i) printf("%04x ", L1data.specialTriggers[i]);
  printf("\n");
  printf("FPD: ");
  for (int i = 0; i < 8; ++i) printf("%04x ", L1data.FPD[i]);
  printf("\n");
  printf("lastDSM: ");
  for (int i = 0; i < 8; ++i) printf("%04x ", L1data.lastDSM[i]);
  printf("\n");
}

inline void printTrgSumData(const TrgSumData& trgSum)
{
  printName(trgSum.name);
  printf("length: %d\n", trgSum.length);
  printf("L1Sum: ");
  for (int i = 0; i < 2; ++i) printf("%08x ", trgSum.L1Sum[i]);
  printf("\n");
  printf("L2Sum: ");
  for (int i = 0; i < 2; ++i) printf("%08x ", trgSum.L2Sum[i]);
  printf("\n");
  printf("L1Result:\n");
  for (int i = 0; i < 32; ++i) {
    printf("%08x ", trgSum.L1Result[i]);
    if (i % 8 == 7) printf("\n");
  }
  printf("L2Result:\n");
  for (int i = 0; i < 64; ++i) {
    printf("%08x ", trgSum.L2Result[i]);
    if (i %8 == 7) printf("\n");
  }
  printf("C2Result:\n");
  for (int i = 0; i < 64; ++i) {
    printf("%08x ", trgSum.C2Result[i]);
    if (i % 8 == 7) printf("\n");
  }
}

inline void printMIXBlock(const MIXBlock& mix)
{
  printName(mix.name);
  printf("length: %d\n", mix.length);
  printf("FPDEastNSLayer1: ");
  for (int i = 0; i < 8; ++i) printf("%04x ", mix.FPDEastNSLayer1[i]);
  printf("\n");
  printf("MTD_P2PLayer1:\n");
  for (int i = 0; i < 16; ++i) {
    printf("%x ", mix.MTD_P2PLayer1[i]);
    if (i % 8 == 7) printf("\n");
  }
  printf("TOFLayer1: ");
  for (int i = 0; i < 8; ++i) printf("%04x ", mix.TOFLayer1[i]);
  printf("\n");
  printf("TOF:\n");
  for (int i = 0; i < 48; ++i) {
    printf("%04x ", mix.TOF[i]);
    if (i % 8 == 7) printf("\n");
  }
}

inline void printBBCBlock(const BBCBlock& bbc)
{
  printName(bbc.name);
  printf("length: %d\n", bbc.length);
  printf("BBClayer1:\n");
  for (int i = 0; i < 16; ++i) {
    printf("%04x ", bbc.BBClayer1[i]);
    if (i % 8 == 7) printf("\n");
  }
  printf("ZDClayer1:\n");
  for (int i = 0; i < 8; ++i) printf("%04x ", bbc.ZDClayer1[i]);
  printf("\n");
  printf("VPD:\n");
  for (int i = 0; i < 8; ++i) printf("%04x ", bbc.VPD[i]);
  printf("\n");
}

inline void printFMSBlock(const FMSBlock& fms)
{
  printName(fms.name);
  printf("length: %d\n", fms.length);
  for (int i = 0; i < 256; ++i) {
    printf("%02x ", fms.FMS[i]);
    if (i % 16 == 15) printf("\n");
  }
}

inline void printQTBlock(const QTBlock& qt)
{
  printName(qt.name);
  printf("length: %d\n", qt.length);
  printf("dataLoss: %d\n", qt.dataLoss);
  int len = qt.length-4;
  unsigned char* data = (unsigned char*)&qt.data;
  for (int i = 0; i < len; ++i) {
    printf("%02x ", data[i]);
    if (i % 16 == 15) printf("\n");
  }
}

inline void printBELayerBlock(const BELayerBlock& bc1)
{
  printName(bc1.name);
  printf("length: %d\n", bc1.length);
  printf("BEMClayer1:\n");
  for (int i = 0; i < 48; ++i) {
    printf("%04x ", bc1.BEMClayer1[i]);
    if (i % 8 == 7) printf("\n");
  }
  printf("EEMClayer1:\n");
  for (int i = 0; i < 16; ++i) {
    printf("%04x ", bc1.EEMClayer1[i]);
    if (i % 8 == 7) printf("\n");
  }
  printf("EEMC:\n");
  for (int i = 0; i < 144; ++i) {
    printf("%02x ", bc1.EEMC[i]);
    if (i % 16 == 15) printf("\n");
  }
}

inline void printBWestBlock(const BWestBlock& bcw)
{
  printName(bcw.name);
  printf("length: %d\n", bcw.length);
  printf("BEMCWest:\n");
  for (int i = 0; i < 240; ++i) {
    printf("%02x ", bcw.BEMCWest[i]);
    if (i % 16 == 15) printf("\n");
  }
}

inline void printBEastBlock(const BEastBlock& bce)
{
  printName(bce.name);
  printf("length: %d\n", bce.length);
  printf("BEMCEast:\n");
  for (int i = 0; i < 240; ++i) {
    printf("%02x ", bce.BEMCEast[i]);
    if (i % 16 == 15) printf("\n");
  }
}

inline void printDataBlock(const DataBlock& dataBlock)
{
  printName(dataBlock.name);
  printf("length: %d\n", dataBlock.length);
}

#endif	// TRG_UTIL_2009_HH
