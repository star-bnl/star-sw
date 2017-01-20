//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M
// 16 July 2012
//

#ifndef CRATE_HH
#define CRATE_HH

#include <stdio.h>
#include "Board.hh"

#if defined(__linux__) || defined(__APPLE__)
#if defined(__linux__) 
/* linux has its own (fast) swaps */
#include <byteswap.h>
#else /* __APPLE__ */
//#warning "byteswap.h is an unportable GNU extension!  Don't use!"
static inline unsigned short bswap_16(unsigned short x) {
  return ((((x) >> 8) & 0xff) | (((x) & 0xff) << 8)) ;
}
static inline unsigned int bswap_32(unsigned int x) {
  return (bswap_16(x&0xffff)<<16) | (bswap_16(x>>16));
}
static inline unsigned long long bswap_64(unsigned long long x) {
  return (((unsigned long long)bswap_32(x&0xffffffffull))<<32) |
(bswap_32(x>>32));
}
#endif /* __APPLE__ */
#endif /* __linux__ || __APPLE__ */
#include <cassert>
#include <cstring>
#include <algorithm>
#include <functional>
#include "RTS/trg/include/trgConfNum.h"
#include "RTS/trg/include/trgDataDefs.h"
#include "Board.hh"

#define  FEQ_CONF_NUM       7

using namespace std;

static const char* crateNames[] = { 0, "L1", "BC1", "MXQ", "MIX", "BCW", "BCE", "FEQ", "BBC", "BBQ", "FMS", "QT1", "QT2", "QT3", "QT4" };

// L1 crate
const int TF201_BASE_ADDRESS = 0x10000000;
const int VT201_BASE_ADDRESS = 0x12000000;
const int EM201_BASE_ADDRESS = 0x14000000;
const int EM202_BASE_ADDRESS = 0x15000000;
const int BX201_BASE_ADDRESS = 0x16000000;
const int BX202_BASE_ADDRESS = 0x18000000;
const int ST201_BASE_ADDRESS = 0x1a000000;
const int FP201_BASE_ADDRESS = 0x1c000000;
const int LD301_BASE_ADDRESS = 0x1e000000;

// BC1 crate
const int BC101_BASE_ADDRESS = 0x21000000;
const int BC102_BASE_ADDRESS = 0x10000000;
const int BC103_BASE_ADDRESS = 0x11000000;
const int BC104_BASE_ADDRESS = 0x12000000;
const int BC105_BASE_ADDRESS = 0x13000000;
const int BC106_BASE_ADDRESS = 0x14000000;
const int EE101_BASE_ADDRESS = 0x15000000;
const int EE102_BASE_ADDRESS = 0x16000000;
const int EE001_BASE_ADDRESS = 0x17000000;
const int EE002_BASE_ADDRESS = 0x18000000;
const int EE003_BASE_ADDRESS = 0x19000000;
const int EE004_BASE_ADDRESS = 0x1a000000;
const int EE005_BASE_ADDRESS = 0x1b000000;
const int EE006_BASE_ADDRESS = 0x1c000000;
const int EE007_BASE_ADDRESS = 0x1d000000;
const int EE008_BASE_ADDRESS = 0x1e000000;
const int EE009_BASE_ADDRESS = 0x1f000000;

// MIX crate
const int FE101_BASE_ADDRESS = 0x10000000;
const int MT101_BASE_ADDRESS = 0x11000000;
const int TF101_BASE_ADDRESS = 0x12000000;
const int TF001_BASE_ADDRESS = 0x14000000;
const int TF002_BASE_ADDRESS = 0x16000000;
const int TF003_BASE_ADDRESS = 0x18000000;
const int TF004_BASE_ADDRESS = 0x1a000000;
const int TF005_BASE_ADDRESS = 0x1c000000;
const int TF006_BASE_ADDRESS = 0x1e000000;

// BCW crate
const int BW001_BASE_ADDRESS = 0x10000000;
const int BW002_BASE_ADDRESS = 0x11000000;
const int BW003_BASE_ADDRESS = 0x12000000;
const int BW004_BASE_ADDRESS = 0x13000000;
const int BW005_BASE_ADDRESS = 0x14000000;
const int BW006_BASE_ADDRESS = 0x15000000;
const int BW007_BASE_ADDRESS = 0x16000000;
const int BW008_BASE_ADDRESS = 0x17000000;
const int BW009_BASE_ADDRESS = 0x18000000;
const int BW010_BASE_ADDRESS = 0x19000000;
const int BW011_BASE_ADDRESS = 0x1a000000;
const int BW012_BASE_ADDRESS = 0x1b000000;
const int BW013_BASE_ADDRESS = 0x1c000000;
const int BW014_BASE_ADDRESS = 0x1d000000;
const int BW015_BASE_ADDRESS = 0x1e000000;

// BCE crate
const int BE001_BASE_ADDRESS = 0x10000000;
const int BE002_BASE_ADDRESS = 0x11000000;
const int BE003_BASE_ADDRESS = 0x12000000;
const int BE004_BASE_ADDRESS = 0x13000000;
const int BE005_BASE_ADDRESS = 0x14000000;
const int BE006_BASE_ADDRESS = 0x15000000;
const int BE007_BASE_ADDRESS = 0x16000000;
const int BE008_BASE_ADDRESS = 0x17000000;
const int BE009_BASE_ADDRESS = 0x18000000;
const int BE010_BASE_ADDRESS = 0x19000000;
const int BE011_BASE_ADDRESS = 0x1a000000;
const int BE012_BASE_ADDRESS = 0x1b000000;
const int BE013_BASE_ADDRESS = 0x1c000000;
const int BE014_BASE_ADDRESS = 0x1d000000;
const int BE015_BASE_ADDRESS = 0x1e000000;

// FEQ crate

const int FE001_BASE_ADDRESS = 0x10000000;
const int FE002_BASE_ADDRESS = 0x11000000;
const int FE003_BASE_ADDRESS = 0x12000000;
const int FE004_BASE_ADDRESS = 0x13000000;
const int FS001_BASE_ADDRESS = 0x19000000;
const int FS002_BASE_ADDRESS = 0x1a000000;
const int FS003_BASE_ADDRESS = 0x1b000000;
const int FS004_BASE_ADDRESS = 0x1c000000;
const int FS005_BASE_ADDRESS = 0x1d000000;
const int FS006_BASE_ADDRESS = 0x1e000000;

// BBC crate
const int BB101_BASE_ADDRESS = 0x10000000;
const int BB102_BASE_ADDRESS = 0x12000000;
const int ZD101_BASE_ADDRESS = 0x14000000;
const int VP101_BASE_ADDRESS = 0x16000000;

// FMS crate
#define RUN15
#ifndef RUN15
const int FM001_BASE_ADDRESS = 0x10000000;
const int FM002_BASE_ADDRESS = 0x11000000;
const int FM003_BASE_ADDRESS = 0x12000000;
const int FM004_BASE_ADDRESS = 0x13000000;
const int FM101_BASE_ADDRESS = 0x14000000;
const int FM005_BASE_ADDRESS = 0x15000000;
const int FM006_BASE_ADDRESS = 0x16000000;
const int FM007_BASE_ADDRESS = 0x17000000;
const int FM008_BASE_ADDRESS = 0x18000000;
const int FM102_BASE_ADDRESS = 0x19000000;
const int FM009_BASE_ADDRESS = 0x1a000000;
const int FM010_BASE_ADDRESS = 0x1b000000;
const int FM011_BASE_ADDRESS = 0x1c000000;
const int FM012_BASE_ADDRESS = 0x1d000000;
const int FM103_BASE_ADDRESS = 0x1e000000;
#endif
#ifdef RUN15
const int FM001_BASE_ADDRESS = 0x21000000;
const int FM002_BASE_ADDRESS = 0x10000000;
const int FM101_BASE_ADDRESS = 0x11000000;
const int FM003_BASE_ADDRESS = 0x12000000;
const int FM004_BASE_ADDRESS = 0x13000000;
const int FM102_BASE_ADDRESS = 0x14000000;
const int FM005_BASE_ADDRESS = 0x15000000;
const int FM006_BASE_ADDRESS = 0x16000000;
const int FM007_BASE_ADDRESS = 0x17000000;
const int FM008_BASE_ADDRESS = 0x18000000;
const int FM103_BASE_ADDRESS = 0x19000000;
const int FM009_BASE_ADDRESS = 0x1a000000;
const int FM010_BASE_ADDRESS = 0x1b000000;
const int FM011_BASE_ADDRESS = 0x1c000000;
const int FM012_BASE_ADDRESS = 0x1d000000;
const int FM104_BASE_ADDRESS = 0x1e000000;
#endif

// BBQ crate
const int BB001_BASE_ADDRESS = 0x10000000;
const int BB002_BASE_ADDRESS = 0x12000000;
const int BB003_BASE_ADDRESS = 0x14000000;
const int VP001_BASE_ADDRESS = 0x16000000;
const int VP002_BASE_ADDRESS = 0x18000000;
const int ZD001_BASE_ADDRESS = 0x1e000000;

// MXQ crate
const int MT001_BASE_ADDRESS = 0x10000000;
const int PP001_BASE_ADDRESS = 0x12000000;
const int ZP001_BASE_ADDRESS = 0x14000000;
const int VP003_BASE_ADDRESS = 0x16000000;
const int VP004_BASE_ADDRESS = 0x18000000;
const int MT002_BASE_ADDRESS = 0x1a000000;

struct Crate {
  enum { NBOARDS = 18 };

  Board boards[NBOARDS];

  //const char* name() const { return crateNames[id]; }
  const Board& boardAt(int i) const { return boards[(i>>24)-0x10]; }
  Board& boardAt(int i) { return boards[(i>>24)-0x10]; }
  void clear() { for_each(boards,boards+NBOARDS,mem_fun_ref(&Board::clear)); }
  void read(const TriggerDataBlk& event, int id);
  void decodeQT(const QTBlock& qt, int crate, int t=MAXPP);
  unsigned long long swapLL(unsigned long long x);
  void copy_and_swap(unsigned char* dest, const unsigned char* src);
  void copy_and_swap(unsigned short* dest, const unsigned short* src);
  void unpack(unsigned short* dest, const unsigned char* src);
};

inline void Crate::read(const TriggerDataBlk& event, int id){
  printf("CRATE::READ CALLED!!!\n");
/*
  clear();
  switch (id) {
    // DSM crates
  case L1_CONF_NUM:
    if (L1_DSM_Data* L1data = (L1_DSM_Data*)((char*)&event+event.L1_DSM_ofl.offset)) {
      assert(strncmp(L1data->name,"L1DS",4) == 0);
      copy_and_swap(boardAt(TF201_BASE_ADDRESS).channels,L1data->TOF);
      copy_and_swap(boardAt(VT201_BASE_ADDRESS).channels,L1data->VTX);
      copy_and_swap(boardAt(EM201_BASE_ADDRESS).channels,L1data->EMC);
      copy_and_swap(boardAt(EM202_BASE_ADDRESS).channels,L1data->TPCMask);
      copy_and_swap(boardAt(BX201_BASE_ADDRESS).channels,L1data->BCdata);
      copy_and_swap(boardAt(BX202_BASE_ADDRESS).channels,L1data->BCdata+8);
      copy_and_swap(boardAt(ST201_BASE_ADDRESS).channels,L1data->specialTriggers);
      copy_and_swap(boardAt(FP201_BASE_ADDRESS).channels,L1data->FPD);
      copy(L1data->lastDSM,L1data->lastDSM+8,boardAt(LD301_BASE_ADDRESS).channels); // new TCU
    }
    break;
  case BC1_CONF_NUM:
    if (event.MainX[BC1_CONF_NUM].offset && event.MainX[BC1_CONF_NUM].length) {
      BELayerBlock* bc1 = (BELayerBlock*)((char*)&event+event.MainX[BC1_CONF_NUM].offset);
      assert(strncmp(bc1->name,"BC1",3) == 0);
      copy_and_swap(boardAt(BC101_BASE_ADDRESS).channels,bc1->BEMClayer1);
      copy_and_swap(boardAt(BC102_BASE_ADDRESS).channels,bc1->BEMClayer1+8);
      copy_and_swap(boardAt(BC103_BASE_ADDRESS).channels,bc1->BEMClayer1+16);
      copy_and_swap(boardAt(BC104_BASE_ADDRESS).channels,bc1->BEMClayer1+24);
      copy_and_swap(boardAt(BC105_BASE_ADDRESS).channels,bc1->BEMClayer1+32);
      copy_and_swap(boardAt(BC106_BASE_ADDRESS).channels,bc1->BEMClayer1+40);
      copy_and_swap(boardAt(EE101_BASE_ADDRESS).channels,bc1->EEMClayer1);
      copy_and_swap(boardAt(EE102_BASE_ADDRESS).channels,bc1->EEMClayer1+8);
      unsigned char buffer[16];
      copy_and_swap(buffer,bc1->EEMC); unpack(boardAt(EE001_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bc1->EEMC+16); unpack(boardAt(EE002_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bc1->EEMC+32); unpack(boardAt(EE003_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bc1->EEMC+48); unpack(boardAt(EE004_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bc1->EEMC+64); unpack(boardAt(EE005_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bc1->EEMC+80); unpack(boardAt(EE006_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bc1->EEMC+96); unpack(boardAt(EE007_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bc1->EEMC+112); unpack(boardAt(EE008_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bc1->EEMC+128); unpack(boardAt(EE009_BASE_ADDRESS).channels,buffer);
    }
    break;
  case MIX_CONF_NUM:
    if (event.MainX[MIX_CONF_NUM].offset && event.MainX[MIX_CONF_NUM].length) {
      MIXBlock* mix = (MIXBlock*)((char*)&event+event.MainX[MIX_CONF_NUM].offset);
      assert(strncmp(mix->name,"MIX",3) == 0);
      copy_and_swap(boardAt(FE101_BASE_ADDRESS).channels,mix->FPDEastNSLayer1);
      copy_and_swap((unsigned char*)boardAt(MT101_BASE_ADDRESS).channels,mix->MTD_P2PLayer1);
      copy_and_swap(boardAt(TF101_BASE_ADDRESS).channels,mix->TOFLayer1);
      copy_and_swap(boardAt(TF001_BASE_ADDRESS).channels,mix->TOF);
      copy_and_swap(boardAt(TF002_BASE_ADDRESS).channels,mix->TOF+8);
      copy_and_swap(boardAt(TF003_BASE_ADDRESS).channels,mix->TOF+16);
      copy_and_swap(boardAt(TF004_BASE_ADDRESS).channels,mix->TOF+24);
      copy_and_swap(boardAt(TF005_BASE_ADDRESS).channels,mix->TOF+32);
      copy_and_swap(boardAt(TF006_BASE_ADDRESS).channels,mix->TOF+40);
    }
    break;
  case BCW_CONF_NUM:
    if (event.MainX[BCW_CONF_NUM].offset && event.MainX[BCW_CONF_NUM].length) {
      BWestBlock* bcw = (BWestBlock*)((char*)&event+event.MainX[BCW_CONF_NUM].offset);
      assert(strncmp(bcw->name,"BCW",3) == 0);
      unsigned char buffer[16];
      copy_and_swap(buffer,bcw->BEMCWest); unpack(boardAt(BW001_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bcw->BEMCWest+16); unpack(boardAt(BW002_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bcw->BEMCWest+32); unpack(boardAt(BW003_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bcw->BEMCWest+48); unpack(boardAt(BW004_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bcw->BEMCWest+64); unpack(boardAt(BW005_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bcw->BEMCWest+80); unpack(boardAt(BW006_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bcw->BEMCWest+96); unpack(boardAt(BW007_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bcw->BEMCWest+112); unpack(boardAt(BW008_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bcw->BEMCWest+128); unpack(boardAt(BW009_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bcw->BEMCWest+144); unpack(boardAt(BW010_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bcw->BEMCWest+160); unpack(boardAt(BW011_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bcw->BEMCWest+176); unpack(boardAt(BW012_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bcw->BEMCWest+192); unpack(boardAt(BW013_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bcw->BEMCWest+208); unpack(boardAt(BW014_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bcw->BEMCWest+224); unpack(boardAt(BW015_BASE_ADDRESS).channels,buffer);
    }
    break;
  case BCE_CONF_NUM:
    if (event.MainX[BCE_CONF_NUM].offset && event.MainX[BCE_CONF_NUM].length) {
      BEastBlock* bce = (BEastBlock*)((char*)&event+event.MainX[BCE_CONF_NUM].offset);
      assert(strncmp(bce->name,"BCE",3) == 0);
      unsigned char buffer[16];
      copy_and_swap(buffer,bce->BEMCEast); unpack(boardAt(BE001_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bce->BEMCEast+16); unpack(boardAt(BE002_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bce->BEMCEast+32); unpack(boardAt(BE003_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bce->BEMCEast+48); unpack(boardAt(BE004_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bce->BEMCEast+64); unpack(boardAt(BE005_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bce->BEMCEast+80); unpack(boardAt(BE006_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bce->BEMCEast+96); unpack(boardAt(BE007_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bce->BEMCEast+112); unpack(boardAt(BE008_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bce->BEMCEast+128); unpack(boardAt(BE009_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bce->BEMCEast+144); unpack(boardAt(BE010_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bce->BEMCEast+160); unpack(boardAt(BE011_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bce->BEMCEast+176); unpack(boardAt(BE012_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bce->BEMCEast+192); unpack(boardAt(BE013_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bce->BEMCEast+208); unpack(boardAt(BE014_BASE_ADDRESS).channels,buffer);
      copy_and_swap(buffer,bce->BEMCEast+224); unpack(boardAt(BE015_BASE_ADDRESS).channels,buffer);
    }
    break;
  case BBC_CONF_NUM:
    if (event.MainX[BBC_CONF_NUM].offset && event.MainX[BBC_CONF_NUM].length) {
      BBCBlock* bbc = (BBCBlock*)((char*)&event+event.MainX[BBC_CONF_NUM].offset);
      assert(strncmp(bbc->name,"BBC",3) == 0);
      copy_and_swap(boardAt(BB101_BASE_ADDRESS).channels,bbc->BBClayer1);
      copy_and_swap(boardAt(BB102_BASE_ADDRESS).channels,bbc->BBClayer1+8);
      copy_and_swap(boardAt(ZD101_BASE_ADDRESS).channels,bbc->ZDClayer1);
      copy_and_swap(boardAt(VP101_BASE_ADDRESS).channels,bbc->VPD);
    }
    break;
  case FMS_CONF_NUM:
    if (event.MainX[FMS_CONF_NUM].offset && event.MainX[FMS_CONF_NUM].length) {
      FMSBlock* fms = (FMSBlock*)((char*)&event+event.MainX[FMS_CONF_NUM].offset);
      assert(strncmp(fms->name,"FMS",3) == 0);
      copy_and_swap((unsigned char*)boardAt(FM001_BASE_ADDRESS).channels,fms->FMS);
      copy_and_swap((unsigned char*)boardAt(FM002_BASE_ADDRESS).channels,fms->FMS+16);
      copy_and_swap((unsigned char*)boardAt(FM003_BASE_ADDRESS).channels,fms->FMS+32);
      copy_and_swap((unsigned char*)boardAt(FM004_BASE_ADDRESS).channels,fms->FMS+48);
      copy_and_swap((unsigned char*)boardAt(FM101_BASE_ADDRESS).channels,fms->FMS+64);
      copy_and_swap((unsigned char*)boardAt(FM005_BASE_ADDRESS).channels,fms->FMS+80);
      copy_and_swap((unsigned char*)boardAt(FM006_BASE_ADDRESS).channels,fms->FMS+96);
      copy_and_swap((unsigned char*)boardAt(FM007_BASE_ADDRESS).channels,fms->FMS+112);
      copy_and_swap((unsigned char*)boardAt(FM008_BASE_ADDRESS).channels,fms->FMS+128);
      copy_and_swap((unsigned char*)boardAt(FM102_BASE_ADDRESS).channels,fms->FMS+144);
      copy_and_swap((unsigned char*)boardAt(FM009_BASE_ADDRESS).channels,fms->FMS+160);
      copy_and_swap((unsigned char*)boardAt(FM010_BASE_ADDRESS).channels,fms->FMS+176);
      copy_and_swap((unsigned char*)boardAt(FM011_BASE_ADDRESS).channels,fms->FMS+192);
      copy_and_swap((unsigned char*)boardAt(FM012_BASE_ADDRESS).channels,fms->FMS+208);
      copy_and_swap((unsigned char*)boardAt(FM103_BASE_ADDRESS).channels,fms->FMS+224);
    }
    break;
    // QT crates
  case MXQ_CONF_NUM:
  case FEQ_CONF_NUM:
  case BBQ_CONF_NUM:
  case QT1_CONF_NUM:
  case QT2_CONF_NUM:
  case QT3_CONF_NUM:
  case QT4_CONF_NUM:
    if (event.MainX[id].offset && event.MainX[id].length) {
      QTBlock* qt = (QTBlock*)((char*)&event+event.MainX[id].offset);
      assert(strncmp(qt->name,crateNames[id],3) == 0);
      decodeQT(*qt,id);
    }
    break;
  default:
    break;
  }
*/
}

inline void Crate::decodeQT(const QTBlock& qt, int crate, int t){
  int sz = qt.length/4;
  assert(qt.data[sz-1] == 0xac10);
  for (int i = 0; i < sz-1;) {
    // Header
    int h = qt.data[i++];
    assert(crate == (h >> 24 & 0xff));
    int nlines = h & 0xff;
    int addr = h >> 16 & 0xff;
    assert(0x10 <= addr && addr < 0x20);
    addr -= 0x10;
    // Data
    while (nlines--) {
      int d = qt.data[i++];
      int ch = d >> 27 & 0x1f;
      assert(0 <= ch && ch < 32);
      boards[addr].channels[t][ch] = d & 0xfff;
    }
  }
}

inline unsigned long long Crate::swapLL(unsigned long long x)
{
  return ((x & 0xffff000000000000ull) >> 48 |
          (x & 0x0000ffff00000000ull) >> 16 |
          (x & 0x00000000ffff0000ull) << 16 |
          (x & 0x000000000000ffffull) << 48);
}

inline void Crate::copy_and_swap(unsigned char* dest, const unsigned char* src)
{
  unsigned long long* x = (unsigned long long*)src;
  unsigned long long* y = (unsigned long long*)dest;

  *y = bswap_64(*x); ++x; ++y;
  *y = bswap_64(*x); ++x; ++y;
}

inline void Crate::copy_and_swap(unsigned short* dest, const unsigned short* src)
{
  unsigned long long* x = (unsigned long long*)src;
  unsigned long long* y = (unsigned long long*)dest;

  *y++ = swapLL(*x++);
  *y++ = swapLL(*x++);
}

inline void Crate::unpack(unsigned short* dest, const unsigned char* src)
{
  const unsigned char* cpMin = src;
  const unsigned char* cpMax = cpMin+15;
  unsigned short* sp = dest;
  for (const unsigned char* cp = cpMin; cp < cpMax; cp += 3) {
    unsigned int* ip = (unsigned int*)cp;
    *sp++ = *ip & 0xfff;
    *sp++ = *ip >> 12 & 0xfff;
  }
}

#endif // CRATE_HH
