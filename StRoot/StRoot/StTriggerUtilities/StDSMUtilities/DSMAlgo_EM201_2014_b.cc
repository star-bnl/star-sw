//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2014_b
//
#include "bits.hh"
#include "DSM.hh"
#include "DSMAlgo_EM201_2014_b.hh"
#include "TString.h"

void DSMAlgo_EM201_2014_b::operator()(DSM& dsm)
{
  // INPUT:

  // 6 channels from BEMC

  // BEMC BC101 10 o'clock
  // BEMC BC102 12 o'clock
  // BEMC BC103 2 o'clock
  // BEMC BC104 4 o'clock
  // BEMC BC105 6 o'clock
  // BEMC BC106 8 o'clock

  // bits 0-7 unused
  // bit 8 DAQ10k test bit
  // bit 9 TP threshold bit
  // bit 10 HT.TP threshold bit
  // bits 11-15 HT threshold bits

  // 2 channels from EEMC

  // EEMC EE101 4, 6 and 8 o'clock
  // EEMC EE102 10, 12 and 2 o'clock

  // bits 0-13 unused
  // bits 14-15 HT threshold bits

  // REGISTERS:

  // R0: DAQ10k-Sector-Count(3)
  // R1: EMC-UPC-Topo-Swith (3)
  // ACTION:
  const int R0 = dsm.registers[0];
  const int R1 = dsm.registers[1]; 
  //Printf("EM201 R1 = 0x%x", R1);
  int bemcHT = 0; 
  int bemcTP = 0;
  int bemcHTTP = 0;

  // Combine (OR) the HT bits from the six BEMC layer 1 DSM's
  int bemcTPBit[6];
  int bemcHTTPBit[6];
  int bemcHTUPCBit[6];

  int counterDAQ10K = 0;

  for (int ichn = 0; ichn < 6; ++ichn) {
    bemcHT |= dsm.channels[ichn] >> 10 & 0x3f;

    bemcTPBit[ichn] = dsm.channels[ichn] >> 8 & 0x1;
    bemcTP |= bemcTPBit[ichn];

    bemcHTTPBit[ichn] = dsm.channels[ichn] >> 9 & 0x1;
    bemcHTTP |= bemcHTTPBit[ichn];

    bemcHTUPCBit[ichn] = dsm.channels[ichn] >> 15 & 0x1;

    counterDAQ10K += dsm.channels[ichn] >> 7 & 0x1;
  }
  int bemcDAQ10K = counterDAQ10K >= R0;

  // Combine (OR) the HT bits from the two EEMC layer 1 DSM's

  int eemcHT = 0; 

  for (int ichn = 6; ichn < 8; ++ichn) {
    eemcHT |= dsm.channels[ichn] >> 14 & 0x3;
  }

  int tpB2B = 0;
  int tpNONADJ = 0;

  int httpB2B = 0;
  int httpNONADJ = 0;

  int htUPCB2B = 0;
  int htUPCNONADJ = 0;

  for(int ichn = 0; ichn < 3; ichn++){
    int jchn = (ichn+3)%12;
    tpB2B |= bemcTPBit[ichn] && bemcTPBit[jchn];
    httpB2B |= bemcHTTPBit[ichn] && bemcHTTPBit[jchn];
    htUPCB2B |= bemcHTUPCBit[ichn] && bemcHTUPCBit[jchn];
  }

  tpNONADJ = (bemcTPBit[0] && (bemcTPBit[2] || bemcTPBit[3] || bemcTPBit[4]))
    || (bemcTPBit[1] && (bemcTPBit[3] || bemcTPBit[4] || bemcTPBit[5]))
    || (bemcTPBit[2] && (bemcTPBit[4] || bemcTPBit[5]))
    || (bemcTPBit[3] && bemcTPBit[5]);

  httpNONADJ = (bemcHTTPBit[0] && (bemcHTTPBit[2] || bemcHTTPBit[3] || bemcHTTPBit[4]))
    || (bemcHTTPBit[1] && (bemcHTTPBit[3] || bemcHTTPBit[4] || bemcHTTPBit[5]))
    || (bemcHTTPBit[2] && (bemcHTTPBit[4] || bemcHTTPBit[5]))
    || (bemcHTTPBit[3] && bemcHTTPBit[5]);

  htUPCNONADJ = (bemcHTUPCBit[0] && (bemcHTUPCBit[2] || bemcHTUPCBit[3] || bemcHTUPCBit[4]))
    || (bemcHTUPCBit[1] && (bemcHTUPCBit[3] || bemcHTUPCBit[4] || bemcHTUPCBit[5]))
    || (bemcHTUPCBit[2] && (bemcHTUPCBit[4] || bemcHTUPCBit[5]))
    || (bemcHTUPCBit[3] && bemcHTUPCBit[5]);

  int tpTopo = 0;
  if(btest(R1, 0)) tpTopo = tpB2B;
  else tpTopo = tpNONADJ;

  int httpTopo = 0;
  if(btest(R1, 1)) { //Printf("back to back"); 
     httpTopo = httpB2B; }
  else httpTopo = httpNONADJ;

  int htUPCTopo = 0;
  if(btest(R1, 2)) htUPCTopo = htUPCB2B;
  else htUPCTopo = htUPCNONADJ;
  // OUTPUT (16):

  // (0:4) Barrel HT bits (5)
  // (5) Barrel HT UPC
  // (6) Barrel TP bit
  // (7) Barrel HT.TP bit
  // (8) Barrel TP-based topo bit
  // (9) Barrel HTTP-based topo bit
  // (10) Barrel UPC-based topo bit
  // (11) Unused
  // (12) Unused
  // (13:14) Endcap HT bits (2)
  // (15) DAQ10k

  int out = 0;

  out |= (bemcHT & 0x3f);
  out |= bemcTP << 6;
  out |= bemcHTTP  << 7;
  out |= tpTopo  << 8;
  out |= httpTopo << 9;
  out |= htUPCTopo << 10;
  out |= eemcHT << 13;
  out |= bemcDAQ10K << 15;

  dsm.output = out;

}
