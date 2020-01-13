#include "DSM.hh"
//#include "sumTriggerPatchChannels.hh"
#include "DSMAlgo_BW001_2014_b.hh"
#include "TString.h"

void DSMAlgo_BW001_2014_b::operator()(DSM& dsm)
{
  // INPUT:

  // 10 x 12-bit BEMC channels
  // (5-0) high-tower
  // (11-6) trigger-patch

  // REGISTERS:

  // R0: BEMC-High-Tower-Th0 (6)
  // R1: BEMC-High-Tower-Th1 (6)
  // R2: BEMC-High-Tower-Th2 (6)
  // R3: BEMC-High-Tower-Th3 (6)
  // R4: BEMC-High-Tower-Th4 (6)
  // R5: BEMC-High-Tower-UPC (6)
  // R6: BEMC-Trig-Patch-UPC (6)

  // Args: dsm, chMin, chMax, step, targetPedestal, sum, highTowerBits

  //  sumTriggerPatchChannels(dsm, 0, 5, 1, 1,  lowEtaSum, highTowerBits);
  //  sumTriggerPatchChannels(dsm, 6, 9, 1, 1, highEtaSum, highTowerBits);

  //Printf("BW001 register 5 and 6: %d and %d", dsm.registers[5], dsm.registers[6]);
  unsigned int highTowerBits[10][6];
  unsigned int trigPatchBits[10];

  for(int ichn = 0; ichn < 10; ichn++){
    unsigned int ht = dsm.channels[ichn] & 0x3f;
    for(int ireg = 0; ireg < 6; ireg++){
      highTowerBits[ichn][ireg] = ht > dsm.registers[ireg];
    }
    unsigned int tp = dsm.channels[ichn] >> 6 & 0x3f;
    trigPatchBits[ichn] = tp > dsm.registers[6];
  }

  unsigned int htBits[6];
  for(int ireg = 0; ireg < 6; ireg++){
    htBits[ireg] = 0;
    for(int ichn = 0; ichn < 10; ichn++){
      htBits[ireg] |= highTowerBits[ichn][ireg];
    }
  }
  unsigned int tpBits = 0;
  for(int ichn = 0; ichn < 10; ichn++){
    tpBits |= trigPatchBits[ichn];
  }
  unsigned int httpBits = 0;
  for(int ichn = 0; ichn < 10; ichn++){
    httpBits |= (highTowerBits[ichn][5] & trigPatchBits[ichn]);
  }
    
  // OUTPUT (16):

  // (0-8) Unused
  // (9) TP threshold bit
  // (10) HT.TP threshold bit
  // (11-15) HT threshold bits

  int out = 0;

  out |=  tpBits << 8;
  out |=   httpBits   <<  9;
  out |=   htBits[0] << 10;
  out |=   htBits[1] << 11;
  out |=   htBits[2] << 12;
  out |=   htBits[3] << 13;
  out |=   htBits[4] << 14;
  out |=   htBits[5] << 15;

  dsm.output = out;

}

