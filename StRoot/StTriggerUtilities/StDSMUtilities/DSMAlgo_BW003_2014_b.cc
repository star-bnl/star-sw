#include "DSM.hh"
//#include "sumTriggerPatchChannels.hh"
#include "DSMAlgo_BW003_2014_b.hh"
#include "TString.h"

void DSMAlgo_BW003_2014_b::operator()(DSM& dsm)
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
  //Printf("BW003 register 5 and 6: %d and %d", dsm.registers[5], dsm.registers[6]);

  unsigned int highTowerBits[10][6];
  unsigned int trigPatchBits[10];

  for(int ichn = 0; ichn < 10; ichn++){
    unsigned int ht = dsm.channels[ichn] & 0x3f;
    for(int ireg = 0; ireg < 6; ireg++){
      highTowerBits[ichn][ireg] = ht > dsm.registers[ireg];
    }
    unsigned int tp = dsm.channels[ichn] >> 6 & 0x3f;
    trigPatchBits[ichn] = tp > dsm.registers[5];
  }

  unsigned int evenhtBits[6];
  unsigned int oddhtBits[6];

  for(int ireg = 0; ireg < 6; ireg++){
    evenhtBits[ireg] = 0;
    oddhtBits[ireg] = 0;

    for(int iichn = 0; iichn < 5; iichn++){
      evenhtBits[ireg] |= highTowerBits[2*iichn][ireg];
      oddhtBits[ireg] |= highTowerBits[2*iichn+1][ireg];
    }
  }

  unsigned int eventpBits = 0;
  unsigned int oddtpBits = 0;

  for(int iichn = 0; iichn < 5; iichn++){
    eventpBits |= trigPatchBits[2*iichn];
    oddtpBits |= trigPatchBits[2*iichn+1];
  }

  unsigned int evenhttpBits = 0;
  unsigned int oddhttpBits = 0;
  for(int iichn = 0; iichn < 5; iichn++){
    evenhttpBits |= (highTowerBits[2*iichn][5] & trigPatchBits[2*iichn]);
    oddhttpBits |= (highTowerBits[2*iichn+1][5] & trigPatchBits[2*iichn+1]);
  }
    
  // OUTPUT (16):

  // (0-8) Unused
  // (9) TP threshold bit odd channels
  // (10) HT.TP threshold bit odd channels
  // (11-15) HT threshold bits odd channels
  // (16-24) Unused
  // (25) TP threshold bit even channels
  // (26) HT.TP threshold bit even channels
  // (27-31) HT threshold bit even channels
  int out = 0;

  out |=  oddtpBits << 8;
  out |=   oddhttpBits   <<  9;
  out |=   oddhtBits[0] << 10;
  out |=   oddhtBits[1] << 11;
  out |=   oddhtBits[2] << 12;
  out |=   oddhtBits[3] << 13;
  out |=   oddhtBits[4] << 14;
  out |=   oddhtBits[5] << 15;

  out |=  eventpBits << 24;
  out |=   evenhttpBits   <<  25;
  out |=   evenhtBits[0] << 26;
  out |=   evenhtBits[1] << 27;
  out |=   evenhtBits[2] << 28;
  out |=   evenhtBits[3] << 29;
  out |=   evenhtBits[4] << 30;
  out |=   evenhtBits[5] << 31;

  dsm.output = out;

}
