//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#include "DSM.hh"
#include "sumTriggerPatchChannels.hh"
#include "DSMAlgo_EE002_2009.hh"

void DSMAlgo_EE002_2009::operator()(DSM& dsm)
{
  // INPUT:

  // 10 x 12-bit EEMC channels
  // (0-5) high tower
  // (6-11) trigger patch

  // REGISTERS:

  // R0: EEMC-High-Tower-Th0 (6)
  // R1: EEMC-High-Tower-Th1 (6)

  // ACTION:

  // J0 (ch0/1/2/3/4) to first output cable (0-15)

  int sumJ0 = 0;
  int highTowerBitsJ0 = 0;

  // Args: dsm, chMin, chMax, step, targetPedestal, sum, highTowerBits
  //printf("ee002 r0=%d\n", dsm.registers[0]); 
  sumTriggerPatchChannels(dsm, 0, 4, 1, 1, sumJ0, highTowerBitsJ0);

  // J1 (ch5/6/7/8/9) to second output cable (16-31)

  int sumJ1 = 0;
  int highTowerBitsJ1 = 0;

  // Args: dsm, chMin, chMax, step, targetPedestal, sum, highTowerBits
  sumTriggerPatchChannels(dsm, 5, 9, 1, 1, sumJ1, highTowerBitsJ1);

  // OUTPUT (32):

  // (0-5) TP sum JO (6)
  // (6-11) Unused (2)
  // (12-13) HT bits J0 (2)
  // (14-15) Unused (2)

  int out = 0;

  out |= sumJ0;
  out |= highTowerBitsJ0 << 12;

  // (16-21) TP sum J1 (6)
  // (22-27) Unused (2)
  // (28-29) HT bits J1 (2)
  // (30-31) Unused (2)

  out |= sumJ1 << 16;
  out |= highTowerBitsJ1 << 28;

  dsm.output = out;
}
