//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#include "DSM.hh"
#include "sumTriggerPatchChannels.hh"
#include "DSMAlgo_EE001_2009.hh"

void DSMAlgo_EE001_2009::operator()(DSM& dsm)
{
  // INPUT:

  // 10 x 12-bit EEMC channels
  // (0-5) high tower
  // (6-11) trigger patch

  // REGISTERS:

  // R0: EEMC-High-Tower-Th0 (6)
  // R1: EEMC-High-Tower-Th1 (6)

  // ACTION:

  int lowEtaSum = 0;
  int highEtaSum = 0;
  int highTowerBits = 0;

  // Args: dsm, chMin, chMax, step, targetPedestal, sum, highTowerBits
  //printf("r0=%d\n", dsm.registers[0]);
  sumTriggerPatchChannels(dsm, 0, 8, 2, 3,  lowEtaSum, highTowerBits);
  sumTriggerPatchChannels(dsm, 1, 9, 2, 2, highEtaSum, highTowerBits);

  // OUTPUT (14):

  // (0-5) TP sum for low-eta group (6)
  // (6-11) TP sum for high-eta group (6)
  // (12-13) HT bits (2)

  int out = 0;

  out |=  lowEtaSum;
  out |= highEtaSum    <<  6;
  out |= highTowerBits << 12;

  dsm.output = out;
}
