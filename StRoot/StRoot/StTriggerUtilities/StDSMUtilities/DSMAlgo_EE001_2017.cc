#include "DSM.hh"
#include "sumTriggerPatchChannels.hh"
#include "DSMAlgo_EE001_2017.hh"

void DSMAlgo_EE001_2017::operator()(DSM& dsm)
{
  // INPUT:

  // 10 x 12-bit EEMC channels
  // (0-5) high tower
  // (6-11) trigger patch

  // REGISTERS:

  // R0: EEMC-HT-Th0 (6)
  // R1: EEMC-HT-Th1 (6)
  // R2: EEMC-HT-UPC (6)
  // R3: EEMC-TP-UPC (6)

  // ACTION:

  int lowEtaSum = 0;
  int highEtaSum = 0;
  int highTowerBits = 0;

  // Args: dsm, chMin, chMax, step, targetPedestal, sum, highTowerBits

  sumTriggerPatchChannels2015(dsm, 0, 8, 2, 3,  lowEtaSum, highTowerBits);
  sumTriggerPatchChannels2015(dsm, 1, 9, 2, 2, highEtaSum, highTowerBits);

  // OUTPUT (14):

  // (0-5) TP sum for low-eta group (6)
  // (6-11) TP sum for high-eta group (6)
  // (12-13) HT bits (2)
  // (14) Unused (1)
  // (15) HT.TP bit (1)
  int out = 0;
  out |=  lowEtaSum;
  out |= highEtaSum    <<  6;
  out |= highTowerBits << 12;
  dsm.output = out;

}
