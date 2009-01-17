//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#include "DSM.hh"
#include "DSMAlgo_EE102_2009.hh"

void DSMAlgo_EE102_2009::operator()(DSM& dsm)
{
  // INPUT:

  // EE102 - ch0 - EE005 - (16-31) jp1 clockwise
  //         ch1 - EE006
  //         ch2 - EE007
  //         ch3 - EE008 - (0-15)  jp0 anti-clockwise
  //         ch4 - EE008 - (16-31) jp1 clockwise
  //         ch5 - EE009

  // From E006/007/009:

  // (0-5) TP sum for low-eta group (6)
  // (6-11) TP sum for high-eta group (6)
  // (12-13) HT bits (2)
  // (14-15) Unused (2)

  // From E005/008:

  // (0-5) TP sum (6)
  // (6-11) Unused (2)
  // (12-13) HT bits (2)
  // (14-15) Unused (2)

  // REGISTERS:

  // R0: EEMC-Jet-Patch-Th0 (8)
  // R1: EEMC-Jet-Patch-Th1 (8)
  // R2: EEMC-Jet-Patch-Th2 (8)

  // ACTION:

  // Make jet patch sums

  int lowEtaSumA = dsm.channels[1] & 0x3f; // 10 o'clock
  int lowEtaSumB = dsm.channels[2] & 0x3f; // 12 o'clock
  int lowEtaSumC = dsm.channels[5] & 0x3f; // 2  o'clock

  int highEtaSumA = (dsm.channels[1] >> 6 & 0x3f) + (dsm.channels[0] & 0x3f); // 10 o'clock
  int highEtaSumB = (dsm.channels[2] >> 6 & 0x3f) + (dsm.channels[3] & 0x3f); // 12 o'clock
  int highEtaSumC = (dsm.channels[5] >> 6 & 0x3f) + (dsm.channels[4] & 0x3f); // 2  o'clock

  int jpa = lowEtaSumA + highEtaSumA; // JP0 - 10 o'clock
  int jpb = lowEtaSumB + highEtaSumB; // JP1 - 12 o'clock
  int jpc = lowEtaSumC + highEtaSumC; // JP2 - 2  o'clock

  // Place jet patch thresholds on sums

  int jpaBits = 0;
  int jpbBits = 0;
  int jpcBits = 0;

  for (int reg = 0; reg < 3; ++reg) {
    if (jpa > dsm.registers[reg]) ++jpaBits;
    if (jpb > dsm.registers[reg]) ++jpbBits;
    if (jpc > dsm.registers[reg]) ++jpcBits;
  }

  // OR HT bits

  int highTowerBits = 0;

  for (int ch = 0; ch < 6; ++ch)
    highTowerBits |= dsm.channels[ch] >> 12 & 0xf;

  // Find largest sum for 1.09 < eta < 1.4 (low eta)

  int maxId = 0;
  int maxSum = 0;

  if (lowEtaSumA > maxSum) { maxId = 1; maxSum = lowEtaSumA; }
  if (lowEtaSumB > maxSum) { maxId = 2; maxSum = lowEtaSumB; }
  if (lowEtaSumC > maxSum) { maxId = 3; maxSum = lowEtaSumC; }

  // If overflow, set to max

  if (maxSum > 63) maxSum = 63;

  // OUTPUT (16):

  // (0-1) JPA threshold bits (2)
  // (2-3) JPB threshold bits (2)
  // (4-5) JPC threshold bits (2)
  // (6-11) Selected partial jet patch sum (6)
  // (12-13) Partial jet patch ID (2)
  // (14-15) HT bits (2)

  int out = 0;

  out |= jpaBits;
  out |= jpbBits << 2;
  out |= jpcBits << 4;
  out |= maxSum << 6;
  out |= maxId << 12;
  out |= highTowerBits << 14;

  dsm.output = out;

  // INFO:

  // I0: JP3 (10 o'clock) ADC sum
  // I1: JP4 (12 o'clock) ADC sum
  // I2: JP5 (2  o'clock) ADC sum

  dsm.info[0] = jpa;
  dsm.info[1] = jpb;
  dsm.info[2] = jpc;
}
