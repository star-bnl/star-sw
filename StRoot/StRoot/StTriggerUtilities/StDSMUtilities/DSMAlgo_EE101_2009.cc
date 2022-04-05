#include "DSM.hh"
#include "DSMAlgo_EE101_2009.hh"

void DSMAlgo_EE101_2009::operator()(DSM& dsm)
{
  // INPUT:

  // EE101 - ch0 - EE001
  //         ch1 - EE002 - (0-15)  jp0 anti-clockwise
  //         ch2 - EE002 - (16-31) jp1 clockwise
  //         ch3 - EE003
  //         ch4 - EE004
  //         ch5 - EE005 - (0-15) jp0 anti-clockwise

  // From E001/003/004:

  // (0-5) TP sum for low-eta group (6)
  // (6-11) TP sum for high-eta group (6)
  // (12-13) HT bits (2)
  // (14-15) Unused (2)

  // From E002/005:

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

  int lowEtaSumA = dsm.channels[0] & 0x3f; // 4 o'clock
  int lowEtaSumB = dsm.channels[3] & 0x3f; // 6 o'clock
  int lowEtaSumC = dsm.channels[4] & 0x3f; // 8 o'clock

  int highEtaSumA = (dsm.channels[0] >> 6 & 0x3f) + (dsm.channels[1] & 0x3f); // 4 o'clock
  int highEtaSumB = (dsm.channels[3] >> 6 & 0x3f) + (dsm.channels[2] & 0x3f); // 6 o'clock
  int highEtaSumC = (dsm.channels[4] >> 6 & 0x3f) + (dsm.channels[5] & 0x3f); // 8 o'clock

  int jpa = lowEtaSumA + highEtaSumA; // JP3 - 4 o'clock
  int jpb = lowEtaSumB + highEtaSumB; // JP4 - 6 o'clock
  int jpc = lowEtaSumC + highEtaSumC; // JP5 - 8 o'clock

  // Place jet patch thresholds on sums

  int jpaBits = 0;

  if ((jpa > dsm.registers[0]) && !(jpa > dsm.registers[1]) && !(jpa > dsm.registers[2])) jpaBits = 1;
  if ((jpa > dsm.registers[0]) &&  (jpa > dsm.registers[1]) && !(jpa > dsm.registers[2])) jpaBits = 2;
  if ((jpa > dsm.registers[0]) &&  (jpa > dsm.registers[1]) &&  (jpa > dsm.registers[2])) jpaBits = 3;

  int jpbBits = 0;

  if ((jpb > dsm.registers[0]) && !(jpb > dsm.registers[1]) && !(jpb > dsm.registers[2])) jpbBits = 1;
  if ((jpb > dsm.registers[0]) &&  (jpb > dsm.registers[1]) && !(jpb > dsm.registers[2])) jpbBits = 2;
  if ((jpb > dsm.registers[0]) &&  (jpb > dsm.registers[1]) &&  (jpb > dsm.registers[2])) jpbBits = 3;

  int jpcBits = 0;

  if ((jpc > dsm.registers[0]) && !(jpc > dsm.registers[1]) && !(jpc > dsm.registers[2])) jpcBits = 1;
  if ((jpc > dsm.registers[0]) &&  (jpc > dsm.registers[1]) && !(jpc > dsm.registers[2])) jpcBits = 2;
  if ((jpc > dsm.registers[0]) &&  (jpc > dsm.registers[1]) &&  (jpc > dsm.registers[2])) jpcBits = 3;
  //printf("ee101 jpa=%d jpb=%d jpc=%d\n", jpa, jpb, jpc);
  //printf("ee101 r0=%d r1=%d r2=%d\n", dsm.registers[0], dsm.registers[1], dsm.registers[2]);
  // OR HT bits

  int highTowerBits = 0;

  for (int ch = 0; ch < 6; ++ch)
    highTowerBits |= dsm.channels[ch] >> 12 & 0xf;

  // Find largest sum for 1.09 < eta < 1.4 (low eta)
  //printf("ee101 lowEtaSumA=%d lowEtaSumB=%d lowEtaSumC=%d", lowEtaSumA, lowEtaSumB, lowEtaSumC);
  int maxA = (lowEtaSumA > lowEtaSumB) && !(lowEtaSumC > lowEtaSumA);
  int maxB = (lowEtaSumB > lowEtaSumC) && !(lowEtaSumA > lowEtaSumB);
  int maxC = (lowEtaSumC > lowEtaSumA) && !(lowEtaSumB > lowEtaSumC);

  int maxId;
  int maxSum;

  if (maxA) {
    maxId  = 1;
    maxSum = lowEtaSumA;
  }
  else if (maxB) {
    maxId  = 2;
    maxSum = lowEtaSumB;
  }
  else if (maxC) {
    maxId  = 3;
    maxSum = lowEtaSumC;
  }
  else {
    maxId  = 1;
    maxSum = lowEtaSumA;
  }
  //printf("maxId=%d", maxId);
  // If overflow, set to max

  if (maxSum > 63) maxSum = 63;

  // OUTPUT (16):

  // (0-1) JPA threshold bits (2)
  // (2-3) JPB threshold bits (2)
  // (4-5) JPC threshold bits (2)
  // (6-11) Selected partial jet patch sum (6)
  // (12-13) Partial jet patch ID (2)
  // (14-15) HT bits (2)

  dsm.output  = jpaBits;
  dsm.output |= jpbBits << 2;
  dsm.output |= jpcBits << 4;
  dsm.output |= maxSum << 6;
  dsm.output |= maxId << 12;
  dsm.output |= highTowerBits << 14;

  // INFO:

  // I0: JP3 (4 o'clock) ADC sum
  // I1: JP4 (6 o'clock) ADC sum
  // I2: JP5 (8 o'clock) ADC sum

  dsm.info[0] = jpa;
  dsm.info[1] = jpb;
  dsm.info[2] = jpc;
}
