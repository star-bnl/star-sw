#include "DSM.hh"
#include "DSMAlgo_EE101_2013.hh"

void DSMAlgo_EE101_2013::operator()(DSM& dsm)
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
  // R3: EEMC-JP-th-dijet (8)

  // ACTION:

  // Make jet patch sums
  // Low eta single-output DSM boards

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
  int jpbBits = 0;
  int jpcBits = 0;

  int jpCmbBits = 0;

  //jpa, jpb, jpb and combined jpCmb output for jp thresholds 0:2
  for(int chn = 0; chn < 3; ++chn)
    {
      if(jpa > dsm.registers[chn]) jpaBits++;
      if(jpb > dsm.registers[chn]) jpbBits++;
      if(jpc > dsm.registers[chn]) jpcBits++;

      if(jpa > dsm.registers[chn] || jpb > dsm.registers[chn] || jpc > dsm.registers[chn]) jpCmbBits++;
	//printf("r%d = %d ", chn, dsm.registers[chn]);
    } 
  //printf("\n");
  //jpa, jpb, jpc di-jet output
  int r3 = dsm.registers[3];

  int jpaDijetBits = jpa > r3;
  int jpbDijetBits = jpb > r3;
  int jpcDijetBits = jpc > r3;
  //printf("jpa = %d jpb = %d jpc = %d R3 = %d\n", jpa, jpb, jpc, r3);
  // OR HT bits

  int highTowerBits = 0;

  for (int ch = 0; ch < 6; ++ch)
    highTowerBits |= dsm.channels[ch] >> 12 & 0x3;

  // Find largest sum for 1.09 < eta < 1.4 (low eta)

  int maxA = (lowEtaSumA > lowEtaSumB) && !(lowEtaSumC > lowEtaSumA);
  int maxB = (lowEtaSumB > lowEtaSumC) && !(lowEtaSumA > lowEtaSumB);
  int maxC = (lowEtaSumC > lowEtaSumA) && !(lowEtaSumB > lowEtaSumC);

  int maxId = 0;
  int maxSum = 0;

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

  // If overflow, set to max

  if (maxSum > 63) maxSum = 63;

  // OUTPUT (16):

  // 0:1 JP threshold (JPA, B and C combined)
  // 2 JPA th-dijet bit
  // 3 JPB th-dijet bit
  // 4 JPC th-dijet bit
  // 5 Unused
  // 6:11 Selected partial jet patch sum
  // 12:13 Partial jet patch ID
  // 14:15 HT bits 

  dsm.output  = jpCmbBits;
  dsm.output |= jpaDijetBits << 2;
  dsm.output |= jpbDijetBits << 3;
  dsm.output |= jpcDijetBits << 4;
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
