//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#include "DSMAlgo_BC101_2009.hh"

void DSMAlgo_BC101_2009::operator()(DSM& dsm)
{
  // INPUT:

  // ----------------------------------------------------------------------
  // BC101 - ch0 - BE001               | B101 - ch1 - BW001               |
  //         ch2 - BE002               |        ch3 - BW002               |
  //         ch4 - BE003 - JP1 (0-15)  |        ch5 - BW003 - JP1 (0-15)  |
  // ----------------------------------------------------------------------
  // BC102 - ch0 - BE003 - JP6 (16-31) | B102 - ch1 - BW003 - JP6 (16-31) |
  //         ch2 - BE004               |        ch3 - BW004               |
  //         ch4 - BE005               |        ch5 - BW005               |
  // ----------------------------------------------------------------------

  // From each channel:

  // (0-5) TP sum for low-eta group (6)
  // (6-11) TP sum for high-eta group (6)
  // (12-15) HT bits (4)

  // REGISTERS:

  // R0: BEMC-Jet-Patch-Th0 (9)
  // R1: BEMC-Jet-Patch-Th1 (9)
  // R2: BEMC-Jet-Patch-Th2 (9)

  // ACTION:

  int jpx = 0;			// East (-1 < eta < 0)
  int jpy = 0;			// Middle (-0.6 < eta < 0.4)
  int jpz = 0;			// West (0 < eta < 1)
  int jpPartial = 0;		// Partial (0.4 < eta < 1)

  int highTowerBits = 0;

  // East (ch0/2/4 - even channels)

  for (int ch = 0; ch <= 4; ch += 2) {
    int lowEtaSum = dsm.channels[ch] & 0x3f;
    int highEtaSum = dsm.channels[ch] >> 6 & 0x3f;
    jpx += lowEtaSum + highEtaSum;
    jpy += lowEtaSum;
    highTowerBits |= dsm.channels[ch] >> 12 & 0xf;
  }

  // West (ch1/3/5 - odd channels)

  for (int ch = 1; ch <= 5; ch += 2) {
    int lowEtaSum = dsm.channels[ch] & 0x3f;
    int highEtaSum = dsm.channels[ch] >> 6 & 0x3f;
    jpy += lowEtaSum;
    jpz += lowEtaSum + highEtaSum;
    jpPartial += highEtaSum;
    highTowerBits |= dsm.channels[ch] >> 12 & 0xf;
  }

  // If overflow, set JPpartial sum to max
  if (jpPartial > 63) jpPartial = 63;

  // Compare each jet patch sum to three thresholds
  // and then pack results for each jet patch into
  // 2-bit integer.

  int jpxBits = 0;
  int jpyBits = 0;
  int jpzBits = 0;

  for (int reg = 0; reg < 3; ++reg) {
    if (jpx > dsm.registers[reg]) ++jpxBits;
    if (jpy > dsm.registers[reg]) ++jpyBits;
    if (jpz > dsm.registers[reg]) ++jpzBits;
  }
  //printf("jpx=%d jpy=%d jpz=%d jppartial=%d\n", jpx, jpy, jpz, jpPartial);
  //printf("r0=%d r1=%d r2=%d\n", dsm.registers[0], dsm.registers[1], dsm.registers[2]);
  // OUTPUT (16):

  // (0-1) JPX threshold bits (2)
  // (2-3) JPY threshold bits (2)
  // (4-5) JPZ threshold bits (2)
  // (6-11) JPpartial sum (6)
  // (12-15) HT bits (4)

  int out = 0;

  out |= jpxBits;
  out |= jpyBits << 2;
  out |= jpzBits << 4;
  out |= jpPartial << 6;
  out |= highTowerBits << 12;

  dsm.output = out;

  // INFO:

  dsm.info[0] = jpx;
  dsm.info[1] = jpy;
  dsm.info[2] = jpz;
}
