#include "DSMAlgo_BC101_2015.hh"

void DSMAlgo_BC101_2015::operator()(DSM& dsm)
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

  unsigned int jpx;			// East (-1 < eta < 0)
  unsigned int jpy;			// Middle (-0.6 < eta < 0.4)
  unsigned int jpz;			// West (0 < eta < 1)
  unsigned int jpPartial;		// Partial (0.4 < eta < 1)

  int highTowerBits;

  getBemcJetPatchSums2015A(dsm,jpx,jpy,jpz,jpPartial,highTowerBits);

  // Compare each jet patch sum to three thresholds
  // and then pack results for each jet patch into
  // 2-bit integer.

  int jpxBits = 0;
  int jpyBits = 0;
  int jpzBits = 0;
  //BEMC-JP-th2-East BEMC-JP-th2-Mid BEMC-JP-th2-West weren't properly set up in the offlince database therefore still using the th2-East for th2-Mid and th2-West
  //printf("r2=%d r3=%d r4=%d\n", dsm.registers[2], dsm.registers[3], dsm.registers[4]);
  for (int reg = 0; reg < 3; ++reg) {
    if (jpx > dsm.registers[reg]) ++jpxBits;
    if (jpy > dsm.registers[reg]) ++jpyBits;
    if (jpz > dsm.registers[reg]) ++jpzBits;
    //if(reg < 2){
      //if (jpx > dsm.registers[reg]) ++jpxBits;
      //if (jpy > dsm.registers[reg]) ++jpyBits;
      //if (jpz > dsm.registers[reg]) ++jpzBits;
    //}else if(reg == 2){
      //if (jpx > dsm.registers[2]) ++jpxBits;
      //if (jpy > dsm.registers[3]) ++jpyBits;
      //if (jpz > dsm.registers[4]) ++jpzBits;
    //}
  }

  int daq10kBits = 0;

  for (int ch = 0; ch < 6; ++ch) {
    int htBits = dsm.channels[ch] >> 12 & 0xf;
    daq10kBits |= (htBits >> dsm.registers[5] & 0x1) << ch;
  }

  // OUTPUT (16+6):

  // (0-1) JPX threshold bits (2)
  // (2-3) JPY threshold bits (2)
  // (4-5) JPZ threshold bits (2)
  // (6-11) JPpartial sum (6)
  // (12-15) HT bits (4)

  // (16-21) DAQ10k HT bits (6)

  int out = 0;

  out |= jpxBits;
  out |= jpyBits << 2;
  out |= jpzBits << 4;
  out |= jpPartial << 6;
  out |= highTowerBits << 12;

  out |= daq10kBits << 16;

  dsm.output = out;
  // INFO:

  dsm.info[0] = jpx;
  dsm.info[1] = jpy;
  dsm.info[2] = jpz;
}
void getBemcJetPatchSums2015A(const DSM& bc101, unsigned int& jpx, unsigned int& jpy, unsigned int& jpz, unsigned int& jpPartial, int& highTowerBits)
{
  jpx = 0;
  jpy = 0;
  jpz = 0;
  jpPartial = 0;
  highTowerBits = 0;

  // East (ch0/2/4 - even channels)

  for (int ch = 0; ch <= 4; ch += 2) {
    int lowEtaSum = bc101.channels[ch] & 0x3f;
    int highEtaSum = bc101.channels[ch] >> 6 & 0x3f;
    jpx += lowEtaSum + highEtaSum;
    jpy += lowEtaSum;
    highTowerBits |= bc101.channels[ch] >> 12 & 0xf;
  }

  // West (ch1/3/5 - odd channels)

  for (int ch = 1; ch <= 5; ch += 2) {
    int lowEtaSum = bc101.channels[ch] & 0x3f;
    int highEtaSum = bc101.channels[ch] >> 6 & 0x3f;
    jpy += lowEtaSum;
    jpz += lowEtaSum + highEtaSum;
    jpPartial += highEtaSum;
    highTowerBits |= bc101.channels[ch] >> 12 & 0xf;
  }

  // If overflow, set JPpartial sum to max
  if (jpPartial > 63) jpPartial = 63;  
}
