#include "DSM.hh"
#include "sum_eemc_http.hh"
#include "DSMAlgo_EE102_2017.hh"

void DSMAlgo_EE102_2017::operator()(DSM& dsm)
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

  int jpa; // JP0 - 10 o'clock
  int jpb; // JP1 - 12 o'clock
  int jpc; // JP2 - 2  o'clock

  getEemcUpperHalfJetPatchSums(dsm,jpa,jpb,jpc);


  int httpa, httpb, httpc;
  getEemcHTTP(dsm, httpa, httpb, httpc);
  
  // Place jet patch thresholds on sums

  int jpBits = 0;
  
  if(jpa > dsm.registers[2] || jpb > dsm.registers[2] || jpc > dsm.registers[2]){
    jpBits = 3;
  }else if(jpa > dsm.registers[1] || jpb > dsm.registers[1] || jpc > dsm.registers[1]){
    jpBits = 2;
  }else if(jpa > dsm.registers[0] || jpb > dsm.registers[0] || jpc > dsm.registers[0]){
    jpBits = 1;
  }else
    jpBits = 0;


  // OR HT bits

  int highTowerBits = 0;

  for (int ch = 0; ch < 6; ++ch)
    highTowerBits |= dsm.channels[ch] >> 12 & 0xf;

  // Find largest sum for 1.09 < eta < 1.4 (low eta)

  int lowEtaSumA = dsm.channels[1] & 0x3f; // 10 o'clock
  int lowEtaSumB = dsm.channels[2] & 0x3f; // 12 o'clock
  int lowEtaSumC = dsm.channels[5] & 0x3f; // 2  o'clock

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

  // If overflow, set to max

  if (maxSum > 63) maxSum = 63;

  // OUTPUT (16):

  // (0-1) JP threshold bits (2)
  // (2) HT.TP-A bit (1)
  // (3) HT.TP-B bit (1)
  // (4) HT.TP-C bit (1)
  // (5) Unused
  // (6-11) Selected partial jet patch sum (6)
  // (12-13) Partial jet patch ID (2)
  // (14-15) HT bits (2)

  int out = 0;
  out  = jpBits;
  out |= httpa << 2;
  out |= httpb << 3;
  out |= httpc << 4;
  out |= maxSum << 6;
  out |= maxId << 12;
  out |= highTowerBits << 14;

  dsm.output = out;
}

void DSMAlgo_EE102_2017::getEemcUpperHalfJetPatchSums(const DSM& dsm, int& jpa, int& jpb, int& jpc)
{
  int lowEtaSumA = dsm.channels[1] & 0x3f; // 10 o'clock
  int lowEtaSumB = dsm.channels[2] & 0x3f; // 12 o'clock
  int lowEtaSumC = dsm.channels[5] & 0x3f; // 2  o'clock

  int highEtaSumA = (dsm.channels[1] >> 6 & 0x3f) + (dsm.channels[0] & 0x3f); // 10 o'clock
  int highEtaSumB = (dsm.channels[2] >> 6 & 0x3f) + (dsm.channels[3] & 0x3f); // 12 o'clock
  int highEtaSumC = (dsm.channels[5] >> 6 & 0x3f) + (dsm.channels[4] & 0x3f); // 2  o'clock

  jpa = lowEtaSumA + highEtaSumA; // JP0 - 10 o'clock
  jpb = lowEtaSumB + highEtaSumB; // JP1 - 12 o'clock
  jpc = lowEtaSumC + highEtaSumC; // JP2 - 2  o'clock
}


