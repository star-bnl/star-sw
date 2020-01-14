#include "DSM.hh"
#include "DSMAlgo_EM201_2015.hh"

void DSMAlgo_EM201_2015::operator()(DSM& dsm)
{
  // INPUT:

  // DSM - ch0 - BEMC BC101 - 10' - JP0 & JP6 (West & East)
  //         ch1 - BEMC BC102 - 12' - JP1 & JP7
  //         ch2 - BEMC BC103 -  2' - JP2 & JP8
  //         ch3 - BEMC BC104 -  4' - JP3 & JP9
  //         ch4 - BEMC BC105 -  6' - JP4 & JP10
  //         ch5 - BEMC BC106 -  8' - JP5 & JP11
  //         ch6 - EEMC EE101 -  4',  6' and 8' - JP3, JP4 & JP5
  //         ch7 - EEMC EE102 - 10', 12' and 2' - JP0, JP1 & JP2

  // From BC101-106 (16):

  // (0-1) JPX (east, -1 < eta < 0) threshold bits (2)
  // (2-3) JPY (middle, -0.6 < eta < 0.4) threshold bits (2)
  // (4-5) JPZ (west, 0 < eta < 1) threshold bits (2)
  // (6-11) JPpartial (0.4 < eta < 1) sum (6)
  // (12-14) HT bits (3)
  // (15) HT.TP bit (1)

  // From EE101 (16):

  // (0-1) JPA (4 o'clock) threshold bits (2)
  // (2-3) JPB (6 o'clock) threshold bits (2)
  // (4-5) JPC (8 o'clock) threshold bits (2)
  // (6-11) Selected partial jet patch sum (6)
  // (12-13) Partial jet patch ID (1=A, 2=B, 3=C) (2)
  // (14-15) HT bits (2)

  // From EE102 (16):

  // (0-1) JPA (10 o'clock) threshold bits (2)
  // (2-3) JPB (12 o'clock) threshold bits (2)
  // (4-5) JPC (2  o'clock) threshold bits (2)
  // (6-11) Selected partial jet patch sum (6)
  // (12-13) Partial jet patch ID (1=A, 2=B, 3=C) (2)
  // (14-15) HT bits (2)

  // REGISTERS:

  // R0: Hybrid jet patch threshold-0
  // R1: Hybrid jet patch threshold-1
  // R2: Hybrid jet patch threshold-2
  // R3: AJP-th-Sel (2)
  // R4: BEMC-HTTP-Sel (2)
  // ACTION:

  // Complete hybrid jet patches using partial jet patch ID from EEMC

  int jpSum1; // Partial sum from EE101
  int jpSum2; // Partial sum from EE102

  getHybridJetPatchSums(dsm,jpSum1,jpSum2);

  // Combine (OR) the HT bits from the six BEMC layer 1 DSM's

  int htBitsBarrel = 0;

  for (int ch = 0; ch < 6; ++ch)
    htBitsBarrel |= dsm.channels[ch] >> 12 & 0x7;

  // Combine (OR) the HT bits from the two EEMC layer 1 DSM's

  int htBitsEndcap = 0;

  for (int ch = 6; ch < 8; ++ch)
    htBitsEndcap |= dsm.channels[ch] >> 14 & 0x3;

  // Combine (OR) the JP bits for the BEMC and EEMC separately

  int jpBitsBarrel = 0;

  for (int ch = 0; ch < 6; ++ch) {
    int jpx = dsm.channels[ch]      & 0x3;
    int jpy = dsm.channels[ch] >> 2 & 0x3;
    int jpz = dsm.channels[ch] >> 4 & 0x3;

    if (jpx > jpBitsBarrel) jpBitsBarrel = jpx;
    if (jpy > jpBitsBarrel) jpBitsBarrel = jpy;
    if (jpz > jpBitsBarrel) jpBitsBarrel = jpz;
  }

  int bjp1 = jpBitsBarrel > 1;
  int bjp2 = jpBitsBarrel > 2;

  int jpBitsEndcap = 0;

  for (int ch = 6; ch < 8; ++ch) {
    int jpa = dsm.channels[ch]      & 0x3;
    int jpb = dsm.channels[ch] >> 2 & 0x3;
    int jpc = dsm.channels[ch] >> 4 & 0x3;

    if (jpa > jpBitsEndcap) jpBitsEndcap = jpa;
    if (jpb > jpBitsEndcap) jpBitsEndcap = jpb;
    if (jpc > jpBitsEndcap) jpBitsEndcap = jpc;
  }

  int ejp1 = jpBitsEndcap > 1;
  int ejp2 = jpBitsEndcap > 2;

  // Compare the two completed hybrid jet patches to three thresholds
  // and combine (OR) the results with the BEMC-only and EEMC-only bits

  int jpBits = 0;

  for (int reg = 0; reg < 3; ++reg)
    if (jpSum1 > dsm.registers[reg] || jpSum2 > dsm.registers[reg]) ++jpBits;

  if (jpBitsBarrel > jpBits) jpBits = jpBitsBarrel;
  if (jpBitsEndcap > jpBits) jpBits = jpBitsEndcap;

  int jp0 = jpBits > 0;
  int jp1 = jpBits > 1;
  int jp2 = jpBits > 2;

  // Adjacent jet patch logic

  int ajpx = ajpBarrel(dsm, 0);
  int ajpy = ajpBarrel(dsm, 2);
  int ajpz = ajpBarrel(dsm, 4);
  int bajp = ajpx || ajpy || ajpz;
  int eajp = ajpEndcap(dsm);
  int  ajp = bajp || eajp;

  // Make the DAQ10k bit
  /*
  int htSel = dsm.registers[4]+12;

  int oclock10 = dsm.channels[0] >> htSel & 1;
  int oclock12 = dsm.channels[1] >> htSel & 1;
  int oclock2  = dsm.channels[2] >> htSel & 1;
  int oclock4  = dsm.channels[3] >> htSel & 1;
  int oclock6  = dsm.channels[4] >> htSel & 1;
  int oclock8  = dsm.channels[5] >> htSel & 1;

  int daq10kBackToBack = ((oclock2 && oclock8 ) ||
			  (oclock4 && oclock10) ||
			  (oclock6 && oclock12));

  unsigned int sectorCount = oclock10+oclock12+oclock2+oclock4+oclock6+oclock8;

  int reg5[2];

  reg5[0] = dsm.registers[5] >> 0 & 1;
  reg5[1] = dsm.registers[5] >> 1 & 1;

  int daq10kBit = ((reg5[0] && sectorCount > dsm.registers[6]) ||
		   (reg5[1] && sectorCount == 2 && daq10kBackToBack));
  */
  int http_b2b = 0;
  int http_nonadj = 0;
  getHTTP(dsm, http_b2b, http_nonadj);
//  printf("b2b=%d, nonadj=%d\n", http_b2b, http_nonadj);
  int http = 0;
  if(dsm.registers[4])
    http = http_nonadj;
  else
    http = http_b2b;
  // OUTPUT (16):

  // (0:3) Barrel HT bits (4)
  // (4:5) Endcap HT bits (2)
  // (6) JP1, unified over the BEMC+EEMC (1)
  // (7) JP2, unified over the BEMC+EEMC (1)
  // (8) BJP1 for the 18 BEMC-only patches (1)
  // (9) BJP2 for the 18 BEMC-only patches (1)
  // (10) EJP1 for the 6 EEMC-only patches (1)
  // (11) EJP2 for the 6 EEMC-only patches (1)
  // (12) AJP for BEMC and EEMC but NOT the boundary (1)
  // (13) BAJP for the BEMC-only patches (1)
  // (14) DAQ10k, DAQ10k trigger bit (1)
  // (15) JP0, unified over the BEMC+EEMC (1)

  int eb2b = getEB2B(dsm);
  eb2b = 0;   //set to zero not triggering on this bit 05/29/15
//  printf("eb2b=%d\n", eb2b);
  int out = 0;

  out |= htBitsBarrel;
  out |= http << 3;
  out |= htBitsEndcap << 4;
  out |= jp1  << 6;
  out |= jp2  << 7;
  out |= bjp1 << 8;
  out |= bjp2 << 9;
  out |= ejp1 << 10;
  out |= ejp2 << 11;
  out |= ajp  << 12;
  out |= bajp << 13;
  out |= eb2b << 14;
  out |= jp0  << 15;

  dsm.output = out;

  // INFO

  dsm.info[0] = jpSum1;
  dsm.info[1] = jpSum2;
}

int ajpBarrel(const DSM& em201, int offset)
{
  int jpBits[6];

  // BC101-106

  for (int ch = 0; ch < 6; ++ch)
    jpBits[ch] = em201.channels[ch] >> offset & 0x3;

  const int R3 = em201.registers[3]; // AJP-th-sel

  return ((jpBits[0] > R3 && jpBits[1] > R3) ||
	  (jpBits[1] > R3 && jpBits[2] > R3) ||
	  (jpBits[2] > R3 && jpBits[3] > R3) ||
	  (jpBits[3] > R3 && jpBits[4] > R3) ||
	  (jpBits[4] > R3 && jpBits[5] > R3) ||
	  (jpBits[5] > R3 && jpBits[0] > R3));
}

int ajpEndcap(const DSM& em201)
{
  int jpBits[6];

  // EE101

  jpBits[0] = em201.channels[6]      & 0x3; // JPA (4 o'clock)
  jpBits[1] = em201.channels[6] >> 2 & 0x3; // JPB (6 o'clock)
  jpBits[2] = em201.channels[6] >> 4 & 0x3; // JPC (8 o'clock)

  // EE102

  jpBits[3] = em201.channels[7]      & 0x3; // JPA (10 o'clock)
  jpBits[4] = em201.channels[7] >> 2 & 0x3; // JPB (12 o'clock)
  jpBits[5] = em201.channels[7] >> 4 & 0x3; // JPC (2  o'clock)

  const int R3 = em201.registers[3]; // AJP-th-sel

  return ((jpBits[0] > R3 && jpBits[1] > R3) ||
	  (jpBits[1] > R3 && jpBits[2] > R3) ||
	  (jpBits[2] > R3 && jpBits[3] > R3) ||
	  (jpBits[3] > R3 && jpBits[4] > R3) ||
	  (jpBits[4] > R3 && jpBits[5] > R3) ||
	  (jpBits[5] > R3 && jpBits[0] > R3));
}

void getHybridJetPatchSums(const DSM& em201, int& jpSum1, int& jpSum2)
{
  jpSum1 = em201.channels[6] >> 6 & 0x3f; // Partial sum from EE101
  jpSum2 = em201.channels[7] >> 6 & 0x3f; // Partial sum from EE102

  int jpId1 = em201.channels[6] >> 12 & 0x3; // Partial jet patch ID from EE101
  int jpId2 = em201.channels[7] >> 12 & 0x3; // Partial jet patch ID from EE102

  switch (jpId1) {
  case 1: jpSum1 += em201.channels[3] >> 6 & 0x3f; break; // Add partial sum from BC104 (4')
  case 2: jpSum1 += em201.channels[4] >> 6 & 0x3f; break; // Add partial sum from BC105 (6')
  case 3: jpSum1 += em201.channels[5] >> 6 & 0x3f; break; // Add partial sum from BC106 (8')
  }

  switch (jpId2) {
  case 1: jpSum2 += em201.channels[0] >> 6 & 0x3f; break; // Add partial sum from BC101 (10')
  case 2: jpSum2 += em201.channels[1] >> 6 & 0x3f; break; // Add partial sum from BC102 (12')
  case 3: jpSum2 += em201.channels[2] >> 6 & 0x3f; break; // Add partial sum from BC103 (2')
  }
}

void getHTTP(const DSM &em201, int &b2b, int &nonadj)
{
  b2b = 0;
  for(int ichn = 0, jchn = 3; ichn < 6 && ichn < jchn; ichn++, jchn = (ichn+3)%6){
    int ihttp = (em201.channels[ichn] >> 15) & 0x1;
//    jchn = (ichn + 3)%6;
    int jhttp = (em201.channels[jchn] >> 15) & 0x1;

    b2b |= ihttp && jhttp;
//    printf("b2b: %d=%d, %d=%d\n", ichn, ihttp, jchn, jhttp);
  }

  nonadj = 0;

  for(int ichn = 0; ichn < 6; ichn++){
    int ihttp = (em201.channels[ichn] >> 15) & 0x1;
    for(int jchn = ichn + 2; jchn < ichn + 5 && jchn < 6; jchn++){
      int jhttp = (em201.channels[jchn] >> 15) & 0x1;
      nonadj |= (ihttp && jhttp);
//      printf("nonadj: %d=%d, %d=%d\n", ichn, ihttp, jchn, jhttp);
    }
  }
}

int getEB2B(const DSM& em201)
{
  int jpBits[6];

  // EE101

  jpBits[0] = em201.channels[6]      & 0x3; // JPA (4 o'clock)
  jpBits[1] = em201.channels[6] >> 2 & 0x3; // JPB (6 o'clock)
  jpBits[2] = em201.channels[6] >> 4 & 0x3; // JPC (8 o'clock)

  // EE102

  jpBits[3] = em201.channels[7]      & 0x3; // JPA (10 o'clock)
  jpBits[4] = em201.channels[7] >> 2 & 0x3; // JPB (12 o'clock)
  jpBits[5] = em201.channels[7] >> 4 & 0x3; // JPC (2  o'clock)

  int b2b = 0;
  int JP = 0;

  for(int ichn = 0, jchn = 3; ichn < 6 && ichn < jchn; ichn++){
    int iJP = jpBits[ichn];
    jchn = (ichn + 3)%6;
    int jJP = jpBits[jchn];

    b2b |= (iJP > JP) && (jJP > JP);
  }
  return b2b;
}
