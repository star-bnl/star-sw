#include "bits.hh"
#include "DSM.hh"
#include "DSMAlgo_EM201_2017.hh"

void DSMAlgo_EM201_2017::operator()(DSM& dsm)
{
  // INPUT:

  // EM201 - ch0 - BEMC BC101 - 10' - JP0 & JP6 (West & East)
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
    htBitsBarrel |= (dsm.channels[ch] >> 12) & 0x7;

  // Combine (OR) the HT bits from the two EEMC layer 1 DSM's

  int htBitsEndcap = 0;

  for (int ch = 6; ch < 8; ++ch)
    htBitsEndcap |= dsm.channels[ch] >> 14 & 0x3;

  // Combine (OR) HT.TP bits from BEMC and EEMC for each hour
  int httpBits[6] = {0, 0, 0, 0, 0, 0};
  getHybridHTTP(dsm, httpBits); 
  
  int http_b2b = 0;
  int http_nonadj = 0;
  getHTTP(httpBits, http_b2b, http_nonadj);
  //printf("%d, %d %d\n", dsm.registers[3], http_nonadj, http_b2b);
  int http = 0;
  if(dsm.registers[3])
    http = http_nonadj;
  else
    http = http_b2b;
   
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
    int jp = dsm.channels[ch]      & 0x3;
    if (jp > jpBitsEndcap) jpBitsEndcap = jp;
  }

  int ejp1 = jpBitsEndcap > 1;
  int ejp2 = jpBitsEndcap > 2;

  // Compare the two completed hybrid jet patches to three thresholds
  // and combine (OR) the results with the BEMC-only and EEMC-only bits

  int jpBits = 0;

  for (int reg = 0; reg < 3; ++reg)
    if (jpSum1 > dsm.registers[reg] || jpSum2 > dsm.registers[reg]) ++jpBits;
  //cout<<"r0: "<<dsm.registers[0]<<" r1: "<<dsm.registers[1]<<" r2: "<<dsm.registers[2]<<endl;
  if (jpBitsBarrel > jpBits) jpBits = jpBitsBarrel;
  if (jpBitsEndcap > jpBits) jpBits = jpBitsEndcap;

  int jp0 = jpBits > 0;
  int jp1 = jpBits > 1;
  int jp2 = jpBits > 2;

  // OUTPUT (16):

  // (0:2) Barrel HT bits (3)
  // (3) HT.TP bit (1)
  // (4:6) Endcap HT(2)
  // (6) BJP1
  // (7) BJP2
  // (8) JP0
  // (9) JP1 
  // (10) JP2
  // (11) EJP1
  // (12) EJP2
  // (13:15) Unused
  
  int out = 0;

  out |= htBitsBarrel;
  out |= http << 3;
  out |= htBitsEndcap << 4;
  out |= bjp1  << 6;
  out |= bjp2  << 7;
  out |= jp0 << 8;
  out |= jp1 << 9;
  out |= jp2 << 10;
  out |= ejp1 << 11;
  out |= ejp2  << 12;
  
  dsm.output = out;

}
void DSMAlgo_EM201_2017::getHybridJetPatchSums(const DSM& dsm, int& jpSum1, int& jpSum2)
{
  jpSum1 = dsm.channels[6] >> 6 & 0x3f; // Partial sum from EE101
  jpSum2 = dsm.channels[7] >> 6 & 0x3f; // Partial sum from EE102

  int jpId1 = dsm.channels[6] >> 12 & 0x3; // Partial jet patch ID from EE101
  int jpId2 = dsm.channels[7] >> 12 & 0x3; // Partial jet patch ID from EE102

  switch (jpId1) {
  case 1: jpSum1 += dsm.channels[3] >> 6 & 0x3f; break; // Add partial sum from BC104 (4')
  case 2: jpSum1 += dsm.channels[4] >> 6 & 0x3f; break; // Add partial sum from BC105 (6')
  case 3: jpSum1 += dsm.channels[5] >> 6 & 0x3f; break; // Add partial sum from BC106 (8')
  }

  switch (jpId2) {
  case 1: jpSum2 += dsm.channels[0] >> 6 & 0x3f; break; // Add partial sum from BC101 (10')
  case 2: jpSum2 += dsm.channels[1] >> 6 & 0x3f; break; // Add partial sum from BC102 (12')
  case 3: jpSum2 += dsm.channels[2] >> 6 & 0x3f; break; // Add partial sum from BC103 (2')
  }
}
void DSMAlgo_EM201_2017::getHTTP(int httpBits[], int &b2b, int &nonadj)
{
  b2b = 0;
  for(int ichn = 0, jchn = 3; ichn < 6 && ichn < jchn; ichn++, jchn = (ichn+3)%6){
    int ihttp = httpBits[ichn];
    //    jchn = (ichn + 3)%6;
    int jhttp = httpBits[jchn];

    b2b |= ihttp && jhttp;
    //printf("b2b: %d=%d, %d=%d\n", ichn, ihttp, jchn, jhttp);
  }

  nonadj = 0;

  for(int ichn = 0; ichn < 6; ichn++){
    int ihttp = httpBits[ichn];
    for(int jchn = ichn + 2; jchn < ichn + 5 && jchn < 6; jchn++){
      int jhttp = httpBits[jchn];
      nonadj |= (ihttp && jhttp);
      //printf("nonadj: %d=%d, %d=%d\n", ichn, ihttp, jchn, jhttp);
    }
  }
}
void DSMAlgo_EM201_2017::getHybridHTTP(const DSM& dsm, int httpBits[])
{
  //ee101
  for(int i = 0; i < 3; i++){
    int j = (i+3)%6;
    int k = i + 2;
    httpBits[i] = btest(dsm.channels[j], 15) | btest(dsm.channels[6], k);
  }
  //ee102
  for(int i = 3; i < 6; i++){
    int j = (i+3)%6;
    int k = (i - 3) + 2;
    httpBits[i] = btest(dsm.channels[j], 15) | btest(dsm.channels[7], k);
  }
}
