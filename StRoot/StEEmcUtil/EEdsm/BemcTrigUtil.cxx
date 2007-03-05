//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 4 March 2007
//

#include "BemcTrigUtil.h"

//
// See $STAR/StRoot/StEvent/StTriggerData.cxx
//
int BemcTrigUtil::decodeBemcTP12bitToRaw8bit(int dsm, int channel, unsigned char* raw)
{
  static const int dsmmap[16] = {7,6,5,4,3,2,1,0,15,14,13,12,11,10,9,8};
  unsigned char* crate_p = raw+dsm*16; // 16 bytes=128 bits per dsm

  if (channel % 2) {  
    // channel is odd, take highest four bits + next byte
    int k = (channel - 1) / 2 * 3 + 1; // position of low byte
    return crate_p[dsmmap[k+1]] << 4 | crate_p[dsmmap[k]] >> 4;
  }
  else {
    // channel is even, take lower byte + lowest 4 bits of next
    int k = channel / 2 * 3; // position of low byte
    return (crate_p[dsmmap[k+1]] & 0x0f) << 8 | crate_p[dsmmap[k]];
  }
}

void BemcTrigUtil::encodeBemcTP12bitToRaw8bit(int dsm, int channel, int HT_ADC, int TP_ADC, unsigned char* raw)
{
  static const int dsmmap[16] = {7,6,5,4,3,2,1,0,15,14,13,12,11,10,9,8};
  unsigned char* crate_p = raw+dsm*16; // 16 bytes=128 bits per dsm

  int triggerWord = TP_ADC << 6 | HT_ADC;

  if (channel % 2) {
    // channel is odd, take highest four bits + next byte
    int k = (channel - 1) / 2 * 3 + 1; // position of low byte
    //assert(0 <= k && k < 16);
    crate_p[dsmmap[k]]   = (triggerWord & 0x00f) << 4 | crate_p[dsmmap[k]] & 0x0f;
    crate_p[dsmmap[k+1]] = (triggerWord & 0xff0) >> 4;
  }
  else {
    // channel is even, take lower byte + lowest 4 bits of next
    int k = channel / 2 * 3;	// position of low byte
    //assert(0 <= k && k < 16);
    crate_p[dsmmap[k]]   =  triggerWord & 0x0ff;
    crate_p[dsmmap[k+1]] = (triggerWord & 0xf00) >> 8 | crate_p[dsmmap[k+1]] & 0xf0;
  }
}
