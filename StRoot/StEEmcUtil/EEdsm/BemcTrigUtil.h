//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 4 March 2007
//
// BEMC utility class to decode/encode 12-bit DSM ADC from the raw trigger structure.
// The lower 6 bits are the ADC for the high tower (HT) and the upper 6 bits are the ADC
// for the sum of all towers in the trigger patch (TP).
//
// Usage:
//
// For patchId in [0, 300[:
//   dsm     = patchId / 10
//   channel = patchId % 10
// If dsm >= 15, then
//   dsm = dsm - 15
//   raw = trgData->rawTriggerDet[0].BEMCEast
// else
//   raw = trgData->rawTriggerDet[0].BEMCWest
//
// where trgData is a pointer to TrgDataType in $STAR/StDaqLib/TRG/trgStructures.h
// If adc is the 12-bit ADC returned by the decoder, then
//
// HT_ADC = adc & 0x3f;
// TP_ADC = adc >> 6;
//
// The encoder works in the reverse direction in that it takes HT_ADC and TP_ADC and
// stores them in trgData->rawTriggerDet[0].BEMCEast or trgData->rawTriggerDet[0].BEMCWest
// depending on the dsm and channel.
//

#ifndef BEMC_TRIG_UTIL_HH
#define BEMC_TRIG_UTIL_HH

class BemcTrigUtil {
public:
  static  int decodeBemcTP12bitToRaw8bit(int dsm, int channel, unsigned char* raw);
  static void encodeBemcTP12bitToRaw8bit(int dsm, int channel, int HT_ADC, int TP_ADC, unsigned char* raw);
};

#endif // BEMC_TRIG_UTIL_HH
