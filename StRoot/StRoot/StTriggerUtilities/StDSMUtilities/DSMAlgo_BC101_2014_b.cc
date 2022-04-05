#include "DSMAlgo_BC101_2014_b.hh"

void DSMAlgo_BC101_2014_b::operator()(DSM& dsm)
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

  // (0-8) Unused
  // (9) TP threshold bit
  // (10) HT.TP threshold bit
  // (11-15) HT threshold bits

  // REGISTERS:

  // R0: BEMC-DAQ10k-HT-Sel (3)
  int r0 = dsm.registers[0];

  int upperHT = 0;
  int lowerHT = 0;

  int upperTP = 0;
  int lowerTP = 0;

  int upperHTTP = 0;
  int lowerHTTP = 0;

  for(int iichn = 0; iichn < 3; iichn++){
    lowerHT |= dsm.channels[iichn] >> 10 & 0x3f;
    lowerTP |= dsm.channels[iichn] >> 8 & 0x1;
    lowerHTTP |= dsm.channels[iichn] >> 9 & 0x1;
    
    upperHT |= dsm.channels[iichn+3] >> 10 & 0x3f;
    upperTP |= dsm.channels[iichn+3] >> 8 & 0x1;
    upperHTTP |= dsm.channels[iichn+3] >> 9 & 0x1;
  }

  int daq10kSel[6];
  int DAQ10k = 0;

  for(int ichn = 0; ichn < 6; ichn++){
    daq10kSel[ichn] = 0;
    daq10kSel[ichn] = dsm.channels[ichn] >> (10 + r0) & 0x1;
    DAQ10k |= daq10kSel[ichn];
  }

  int HT = 0;
  int TP = 0;
  int HTTP = 0;

  HT = lowerHT | upperHT;
  TP = lowerTP | upperTP;
  HTTP = lowerHTTP | upperHTTP;

  // OUTPUT (16+16):
  // To EM201
  // (0-7) Unused
  // (8) DAQ10k test bit
  // (9) TP threshold bit
  // (10) HT.TP threshold bit
  // (11-15) HT threshold bits
  // To EM202
  // (16-21) DAQ10k HT bits
  // (22-31) Unused

  int out = 0;
  out |= DAQ10k << 7;
  out |= TP << 8;
  out |= HTTP << 9;
  out |= HT << 10;

  out |= daq10kSel[0] << 16;
  out |= daq10kSel[1] << 17;
  out |= daq10kSel[2] << 18;
  out |= daq10kSel[3] << 19;
  out |= daq10kSel[4] << 20;
  out |= daq10kSel[5] << 21;

  dsm.output = out;

}
