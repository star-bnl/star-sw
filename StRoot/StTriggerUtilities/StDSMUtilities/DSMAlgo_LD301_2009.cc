//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#include "DSM.hh"
#include "DSMAlgo_LD301_2009.hh"

void DSMAlgo_LD301_2009::operator()(DSM& dsm)
{
  // Input:
  //
  // ch0 - Unused
  //
  // ch1 - VTX information
  //       (0) BBC TAC difference in window
  //       (1) Unused
  //       (2) BBC East small-tile ADC sum over threshold 0
  //       (3) BBC West small-tile ADC sum over threshold 0
  //       (4-9) Unused
  //       (10) MTD trigger
  //       (11-15) Unused
  //
  // ch2 - Unused
  //
  // ch3 - EMC information
  //       (0-1) BEMC jet patch threshold bits
  //       (2-5) BEMC high-tower threshold bits
  //       (6) J/Psi topology bit from BEMC high-towers
  //       (7-8) EEMC jet patch threshold bits
  //       (9-12) EEMC high-tower threshold bits
  //       (13-14) BEMC+EEMC jet patch bits (0.4 < eta < 1.4)
  //       (15) BEMC adjacent jet patch bit
  //
  // ch4 - Miscellanious Information
  //       (0) Blue bunch filled
  //       (1) Yellow bunch filled
  //       (2-15) Unused
  //
  // ch5 - FPD information
  //       (0-7) to be updated
  //       (8-15) Unused
  //
  // ch6 - Special Trigger Requests
  //       (0-13) Unused
  //       (14) Zero-bias bit
  //       (15) Unused
  //
  // ch7 - Unused

  // Registers:
  //
  // R0: 2-bit integer for selecting one of the three energy sums
  // R1: 8-bit minimum bias prescale counter
  // R2: 8-bit mask for FPD1
  // R3: 8-bit mask for FPD2

  // Internal Logic:

  // VTX (ch1) & MISC (ch4)

  int mtd = dsm.channels[1] >> 10 & 0x1;

  // The minimum bias trigger bit is set when the following condition is met:
  //   BBC East small-tile DC sum over threshold 0 AND
  //   BBC West small-tile DC sum over threshold 0 AND
  //   BBC TAC difference in window AND
  //   Blue bunch filled AND
  //   Yellow bunch filled

  //int minbias = (dsm.channel(1) & 0xd) == 0xd && (dsm.channel(4) & 0x3) == 0x3;

  // *** WARNING: Force minbias condition ***

  int minbias = 1;

  // EMC (ch3)

  int emc = dsm.channels[3];

  //int BEMC_HT      = emc >> 2  & 0xf;
  //int EEMC_HT      = emc >> 9  & 0xf;

  int BEMC_JP      = emc       & 0x3;
  int JPSI_TRIGGER = emc >> 6  & 0x1;
  int EEMC_JP      = emc >> 7  & 0x3;
  int BEMC_EEMC_JP = emc >> 13 & 0x3;
  int BEMC_AJP     = emc >> 15 & 0x1;

  // Special Trigger Requests (ch6)

  int zerobias = dsm.channels[6] >> 14 & 0x1;

  // Output:
  //
  // (0) MTD trigger
  // (1) FPD1
  // (2) FPD2
  // (3) J/Psi topology trigger
  // (4-5) BEMC JP bits
  // (6-7) EEMC JP bits
  // (8-9) BEMC+EEMC JP bits (0.4 < eta < 1.4)
  // (10) BEMC AJP bit
  // (11-12) Unused
  // (13) Prescaled minimum bias trigger
  // (14) Minimum bias trigger
  // (15) Zero bias trigger
  // (16-31) Same definitions as bits 0-15

  int out = 0;

  out |= mtd;
  out |= JPSI_TRIGGER << 3;
  out |= BEMC_JP      << 4;
  out |= EEMC_JP      << 6;
  out |= BEMC_EEMC_JP << 8;
  out |= BEMC_AJP     << 10;
  out |= minbias      << 14;
  out |= zerobias     << 15;

  // Copy bits 0-15 to 16-31

  out |= out << 16;

  dsm.output = out;
}
