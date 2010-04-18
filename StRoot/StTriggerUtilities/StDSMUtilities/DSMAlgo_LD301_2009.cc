//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 14 Mar 2009
//

#include "DSM.hh"
#include "DSMAlgo_LD301_2009.hh"

void DSMAlgo_LD301_2009::operator()(DSM& dsm)
{
  // INPUT:

  // ch0 - MTD information
  //       Bit 0 - MTD trigger (MTD)
  //       Bits 1:15 - Unused

  // ch1 - VTX information
  //       Bit 0 - BBC TAC difference in window (BBC-TAC)
  //       Bit 1 - BBC East ADC sum > threshold (BBCE)
  //       Bit 2 - BBC West ADC sum > threshold (BBCW)
  //       Bits 3:5 - Unused
  //       Bit 6 - ZDC TAC difference in window (ZDC-TAC)
  //       Bit 7 - ZDC East ADC sum > threshold (ZDCE)
  //       Bit 8 - ZDC West ADC sum > threshold (ZDCW)
  //       Bit 9 - ZDC East Front ADC > threshold (ZDCE-Front)
  //       Bit 10 - ZDC East Back ADC > threshold (ZDCE-Back)
  //       Bit 11 - ZDC West Front ADC > threshold (ZDCW-Front)
  //       Bit 12 - ZDC West Back ADC > threshold (ZDCW-Back)
  //       Bit 13 - VPD TAC difference in window (VPD-TAC)
  //       Bit 14 - VPD East ADC sum > threshold (VPDE)
  //       Bit 15 - VPD West ADC sum > threshold (VPDW)

  // ch2 - Unused

  // ch3 - EMC information
  //       Bits 0:3 - Barrel HT bits (BHT(0:3))
  //       Bits 4:5 - Endcap HT bits (EHT2 and EHT4)
  //       Bit 6 - Barrel+Endcap jet patch 1 (JP1)
  //       Bit 7 - Barrel+Endcap jet patch 2 (JP2)
  //       Bit 8 - Barrel-only jet patch 1 (BJP1)
  //       Bit 9 - Unused
  //       Bit 10 - Endcap-only jet patch 1 (EJP1)
  //       Bit 11 - Unused
  //       Bit 12 - Barrel+Endcap adjacent jet patch (AJP)
  //       Bits 13:15 - Unused

  // ch4 - RAT board
  //       Bit 0 - FMS led (FMS-led)
  //       Bits 1:7 - Unused
  //       Bits 8:15 - Detector status [1=Live, 0=Busy] (status(0:7))

  // ch5 - FMS/FPD information
  //       Bits 0:2 - FMS small-cell cluster threshold bits (FMS-small(0:2))
  //       Bits 3:6 - Unused
  //       Bits 7:9 - FMS large-cell cluster threshold bits (FMS-large(0:2))
  //       Bits 10:13 - Unused
  //       Bit 14 - FPE trigger (FPE)
  //       Bit 15 - Unused

  // ch6 - Special Trigger Requests
  //       Bits 0:13 - Unused
  //       Bit 14 - Zero-bias bit (Zerobias)
  //       Bit 15 - Unused

  // ch7 - Unused

  // REGISTERS:

  // R0 - BBCMBLive-PS-lo (12)
  // R1 - BBCMBLive-PS-hi (12)
  // R2 - VPDMBLive-PS-lo (12)
  // R3 - VPDMBLive-PS-hi (12)
  // R4 - ZDCMB-PS-lo (12)
  // R5 - ZDCMB-PS-hi (12)
  // R6 - BBC-Live-Det-Select (8)
  // R7 - VPD-Live-Det-select (8)
  // R8 - Output-Bit1-Select (3)
  // R9 - Output-Bit2-Select (4)
  // R10 - JP1-Select (4)

  // INTERNAL LOGIC:

  int bbctac = dsm.channels[1] & 0x1;
  int bbce = dsm.channels[1] >> 1 & 0x1;
  int bbcw = dsm.channels[1] >> 2 & 0x1;
  int bbcmb = bbctac && bbce && bbcw;

  int bht3 = dsm.channels[3] >> 3 & 0x1;
  int bht2 = dsm.channels[3] >> 2 & 0x1;
  int bjp1 = dsm.channels[3] >> 8 & 0x1;
  int bit1 = ((bht3 &&          (dsm.registers[8] & 0x1)) ||
	      (bht2 && bjp1  && (dsm.registers[8] & 0x2)) ||
	      (bht2 && bbcmb && (dsm.registers[8] & 0x4)));

  int jp2 = dsm.channels[3] >> 7 & 0x1;
  int ajp = dsm.channels[3] >> 12 & 0x1;
  int eht4 = dsm.channels[3] >> 5 & 0x1;
  int eht2 = dsm.channels[3] >> 4 & 0x1;
  int ejp1 = dsm.channels[3] >> 10 & 0x1;
  int bit2 = ((jp2  &&         (dsm.registers[9] & 0x1)) ||
	      (ajp  &&         (dsm.registers[9] & 0x2)) ||
	      (eht4 &&         (dsm.registers[9] & 0x4)) ||
	      (eht2 && ejp1 && (dsm.registers[9] & 0x8)));

  int bht = 0;
  switch (dsm.channels[3] & 0x7) {
  case 1: bht = 1; break;
  case 3: bht = 2; break;
  case 7: bht = 3; break;
  }

  int jp1 = dsm.channels[3] >> 6 & 0x1;
  int overlap_jp1 = jp1 && !(bjp1 || ejp1);
  int jp1_selected = ((jp1         && (dsm.registers[10] & 0x1)) ||
		      (bjp1        && (dsm.registers[10] & 0x2)) ||
		      (ejp1        && (dsm.registers[10] & 0x4)) ||
		      (overlap_jp1 && (dsm.registers[10] & 0x8)));

  int status = dsm.channels[4] >> 8 & 0xff;
  int bbcdetlive = (((status & dsm.registers[6]) | ~dsm.registers[6]) & 0xff) == 0xff;
  int bbcmblive = bbcmb && bbcdetlive;
  int bbcpre = dsm.registers[0] | dsm.registers[1] << 12;
  if (bbcmblive) --bbcpre;
  int bbcmblivepre = bbcpre == 1;

  int vpdtac = dsm.channels[1] >> 13 & 0x1;
  int vpde = dsm.channels[1] >> 14 & 0x1;
  int vpdw = dsm.channels[1] >> 15 & 0x1;
  int vpdmb = vpdtac && vpde && vpdw;
  int vpddetlive = (((status & dsm.registers[7]) | ~dsm.registers[7]) & 0xff) == 0xff;
  int vpdmblive = vpdmb && vpddetlive;
  int vpdpre = dsm.registers[2] | dsm.registers[3] << 12;
  if (vpdmblive) --vpdpre;
  int vpdmblivepre = vpdpre == 1;

  int zdctac = dsm.channels[1] >> 6 & 0x1;
  int zdce = dsm.channels[1] >> 7 & 0x1;
  int zdcw = dsm.channels[1] >> 8 & 0x1;
  int zdcmb = zdctac && zdce && zdcw;

  int zdcefront = dsm.channels[1] >>  9 & 0x1;
  int zdceback  = dsm.channels[1] >> 10 & 0x1;
  int zdcwfront = dsm.channels[1] >> 11 & 0x1;
  int zdcwback  = dsm.channels[1] >> 12 & 0x1;
  int zdcpole = bbce && bbcw && zdcefront && zdceback;
  int zdcpolw = bbce && bbcw && zdcwfront && zdcwback;
  int zdcpol = zdcpole || zdcpolw;

  int mtd = dsm.channels[0] & 0x1;

  int fmssmall1 = dsm.channels[5] >> 1 & 0x1;
  int fmslarge1 = dsm.channels[5] >> 8 & 0x1;
  int fmssmall2 = dsm.channels[5] >> 2 & 0x1;
  int fmslarge2 = dsm.channels[5] >> 9 & 0x1;
  int fmsfast = fmssmall1 && fmslarge1;
  int fmsslow = fmssmall2 && fmslarge2;
  int fmsled = dsm.channels[4] & 0x1;
  int fpe = dsm.channels[5] >> 14 & 0x1;
  int fmsledfpe = fmsled || fpe;

  int zerobias = dsm.channels[6] >> 14 & 0x1;

  // OUTPUT:

  // Bit 0 - BIT1
  // Bit 1 - BIT2
  // Bit 2 - BHT-0
  // Bit 3 - BHT-1
  // Bit 4 - VPDMB
  // Bit 5 - EHT2
  // Bit 6 - JP1-selected
  // Bit 7 - BBCMBLive-pre
  // Bit 8 - VPDMBLive-pre
  // Bit 9 - ZDCMB-pre
  // Bit 10 - ZDCpol
  // Bit 11 - MTD
  // Bit 12 - FMSfast
  // Bit 13 - FMSslow
  // Bit 14 - FMSled-FPE
  // Bit 15 - Zerobias
  // Bits 16:31 - Same definitions as bits 0:15

  int out = 0;

  out |= bit1;
  out |= bit2 << 1;
  out |= bht << 2;
  out |= vpdmb << 4;
  out |= eht2 << 5;
  out |= jp1_selected << 6;
  out |= bbcmblivepre << 7;
  out |= vpdmblivepre << 8;
  out |= zdcmb << 9;
  out |= zdcpol << 10;
  out |= mtd << 11;
  out |= fmsfast << 12;
  out |= fmsslow << 13;
  out |= fmsledfpe << 14;
  out |= zerobias << 15;

  // Copy bits 0:15 to bits 16:31

  out |= out << 16;

  dsm.output = out;
}
