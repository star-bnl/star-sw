#include "BEMC_DSM_decoder.h"

#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <iostream>
using namespace std;

// rawDsmL0West and rawDsmL0East are the raw DSM level-0 inputs from the TRGD bank
// HighTower and PatchSum are the arrays to store the decoded values in the triggerPatch order (0 <= triggerPatch < 300)
// See the StEmcDecoder for the conversions between triggerPatch, JetPatch and towers
int BEMC_DSM_L0_decoder(const unsigned char* rawDsmL0West, const unsigned char* rawDsmL0East, int *HighTower, int* PatchSum) {
  
  if (!rawDsmL0West && !rawDsmL0East) return 0;
  int dsm_to_patch[30] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29};
  int patch;
  int dsm_read_map[16]={7,6,5,4,3,2,1,0,15,14,13,12,11,10,9,8};
  int tower_map[10]={0,1,2,3,4,5,6,7,8,9};  // map into DSM board
  unsigned char dsmby[30][16];
  unsigned char ch[16];
  for (int i = 0;i < 30;i++) {
    for (int j = 0;j < 16;j++) {
      int k = (16 * (i % 15)) + j;
      dsmby[i][j] = (i < 15) ? (rawDsmL0East ? rawDsmL0East[k] : 0) : (rawDsmL0West ? rawDsmL0West[k] : 0);
    }
  }
  for(int i = 0;i < 30;i++) {
    patch = dsm_to_patch[i];
    for(int j = 0;j < 16;j++) {
      int k = dsm_read_map[j];
      ch[k] = dsmby[i][j];
    }
    int nt=0;
    for(int k = 0;k < 5;k++) {
      int nby = 3 * k;
      int hi_tower = (ch[nby]) & 0x3f;
      int sum_tower = ((ch[nby]>>6) & 0x3) + (((ch[nby+1]) & 0xf) << 2);
      int it =  tower_map[nt] + 10*(patch);
      if (HighTower) HighTower[it] = hi_tower;
      if (PatchSum) PatchSum[it] = sum_tower;
      nt++;
      hi_tower = ((ch[nby+1]>>4) & 0xf) + (((ch[nby+2]) & 0x3) << 4);
      sum_tower = ((ch[nby+2]>>2) & 0x3f);
      it = tower_map[nt] + 10*(patch);
      if (HighTower) HighTower[it]=hi_tower;
      if (PatchSum) PatchSum[it]= sum_tower;
      nt++;
    }
  }
  return 1;
}

// rawDsmL1 is the raw DSM level-1 input from the TRGD bank
// HighTowerBits[nDSML1Boards][nDSMInputChannels] is the HT threshold bits '00' < th0 < '01' < th1 < '10' < th2 < '11'
// PatchSum[nDSML1Boards][nDSMInputChannels] is the patch sums
// nDSML1Boards = 6, nDSMInputChannels = 6
int BEMC_DSM_L1_decoder(const unsigned short* rawDsmL1, int* HighTowerBits, int* PatchSum) {
    if (!rawDsmL1) return 0;
    int ch[] = {3, 2, 1, 0, 7, 6, 5, 4};
    for (int idsm = 0;idsm < 6;idsm++) {
	for (int ichannel = 0;ichannel < 6;ichannel++) {
	    unsigned short channelData = rawDsmL1[(idsm * 8) + ch[ichannel]];
	    if (HighTowerBits) HighTowerBits[(idsm * 6) + ichannel] = (channelData >> 10) & 0x3;
	    if (PatchSum) PatchSum[(idsm * 6) + ichannel] = channelData & (((ichannel == 2) || (ichannel == 3)) ? 0x1f : 0x3f);
	}
    }
    return 1;
}

// rawDsmL2[8] is the raw DSM level-2 input from the TRGD bank (i.e. from BC1 to EM201)
// HighTowerBits[12] is the HT threshold bits for all 12 JetPatches
// PatchSumBits[12] is the PatchSum threshold bits for all 12 JetPatches
// PatchSum[6] is the PatchSums for 6 pairs of JetPatches (JP 6+7, JP 8+9, JP 10+11, JP 0+1, JP 2+3, JP 4+5)
int BEMC_DSM_L2_decoder(const unsigned short* rawDsmL2, int* HighTowerBits, int* PatchSumBits, int* PatchSum) {
    if (!rawDsmL2) return 0;
    int ch[] = {3, 2, 1, 0, 7, 6, 5, 4};
    int JP[] = {6, 7, 8, 9, 10, 11, 0, 1, 2, 3, 4, 5};
    for (int ich = 0;ich < 6;ich++) {
	unsigned int data = rawDsmL2[ch[ich]];
	if (HighTowerBits) {
	    HighTowerBits[JP[(ich * 2) + 0]] = (data >> 12) & 0x3;
	    HighTowerBits[JP[(ich * 2) + 1]] = (data >> 14) & 0x3;
	}
	if (PatchSumBits) {
	    PatchSumBits[JP[(ich * 2) + 0]] = (data >> 8) & 0x3;
	    PatchSumBits[JP[(ich * 2) + 1]] = (data >> 10) & 0x3;
	}
	if (PatchSum) {
	    PatchSum[ich] = (data >> 0) & 0xf;
	}
    }
    return 1;
}

// rawDsmL3[8] is the raw DSM level-3 input from the TRGD bank (i.e. from EM201 to the last DSM)
// HighTowerBits[1] is the HT threshold bits for the whole BEMC
// PatchSumBits[1] is the PatchSum threshold bits for the whole BEMC
// BackToBackBit[1] is the Back-To-Back bit for BEMC
// JPsiTopoBit[1] is the J/Psi topology bit for BEMC
// JetPatchTopoBit[1] is the jet patch topology bit for BEMC or EEMC
int BEMC_DSM_L3_decoder(const unsigned short* rawDsmL3, int* HighTowerBits, int* PatchSumBits, int* BackToBackBit, int *JPsiTopoBit, int *JetPatchTopoBit) {
    if (!rawDsmL3) return 0;
    unsigned int data = rawDsmL3[0];
    if (HighTowerBits) HighTowerBits[0] = (data >> 2) & 0x3;
    if (PatchSumBits) PatchSumBits[0] = (data >> 0) & 0x3;
    if (BackToBackBit) BackToBackBit[0] = (data >> 4) & 0x1;
    if (JPsiTopoBit) JPsiTopoBit[0] = (data >> 5) & 0x1;
    if (JetPatchTopoBit) JetPatchTopoBit[0] = (data >> 6) & 0x1;    
    return 0;
}

// The function to translate the tower pedestals (in ADC counts) into FEE pedestals to be loaded into the tower crates
// It was originally written in TCL (emc.tcl)
int getFEEpedestal(float towerPedestal, float pedestalShift, bool debug) {
    /*
    set scale10bits 4
    set operationBit 1
    # operationBit == 1 means subtract (default)
    # operationBit == 0 means add
    set pedestal1 [expr $pedestal - $PedestalShift]
    if ($pedestal1<0) then {
        set pedestal1 [expr $pedestal1*(-1)]
        set operationBit 0
    }
    set operationBit [format "%1.0f" $operationBit]
    set value2 [expr $pedestal1/$scale10bits]
    set value1 [format "%3.0f" $value2]
    set value2 [expr $pedestal1 - $value1*$scale10bits]
    if ($value2>2) then {
        set value2 [expr $value1 + 1]
        set value1 [format "%3.0f" $value2]
    }
    if ($value1>15) then {
        set value3 [expr ($value1-11)/$scale10bits]
        set value3 [format "%3.0f" $value3]
        set value3 [expr $value3*$scale10bits]
        set value2 [expr $value1-$value3]
        set value1 [format "%3.0f" $value2]
    }
    if {$operationBit == 1} {set value [expr ($value1&0x0F)|(0x10)]}
    if {$operationBit == 0} {set value [expr ($value1&0x0F)]}
    return $value
    */
    char buffer[10];
    int scale10bits = 4;
    int operationBit = 1;
    double ped1 = towerPedestal - pedestalShift;
    if (ped1 < 0) {
	ped1 = -ped1;
	operationBit = 0;
    }
    double value2 = ped1 / scale10bits;
    sprintf(buffer, "%3.0f", value2);
    int value1 = atoi(buffer);
    value2 = ped1 - value1 * scale10bits;
    if (value2 > 2) {
	value2 = value1 + 1;
	sprintf(buffer, "%3.0f", value2);
	value1 = atoi(buffer);
    }
    if (value1 > 15) {
        sprintf(buffer, "%3.0f", double(value1 - 11) / scale10bits);
	int value3 = atoi(buffer);
	value3 *= scale10bits;
	value2 = value1 - value3;
	sprintf(buffer, "%3.0f", value2);
	value1 = atoi(buffer);
    }
    int value = 0;
    if (operationBit == 1) {
	value = (value1 & 0x0F) | 0x10;
    }
    if (operationBit == 0) {
	value = (value1 & 0x0F);
    }
    if (debug) cout << "Calculating FEE pedestal: pedAdc = " << towerPedestal << ", shift = " << pedestalShift << "; PED = " << value << endl;
    return value;
}

void simulateFEEaction(int adc, int ped, int bitConv, int &ht, int &pa, bool debug) {

  int operationBit = ped & 0x10;
  int pedestal = ped & 0x0F;
  int adc1 = adc >> 2;
  int adc2 = operationBit ? (adc1 - pedestal) : (adc1 + pedestal);
  int adc3 = adc2 >> 2;
  pa = adc3;
  if (bitConv == 0) {
    ht = adc2;
  } else if (bitConv == 1) {
    int adc4 = ((adc2 >> 1) & 0x1F) | ((adc2 & 0x03C0) ? 0x20 : 0);
    ht = adc4;
  } else if (bitConv == 2) {
    int adc4 = ((adc2 >> 2) & 0x1F) | ((adc2 & 0x0380) ? 0x20 : 0);
    ht = adc4;
  } else if (bitConv == 3) {
    int adc4 = ((adc2 >> 3) & 0x1F) | ((adc2 & 0x0300) ? 0x20 : 0);
    ht = adc4;
  }
 

  if (debug) cout << "Simulating FEE: adc = " << adc << ", ped = " << ped << ", bitConv = " << bitConv
		  << ";pedestal = " << pedestal << ", adc2 = " << adc2
		  << "; HT = " << ht << ", PA = " << pa << endl;
}

// This function calculates lookup table (LUT)
// Originally written in TCL (emc.tcl)
void simulateFEELUT(int sum, int formula, int lutScale, int lutPed, int lutSigma, int lutUsePowerup, int parameter4, int parameter5, int numberOfMaskedTowers, int pedestalShift, int &lut, bool debug) {
/*
proc getLUTscale { board patch } {    global lutScale lutPed lutSigma lutUseMask lutUsePowerup
    global triggermask1 triggermask2

    set patchLutScale $lutScale
    if {$lutUseMask == 1} {
        if {$patch == 0} {set triggerMask $triggermask1($board)} else {set triggerMask $triggermask2($board)}
        set numberOfMaskedChannels 0
        for {set bit 1} {$bit <= 0xffff} {set bit [expr $bit * 2]} {
            if {[expr ($triggerMask & $bit)] == 0} {incr numberOfMaskedChannels}
        }
        if {$numberOfMaskedChannels != 16} {
            set patchLutScale [expr $lutScale * ((16.0 - $numberOfMaskedChannels) / 16.0)]
        } else {
            set patchLutScale 1
        }
    }
    return $patchLutScale
}

proc getLUTped { board patch } {
    global lutScale lutPed lutSigma lutUseMask lutUsePowerup
    global triggermask1 triggermask2 PedestalShift

    set ped $lutPed([expr $patch+1],$board)
    if {$lutUsePowerup} {set ped [expr $ped + 15]}
    if {$lutUseMask == 2} {
        set triggerMask 0xffff
        if {$patch == 0} {set triggerMask $triggermask1($board)}
        if {$patch == 1} {set triggerMask $triggermask2($board)}
        set numberOfMaskedChannels 0
        for {set bit 1} {$bit <= 0xffff} {set bit [expr $bit * 2]} {
            if {[expr ($triggerMask & $bit)] == 0} {incr numberOfMaskedChannels}
        }
        set ped [expr $ped - ($numberOfMaskedChannels) * (($PedestalShift - 8) / 16)]
    }
    return $ped
}

proc getLUTrange { board patch fast } {
    global lutScale lutPed lutSigma lutUseMask lutUsePowerup
    global triggermask1 triggermask2

    set ped [getLUTped $board $patch]
    set patchLutScale [getLUTscale $board $patch]
    set range [expr $patchLutScale * ($ped + 62)]
    if {$range < 78} {set range 78}
    if {$fast == "slow"} {set range 4096}
    return $range
}

proc getLUTvalue { board patch index } {
    global lutScale lutPed lutSigma lutUseMask lutUsePowerup
    global triggermask1 triggermask2

    set ped [getLUTped $board $patch]
    set nsigma $lutSigma([expr $patch+1],$board)
    set patchLutScale [getLUTscale $board $patch]
    set value [expr ($index - $ped) / $patchLutScale]
    if {$value < 0} then {set value 0}
    if {$value > 62} then {set value 62}
    if {[expr $index - $ped] < $nsigma} then {set value 0}
    return [expr round($value)]
}
*/
    float scale = lutScale;
    if (formula == 1) {
      if (numberOfMaskedTowers != 16) {
	scale *= (16.0 - numberOfMaskedTowers) / 16.0;
      } else {
	scale = 1;
      }
    }
    float ped = lutPed;
    if (lutUsePowerup) ped += 15;
    if (formula == 2) {
      ped -= numberOfMaskedTowers * ((pedestalShift - 8.0) / 16.0);
    }
    float value = (sum - ped) / scale;
    if (value < 0) value = 0;
    if (value > 62) value = 62;
    if (sum - ped < lutSigma) value = 0;
    lut = int(round(value));
    if (debug) cout << "Simulating LUT: sum = " << sum << ", formula = " << formula << ", lutPed = " << lutPed << ", lutScale = " << lutScale << ", lutUsePowerup = " << lutUsePowerup << ", numberOfMaskedTowers = " << numberOfMaskedTowers << ", pedestalShift = " << pedestalShift << "; LUT = " << lut << endl;
}
