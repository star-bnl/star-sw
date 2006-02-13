#ifndef BEMC_DSM_decoder_H
#define BEMC_DSM_decoder_H

// rawDsmL0West and rawDsmL0East are the raw DSM level-0 inputs from the TRGD bank (i.e. from BTOW crates to BCE and BCW)
// HighTower and PatchSum are the arrays to store the decoded values in the triggerPatch order (0 <= triggerPatch < 300)
// See the StEmcDecoder for the conversions between triggerPatch, JetPatch and towers
int BEMC_DSM_L0_decoder(const unsigned char* rawDsmL0West, const unsigned char* rawDsmL0East, int *HighTower, int* PatchSum);

// rawDsmL1[48] is the raw DSM level-1 input from the TRGD bank (i.e. from BCE and BCW to BC1)
// HighTowerBits[nDSML1Boards][nDSMInputChannels] is the HT threshold bits '00' < th0 < '01' < th1 < '10' < th2 < '11'
// PatchSum[nDSML1Boards][nDSMInputChannels] is the patch sums
// nDSML1Boards = 6, nDSMInputChannels = 6
int BEMC_DSM_L1_decoder(const unsigned short* rawDsmL1, int* HighTowerBits, int* PatchSum);

// rawDsmL2[8] is the raw DSM level-2 input from the TRGD bank (i.e. from BC1 to EM201)
// HighTowerBits[12] is the HT threshold bits for all 12 JetPatches
// PatchSumBits[12] is the PatchSum threshold bits for all 12 JetPatches
// PatchSum[6] is the PatchSums for 6 pairs of JetPatches (JP 6+7, JP 8+9, JP 10+11, JP 0+1, JP 2+3, JP 4+5)
int BEMC_DSM_L2_decoder(const unsigned short* rawDsmL2, int* HighTowerBits, int* PatchSumBits, int* PatchSum);

// rawDsmL3[8] is the raw DSM level-3 input from the TRGD bank (i.e. from EM201 to the last DSM)
// HighTowerBits[1] is the HT threshold bits for the whole BEMC
// PatchSumBits[1] is the PatchSum threshold bits for the whole BEMC
// BackToBackBit[1] is the Back-To-Back bit for BEMC
// JPsiTopoBit[1] is the J/Psi topology bit for BEMC
// JetPatchTopoBit[1] is the jet patch topology bit for BEMC or EEMC
int BEMC_DSM_L3_decoder(const unsigned short* rawDsmL3, int* HighTowerBits, int* PatchSumBits, int* BackToBackBit, int *JPsiTopoBit, int *JetPatchTopoBit);

int getFEEpedestal(float towerPedestal, float pedestalShift, bool debug = false);
void simulateFEEaction(int adc, int ped, int bitConv, int &ht, int &pa, bool debug = false);
void simulateFEELUT(int sum, int formula, int parameter0, int parameter1, int parameter2, int parameter3, int parameter4, int parameter5, int numberOfMaskedTowers, int pedestalShift, int &lut, bool debug = false);

#endif
