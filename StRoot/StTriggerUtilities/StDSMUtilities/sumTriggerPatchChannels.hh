//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#ifndef SUM_TRIGGER_PATCH_CHANNELS_HH
#define SUM_TRIGGER_PATCH_CHANNELS_HH

class DSM;

void sumTriggerPatchChannels(const DSM& dsm, int chMin, int chMax, int step, int targetPedestal, int& sum, int& highTowerBits);

#endif	// SUM_TRIGGER_PATCH_CHANNELS_HH
