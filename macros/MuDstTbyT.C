#include "StMuDSTMaker/COMMON/StMuDst.h"
void MuDstTbyT(const Char_t *oldf="/gpfs02/eic/ayk/STAR/reco/MuDst/AuAu_200_production_2016/ReversedFullField/P16ij/2016/125/17125034/st_physics_adc_17125034_raw_1000007.MuDst.root", 
	       const Char_t *newf="/gpfs01/star/pwg/fisyak/Embedding/2016/piNmTsq5PerCentZ6cm/st_physics_adc_17125034_raw_1000007.MuDst.root") {
  TFile *fON[2] = {TFile::Open(oldf), TFile::Open(newf)};
  StMuDst *MuDsts[2] = {0};
  TTree *trees[2] = {0}
}
