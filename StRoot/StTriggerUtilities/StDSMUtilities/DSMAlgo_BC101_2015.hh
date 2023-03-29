#ifndef DSM_ALGO_BC101_2015_HH
#define DSM_ALGO_BC101_2015_HH

#include "DSMAlgo.hh"
#include "DSM.hh"

struct DSMAlgo_BC101_2015 : public DSMAlgo {
  void operator()(DSM& dsm);
};
void getBemcJetPatchSums2015A(const DSM& bc101, int& jpx, int& jpy, int& jpz, int& jpPartial, int& highTowerBits);
#endif	// DSM_ALGO_BC101_2015_HH
