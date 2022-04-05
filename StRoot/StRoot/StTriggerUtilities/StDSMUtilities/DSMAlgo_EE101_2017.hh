#ifndef DSM_ALGO_EE101_2017_HH
#define DSM_ALGO_EE101_2017_HH

#include "DSMAlgo.hh"

struct DSMAlgo_EE101_2017 : public DSMAlgo {
  void operator()(DSM& dsm);
  void getEemcLowerHalfJetPatchSums(const DSM& dsm, int& jpa, int& jpb, int& jpc);
};

#endif	// DSM_ALGO_EE101_2017_HH
