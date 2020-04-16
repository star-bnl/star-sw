#ifndef DSM_ALGO_EE102_2017_HH
#define DSM_ALGO_EE102_2017_HH

#include "DSMAlgo.hh"

struct DSMAlgo_EE102_2017 : public DSMAlgo {
  void operator()(DSM& dsm);
  void getEemcUpperHalfJetPatchSums(const DSM& dsm, int& jpa, int& jpb, int& jpc);
};

#endif	// DSM_ALGO_EE102_2017_HH
