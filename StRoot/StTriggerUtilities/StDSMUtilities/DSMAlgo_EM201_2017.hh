#ifndef DSM_ALGO_EM201_2017_HH
#define DSM_ALGO_EM201_2017_HH

#include "DSMAlgo.hh"

struct DSMAlgo_EM201_2017 : public DSMAlgo {
  void operator()(DSM& dsm);
  void getHybridJetPatchSums(const DSM& dsm, int& jpSum1, int& jpSum2);
  void getHTTP(int httpBits[], int &b2b, int &nonadj);
  void getHybridHTTP(const DSM& dsm, int httpBits[]);

};

#endif	// DSM_ALGO_EM201_2017_HH
