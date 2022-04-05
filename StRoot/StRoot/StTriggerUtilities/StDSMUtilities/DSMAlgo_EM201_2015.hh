#ifndef DSM_ALGO_EM201_2015_HH
#define DSM_ALGO_EM201_2015_HH

#include "DSMAlgo.hh"

struct DSMAlgo_EM201_2015 : public DSMAlgo {
  void operator()(DSM& dsm);
  int ajpBarrel(const DSM& dsm, int offset);
  int ajpEndcap(const DSM& dsm);
  void getHybridJetPatchSums(const DSM& dsm, int& jpSum1, int& jpSum2);
  void getHTTP(const DSM &dsm, int &b2b, int &nonadj);
  int getEB2B(const DSM &dsm);
};

#endif	// DSM_ALGO_EM201_2015_HH
