#ifndef DSM_ALGO_EM201_2015_HH
#define DSM_ALGO_EM201_2015_HH

#include "DSMAlgo.hh"

struct DSMAlgo_EM201_2015 : public DSMAlgo {
  void operator()(DSM& dsm);
};
int ajpBarrel(const DSM& em201, int offset);
int ajpEndcap(const DSM& em201);
void getHybridJetPatchSums(const DSM& em201, int& jpSum1, int& jpSum2);
void getHTTP(const DSM &em201, int &b2b, int &nonadj);
int getEB2B(const DSM &em201);

#endif	// DSM_ALGO_EM201_2015_HH
