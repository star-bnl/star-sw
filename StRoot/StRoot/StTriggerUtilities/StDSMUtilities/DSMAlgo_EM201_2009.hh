//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#ifndef DSM_ALGO_EM201_2009_HH
#define DSM_ALGO_EM201_2009_HH

#include "DSMAlgo.hh"

struct DSMAlgo_EM201_2009 : public DSMAlgo {
  int ajpBarrel(const DSM& dsm, int offset) const;
  int ajpEndcap(const DSM& dsm) const;

  void operator()(DSM& dsm);
};

#endif	// DSM_ALGO_EM201_2009_HH
