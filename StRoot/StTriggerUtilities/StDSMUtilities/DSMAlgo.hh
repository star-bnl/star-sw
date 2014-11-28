//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 1 Jan 2009
//

#ifndef DSM_ALGO_HH
#define DSM_ALGO_HH

struct DSM;

struct DSMAlgo {
  virtual void operator()(DSM& dsm) = 0;
  virtual ~DSMAlgo() {}
};

#endif	// DSM_ALGO_HH
