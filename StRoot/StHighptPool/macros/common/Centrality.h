/*
  manuel's zdc-ctb, centrality definitions

 */

#ifndef Centrality_hh
#define Centrality_hh

enum NchCentrality {kFive, kTen, kTwenty,
		    kThirty, kForty, kFifty,
		    kSixty, kSeventy, kEighty, kTotal, KNull};

NchCentrality centrality(double zdcsum, double cdbevt);

NchCentrality centralityHMinus(int);

NchCentrality centralityNch(int);

NchCentrality centralityNchKludge(int);
#endif
