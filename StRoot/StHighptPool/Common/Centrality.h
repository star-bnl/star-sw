/*
  manuel's zdc-ctb, centrality definitions

 */

#ifndef Centrality_hh
#define Centrality_hh

enum NchCentrality {kFive, kTen, kTwenty,
		    kThirty, kForty, kFifty,
		    kSixty, kSeventy, kEighty, kTotal, kNull, kNoClue};

NchCentrality centralityNch(int);

#endif
