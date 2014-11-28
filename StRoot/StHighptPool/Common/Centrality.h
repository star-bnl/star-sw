/*
  manuel's zdc-ctb, centrality definitions

 */

#ifndef Centrality_hh
#define Centrality_hh

enum NchCentrality {kFive, kTen, kTwenty,
		    kThirty, kForty, kFifty,
		    kSixty, kSeventy, kEighty, kTotal, kNull, kNoClue};

NchCentrality centralityNch(int);

//Added this for offline centrality determination
//4-Jun-2002
int flowCentrality(int);

#endif
