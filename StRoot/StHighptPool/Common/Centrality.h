/*
  manuel's zdc-ctb, centrality definitions

 */

#ifndef Centrality_hh
#define Centrality_hh

//4-Jun-2002 JLK
//Changed the order of these to match the Flow centrality definitions
enum NchCentrality {kEighty, kSeventy, kSixty,
		    kFifty, kForty, kThirty,
		    kTwenty, kTen, kFive, kTotal, kNull, kNoClue};

NchCentrality centralityNch(int);

#endif
