/*:>-------------------------------------------------------------------
**: FILE:     ftfSl3.h 
**: HISTORY:  may 29, 1999  Frame for sl3 tracking
**:   
**:  
**:<------------------------------------------------------------------*/
#ifndef FTFSL3
#define FTFSL3
#include "FtfFinder.h"
#include "l3Point.h"
#include "t2l.h"
#include "sl3MPTrack.h"

class TrackHeader {
public:
   float cpuTime ;
   float realTime;
   int   nHits   ;
   int   nTracks ;
};


class FtfSl3: public FtfFinder {

public:
//   FtfFinder  tracker ;
   sl3MPTrack *sl3Track ;
   
   short     debugLevel  ;
   FtfSl3 (  ) { 
 //     FtfFinder tracker ; 
      debugLevel = 0 ;
   };
   ~FtfSl3 ( ) {
        if ( track != 0 ) delete []track ;
        if ( hit   != 0 ) delete []hit   ;
   };
   int fillTracks ( int maxBytes, int* buff ) ;
   int processSector ( ) ;
   int read ( int nHits, l3Point* hit ) ;
   int read ( int nBytes, int* buff  ) ;
   int setParameters ( ) ;
   int setup ( ) ;
};
#endif
