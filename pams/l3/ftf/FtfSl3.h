/*:>-------------------------------------------------------------------
**: FILE:     ftfSl3.h 
**: HISTORY:  may 29, 1999  Frame for sl3 tracking
**:           aug 23, 1999  add setup with number of hits and tracks
**:           sep 28, 1999  cs: new input functions (pointer to
**:                         bank instead of FILE) using
**:                         daqFormats.h
**:   
**:  
**:<------------------------------------------------------------------*/
#ifndef FTFSL3
#define FTFSL3

#include "FtfFinder.h"
#include "sl3MPTrack.h"
#include "sl3USTrack.h"
#include "daqFormats.h"

#define checkByteOrder(byte_order)    ( (byte_order) == (DAQ_RAW_FORMAT_ORDER) ? (1) : (-1) )

#ifdef SL3ROOT
#include "Rtypes.h"
#else
#define ClassDef(a,b)
#endif

int rawToGlobal ( int sector, int row, double pad, double tb,
		  double &x, double &y, double &z);

class FtfSl3: public FtfFinder 
{

public:
   sl3MPTrack *sl3Track ;
   
   short     debugLevel  ;
   FtfSl3 (  ) { 
      debugLevel = 0 ;
   };
   ~FtfSl3 ( ) {
        if ( track != 0 ) delete []track ;
        if ( hit   != 0 ) delete []hit   ;
   };
   int   fillTracks      ( int maxBytes, int* buff ) ;
   int   fillUSTracks    ( int maxBytes, int* buff ) ;
   int   processSector   ( ) ;
   int   readMezzanine   ( int sector, struct TPCMZCLD_local *mzcld );
   int   readSector      ( struct TPCSECLP *seclp ) ; 
   int   setParameters   ( ) ;
   int   setup           ( int maxHitsIn=20000, int maxTracksIn=2000 ) ;
   void  Print() const{};

   ClassDef(FtfSl3,1)

};
#endif
