/*:>-------------------------------------------------------------------
**: FILE:     FtfSl3.h 
**: HISTORY:  may 29, 1999  Frame for sl3 tracking
**:           aug 23, 1999  add setup with number of hits and tracks
**:           sep 28, 1999  cs: new input functions (pointer to
**:                         bank instead of FILE) using
**:                         daqFormats.h
**:           oct 25, 1999  ppy: use sl3CoordianteTransform header
**:           11/24/99      cle: include <L3Formats.h> intead daqFormats.h
**:                         commented out fillUSTracks
**:                         added public sectorNr variable
**:           12/03/99      ppy: sectorGeometry class added
**:                              variable sectorGeo added
**:                              clean extra includes    
**:           12/06/99      ppy: method added to check whether track can be merged
**:           01/26/00      ppy: delete rawToGlobal declaration
**:           feb 10, 2000  ppy: add xyError and zError
**:           mar 21  2000  ppy: add fill Hits
**:           apr  8  2000  ppy: add variables to cut hit based on charge and time
**:           apr 18  2000  ppy: modify readMezzannine to include rb and mz as arguments
**:           apr 19  2000  ppy: dEdx from Christof
**:           apr 19  2000  cs ppy: include FtfDedx added
**:<------------------------------------------------------------------*/
#ifndef FTFSL3
#define FTFSL3

#include "FtfFinder.h"
#include "FtfDedx.h"
//#include <SECTOR/daqFormats.h>
#include "l3GeneralHeaders.h"
#include "daqFormats.h"
#include "sl3CoordinateTransform.h"

#define checkByteOrder(byte_order)    ( (byte_order) == (DAQ_RAW_FORMAT_ORDER) ? (1) : (0) )

#ifdef SL3ROOT
#include "Rtypes.h"
#else
#define ClassDef(a,b)
#endif

class sectorGeometry {
public:
   double phiMin ;
   double phiMax ;
   double phiShift ;
   double etaMin ;
   double etaMax ;
};

class FtfSl3: public FtfFinder 
{

public:
   
   int sectorNr; 
   short     debugLevel  ;
   double    xyError ;
   double    zError ;
   int       minTimeBin ;
   int       maxTimeBin ;
   int       minClusterCharge ;
   int       maxClusterCharge ;
   //
   //  Sector phase space
   //
   sectorGeometry sectorGeo[NSECTORS] ;

   FtfSl3 (  ) { 
      debugLevel = 0 ;
      xyError    = 0.3 ;
      zError     = 1.0 ;
      minTimeBin = 0 ;
      maxTimeBin = 500 ;
//    minTimeBin =  35 ;
//    maxTimeBin = 300 ;
      minClusterCharge = 80 ;
      maxClusterCharge = 100000 ;
   };
   ~FtfSl3 ( ) {
        if ( track != 0 ) delete []track ;
        if ( hit   != 0 ) delete []hit   ;
   };
   int   fillTracks      ( int maxBytes, char* buff, unsigned int token ) ;
   int   fillHits        ( unsigned int maxBytes, char* buff, unsigned int token ) ;
   int   canItBeMerged   ( FtfTrack* thisTrack ) ;
   int   dEdx ( ) ;
   int   processSector   ( ) ;
   int   readMezzanine   ( int sector, int readOutBoard,
                           int MezzanninneNr, struct TPCMZCLD_local *mzcld );
   int   readSector      ( struct TPCSECLP *seclp ) ; 
   int   setParameters   ( ) ;
   int   setup           ( int maxHitsIn=20000, int maxTracksIn=2000 ) ;
   void  Print() const{};

   ClassDef(FtfSl3,1)

};
#endif




