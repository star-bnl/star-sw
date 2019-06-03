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


#ifndef TRG_VERSION
#define TRG_VERSION 0x32
#endif

#include "l3CoordinateTransformer.h"
#include "FtfFinder.h"
#include "FtfDedx.h"
#include "daqFormats.h"
#include "sizes.h"
#include "DAQ_READER/daqReader.h"
#include "gl3Event.h"

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

 private:   
   int sectorNr; 
   short   debugLevel  ;
   double  xyError ;
   double  zError ;

   sectorGeometry*  sectorGeo ;

 public:
   daqReader *rdr;

 public:
   int       minTimeBin ;
   int       maxTimeBin ;
   int       minClusterCharge ;
   int       maxClusterCharge ;
   int       embedded;// flag to show that the current event being read in is embedded

//
//     Constructor
//
   FtfSl3 ( l3CoordinateTransformer* inTrans, daqReader *rdr ) { 
       debugLevel = 0 ;
       xyError    = 0.3 ;
       zError     = 1.0 ;
       minTimeBin = 0 ;
       maxTimeBin = 380 ;
       minClusterCharge = 80 ;
       maxClusterCharge = 100000 ;
       coordinateTransformer = inTrans ; 
       sectorGeo = new sectorGeometry[24];
       fDedx = new FtfDedx(inTrans);
       this->rdr = rdr;
   };
   ~FtfSl3 ( ) {
       if ( track != 0 ) delete []track ;
       if ( hit   != 0 ) delete []hit   ;
       delete []sectorGeo ;
       delete fDedx;
   };

   //
   //  Coordinate Transformer classes
   //
   inline l3CoordinateTransformer* getCoordinateTransformer()
       { return (l3CoordinateTransformer*)coordinateTransformer ; } ;
   //
   //  Sector phase space
   //
   sectorGeometry* getSectorGeometry ( int n ) {
       if ( n < 1 && n > 24 ) { 
	   printf ( "FtfSl3::getSectorGeometry: wrong sector %d, returning sector 1 \n", n ) ;
	   n = 0 ;
       }
       return &(sectorGeo[n]);
   }

   //
   int   fillTracks      ( int maxBytes, char* buff, unsigned int token ) ;
   int   fillHits        ( unsigned int maxBytes, char* buff, unsigned int token ) ;
   float getXyError      ( ) { return xyError ; } ;
   float getZError       ( ) { return zError  ; } ;
   int   getNrTracks     ( ) ;

   int   canItBeMerged   ( FtfTrack* thisTrack ) ;
   int   dEdx ( ) ;
   int   processSector   ( ) ;
   //int   processData     (TPCSECLP* seclp, char* trackBuffer, int maxTrackBytes, int& nTrackBytes,
   //char* hitBuffer, int maxHitBytes, int& nHitBytes);
   int   readMezzanine   ( int sector, int readOutBoard,
                           int MezzanninneNr, struct TPCMZCLD_local *mzcld );
   int   readSector      ( struct bankHeader *bank ) ; 
   int   readSector (DATAP *datap, int sector);
   int   readSector      ( struct TPCSECLP *seclp1, struct TPCSECLP *seclp2 ); 

   int setTrackingAngles(int hypersector);
   // int readSectorFromEvpReader(int sector);
   void setClustersFromGl3Event(gl3Event *event, int sector);

   int   setParameters   ( ) ;
   void  setCoordinateTransformer ( l3CoordinateTransformer* in )
                                     { coordinateTransformer = (void *)in ; } ;
   void  setDebugLevel   ( int _debugLevel ) { debugLevel = _debugLevel ; } ;
   void  setSector       ( int _sector     ) { sectorNr   = _sector     ; } ;
   void  setXyError      ( float _xyError  ) { xyError    = _xyError    ; } ;
   void  setZError       ( float _zError   ) { zError     = _zError     ; } ;
   int   setup           ( int maxHitsIn=szSL3_default_mHits, int maxTracksIn=szSL3_default_mTracks ) ;
   void  Print() const{};
private:
   void*     coordinateTransformer ;
   FtfDedx   *fDedx;

};
#endif
