/*:--------------------------------------------------------------------
**: FILE:       tpcHit2L3Hit.cc
**: HISTORY:
**:             7/06/99  initial versions with buffers
**:             7/07/99  Replace SL3HIT with SL3BUFFER 
**:--------------------------------------------------------------------*/

#include "t2l.h"
#include "l3Point.h"
#include "tpcHit2L3Hit.h"

extern "C" long tpcHit2L3Hit_(
  TABLE_HEAD_ST     *l3GeomH,       L3GEOM_ST       *l3Geom,
  TABLE_HEAD_ST     *tpHitH,        TCL_TPHIT_ST    *tpHit ,
  TABLE_HEAD_ST     *bufferH,       SL3BUFFER_ST    *buffer )        
{

/*:-------------------------------------------------------------------
**: ROUTINE:    tpcHit2L3Hit_
**: DESCRIPTION: Converts tpc hits to l3   
**: 
**: AUTHORS:     Martin DeMello, Pablo Yepes
**: 
**: ARGUMENTS:
**:    INOUT:
**: RETURNS:    STAF Condition Value
**:-------------------------------------------------------------------*/
//
   l3Point globalHit ;
   l3Point  *hit;
//
   int maxClusters = tpHitH->nok ;
   if ( maxClusters < 2 ) {
      printf ( " tpcHit2L3Hit: too few hits %d\n", maxClusters ) ;
      return STAFCV_BAD ;
   }
//
   hit = new l3Point[maxClusters] ;
//
// iterate over l3 hits
//
  int nHits = 0 ;
  for (int j=0; j<tpHitH->nok; j++) {
//
//    Copy info to global Hit class
//
     int row = tpHit[j].row%100 ;
     globalHit.set ( row, tpHit[j].x, tpHit[j].y, tpHit[j].z ) ;
//
//    Go to local coordinates
//
     int sector = tpHit[j].row/100;
     if ( sector < 1 || sector > 24 ) {
        printf ( " \n sector out of range %d ", sector ) ;
        continue ; 
      }
      hit[nHits] = globalHit.toLocal(sector);
      nHits++ ;
  }
//
//    Fill online buffer now
//
   int maxBytes = bufferH->maxlen*sizeof(SL3BUFFER_ST) ; 
//
   int nBytes = hit2Cluster ( nHits, hit, maxBytes, (int *)buffer ) ;

   bufferH->nok = nBytes  ;
   delete []hit ;

   return STAFCV_OK;
}

