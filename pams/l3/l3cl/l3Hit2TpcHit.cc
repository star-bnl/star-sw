/*:--------------------------------------------------------------------
**: FILE:       l3Hit2TpcHit.cc
**: HISTORY:
**:           7/06/99  ppy initial version
**:           7/07/99  ppy Replace SL3Hit with SL3BUFFER
**:--------------------------------------------------------------------*/
#include "t2l.h"
#include "l3Point.h"
#include "l3Hit2TpcHit.h"

extern "C" long l3Hit2TpcHit_(
  TABLE_HEAD_ST     *l3GeomH,       L3GEOM_ST       *l3Geom,
  TABLE_HEAD_ST     *bufferH,       SL3BUFFER_ST    *buffer,         
  TABLE_HEAD_ST     *tpHitH,        TCL_TPHIT_ST    *tpHit )
{
/*:-------------------------------------------------------------------
**: ROUTINE:    l3Hit2TpcHit_
**: DESCRIPTION: Converts l3 hits to tpc hits 
**: 
**: AUTHORS:     Martin DeMello, Pablo Yepes
**: 
**: ARGUMENTS:
**:    INOUT:
**: RETURNS:    STAF Condition Value
**:-------------------------------------------------------------------*/
//
   l3Point globalHit ;
   int const maxHit = 15000 ;
   l3Point   hit[maxHit];
//
   int maxBytes = bufferH->nok * sizeof(SL3BUFFER_ST) ;
   int nHits = cluster2Hit ( maxBytes, (int *)buffer, maxHit, hit  ) ;
//
   if ( nHits > tpHitH->maxlen ) {
      printf ( " l3Hit2TpcHit: too many hits: nHits %d maxHits %d \n", 
                 nHits, tpHitH->maxlen ) ;  
   }
   //
   if ( l3Geom->sector < 0 && l3Geom->sector > 24 ) {
      printf ( " l3Hit2TpcHit: wrong sector %d \n", l3Geom->sector ) ;
      return STAFCV_BAD ;
   }
   int sector = l3Geom->sector ;

   int hitError = 0.2 ;
//
// iterate over l3 hits
//
   for (int j=0; j<nHits; j++) {
//
//    Copy info to global Hit class
//
     int row = tpHit[j].row%100 ;
     globalHit = hit[j].toGlobal ( sector ) ;
//
//    Go to local coordinates
//
     tpHit[j].row = globalHit.Row();
     tpHit[j].x   = globalHit.X();
     tpHit[j].y   = globalHit.Y();
     tpHit[j].z   = globalHit.Z();
     tpHit[j].dx  = hitError ;
     tpHit[j].dy  = hitError ;
     tpHit[j].dz  = hitError ;
   }
//
   tpHitH->nok = nHits  ;

   return STAFCV_OK;
}

