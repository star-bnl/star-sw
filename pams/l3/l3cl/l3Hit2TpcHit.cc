/*:--------------------------------------------------------------------
**: FILE:       l3Hit2TpcHit.cc
**: HISTORY:
**:--------------------------------------------------------------------*/

#include "PAM.h"
#include "l3Hit2TpcHit.h"

extern "C" long l3Hit2TpcHit_(
  TABLE_HEAD_ST     *l3GeomH,       L3GEOM_ST      *l3Geom,
  TABLE_HEAD_ST     *sl3HitH,       SL3HIT_ST      *sl3Hit,        
  TABLE_HEAD_ST     *tpHitH,        TCL_TPHIT_ST   *tpHit )
{

/*:-------------------------------------------------------------------
**: ROUTINE:    l3Hit2TpcHit_
**: DESCRIPTION: Converts l3 hits to tpchits   
**: 
**: AUTHORS:     Martin DeMello, Pablo Yepes
**: 
**: ARGUMENTS:
**:    INOUT:
**: RETURNS:    STAF Condition Value
**:-------------------------------------------------------------------*/
//
// iterate over l3 hits
//
  for (int j=0; j<sl3HitH->nok; j++) {
     int sector = sl3Hit[j].row/100;
     if ( sector < 1 || sector > 24 ) {
        printf ( " \n sector out of range %d ", sector ) ;
        tpHit[j].x = tpHit[j].y = tpHit[j].z = 0 ; 
      }
//
//    Get sector index
//
     int is = (sector == 24) ? 11 : (sector > 12) ? (23-sector) : sector - 1;
//
     int   padrow = sl3Hit[j].row%100;
     float pad    = (float)(sl3Hit[j].pad)/ 64. ;
     float time   = (float)(sl3Hit[j].time)/64. ;
//    
//  Calculate unrotated cartesian base-coordinates 
//
      /* if ( pad < 0 ) {
	printf ( "\n pad %f in row %d , out of bounds (0, %d) ", 
		 pad, padrow, NumberOfPadsInRow[padrow-1] ) ;
	pad = 0 ;
      }
      */
     int p = padrow-1; // ?????
     float yCart = l3Geom->padrowOffset[p];
     float xCart = (pad - (l3Geom->nPadsInRow[p] >> 1)+.5) * l3Geom->padSpacing[p];
//    
// rotate these coordinates 
//
     tpHit[j].id  = j + 1 ;
     tpHit[j].row = sl3Hit[j].row;
//
     tpHit[j].x = l3Geom->sinPhi[is]*xCart+l3Geom->cosPhi[is]*yCart;
     tpHit[j].y = l3Geom->sinPhi[is]*yCart-l3Geom->cosPhi[is]*xCart;
//    
//  calculate time-direction 
//
     tpHit[j].z =  l3Geom->offset + l3Geom->driftLength - time * l3Geom->timeScale;
     if (sector > 12 ) tpHit[j].z *= -1. ;
//
//   Fixed errors
//
     tpHit[j].dx = 0.1 ;
     tpHit[j].dy = 0.1 ;
     tpHit[j].dz = 0.1 ;
// 
  }

  tpHitH->nok = sl3HitH->nok ;

  return STAFCV_OK;
}









