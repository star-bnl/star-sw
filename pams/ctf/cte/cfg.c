/*:>--------------------------------------------------------------------
**: FILE:       cfg.c.template
**: HISTORY:
**:             00jan93-v000a-hpl- Created by stic Version
**:  Id: idl.y,v 1.8 1996/10/15 18:33:35 ward Exp  
**:<------------------------------------------------------------------*/
#include "cfg.h"

long type_of_call cfg_(
  TABLE_HEAD_ST       *tptrack_h,     TPT_TRACK_ST        *tptrack ,
  TABLE_HEAD_ST       *globtrk_h,     EGR_GLOBTRK_ST      *globtrk )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    cfg_
**: DESCRIPTION: Fills globtrk table with tptrack table  
**:  
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu      
**: ARGUMENTS:
**:       IN:
**:    INOUT:
**:             tptrack   - TPC tracks
**:             globtrk   - Global tracks
**:      OUT:
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/

/*------------------------------------------------------------
           Loop over TPC tracks and fill globtrk
-------------------------------------------------------------*/
   long counter, itrk, i ;
   counter = -1 ;

   for ( itrk=0 ;itrk<tptrack_h->nok; itrk++ ) {
/*------------------------------------------------------------
     Check track is good
------------------------------------------------------------- */
      if ( tptrack[itrk].flag < 1 ) continue ;
/*------------------------------------------------------------
     Copy information   
------------------------------------------------------------- */
      counter++ ;

      globtrk[counter].id       = itrk + 1 ;
      globtrk[counter].icharge  = tptrack[itrk].q     ;
      globtrk[counter].ndegf    = tptrack[itrk].nfit  ;
      globtrk[counter].chisq    = tptrack[itrk].chisq[0] + tptrack[itrk].chisq[1] ;
      for ( i=0 ;i<15; i++ )
         globtrk[counter].cov[i]  = tptrack[itrk].cov[i] ;
      globtrk[counter].invpt    = tptrack[itrk].invp ;
      globtrk[counter].phi0     = tptrack[itrk].phi0 ;
      globtrk[counter].psi      = tptrack[itrk].psi  ;
      globtrk[counter].r0       = tptrack[itrk].r0   ;
      globtrk[counter].tanl     = tptrack[itrk].tanl ;
      globtrk[counter].z0       = tptrack[itrk].z0   ;

      
      globtrk[counter].id_emc        = 0 ;
      globtrk[counter].id_global_pid = 0 ;
      globtrk[counter].id_hypo_pid   = 0 ;
      globtrk[counter].id_part       = 0 ;
      globtrk[counter].id_pver       = 0 ;
      globtrk[counter].id_tof        = 0 ;
      globtrk[counter].last_row      = 0 ;
      globtrk[counter].length        = 0 ;
      globtrk[counter].sflag         = 0 ;
      globtrk[counter].xlast[0]      = 0.F ;
      globtrk[counter].xlast[1]      = 0.F ;
      globtrk[counter].xlast[2]      = 0.F ;

      tptrack[itrk].id_globtrk = globtrk[counter].id ;
   }

   globtrk_h->nok = counter + 1 ;

   return STAFCV_OK;
}
