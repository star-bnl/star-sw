/*:>--------------------------------------------------------------------
**: FILE:       cpi.cc
**: HISTORY:
**:             03mar97-v0001-cpi- First STAF version     
**:             26sep97-v0002-cpi- old cpi split in two packages: cte & cpi
**:<------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sys/types.h>
#include "cpi.h"
#define M_1_PI          0.31830988618379067154

/*------------------------------------------------------------------
      Declarations of routines
------------------------------------------------------------------ */
void globtrk_pnt     ( TABLE_HEAD_ST  *globtrk_h,    EGR_GLOBTRK_ST  *globtrk ) ;

long cpi_linit    ( CPI_PARA_ST* para ) ;
void cpi_fill_hypo ( TABLE_HEAD_ST  *para_h,       CPI_PARA_ST     *para ,
                     TABLE_HEAD_ST  *globtrk_h,    EGR_GLOBTRK_ST  *globtrk ,
                     TABLE_HEAD_ST  *extra_h,      CTE_EXTRA_ST    *extra ,
                     TABLE_HEAD_ST  *hypo_pid_h,   EPI_HYPO_PID_ST *hypo_pid ) ;




#define min(a,b)        ( ( (a) < (b) ) ? (a) : (b) )
#define GLOBTRK_LEN 10000
long   globtrk_pointer [GLOBTRK_LEN] ;



long type_of_call cpi_(
  TABLE_HEAD_ST *para_h,     CPI_PARA_ST     *para ,
  TABLE_HEAD_ST *globtrk_h,  EGR_GLOBTRK_ST  *globtrk ,
  TABLE_HEAD_ST *extra_h,    CTE_EXTRA_ST    *extra ,
  TABLE_HEAD_ST *hypo_pid_h, EPI_HYPO_PID_ST *hypo_pid )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    cpi_
**: DESCRIPTION: Physics Analysis Module cpi.               
**:              It gets particle id information from TOF
**:              and global tracks
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu       
**: ARGUMENTS:
**:       IN:
**:             para_h    - header Structure for para
**:               para    - Parameters controlling module behavior
**:          globtrk_h    - header Structure for globtrk
**:            globtrk    - Global tracks
**:            extra_h    - header Structure for extra
**:              extra    - Tracks extrapolated to CTF
**:    INOUT:
**:         hypo_pid_h    - header Structure for hypo_pid
**:           hypo_pid    - Hypothesis on Particle id      
**:  
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
/*------------------------------------------------------------
           Initialize module if necessary
-------------------------------------------------------------*/
   if ( para->init == 0 ) 
      if ( cpi_linit( para ) != STAFCV_OK ) return STAFCV_BAD ;
/*-----------------------------------------------------------------
    Build globtrk pointers
------------------------------------------------------------------*/
   globtrk_pnt     ( globtrk_h,  globtrk ) ;
/*------------------------------------------------------------
           Fill hypo_pid table      
-------------------------------------------------------------*/
   cpi_fill_hypo ( para_h,  para,  globtrk_h,  globtrk, 
                   extra_h, extra, hypo_pid_h, hypo_pid ) ;
/*------------------------------------------------------------
           This is it for the moment
-------------------------------------------------------------*/
   return STAFCV_OK ;
} 
/*

            >-----------------------------------<

*/
long cpi_linit ( CPI_PARA_ST  *para )
/*:>--------------------------------------------------------------------
**: ROUTINE:    cpi_linit
**: DESCRIPTION: Initialize the CTF (TOF) particle id module 
**: 
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**: 
**:    INOUT:
**:                para   - Control cpi behavior
**:   
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
{
   if ( para->nbin_pion > 20 ) {
      printf ( "\n CPI_INIT: para->nbin_pion = %i ",para->nbin_pion ) ;
      return STAFCV_BAD ;
   }

   if ( para->nbin_kaon > 20 ) {
      printf ( "\n CPI_INIT: para->nbin_kaon = %i ",para->nbin_kaon ) ;
      return STAFCV_BAD ;
   }

   if ( para->nbin_proton > 20 ) {
      printf ( "\n CPI_INIT: para->nbin_proton = %i ",para->nbin_proton ) ;
      return STAFCV_BAD ;
   }

   para->init = 1 ;

   return STAFCV_OK ;
}
/*

            >-----------------------------------<

*/
void cpi_fill_hypo ( TABLE_HEAD_ST  *para_h,       CPI_PARA_ST     *para ,
                     TABLE_HEAD_ST  *globtrk_h,    EGR_GLOBTRK_ST  *globtrk ,
                     TABLE_HEAD_ST  *extra_h,      CTE_EXTRA_ST    *extra ,
                     TABLE_HEAD_ST  *hypo_pid_h,   EPI_HYPO_PID_ST *hypo_pid ) 
/*:>--------------------------------------------------------------------
**: ROUTINE:    cpi_fill_hypo
**: DESCRIPTION: Fills hypo_pid table with information from cte_extra
**:     
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       IN:
**:             para_h    - header Structure for para
**:               para    - Parameters controlling module behavior
**:          globtrk_h    - header Structure for globtrk
**:            globtrk    - Global tracks
**:            extra_h    - header Structure for extra
**:              extra    - Tracks extrapolated to CTF
**:    INOUT:
**:         hypo_pid_h    - header Structure for hypo_pid
**:           hypo_pid    - Hypothesis on Particle id
**:    
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
{
   long      i, id, itof, ihypo, idhypo, ihypo_max ;
   float     mass ;
   float     pion_sigma, kaon_sigma, proton_sigma  ;
   float     xs_pion, xs_kaon, xs_proton  ;

   ihypo_max = 0 ;
   for ( i=0 ; i<hypo_pid_h->nok ; i++ )
      if ( hypo_pid[i].id > ihypo_max )
         ihypo_max = hypo_pid[i].id ;
/*------------------------------------------------------------
           Set pointer in hypo
-------------------------------------------------------------*/

   ihypo = hypo_pid_h->nok - 1 ;

/*------------------------------------------------------------
           Loop over information in pid
-------------------------------------------------------------*/
   for ( itof=0 ;itof<extra_h->nok; itof++ ) {
/*------------------------------------------------------------
           Forget about tracks with no time info
-------------------------------------------------------------*/
      if ( extra[itof].beta <= 0. || extra[itof].beta > 1. ) continue ;

/*------------------------------------------------------------
           Calculate mass
-------------------------------------------------------------*/
      mass = extra[itof].p * 
             sqrt ( 1./ (extra[itof].beta*extra[itof].beta) - 1. ) ;

/*----------------------------------------------------------
       Find pion resolution for given momentum
----------------------------------------------------------*/

      for ( i=0 ; i<para->nbin_pion ; i++ )
          if ( extra[itof].p < para->p_pion[i] ) break ;
      i = min(i,para->nbin_pion-1) ;
      pion_sigma  = para->dmass_pion[i] ;

/*----------------------------------------------------------
       Calculate # sigmas and probability for pions    
----------------------------------------------------------*/
      xs_pion   = fabs(mass-para->pion_mean[i])/pion_sigma ;

/*----------------------------------------------------------
       Fill info in #sigmas < cutoff
----------------------------------------------------------*/
  
      if ( xs_pion < para->nsigma ) 
      {
         ihypo++  ;
         id  = extra[itof].id_globtrk ;
         if ( id > 0 && id <= GLOBTRK_LEN )
         {
            id = globtrk_pointer[id] ;
            if ( id >= 0 && id < GLOBTRK_LEN )
            {
               idhypo = globtrk[id].id_hypo_pid ;
               if ( idhypo == 0 )
               {
                  ihypo_max++ ;
                  globtrk[id].id_hypo_pid = ihypo_max ;
                  idhypo = ihypo_max ; 
               }
               hypo_pid[ihypo].id = idhypo  ;
            }
         }
         hypo_pid[ihypo].det = 3 ;
         if ( extra[itof].q > 0 ) 
            hypo_pid[ihypo].gid = 8 ;
         else
            hypo_pid[ihypo].gid = 9 ;
         hypo_pid[ihypo].nsigma = xs_pion ;
         hypo_pid[ihypo].prob   = 
            M_1_PI * exp(-(xs_pion*xs_pion)/2.)/sqrt(2.) ; 
         hypo_pid[ihypo].method = 0 ;
         hypo_pid[ihypo].weight = 0 ;
         hypo_pid[ihypo].id_global_pid = 0 ;
      }

/*----------------------------------------------------------
       Find kaon resolution for given momentum
----------------------------------------------------------*/

      for ( i=0 ; i<para->nbin_kaon ; i++ )
          if ( extra[itof].p < para->p_kaon[i] ) break ;
      i = min(i,para->nbin_kaon-1) ;
      kaon_sigma  = para->dmass_kaon[i] ;

/*----------------------------------------------------------
       Calculate # sigmas and probability for kaons
----------------------------------------------------------*/
      xs_kaon   = fabs(mass-para->kaon_mean[i])/kaon_sigma ;

/*----------------------------------------------------------
       Fill info in #sigmas < cutoff
----------------------------------------------------------*/
 
      if ( xs_kaon < para->nsigma )
      {
         ihypo++  ;
         id  = extra[itof].id_globtrk ;
         if ( id > 0 && id <= GLOBTRK_LEN )
         {
            id = globtrk_pointer[id] ;
            if ( id >= 0 && id < GLOBTRK_LEN )
            {
               idhypo = globtrk[id].id_hypo_pid ;
               if ( idhypo == 0 )
               {
                  ihypo_max++ ;
                  globtrk[id].id_hypo_pid = ihypo_max ;
                  idhypo = ihypo_max ;
               }
               hypo_pid[ihypo].id = idhypo  ;
            }
         }
         hypo_pid[ihypo].det = 3 ;
         if ( extra[itof].q > 0 )
            hypo_pid[ihypo].gid = 11 ;
         else
            hypo_pid[ihypo].gid = 12 ;
         hypo_pid[ihypo].nsigma = xs_kaon ;
         hypo_pid[ihypo].prob   =
            M_1_PI * exp(-(xs_kaon*xs_kaon)/2.)/sqrt(2.) ;
         hypo_pid[ihypo].method = 0 ;
         hypo_pid[ihypo].weight = 0 ;
         hypo_pid[ihypo].id_global_pid = 0 ;
      }

/*----------------------------------------------------------
       Find proton resolution for given momentum
----------------------------------------------------------*/

      for ( i=0 ; i<para->nbin_proton ; i++ )
          if ( extra[itof].p < para->p_proton[i] ) break ;
      i = min(i,para->nbin_proton-1) ;
      proton_sigma  = para->dmass_proton[i] ;

/*----------------------------------------------------------
       Calculate # sigmas and probability for protons
----------------------------------------------------------*/
      xs_proton   = fabs(mass-para->proton_mean[i])/proton_sigma ;

/*----------------------------------------------------------
       Fill info in #sigmas < cutoff
----------------------------------------------------------*/
 
      if ( xs_proton < para->nsigma )
      {
         ihypo++  ;
         id  = extra[itof].id_globtrk ;
         if ( id > 0 && id <= GLOBTRK_LEN )
         {
            id = globtrk_pointer[id] ;
            if ( id >= 0 && id < GLOBTRK_LEN )
            {
               idhypo = globtrk[id].id_hypo_pid ;
               if ( idhypo == 0 )
               {
                  ihypo_max++ ;
                  globtrk[id].id_hypo_pid = ihypo_max ;
                  idhypo = ihypo_max ;
               }
               hypo_pid[ihypo].id = idhypo  ;
            }
         }
         hypo_pid[ihypo].det = 3 ;
         if ( extra[itof].q > 0 )
            hypo_pid[ihypo].gid = 14 ;
         else
            hypo_pid[ihypo].gid = 15 ;
         hypo_pid[ihypo].nsigma = xs_proton ;
         hypo_pid[ihypo].prob   =
            M_1_PI * exp(-(xs_proton*xs_proton)/2.)/sqrt(2.) ;
         hypo_pid[ihypo].method = 0 ;
         hypo_pid[ihypo].weight = 0 ;
         hypo_pid[ihypo].id_global_pid = 0 ;
      }

/*----------------------------------------------------------
       End loop over pid
----------------------------------------------------------*/
   }
/*----------------------------------------------------------
       Set # entries in hypo_pid
----------------------------------------------------------*/
   hypo_pid_h->nok = ihypo + 1 ;

   return ;
}
/*

            >-----------------------------------<

*/
void globtrk_pnt  ( TABLE_HEAD_ST  *globtrk_h,    EGR_GLOBTRK_ST  *globtrk ) 
/*:>--------------------------------------------------------------------
**: ROUTINE:       globtrk_pnt
**: DESCRIPTION:   Fills the globtrk local pointer array
**: 
**:    INOUT:
**:          globtrk_h    - header Structure for globtrk
**:            globtrk    - Global tracks
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**:
**: RETURNS:    void
**:>------------------------------------------------------------------*/
{
   long i, id ;

/*-------------------------------------------------------------------
    Reset array
--------------------------------------------------------------------*/

   for ( i=0 ; i<GLOBTRK_LEN ; i++ )
       globtrk_pointer[i] = -1 ;

/*-------------------------------------------------------------------
    Loop globtrks
--------------------------------------------------------------------*/

   for ( i=0 ;i<globtrk_h->nok; i++ )
   {
/*------------------------------------------------------------
           Fill pointer array
-------------------------------------------------------------*/
      id = globtrk[i].id ;
      if ( id > -1 && id < GLOBTRK_LEN )
         globtrk_pointer[id] = i ;
      else
         printf ( "\n GLOBTRK_PNT:  %d index out of bounds ", id ) ;
   }
}
