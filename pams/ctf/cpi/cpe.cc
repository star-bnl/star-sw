/*:>--------------------------------------------------------------------
**: FILE:       cpe.cc
**: HISTORY:
**:             26sep97-v000a-ppy- 
**: 
**:<------------------------------------------------------------------*/
#include <math.h>
#include "cpe.h"
#include "ctfgeo.h"
/*------------------------------------------------------------------
     Declaration variables
------------------------------------------------------------------ */
#define M_PI            3.14159265358979323846
#define MX_PHI          420
#define MX_ETA           42
#define TRACK_LEN     10000
#define EMPTY            -1
#define MX_KINE      400000
#define MX_VERT       40000
#define todeg (180./M_PI)

long       particle_pointer  [MX_KINE+1] ;
long       vertex_pointer    [MX_VERT+1] ;
long       hits_pointer[MX_PHI+1][MX_ETA+1] ;
long       mslat_pointer    [MX_PHI+1][MX_ETA+1] ;
long       extra_pointer    [TRACK_LEN] ;
long       hypo_pid_pointer [3][3][TRACK_LEN+1] ;

long   itof ;

/* Declarations of routines ... */

int   cpe_mslat_pnt       ( void  ) ;
int   cpe_hypo_pid_pnt    ( void  ) ;
int   cpe_hits_pnt        ( void  ) ;
int   cpe_kine_pnt        ( void  ) ;
int   cpe_vert_pnt        ( void  ) ;
int   cpe_extra_pnt       ( void  ) ;
void  cpe_get_mkine       ( long itof  ) ;
void  cpe_fill_eval_tof   ( long itof  ) ;
void  cpe_get_hypo_pid    ( long itof ) ;
int   eut_extra_r_        ( EGR_GLOBTRK_ST* track,
                            float *r,  float *x,  float *y, float *z,
                            float *xc, float *yc, float *rr ) ;

#define min(a,b)        ( ( (a) < (b) ) ? (a) : (b)  )
#define sgn(a)          ( ( (a) >  0. ) ? (1) : (-1) )


TABLE_HEAD_ST       *lgeo_h      ;
CTG_GEO_ST          *lgeo        ;
TABLE_HEAD_ST       *lslat_phi_h ;
CTG_SLAT_PHI_ST     *lslat_phi   ;
TABLE_HEAD_ST       *lslat_eta_h ;
CTG_SLAT_ETA_ST     *lslat_eta   ;
TABLE_HEAD_ST       *lparticle_h ;
G2T_TRACK_ST        *lparticle   ; 
TABLE_HEAD_ST       *lvertex_h   ;
G2T_VERTEX_ST       *lvertex     ; 
TABLE_HEAD_ST       *lhits_h     ;    
G2T_CTF_HIT_ST      *lhits       ;
TABLE_HEAD_ST       *lmslat_h    ;
CTS_MSLAT_ST        *lmslat      ;
TABLE_HEAD_ST       *ltrack_h    ;
EGR_GLOBTRK_ST      *ltrack      ;
TABLE_HEAD_ST       *lextra_h    ;
CTE_EXTRA_ST        *lextra      ;
TABLE_HEAD_ST       *lhypo_pid_h ;   
EPI_HYPO_PID_ST     *lhypo_pid   ;
TABLE_HEAD_ST       *leval_h     ;
CPE_EVAL_ST         *leval       ;
TABLE_HEAD_ST       *lkine_h     ;
CPE_MKINE_ST        *lkine       ; 



long type_of_call cpe_(
  TABLE_HEAD_ST            *ge_h,        CTG_GEO_ST         *ge ,
  TABLE_HEAD_ST       *slt_phi_h,   CTG_SLAT_PHI_ST         *slt_phi,
  TABLE_HEAD_ST       *slt_eta_h,   CTG_SLAT_ETA_ST         *slt_eta,
  TABLE_HEAD_ST       *prticle_h,      G2T_TRACK_ST         *prticle ,
  TABLE_HEAD_ST         *vrtex_h,     G2T_VERTEX_ST           *vrtex ,
  TABLE_HEAD_ST           *hts_h,    G2T_CTF_HIT_ST             *hts ,
  TABLE_HEAD_ST          *mslt_h,      CTS_MSLAT_ST            *mslt ,
  TABLE_HEAD_ST          *trck_h,    EGR_GLOBTRK_ST            *trck ,
  TABLE_HEAD_ST          *xtra_h,      CTE_EXTRA_ST            *xtra ,
  TABLE_HEAD_ST       *hpo_pid_h,   EPI_HYPO_PID_ST         *hpo_pid ,
  TABLE_HEAD_ST           *evl_h,       CPE_EVAL_ST             *evl ,
  TABLE_HEAD_ST           *kne_h,      CPE_MKINE_ST             *kne )
/*:>--------------------------------------------------------------------
**: ROUTINE:    cpe_
**: DESCRIPTION: Evaluates performance of CTF Particle Id module, cpi
**: 
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu      
**: ARGUMENTS:
**:       IN:
**:    INOUT:
**:               ge_h    - CTF geometry table header
**:                  ge   - CTF geometry 
**:          slt_phi_h    - Header structure for slt_phi
**:            slt_phi    - CTF slat phi info
**:          slt_eta_h    - Header structure for slt_eta
**:            slt_eta    - CTF slat eta info
**:          prticle_h    - header Structure for prticle
**:            prticle    - GEANT  produced particles
**:            vrtex_h    - header Structure for vrtex
**:              vrtex    - GEANT  vertices
**:              hts_h    - header Structure for hts
**:                hts    - CTF GEANT hits
**:             mslt_h    - header Structure for mslt
**:               mslt    - CTF slat MC information
**:             trck_h    - header Structure for trck
**:               trck    - Global tracks
**:             xtra_h    - header Structure for xtra
**:               xtra    - Global tracks extrapolated to CTF
**:          hpo_pid_h    - header Structure for hpo_pid
**:            hpo_pid    - Hypotheses for particle identification
**:              evl_h    - header Structure for evl
**:                evl    - Evaluation table
**:              kne_h    - header Structure for kine
**:                kne    - MC tracks reaching CTF
**:      OUT:
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
{
   long  i, nkine, ikine ;

   lgeo_h          = ge_h       ;
   lgeo            = ge         ;
   lslat_phi_h     = slt_phi_h  ;
   lslat_phi       = slt_phi    ;
   lslat_eta_h     = slt_eta_h  ;
   lslat_eta       = slt_eta    ;
   lparticle_h     = prticle_h  ;
   lparticle       = prticle    ;
   lvertex_h       = vrtex_h    ;
   lvertex         = vrtex      ;
   lhits_h         = hts_h      ;
   lhits           = hts        ;
   lmslat_h        = mslt_h     ;
   lmslat          = mslt       ;
   ltrack_h        = trck_h     ;
   ltrack          = trck       ;
   lextra_h        = xtra_h     ;
   lextra          = xtra       ;
   leval_h         = evl_h      ;
   leval           = evl        ;
   lkine_h         = kne_h      ;
   lkine           = kne        ;
   lhypo_pid_h     = hpo_pid_h  ;
   lhypo_pid       = hpo_pid    ;
/*-----------------------------------------------------------
     Zero eval and kine
-------------------------------------------------------------*/

   memset ( leval, 0, (int)leval_h->maxlen*sizeof(CPE_EVAL_ST) ) ;
   memset ( lkine, 0, (int)lkine_h->maxlen*sizeof(CPE_MKINE_ST) ) ;

/*------------------------------------------------------------
           Check there is something in pid
-------------------------------------------------------------*/
   if ( lextra_h->nok < 1 ) return STAFCV_OK ;
/*------------------------------------------------------------
           Set track pointers                 
-------------------------------------------------------------*/

   if ( cpe_mslat_pnt( )     != STAFCV_OK ) return STAFCV_BAD ;
   if ( cpe_hypo_pid_pnt( )  != STAFCV_OK ) return STAFCV_BAD ;
   if ( cpe_kine_pnt( )      != STAFCV_OK ) return STAFCV_BAD ;
   if ( cpe_vert_pnt( )      != STAFCV_OK ) return STAFCV_BAD ;
   if ( cpe_hits_pnt( )      != STAFCV_OK ) return STAFCV_BAD ;
   if ( cpe_extra_pnt( )       != STAFCV_OK ) return STAFCV_BAD ;

/*------------------------------------------------------------
           Loop over reconstructed tracks 
-------------------------------------------------------------*/

   for ( itof=0 ; itof<lextra_h->nok ; itof++ ) {
//------------------------------------------------------------------
//   Get info about MC track
//------------------------------------------------------------------
     cpe_get_mkine ( itof ) ;

//-------------------------------------------------------------------
//    Fill evaluation array with TOF information
//-------------------------------------------------------------------
      cpe_fill_eval_tof ( itof ) ;
//-------------------------------------------------------------------
//    Fill evaluation array hypo_pid 
//-------------------------------------------------------------------
      cpe_get_hypo_pid ( itof ) ;
   }
   leval_h->nok = ltrack_h->nok ;

/*-----------------------------------------------------------------------
      Loop over mkine and copy to tof array those particles that hit it
------------------------------------------------------------------------*/
   nkine = -1 ;
   for ( i=0 ; i<=MX_KINE ; i++ ) {
      ikine = particle_pointer[i] ;
      ikine = -ikine - 1 ;
      if ( ikine >= 0 && ikine < lparticle_h->maxlen ) {
         nkine++ ;
         if ( nkine < lkine_h->maxlen ) {
            lkine[nkine].id   = lparticle[ikine].id ; 
            lkine[nkine].ivor = lparticle[ikine].start_vertex_p  ; 
            lkine[nkine].pid  = lparticle[ikine].ge_pid ; 
            lkine[nkine].e    = lparticle[ikine].e     ; 
            lkine[nkine].p[0] = lparticle[ikine].p[0]  ; 
            lkine[nkine].p[1] = lparticle[ikine].p[1]  ; 
            lkine[nkine].p[2] = lparticle[ikine].p[2]  ; 
         }
         else
            printf ( " \n CPE_MAIN : nkine %i out of range ", nkine ) ;
      }
   } 

   lkine_h->nok = nkine + 1 ;

   return STAFCV_OK ;

}
        

void cpe_get_mkine (  long itof ) {
/*:>--------------------------------------------------------------------
**: ROUTINE:    cpe_get_kine
**: DESCRIPTION: Gets relevant information from mkine table
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       IN:
**:               itof    - index of track being considered in the extra table 
**:  
**: RETURNS:    nothing
**:>------------------------------------------------------------------*/
   long    imvor, imother ;

//------------------------------------------------------------------
//   Look at all track hits and select most frequent Pid
//------------------------------------------------------------------


   long i_phi = lextra[itof].iphi ;
   long i_eta = lextra[itof].ieta ;
   long islat =  mslat_pointer[i_phi][i_eta] ;
   if ( islat >= 0 && islat < lmslat_h->nok ) {
      long imtrk = lmslat[islat].mc_trk_id ;
      if ( imtrk >= 0 && imtrk <= MX_KINE ) {
         imtrk = abs((int)particle_pointer[imtrk]) - 1 ;
         if ( imtrk >= 0 && imtrk < lparticle_h->maxlen ) {
            leval[itof].mtrk    = lparticle[imtrk].id ;
            leval[itof].pid     = lparticle[imtrk].ge_pid;
            leval[itof].mptot   = lparticle[imtrk].ptot ;
            leval[itof].meta    = lparticle[imtrk].eta  ;
            leval[itof].mphi    = atan2 ( lparticle[imtrk].p[1],
                                          lparticle[imtrk].p[0] ) ;
            leval[itof].mvtx = lparticle[imtrk].start_vertex_p   ;
/*--------------------------------------------------------------
		Find mother
---------------------------------------------------------------------*/
            imvor = lparticle[imtrk].next_parent_p ;
            
            if ( imvor > 0 && imvor <= MX_VERT ) {
               imvor = abs((int)vertex_pointer[imvor]) - 1L ;
               if ( imvor >= 0 && imvor < lvertex_h->maxlen ) {
                  imother = lvertex[imvor].parent_p ;
                  if ( imother > 0 && imother <= MX_KINE ) {
                     imother = abs((int)particle_pointer[imother]) - 1 ;
                     if ( imother >= 0 && imother < lparticle_h->maxlen ) 
                        leval[itof].mother = lparticle[imother].ge_pid ;
                  }
               }
            }
         }
      }
   }

   return ;
}


  void cpe_fill_eval_tof ( long itof  ) {
/*:>--------------------------------------------------------------------
**: ROUTINE:    cpe_get_kine
**: DESCRIPTION: Fills eval table for tracks extrapolated to CTF 
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       IN:
**:               itof    - index of track being considered in the extra table
**:
**: RETURNS:    nothing
**:>------------------------------------------------------------------*/


  long  iphi, ieta ;
  long  ihit, islat ;
  float xp, yp, rp ;


  leval[itof].id          = lextra[itof].id      ;
  leval[itof].nrow        = lextra[itof].nrow    ;
  leval[itof].tof_id      = lextra[itof].id      ;
  leval[itof].p           = lextra[itof].p       ;
  leval[itof].q           = lextra[itof].q       ;
  leval[itof].rimp        = lextra[itof].rimp    ;
  leval[itof].zimp        = lextra[itof].zimp    ;
  leval[itof].sleng       = lextra[itof].sleng   ;
  leval[itof].x_dis       = lextra[itof].s_extra ;
  leval[itof].phi         = lextra[itof].phi     ;
  leval[itof].eta         = lextra[itof].eta     ;


  leval[itof].nslat       = lextra[itof].nslat   ;
  leval[itof].iphi        = lextra[itof].iphi    ;
  leval[itof].ieta        = lextra[itof].ieta    ;
  leval[itof].traw        = lextra[itof].traw    ;
  leval[itof].tcor        = lextra[itof].tcor    ;
  leval[itof].nmip        = lextra[itof].nmip    ;
  leval[itof].dz_lit      = lextra[itof].dz_lit  ;
  leval[itof].dz          = lextra[itof].dz      ;
  leval[itof].drphi       = lextra[itof].drphi   ;
  leval[itof].beta        = lextra[itof].beta    ;
  
  leval[itof].x_extra     = lextra[itof].x_extra ;
  leval[itof].y_extra     = lextra[itof].y_extra ;
  leval[itof].z_extra     = lextra[itof].z_extra ;

/*-------------------------------------------------------------------
      Fill evaluation array
      with hit information (last one right now)
---------------------------------------------------------------------*/
  iphi = lextra[itof].iphi ;
  ieta = lextra[itof].ieta ;

  if ( iphi > 0 && iphi <= MX_PHI &&
       ieta > 0 && ieta <= MX_ETA )
  {
     ihit =  hits_pointer[iphi][ieta] ;
     if ( ihit >= 0 && ihit < lhits_h->nok )
     {
        leval[itof].mxhit = lhits[ihit].x[0] ;
        leval[itof].myhit = lhits[ihit].x[1] ;
        leval[itof].mzhit = lhits[ihit].x[2] ;

        xp = lhits[ihit].x[0] ;
        yp = lhits[ihit].x[1] ;
        rp = sqrt ( xp * xp + yp * yp ) ;
     }
     else
        rp = 213. ;

/*-------------------------------------------------------------------
      Fill evaluation array
      with mslat information
---------------------------------------------------------------------*/
     islat =  mslat_pointer[iphi][ieta] ;
     if ( islat >= 0 && islat < lmslat_h->nok )
     {
        leval[itof].mhits     = lmslat[islat].n_hits  ;
        leval[itof].msleng    = lmslat[islat].s_length ;
        leval[itof].mtof      = lmslat[islat].tof     ;
     }
  } 
  else
     printf ( " \n CPE_MAIN : iphi %d ieta %d out of range ", iphi, ieta ) ;


  return ;
}

   void cpe_get_hypo_pid ( long itof  ) {
/*:>--------------------------------------------------------------------
**: ROUTINE:    cpe_get_hypo_pid
**: 
**: DESCRIPTION:  Fills rest if eval tables with hypo_pid info 
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       IN:
**:               itof    - index of track being considered in the extra table
**:
**: RETURNS:    nothing
**:>------------------------------------------------------------------*/

   long ipart, id, ihypo ;

/*---------------------------------------------------------------------------
     Get global id
----------------------------------------------------------------------------*/
   id          = lextra[itof].id_globtrk  ;

   if ( id > 0 && id < TRACK_LEN ) {
/*---------------------------------------------------------------------------
     Loop over detectors TPC and TOF (only for the moment)
----------------------------------------------------------------------------*/
      for ( long idet=2 ; idet<4 ; idet++ ) {
/*---------------------------------------------------------------------------
     Loop over particles
----------------------------------------------------------------------------*/
         for ( ipart=0 ; ipart<3 ; ipart++ ) {
            ihypo = hypo_pid_pointer[ipart][idet-1][id] ;
            if ( ihypo > -1 && ihypo < lhypo_pid_h->maxlen ) {
               if ( idet == 2 ) 
               {
                  leval[itof].tpc_pr[ipart] = lhypo_pid[ihypo].prob   ;
                  leval[itof].tpc_ns[ipart] = lhypo_pid[ihypo].nsigma ;
               }
               else if ( idet == 3 )
               {
                  leval[itof].tof_pr[ipart] = lhypo_pid[ihypo].prob   ;
                  leval[itof].tof_ns[ipart] = lhypo_pid[ihypo].nsigma ;
               }
            }
/*---------------------------------------------------------------------------
     End loop over particles
----------------------------------------------------------------------------*/
         }
/*---------------------------------------------------------------------------
     End loop over detectors 
----------------------------------------------------------------------------*/
      } 
   }
   else
     printf ( " \n CPE_GET_HYPO_PID : globtrk %i out of range ", id ) ;
}



   int cpe_extra_pnt ( void  ) {
/*:>--------------------------------------------------------------------
**: ROUTINE:    cpe_extra_pid
**:
**: DESCRIPTION:  Builds look-up array for extra 
**:               Gives the extra index for the corresponding track index 
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       NONE
**:
**: RETURNS:    STAF condition value
**:>------------------------------------------------------------------*/

   long i, id ;

   for ( i=0 ; i < TRACK_LEN ; i++ )
       extra_pointer[i] = -1 ;

   for ( i=0 ; i < lextra_h->nok ; i++ ) {
      id = lextra[i].id ;
      if ( id >0 && id < TRACK_LEN )
           extra_pointer[id] = i ;
      else {
         printf ( "\n CTE_TOF_PID_PNT - id exceeds array size \n" ) ;
         printf ( "\n CTE_TOF_PID_PNT - quitting \n" ) ;
         return -999 ;
      }
   }
   return STAFCV_OK ;
}


   int cpe_kine_pnt ( void  ) {
/*:>--------------------------------------------------------------------
**: ROUTINE:    cpe_kine_pid
**:
**: DESCRIPTION:  Builds look-up array for kine 
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       NONE
**:
**: RETURNS:    STAF condition value
**:>------------------------------------------------------------------*/

   long i, id ;

   for ( i=0 ; i < MX_KINE ; i++ )
       particle_pointer[i] = 0 ;

   for ( i=0 ; i<lparticle_h->nok ; i++ )
   {
      id = lparticle[i].id ;
      if ( id >0 && id < MX_KINE )
           particle_pointer[id] = i + 1 ;
      else
      {
         printf ( "\n CPE_MKINE_PNT - id exceeds array size \n" ) ;
      }
   }
   return STAFCV_OK ;
}

   int cpe_vert_pnt ( void  ) {
/*:>--------------------------------------------------------------------
**: ROUTINE:    cpe_vert_pnt
**:
**: DESCRIPTION:  Builds look-up array for vertex
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       NONE
**:
**: RETURNS:    STAF condition value
**:>------------------------------------------------------------------*/

   long i, id ;

   for ( i=0 ; i < MX_VERT ; i++ )
       vertex_pointer[i] = 0 ;

   for ( i=0 ; i<lvertex_h->nok ; i++ )
   {
      id = lvertex[i].id ;
      if ( id >0 && id < MX_VERT )
           vertex_pointer[id] = i + 1 ;
      else
      {
         printf ( "\n CPE_MVERT_PNT - id exceeds array size \n" ) ;
      }
   }
   return STAFCV_OK ;
}


   int cpe_hypo_pid_pnt ( void  ) {
/*:>--------------------------------------------------------------------
**: ROUTINE:    cpe_hypo_pid_pnt
**:
**: DESCRIPTION:  Builds look-up table for hypo_pid
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       NONE
**:
**: RETURNS:    STAF condition value
**:>------------------------------------------------------------------*/

   long i, j, k, id, idet, pid, ipid ;

   for ( i=0 ; i <= TRACK_LEN ; i++ )
      for ( j=0 ; j < 3 ; j++ )
         for ( k=0 ; k < 3 ; k++ )
            hypo_pid_pointer[k][j][i] = 0 ;

   for ( i=0 ; i< lhypo_pid_h->nok ; i++ )
   {
      id = lhypo_pid[i].id ;
      if ( id >0 && id < TRACK_LEN )
      {
/*--------------------------------------------------------------------------
        Check what detector is this (keep only svt,tpc and TOF)
---------------------------------------------------------------------------*/
         idet = lhypo_pid[i].det ;
         if ( idet > 3 || idet < 1 ) continue ;
/*--------------------------------------------------------------------------
        Check particle
---------------------------------------------------------------------------*/
         pid = lhypo_pid[i].gid ;
         if ( pid == 8 || pid == 9 ) 
            ipid = 0 ;
         else if ( pid == 11 || pid == 12 )
            ipid = 1 ;
         else if ( pid == 14 || pid == 15 )
            ipid = 2 ;

         hypo_pid_pointer[ipid][idet-1][id] = i ;
      }
      else
      {
         printf ( "\n CPE_HYPO_PID_PNT - id exceeds array size \n" ) ;
      }
   }
   return STAFCV_OK ;
}


   int cpe_hits_pnt ( void  ) {
/*:>--------------------------------------------------------------------
**: ROUTINE:    cpe_hits_pnt
**:
**: DESCRIPTION:  Builds look-up table for hits    
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       NONE
**:
**: RETURNS:    STAF condition value
**:>------------------------------------------------------------------*/

   long i, iphi, ieta, itrk ;
   float  x, y, z ;
/*-----------------------------------------------------------------------------
      Reset array to empty
-----------------------------------------------------------------------------*/
   for ( iphi = 0 ; iphi <= MX_PHI ; iphi++ )
      for ( ieta = 0 ; ieta <= MX_ETA ; ieta++ )
           hits_pointer[iphi][ieta] = EMPTY ;
/*-----------------------------------------------------------------------------
      Fill pointer array  
-----------------------------------------------------------------------------*/
   for ( i = 0 ; i < lhits_h->nok ; i++ ) {
      x     = lhits[i].x[0] ;
      y     = lhits[i].x[1] ;
      z     = lhits[i].x[2] ;
      float phi = atan2 ( y, x ) ;
      if ( phi < 0 ) phi += 2. * M_PI ;
      iphi = ctg_i_phi ( phi, lgeo_h, lgeo, lslat_phi_h, lslat_phi ) ; 
      if ( iphi > 0 && iphi <= MX_PHI ) {
         ieta = ctg_i_eta ( z, lslat_eta_h, lslat_eta ) ;
         if ( ieta > 0 && ieta <= MX_ETA ) { 
           hits_pointer[iphi][ieta] = i ;
/*-----------------------------------------------------------------------------
       Set mkine pointer negative (this means particle hit ctf
------------------------------------------------------------------------------*/
           itrk = lhits[i].track_p ;
           if ( itrk >= 1 && itrk <= MX_KINE ) {
              particle_pointer[itrk] = -1 * abs((int)particle_pointer[itrk]) ;
           }
         } 
         else
            printf ( "\n CPE_MHITS_CTF_PNT - ieta %d out of range \n", ieta ) ;
      }
      else
         printf ( "\n CPE_MHITS_CTF_PNT - iphi %d out of range \n", iphi ) ;
   }
   return STAFCV_OK ;
}


   int cpe_mslat_pnt ( void  ) {
/*:>--------------------------------------------------------------------
**: ROUTINE:    cpe_mslat_pnt
**:
**: DESCRIPTION:  Builds look-up table for mslat
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       NONE
**:
**: RETURNS:    STAF condition value
**:>------------------------------------------------------------------*/

   long i, iphi, ieta ;
/*-----------------------------------------------------------------------------
      Reset array to empty
-----------------------------------------------------------------------------*/
   for ( iphi = 0 ; iphi <= MX_PHI ; iphi++ )
      for ( ieta = 0 ; ieta <= MX_ETA ; ieta++ )
           mslat_pointer[iphi][ieta] = EMPTY ;
/*-----------------------------------------------------------------------------
      Fill pointer array
-----------------------------------------------------------------------------*/
   for ( i = 0 ; i < lmslat_h->nok ; i++ ) {
      iphi  = lmslat[i].i_phi ;
      if ( iphi > 0 && iphi <= MX_PHI )
      {
         ieta = lmslat[i].i_eta ;
         if ( ieta > 0 && ieta <= MX_ETA )
         {
           mslat_pointer[iphi][ieta] = i ;
         }
         else
            printf ( "\n CPE_MSLAT_PNT - ieta %d out of range \n", ieta ) ;
      }
      else
         printf ( "\n CPE_MSLAT_PNT - iphi %d out of range \n", iphi ) ;
   }
   return STAFCV_OK ;
}
