/*:>--------------------------------------------------------------------
**: FILE:       cte.cc
**: HISTORY:
**:             09sep97-v0001-cte- First STAF version     
**:<------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "cte.h"
#include "ctfgeo.h"

/*------------------------------------------------------------------
      Declarations of routines
------------------------------------------------------------------ */

long  cte_extra       ( void ) ;
long  cte_linit       ( CTG_GEO_ST* geo, CTE_PARA_ST* para ) ;

long  cte_select_slat ( CTE_EXTRA_ST* l_extra, EGR_GLOBTRK_ST* track ) ;

void  cte_cor_time    ( float  z, long iphi, long ieta,
                        float& tcor, float& dz  ) ;
void cte_slat_distance( float phi ,  float z,
                        long  iphi,  long ieta,
                        float& drphi, float& dz, float& dis ) ;

void  ctf_cor_pnt     ( void ) ;
long cte_is_empty     (  long iphi, long ieta ) ;

//
//     Extrapolation routines
//
extern "C" int   eut_closest_    ( float b_field, EGR_GLOBTRK_ST *track, 
                                   float *xp, float *yp, float *zp ) ;
extern "C" int   eut_extra_r_cyl_( float b_field, EGR_GLOBTRK_ST *ttr,
                                   float *r,  float *phi, float *z,
                                   float *xc, float *yc, float *rr ) ;
extern "C" float eut_length_ ( float b_field, EGR_GLOBTRK_ST* track,
                    float x1, float y1, float x2, float y2 ) ;

//
//   Tools routine to output messages
//
extern "C" void    MessageOut( const char *msg );


/*------------------------------------------------------------------
     Declaration variableS
------------------------------------------------------------------ */
#define M_PI            3.14159265358979323846
#define MX_PHI        420
#define MX_ETA         42
#define CTF_SLAT_LEN 7000
long    ctf_cor_pointer [CTF_SLAT_LEN] ;


float   angular_tolerance        ;
char    OutMessage[50] ;

#define min(a,b)        ( ( (a) < (b) ) ? (a) : (b) )
#define max(a,b)        ( ( (a) > (b) ) ? (a) : (b) )
#define sgn(a)          ( ( (a) >  0  ) ? (1) : (-1) )
#define square(a)       ( (a) * (a) )


TABLE_HEAD_ST           *geo_h  ; 
CTG_GEO_ST              *geo    ; 
TABLE_HEAD_ST           *slat_h ;
CTG_SLAT_ST             *slat   ;
TABLE_HEAD_ST           *slat_eta_h ;
CTG_SLAT_ETA_ST         *slat_eta   ;
TABLE_HEAD_ST           *slat_phi_h ;
CTG_SLAT_PHI_ST         *slat_phi   ;
TABLE_HEAD_ST           *cor_h  ;
CTU_COR_ST              *cor    ;
TABLE_HEAD_ST           *globtrk_h;
EGR_GLOBTRK_ST          *globtrk ;
TABLE_HEAD_ST           *para_h  ;
CTE_PARA_ST             *para    ;
TABLE_HEAD_ST           *extra_h ;
CTE_EXTRA_ST            *extra   ;


extern "C" long type_of_call cte_(
  TABLE_HEAD_ST            *ge_h,        CTG_GEO_ST             *ge,
  TABLE_HEAD_ST           *slt_h,       CTG_SLAT_ST             *slt ,
  TABLE_HEAD_ST       *slt_eta_h,   CTG_SLAT_ETA_ST         *slt_eta ,
  TABLE_HEAD_ST       *slt_phi_h,   CTG_SLAT_PHI_ST         *slt_phi ,
  TABLE_HEAD_ST            *cr_h,        CTU_COR_ST              *cr ,
  TABLE_HEAD_ST        *glbtrk_h,    EGR_GLOBTRK_ST          *glbtrk ,
  TABLE_HEAD_ST           *pra_h,       CTE_PARA_ST             *pra ,
  TABLE_HEAD_ST          *xtra_h,      CTE_EXTRA_ST            *xtra ) 
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    cte_
**: DESCRIPTION: Extrapolates global tracks to CTF          
**:              Tries to match each tracks with a slat with signal
**:              If time info available calculated corrected time
**:            
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu       
**: ARGUMENTS:
**:       IN:
**:              geo_h    - header Structure for geo
**:                geo    - CTF geometry 
**:             slat_h    - header Structure for slat
**:               slat    - Slat or counter information
**:              cor_h    - header Structure for cor
**:                cor    - Corrected CTF data
**:          globtrk_h    - header Structure for globtrk
**:            globtrk    - Global tracks
**:    INOUT:
**:             para_h    - header Structure for para
**:               para    - Parameters controlling module behavior
**:            extra_h    - header Structure for extra
**:              extra    - Tracks extrapolated and matched to CTF
**: 
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
geo_h      = ge_h  ;
geo        = ge    ;
slat_h     = slt_h ;
slat       = slt   ;
slat_eta_h = slt_eta_h ;
slat_eta   = slt_eta   ;
slat_phi_h = slt_phi_h ;
slat_phi   = slt_phi   ;
cor_h      = cr_h  ;
cor        = cr    ;
globtrk_h  = glbtrk_h ;
globtrk    = glbtrk   ;
para_h     = pra_h ;
para       = pra   ;
extra_h    = xtra_h  ;
extra      = xtra  ;
/*------------------------------------------------------------
           Check CTF geometry is initialized
-------------------------------------------------------------*/
   if ( geo->init == 0 ) {
      MessageOut ( "\n CTE: CTF geometry not initialized " ) ;
      return STAFCV_BAD ;
   }
/*------------------------------------------------------------
           Initialize module if necessary
-------------------------------------------------------------*/
   if ( para->init == 0 ) 
      if ( cte_linit( geo, para ) != STAFCV_OK ) return STAFCV_BAD ;

/*------------------------------------------------------------
           Fill pointer arrays
-------------------------------------------------------------*/
   ctf_cor_pnt() ;
/*------------------------------------------------------------
           Extrapolate tracks to TOF
-------------------------------------------------------------*/
   if ( cte_extra() ) return STAFCV_BAD ;
/*------------------------------------------------------------
           This is it for the moment
-------------------------------------------------------------*/
   return STAFCV_OK ;
} 
/*

            >-----------------------------------<


*/
void ctf_cor_pnt ( void )
/*:>--------------------------------------------------------------------
**: ROUTINE:       cte_cor_pnt 
**: DESCRIPTION:   Fills the CTF local pointer array
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**:
**: RETURNS:    void
**:>------------------------------------------------------------------*/
{
   long iphi, ieta, n_eta, index ;

/*-------------------------------------------------------------------
    Loop over CTF slats
--------------------------------------------------------------------*/
   for ( long i=0 ; i<slat_h->maxlen ; i++ )
       ctf_cor_pointer[i] = -1 ;

/*-------------------------------------------------------------------
    Loop over CTF slats
--------------------------------------------------------------------*/

   n_eta = geo->n_counter_eta * geo->n_tray_eta ;
   for ( i=0 ;i<cor_h->nok; i++ ) {
/*------------------------------------------------------------
           Fill pointer array
-------------------------------------------------------------*/

      iphi = cor[i].i_phi ;      
      ieta = cor[i].i_eta ;
      index = ctg_index ( iphi, ieta, n_eta ) ;

      if ( index >= 0 && index < CTF_SLAT_LEN )
         ctf_cor_pointer[index] = i ;
      else {
         sprintf ( OutMessage, "\n CTE:  %d index out of bounds ", index ) ;
         MessageOut ( OutMessage ) ;
      }
   }
}
/* ********************************************************************

        End ctf_cor_pnt

**********************************************************************/
/*

            >-----------------------------------<


*/

void cte_cor_time (  float z, long iphi, long ieta, float& tcor, float& dz )
/*:>--------------------------------------------------------------------
**: ROUTINE:     cte_cor_time
**: DESCRIPTION: Provides corrected time and distance to PMT
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@physics.rice.edu
**: ARGUMENTS:
**:          IN:
**:             z               - Hit z position
**:             iphi, ieta      - Slat indexes
**:         OUT:
**:             tcor            - Corrected time
**:             dz              - Distance from impact (z) to PMT
**:
**: RETURNS:    void
**:>------------------------------------------------------------------*/
{

   tcor = dz = 0.F ;
   long n_eta = geo->n_counter_eta * geo->n_tray_eta ;
   long index = ctg_index ( iphi, ieta, n_eta ) ;
   if ( index < 0 || index >= CTF_SLAT_LEN ) {
      sprintf ( OutMessage, "\n CTE_COR_TIME: Crazy index = %d ",index ) ;
      MessageOut ( OutMessage ) ;
      return ;
   }
//
   long islat = ctf_cor_pointer[index]   ;
//
   if ( islat < 0 )  return       ;
//
//     Check time has a reasonable value
//
   if ( cor[islat].time < 0 ) return ;
//
//   If there is signal correct time
//
   float zedge ;
   if ( slat_eta[ieta-1].z > 0 )
      zedge = slat_eta[ieta-1].z + geo->tray_length/geo->n_counter_eta ;
   else
      zedge = slat_eta[ieta-1].z - geo->tray_length/geo->n_counter_eta ;

   dz     = sgn(zedge) * ( zedge - z ) ;
   if ( dz > 0 )
      tcor   = cor[islat].time - para->delay * dz ;
   else
      tcor   = cor[islat].time ;
}
/*

            >-----------------------------------<

*/
long cte_extra ( void ) 
/*:>--------------------------------------------------------------------
**: ROUTINE:       cte_extra   
**: DESCRIPTION:   Extrapolates tracks to CTF and match them with the
**:                corresponding slats
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**:
**: RETURNS:    void
**:>------------------------------------------------------------------*/
{
   long  itrack   ;
   long  islat, index, nctf_tracks ;
   float xc, yc, rr      ; /* center and radius track circle in x-y */
   float xp, yp, zp, rp  ; /* information point closest approach to origin */
   float phih_1, zh_1       ; /* track hit in inner most TOF edge */
   float phih_2, zh_2       ; /* track hit in outer most TOF edge */
   float r_plus ;

   long n_eta = geo->n_counter_eta * geo->n_tray_eta ;

/*------------------------------------------------------------
           Set counters to zero   
-------------------------------------------------------------*/
   nctf_tracks = -1 ;
/*------------------------------------------------------------
           Loop over found tracks
-------------------------------------------------------------*/

   for ( itrack=0 ;itrack<globtrk_h->nok; itrack++ ) {
/*------------------------------------------------------------
           Extrapolates to TOF     
           inner most edge
-------------------------------------------------------------*/
      if ( eut_extra_r_cyl_ ( para->b_field, &globtrk[itrack], &(geo->r),
                              &phih_1, &zh_1, &xc, &yc, &rr ) !=0 )
         continue ;

/*------------------------------------------------------------
           Extrapolates to TOF
           outer most edge
-------------------------------------------------------------*/
      r_plus = geo->r + geo->counter_thickness ;
      if ( eut_extra_r_cyl_ ( para->b_field, &globtrk[itrack], &r_plus,
                              &phih_2, &zh_2, &xc, &yc, &rr ) !=0 )
         continue ;

/*----------------------------------------------------------
       If track outside acceptance forget about it
----------------------------------------------------------*/

      if ( fabs(zh_1) > 2. * geo->tray_length ) 
           zh_1 = sgn(zh_1) * 2. * geo->tray_length ;

      if ( fabs(zh_2) > 2. * geo->tray_length ) 
           zh_2 = sgn(zh_2) * 2. * geo->tray_length ;

      if ( fabs(zh_1) >= 2. * geo->tray_length &&
           fabs(zh_2) >= 2. * geo->tray_length ) continue ;
       
/*-------------------------------------------------------------------
       Increment counter
--------------------------------------------------------------------*/
      nctf_tracks++ ;
      if ( nctf_tracks > extra_h->maxlen ) {
         sprintf ( OutMessage, "\n CTE_EXTRA: cte_extra too short, maxlen %d ",
                               extra_h->maxlen ) ;
         MessageOut ( OutMessage ) ;
         return STAFCV_BAD ;
      }

      CTE_EXTRA_ST* l_extra = &extra[nctf_tracks] ;
/*----------------------------------------------------------
       Get mean hit and errors
----------------------------------------------------------*/
      l_extra->phi_extra   =      ( phih_1 + phih_2 ) / 2. ;
      l_extra->e_phi_extra = fabs ( phih_1 - phih_2 ) / 2. ;

      l_extra->z_extra     =      ( zh_1 + zh_2 ) / 2. ;
      l_extra->e_z_extra   = fabs ( zh_1 - zh_2 ) / 2. ;
//
//     Store some info in extra(polation) table
//   
      l_extra->id          = nctf_tracks+1 ;
      l_extra->id_globtrk  = globtrk[itrack].id ;
      l_extra->q           = globtrk[itrack].icharge  ;
      l_extra->nrow        = globtrk[itrack].ndegf ;
//
//     Try to get a slat corresponding to this track
//
      if ( cte_select_slat ( l_extra, &globtrk[itrack] ) ) {
         nctf_tracks-- ;
         continue ;
      }
//
//     Get point closest approach to detector center
//     and check origin of the track is reasonable
//
      long nok = eut_closest_ ( para->b_field, &globtrk[itrack], &xp, &yp, &zp ) ;
      rp = sqrt ( xp * xp + yp * yp ) ;
//
      if ( nok == 0 || rp > para->rmax || fabs(zp) > para->zmax ) {
         nctf_tracks-- ;
         continue ;
      }
//
      l_extra->rimp        = rp    ;
      l_extra->zimp        = zp    ;
//
//     Get angle between origin and TOF hit
// 
      l_extra->x_extra = geo->r * cos ( l_extra->phi_extra ) ;
      l_extra->y_extra = geo->r * sin ( l_extra->phi_extra ) ;
//--------------------------------------------------------
//     Get total trajectory length
//--------------------------------------------------------
      l_extra->sleng = eut_length_ ( para->b_field, &(globtrk[itrack]),
                                     xp, yp, l_extra->x_extra, l_extra->y_extra ) ;
//--------------------------------------------------------
//     Get extrapolation length
//-------------------------------------------------------
      l_extra->s_extra = eut_length_ ( para->b_field, &(globtrk[itrack]),
                                       globtrk[itrack].xlast[0], globtrk[itrack].xlast[1], 
                                       l_extra->x_extra, l_extra->y_extra ) ;
/*-------------------------------------------------------
       Get  phi and eta
---------------------------------------------------------*/
      l_extra->phi       = atan2(yp-yc,xp-xc) - 0.5 * sgn(globtrk[itrack].icharge) * M_PI ; 
      float theta = atan2 ( 1, globtrk[itrack].tanl ) ;
      l_extra->eta       = -log(tan(theta/2.));
/*---------------------------------------------------------
           Fill extra table now
----------------------------------------------------------*/
      if ( l_extra->tcor > 0. ) {
          index = ctg_index ( l_extra->iphi, l_extra->ieta, n_eta ) ;
          islat = ctf_cor_pointer[index]   ;
          l_extra->traw    = cor[islat].time ; 
          l_extra->beta    = l_extra->sleng / l_extra->tcor / 3.e10 ;
          l_extra->nmip    = cor[islat].n ;
      }
      else {
         extra[nctf_tracks].traw    = 0. ; 
         extra[nctf_tracks].beta    = 0. ;
         extra[nctf_tracks].nmip    = 0. ;
      } 
/*----------------------------------------------------------
       ends track loop
----------------------------------------------------------*/
   }
/*----------------------------------------------------------
       Set # entries in extra table
----------------------------------------------------------*/
   extra_h->nok = nctf_tracks + 1 ; 

   return STAFCV_OK ;
}
/*

            >-----------------------------------<

*/
long cte_is_empty (  long iphi, long ieta )
/*:>--------------------------------------------------------------------
**: ROUTINE:      cte_is_empty
**: DESCRIPTION:  Provides info about whether PMT has fired
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@physics.rice.edu
**: ARGUMENTS:
**:          IN:
**:             iphi, ieta      - Slat indexes
**:
**: RETURNS:    1 is empty, 0 is not empty
**:>------------------------------------------------------------------*/
{
   long n_eta = geo->n_counter_eta * geo->n_tray_eta ;
   long index = ctg_index ( iphi, ieta, n_eta ) ;
   long islat = ctf_cor_pointer[index]   ;
   if ( islat < 0 )  return 1     ;
   return 0 ;
}
long cte_linit ( CTG_GEO_ST *geo, CTE_PARA_ST  *para )
/*:>--------------------------------------------------------------------
**: ROUTINE:     cte_linit
**: DESCRIPTION: Initializes the CTF (TOF) track extrapolation
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       IN:
**:                geo    - CTF geometry
**:    INOUT:
**:                para   - Control cte behavior
**:
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
{
/*------------------------------------------------------------------------
      Check # bins does not exceed the array dimension
--------------------------------------------------------------------------*/
   if ( para->nbin_rphi  > 10 ) {
      sprintf ( OutMessage, "\n CTE_INIT: para->nbin_rphi = %i ",para->nbin_rphi ) ;
      MessageOut ( OutMessage ) ;
      return STAFCV_BAD ;
   }

   if ( para->nbin_z > 10 ) {
      sprintf ( OutMessage, "\n CTE_INIT: para->nbin_z = %i ",para->nbin_z ) ;
      MessageOut ( OutMessage ) ;
   }

/*----------------------------------------------------------------------------
       Transform drphi tolerance in degrees
-----------------------------------------------------------------------------*/


   if ( geo->r > 0 )
      angular_tolerance = para->r_tol / geo->r ;
   else {
      sprintf ( OutMessage, "\n CTE_INIT: geo->r = %f ",geo->r ) ;
      MessageOut ( OutMessage ) ;
      return STAFCV_BAD ;
   }

   para->init = 1 ;

   return STAFCV_OK ;
}
/*

            >-----------------------------------<

*/
void cte_slat_distance (  float phi ,  float z,
                          long  iphi,  long  ieta,
                          float& drphi, float& dz, float& dis )
/*:>--------------------------------------------------------------------
**: ROUTINE:     cte_slat_distance
**: DESCRIPTION: Calculates distance from impact to Slat edge
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@physics.rice.edu
**: ARGUMENTS:
**:          IN:
**:             phi,z           - Impact position
**:             iphi, ieta      - Slat indexes
**:         OUT:
**:             drphi,dz        - rphi and z distance from impact to slat center
**:             dis             - Slat distance
**:
**: RETURNS:    void
**:>------------------------------------------------------------------*/
{
   float dphi ;
   float local_drphi, local_dz ;
//
//     Distance in phi
//
   if ( phi > slat_phi[iphi-1].phi )
      dphi = phi - slat_phi[iphi-1].phi_max ;
   else
      dphi = slat_phi[iphi-1].phi_min - phi ;

   if ( fabs(dphi) > M_PI )
       dphi = dphi - sgn(dphi) * 2. * M_PI ;
   drphi = geo->r * dphi  ;

   local_drphi = max(0,drphi) ;
/*-------------------------------------------------------------------
       Distance in z
--------------------------------------------------------------------*/
   if ( z > slat_eta[ieta-1].z )
      dz = z - slat_eta[ieta-1].z_max ;
   else
      dz = slat_eta[ieta-1].z_min - z ;

   local_dz = max(0,dz) ;

   dis = sqrt(local_drphi*local_drphi+local_dz*local_dz) ;
}


/*

            >-----------------------------------<

*/
long  cte_select_slat ( CTE_EXTRA_ST* l_extra, EGR_GLOBTRK_ST* track ) 
/*:>--------------------------------------------------------------------
**: ROUTINE:     cte_select_slat
**: DESCRIPTION: Selects slat corresponding to a given impact    
**:              If several slats possible choose closest to impact 
**:           
**:          
**: AUTHOR:     ppy - P.P. Yepes,  yepes@physics.rice.edu
**: ARGUMENTS:
**:          IN:
**:             track           - Corresponding global track table row
**:       INOUT:
**:             l_extra         - Extrapolation table row to be used
**:
**: RETURNS:    0=ok, 1=error
**:>------------------------------------------------------------------*/
{
   long  ilphi, ileta, ihphi, iheta, i ;

   float local_drp,  local_dz, local_dis ;
   float min_distance ;
   float rphi, rz, dphi_res, dz_res ;
//
//     Get track momentum
//
   float pmom = 1.0 / track->invpt *
                sqrt ( 1. + track->tanl * track->tanl ) ;
//
   l_extra->p          = pmom  ; 
//
//     Find resolution in rphi
//
   for ( i=0 ; i<para->nbin_rphi ; i++ )
       if ( pmom < para->p_rphi[i] ) break ;
   i = min(i,para->nbin_rphi-1) ;
   dphi_res = para->d_rphi[i] ;

//
//     Find resolution in z   
//
   for ( i=0 ; i<para->nbin_z ; i++ )
       if ( pmom < para->p_z[i] ) break ;
   i = min(i,para->nbin_z-1) ;
   dz_res = para->d_z[i] ;
//
//     Get tolerances 
//
   rphi  = angular_tolerance * dphi_res + l_extra->e_phi_extra ;
   rz    = para->z_tol * dz_res + l_extra->e_z_extra ; 
//
//     Check within acceptance
//
   l_extra->nslat    =
   l_extra->ieta     =
   l_extra->iphi     = 0 ;
   l_extra->drphi    =
   l_extra->dz       =
   l_extra->dz_lit   = 0. ;
   l_extra->tcor     = 0. ;
   min_distance = 1.e9 ;

   iheta = ctg_i_eta ( l_extra->z_extra, slat_eta_h, slat_eta ) ;
   if ( iheta == 0 ) return 1 ;
//
//    Get phi index now
//
   ihphi = ctg_i_phi ( l_extra->phi_extra, geo_h, geo, slat_phi_h, slat_phi ) ;
   if ( ihphi <= 0 || ihphi > MX_PHI ) {
      sprintf ( OutMessage, "\n CTE_SELECT_SLAT: ihphi %d out of range ",ihphi ) ;
      MessageOut ( OutMessage ) ;
      return 1 ;
   }
//
//    Fill extrapolation table with relevant info for selected slat
//
   cte_slat_distance ( l_extra->phi_extra, l_extra->z_extra, ihphi, iheta,
                       local_drp, local_dz, local_dis ) ;
//
   l_extra->iphi         = ihphi     ;
   l_extra->ieta         = iheta     ;
   l_extra->drphi        = local_drp ;
   l_extra->dz           = local_dz  ;
/*----------------------------------------------------------
      If selected slat is not empty set n_slat to 1  
----------------------------------------------------------*/
   if ( !cte_is_empty ( ihphi, iheta ) ) 
      l_extra->nslat = 1 ;
   else {
//
//    If slat has not been fired, looked around for another slat  
//
//      Move around in phi direction first
//
      float phi_step  = min(2.*geo->tray_height/geo->r ,rphi) ;
//
      for ( float phi_now = l_extra->phi_extra - rphi ;
                  phi_now < l_extra->phi_extra + 1.1 * rphi ;
                  phi_now += phi_step ) { 
//
         if ( fabs(phi_now) < 1.e-5 ) continue ;
         
         ilphi = ctg_i_phi ( phi_now, geo_h, geo, slat_phi_h, slat_phi ) ;
//
//    Check new time if different slat 
//
         if ( ilphi == ihphi || cte_is_empty ( ilphi, iheta ) ) continue ;
//
         l_extra->nslat++ ;
//
//    Calculate distance
//
         cte_slat_distance ( l_extra->phi_extra , l_extra->z_extra, ilphi, iheta,
                             local_drp, local_dz, local_dis ) ;
//
//         Check distance
//
         if ( local_dis >  min_distance ) continue ;
//
         l_extra->iphi        = ilphi  ;
         l_extra->ieta        = iheta  ;
         l_extra->drphi       = local_drp ;
         l_extra->dz          = local_dz  ;
         min_distance         = local_dis ;
      }
//
//    Move in z now
//
      float z_step = min(rz, 2.*geo->tray_length/geo->n_counter_eta ) ;
//
      for ( float z_now  =  l_extra->z_extra - rz ;
                  z_now < (l_extra->z_extra + 1.1*rz) ;
                  z_now += z_step ) {

         if ( fabs(z_now-l_extra->z_extra) < 0.01 || 
              fabs(z_now) >  2.*geo->tray_length ) continue ;

         ileta = ctg_i_eta ( z_now, slat_eta_h, slat_eta ) ;

         if ( ileta < 0 || ileta == iheta   ) continue ;
         if ( cte_is_empty ( ihphi, ileta ) ) continue ;

         l_extra->nslat++ ;
//
//    Calculate distance
//
         cte_slat_distance ( l_extra->phi_extra, l_extra->z_extra, ihphi, ileta,
                             local_drp, local_dz, local_dis ) ;
//
//         Check distance
//
         if ( local_dis >  min_distance ) continue ;
//
         l_extra->iphi        = ihphi  ;
         l_extra->ieta        = ileta  ;
         l_extra->drphi       = local_drp ;
         l_extra->dz          = local_dz  ;
         min_distance = local_dis ;
      }
   }
//
//     Get corrected time if any
//
   cte_cor_time (  l_extra->z_extra, 
                   l_extra->iphi,    l_extra->ieta,
                   l_extra->tcor,    l_extra->dz_lit ) ; 
//
   return 0 ;
}
/* ********************************************************************

        End ctf_select_slat

**********************************************************************/
