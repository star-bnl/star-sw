*
* $Id: finuc.inc,v 1.2 1998/04/16 16:58:44 fisyak Exp $
*
* $Log: finuc.inc,v $
* Revision 1.2  1998/04/16 16:58:44  fisyak
* 2nd pass with gstar
*
*
*
* finuc.inc
*
#if defined(CERNLIB_HADR)
*CMZ :          16/12/97  14.29.01  by  Pavel Nevski
*CMZ :  3.21/02 29/03/94  15.41.41  by  S.Giani
*-- Author :    A. Ferrari
*
*=== finuc ============================================================*
*
*----------------------------------------------------------------------*
*     Last change on 5 april 1990     by Alfredo Ferrari, INFN Milan   *
*     include file: finuc copy                   created 26/11/86 by pa*
*     note: if changed change finuc2 copy also                         *
*     changes: none                                                    *
*     included in the following subroutines or functions: not updated  *
*                                                                      *
*     description of the common block(s) and variable(s)               *
*                                                                      *
*     /finuc/ is the storage for secondaries created in event          *
*        np        = number of secondaries                             *
*        kpart(ip) = type of the secondary ip                          *
*        cxr(ip)   = direction cosine of the secondary ip              *
*                    with respect to x-axis                            *
*        cyr(ip)   = direction cosine of the secondary ip              *
*                    with respect to y-axis                            *
*        czr(ip)   = direction cosine of the secondary ip              *
*                    with respect to z-axis                            *
*        tki(ip)   = kinetic energy of secondary ip                    *
*        plr(ip)   = momentum of the secondary ip                      *
*        wei(ip)   = weight of the secondary ip                        *
*        tv        = excitation energy                                 *
*        tvcms     = actual excitation energy of the residual nucleus  *
*        tvrecl    = recoil kinetic energy of the residual nucleus     *
*        tvheav    = recoil kinetic energies of heavy (2-H, 3-H, 3-He, *
*                    4-He) fragments after evaporation                 *
*        tvbind    = approximate energy wasted in nuclear binding      *
*                    effects
*----------------------------------------------------------------------*
*
      PARAMETER (MXP=999)
      COMMON / FKFINU / CXR (MXP), CYR (MXP), CZR (MXP), TKI (MXP),
     &                 PLR (MXP), WEI (MXP), TV, TVCMS, TVRECL, TVHEAV,
     &                 TVBIND, NP0, NP, KPART (MXP)
 
#endif
