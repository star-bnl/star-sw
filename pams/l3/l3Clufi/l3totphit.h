/* .h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
     

  

 */
/* l3totphit.h */
#ifndef L3TOTPHIT_H
#define L3TOTPHIT_H
/*----------------------------------------------- INCLUDES   --*/
#include "PAM.h"
#include "tcl_tphit.h"
#include "hitarray.h"
/*----------------------------------------------- MACROS     --*/
#define L3TOTPHIT_RANK 2
/*----------------------------------------------- FORTRAN NAMES  --*/
#ifdef F77_NAME
#define l3totphit_ F77_NAME(l3totphit,L3TOTPHIT)
#endif
#ifndef type_of_call
#define type_of_call
#endif
/*----------------------------------------------- TYPEDEFS   --*/
typedef STAFCV_T (type_of_call *L3TOTPHIT_FT)
(
  TABLE_HEAD_ST       *hitarray_h,      HITARRAY_ST    *hitarray_d ,
  TABLE_HEAD_ST          *tpHit_h,     TCL_TPHIT_ST    *tpHit_d 
);
/*----------------------------------------------- PROTOTYPES --*/
extern CC_P STAFCV_T type_of_call l3totphit_ (
  TABLE_HEAD_ST       *hitarray_h,      HITARRAY_ST    *hitarray_d ,
  TABLE_HEAD_ST          *tpHit_h,     TCL_TPHIT_ST    *tpHit_d 
);
#ifdef __cplusplus
extern CC_P STAFCV_T l3totphit_load_ami(amiBroker *broker);
#endif /* __cplusplus */
#endif /* L3TOTPHIT_H */
