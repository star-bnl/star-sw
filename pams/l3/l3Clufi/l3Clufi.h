/* .h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* l3Clufi.h */
#ifndef L3CLUFI_H
#define L3CLUFI_H
/*----------------------------------------------- INCLUDES   --*/
#include "PAM.h"
#include "hitarray.h"
#include "pixelarray.h"
/*----------------------------------------------- MACROS     --*/
#define L3CLUFI_RANK 2
/*----------------------------------------------- FORTRAN NAMES  --*/
#ifdef F77_NAME
#define l3Clufi_ F77_NAME(l3Clufi,L3CLUFI)
#endif
#ifndef type_of_call
#define type_of_call
#endif
/*----------------------------------------------- TYPEDEFS   --*/
typedef STAFCV_T (type_of_call *L3CLUFI_FT)
(
  TABLE_HEAD_ST         *pixels_h,    PIXELARRAY_ST    *pixels_d ,
  TABLE_HEAD_ST           *hits_h,      HITARRAY_ST    *hits_d 
);
/*----------------------------------------------- PROTOTYPES --*/
extern CC_P STAFCV_T type_of_call l3Clufi_ (
  TABLE_HEAD_ST         *pixels_h,    PIXELARRAY_ST    *pixels_d ,
  TABLE_HEAD_ST           *hits_h,      HITARRAY_ST    *hits_d 
);
#ifdef __cplusplus
extern CC_P STAFCV_T l3Clufi_load_ami(amiBroker *broker);
#endif /* __cplusplus */
#endif /* L3CLUFI_H */
