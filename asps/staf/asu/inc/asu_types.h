/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         asu_types.h
*:DESCRIPTION:  Variable types for ASU
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      20nov95-v000a-cet- create
*:<---------------------------------------------------------------------
*/
#ifndef ASU_TYPES_H
#define ASU_TYPES_H

/*-------------------------------------------- INCLUDES             --*/
#ifdef CORBA
#include "asu_i.hh"
#endif

#include "emlLib.h"

/*-------------------------------------------- TYPEDEFS             --*/

/*-------------------------------------------- PROTOTYPES           --*/
extern CC_P int asu_init();
extern CC_P int asu_start();
extern CC_P int asu_stop();

#ifndef NOKUIP
extern CC_P void asu_def_();

extern CC_P void kam_asu_hello_();
extern CC_P void kam_asu_time_();
extern CC_P void kam_asuallocstats_();

extern CC_P void asu_kuip_init_();
#endif /*NOKUIP*/

extern CC_P STAFCV_T asu_hello(char* msg);
extern CC_P STAFCV_T asu_time();
extern CC_P STAFCV_T asuallocstats();

/*- F77 prototypes for STAF vector functions. -*/
extern CC_P void set_staf_status_(long* status);
extern CC_P long get_staf_status_(long* n);
extern CC_P void set_staf_result_(float* result);
extern CC_P float get_staf_result_(long* n);

/*- C Stubs for STAF vector functions. -*/
extern CC_P void set_staf_status(long status);
extern CC_P long get_staf_status(long n);
extern CC_P void set_staf_result(float result);
extern CC_P float get_staf_result(long n);

#endif /* ASU_TYPES_H */

