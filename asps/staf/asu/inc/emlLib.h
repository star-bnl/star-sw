/* Copyright 1996, Lawrence Berkeley National Laboratory              */
/*
*:FILE:		emlLib.h
*:DESCRIPTION:	TRIVIAL include file for EML
*/
#ifndef EMLLIB_H
#define EMLLIB_H

#include <stdio.h>
#include "stafGeneric.h"

#define EML Error Messaging and Logging

/*- STAFCV - STAR Analysis Framework Condition Value -*/
#ifndef				  STAFCV_T
typedef long STAFCV_T;
#define STAFCV_BAD	0
#define STAFCV_OK	1
#endif				/*STAFCV_T*/

/*- EML prototypes -*/
extern CC_P void eml_kuvec_init_();
extern CC_P int eml_init();
extern CC_P int eml_start();
extern CC_P int eml_stop();

/*- EML macro calls -*/

#define EML_ERROR(code) {EML_LOG_ERROR(code); return FALSE;}
#define EML_SUCCESS(code) {EML_LOG_SUCCESS(code); return TRUE;}
#define EML_MESSAGE(code) {printf( #code "\n");fflush(0);}
#define EML_LOG_ERROR(code) {EML_TRACE(#code);set_staf_status(STAFCV_BAD);}
/*DEBUG HACK #define EML_LOG_SUCCESS(code) {EML_TRACE(#code);set_staf_status(STAFCV_OK);} */
#define EML_LOG_SUCCESS(code) {set_staf_status(STAFCV_OK);}
#define EML_TRACE(msg) {printf("%s.%d-%s\n",__FILE__,__LINE__,msg);fflush(0);}
#define EML_PRINTF printf("%s.%d-",__FILE__,__LINE__);fflush(0);printf
#define EML_DSPERROR(msg) EML_TRACE(#msg);dsPerror("(dsl):")

#endif /*EMLLIB_H*/

