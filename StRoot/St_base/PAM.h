/*!
 * \file PAM.h
 */
#ifndef PAM_H
#define PAM_H
/* -------------- emlLib.h --------------------------*/
/*#include "emlLib.h" */
#ifdef __cplusplus
#define CC_P "C"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
class amiBroker;
#else
#define CC_P
#endif
/*- STAFCV - STAF Condition Value -*/
#ifndef				  STAFCV_T
typedef long STAFCV_T;
#define STAFCV_BAD        0                                            /*! \def STAFCV_BAD */
#define STAFCV_OK        1                                             /*! \def STAFCV_OK */
#define STAFCV_ERR        2                                            /*! \def STAFCV_ERR */
#define STAFCV_FATAL        3                                          /*! \def STAFCV_FATAL */
#endif				/*STAFCV_T*/
#ifndef TRUE
#define TRUE 1                                                         /*! \def TRUE */
#endif
#ifndef FALSE
#define FALSE 0                                                        /*! \def FALSE */
#endif
#ifndef NULL
#define NULL 0                                                         /*! \def NULL */
#endif
/*-------------------------------------------- GLOBALS              --*/
/*#include "idl_types.h"*/
#include "table_header.h"
/*#include "amiLib.h" */

#include "StarCallf77.h"

#ifndef EXTERN
#define EXTERN extern                                                  /*! \def EXTERN */
#endif


#endif /*PAM_H*/

