#ifndef PAM_H
#define PAM_H
/* -------------- emlLib.h --------------------------*/
/*#include "emlLib.h" */
#ifdef __cplusplus
#define CC_P "C"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#else
#define CC_P
#endif
/*- STAFCV - STAF Condition Value -*/
#ifndef				  STAFCV_T
typedef long STAFCV_T;
#define STAFCV_BAD	0
#define STAFCV_OK	1
#define STAFCV_ERR	2
#define STAFCV_FATAL	3
#endif				/*STAFCV_T*/
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif
#ifndef NULL
#define NULL 0
#endif
/*-------------------------------------------- GLOBALS              --*/
/*#include "idl_types.h"*/
#include "table_header.h"
/*#include "amiLib.h" */

#include "StarCallf77.h"

#ifndef EXTERN
#define EXTERN extern
#endif


#endif /*PAM_H*/

