#ifndef PAM_INC
#define PAM_INC
#endif 
/*- STAFCV - STAF Condition Value -*/
#ifndef				  STAFCV_T
#define STAFCV_T        INTEGER*4
#define STAFCV_BAD	0
#define STAFCV_OK	1
#define STAFCV_ERR	2
#define STAFCV_FATAL	3
#endif				/*STAFCV_T*/

      integer*4 stafcv_bad,stafcv_ok,stafcv_err,stafcv_fatal
      parameter (stafcv_bad=STAFCV_BAD,stafcv_ok   =STAFCV_OK   )
      parameter (stafcv_err=STAFCV_ERR,stafcv_fatal=STAFCV_FATAL)

#include "table_header.inc"

