#ifndef _MVME_FAST_TICKER_LIB_H
#define _MVME_FAST_TICKER_LIB_H

#include <vxWorks.h>

#include <MVME/ppcIOLib.h>

#define MVME_FAST_TICKER_DIV    ((UINT32 *)0xfc0010f0)
#define MVME_FAST_TICKER_LOAD   ((UINT32 *)0xfc001110)
#define MVME_FAST_TICKER_READ   ((UINT32 *)0xfc001100)
#define MVME_FAST_TICKER_VECTOR ((UINT32 *)0xfc001120)
#define MVME_FAST_TICKER_DEST   ((UINT32 *)0xfc001130) 

extern void mvmeFastTickerInit(void) ;
extern UINT32 mvmeFastTickerGet(void) ;

extern inline volatile unsigned int mvmeFastTickerMark(void)
{
        return rs32(MVME_FAST_TICKER_READ) & 0x7FFFFFFF ;
}


extern inline volatile unsigned int mvmeFastTickerDelta(unsigned int old)
{
        volatile unsigned int cur = rs32(MVME_FAST_TICKER_READ) & 0x7FFFFFFF ;
	u_int ret ;

        if(old >= cur) ret = (old-cur) ;
        else ret = (((unsigned int)0x7FFFFFFF-cur)+old) ;

	return (ret*4 + 17)/33 ;	// round off the value to microseconds
}



#endif 
