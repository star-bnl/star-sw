#include "FtfLog.h"

#ifdef L3_ONLINE
#include "rtsLog.h"
#endif

#include <stdio.h>
#include <stdarg.h>

int ftfLogTarget = FTF_LOG_PRINTF;


void ftfLog(const char *fmt, ...)
{
    va_list args;

    switch (ftfLogTarget) {
	
    case FTF_LOG_PRINTF:
	va_start(args,fmt);
	vprintf(fmt,args);
	va_end(args);
	break;

    case FTF_LOG_REMOTE:
	
#ifdef L3_ONLINE
	
	char tmp[100];
	
	va_start(args,fmt);
	vsprintf(tmp,fmt,args);
	va_end(args);

	//int l;
	
	LOG_LOCAL(tmp, 0,0,0,0,0,0);
	break;

#else
	printf("ftfLog: No remote logging available in this version of libL3Base\n");
	break;
#endif

    default:
	printf("ftfLog: unknown value of global variable ftfLogTarget (%d). Doing nothing.", ftfLogTarget);
	
    }									
}

