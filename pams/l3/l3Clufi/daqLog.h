#ifndef _DAQ_LOG_H
#define _DAQ_LOG_H

#ifdef __cplusplus
extern "C" {
#endif

#include <vxWorks.h>

#ifdef _NEW_DAQ_LOG
extern int sbLoggerStart(void) ;
extern int sbLOG(char *str, UINT32 a1, UINT32 a2, UINT32 a3, UINT32 a4, UINT32 a5, UINT32 a6) ;
extern int sbLoggerRemoveDesc(int desc) ;
extern int sbLoggerAddDesc(int desc) ;
#else
#include <logLib.h>
#endif

#define LOG(SEV,STRING,A1,A2,A3,A4,A5) \
        LOG_LOCAL(""SEV": "__FILE__" [line %d]: "STRING"\n",__LINE__,A1,A2,A3,A4,A5)



#define CRIT	"CRITICAL"
#define ERR	"ERROR"
#define WARN	"WARNING"
#define NOTE	"NOTICE"
#define DBG	"DEBUG"

extern volatile int tonkoLogLevel ;

extern __inline__ void LOG_LOCAL(char *str, UINT32 l, UINT32 a1, UINT32 a2, UINT32 a3, UINT32 a4,
	   UINT32 a5)
{


	switch((int) *str) {
	case 'D' :
		if(tonkoLogLevel > 0) return ;
		break ;
	case 'N' :
		if(tonkoLogLevel > 1) return ;
		break ;
	case 'W' :
		if(tonkoLogLevel > 2) return ;
		break ;
	case 'E' :
		if(tonkoLogLevel > 3) return ;
		break ;
	case 'C' :
		if(tonkoLogLevel > 4) return ;
		break ;
	default :
		break ;
	}

#ifdef _NEW_DAQ_LOG
	sbLOG(str,l,a1,a2,a3,a4,a5) ;
#else
	logMsg(str,l,a1,a2,a3,a4,a5) ;
#endif
	return ;
}


#ifdef __cplusplus
}
#endif
#endif	/* _DAQ_LOG_H */
