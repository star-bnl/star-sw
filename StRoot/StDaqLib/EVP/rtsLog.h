#ifndef _RTS_LOG_H
#define _RTS_LOG_H

/* 
	Version 1.2	Added RTS_PROJECT so that the defaults work
			even with PP2PP
	Version 1.11	Replaced "extern __inline__" with just "inline"
			so that Sun's CC can use it...
	Version 1.1	Should be useful by	vxWorks: I960, PPC
						unix


*/

#ifdef __cplusplus
extern "C" {
#endif

/* this is only used in the UNIX version */
#define RTS_LOG_PORT	8000	/* default, rts.log, port */

/* the hostname _must_ be in the number notation! */
#define RTS_LOG_HOST	"130.199.60.86"	/* daqman.starp */




/* strings used */
#define CRIT	"CRITICAL"	/* unmasked (5) */
#define OPER	"OPERATOR"	/* 4 */
#define ERR	"ERROR"		/* 3 */
#define WARN	"WARNING"	/* 2 */
#define NOTE	"NOTICE"	/* 1 */
#define DBG	"DEBUG"		/* 0 */
/* Tonko added this 02/27/2002 */
#define TERR	"TERR"		/* unmasked (5) */
/* Tonko added this 02/05/2003 */
#define SHIFT	"SHIFTLOG"	/* unmasked (5) */

#define LOG(SEV,STRING,A1,A2,A3,A4,A5) \
        LOG_LOCAL(""SEV": "__FILE__" [line %d]: "STRING"\n",__LINE__,A1,A2,A3,A4,A5)

/* exists in all flavors */
/* extern volatile int tonkoLogLevel ;	 */

/* if it's not unix it must be vxWorks */
#ifndef unix
	#include <vxWorks.h>
	#include <logLib.h>

	

	/* Only in MVME vxworks kernels! */
	#ifdef _ARCH_PPC	
		extern int sbLoggerStart(void) ;
		extern int sbLOG(char *str, unsigned int a1, unsigned int a2, 
				 unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6) ;
		extern int sbLoggerRemoveDesc(int desc) ;
		extern int sbLoggerAddDesc(int desc) ;
	#endif
#else /* unix */

#define RTS_LOG_NET	1
#define RTS_LOG_STDERR	2

	extern int rtsLogUnix(char *str, unsigned int a1, unsigned int a2, 
			 unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6) ;

	extern int rtsLogOutput(int flag) ;


#endif

	/* both flavors */
	extern int rtsLogAddDest(char *server, int port) ;

#ifdef __GNUC__
	#define INLINE_HACK extern __inline__
#else
	#define INLINE_HACK inline
#endif

/* jml... */
INLINE_HACK void rtsLogLevelInt(int level)
{
/*   tonkoLogLevel = level; */
  return;
}

/* let's have a function too... */
INLINE_HACK void rtsLogLevel(char *level) 
{
	switch((int) *level) {
	case 'D' :
/* 		tonkoLogLevel = 0 ; */
		break ;
	case 'N' :
/* 		tonkoLogLevel = 1 ; */
		break ;
	case 'W' :
/* 		tonkoLogLevel = 2 ; */
		break ;
	case 'E' :
/* 		tonkoLogLevel = 3 ; */
		break ;
	case 'O' :
/* 		tonkoLogLevel = 4 ; */
		break ;
	case 'C' :
	default :
/* 		tonkoLogLevel = 5 ; */
		break ;
	}

	return ;
}

/* this is the real work */
INLINE_HACK void LOG_LOCAL(char *str, unsigned int l, unsigned int a1, unsigned int a2, 
				 unsigned int a3, unsigned int a4, unsigned int a5)
{

	switch((int) *str) {
	case 'D' :
/* 		if(tonkoLogLevel > 0) return ; */
		break ;
	case 'N' :
/* 		if(tonkoLogLevel > 1) return ; */
		break ;
	case 'W' :
/* 		if(tonkoLogLevel > 2) return ; */
		break ;
	case 'E' :
/* 		if(tonkoLogLevel > 3) return ; */
		break ;
	case 'O' :
/* 		if(tonkoLogLevel > 4) return ; */
		break ;
	default :	/* all others are non-maskable! */
		break ;
	}


#ifndef unix
	logMsg(str,l,a1,a2,a3,a4,a5) ;
	/* use the rest only if on MVME kernels! */
	#ifdef _ARCH_PPC
		sbLOG(str,l,a1,a2,a3,a4,a5) ;
	#endif
#else
	rtsLogUnix(str,l,a1,a2,a3,a4,a5) ;
#endif

	return ;
}








#ifdef __cplusplus
}
#endif

#endif	/* _RTS_LOG_H */
