#ifndef _RTS_LOG_H_
#define _RTS_LOG_H_

/* 
	Version 2.0	Revamped to use funky variadic GNU cpp stuff...
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

#define RTS_LOG_NET	1
#define RTS_LOG_STDERR	2


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
/* Tonko added this on 3/10/2003 */
#define INFO	"INFO"		/* unmasked */



#ifdef __GNUC__
	#define INLINE_HACK extern __inline__
#else
	#define INLINE_HACK inline
#endif

/* exists in all flavors */
extern volatile int tonkoLogLevel ;	

extern int rtsLogAddDest(char *server, int port) ;


INLINE_HACK void rtsLogLevelInt(int level)
{
  tonkoLogLevel = level;
  return;
}

/* let's have a function too... */
INLINE_HACK void rtsLogLevel(char *level) 
{
	switch((int) *level) {
	case 'D' :
		tonkoLogLevel = 0 ;
		break ;
	case 'N' :
		tonkoLogLevel = 1 ;
		break ;
	case 'W' :
		tonkoLogLevel = 2 ;
		break ;
	case 'E' :
		tonkoLogLevel = 3 ;
		break ;
	case 'O' :
		tonkoLogLevel = 4 ;
		break ;
	case 'C' :
	default :
		tonkoLogLevel = 5 ;
		break ;
	}

	return ;
}




/* if it's not unix it must be vxWorks */
#ifndef __unix__
	#include <vxWorks.h>
	#include <logLib.h>

	/* Only in MVME vxworks kernels! */
	#ifdef _ARCH_PPC	
		extern int sbLoggerStart(void) ;
		extern int sbLOG(char *str, unsigned int a1, unsigned int a2, 
				 unsigned int a3, unsigned int a4, unsigned int a5, unsigned int a6) ;
		extern int sbLoggerRemoveDesc(int desc) ;
		extern int sbLoggerAddDesc(int desc) ;
	#else
		#define sbLOG(args...) 
	#endif


#define LOG(SEV,STRING,A1,A2,A3,A4,A5) \
        do { \
                const char *const yada = SEV ; \
                if((tonkoLogLevel>0) && (*yada == 'D')) ; \
                else if((tonkoLogLevel>1) && (*yada == 'N')) ; \
                else if((tonkoLogLevel>2) && (*yada == 'W')) ; \
                else if((tonkoLogLevel>3) && (*yada == 'E')) ; \
                else if((tonkoLogLevel>4) && (*yada == 'O')) ; \
                else { \
			logMsg(""SEV": "__FILE__" [line %d]: "STRING"\n",__LINE__,(unsigned int)A1,(unsigned int)A2,(unsigned int)A3,(unsigned int)A4,(unsigned int)A5) ;\
			sbLOG(""SEV": "__FILE__" [line %d]: "STRING"\n",__LINE__,(unsigned int)A1,(unsigned int)A2,(unsigned int)A3,(unsigned int)A4,(unsigned int)A5) ;\
		} \
	} while(0) \


#else /* unix */

	extern int rtsLogUnix_v(const char *str, ...) ;

	extern int rtsLogOutput(int flag) ;
#ifndef __ROOT__
#define LOG(SEV,STRING,ARGS...) \
        do { \
                const char *const yada = SEV ; \
                if((tonkoLogLevel>0) && (*yada == 'D')) ; \
                else if((tonkoLogLevel>1) && (*yada == 'N')) ; \
                else if((tonkoLogLevel>2) && (*yada == 'W')) ; \
                else if((tonkoLogLevel>3) && (*yada == 'E')) ; \
                else if((tonkoLogLevel>4) && (*yada == 'O')) ; \
                else { \
                        rtsLogUnix_v(""SEV": "__FILE__" [line %d]: "STRING"\n" , __LINE__ , ##ARGS) ;\
		} \
	} while(0) \

#else
/* -pedantic does not like variadic macros :) */
#define LOG(SEV,STRING,ARG1,ARG2,ARG3,ARG4,ARG5) \
        do { \
                const char *const yada = SEV ; \
                if((tonkoLogLevel>0) && (*yada == 'D')) ; \
                else if((tonkoLogLevel>1) && (*yada == 'N')) ; \
                else if((tonkoLogLevel>2) && (*yada == 'W')) ; \
                else if((tonkoLogLevel>3) && (*yada == 'E')) ; \
                else if((tonkoLogLevel>4) && (*yada == 'O')) ; \
                else { \
                        rtsLogUnix_v(""SEV": "__FILE__" [line %d]: "STRING"\n" , __LINE__ , \
                        ARG1,ARG2,ARG3,ARG4,ARG5) ;\
		} \
	} while(0) \

#endif /* __ROOT__ */
#endif


#ifdef __cplusplus
}
#endif

#endif	/* _RTS_LOG_H */
