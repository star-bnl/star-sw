#ifndef _RTS_LOG_H_
#define _RTS_LOG_H_

/* 
	Version 2.1	Added __ROOT__ to stderr. 
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


/* colored output, :-) */
#define ANSI_GREEN      "\033[32m"
#define ANSI_RED        "\033[31m"
#define ANSI_BLUE       "\033[34m"
#define ANSI_YELLOW     "\033[33m"
#define ANSI_MAGENTA    "\033[35m"
#define ANSI_CYAN       "\033[36m"
#define ANSI_BOLD       "\033[1m"
#define ANSI_ITALIC     "\033[3m"
#define ANSI_UNDERLINE  "\033[4m"
#define ANSI_REVERSE    "\033[7m"
#define ANSI_RESET      "\033[0m"


/* this is only used in the UNIX version */


#define RTS_LOG_PORT_RTS	8000
#define RTS_LOG_PORT_TEST	8001
#define RTS_LOG_PORT_DAQ	8002
#define RTS_LOG_PORT_TRG	8003
#define RTS_LOG_PORT_EVP	8004
#define RTS_LOG_PORT_READER	8005
#define RTS_LOG_PORT_TPX	8006
#define RTS_LOG_PORT_DB		8007
#define RTS_LOG_PORT_ESB	8008
#define RTS_LOG_PORT_L3		8009
#define RTS_LOG_PORT_DET	8010
#define RTS_LOG_PORT_EMAIL	8013
#define RTS_LOG_PORT_DCS	8014
#define RTS_LOG_PORT_ITPC	8015

#define RTS_LOG_PORT	RTS_LOG_PORT_TEST	/* default, test.log, port */

/* the hostname _must_ be in the number notation! */
#ifdef RTS_DAQMAN
	#define RTS_LOG_HOST	RTS_DAQMAN
#else
	#define RTS_LOG_HOST	"130.199.60.86"	/* daqman.starp */
#endif

/* Bit pattern: log over network and/or stderr. Default is both. */
#define RTS_LOG_NET	1
#define RTS_LOG_STDERR	2
#define RTS_LOG_FILE	4
#define RTS_LOG_JML     8

void rtsLogAddJmlFile (char *fname);

/* strings used */
#define CRIT	"CRITICAL"	/* unmasked (5) */
#define OPER	"OPERATOR"	/* 4 */
#define ERR	"ERROR"		/* 3 */
#define WARN	"WARNING"	/* 2 */
#define NOTE	"NOTICE"	/* 1 */
#define DBG	"DEBUG"		/* 0 */

#define INFO	"INFO"		/* unmasked */

#define CAUTION	"CAUTION"	/* unmasked - for the operator */

#define TERR	"Tonko"	

#define U_TONKO	"U_TONKO"		/* Tonko gets email */
#define U_JEFF	"U_JEFF"		/* Jeff gets email */
#define U_IST	"U_IST"			/* IST manager gets email currently Gerrit */
#define U_TOF	"U_TOF"			/* TOF/MTD manager, Geary */
#define U_RP2	"U_RP2"			/* New pp2pp */
#define SAVEme	"SAVEme"	/* gets saved in saved.log */




/*Tonko: not used Special (mis)handling for STAR Offline Code
#ifdef __ROOT__	
#define RTS_DISABLE_LOG
#endif
*/

#ifdef RTS_DISABLE_LOG

#define RTS_ASSERT(expr)	assert(expr)


	#define LOG(SEV,STRING,ARGS...) \
        do { \
                const char *const yada = SEV ; \
                if((*yada == 'E')) { \
                        fprintf(stderr,"" ANSI_RED "RTS_" SEV ": " __FILE__ " [line %d]: " STRING "" ANSI_RESET "\n" , __LINE__ , ##ARGS) ;\
		} \
                else if((*yada == 'C')) { \
                        fprintf(stderr,"" ANSI_RED "" ANSI_REVERSE "RTS_" SEV ": " __FILE__ " [line %d]: " STRING "" ANSI_RESET "\n" , __LINE__ , ##ARGS) ;\
		} \
                else if((*yada == 'I')) { \
                        fprintf(stderr,"" ANSI_BLUE "" ANSI_BOLD "RTS_" SEV ": " __FILE__ " [line %d]: " STRING "" ANSI_RESET "\n" , __LINE__ , ##ARGS) ;\
		} \
                else if((*yada == 'T')) { \
                        fprintf(stderr,"" ANSI_GREEN "" ANSI_BOLD "RTS_" SEV ": " __FILE__ " [line %d]: " STRING "" ANSI_RESET "\n" , __LINE__ , ##ARGS) ;\
		} \
                else if((*yada == 'W')) { \
                        fprintf(stderr,"" ANSI_CYAN "" ANSI_BOLD "RTS_" SEV ": " __FILE__ " [line %d]: " STRING "" ANSI_RESET "\n" , __LINE__ , ##ARGS) ;\
		} \
                else if((*yada == 'O')) { \
                        fprintf(stderr,"" ANSI_BLUE "" ANSI_REVERSE "RTS_" SEV ": " __FILE__ " [line %d]: " STRING "" ANSI_RESET "\n" , __LINE__ , ##ARGS) ;\
		} \
	} while(0) \




// the following become noops...
#define rtsLogLevel(x)
#define rtsLogAddDest(x,y)
#define rtsLogLevelInt(x)
#define rtsLogOutput(x)

#else	/* RTS_DISABLE_LOG */


#ifdef __GNUC__
	#define INLINE_HACK extern __inline__
#else
	#define INLINE_HACK inline
#endif



/* exists in all flavors */
extern volatile int tonkoLogLevel ;	/* the unfortunte name is due to historic reasons... */

extern int rtsLogAddDest(const char *server, int port) ;


INLINE_HACK void rtsLogLevelInt(int level)
{
  tonkoLogLevel = level;
  return;
}

/* let's have a function too... */
INLINE_HACK void rtsLogLevel(const char *level) 
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





#ifdef __vxworks	
/* Following is vxWorks specific */
	#include <vxWorks.h>
	#include <logLib.h>

	#define RTS_ASSERT(expr)

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
			logMsg("" SEV ": " __FILE__ " [line %d]: " STRING "\n",__LINE__,(unsigned int)A1,(unsigned int)A2,(unsigned int)A3,(unsigned int)A4,(unsigned int)A5) ;\
			sbLOG("" SEV ": " __FILE__ " [line %d]: " STRING "\n",__LINE__,(unsigned int)A1,(unsigned int)A2,(unsigned int)A3,(unsigned int)A4,(unsigned int)A5) ;\
		} \
	} while(0) \

	
	#define rtsLogOutput(x)

#else /* unix */

	#define RTS_ASSERT(expr)        LOG(CRIT,"assert(%s) true -- certain death follows",__STRING(expr))

	extern int rtsLogUnix_v(const char *str, ...) ;

	extern int rtsLogOutput(int flag) ;

	extern void rtsLogAddCmd(const char *cmd) ;

	extern int rtsLogAddFile(char *fname) ;

#ifdef RTS_LOG_COLORED

	#define LOG(SEV,STRING,ARGS...) \
        do { \
                const char *const yada = SEV ; \
                if((tonkoLogLevel>0) && (*yada == 'D')) ; \
                else if((tonkoLogLevel>1) && (*yada == 'N')) ; \
                else if((tonkoLogLevel>2) && (*yada == 'W')) ; \
                else if((tonkoLogLevel>3) && (*yada == 'E')) ; \
                else if((tonkoLogLevel>4) && (*yada == 'O')) ; \
                else { \
                        rtsLogUnix_v("COLOR" SEV ": " __FILE__ " [line %d]: " STRING "\n" , __LINE__ , ##ARGS) ;\
		} \
	} while(0) \

#else
	#define LOG(SEV,STRING,ARGS...) \
        do { \
                const char *const yada = SEV ; \
                if((tonkoLogLevel>0) && (*yada == 'D')) ; \
                else if((tonkoLogLevel>1) && (*yada == 'N')) ; \
                else if((tonkoLogLevel>2) && (*yada == 'W')) ; \
                else if((tonkoLogLevel>3) && (*yada == 'E')) ; \
                else if((tonkoLogLevel>4) && (*yada == 'O')) ; \
                else { \
                        rtsLogUnix_v("" SEV ": " __FILE__ " [line %d]: " STRING "\n" , __LINE__ , ##ARGS) ;\
		} \
	} while(0) \



#endif

#endif


#endif	/* RTS_DISABLE_LOG */

#ifdef __cplusplus
}
#endif

#endif	/* _RTS_LOG_H */
