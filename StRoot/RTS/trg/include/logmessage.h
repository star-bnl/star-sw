/* $Id: logmessage.h,v 1.1 2008/02/27 16:32:46 fine Exp $ */

/* **********************************************************************
 *  This include file defines everything with respect to the Trigger
 *  generic logmessage function
 */

#ifndef _LOGMESSAGE_INCLUDED_
#define _LOGMESSAGE_INCLUDED_

#define logmessage(a, b, c) parseMsg(__FILE__,__LINE__, a, b, c)

/* changecom  - switch off any m4 comment handling */
/* divert(11) - discard all further input */

#include <stdio.h>
#include <syslog.h>

#ifndef FLAG
#define FLAG                    int
#endif


#ifndef MAX_LOG_MSG
#define MAX_LOG_MSG 512       /* couldn't find it in VL's code, so take a guess - zm */
#endif

#define DEFAULT_LOG_OPTION      LOToConsole /* write to all */
#define MAX_LOG_MSG_LEN 256             /* max. log message len */
#define MAX_LOGWHO              16              /* max len of who id */
#define MAX_LOGSTR              MAX_LOG_MSG-MAX_LOGWHO-16

#ifndef TRUE
#define TRUE    1                               /* define pseudo booleans */
#define FALSE   0                               /* define pseudo booleans */
#endif
/* divert(0)  - end discarding */

/*
 *  the logmessage options
 */

typedef enum {
  LOToHost,                                     /* write to loghost only */
  LOToConsole,                                  /* write to console only */
  LOToHostAndConsole,                           /* write to console and host */
  LOUnknown
} logopt;


/*
 *  static control variables
 *  they are defined once at startup
 */

/* divert(11) - discard all further input */
extern char *logcomponent;                      /* component posting */
extern logopt logoptions;                       /* static options */
extern int LogLevelThreshold;                       /* log level threshold */
extern FLAG LogFlushEn;                         /* always flush output */
void trgLog( int pri, char *message );

void TRGlogmessage(int severity_level, char *who, char *msg);
void parseMsg(char *srcFile, int srcLine, int severity_level, char *who, char *msg);
char *date_str(void);

//char *syslogPriNam(int pri);
inline char *syslogPriNam( int pri )
{
  switch (pri&0x7) {
    case LOG_EMERG:     return "F";     /* system is unusable */
    case LOG_ALERT:     return "A";     /* action must be taken immediately */
    case LOG_CRIT:      return "C";     /* critical conditions */
    case LOG_ERR:       return "E";     /* error conditions */
    case LOG_WARNING:   return "W";     /* warning conditions */
    case LOG_NOTICE:    return "N";     /* normal but signification cond. */
    case LOG_INFO:      return "I";     /* informational */
    case LOG_DEBUG:     return "D";     /* debug-level messages */
    default:            return "U";     /* unknown priority */
  }
}

/* divert(0)  - end discarding */

#endif
/* end of file logmessage.h */


