/*:Copyright 1997, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         emlLib.h
*:DESCRIPTION:  Error Messaging & Logging
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- NONE KNOWN --
*:HISTORY:      16mar97-v000a-cet- recreation
*:<---------------------------------------------------------------------
*/

#ifndef EMLLIB_H
#define EMLLIB_H
/*--------------------------------------------------------------------*/

/*-------------------------------------------- MACROS               --*/
#define EML Error Messaging & Logging

/*-------------------------------------------- INCLUDES             --*/
/*-------------------------------------------- TYPEDEFS             --*/
#ifdef __cplusplus
#define CC_P "C"
#else
#define CC_P
#endif

/*- STAFCV - STAF Condition Value -*/
#ifndef				  STAFCV_T
typedef long STAFCV_T;
#define STAFCV_BAD	0
#define STAFCV_OK	1
#endif				/*STAFCV_T*/

/*- EMLCV - EML Condition Value -*/
#ifndef				  EMLCV_T
typedef long EMLCV_T;
#define EMLCV_FALSE	0
#define EMLCV_TRUE	1
#define EMLCV_ERROR	0
#define EMLCV_FAILURE	0
#define EMLCV_SUCCESS	1
#define EMLCV_WARNING	2
#define EMLCV_MESSAGE	4
#endif				/*EMLCV_T*/

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif
#ifndef NULL
#define NULL 0
#endif

/*-------------------------------------------- PARAMETER MACROS     --*/
#define EML_STACKSIZE 8192
#define EML_BUFFERSIZE 1024
#define EML_CONTEXTSIZE 1024

/*-------------------------------------------- GLOBALS              --*/
#ifndef  EXTERN
#ifdef EML_MAIN
#   define EXTERN
#else
#   define EXTERN extern
#endif
#endif
#ifndef  CEXTERN
#ifdef EML_MAIN
#   define CEXTERN(id) extern CC_P id; id
#else
#   define CEXTERN(id) extern CC_P id
#endif
#endif
/* pretty err messages */
CEXTERN(char eml_beep_on);
CEXTERN(char eml_pretty_on);
CEXTERN(char eml_demand_ack_on);

CEXTERN(char eml_stack[EML_STACKSIZE+100]);
CEXTERN(char eml_buffer[EML_BUFFERSIZE]);
CEXTERN(char eml_context[EML_CONTEXTSIZE]);

/*-------------------------------------------- PROTOTYPES           --*/
/*- Initialization functions -*/
extern CC_P void eml_kuvec_init_();
extern CC_P int eml_init();
extern CC_P int eml_start();
extern CC_P int eml_stop();

/*- Error and message handling functions -*/
extern CC_P void emlPrettifyErrorMessage(char *errorString,int maxlen);
extern CC_P int emlMessage(char *fmt, ...);
extern CC_P char * emlContext(char *fmt, ...);
extern CC_P void ku_sibr();
extern CC_P void dsPerror(const char *msg);

/*-------------------------------------------- COMPATABILITY MACROS --*/
#ifndef NOKUIP
#	define SETCV(CODE) set_staf_status(CODE)
#else
#	define SETCV(CODE)
#endif

#ifndef NODSL
#	define DSPERROR(CODE) dsPerror(#CODE)
#else
#	define DSPERROR(CODE)
#endif

/*-------------------------------------------- INTERFACE MACROS     --*/

#define EML_INITSTACK() {memset(eml_stack,0,EML_STACKSIZE);eml_stack[EML_STACKSIZE]=(char)(-1);}
#define EML_INITCONTEXT() {memset(eml_context,0,EML_CONTEXTSIZE);}
#define EML_INITBUFFER() {memset(eml_buffer,0,EML_BUFFERSIZE);}
#define EML_INIT() {EML_INITSTACK(); EML_INITBUFFER(); \
	EML_INITCONTEXT();}

#define EML_CLEARSTACK() {eml_stack[0] = '\0';eml_stack[EML_STACKSIZE]=(char)(-1);}
#define EML_CLEARCONTEXT() {eml_context[0] = '\0';}
#define EML_CLEARBUFFER() {eml_buffer[0] = '\0';}
#define EML_CLEAR() {EML_CLEARSTACK(); EML_CLEARBUFFER(); \
	EML_CLEARCONTEXT();}
#define EML_OVERFULL() {\
        if (eml_stack[EML_STACKSIZE]!=(char)(-1)) {\
          printf("***EML_STACK Overfull***\n%s\n",eml_stack);\
          EML_CLEAR();}}
#define EML_PRINTCONTEXT(STREAM) \
	{ if(eml_context[0])fprintf(STREAM,"%s\n",eml_context); \
          EML_OVERFULL(); EML_CLEARCONTEXT(); fflush(STREAM); }

#define EML_PUSHCONTEXT() \
	{if(eml_context[0])strcat(eml_stack,eml_context); \
	EML_OVERFULL(); EML_CLEARCONTEXT();}
#define EML_PUSHERROR(CODE) \
	{sprintf(eml_buffer,"%s-%s.%d\n",#CODE,__FILE__,__LINE__); \
	strcat(eml_stack,eml_buffer); \
	EML_OVERFULL(); SETCV(STAFCV_BAD); \
	}
#define EML_PUSHSUCCESS(CODE) \
	{ SETCV(STAFCV_OK); }
#define EML_POPSTACK() { \
        char eml_error_ack[4]; \
	fflush(0); \
        emlPrettifyErrorMessage(eml_stack,EML_STACKSIZE); \
	fprintf(stderr,"-------------------------------"); \
	fprintf(stderr,"------------------------------------------------\n"); \
	fprintf(stderr,"%s\n",eml_stack); \
	DSPERROR((dsl):); \
	fflush(stderr); EML_CLEARSTACK();\
	if(eml_demand_ack_on) {\
	  fprintf(stderr,"Press return to continue or q to quit: "); \
	  fgets(eml_error_ack,4,stdin); \
	  if (eml_error_ack[0]=='q') ku_sibr();}\
	  else {;}\
	}

/*- EML_SUCCESS - clear error stack and return TRUE                  -*/
#define EML_SUCCESS(CODE) {EML_PUSHSUCCESS(CODE); EML_CLEAR(); \
	return TRUE;}

/*- EML_ERROR - push error onto error stack and return FALSE         -*/
#define EML_ERROR(CODE) {EML_PUSHERROR(CODE); EML_PUSHCONTEXT(); \
	return FALSE;}

/*- EML_FAILURE - print & clear error stack and return FALSE         -*/
#define EML_FAILURE(CODE) {EML_PUSHERROR(CODE); EML_PUSHCONTEXT(); \
	EML_POPSTACK(); return FALSE;}

/*- EML_MESSAGE - print formatted message                            -*/
#define EML_MESSAGE emlMessage

/*- EML_WARNING - print warning                                      -*/
#define EML_WARNING(MESS) { EML_TRACE(WARNING: MESS); }

/*- EML_CONTEXT - define formated context for error                  -*/
#define EML_CONTEXT emlContext

/*- EML_TRACE - print trace                                          -*/
#define EML_TRACE(CODE) \
	{fflush(0); \
	fprintf(stderr,"%s-%s.%d\n",#CODE,__FILE__,__LINE__); \
	if(eml_context[0])fprintf(stderr,"%s\n",eml_context); \
	EML_CLEARCONTEXT(); fflush(stderr); }

#define EML_DSPERROR(msg)

/*--------------------------------------------------------------------*/
#endif /*EMLLIB_H*/
/*--------------------------------------------------------------------*/

