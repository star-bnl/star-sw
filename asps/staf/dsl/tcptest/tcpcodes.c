/*
**	tcpcodes.c  -  
*/


#include <sys/types.h>
#include "tcplib.h"


char *tcpCodeStr(status)
int status ;
    {
    switch(status)
	{
	case ok:			return "ok";
	case acceptFailure:		return "acceptFailure";
	case bindFailure:		return "bindFailure";
	case connectFailure:		return "connectFailure";
	case gethostbynameFailure:	return "gethostbynameFailure";
	case listenFailure:		return "listenFailure";
	case socketCallFailed:		return "socketCallFailed";
	default:			return "unknown tcpCode";
	}
    }
