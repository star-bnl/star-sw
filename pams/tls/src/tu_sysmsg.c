/* tu_sysmsg.c  translates system dependent error numbers
 *
 * Created 24-April-1992  D. Olson
 *
 */

#define MAXMSG 1024
static char *blanks = 
"                                                                             ";

#ifdef VMS

#include <descrip.h>

int tu_sysmsg( code, msg )
    int *code;
    struct dsc$descriptor *msg;
{
    short mlen;
    int status;
    int flag = 0;
    char out[4];

    status = sys$getmsg( *code, &mlen, msg, flag, out );
    return status;
}

#else

#include <errno.h>
extern int sys_nerr;
extern char *sys_errlist[];
extern int errno;

int tu_sysmsg_( code, msg, mlen )
    int *code;
    char *msg;
    int mlen;
{
    if(*code > 0 && *code < sys_nerr)
    {
	strncpy( msg, sys_errlist[*code], mlen-1);
	return 1;
    }
    else
    {
	strncpy( msg, blanks, mlen-1);
	return 0;
    }
}

#endif	
    
