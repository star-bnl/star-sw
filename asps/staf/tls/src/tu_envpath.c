/* %W%  %G%  A Fortran routine translates a path name with environment variables.
 *
 */

/****************************************************************************
 *  tu_envpath - a Fortran callable routine to translate a path name
 *               with environment variables in it.
 *
 * On VMS it just copies the input argument to the output argument.
 */

#ifndef VMS
#include <string.h>
#include <stdlib.h>
#endif /* not VMS */

#ifdef VMS

#include <descrip.h>


void tu_envpath( envpath, pathname )
struct dsc$descriptor *envpath;
struct dsc$descriptor *pathname;
{
  int minlen;
  minlen = envpath->dsc$w_length;
  if(pathname->dsc$w_length < minlen) minlen = pathname->dsc$w_length;
  strncpy( pathname->dsc$a_pointer, envpath->dsc$a_pointer, minlen );
}

#else
void tu_envpath_( envpath, pathname, elen, plen )
char *envpath;
char *pathname;
int elen;
int plen;
{
#define MAX_ENV 100
  char *slash, *colon;
  char *eptr;
  char *ebuf;
  char envbuf[MAX_ENV];
  int eblen;
  int i;
  char *envptr;
  int minlen;
  int clen;
  int free_ebuf;
  int rlen;
  int p1len;

  ebuf = (char *)malloc( elen + 1);
  if(ebuf == NULL)
    {
      free_ebuf = 0;
      eptr = envpath;
      eptr[elen-1] = 0;
    }
  else
    {
      free_ebuf = 1;
      strncpy(ebuf, envpath, elen);
      ebuf[elen] = 0;
      eptr = ebuf;
    }
  
  minlen = elen;
  if(plen < minlen) minlen = plen;
  pathname[0] = 0;
  for(i=elen-1; eptr[i] == ' ' && i>0; i--) eptr[i] = 0;

  if(eptr[0] == '$')
    {
      slash = strchr(&eptr[1], '/');
      if(slash == NULL)
	{
	  envptr = getenv( &eptr[1] );
	  if(envptr == NULL)
	    strncpy( pathname, &eptr[1], plen);
	  else /* envptr == NULL */
	    strncpy( pathname, envptr, plen);
	}
      else /* slash == NULL */
	{
	  i = (int)slash - (int)eptr - 1;
	  strncpy( envbuf, &eptr[1], i);
	  envbuf[i] = 0;
	  envptr = getenv( envbuf );
	  if(envptr == NULL)
	    strncpy( pathname, ++slash, plen);
	  else /* envptr == NULL */
	    {
	      strncpy( pathname, envptr, plen-1);
	      pathname[plen-1] = 0;
	      p1len = strlen( pathname );
	      strncat( pathname, slash, plen - p1len);
	    }
	}
    }
  else
    {
	colon = strchr( &eptr[0], ':');
	if(colon != NULL) 
	  {
	      i = (int) colon - (int)eptr;
	      strncpy( envbuf, eptr, i);
	      envbuf[i] = 0;
	      envptr = getenv( envbuf );
	      if(envptr != NULL) 
		{
		    strncpy( pathname, envptr, plen-1 );
		    pathname[plen-1] = 0;
		    strncat( pathname, "/", plen-1);
		    strncat( pathname, &colon[1], plen );
		}
	      else
		strncpy( pathname, eptr, plen );
	  }
	else
	  strncpy( pathname, eptr, plen);
    }
  if(free_ebuf == 1) free( ebuf );
}  
#endif /* SUN */
