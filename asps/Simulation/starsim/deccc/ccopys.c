/*
 * $Id: ccopys.c,v 1.1 2018/11/19 23:20:11 perev Exp $
 *
 * $Log: ccopys.c,v $
 * Revision 1.1  2018/11/19 23:20:11  perev
 * 64bits new comis files added from /CERN
 *
 * Revision 1.4  1997/09/02 15:50:38  mclareni
 * WINNT corrections
 *
 * Revision 1.3  1997/03/14 12:02:22  mclareni
 * WNT mods
 *
 * Revision 1.2.2.1  1997/01/21 11:34:41  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.2  1996/03/14 13:44:09  berezhno
 * mods for WINNT
 *
 * Revision 1.1.1.1  1996/02/26 17:16:55  mclareni
 * Comis
 *
 */
#include "comis/pilot.h"
#if !defined(CERNLIB_ALPHA_OSF)
/*CMZ :  1.16/13 16/09/93  16.08.50  by  Rene Brun*/
/*-- Author :*/
/*-- Author :*/
char *getPntB(int dif); 

void ccopys_(ja,jb,nn)
     char **ja, **jb;
     int *nn;
{
  int i,n; char *a,*b;
  a = getPntB(*ja);
  b = getPntB(*jb);
  n=*nn; 
  if ( a >= b )
     for ( i=0; i<n; i++ )
          b[i]=a[i];
  else
    { if(a+n > b)
       for ( i=n-1; i>=0; i-- )
           b[i]=a[i];
      else
        for ( i=0; i<n; i++ )
           b[i]=a[i];

     }
}
#endif
