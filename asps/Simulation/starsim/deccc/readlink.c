/*********************************************************
* $Id: readlink.c,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Log: readlink.c,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.2  2001/03/05 11:55:22  nevski
* headers clean-up
*
* Revision 1.1  2001/02/27 10:15:18  nevski
*  first working release
*
* Revision 1.1  2001/02/17 18:10:12  nevski
* support for the true filename search
*
*********************************************************/
#include <unistd.h>
int readlink_(char* a, char *b, int La, int Lb)
{
   char path[256];
   path[0]=0;
   strncat (path,a,La);
   return readlink(path,b,Lb);
}

