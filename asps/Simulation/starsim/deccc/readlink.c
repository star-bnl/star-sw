/*********************************************************
* $Id: readlink.c,v 1.3 2004/09/18 01:38:49 jeromel Exp $
* $Log: readlink.c,v $
* Revision 1.3  2004/09/18 01:38:49  jeromel
* Removed implict dec
*
* Revision 1.2  2004/06/26 00:16:28  potekhin
* Added size_t to the variable La in strncat
*
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
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
#include <string.h>
int readlink_(char* a, char *b, int La, int Lb)
{
   char path[256];
   path[0]=0;
   strncat (path,a,(size_t) La);
   return readlink(path,b,Lb);
}

