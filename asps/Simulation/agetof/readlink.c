/*********************************************************
* $Id: readlink.c,v 1.2 2004/01/25 01:36:12 fisyak Exp $
* $Log: readlink.c,v $
* Revision 1.2  2004/01/25 01:36:12  fisyak
* Clean up unused variable to avoid waning by Victor request
*
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.1  2001/02/28 22:56:17  nevski
* improved comment treatment, f77-structures etc
*
*
* support for the true filename search
*********************************************************/
#include <unistd.h>
#include <string.h>
int readlink_(char* a, char *b, int La, int Lb)
{
   char path[256];
   path[0]=0;
   strncat (path,a,La);
   return readlink(path,b,Lb);
}

