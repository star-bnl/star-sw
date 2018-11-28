/*
 * $Id: cscali.c,v 1.1 2018/11/19 23:20:11 perev Exp $
 *
 * $Log: cscali.c,v $
 * Revision 1.1  2018/11/19 23:20:11  perev
 * 64bits new comis files added from /CERN
 *
 * Revision 1.4  2004/10/22 12:42:40  couet
 * - for consistency with cscalr.c, this function has been modified the same way
 *
 * Revision 1.3  1997/03/14 12:02:24  mclareni
 * WNT mods
 *
 * Revision 1.2.2.1  1997/01/21 11:34:43  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.2  1996/03/14 13:44:11  berezhno
 * mods for WINNT
 *
 * Revision 1.1.1.1  1996/02/26 17:16:55  mclareni
 * Comis
 *
 */
#include "comis/pilot.h"
#if !defined(CERNLIB_ALPHA_OSF)
/*CMZ :          22/11/95  10.39.30  by  Julian Bunn*/
/*-- Author :*/

#define cscali
#undef  cscali
int csToken(unsigned long fun);
unsigned long  csPoter( int token); 
char*  getPntB(int);

int cscali_ (fptr,n,pp)
 int *fptr;	/* Contains the address of target routine 	*/
 int *n;
 int pp[15];
{
   int (*name)();
   name = (int (*)())csPoter(*fptr);
   void *p[15]; int j;
   for (j=0;j<*n;j++) {
     p[j] = 0; if (!pp[j]) continue;
     p[j] = getPntB(pp[j]);
   }
   int i;
   switch (*n)
   {
      case 0:
         i = ((*(*name))());
         break;
      case 1:
         i = ((*(*name))(p[0]));
         break;
      case 2:
         i = ((*(*name))(p[0],p[1]));
         break;
      case 3:
         i = ((*(*name))(p[0],p[1],p[2]));
         break;
      case 4:
         i = ((*(*name))(p[0],p[1],p[2],p[3]));
         break;
      case 5:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4]));
         break;
      case 6:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5]));
         break;
      case 7:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]));
         break;
      case 8:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7]));
         break;
      case 9:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8]));
         break;
      case 10:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9]));
         break;
      case 11:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10]));
         break;
      case 12:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11]));
         break;
      case 13:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12]));
         break;
      case 14:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12],p[13]));
         break;
      case 15:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14]));
         break;
      case 16:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14],p[15]));
         break;
      default:
         printf("\n More then 16 arguments in call users routine");
   }
   return i;
}
#endif
