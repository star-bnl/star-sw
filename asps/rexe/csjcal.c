#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

void *G__findsym(char*);

#ifndef type_of_call
#define type_of_call
#endif

#ifdef F77_NAME
#define csjcal_ F77_NAME(csjcal,CSJCAL)
#define csaddr_ F77_NAME((csaddr,CSADDR)
#endif

typedef  int (type_of_call *fun0 )();
typedef  int (type_of_call *fun1 )(int*);
typedef  int (type_of_call *fun2 )(int*,int*);
typedef  int (type_of_call *fun3 )(int*,int*,int*);
typedef  int (type_of_call *fun4 )(int*,int*,int*,int*);
typedef  int (type_of_call *fun5 )(int*,int*,int*,int*,int*);
typedef  int (type_of_call *fun6 )(int*,int*,int*,int*,int*,int*);
typedef  int (type_of_call *fun7 )(int*,int*,int*,int*,int*,int*,int*);
typedef  int (type_of_call *fun8 )(int*,int*,int*,int*,int*,int*,int*,int*);
typedef  int (type_of_call *fun9 )(int*,int*,int*,int*,int*,int*,int*,int*,int*);
typedef  int (type_of_call *fun10)(int*,int*,int*,int*,int*,int*,int*,int*,int*,int*);
typedef  int (type_of_call *fun11)(int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*);
typedef  int (type_of_call *fun12)(int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*);

long int type_of_call csjcal_(
/************************************************************************/
/*                                                                      */
/*       csjcal_ F77 interface to call external routine       VP 960229 */
/*                    by pointer(address)                               */
/*                                                                      */
/*   ARGUMENTS:                                                         */
/*                                                                      */
long int (**fun)(),     /* addres of external routine,              	*/
int  *narg,             /* number   of arguments      			*/
...)			/* other narg arguments				*/
/*                                                                      */
/************************************************************************/
{

  int *a[50];
  int iarg;
  va_list ap;

  va_start(ap,narg);
  for(iarg=0; iarg < *narg ; iarg++) { a[iarg] = va_arg(ap,int*);};
  va_end(ap);


  switch (*narg) {

    case  0: return ((fun0) *fun)(); 
    case  1: return ((fun1) *fun)(a[0]); 
    case  2: return ((fun2) *fun)(a[0],a[1]); 
    case  3: return ((fun3) *fun)(a[0],a[1],a[2]); 
    case  4: return ((fun4) *fun)(a[0],a[1],a[2],a[3]); 
    case  5: return ((fun5) *fun)(a[0],a[1],a[2],a[3],a[4]); 
    case  6: return ((fun6) *fun)(a[0],a[1],a[2],a[3],a[4],a[5]); 
    case  7: return ((fun7) *fun)(a[0],a[1],a[2],a[3],a[4],a[5],a[6]); 
    case  8: return ((fun8) *fun)(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7]); 
    case  9: return ((fun9) *fun)(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8]); 
    case 10: return ((fun10)*fun)(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8],a[9]); 
    case 11: return ((fun11)*fun)(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8],a[9],a[10]); 
    case 12: return ((fun12)*fun)(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8],a[9],a[10],a[11]); 
   default: printf("*** CsJCall: Wrong narg=%d ***\n",*narg);exit(13);};

}

void type_of_call *csaddr_(char *name, int l77name)
{
  char bname[100],*c;
  void *a;int i;
  
  for (i=0;i<l77name && name[i] && !isspace(name[i]);i++){bname[i]=tolower(name[i]);}
  bname[i]='\0';
  
  a = G__findsym(bname);
  if (a) return a;
  strcat(bname,"_");
  a = G__findsym(bname);  
  return a;    
}
