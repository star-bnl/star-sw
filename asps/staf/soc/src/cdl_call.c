#include <stdio.h>

#ifndef __alpha
int 
cdl_calli_ (name,n,p)
 int (*(*name)) ();
 int *n;
#ifdef SGI      /* hjw.   SGI wants char* and not int* */
 char *p[16];   /* hjw.   SGI wants char* and not int* */
#else           /* hjw.   SGI wants char* and not int* */
 int  *p[16];   /* hjw.   SGI wants char* and not int* */
#endif          /* hjw.   SGI wants char* and not int* */
{
      switch (*n)
     {
        case 0:
    return( (*(*name))());
        case 1:
    return( (*(*name))(p[0]));
        case 2:
    return( (*(*name))(p[0],p[1] ));
        case 3:
    return( (*(*name))(p[0],p[1],p[2] ));
        case 4:
    return( (*(*name))(p[0],p[1],p[2],p[3] ));
        case 5:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4] ));
        case 6:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5] ));
        case 7:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6] ));
        case 8:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7] ));
        case 9:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8] ));
        case 10:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9] ));
        case 11:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10] ));
        case 12:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10],p[11] ));
        case 13:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10],p[11],p[12] ));
        case 14:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10],p[11],p[12],p[13] ));
        case 15:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14]));
        case 16:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14],p[15]));
       default:
        printf("\n More then 16 arguments in call users routine");
     }
  return 0;  /* added by hjw to keep compiler quiet */
}

float 
cdl_callr_ (name,n,p)
 float (*(*name)) ();
 int *n;
#ifdef SGI      /* hjw.   SGI wants char* and not int* */
 char *p[16];   /* hjw.   SGI wants char* and not int* */
#else           /* hjw.   SGI wants char* and not int* */
 int  *p[16];   /* hjw.   SGI wants char* and not int* */
#endif          /* hjw.   SGI wants char* and not int* */

{
      switch (*n)
     {
        case 0:
    return( (*(*name))());
        case 1:
    return( (*(*name))(p[0]));
        case 2:
    return( (*(*name))(p[0],p[1] ));
        case 3:
    return( (*(*name))(p[0],p[1],p[2] ));
        case 4:
    return( (*(*name))(p[0],p[1],p[2],p[3] ));
        case 5:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4] ));
        case 6:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5] ));
        case 7:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6] ));
        case 8:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7] ));
        case 9:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8] ));
        case 10:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9] ));
        case 11:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10] ));
        case 12:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10],p[11] ));
        case 13:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10],p[11],p[12] ));
        case 14:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10],p[11],p[12],p[13] ));
        case 15:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14]));
        case 16:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14],p[15]));
       default:
        printf("\n More then 16 arguments in call users routine");
     }
  return 0;  /* added by hjw to keep compiler quiet */
}

double 
cdl_calld_ (name,n,p)
 double (*(*name)) ();
 int *n;
#ifdef SGI      /* hjw.   SGI wants char* and not int* */
 char *p[16];   /* hjw.   SGI wants char* and not int* */
#else           /* hjw.   SGI wants char* and not int* */
 int  *p[16];   /* hjw.   SGI wants char* and not int* */
#endif          /* hjw.   SGI wants char* and not int* */
{
      switch (*n)
     {
        case 0:
    return( (*(*name))());
        case 1:
    return( (*(*name))(p[0]));
        case 2:
    return( (*(*name))(p[0],p[1] ));
        case 3:
    return( (*(*name))(p[0],p[1],p[2] ));
        case 4:
    return( (*(*name))(p[0],p[1],p[2],p[3] ));
        case 5:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4] ));
        case 6:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5] ));
        case 7:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6] ));
        case 8:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7] ));
        case 9:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8] ));
        case 10:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9] ));
        case 11:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10] ));
        case 12:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10],p[11] ));
        case 13:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10],p[11],p[12] ));
        case 14:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10],p[11],p[12],p[13] ));
        case 15:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14]));
        case 16:
    return( (*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
         ,p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14],p[15]));
       default:
        printf("\n More then 16 arguments in call users routine");
     }
  return 0;  /* added by hjw to keep compiler quiet */
}
#endif /* not __alpha */
#ifdef __alpha

#define cdl_calli
#undef  cdl_calli

#ifdef __osf__

#ifdef JMP_DEBUG
#define PRINTF(x)       printf x
#else
#define PRINTF(x)
#endif

/*
 * Note from Antonio Pastore - DJP
 *
 * This package HAS to be LINKED with the following options:
 *
 *	f77 -D 4000000 -T 2000000
 *
 */

int
cdl_calli_ (
	int *fptr,	/* Contains the address of target routine 	*/
	int *n,		/* number of aguments				*/
	unsigned p[])	/* array of pointers!				*/
{
#ifdef LINK_LOW
	int (*name)();

	name = (int (*)())*fptr;
#else
	int jumpad_();
	int (*name)();
	unsigned long ptr = (unsigned long)jumpad_;

	ptr += *fptr;
	name = (int (*)())ptr;
#endif

	switch (*n)
	{
	case 0:
		return((*name)());
	case 1:
		return((*name)(p[0]));
	case 2:
		return((*name)(p[0],p[1]));
	case 3:
		return((*name)(p[0],p[1],p[2]));
	case 4:
		return((*name)(p[0],p[1],p[2],p[3]));
	case 5:
		return((*name)(p[0],p[1],p[2],p[3],p[4]));
	case 6:
		return((*name)(p[0],p[1],p[2],p[3],p[4],p[5]));
	case 7:
		return((*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]));
	case 8:
		return((*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],
			p[7]));
	case 9:
		return((*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],
			p[7],p[8]));
	case 10:
		return((*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],
			p[7],p[8],p[9]));
	case 11:
		return((*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],
			p[7],p[8],p[9],p[10]));
	case 12:
		return((*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],
			p[7],p[8],p[9],p[10],p[11]));
	case 13:
		return((*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],
			p[7],p[8],p[9],p[10],p[11],p[12]));
	case 14:
		return((*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],
			p[7],p[8],p[9],p[10],p[11],p[12],p[13]));
	case 15:
		return((*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],
			p[7],p[8],p[9],p[10],p[11],p[12],p[13],
			p[14]));
	case 16:
		return((*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],
			p[7],p[8],p[9],p[10],p[11],p[12],p[13],
			p[14],p[15]));
	default:
		printf("\n More then 16 arguments in call users routine");
	}
}
#endif

#define cdl_callr
#undef  cdl_callr

#ifdef __osf__

#ifdef JMP_DEBUG
#define PRINTF(x)       printf x
#else
#define PRINTF(x)
#endif

/*
 * Note from Antonio Pastore - DJP
 *
 * This package HAS to be LINKED with the following options:
 *
 *	f77 -D 4000000 -T 2000000
 *
 * if LINK_LOW is defined!
 */

float
cdl_callr_ (fptr,n,p)
	int *fptr;
	int *n;
#ifdef SGI      /* hjw.   SGI wants char* and not int* */
 char *p[16];   /* hjw.   SGI wants char* and not int* */
#else           /* hjw.   SGI wants char* and not int* */
 int  *p[16];   /* hjw.   SGI wants char* and not int* */
#endif          /* hjw.   SGI wants char* and not int* */
{
#ifdef LINK_LOW
	float (*name)();
	name = (float (*)())*fptr;

#else
	int jumpad_();
	float (*name)();
	unsigned long ptr = (unsigned long)jumpad_;

	ptr += *fptr;
	name = (float (*)())ptr;
#endif

	switch (*n)
	{
	case 0:
		return( (*name)());
	case 1:
		return( (*name)(p[0]));
	case 2:
		return( (*name)(p[0],p[1] ));
	case 3:
		return( (*name)(p[0],p[1],p[2] ));
	case 4:
		return( (*name)(p[0],p[1],p[2],p[3] ));
	case 5:
		return( (*name)(p[0],p[1],p[2],p[3],p[4] ));
	case 6:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5] ));
	case 7:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6] ));
	case 8:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7] ));
	case 9:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8] ));
	case 10:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9] ));
	case 11:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10] ));
	case 12:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10],p[11] ));
	case 13:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10],p[11],p[12] ));
	case 14:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10],p[11],p[12],p[13] ));
	case 15:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14]));
	case 16:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14],p[15]));
	default:
		printf("\n More then 16 arguments in call users routine");
	}

}

#endif

#define cdl_calld
#undef  cdl_calld

#ifdef __osf__

#ifdef JMP_DEBUG
#define PRINTF(x)       printf x
#else
#define PRINTF(x)
#endif

/*
 * Note from Antonio Pastore - DJP
 *
 * This package HAS to be LINKED with the following options:
 *
 *	f77 -D 4000000 -T 2000000
 *
 * if LINK_LOW is defined!
 */

double
cdl_calld_ (fptr,n,p)
	int *fptr;
	int *n;
	int p[];
{
#ifdef LINK_LOW
	double (*name)();
	name = (double (*)())*fptr;
#else
	unsigned jumpad_();
	double (*name)();
	unsigned long ptr = (unsigned long)jumpad_;

	ptr += *fptr;
	name = (double (*)())ptr;
#endif

	switch (*n)
	{
	case 0:
		return( (*name)());
	case 1:
		return( (*name)(p[0]));
	case 2:
		return( (*name)(p[0],p[1] ));
	case 3:
		return( (*name)(p[0],p[1],p[2] ));
	case 4:
		return( (*name)(p[0],p[1],p[2],p[3] ));
	case 5:
		return( (*name)(p[0],p[1],p[2],p[3],p[4] ));
	case 6:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5] ));
	case 7:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6] ));
	case 8:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7] ));
	case 9:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8] ));
	case 10:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9] ));
	case 11:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10] ));
	case 12:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10],p[11] ));
	case 13:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10],p[11],p[12] ));
	case 14:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10],p[11],p[12],p[13] ));
	case 15:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14]));
	case 16:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14],p[15]));
	default:
		printf("\n More then 16 arguments in call users routine");
	}

}

#endif
#endif /* APLHA_OSF */

#include <varargs.h>

#define MAXARGS 16

int
cdl_fcalli_(fp, narg, va_alist)
int     (*(*fp))();
int        *narg;
va_dcl
{  
     va_list ap;
     char   *arglist[MAXARGS];
     int     i;

     va_start(ap);
     for ( i=0; i<*narg; i++)  arglist[i] = va_arg(ap, char *);
     va_end(ap);
     return cdl_calli_(fp, narg, arglist);
}

float
cdl_fcallr_(fp, narg, va_alist)
  float (*(*fp))();
  int      *narg;
va_dcl
{
     va_list ap;
     char   *arglist[MAXARGS];
     int     i;

     va_start(ap);
     for ( i=0; i<*narg; i++)  arglist[i] = va_arg(ap, char *);
     va_end(ap);
     return cdl_callr_(fp, narg, arglist);
}

double
cdl_fcalld_(fp, narg, va_alist)
 double (*(*fp))();
 int       *narg;
va_dcl
{
     va_list ap;
     char   *arglist[MAXARGS];
     int     i;

     va_start(ap);
     for ( i=0; i<*narg; i++)  arglist[i] = va_arg(ap, char *);
     va_end(ap);
     return cdl_calld_(fp, narg, arglist);
}

