/*
* $Id: idisp.c,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Name:  $
* $Log: idisp.c,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.2  2001/03/05 11:55:22  nevski
* headers clean-up
*
* Revision 1.1  2001/02/27 10:15:17  nevski
*  first working release
*/
/*CMZ :          28/05/2000  14.36.45  by  Pavel Nevski*/
/*-- Author :    Pavel Nevski   01/09/99*/
/* provide displacements for pgf77 simulation */
int idisp0_ (a,b)   char  *a,*b;     { return  b-a;    }
int idisp1_ (a,b)   char  *a,*b;     { return (b-a)+1; }
int idisp2_ (a,b)   short *a,*b;     { return (b-a)+1; }
int idisp4_ (a,b)   int   *a,*b;     { return (b-a)+1; }
int iponter_(a,b)   int   *a,**b;    { return (*b-a);  }

