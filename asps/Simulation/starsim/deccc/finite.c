/*
* $Id: finite.c,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Log: finite.c,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.2  2001/03/05 11:55:21  nevski
* headers clean-up
*
* Revision 1.1  2001/02/27 10:15:17  nevski
*  first working release
*/
/*CMZ :  2.00/00 15/12/98  23.32.59  by  Pavel Nevski*/
/*-- Author :    Pavel Nevski   05/12/98*/
/*include <ieeefp.h>*/
  int finite  (double);
  int finite_ (float *r)  { double d; d=*r; return finite(d); }

