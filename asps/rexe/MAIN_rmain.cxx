//*CMZ :  2.00/00 05/03/98  03.53.49  by  Fons Rademakers
//*CMZ :  1.03/01 12/08/97  15.37.03  by  Valery Fine(fine@vxcern.cern.ch)
//*-- Author :    Fons Rademakers   02/03/95
 
//*KEEP,CopyRight,T=C.
/*************************************************************************
 * Copyright(c) 1995-1998, The ROOT System, All rights reserved.         *
 * Authors: Rene Brun, Nenad Buncic, Valery Fine, Fons Rademakers.       *
 *                                                                       *
 * Permission to use, copy, modify and distribute this software and its  *
 * documentation for non-commercial purposes is hereby granted without   *
 * fee, provided that the above copyright notice appears in all copies   *
 * and that both the copyright notice and this permission notice appear  *
 * in the supporting documentation. The authors make no claims about the *
 * suitability of this software for any purpose.                         *
 * It is provided "as is" without express or implied warranty.           *
 *************************************************************************/
//*KEND.
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// RMain                                                                //
//                                                                      //
// Main program used to create RINT application.                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
//*KEEP,TROOT.
#include "TROOT.h"
//*KEEP,TRint.
#include "TRint.h"
//*KEND.
int        __argc_save=0;       // Pgf77
char **    __argv_save=NULL;    //

int xargc=0;                    // g77
char **xargv=NULL;              // g77

int f77argc = 0;                // For mips Fortran
char **f77argv=NULL;            //
 
extern void InitGui();
VoidFuncPtr_t initfuncs[] = { InitGui, 0 };
 
 
TROOT root("Rint","The ROOT Interactive Interface", initfuncs);
 
//______________________________________________________________________________
int main(int argc, char **argv)
{
#ifndef WIN32
   char appname[] = "Rint";
#else
   char appname[] = "Root_Rint";
#endif
 __argc_save=argc;  __argv_save=argv; 
       xargc=argc;      xargv = argv;
   f77argc = argc;    f77argv = argv;

   TRint *theApp = new TRint(appname, &argc, argv, 0, 0);
 
   // Init Intrinsics, build all windows, and enter event loop
   theApp->Run();
 
   delete theApp;
 
   return(0);
}
