//:>------------------------------------------------------------------
//: FILE:       gl3Tracks.h
//: HISTORY:
//:              6dec1999 version 1.00
//:<------------------------------------------------------------------
#include <stdio.h>
#include <math.h>


#ifndef GL3HISTO 
#define GL3HISTO 

#ifdef SL3ROOT
#include "Rtypes.h"
#else
#define ClassDef(a,b)
#endif

class gl3HistoHeader {
public:
   char      id[50];      /* histo id */
   char      title[100];
   long      nEntries ;
   double    sum ;
   double    yMin ;
   double    yMax ;
   long      nBins;   /* Number of points assigned to that track */
   double    xMin ;
   double    xMax ;
   double    xStep ;
};

class gl3Histo {
public:
   gl3HistoHeader header ;
   double*   info ;
public:
   gl3Histo ( char iId[10]="id", char iTitle[100]="default", 
                   long iNBins=100,  double iXMin=0., double iXMax=100. ) ;
   ~gl3Histo ( ) ;
   long   Fill   ( double x, double weight ) ;
   double GetY   ( long iBin ) ;
   long   Print  ( short Level=1 ) ;
   long   Read   ( char* input  ) ; 
   long   Write  ( char* output ) ; 

   ClassDef(gl3Histo,1)

};
#endif
