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
   char      id[64];      /* histo id */
   char      title[128];
   int       nEntries ;
   double    sum ;
   double    yMin ;
   double    yMax ;
   int       nBins;   /* Number of points assigned to that track */
   double    xMin ;
   double    xMax ;
   double    xStep ;
};

class gl3Histo {
public:
   gl3HistoHeader header ;
   double*   info ;
public:
   gl3Histo ( char iId[10]="id", char iTitle[100]="name", 
                   int iNBins=100,  double iXMin=0., double iXMax=100. ) ;
   ~gl3Histo ( ) ;
   int   Fill   ( double x, double weight ) ;
   double GetY   ( int iBin ) ;
   int   Print  ( short Level=1 ) ;
   int   Read   ( char* input  ) ; 
   int   Write  ( int maxBytes, char* output ) ; 

   ClassDef(gl3Histo,1)

};
#endif
