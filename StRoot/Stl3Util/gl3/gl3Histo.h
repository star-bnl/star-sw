//:>------------------------------------------------------------------
//:<------------------------------------------------------------------
#include <stdio.h>
#include <math.h>

#ifndef GL3HISTO 
#define GL3HISTO 

class gl3HistoHeader {
 public:
   char      id[64]; 
   char      title[128]; 
   int       nEntries ;
   int       nBins;
   double    sum ;
   double    yMin ;
   double    yMax ;
   double    xMin ;
   double    xMax ;
   double    xStep ;
   // JB 08312K
   int      maxBin;
   int      padding; // needed for 64-bit alignment
};

class gl3Histo {
public:
   gl3HistoHeader header ;
   double*   info ;

public:
   gl3Histo ( char iId[10]="id", char iTitle[100]="name", 
                   int iNBins=100,  double iXMin=0., double iXMax=100. ) ;
   ~gl3Histo ( ) ;
   int   Fill   (double x, double weight=1) ;
   double GetY   (int iBin ) ;
   int   Print  (short Level=1 ) ;
   int   Read   (char* input  ) ; 
   int   Reset  (  ) ;
   int   Write  ( int maxBytes, char* output ) ; 

   // JB 08/15/2K added some methods
   // --------------------------------------------------------------------
   double GetMaximum();
   int GetMaximumBin();
   double GetBinCenter(int Bin);
   double Integral(int minBin, int maxBin);
   double getWeightedMean(double sigmaWidthBins=4);
};
#endif
