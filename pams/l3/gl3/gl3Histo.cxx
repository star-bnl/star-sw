//:>------------------------------------------------------------------
//: FILE:       gl3Tracks.h
//: HISTORY:
//:              6dec1999 version 1.00
//:<------------------------------------------------------------------
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include "gl3Histo.h"

#ifdef SL3ROOT
ClassImp(gl3Histo)
#endif


//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//####################################################################################
gl3Histo::gl3Histo ( char iId[10], char iTitle[100], 
      long iNBins,  double iXMin, double iXMax ) {
   info = 0 ;
   if ( iNBins < 0 ) {
      fprintf ( stderr, " %d bins, not valid value \n", (int)iNBins );
      return ;
   }
   header.nBins = iNBins ;
   if ( iXMin > iXMax  ) {
      fprintf ( stderr, " xMin %e xMax %e, not valid values \n", iXMin, iXMax );
      return ;
   }
   strcpy ( header.id, iId ) ;
   strcpy ( header.title, iTitle ) ;
   header.nBins    = iNBins ;
   header.xMin     = iXMin ;
   header.xMax     = iXMax ;
   header.nEntries = 0 ;
   header.sum      = 0 ;
   header.yMin     = header.yMax = 0. ;
   header.xStep    = (header.xMax-header.xMin)/float(header.nBins);
   info     = new double[header.nBins+2]; 
   memset ( info, 0, (header.nBins+2)*sizeof(double) ) ;
}
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//####################################################################################
gl3Histo::~gl3Histo ( ) {
   if ( info ) delete[] info ;
}
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//####################################################################################
long gl3Histo::Fill ( double x, double weight ) {
   long iBin = (long)((x-header.xMin)/header.xStep)+1 ;
   if ( iBin < 1 ) iBin = 0 ;
   else if ( iBin > header.nBins ) iBin = header.nBins + 1 ; 
   info[iBin] += weight ;
   if ( iBin > 1 && iBin < header.nBins + 1 ) {
      if ( info[iBin] > header.yMax ) header.yMax = info[iBin] ;
      if ( info[iBin] < header.yMin ) header.yMin = info[iBin] ;
   }
   header.sum += weight ;
   header.nEntries++ ;
   return 0 ;
}
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//####################################################################################
double gl3Histo::GetY ( long iBin ) {
   if ( iBin < 0 || iBin > header.nBins+2 ) return 0 ;
   return info[iBin] ;
}
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//####################################################################################
long gl3Histo::Print ( short level  ) {
   printf ( "gl3Histo::Print: id %s title %s nBins %d \n", 
             header.id, header.title, (int)header.nBins ) ;
   return 0 ;
}
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//####################################################################################
long gl3Histo::Read ( char* input ) {
   char* contents = input  ;
   size_t length_1 = sizeof(gl3HistoHeader); 
   memcpy ( this, input, length_1 ) ;
// printf ( "gl3Histo::Read nBins %d\n",nBins ) ;
   contents = contents+ length_1 ;
   size_t length_2 = (header.nBins+2)*sizeof(double) ;
   if ( info != 0 ) delete[] info ;
   info = new double[header.nBins+2] ;
   memcpy ( info, contents, length_2 ) ; 
// printf ( "length 1 2 %d %d \n",length_1, length_2 ) ;
   return length_1 + length_2 ;
}
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//####################################################################################
long gl3Histo::Write ( char* output ) {
   char* contents  = output  ;
   size_t length_1 = sizeof(gl3HistoHeader) ; 
   size_t length_2 = (header.nBins+2)*sizeof(double) ; 
// printf ( "length 1 2 %d %d \n",length_1, length_2 ) ;
   memcpy ( contents, this, length_1 ) ;
   contents = contents + length_1 ;
   memcpy ( contents, info, length_2 ) ;
   return length_1 + length_2 ;
}
