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
      int iNBins,  double iXMin, double iXMax ) {
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
int gl3Histo::Fill ( double x, double weight ) {
   int iBin = (int)((x-header.xMin)/header.xStep)+1 ;
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
double gl3Histo::GetY ( int iBin ) {
   if ( iBin < 0 || iBin > header.nBins+2 ) return 0 ;
   return info[iBin] ;
}
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//####################################################################################
int gl3Histo::Print ( short level  ) {
   printf ( "gl3Histo::Print: id %s title %s nBins %d \n", 
             header.id, header.title, (int)header.nBins ) ;
   return 0 ;
}
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//####################################################################################
int gl3Histo::Read ( char* input ) {
   char* contents  = input  ;
   memcpy ( header.id, contents, 64*sizeof(char) ) ;
   contents += 64 * sizeof(char) ;
   memcpy ( header.title, contents, 128*sizeof(char) ) ;
   contents += 128 * sizeof(char) ;
   header.nEntries = *(int *)contents ;
   contents += sizeof(int) ;
   header.nBins = *(int *)contents ;
   contents += sizeof(int) ;
   header.sum =  *(double *)contents ;
   contents += sizeof(double);
   header.yMin = *(double *)contents ;
   contents += sizeof(double);
   header.yMax = *(double *)contents ;
   contents += sizeof(double);
   header.xMin = *(double *)contents ;
   contents += sizeof(double);
   header.xMax = *(double *)contents ;
   contents += sizeof(double);
   header.xStep = *(double *)contents ;
   contents += sizeof(double);
   size_t length_2 = (header.nBins+2)*sizeof(double) ; 
   memcpy ( info, contents, length_2 ) ;
   contents += (header.nBins+2)*sizeof(double);
// printf ( "length 2 %d \n", length_2 ) ;

   return (contents - input ) ;
}
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//####################################################################################
int gl3Histo::Write ( int maxBytes, char* output ) {
   char* contents  = output  ;

   int minBytes = 64 + 128 + 2 * sizeof(int) + 6 * sizeof(double) +
                  (header.nBins+2)*sizeof(double) ; 

   if ( minBytes >= maxBytes ) {
      printf ( "gl3Histo::Write %d bytes in buffer not enough \n", maxBytes ) ;
      return 0 ;
   }
   
   memcpy ( contents, header.id, 64*sizeof(char) ) ;
   contents += 64 * sizeof(char) ;
   memcpy ( contents, header.title, 128*sizeof(char) ) ;
   contents += 128 * sizeof(char) ;
   *(int *)contents = header.nEntries ;
   contents += sizeof(int) ;
   *(int *)contents = header.nBins    ;
   contents += sizeof(int) ;
   *(double *)contents = header.sum ;
   contents += sizeof(double);
   *(double *)contents = header.yMin ;
   contents += sizeof(double);
   *(double *)contents = header.yMax ;
   contents += sizeof(double);
   *(double *)contents = header.xMin ;
   contents += sizeof(double);
   *(double *)contents = header.xMax ;
   contents += sizeof(double);
   *(double *)contents = header.xStep;
   contents += sizeof(double);
   size_t length_2 = (header.nBins+2)*sizeof(double) ; 
   memcpy ( contents, info, length_2 ) ;
   contents += (header.nBins+2)*sizeof(double);
// printf ( "length 2 %d \n", length_2 ) ;

   return (contents - output ) ;
}
