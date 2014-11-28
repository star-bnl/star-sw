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
   header.maxBin   = 0;
   
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
	if ( info[iBin] > header.yMax ) {
	    header.yMax = info[iBin] ;
	    header.maxBin= iBin ;       
	}
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
   if ( iBin < 0 || iBin > header.nBins+2 ) {
       printf("out of range\n");
       return 0 ;
   }
   return info[iBin] ;
}
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//####################################################################################
int gl3Histo::Print ( short level  ) {
   printf ( "gl3Histo::Print: id \"%s\" title \"%s\" nBins %d nEntries %d\n", 
             header.id, header.title, 
	    (int)header.nBins, (int)header.nEntries ) ;
   return 0 ;
}
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//#####################################################################
int gl3Histo::Read ( char* input ) {

//     printf("%x / %x / %x\n",
// 	   sizeof(gl3HistoHeader),
// 	   ((gl3HistoHeader*)input)->nBins,
// 	   ( sizeof(gl3HistoHeader) + 
// 	     sizeof(double) * (((gl3HistoHeader*)input)->nBins+2)));
	   
    memcpy ( &header, input, sizeof(gl3HistoHeader) ) ;
    
    if ( info ) delete info ;
    info     = new double[header.nBins+2]; 
    
    memcpy ( info, input+sizeof(gl3HistoHeader), 
	     (header.nBins+2)*sizeof(double) ) ;
    
    // printf ( "length 2 %d \n", length_2 ) ;
    
//     for (int i=0; i<header.nBins+2; i++) {
// 	printf("%d: %f\n", i, info[i]);
//     }
    
    return sizeof(gl3HistoHeader) + (header.nBins+2)*sizeof(double);
}
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//##########################################################################
int gl3Histo::Reset (  ) {
   header.nEntries = 0 ;
   header.sum      = 0 ;
   header.maxBin   = 0;
   header.yMin     = 0;
   header.yMax     = 0;
   memset ( info, 0, (header.nBins+2)*sizeof(double) ) ;
   return 0 ;
}

//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//#########################################################################
int gl3Histo::Write ( unsigned int maxBytes, char* output ) {

    if ( (sizeof(gl3HistoHeader) + (header.nBins+2)*sizeof(double)) 
	 >= maxBytes ) {
	
	printf("gl3Histo::Write %d bytes in buffer not enough \n", maxBytes);
	return 0 ;
    }
   
    memcpy ( output, &header, sizeof(gl3HistoHeader) ) ;
    memcpy ( output + sizeof(gl3HistoHeader), info,
	     (header.nBins+2)*sizeof(double) );

//     for (int i=0; i<header.nBins+2; i++) {
// 	printf("%d: %f\n", i, info[i]);
//     }

    return sizeof(gl3HistoHeader) + (header.nBins+2)*sizeof(double);
}

// JB 08/15/2K added some methods
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//####################################################################################
double gl3Histo::GetMaximum()
{
 return header.yMax;
}

//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//####################################################################################
int gl3Histo::GetMaximumBin()
{
 return header.maxBin;
}

//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//####################################################################################
double gl3Histo::GetBinCenter(int Bin)
{
 return(header.xMin+(float(Bin-0.5)*header.xStep)) ;
}

//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//####################################################################################
double gl3Histo::Integral(int minBin, int maxBin)
{
      double sum=0;

      //if (minBin < 0) minBin = 0;
      //if (maxBin >= header.nBins+2) maxBin=header.nBins+1;

      if(minBin>=0 && maxBin<header.nBins) {
	    for(int cnt=minBin; cnt<=maxBin; cnt++) sum += GetY(cnt);
	    return sum;
      }
      else return 0;
}



//####################################################################
// calculates (fast) the fuckin weighted mean of a gl3Histo
//####################################################################
double gl3Histo::getWeightedMean(double sigmaWidthBins)
{
  // Weighted mean of Histo
  
  // suggestion
  // sigmaWidthBins = 4; 
  double HistoMax = GetMaximum();
  int HistoMaxBin = GetMaximumBin();
  double HistoMaxCenter = GetBinCenter(HistoMaxBin);
  
  double SigmaHalfL = Integral((int)(HistoMaxBin-sigmaWidthBins),HistoMaxBin-1);
  double SigmaHalfR = Integral(HistoMaxBin+1,(int)(HistoMaxBin+sigmaWidthBins));
  double SigmaHalfLCenter=0;
  
  for(int cnt=HistoMaxBin-1; cnt>=(HistoMaxBin-sigmaWidthBins); cnt--) {
    SigmaHalfLCenter = SigmaHalfLCenter + GetBinCenter(cnt);
  }
  SigmaHalfLCenter=SigmaHalfLCenter/sigmaWidthBins;
  
  double SigmaHalfRCenter=0;
  for(int cnt=HistoMaxBin+1; cnt<=(HistoMaxBin+sigmaWidthBins); cnt++) {
    SigmaHalfRCenter = SigmaHalfRCenter+GetBinCenter(cnt);
  }
  SigmaHalfRCenter=SigmaHalfRCenter/sigmaWidthBins;
  
  double weightedMean;
  if((SigmaHalfL+HistoMax+SigmaHalfR)>0) {
    weightedMean = ( (SigmaHalfL * SigmaHalfLCenter) + 
		     (HistoMax * HistoMaxCenter) + 
		     (SigmaHalfR * SigmaHalfRCenter) ) 
      / (SigmaHalfL+HistoMax+SigmaHalfR);
  }
  else weightedMean=0.0;  // CAUTION METHOD RETURNS 0.0 IF IT FAILS
  
  return(weightedMean);
}
