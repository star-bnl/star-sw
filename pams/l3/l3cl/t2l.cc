/**:--------------------------------------------------------------------
**: FILE:       t2l.cc
**: HISTORY:
**:           7/06/99  ppy initial version
**:--------------------------------------------------------------------*/
#include <time.h>
#include <stdio.h>
#include <memory.h>
#include <math.h>
#include <string.h>

#include "l3Point.h"
#include "t2l.h"

size_t sizeInt     = sizeof(int) ;
size_t sizeCluster = sizeof(l3Cluster) ;

/*:-------------------------------------------------------------------
**: ROUTINE:     hit2Cluster
**: DESCRIPTION: Fills hit online buffer from l3Point array
**:
**: AUTHORS:     Pablo Yepes, yepes@rice.edu
**:
**: ARGUMENTS:
**:        IN:
**:              nHits    :  # hits in array
**:              hit      :  hit(point) array
**:       OUT:
**:              maxBytes :  maximum buffer length 
**:              buff     :  pointer to buffer
**:           
**: RETURNS:    Number of bytes put in buffer
**:-------------------------------------------------------------------*/
int hit2Cluster ( int nHits, l3Point* hit, int maxBytes, int* buff ) {
//
   long const maxHits = 10000;
   long lastRow = 0 ;
   long clusterCounter = 0 ;
   long row ;
   long rowSize, overflow ;
 
   if ( nHits > maxHits ) return 1 ;
   l3Cluster cluster[maxHits] ;
   int nRows = 0 ;
   char *cP = (char *)buff ;
   char *cPFirst, *cPLast ;
   cPFirst = cP ;
   cPLast  = cP + maxBytes ;
   cP+=sizeInt;
//
   for ( int i = 0 ; i < nHits ; i++ ) {
      row = hit[i].Row() ;
      if ( row > hit[0].Row() && row != lastRow ) {
         if ( cP > cPLast ) {
            printf ( " Buffer overflow \n " ) ;
            break ;
         }
         *((int *)cP ) = lastRow ;
         cP+=sizeInt;
         *((int *)cP ) = clusterCounter ;
         cP+=sizeInt;
         memcpy ( (void *)cP, (void *)cluster, clusterCounter*sizeCluster ) ;
         rowSize = clusterCounter*sizeCluster ; 
         overflow    = fmod(rowSize,4);
         if ( overflow != 0 ) rowSize += 4 - overflow ;
         cP += rowSize ;
//         printf ( " row %d with %d pads \n ", lastRow, clusterCounter ) ;
         clusterCounter = 0 ;
         nRows++ ;
      }
      lastRow = row ;
//
      l3Cluster clu = hit[i].toCluster();
      cluster[clusterCounter] = clu ;
      clusterCounter++ ;
   }
//
   if ( cP > cPLast ) {
      printf ( " Buffer overflow \n " ) ;
      return  (int)(cP - cPFirst ) ;
   }
   buff[0] = nRows + 1 ;
   *((int *)cP ) = lastRow ;
   cP+=sizeInt;
   *((int *)cP ) = clusterCounter ;
   cP+=sizeInt;
   memcpy ( (void *)cP, (void *)cluster, clusterCounter*sizeCluster ) ;
   rowSize = clusterCounter*sizeCluster ; 
   overflow    = fmod(rowSize,4);
   if ( overflow != 0 ) rowSize += 4 - overflow ;
   cP += rowSize ;
// printf ( " Row %d: %d clusters written \n ", lastRow, clusterCounter ) ;
//
   int nBytes = (int)(cP - cPFirst) ;
   return nBytes ;
} 
/*:-------------------------------------------------------------------
**: ROUTINE:     cluster2Hit
**: DESCRIPTION: Fills hit l3Point array from online buffer
**:
**: AUTHORS:     Pablo Yepes, yepes@rice.edu
**:
**: ARGUMENTS:
**:        IN:
**:              nBytes   :  # bytes in buffer
**:              buff     :  pointer to buffer
**:       OUT:
**:              maxHit   :  maximum # hits in array
**:              hit      :  hit(point) array
**: 
**: RETURNS:    Number of hits(points)  
**:-------------------------------------------------------------------*/

int cluster2Hit ( int nBytes, int* buff, int maxHit, l3Point* hit  ) {
//
   int counter = 0 ;
   l3Cluster* pCluster ;
   int rowSize, overflow ;
   int row, nPads ;
   char *cP = (char *)buff ;
   char *cPFirst = cP ;
   char *cPLast = cP + nBytes ; 
//
   int nRows = *((int *)cP ) ;
   cP+=4;
   //
   while ( cP < cPLast ) {
      row = *((int *)cP ) ;
      cP+=4;
      nPads = *((int *)cP ) ;
//    printf ( " Row %d with %d pads \n ", row, nPads ) ;
      cP+=4;
      pCluster = (l3Cluster *)(cP) ;
      for ( int i = 0 ; i < nPads ; i++ ) {
//         printf ( " cluster %d ",i ) ; pCluster[i].print();
         hit[counter].set ( row, pCluster[i] ) ;
         counter++;
         if ( counter > maxHit ) {
            printf ( " Hit array too small \n " ) ;
	    break ;
	 }
      }
      if ( counter > maxHit ) break ;
      rowSize = nPads*sizeCluster ; 
      overflow    = fmod(rowSize,4);
      if ( overflow != 0 ) rowSize += 4 - overflow ;
      cP+=rowSize ;
   }
   return counter;
}
