//:>------------------------------------------------------------------
//: FILE:       FTF_Volume.cpp
//: HISTORY:
//:             28oct1996 version 1.00
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FTF_Volume
//: DESCRIPTION: Functions associated with this class
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include "FtfFinder.h"
//
//    Some functions now
//
void ROW::print()
{
    for ( FtfHit *hit=firstHit ;
            hit != 0 ;
            hit = hit->nextRowHit ) {

         printf( "\n Hit nr = %i\n Ir = %i",
                        hit->id, hit->row) ;
         printf( "\n Next should be %i", hit->nextRowHit ) ;
    }
    return ;
}
//
//    Print volume now
//
void VOLUME::print(  )
{ 
    
    for ( FtfHit *hit = firstHit ;
       hit != 0 ;  hit = hit->nextVolumeHit ) {

       printf( "\n Hit nr = %i\n Ir = %i\n Iphi = %i\n Ieta=%i" ,
               hit->id, hit->row, hit->phiIndex, hit->etaIndex ) ;
       printf( "\n Next should be %i", hit->nextVolumeHit ) ;
   }
   return ;
}


