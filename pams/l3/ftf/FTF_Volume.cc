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
#include "FTFinder.h"
//
//    Some functions now
//
void ROW::print()
{
    for ( FTF_Hit *hit=first_hit ;
            hit != 0 ;
            hit = hit->nxrhit ) {

         printf( "\n Hit nr = %i\n Ir = %i",
                        hit->id, hit->i_r) ;
         printf( "\n Next should be %i", hit->nxrhit ) ;
    }
    return ;
}

//
//    Print volume now
//
void VOLUME::print(  )
{ 
    
    for ( FTF_Hit *hit = first_hit ;
       hit != 0 ;  hit = hit->nxvhit ) {

       printf( "\n Hit nr = %i\n Ir = %i\n Iphi = %i\n Ieta=%i" ,
               hit->id, hit->i_r, hit->i_phi, hit->i_eta) ;
       printf( "\n Next should be %i", hit->nxvhit ) ;
   }
   return ;
}


