#include "gl3Hit.h"

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    Print a gl3Hit 
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void gl3Hit::print (  ){
   printf ( "%d %7.2f %7.2f %7.2f\n", rowSector, x, y, z );
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    Set a gl3Hit from a l3_cluster structure
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int gl3Hit::set ( int sector, l3_cluster* cluster ){

   double pad  = double(cluster->pad)/64.;
   double time = double(cluster->time)/64.;

   readoutBoard    = cluster->RB_MZ / 16 ;
   if ( readoutBoard > 2 ) sector++ ; // when two sectors in one crate
                                      // last 3 readout boards belong to next sector
   mezzanninneCard = cluster->RB_MZ%16 ; 

   rowSector  = sector * 100 + cluster->padrow ;
   double xx, yy, zz ;
   
   rawToGlobal(sector, (int)cluster->padrow, pad, time, &xx, &yy, &zz);

   x = xx ;
   y = yy ;
   z = zz ;

   trackId = cluster->trackId ;
   flags   = cluster->flags   ;

   nextHit = 0 ; // needed for the linked hit list

   charge          = cluster->charge ;

   return 0 ;
}
