#include "Stl3Util/gl3/gl3Hit.h"

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    Print a gl3Hit 
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void gl3Hit::print (  ){
   printf ( "%d %7.2f %7.2f %7.2f\n", rowSector, x, y, z );
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    Set a gl3Hit from a l3_cluster structure
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int gl3Hit::set ( St_l3_Coordinate_Transformer* transformer,
                  int sector, l3_cluster* cluster ){

   St_l3_xyz_Coordinate XYZ(0,0,0) ;
   St_l3_ptrs_Coordinate PTRS(0,0,0,0) ;

   double pad  = double(cluster->pad)/64.;
   double time = double(cluster->time)/64.;

   readoutBoard    = cluster->RB_MZ / 16 ;
   if ( readoutBoard > 5  && sector%2==1) sector++ ; // when two sectors in one crate
                                      // last 3 readout boards belong to next sector
   mezzanninneCard = cluster->RB_MZ%16 ; 

   rowSector  = sector * 100 + cluster->padrow ;
   
// rawToGlobal(sector, (int)cluster->padrow, pad, time, &xx, &yy, &zz);
 
   PTRS.Setptrs((double)pad, (double)time,(double)(cluster->padrow), (double) sector) ;
   transformer->raw_to_global(PTRS,XYZ) ;

   x = XYZ.Getx();
   y = XYZ.Gety();
   z = XYZ.Getz();

   trackId = cluster->trackId ;
   flags   = cluster->flags   ;

   nextHit = 0 ; // needed for the linked hit list

   charge          = cluster->charge ;

   return 0 ;
}
