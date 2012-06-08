#include "gl3Hit.h"

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    Print a gl3Hit 
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void gl3Hit::print (  ){
  printf ( "%d %d %d %7.2f %7.2f %7.2f %7.2f\n", rowSector, mezzanninneCard, readoutBoard, x, y, z,sqrt(x*x + y*y) );
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    Set a gl3Hit from a l3_cluster structure
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int gl3Hit::set ( l3CoordinateTransformer* transformer,
                  int sector, l3_cluster* cluster ){

   l3xyzCoordinate XYZ(0,0,0) ;
   l3ptrsCoordinate PTRS(0,0,0,0) ;

   double pad  = double(cluster->pad)/64.;
   double time = double(cluster->time)/64.;

   //printf("pad=%7.2lf time=%7.2lf padrow=%d sector=%d\n",pad,time,cluster->padrow,sector);

   readoutBoard    = cluster->RB_MZ / 16 ;
   if ( readoutBoard > 5  && sector%2==1) sector++ ; // when two sectors in one crate
                                      // last 3 readout boards belong to next sector
   mezzanninneCard = cluster->RB_MZ%16 ; 

   rowSector  = sector * 100 + cluster->padrow ;


   //printf("--->pad=%7.2lf time=%7.2lf padrow=%d sector=%d\n",pad,time,cluster->padrow,sector);


// rawToGlobal(sector, (int)cluster->padrow, pad, time, &xx, &yy, &zz);
 
   PTRS.Setptrs((double)pad, (double)time,(double)(cluster->padrow+1), (double) sector) ;
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
