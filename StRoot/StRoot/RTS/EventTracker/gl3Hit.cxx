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

   // printf("tpx: %lf\n", z);

   //printf("tpx: %d\n", rowSector);
   return 0 ;
}

void gl3Hit::setITPCHit(l3CoordinateTransformer *trans, int sec, int row, double pad, double tb, unsigned short charge, unsigned short flags) {

    int pads = 2 * (row + 25 - (int)((double)row/7.0));
    int padpos = pad - pads/2;

    l3ptrsCoordinate raw;
    l3xyzCoordinate local;
    l3xyzCoordinate global;

    raw.Sets(sec);
    local.Setx(padpos * .5);
    local.Sety(55.80 + 1.6 * (row - 1));
    local.Setz(trans->drift_length_inner - tb * trans->lengthPerTb);
 
    trans->local_to_global(raw, local, global);
    
    x = global.Getx();
    y = global.Gety();
    z = global.Getz();



    //printf("drift_length_inner = %lf  length_per_tb = %lf  drift_length_outer = %lf\n",
    //	   trans->drift_length_inner, trans->lengthPerTb, trans->drift_length_outer);
    //printf("itpc: %lf\n", z);

    rowSector  = sec * 100 + row ;
    //printf("itpc: %d\n", rowSector);

    this->flags = flags;
    this->charge = charge;
}
