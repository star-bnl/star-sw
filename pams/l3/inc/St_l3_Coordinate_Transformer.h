//:>-----------------------------------------------------------------
//: FILE:       St_l3_Coordinate_Transformer.h
//: HISTORY:
//:            may2000 version 1.00
//:          29jun2000 ppy local_to_raw (int row, ... ) added
//:          29jun2000 ppy local_to_raw (int row, ... ) added
//:          29jun2000 ppy add methods to access SectorSin, SectorCos and radialDistanceAtRow
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       St_l3_Coordinate_Transformer
//: DESCRIPTION: Transforms coordinates from/to: global, local and raw
//: AUTHOR:      dfl - Dominik Flierl, flierl@bnl.gov  
//:>------------------------------------------------------------------

#ifndef St_l3_Coordinate_Transformer_hh
#define St_l3_Coordinate_Transformer_hh
#include "St_l3_Coordinates.h"
//#include <iostream.h>
#include <math.h>

//#define OFFLINE

class St_l3_Coordinate_Transformer {

private:
   
    // basic geometry fixed here ... this could be also taken from db
    static int    numberOfPadsAtRow[45];
    static double radialDistanceAtRow[45];
    static double SectorSin[24];
    static double SectorCos[24];
    static double innerSectorPadPitch;
    static double outerSectorPadPitch;

    // parameters of the transformation in different regions of the tpc 
    // east means sector 13-24, west means 1-12 
    double drift_length_inner ;
    double drift_length_outer ;
    double lengthPerTb;

    // max tb
    double max_tb_inner;
    double max_tb_outer;
    int transformation_errors;

public:
    // Constructors
    St_l3_Coordinate_Transformer() ;  // Init parameters and variables
    virtual ~St_l3_Coordinate_Transformer() ; // Delete variables
    
    // Memberfunctions
    // raw -> global
    void raw_to_global(const St_l3_ptrs_Coordinate &raw ,St_l3_xyz_Coordinate &global ) ;
    void raw_to_local(const St_l3_ptrs_Coordinate &raw ,St_l3_xyz_Coordinate &local ) ;
    void local_to_global(const St_l3_ptrs_Coordinate &raw ,const St_l3_xyz_Coordinate &local ,St_l3_xyz_Coordinate &global ) ;

    // global -> raw
    void global_to_raw(const St_l3_xyz_Coordinate &global , St_l3_ptrs_Coordinate &raw ) ;
    void global_to_raw(int sector, int row, const St_l3_xyz_Coordinate &global , St_l3_ptrs_Coordinate &raw ) ;
    void global_to_local(const St_l3_xyz_Coordinate &global, St_l3_xyz_Coordinate &local ,St_l3_ptrs_Coordinate &raw) ;
    void global_to_local(int sector, int row, 
                         const St_l3_xyz_Coordinate &global, St_l3_xyz_Coordinate &local ) ;
    void local_to_raw(const St_l3_xyz_Coordinate &global , const St_l3_xyz_Coordinate &local , St_l3_ptrs_Coordinate &raw ) ; 
    void St_l3_Coordinate_Transformer::local_to_raw( int row ,const St_l3_xyz_Coordinate &local ,
                                                              St_l3_ptrs_Coordinate &raw  ) ;

    //
    //   Get parameters
    double GetSectorCos ( int i ) { if ( i < 0 || i >= 24 ) return -999. ;
                                    return SectorCos[i] ; } ;
    double GetSectorSin ( int i ) { if ( i < 0 || i >= 24 ) return -999. ;
                                    return SectorSin[i] ; } ;
    double GetRadialDistanceAtRow ( int i ) { if ( i < 0 || i >= 45 ) return -999. ;
                                            return radialDistanceAtRow[i] ; } ;

     // Set paramaters needed for the transformation in different ways
    // This is only for the transformation z <-> timebucket the others are fixed
    void Set_parameters_by_hand() ;
    void Set_parameters_by_hand(const double lengthPerTb, const double drift_length_inner, const double drift_length_outer) ;
    void Get_parameters_from_db() ;
    void Use_transformation_provided_by_db() ;
    void Print_parameters() ;
    double Get_drift_length_inner() { return drift_length_inner ;} ;
    double Get_drift_length_outer() { return drift_length_outer ;} ;
    double Get_lengthPerTb() { return lengthPerTb ; } ;
    int Get_transformation_errors() { return transformation_errors; } ;

    #ifdef OFFLINE
    // Root connection
    ClassDef(St_l3_Coordinate_Transformer, 0)
    #endif
};

#endif //St_l3_Coordinate_Transformer_hh
