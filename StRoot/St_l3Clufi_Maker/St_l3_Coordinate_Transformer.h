#ifndef St_l3_Coordinate_Transformer_hh
#define St_l3_Coordinate_Transformer_hh
#include "St_l3_Coordinates.h"

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
    void global_to_local(const St_l3_xyz_Coordinate &global, St_l3_xyz_Coordinate &local ,St_l3_ptrs_Coordinate &raw) ;
    void local_to_raw(const St_l3_xyz_Coordinate &local , St_l3_ptrs_Coordinate &raw ) ; 
    
    // Set paramaters needed for the transformation in different ways
    // This is only for the transformation z <-> timebucket the others are fixed
    void Set_parameters_by_hand() ;
    void Get_parameters_from_db() ;
    void Use_transformation_provided_by_db() ;
    void Print_parameters() ;

    #ifdef OFFLINE
    // Root connection
    ClassDef(St_l3_Coordinate_Transformer, 0)
    #endif
};

#endif //St_l3_Coordinate_Transformer_hh
