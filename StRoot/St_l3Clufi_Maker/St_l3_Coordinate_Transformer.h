#ifndef St_l3_Coordinate_Transformer_hh
#define St_l3_Coordinate_Transformer_hh
#include <Rtypes.h>
#include <StObject.h>
#include "St_l3_Coordinates.h"

class St_l3_Coordinate_Transformer: public StObject {

private:
  
    // coordinates    
    Double_t* pa_ti_ro_se;   // pad time row secotor coordinate
    Double_t* xyz_local;     // xyz in sector system
    Double_t* xyz_global;    // xyz in global system


    // basic geometry fixed here ... this could be also taken from db
    static Int_t    numberOfPadsAtRow[45];
    static Double_t radialDistanceAtRow[45];
    static Double_t SectorSin[24];
    static Double_t SectorCos[24];
    static Double_t innerSectorPadPitch;
    static Double_t outerSectorPadPitch;

    // parameters of the transformation in different regions of the tpc 
    // east means sector 1-12, west means 12-24 
    Double_t drift_length_inner_east;
    Double_t drift_length_outer_east;
    Double_t drift_length_inner_west;
    Double_t drift_length_outer_west;
    
    Double_t lengthPerTb;

public:
    // Constructors
    St_l3_Coordinate_Transformer() ;  // Init parameters and variables
    ~St_l3_Coordinate_Transformer() ; // Delete variables
    
    // Memberfunctions
    void raw_to_global(const St_l3_ptrs_Coordinate &raw ,St_l3_xyz_Coordinate &global ) ;
    void raw_to_local(const St_l3_ptrs_Coordinate &raw ,St_l3_xyz_Coordinate &local ) ;
    void local_to_global(const St_l3_ptrs_Coordinate &raw ,const St_l3_xyz_Coordinate &local ,St_l3_xyz_Coordinate &global ) ;		      
    // old style
    void raw_to_local();
    Double_t* raw_to_global(Double_t* Pa_Ti_Ro_Se);
    void local_to_global() ;
   

    // Set paramaters needed for the transformation in different ways
    // This is only for the transformation z <-> timebucket
    void Set_parameters_by_hand() ;
    void Get_parameters_from_db() ;
    void Use_transformation_provided_by_db() ;
    void Print_parameters() ;

    // Setter
    //void Set_pa_ti_ro(Double_t Pa_Ti_Ro[3]);

    // Root connection
    ClassDef(St_l3_Coordinate_Transformer, 0)
};

#endif //St_l3_Coordinate_Transformer_hh
