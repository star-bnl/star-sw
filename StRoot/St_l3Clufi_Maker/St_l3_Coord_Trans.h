#ifndef St_l3_Coord_Trans_hh
#define St_l3_Coord_Trans_hh
#include <Rtypes.h>
#include <StObject.h>

class St_l3_Coord_Trans: public StObject {

private:
    // coordinates    
    Double_t* pa_ti_ro_se;   // pad time row coordinate
    Double_t* xyz_local;  // xyz in sector system
    Double_t* xyz_global; // xyz in global system

    // basic geometry
    static Int_t  numberOfPadsAtRow[45];
    static Double_t radialDistanceAtRow[45];
    static Double_t SectorSin[24];
    static Double_t SectorCos[24];
    static Double_t innerSectorPadPitch;
    static Double_t outerSectorPadPitch;
    static Double_t driftLength;
    static Double_t lengthPerTb;

public:
    // Constructors
    St_l3_Coord_Trans();
    ~St_l3_Coord_Trans();
    
    // Memberfunctions
    void raw_to_local();
    void local_to_global();
    Double_t* raw_to_global(Double_t* Pa_Ti_Ro);

    // Setter
    //void Set_pa_ti_ro(Double_t Pa_Ti_Ro[3]);

    // Root connection
    ClassDef(St_l3_Coord_Trans, 0)
};

#endif //St_l3_Coord_Trans_hh
