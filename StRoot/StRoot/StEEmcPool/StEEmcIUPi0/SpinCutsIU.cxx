#include "SpinCutsIU.h"

ClassImp(SpinCutsIU);

SpinCutsIU::SpinCutsIU()
{
    z_vertex_min = -150.0;
    z_vertex_max = +150.0; 
    zgg_min = 0.0;
    zgg_max = 1.0; 
    adc_cut = 0;
    tower_et_cut = 3.00;
    eta_min = 1.086;
    eta_max = 2.0; 
    //tower_deta = 0.0;
    //tower_dphi = 0.0; 
} 

Bool_t SpinCutsIU::operator()( StEEmcIUPair &pair )
{
    if ( pair.vertex().Z() < z_vertex_min ) return false;
    if ( pair.vertex().Z() > z_vertex_max ) return false;

    if ( pair.zgg() < zgg_min ) return false;
    if ( pair.zgg() > zgg_max ) return false;

    return true; 
} 
