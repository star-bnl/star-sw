#include "SpinCuts.h"

ClassImp(SpinCuts);

SpinCuts::SpinCuts()
{
    z_vertex_min = -150.0;
    z_vertex_max = +150.0; 
    zgg_min = 0.0;
    zgg_max = 1.0; 
    adc_cut = 0;
    tower_et_cut = 3.00;
    eta_min = 1.10;
    eta_max = 1.95; 
    tower_deta = 1.0;
    tower_dphi = 1.0; 
} 

Bool_t SpinCuts::operator()( StEEmcPair &pair )
{
    if ( pair.vertex().Z() < z_vertex_min ) return false;
    if ( pair.vertex().Z() > z_vertex_max ) return false;

    if ( pair.zgg() < zgg_min ) return false;
    if ( pair.zgg() > zgg_max ) return false;

    return true; 
} 
