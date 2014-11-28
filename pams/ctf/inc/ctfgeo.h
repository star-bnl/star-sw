#include "ctg_geo.h"
#include "ctg_slat_phi.h"
#include "ctg_slat_eta.h"
long ctg_index ( long i_phi, long i_eta, long n_eta ) ; 
long ctg_i_eta ( float z, 
                 TABLE_HEAD_ST *slat_eta_h, CTG_SLAT_ETA_ST  *slat_eta ) ;
long ctg_i_phi ( float phi,
                 TABLE_HEAD_ST *geo_h,      CTG_GEO_ST       *geo,   
                 TABLE_HEAD_ST *slat_phi_h, CTG_SLAT_PHI_ST  *slat_phi ) ;
