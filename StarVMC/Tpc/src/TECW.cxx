#include "TECW.h"

const Float_t TECW::GapWidI[2]   = {2.0*27.56, 2.0*28.92};
const Float_t TECW::GapWidO[2]   = {2.0*10.91, 2.0*46.74};
const Float_t TECW::GapHeit[2]   = {62.15,     66.47};
const Float_t TECW::GapRad[2]    = {87.6,      157.34};
const Float_t TECW::inwidth[2]   = {2.0*13.75, 64.92};
const Float_t TECW::ouwidth[2]   = {2.0*32.38, 103.25};
const Float_t TECW::height[2]    = {69.52,     71.51};
const Float_t TECW::ppdepth[2]   = {1.35+0.32, 0.95+0.32};
const Float_t TECW::asdepth[2]   = {6.99,      6.99};
const Float_t TECW::ggdepth[2]   = {1.0,       1.4};
const Float_t TECW::MWCdepth[2]  = {1.0,       1.4};
const Float_t TECW::boundary[2]  = {3.74,      3.74};
const Float_t TECW::Rcenter[2]   = {86.669,    157.488};
const Float_t TECW::MwcInn[2]    = {2*12.804,  2*32.202};
const Float_t TECW::MwcOut[2]    = {2*29.953,  2*49.351};
const Float_t TECW::MwcHei[2]    = {64.0,      64.0};
const Float_t TECW::MwcCent[2]   = {-1.669,    -0.093};
const Int_t   TECW::MwcNwir[2]   = {160,       160};
const Int_t   TECW::n[2]         = {5,         8};
const Int_t   TECW::nex[2]       = {0,         5};

const Float_t TECW::z[2][8]      = {{6.67,  18.22,  34.22,  50.22,  62.65 ,  0.0,    0.0,    0.0},
				    {8.216, 16.217, 24.218, 32.219, 40.220, 48.221, 56.222, 64.143}};

const Float_t TECW::dz[2][8]     = {{6.13,  15.05,  15.05,  15.05,   7.09,   0.0,    0.0,    0.0},
				    {7.05,   7.05,   7.05,   7.05,   7.05,   7.05,   7.05,   6.89}};

const Float_t TECW::xex[2][5]    = {{0.0,    0.0,    0.0,    0.0,    0.0},
				    {0.0,    0.0,    0.0,  -21.907, 21.907}};

const Float_t TECW::zex[2][5]    = {{0.0,    0.0,    0.0,    0.0,    0.0},
				    {6.69,  32.22,  40.22,  56.22,  56.22}};

const Float_t TECW::dxex[2][5]   = {{0.0,    0.0,    0.0,    0.0,    0.0},
				    {2.54,   0.476,  0.476,  3.175,  3.175}};

const Float_t TECW::dzex[2][5]   = {{0.0,    0.0,    0.0,    0.0,    0.0},
				    {4.00,   7.05,   7.05,   7.05,   7.05}};
