#include <Rtypes.h>

class  TECW {             //  TPC basic dimensions
 public:

  static const Float_t GapWidI[2]   ;    // air in support wheel - inner width
  static const Float_t GapWidO[2]   ;    // air in support wheel - outer width
  static const Float_t GapHeit[2]   ;    // air in support wheel - height (dr)
  static const Float_t GapRad[2]    ;    // air in support wheel - center radius
  static const Float_t inwidth[2]   ;    // sector width at inner radius
  static const Float_t ouwidth[2]   ;    // sector width at outer radius
  static const Float_t height[2]    ;    // sector radial height
  static const Float_t ppdepth[2]   ;    // padplane thickness (both Al and PCB)
  static const Float_t asdepth[2]   ;    // depth of openings in aluminum structure
  static const Float_t ggdepth[2]   ;    // MWC gap from gated grid to pad plane
  static const Float_t MWCdepth[2]  ;    // sensitive MWC gas gap full thickness
  static const Float_t boundary[2]  ;    // al frame - boundary width
  static const Float_t Rcenter[2]   ;    // sector center radius (set by precision holes)
  static const Float_t MwcInn[2]    ;    // MWC sensitive region inner size
  static const Float_t MwcOut[2]    ;    // MWC sensitive region outer size
  static const Float_t MwcHei[2]    ;    // MWC sensitive region height (radial)
  static const Float_t MwcCent[2]   ;    // sensitive region center position
  static const Int_t   MwcNwir[2]   ;    // number of MWC sensitive wires
  static const Int_t   n[2]         ;    // number of air gaps in Al
  static const Int_t   nex[2]       ;    // number of extra aluminum support pieces
  static const Float_t z[2][8]      ;    // positions of air gaps
  static const Float_t dz[2][8]     ;    // size of air gaps
  static const Float_t xex[2][5]    ;    // x positions of extra aluminum
  static const Float_t zex[2][5]    ;    // z positions of extra aluminum
  static const Float_t dxex[2][5]   ;    // x-thickness of extra aluminum
  static const Float_t dzex[2][5]   ;    // z-thickness of extra aluminum

};
