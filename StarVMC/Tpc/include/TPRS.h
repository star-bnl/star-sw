#include <Rtypes.h>

class  TPRS {             //  sector of padrows
 public:

  static const Int_t   nRow[2]      ;    // number of padrows in the sector
  static const Float_t pitch[2]     ;    // tpc padrow pitch width
  static const Float_t width[2]     ;    // tpc padrow thickness
  static const Int_t   super[2]     ;    // number of padraws in a superpadrow
  static const Int_t   Npads[2][32] ;    // number of pads in row
  static const Float_t Rpads[2][32] ;    // tpc padrow radii
};
