#include "Bichsel.h"
dEdxParameterization *Bichsel::m_dEdxParameterization = 0;
ClassImp(Bichsel);
Bichsel::Bichsel() {
  if (! m_dEdxParameterization) 
    m_dEdxParameterization = new dEdxParameterization("bich",
						      5.07402529167365057e-01, // MostProbableZShift
						      5.07402529167365057e-01, // AverageZShft
						      9.16531837651389347e-01, // I70Shft
						      9.75432754685096048e-01  // I60Shift
						      );
}
