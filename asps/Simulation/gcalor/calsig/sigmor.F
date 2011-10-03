* $Id: sigmor.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      FUNCTION SIGMOR(EK,NMED)
C***************************************************
C    Get x-section for low energetic neutrons
C    Ek < 20 MeV (Ek is given in eV)
C  INPUT: material and neutron energy
C  OUTPUT: SIG = x-section
C
C**************************************************
C MICAP common
#include "geant321/mmicap.inc"
#include "geant321/mpoint.inc"
C
      CALL NSIGTA(EK,NMED,TSIG,D,D(LFP32),D(LFP33))
      SIGMOR = TSIG
      RETURN
      END
