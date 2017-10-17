#ifndef __eemcGammaFilterMakerParams_st__
#define __eemcGammaFilterMakerParams_st__
#include "TDataSet.h"
#include "TTable.h"
#include "Ttypes.h"
/*	eemcGammaFilterMakerParams.idl
 *
 *	Table: eemcGammaFilterMakerParams
 *
 *	description: *	//:	parameters for the EEMC-based gamma filter maker
 */

struct eemcGammaFilterMakerParams_st {
	float eemcSamplingFraction; /* EEMc sampling fraction to be used in the calculations */
	float seedEnergyThreshold; /* seed tower energy threshold for foming 3x3 clusters */
	float clusterEtThreshold; /* total cluster E_T threshold */
	float maxVertexZ; /* vertex |z| limit */
	int   filterMode; /* filter enabled or disabled */
};
class St_eemcGammaFilterMakerParams : public TTable {
 public:
  ClassDefTable(St_eemcGammaFilterMakerParams,eemcGammaFilterMakerParams_st)
  ClassDef(St_eemcGammaFilterMakerParams,1) //C++ container for chain/makers status 
};

#endif /* __eemcGammaFilterMakerParams_st__ */
