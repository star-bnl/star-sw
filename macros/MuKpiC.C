// MuKpi for CuCu Run V
#define __CuCu__
#include "MuKpi.C"
void MuKpiC(const Char_t *files="*.MuDst.root", 
	    const Char_t *Out="MuKpi.root"
#ifdef __TCFIT__
	    , Double_t dLCut = 2
#endif
	    ) {
  MuKpi(files,Out
#ifdef __TCFIT__
	,dLCut
#endif
	);
}
