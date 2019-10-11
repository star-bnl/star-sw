#ifndef St_tpcBXT0CorrEPDC_h
#define St_tpcBXT0CorrEPDC_h

#include "St_tpcBXT0CorrC.h"

class StTpcBXT0CorrEPDC : public St_tpcBXT0CorrC {
	public:
		static StTpcBXT0CorrEPDC * instance();
		StTpcBXT0CorrEPDC(St_tpcBXT0Corr * table = 0) : St_tpcBXT0CorrC(table) {}
		virtual ~StTpcBXT0CorrEPDC() {fgInstance = 0;}

		double getCorrection (double epdTAC) {
			double timeBucketShiftScale = a(0)[0];
			double generalOffset = a(0)[1];
			if (epdTAC == -1) return timeBucketShiftScale*generalOffset;
			else return timeBucketShiftScale*(generalOffset + a(0)[2] + a(0)[3]*epdTAC);
		}
	private:
		static StTpcBXT0CorrEPDC * fgInstance;
		ClassDef(StTpcBXT0CorrEPDC, 1)
};

#endif
