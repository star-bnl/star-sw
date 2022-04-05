#ifndef __StEEmcPointFitMaker_h__
#define __StEEmcPointFitMaker_h__

#include "StEEmcPointMaker.h"

class EEmcSectorFit; 

class StEEmcPointFitMaker : public StEEmcPointMaker
{
public:
	StEEmcPointFitMaker(const Char_t *n);
	virtual ~StEEmcPointFitMaker(){ /*nada*/ }; 
	/// Initialize
	virtual Int_t Init();
	/// Process event
	virtual Int_t Make();
	/// Fit the specified sector
	Int_t FitSector(Int_t s); 
	/// Clear the maker
	virtual void  Clear(Option_t *opts="");
	/// Return the sector fit for the specified sector
	EEmcSectorFit *fit(Int_t sec){ return mSectorFit[sec]; }
	const EEmcSectorFit *fit(Int_t sec) const { return mSectorFit[sec]; }

	/// \param p: true=try all reco positions, false=no try
	void doPermutations(Bool_t p){ mPermutations=p; }

	/// Print summary 
	void print() const; 

	void limit(Int_t lim){ mLimitFits = lim; }

protected:

	/// Fits
	EEmcSectorFit *mSectorFit[12]; 
	/// Do or don't do permutations
	Bool_t mPermutations;

	Int_t mClusterId; 

	/// Maximum number of points to fit per sector
	Int_t mLimitFits;

	ClassDef(StEEmcPointFitMaker,1);
}; 

#endif
