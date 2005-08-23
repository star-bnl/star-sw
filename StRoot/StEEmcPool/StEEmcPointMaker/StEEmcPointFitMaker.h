#ifndef __StEEmcPointFitMaker_h__
#define __StEEmcPointFitMaker_h__

#include "StEEmcPointMaker.h"

class EEmcSectorFit; 

class StEEmcPointFitMaker : public StEEmcPointMaker
{

    public:

        /// Constructor
	StEEmcPointFitMaker(const Char_t *n);
	~StEEmcPointFitMaker(){ /*nada*/ }; 
	/// Initialize
	Int_t Init();
	/// Process event
	Int_t Make();
	/// Fit the specified sector
	Int_t FitSector(Int_t s); 
	/// Clear the maker
	void  Clear(Option_t *opts="");
	/// Return the sector fit for the specified sector
	EEmcSectorFit *fit(Int_t sec){ return mSectorFit[sec]; }

	/// \param p: true=try all reco positions, false=no try
	void doPermutations(Bool_t p){ mPermutations=p; }

	/// Print summary 
	void print(); 

    private:
    protected:

	/// Fits
	EEmcSectorFit *mSectorFit[12]; 
	/// Do or don't do permutations
	Bool_t mPermutations;

	Int_t mClusterId; 

	/// Makes available to root
	ClassDef(StEEmcPointFitMaker,1);
}; 

#endif
