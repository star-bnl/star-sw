// TVolume
//
//	This class contains pointers to hits.
//	These hits are sorted in r (padrow), phi and eta and can be retrieved by (padrow, phi, eta)
//	tuples or using next- and previous-methods.
//	Additionally a default padrow can be set. This is useful, if you only want to move
//	around in one particular padrow (this is the case in TTrackFFT::Follow)
//
//	Parameters:
//		FPadRows					- An array sorted by padrow; contains eta-slices (which contain phi-slices)
//	Public methods:
//		TVolume						- constructor
//		~TVolume					- destructor
//		SetPadRow					- sets default padrow
//		AddHit						- adds a hit to the volume
//		operator[]					- some overloaded [] operators, which return a hit-list or NULL

// some defines

// some includes
#include "TVolume.hpp"					// include TVolume
#include "Common.h"
#include "types.h"
//
//  Constructor
//
TVolume::TVolume()
{
// reset padrow-array
   memset(FPadRows, 0, (HIGH_PADROW-LOW_PADROW+2)*sizeof(void*));
// set members
// default padrow
   FPadRow = 0;
// initialize looping variables
   FLastHitItem = NULL;
// get hitlists for the FPadRowHits array
   for(int count = 0; count < HIGH_PADROW-LOW_PADROW+2; count++)
      FPadRowHits[count] = new THitList;
}
//
// - destructor
//
// releases all allocated memory
TVolume::~TVolume()
{
/*	test
	THitList** listarray;

	// loop through padrows
	for(int count = 0; count < HIGH_PADROW-LOW_PADROW+2; count++)
	{
		// eta-phi-slice present?
		if ((listarray=FPadRows[count]) != NULL)
		{
			// yes, delete all the hitlists in this slice
			for(int index = 0; index < FSNumberOfPhiSlices*FSNumberOfEtaSlices; index++)
			{
				delete listarray[index];
			}
		}
		// now delete the hitlist array
		free(listarray);
	}
*/
	// loop through padrow hitlists
/*	this is now automatically freed in trackframe.done
	for(int count = 0; count < HIGH_PADROW-LOW_PADROW+2; count++)
	{
		delete FPadRowHits[count];
	}
*/
	// free the phi eta arrays
	free(FPadRows[0]);
}

// - adds a hit to the volume
void TVolume::AddHit(THit* NewValue)
{
	WORD	phislice, etaslice;
	THitList*	temp2;

	// calculate phi and eta slice
	phislice = (WORD) ((NewValue->GetPhi() - FSPhiMin) * FPhiSliceMultiplier + 1.0);
	etaslice = (WORD) ((NewValue->GetEta() - FSEtaMin) * FEtaSliceMultiplier + 1.0);
	// check bounds
	if ((phislice < 1) || (phislice > FSNumberOfPhiSlices) || (etaslice < 1) || (etaslice > FSNumberOfEtaSlices))
		return;		// out of range, exit
	if ((NewValue->GetPadrow() < LOW_PADROW) || (NewValue->GetPadrow() > HIGH_PADROW))
		return;		// out of range, exit
	// put into volume
	// first into padrow hitlist
	FPadRowHits[NewValue->GetPadrow()]->append(NewValue);
	// then in phi eta slices
	// desired hitlist in this phi-eta-slice already present?
	if ((temp2 = FPadRows[NewValue->GetPadrow()][CalcMapIndex(etaslice, phislice)]) == NULL)
	{
		// no, create first (and assign to temp)
		temp2 = FPadRows[NewValue->GetPadrow()][CalcMapIndex(etaslice, phislice)] = new THitList();
	}
	// add hit to the list
	temp2->append(NewValue);
}

// methods for looping over hits in a particular padrow
// set pointers to first hit in padrow
// Parameters:
//	padrow		- the padrow where we look for hits
void TVolume::SetFirstHitInPadrow(int padrow)
{
	// position on first hit in the padrows hitlist (if any)
	FLastHitList = FPadRowHits[padrow];
	FLastHitItem =  FLastHitList->first();
	if (FLastHitItem == NULL)
	{
		// no hit in this hitlist
		FLastHitList = NULL;
		return;
	}
}
	
// get the next hit in a padrow
THit* TVolume::GetNextHitInPadrow()
{
	// check: any hits in this padrow?
	if (FLastHitItem == NULL)
		return NULL;	// no; exit
	// get this hit
	THit* temp = FLastHitList->inf(FLastHitItem);
	// advance to next hit
	FLastHitItem = FLastHitList->succ(FLastHitItem);
	// and return the previously found hit
	return temp;
}
