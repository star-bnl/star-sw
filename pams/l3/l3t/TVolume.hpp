#ifndef TVOLUME_INC
#define TVOLUME_INC
// TVolume-includefile
//
//	This class contains pointers to hits.
//	These hits are sorted in r (padrow), phi and eta and can be retrieved by (padrow, phi, eta)
//	tuples or using next- and previous-methods.
//	Additionally a default padrow can be set. This is useful, if you only want to move
//	around in one particular padrow (this is the case in TTrackFFT::Follow)
//
//	Parameters:
//		FPadRows      - An array sorted by padrow; contains eta-slices 
//	Public methods:
//		TVolume	      - constructor
//		~TVolume      - destructor
//		SetPadRow     - sets default padrow
//		AddHit        - adds a hit to the volume
//		operator[]    - some overloaded [] operators

// some defines

// some includes
#include "THit.hpp"	
#include "Common.h"
#include "types.h"
#include <string.h>

// some typedefs


// class declaration

class TVolume 
{
private:
	// private data members
	// padrow-array of eta/phi-slices (should start at index 1)
	THitList** FPadRows[HIGH_PADROW-LOW_PADROW+2];
	// padrow array of all hits in each padrow (should start at index 1)
	THitList* FPadRowHits[HIGH_PADROW-LOW_PADROW+2];
	// default padrow
	int FPadRow;
	// number of eta- and phi-slices
	WORD FSNumberOfEtaSlices;
	WORD FSNumberOfPhiSlices;
	// minimum and maximum of phi- and eta-slices
	double FSPhiMin, FSPhiMax;
	double FSEtaMin, FSEtaMax;
	// iteration members
	// last hitlist while iterating through a padrow
	THitList* FLastHitList;
	// last position in hit-list
	slist_item FLastHitItem;
	// multipliers to avoid division
	float FPhiSliceMultiplier;
	float FEtaSliceMultiplier;
public:
// public methods
// - constructor

// sets the number of slices in eta and phi and initializes some values and arrays
   TVolume();
// - destructor
// releases all allocated memory
   ~TVolume();
// set statics
   void SetStatics(WORD NumberOfEtaSlices, WORD NumberOfPhiSlices, 
                   double phimin, double phimax, double etamin, double etamax)
	{
		int index;
		THitList** tempptr;

		// eta-slices
		FSNumberOfEtaSlices = NumberOfEtaSlices;
		// phi-slices
		FSNumberOfPhiSlices = NumberOfPhiSlices;
		// mins and maxes
		FSPhiMin = phimin;
		FSPhiMax = phimax;
		FSEtaMin = etamin;
		FSEtaMax = etamax;
		// calculate some multiplicators, to avoid division
		FPhiSliceMultiplier = (float)(1.0F / (FSPhiMax - FSPhiMin) * FSNumberOfPhiSlices);
		FEtaSliceMultiplier = (float)(1.0F / (FSEtaMax - FSEtaMin) * FSNumberOfEtaSlices);
		// get a phi eta array for each padrow
/*		for(index = 0; index < (HIGH_PADROW-LOW_PADROW)+2; index++)
		{
			FPadRows[index] = (THitList**) malloc(NumberOfEtaSlices*NumberOfPhiSlices*sizeof(THit*));
			memset(FPadRows[index], 0, NumberOfEtaSlices*NumberOfPhiSlices*sizeof(THit*));
		}
*/		
		// allocate phi eta array en block and divide afterwards (saves a lot time)
		tempptr = (THitList**) malloc((HIGH_PADROW-LOW_PADROW+2) * NumberOfEtaSlices*NumberOfPhiSlices*sizeof(THit*));
		memset((void*)tempptr, 0, (HIGH_PADROW-LOW_PADROW+2) * NumberOfEtaSlices*NumberOfPhiSlices*sizeof(THit*));
// test
//		memset((void*)tempptr, 0, (HIGH_PADROW-LOW_PADROW+2) * NumberOfEtaSlices*NumberOfPhiSlices*sizeof(THit*));
		for(index = 0; index < (HIGH_PADROW-LOW_PADROW)+2; index++)
		{
			FPadRows[index] = tempptr + index * NumberOfEtaSlices*NumberOfPhiSlices;
		}

	};
	// - sets default padrow
	void SetPadRow(int NewValue)
	{
		// padrow starts at 1
		NewValue--;
		// check bounds
		if (NewValue < 1)
			NewValue = 1;
		if (NewValue >= HIGH_PADROW)
			NewValue = HIGH_PADROW;
		FPadRow = NewValue;
	};
	// - adds a hit to the volume
	void AddHit(THit* NewValue);
	// - some overloaded [] operators
	// padrow, eta and phi
	THitList* operator()(int padrow, int eta, int phi);
	// eta and phi (default padrow)
	THitList* operator()(int eta, int phi);
	// methods for looping over hits in a particular padrow
	// set pointers to first hit in padrow
	// Parameters:
	//	padrow		- the padrow where we look for hits
	void SetFirstHitInPadrow(int padrow);
	// get the next hit in a padrow
	THit* GetNextHitInPadrow();
private:
	// private members...
	// calculate map-search-index from eta-index and phi-index
	int CalcMapIndex(int eta, int phi) {return (eta * FSNumberOfPhiSlices) + phi;};
};

// implementation


// - some overloaded [] operators
// padrow, eta and phi; return hit-list for this eta/phi-slice
inline THitList* TVolume::operator()(int padrow, int eta, int phi)
{
// check bounds
   if ((padrow < LOW_PADROW) || (padrow >= HIGH_PADROW) || (FPadRows[padrow] == NULL))
      return NULL;
// get entry
// return this hitlist
   else return FPadRows[padrow][CalcMapIndex(eta, phi)];
}
//
// eta and phi (default padrow)
//
inline THitList* TVolume::operator()(int eta, int phi)
{
   return operator()(FPadRow, eta, phi);
}



#endif
