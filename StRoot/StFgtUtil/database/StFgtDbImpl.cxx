#include "StFgtDbImpl.h"

//  This is not a very efficient implementation of this. It feels like this
//  could be more efficient.
Double_t StFgtDbImpl::getMapping(
    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
)
{
	Short_t disc, quadrant;
	Char_t layer;
	Double_t ordinate, lowerSpan, upperSpan;

	getPhysCoordFromElecCoord(
	    rdo, arm, apv, channel,
	    disc, quadrant, layer,
	    ordinate, lowerSpan, upperSpan
	);
     
	return ordinate;
}

//  This, similarly, seems needlessly complicated.
bool StFgtDbImpl::isR(
    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
)
{
	Short_t disc, quadrant;
	Char_t layer;
	Double_t ordinate, lowerSpan, upperSpan;

	getPhysCoordFromElecCoord(
	    rdo, arm, apv, channel,
	    disc, quadrant, layer,
	    ordinate, lowerSpan, upperSpan
	);
     
	return (layer == 'R');
}
