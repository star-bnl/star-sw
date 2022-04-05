/*  StFgtGeom.cxx
 *
 *  FGT geometry class implementation.
 *
 *  \author W. Witzke (wowitz0@uky.edu)
 *
 */

#include <TMath.h>
//#include <assert.h>
#include <iostream>
#include <algorithm>
#include "StFgtGeom.h"
#include "StMessMgr.h"
#include "fgtAlignment.h"

//ClassImp(StFgtGeom)

double	StFgtGeom::mPi = TMath::Pi();
double	StFgtGeom::mHalfPi = TMath::PiOver2();

double	StFgtGeom::mRadStripOff =
    (
	(rOut()-rIn()/radStrip_pitch())
	-
	int (
	    (rOut() - rIn())/radStrip_pitch()
	)
    ) * radStrip_pitch();

double	StFgtGeom::mPhiStripOff =
    (
	mHalfPi/phiStrip_pitch()
	-
	int (
	    mHalfPi/phiStrip_pitch()
	)
    ) * phiStrip_pitch();


int	StFgtGeom::mRadStripLOCId_number =
  int (
	(rOut()-rIn()-radStripOff()) / radStrip_pitch()
    ) + 5;

int	StFgtGeom::mPhiStripLOCId_number =
    int (
	(mHalfPi-phiStripOff()) / phiStrip_pitch()
    ) + 5;
// function to setup the reverse mapping based on the forward mapping
void StFgtGeom::makeReverseNaiveMappingValid(){
   for( Int_t *p = mReverseNaiveMapping; p != &mNaiveMapping[2*kFgtNumStrips]; ++p )
      (*p) = 0;

   Int_t i = 0;
   for( const Int_t *p = mNaiveMapping; p != &mNaiveMapping[ kFgtNumChannels*kFgtApvsPerQuad]; ++p, ++i )
      mReverseNaiveMapping[ (*p) ] = i;

   mReverseNaiveMappingValid = 1;
}

Double_t StFgtGeom::getDiscZ(int iDisc)
{
    switch(iDisc)
    {
	case 0:
	    return 67.399;
	case 1:
	    return 77.8765;
	case 2:
	    return 87.084;
	case 3:
	    return 97.4821;
	case 4:
	    return 108.9121;
	case 5:
	    return 118.9927;
	default:
	    LOG_DEBUG << "Disc " << iDisc << " out of range in StFgtGeom::getDiscZ." << endm;
	    return kFgtError;
    }
}

double StFgtGeom::phiQuadXaxis( int iquad )
{
    switch( iquad )
    {
	case 0:
	    return -15.0*mPi/180.0;
	case 1:
	    return -105.0*mPi/180.0;
	case 2:
	    return 165.0*mPi/180.0;
	case 3:
	    return 75.0*mPi/180.0;
	default:
	    LOG_DEBUG << "Quadrant " << iquad << " out of range in StFgtGeom::phiQuadXaxis." << endm;
	    return kFgtError;
	    //assert(2==3);   //	Safe without costing us any clock cycles.
    }
}

bool StFgtGeom::inDisc( TVector3 r ) //	'r' in LAB ref
{
    double Rxy = r.Perp();
    //printf("StFgtGeom::inDisc Rxy=%f rIn=%f rOut=%f\n",Rxy,kFgtRin,kFgtRout);
    if ( Rxy < kFgtRin )
	return false;
    if ( Rxy > kFgtRout )
	return false;
    return true;
}

bool StFgtGeom::belowFlat( TVector3 r ) //	'r' in LOC ref
{
  double Rf = r.X()*cos(kFgtPhiflat) + r.Y()*sin(kFgtPhiflat);
  //printf("StFgtGeom::belowFlat  Rf=%f Rflat=%f \n",Rf,kFgtRflat);
  if ( Rf > kFgtRflat )   
    return false;
  return true;
}

int StFgtGeom::getQuad( double phiLab )
{
  //  printf("StFgtGeom::getQuad phiLab/rad=%f\n", phiLab );
  //assert(phiLab <= mPi );
  //assert(phiLab >= -mPi );
  
  if(phiLab<(-mPi))
    phiLab+=(2*mPi);
  if(phiLab>mPi)
    phiLab-=(2*mPi);


  if ( phiLab > phiQuadXaxis(1) && phiLab <= phiQuadXaxis(0) )
    return 1;
  if ( phiLab > phiQuadXaxis(0) && phiLab <= phiQuadXaxis(3) )
    return 0;
  if ( phiLab > phiQuadXaxis(3) && phiLab <= phiQuadXaxis(2) )
    return 3;
  return 2;
}

Int_t StFgtGeom::encodeGeoId
(
    Int_t disc, Int_t quadrant, Char_t layer, Int_t strip
)   
{

    Char_t testR='R';
    Char_t testP='P';

    if ( disc < 0 || disc >= kFgtNumDiscs )
    {
	LOG_DEBUG << "Disc " << disc << " out of range in StFgtGeom::encodeGeoId." << endm;
	return kFgtError;
    }
    else if ( quadrant < 0 || quadrant >= kFgtNumQuads )
    {
	LOG_DEBUG << "Quadrant " << quadrant << " out of range in StFgtGeom::encodeGeoId." << endm;
	return kFgtError;
    }
    else if (
	     layer != testR
	     && layer != testP
    )
    {
	LOG_DEBUG << "Layer " << layer << " out of range in StFgtGeom::encodeGeoId." << endm;
	return kFgtError;
    }
    else if ( strip < 0 || strip >= kFgtNumStrips )
    {
	LOG_DEBUG << "Strip " << strip << " out of range in StFgtGeom::encodeGeoId." << endm;
	return kFgtError;
    }

    return
        (
            ( disc*kFgtNumQuads + quadrant ) * kFgtNumLayers + ( layer == 'P' )
        ) * kFgtNumStrips + strip;
}

Int_t StFgtGeom::decodeGeoId
(
    Int_t geoId,
    Short_t & disc, Short_t & quadrant, Char_t & layer, Short_t & strip
)
{
    if ( geoId < 0 || geoId >= kFgtNumGeoIds )
    {
	LOG_DEBUG << "GeoId " << geoId << " out of range in StFgtGeom::decodeGeoId." << endm;
	disc = kFgtError;
	quadrant = kFgtError;
	layer = kFgtErrorChar;
	strip = kFgtError;

	return kFgtError;
    }

    strip = geoId % kFgtNumStrips;
    geoId /= kFgtNumStrips;

    layer = ( geoId % kFgtNumLayers ) ? 'P' : 'R';
    geoId /= kFgtNumLayers;

    quadrant = geoId % kFgtNumQuads;
    disc = geoId / kFgtNumQuads;

    return 0;
}

std::string StFgtGeom::encodeGeoName
(
    Int_t disc, Int_t quadrant, Char_t layer, Int_t strip
)
{
   Char_t testR='R';
   Char_t testP='P';

    if ( disc < 0 || disc >= kFgtNumDiscs )
    {
	LOG_DEBUG << "Disc " << disc << " out of range in StFgtGeom::encodeGeoName." << endm;
	return kFgtErrorString;
    }
    else if ( quadrant < 0 || quadrant >= kFgtNumQuads )
    {
	LOG_DEBUG << "Quadrant " << quadrant << " out of range in StFgtGeom::encodeGeoName." << endm;
	return kFgtErrorString;
    }
    else if (
	     layer != testR
	     && layer != testP
    )
    {
	LOG_DEBUG << "Layer " << layer << " out of range in StFgtGeom::encodeGeoName." << endm;
	return kFgtErrorString;
    }
    else if ( strip < 0 || strip >= kFgtNumStrips )
    {
	LOG_DEBUG << "Strip " << strip << " out of range in StFgtGeom::encodeGeoName." << endm;
	return kFgtErrorString;
    }

    std::stringstream buff;
    buff << disc+1 << (Char_t)(quadrant+'A') << layer;

    if ( strip < 10 )
        buff << "00";
    else if ( strip < 100 )
        buff << "0";

    buff << strip;
    return buff.str();
}

Int_t StFgtGeom::decodeGeoName 
(
    const std::string & geoName,
    Short_t & disc, Short_t & quadrant, Char_t & layer, Short_t & strip
)
{
    Char_t testR='R';
    Char_t testP='P';

    //assert( geoName.size() == 6 );
    disc = geoName[0] - '1';
    quadrant = geoName[1] - 'A';
    layer = geoName[2];
    strip = std::atoi( (geoName.substr(3)).c_str() );

    //	This is unlikely to catch all errors with the geoName, but it should
    //	do fairly well.
    if (
	   disc < 0
	|| disc >= kFgtNumDiscs 
	|| quadrant < 0
	|| quadrant >= kFgtNumQuads
	|| (
	     layer != testR
	     && layer != testP
	   )
	|| strip < 0
	|| strip > kFgtNumStrips
    )
    {
	LOG_DEBUG << "Malformed geoName " << geoName << " in StFgtGeom::decodeGeoName." << endm;
	disc = kFgtError;
	quadrant = kFgtError;
	layer = kFgtErrorChar;
	strip = kFgtError;

	return kFgtError;
    }

    return 0;
}

std::string StFgtGeom::translateGeoIdToGeoName( Int_t geoId )
{
    Short_t disc, quadrant, strip;
    Char_t layer;
    
    if ( geoId < 0 || geoId >= kFgtNumGeoIds )
    {
	LOG_DEBUG << "GeoId " << geoId << " out of range in StFgtGeom::translateGeoIdToGeoName." << endm;
	return kFgtErrorString;
    }

    decodeGeoId( geoId, disc, quadrant, layer, strip );
    return encodeGeoName( disc, quadrant, layer, strip );
}

Int_t StFgtGeom::translateGeoNameToGeoId( const std::string & geoName )
{
    Short_t disc, quadrant, strip;
    Char_t layer;

    //	Error message already taken care of in decodeGeoName.
    if ( decodeGeoName( geoName, disc, quadrant, layer, strip ) < 0 )
	return kFgtError;

    return encodeGeoId( disc, quadrant, layer, strip );
}

//  The ordinate, lowerSpan and upperSpan are all in centimeters or
//  radians, depending on the layer.
Int_t StFgtGeom::getPhysicalCoordinate
(
    Int_t geoId,
    Short_t & disc, Short_t & quadrant, Char_t & layer,
    Double_t & ordinate, Double_t & lowerSpan, Double_t & upperSpan
)
{
    if ( geoId < 0 || geoId >= kFgtNumGeoIds )
    {
	LOG_DEBUG << "GeoId " << geoId << " out of range in StFgtGeom::getPhysicalCoordinate." << endm;
	disc = kFgtError;
	quadrant = kFgtError;
	layer = kFgtErrorChar;
	ordinate = kFgtError;
	lowerSpan = kFgtError;
	upperSpan = kFgtError;

	return kFgtError;
    }

    Short_t strip;

    decodeGeoId( geoId, disc, quadrant, layer, strip );
    ordinate =
        mStrips[
            (layer == 'P') * kFgtNumStrips + strip
            ].ordinate;
    lowerSpan =
    mStrips[
            (layer == 'P') * kFgtNumStrips + strip
            ].lowerSpan;
    upperSpan =
        mStrips[
            (layer == 'P') * kFgtNumStrips + strip
            ].upperSpan;

    return 0;
}

//  The ordinate, lowerSpan and upperSpan are all in centimeters or
//  radians, depending on the layer.
Int_t StFgtGeom::getPhysicalCoordinate
(
    const std::string & geoName,
    Short_t & disc, Short_t & quadrant, Char_t & layer,
    Double_t & ordinate, Double_t & lowerSpan, Double_t & upperSpan
)
{
    Short_t strip;

    if ( decodeGeoName( geoName, disc, quadrant, layer, strip ) < 0 )
    {
	//  Error is mostly handled by the decodeGeoName call.
	disc = kFgtError;
	quadrant = kFgtError;
	layer = kFgtErrorChar;
	ordinate = kFgtError;
	lowerSpan = kFgtError;
	upperSpan = kFgtError;

	return kFgtError;
    }

    ordinate =
        mStrips[
            (layer == 'P') * kFgtNumStrips + strip
            ].ordinate;
    lowerSpan =
        mStrips[
            (layer == 'P') * kFgtNumStrips + strip
            ].lowerSpan;
    upperSpan =
        mStrips[
            (layer == 'P') * kFgtNumStrips + strip
            ].upperSpan;

    return 0;
}


//  Calculates coordinates of strip in global coordinate system
//  Units are in cm andradians, depending on the layer.
Int_t StFgtGeom::computeGlobalPhysicalCoordinate
(
    Short_t & quadrant, Char_t & layer,
    Double_t & ordinate, Double_t & lowerSpan, Double_t & upperSpan, Short_t & strip
)
{
    switch (layer) {
	case ('P') :
            ordinate =
                mStrips[kFgtNumStrips + strip].ordinate + StFgtGeom::phiQuadXaxis(quadrant);
            lowerSpan =
                mStrips[kFgtNumStrips + strip].lowerSpan;
            upperSpan =
                mStrips[kFgtNumStrips + strip].upperSpan;
            break;
	case ('R') :
            ordinate =
                mStrips[strip].ordinate;
            lowerSpan =
                mStrips[strip].lowerSpan + StFgtGeom::phiQuadXaxis(quadrant);
            upperSpan =
                mStrips[strip].upperSpan + StFgtGeom::phiQuadXaxis(quadrant);
            break;
        default:
	    return kFgtError;
	    break;
    }

    return 0;
}

Int_t StFgtGeom::getGlobalPhysicalCoordinate
(
    Int_t geoId,
    Short_t & disc, Short_t & quadrant, Char_t & layer,
    Double_t & ordinate, Double_t & lowerSpan, Double_t & upperSpan
)
{
  Short_t strip;
 
  decodeGeoId( geoId, disc, quadrant, layer, strip );


  if ( geoId < 0 || geoId >= kFgtNumGeoIds )
    {
	LOG_DEBUG << "GeoId " << geoId << " out of range in StFgtGeom::getPhysicalCoordinate." << endm;
	disc = kFgtError;
	quadrant = kFgtError;
	layer = kFgtErrorChar;
	ordinate = kFgtError;
	lowerSpan = kFgtError;
	upperSpan = kFgtError;

	return kFgtError;
    }

    return computeGlobalPhysicalCoordinate( quadrant, layer,
                                            ordinate, lowerSpan, upperSpan, strip);
}

//  The ordinate, lowerSpan and upperSpan are all in centimeters or
//  radians, depending on the layer.
Int_t StFgtGeom::getGlobalPhysicalCoordinate
(
    const std::string & geoName,
    Short_t & disc, Short_t & quadrant, Char_t & layer,
    Double_t & ordinate, Double_t & lowerSpan, Double_t & upperSpan
)
{
    Short_t strip;

    if ( decodeGeoName( geoName, disc, quadrant, layer, strip ) < 0 )
    {
	//  Error is mostly handled by the decodeGeoName call.
	disc = kFgtError;
	quadrant = kFgtError;
	layer = kFgtErrorChar;
	ordinate = kFgtError;
	lowerSpan = kFgtError;
	upperSpan = kFgtError;

	return kFgtError;
    }

    return computeGlobalPhysicalCoordinate( quadrant, layer,
                                            ordinate, lowerSpan, upperSpan, strip);

}




//  Please note that the following functions do NOT access the STAR
//  database to find mapping information. They assume the most
//  straight-forward mapping scheme and use that.
//  For those functions that have them, currently rdo can be 1-2, arm
//  can be 0-5, apv can be 0-23 (although 10, 11, 22, and 23 are not
//  technically valid) and channel is 0-127.
Int_t StFgtGeom::encodeElectronicId
(
    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
)
{
    if ( (rdo - 1) < 0 || (rdo - 1) >= kFgtNumRdos )
    {
	LOG_DEBUG << "RDO " << rdo << " out of range in StFgtGeom::encodeElectronicId." << endm;
	return kFgtError;
    }
    else if ( arm < 0 || arm >= kFgtNumArms )
    {
	LOG_DEBUG << "ARM " << arm << " out of range in StFgtGeom::encodeElectronicId." << endm;
	return kFgtError;
    }
    else if ( apv < 0 || apv > kFgtMaxApvId || apv == 10 || apv == 11 )
    {
	LOG_DEBUG << "APV " << apv << " out of range in StFgtGeom::encodeElectronicId." << endm;
	return kFgtError;
    }
    else if ( channel < 0 || channel >= kFgtNumChannels )
    {
	LOG_DEBUG << "Channel " << channel << " out of range in StFgtGeom::encodeElectronicId." << endm;
	return kFgtError;
    }


    if ( apv >= 12 )
    {
        apv -= 2;
    }
    return channel+128*(apv+20*(arm+6*(rdo-1)));
}

Int_t StFgtGeom::decodeElectronicId
(
    Int_t elecId,
    Int_t &rdo, Int_t &arm, Int_t &apv, Int_t &channel
)
{
    if ( elecId < 0 || elecId >= kFgtNumElecIds )
    {
	LOG_DEBUG << "Electronic ID " << elecId << " out of range in StFgtGeom::decodeElectronicId." << endm;

	rdo = kFgtError;
	arm = kFgtError;
	apv = kFgtError;
	channel = kFgtError;

	return kFgtError;
    }

    channel = elecId % 128;
    elecId /= 128;

    apv = elecId % 20;
    elecId /= 20;

    arm = elecId % 6;
    rdo = 1 + elecId / 6;

    if ( apv > 9 )
    {
        apv += 2;
    }
    return 0;
}

Int_t StFgtGeom::getNaiveGeoIdFromElecCoord
(
    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
)
{
    if ( (rdo - 1) < 0 || (rdo - 1) >= kFgtNumRdos )
    {
	LOG_DEBUG << "RDO " << rdo << " out of range in StFgtGeom::getNaiveGeoIdFromElecCoord." << endm;
	return kFgtError;
    }
    else if ( arm < 0 || arm >= kFgtNumArms )
    {
	LOG_DEBUG << "ARM " << arm << " out of range in StFgtGeom::getNaiveGeoIdFromElecCoord." << endm;
	return kFgtError;
    }
    else if ( apv < 0 || apv > kFgtMaxApvId || apv == 10 || apv == 11 )
    {
	LOG_DEBUG << "APV " << apv << " out of range in StFgtGeom::getNaiveGeoIdFromElecCoord." << endm;
	return kFgtError;
    }
    else if ( channel < 0 || channel >= kFgtNumChannels )
    {
	LOG_DEBUG << "Channel " << channel << " out of range in StFgtGeom::getNaiveGeoIdFromElecCoord." << endm;
	return kFgtError;
    }

    Short_t disc = int(arm/2) + (rdo-1)*3;
    Short_t quadrant = (arm & 1)*2 + int( apv/12 );

    if ( apv >= 12 )
        return
        (
            disc*kFgtNumQuads + quadrant
        ) * kFgtNumLayers * kFgtNumStrips
            + mNaiveMapping[ (apv-12)*128+channel ];
    else
        return
        (
            disc*kFgtNumQuads + quadrant
        ) * kFgtNumLayers * kFgtNumStrips
            + mNaiveMapping[ apv*128+channel ];

}

// get the octant for a given layer and strip
Char_t StFgtGeom::getOctant( Char_t layer, Int_t strip )
{
    if (
	   layer != kFgtLowerStripOctant
	&& layer != kFgtHigherStripOctant
    )
    {
	LOG_DEBUG << "Layer " << layer << " out of range in StFgtGeom::getOctant." << endm;
	return kFgtErrorChar;
    }
    else if ( strip < 0 || strip >= kFgtNumStrips )
    {
	LOG_DEBUG << "Strip " << strip << " out of range in StFgtGeom::getOctant." << endm;
	return kFgtErrorChar;
    }

    return
        ( strip < ( layer == 'R' ? kFgtNumRstripsPerOctant : kFgtNumPstripsPerOctant )
            ? kFgtLowerStripOctant
            : kFgtHigherStripOctant );
}

// get the octant given the APV number
Char_t StFgtGeom::getOctant( Int_t apv )
{
    if ( apv < 0 || apv > kFgtMaxApvId || apv == 10 || apv == 11 )
    {
	LOG_DEBUG << "APV " << apv << " out of range in StFgtGeom::getOctant." << endm;
	return kFgtErrorChar;
    }

    return ( (apv%kFgtApvsPerAssembly) < kFgtApvsPerOct
	     ? kFgtLowerStripOctant
	     : kFgtHigherStripOctant );
}

//  This is similar to the above functions, but it takes electronic
//  coordinates and only returns the final ordinate. This is here
//  primarily so that it can be used as a drop in replacement for
//  older code that has similar functionality.
Double_t StFgtGeom::getNaiveMapping(
    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
)
{
    if ( (rdo - 1) < 0 || (rdo - 1) >= kFgtNumRdos )
    {
	LOG_DEBUG << "RDO " << rdo << " out of range in StFgtGeom::getNaiveMapping." << endm;
	return kFgtError;
    }
    else if ( arm < 0 || arm >= kFgtNumArms )
    {
	LOG_DEBUG << "ARM " << arm << " out of range in StFgtGeom::getNaiveMapping." << endm;
	return kFgtError;
    }
    else if ( apv < 0 || apv > kFgtMaxApvId || apv == 10 || apv == 11 )
    {
	LOG_DEBUG << "APV " << apv << " out of range in StFgtGeom::getNaiveMapping." << endm;
	return kFgtError;
    }
    else if ( channel < 0 || channel >= kFgtNumChannels )
    {
	LOG_DEBUG << "Channel " << channel << " out of range in StFgtGeom::getNaiveMapping." << endm;
	return kFgtError;
    }

    if ( apv >= 12 )
        return
            mStrips[ mNaiveMapping[ (apv-12)*128+channel ] ].ordinate;
    else
        return
            mStrips[ mNaiveMapping[ apv*128+channel ] ].ordinate;
}

bool StFgtGeom::isNaiveR(
    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
)
{
    //	There isn't much I can do here without causing potential problems with
    //	calls to this function. So, warnings are all I can do.
    if ( (rdo - 1) < 0 || (rdo - 1) >= kFgtNumRdos )
    {
	LOG_DEBUG << "RDO " << rdo << " out of range in StFgtGeom::isNaiveR." << endm;
    }
    else if ( arm < 0 || arm >= kFgtNumArms )
    {
	LOG_DEBUG << "ARM " << arm << " out of range in StFgtGeom::isNaiveR." << endm;
    }
    else if ( apv < 0 || apv > kFgtMaxApvId || apv == 10 || apv == 11 )
    {
	LOG_DEBUG << "APV " << apv << " out of range in StFgtGeom::isNaiveR." << endm;
    }
    else if ( channel < 0 || channel >= kFgtNumChannels )
    {
	LOG_DEBUG << "Channel " << channel << " out of range in StFgtGeom::isNaiveR." << endm;
    }

    if ( apv >= 12 )
        return
            mNaiveMapping[ (apv-12)*128+channel ] < 720;
    else
        return
            mNaiveMapping[ apv*128+channel ] < 720;
}

Int_t StFgtGeom::getNaiveElecCoordFromGeoId
(
    Int_t geoId,
    Int_t& rdo, Int_t& arm, Int_t& apv, Int_t& channel
)
{
    Short_t disc, quadrant, strip;
    Char_t layer;

    //	Most of this error should be handled by the decodeGeoId call.
    if ( decodeGeoId( geoId, disc, quadrant, layer, strip ) < 0 )
	return kFgtError;

    if( !mReverseNaiveMappingValid )
	makeReverseNaiveMappingValid();

    Int_t key = ( (layer=='P')*kFgtNumStrips + strip );
    channel = mReverseNaiveMapping[ key ];
    apv = channel / 128;
    channel %= 128;

    if( quadrant % 2 )
    apv += 12;

    rdo = disc/3+1;
    arm = (disc % 3)*2 + (quadrant>1);

    return 0;
}

//This function takes in a radius and a LOCAL phi values (0-pi/2 radians)
//It returns the closest phi strip to the input radius,phi pair (0-719)
//If there are not strips implemented on the detector at that r,phi 
//then it returns -1
int StFgtGeom::phi2LocalStripId( double rad, double phiLoc, double *binFrac )
{

  /*
  const int pbins = ((pLast() - pFirst())/phiStrip_pitch()) + 1;//720 strips total numbered from 0-719

  double pstrip[pbins];//array that holds phi location of strips

  //fill array of pstrip with difference between passed phi and strip phi location values
  double min_p_diff = phiStrip_pitch()/2.0;
  int pindex = -1;//index in array is the strip value
  for ( int i =0; i < pbins; i++)
    {
      double p=-i*phiStrip_pitch() + pLast();
      pstrip[i] = (-i*phiStrip_pitch() + pLast() - phiLoc);
      if ( fabs(pstrip[i]) < fabs(min_p_diff) ) 
	{
	  min_p_diff = pstrip[i];
	  pindex = i;
	  //printf("ppp i=%3d phi=%8.4f p=%8.4f %8.4f %8.4f pst=%8.4f pindex=%3d ",
	  //        i,phiLoc,p-phiStrip_pitch()/2.0,p,p+phiStrip_pitch()/2.0,pstrip[i],pindex);
	  //break;
	}
      //printf("ppp i=%3d phi=%8.4f p=%8.4f %8.4f %8.4f pst=%8.4f pindex=%3d\n",
      //       i,phiLoc,p-phiStrip_pitch()/2.0,p,p+phiStrip_pitch()/2.0,pstrip[i],pindex);
    }
  */
  int pindex=-1;
  double p3=0.0;
  if(phiLoc>=pFirst()-phiStrip_pitch()/2.0 || phiLoc<=pLast()+phiStrip_pitch()/2.0){
    double p2=pLast()+phiStrip_pitch()/2.0-phiLoc;
    p3=p2/phiStrip_pitch();
    pindex=int(p3);
  }
  //printf("pindex=%d pindex2=%d diff=%d\n",pindex,pindex2,pindex-pindex2);  
  //if(pindex!=pindex2) printf("pindex=%d pindex2=%d diff=%d\n",pindex,pindex2,pindex-pindex2);


  //if r is < 19.125 and >= 11.5 then all even strips are not there
  //if index is odd the width is 2x phiStrip_pitch()
  if ( (rad < rMid()) && (rad >= rIn()) && (pindex%2==1) && pindex>-1){
 
    /*
    //if min_p_diff < 0 then phiLoc > stripPhi and is closer to the lower numbered odd strip
    if (min_p_diff <= 0) pindex-=1;
    //if min_p_diff > 0 then phiLoc < stripPhi and is closer to higher numbered odd strip 
    if (min_p_diff > 0) pindex+=1;
    */
    //printf("EvenOdd pindex=%3d p3=%6.2f mod=%2d\n",pindex,p3,pindex%2);
    if(p3-pindex<0.5) {pindex-=1;}
    else              {pindex+=1;}
  }
  
  //the first and last 35 strips have irregular rinner values
  if ( (pindex < 35) || ( pindex > 684) )
    {
      if ( rad < pHistrip_R_Low(pindex)) pindex  = -1;
    }
  
  //these strips have irregular router values
  if ( (pindex < 347) || ( pindex > 306) )
    {
      if ( rad > pHistrip_R_High(pindex)) pindex  = -1;
    }

  //if radius is outside of geometric location of the foil then return -1
  if ( (rad > rOut()) || (rad < rIn()) ) pindex = -1;

  //if phi is outside of first strip - width or last strip + width then return -1
  if ( (phiLoc > (pLast()+phiStrip_pitch())) || (phiLoc < (pFirst()-phiStrip_pitch())) ) pindex = -1;

  return pindex;
  
}



double StFgtGeom::pHistrip_R_High(int pindex){
  
  double r;

  double holdtemp[340]={38.2243,//strip 307
			38.1948,
			38.1655,
			38.1364,
			38.1075,
			38.0789,
			38.0504,
			38.0222,
			37.9942,
			37.9663,
			37.9387,
			37.9113,
			37.8841,
			37.8571,
			37.8303,
			37.8037,
			37.7773,
			37.7511,
			37.7251,
			37.6993,
			37.6737,
			37.6483,
			37.6231,
			37.5981,
			37.5733,
			37.5488,
			37.5244,
			37.5002,
			37.4761,
			37.4523,
			37.4287,
			37.4053,
			37.382,
			37.359,
			37.3362,
			37.3135,
			37.2911,
			37.2688,
			37.2467,
			37.2248,
			37.2031,
			37.1816,
			37.1602,
			37.1391,
			37.1181,
			37.0974,
			37.0768,
			37.0564,
			37.0362,
			37.0162,
			36.9963,
			36.9766,
			36.9572,
			36.9379,
			36.9188,
			36.8998,
			36.8811,
			36.8625,
			36.8441,
			36.8259,
			36.8079,
			36.79,
			36.7723,
			36.7548,
			36.7375,
			36.7204,
			36.7034,
			36.6866,
			36.67,
			36.6536,
			36.6373,
			36.6213,
			36.6053,
			36.5896,
			36.574,
			36.5587,
			36.5434,
			36.5284,
			36.5135,
			36.4988,
			36.4843,
			36.47,
			36.4558,
			36.4418,
			36.4279,
			36.4143,
			36.4008,
			36.3874,
			36.3743,
			36.3613,
			36.3484,
			36.3358,
			36.3233,
			36.311,
			36.2988,
			36.2868,
			36.275,
			36.2634,
			36.2519,
			36.2406,
			36.2294,
			36.2184,
			36.2076,
			36.1969,
			36.1865,
			36.1761,
			36.166,
			36.156,
			36.1461,
			36.1365,
			36.127,
			36.1176,
			36.1084,
			36.0994,
			36.0906,
			36.0819,
			36.0733,
			36.065,
			36.0568,
			36.0487,
			36.0409,
			36.0331,
			36.0256,
			36.0182,
			36.0109,
			36.0039,
			35.997,
			35.9902,
			35.9836,
			35.9772,
			35.9709,
			35.9648,
			35.9588,
			35.953,
			35.9474,
			35.9419,
			35.9366,
			35.9315,
			35.9265,
			35.9216,
			35.9169,
			35.9124,
			35.9081,
			35.9039,
			35.8998,
			35.8959,
			35.8922,
			35.8886,
			35.8852,
			35.882,
			35.8789,
			35.8759,
			35.8732,
			35.8705,
			35.8681,
			35.8658,
			35.8636,
			35.8616,
			35.8598,
			35.8581,
			35.8566,
			35.8552,
			35.854,
			35.853,
			35.8521,
			35.8514,
			35.8508,
			35.8504,
			35.8501,
			35.85,
			35.8501,
			35.8503,
			35.8506,
			35.8512,
			35.8518,
			35.8527,
			35.8537,
			35.8548,
			35.8562,
			35.8576,
			35.8593,
			35.861,
			35.863,
			35.8651,
			35.8673,
			35.8698,
			35.8723,
			35.8751,
			35.878,
			35.881,
			35.8842,
			35.8876,
			35.8911,
			35.8948,
			35.8986,
			35.9026,
			35.9068,
			35.9111,
			35.9155,
			35.9202,
			35.925,
			35.9299,
			35.935,
			35.9403,
			35.9457,
			35.9513,
			35.957,
			35.9629,
			35.969,
			35.9752,
			35.9816,
			35.9882,
			35.9949,
			36.0017,
			36.0088,
			36.0159,
			36.0233,
			36.0308,
			36.0385,
			36.0463,
			36.0543,
			36.0625,
			36.0708,
			36.0792,
			36.0879,
			36.0967,
			36.1057,
			36.1148,
			36.1241,
			36.1335,
			36.1432,
			36.1529,
			36.1629,
			36.173,
			36.1833,
			36.1937,
			36.2043,
			36.2151,
			36.226,
			36.2371,
			36.2484,
			36.2598,
			36.2714,
			36.2832,
			36.2951,
			36.3072,
			36.3195,
			36.3319,
			36.3446,
			36.3573,
			36.3703,
			36.3834,
			36.3967,
			36.4101,
			36.4237,
			36.4375,
			36.4515,
			36.4656,
			36.4799,
			36.4944,
			36.509,
			36.5238,
			36.5388,
			36.554,
			36.5693,
			36.5848,
			36.6005,
			36.6164,
			36.6324,
			36.6486,
			36.665,
			36.6815,
			36.6983,
			36.7152,
			36.7323,
			36.7495,
			36.767,
			36.7846,
			36.8024,
			36.8204,
			36.8385,
			36.8569,
			36.8754,
			36.8941,
			36.9129,
			36.932,
			36.9512,
			36.9707,
			36.9903,
			37.0101,
			37.03,
			37.0502,
			37.0705,
			37.0911,
			37.1118,
			37.1327,
			37.1538,
			37.175,
			37.1965,
			37.2181,
			37.24,
			37.262,
			37.2842,
			37.3066,
			37.3292,
			37.352,
			37.375,
			37.3982,
			37.4215,
			37.4451,
			37.4688,
			37.4928,
			37.5169,
			37.5413,
			37.5658,
			37.5905,
			37.6155,
			37.6406,
			37.6659,
			37.6915,
			37.7172,
			37.7431,
			37.7692,
			37.7956,
			37.8221,
			37.8488,
			37.8758,
			37.9029,
			37.9303,
			37.9578,
			37.9856,
			38.0136,
			38.0418,
			38.0702,
			38.0988,
			38.1276,
			38.1566,
			38.1858,
			38.2153,
			38.2449};//647


  double hold[720];
  for (int i=0;i<307;i++) hold[i]=38.25;
  for (int i=307;i<647;i++) hold[i]=holdtemp[i-307];
  for (int i=647;i<720;i++) hold[i]=38.25;


  r = hold[pindex];
  return r;

}

double StFgtGeom::pHistrip_R_Low(int pindex){
  
  double r;

  double holdLow[35]={37.0706,//strip 0
		      34.8235,
		      32.8244,
		      31.0505,
		      29.4515,
		      28.0157,
		      26.7075,
		      25.5216,
		      24.4317,
		      23.4357,
		      22.5179,
		      21.6654,
		      20.8788,
		      20.144,
		      19.4624,
		      19.125,
		      18.2264,
		      19.125,
		      17.1382,
		      19.125,
		      16.1729,
		      19.125,
		      15.3108,
		      19.125,
		      14.5362,
		      19.125,
		      13.8365,
		      19.125,
		      13.2012,
		      19.125,
		      12.6219,
		      19.125,
		      12.0916,
		      19.125,
		      11.6042};//strip 34
  
  double holdHigh[35]={19.125,//strip 685
		       11.8432,
		       19.125,
		       12.3514,
		       19.125,
		       12.9053,
		       19.125,
		       13.5115,
		       19.125,
		       14.1778,
		       19.125,
		       14.9134,
		       19.125,
		       15.7299,
		       19.125,
		       16.6412,
		       19.125,
		       17.6649,
		       19.125,
		       18.8232,
		       19.4614,
		       20.1444,
		       20.8773,
		       21.6656,
		       22.5158,
		       23.4356,
		       24.4339,
		       25.5211,
		       26.7097,
		       28.0145,
		       29.4536,
		       31.0486,
		       32.8265,
		       34.8204,
		       37.0723};//strip 719


 
  if (pindex < 35) {  r = holdLow[pindex]; }
  if (pindex > 684){  r = holdHigh[pindex-685]; }

  return r;
}


//This function takes in a radius and a LOCAL phi values (0-pi/2 radians)
//It returns the closest r strip to the input radius,phi pair
//If there are no strips implemented on the detector at that r,phi 
//then it returns -1
int StFgtGeom::rad2LocalStripId( double rad, double phiLoc, double *binFrac )
{
  const int rbins = ((rLast() - rFirst())/radStrip_pitch())+1;//280 strips on each side of the quadrant

  /*
  double rstrip[rbins];//array that holds r location of strips
  //fill array of rstrip with difference between passed radius and strip r location values
  //double min_r_diff = radStrip_pitch();
  double min_r_diff = radStrip_pitch()/2.0;
  int rindex = -1;//index in array is the strip value
  for ( int i =0; i < rbins; i++)
    {
      //double r=-i*radStrip_pitch() + rLast();
      rstrip[i] = fabs(-i*radStrip_pitch() + rLast() - rad);
      
      if (rstrip[i] < min_r_diff) 
	{
	  min_r_diff = rstrip[i];
	  rindex = i;
	  //printf("rrr i=%3d rad=%8.4f r=%8.4f %8.4f %8.4f rst=%8.4f rindex=%3d ",
	  //       i,rad,r-radStrip_pitch()/2.0,r,r+radStrip_pitch()/2.0,rstrip[i],rindex);
	  break;
	}
      //printf("rrr i=%3d r=%8.4f mid=%8.4f rindex=%3d\n",i,r,r+radStrip_pitch()*0.5,rindex);
    }
  */
  int rindex=-1;
  if(rad>=rFirst()-radStrip_pitch()/2.0 || rad<=rLast()+radStrip_pitch()/2.0){
    double r2=rLast()+radStrip_pitch()/2.0-rad;
    rindex=int(r2/radStrip_pitch());
  }
  //printf("rindex=%d rindex2=%d diff=%d\n",rindex,rindex2,rindex-rindex2);
  //if(rindex!=rindex2) printf("rindex=%d rindex2=%d diff=%d\n",rindex,rindex2,rindex-rindex2);

  //if phi = 45-90 (135-180,0--45, -90-135),  then strips are 0-279  and flag =0  
  Int_t Phi_flag = 0;
  
  //if phi = 0-45 (90-135,-45--90, -135--180)  then strips are 400-678  and flag =1  
  if ((phiLoc >= 0)&&(phiLoc < mPi/4)) Phi_flag = 1;
 

  //only exception are for strips 13-24 that extend over the midway point 
  //this is so ugly but what can I do - it is the hardware!
  if ((rindex<25)&&(rindex>12)) 
    {
    
      if ((phiLoc > 0.57)&&(phiLoc <= mHalfPi)) Phi_flag = 0;  
      
    }
    
  if (Phi_flag == 1) rindex+=400;
  
  //Last make a series of checks to be sure that there are strips in the phi region for strips  0 - 24 and 400-424
  if (((rindex < 25)&&(rindex >=0)) || (( rindex < 425 )&&( rindex >= 400)))
    {
      if (phiLoc > (rStrip_Phi_High(rindex))) rindex = -1;
      if (phiLoc < (rStrip_Phi_Low(rindex))) rindex = -1;								   
    }


  Int_t checkHighRad = rbins;// max r strip value
  Int_t checkLowRad  = 0;// min r strip values 
  if (Phi_flag ==1 ) 
    {
      checkHighRad +=400;
      checkLowRad +=400;
    }

  if (( rindex < checkLowRad ) || (rindex >= checkHighRad) || ( rad < (rFirst() - (radStrip_pitch()/2))) || ( rad > ( rLast() + (radStrip_pitch()/2) ) ) )  rindex = -1;
    
  return rindex;
  
}

double StFgtGeom::rStrip_Phi_High(int rindex){
  
  double phi=0.0;
  
  double hold[25]={0.19153084,
		   0.19847378,
		   0.20559055,
		   0.21289241,
		   0.22039270,
		   0.22809740,
		   0.23603912,
		   0.24422818,
		   0.25268644,
		   0.26143829,
		   0.27050272,
		   0.27993242,
		   0.28975896,
		   0.30003085,
		   0.31080764,
		   0.32215264,
		   0.33418531,
		   0.34702006,
		   0.36082815,
		   0.37585491,
		   0.39245105,
		   0.41125997,
		   0.43341014,
		   0.46165336,
		   0.50945919};
  
  double high[kFgtNumStrips];

  for (int i = 0; i < 25; i++)
    {
      high[i+400]=hold[i];
    }

  for (int i = 425; i < 680; i++)
    {
      high[i]=0.7866672;
    }

  for (int i = 0;i<280; i++)
    {
      high[i] = mHalfPi;
    }

  if (((rindex >=0)&&(rindex < 280))||((rindex < 680)&&(rindex >=400)))
    phi = high[rindex];    

  return phi;
}

double StFgtGeom::rStrip_Phi_Low(int rindex){
  
  double phi=0.0;

  double low[kFgtNumStrips]={0.8905733,
				     0.88363036,
				     0.87651359,
				     0.86921173,
				     0.86171144,
				     0.85400674,
				     0.84606502,
				     0.83787596,
				     0.82941770,
				     0.82066585,
				     0.81160142,
				     0.80217172,
				     0.79234517,
				     0.78207329,
				     0.77129650,
				     0.75995149,
				     0.74791883,
				     0.73508408,
				     0.72127599,
				     0.70624922,
				     0.68965309,
				     0.67084417,
				     0.64869399,
				     0.62045077,
				     0.57264494};
  
  for ( int i = 25; i< 280; i++)
    {
      low[i] = 0.7866672;
    }
  
  //note all other values are set to zero which isn't completely true.  
  //On other side of quadrant we have low values of 0.03 - 0.05.  
  //For now we go with simple approach

  if (((rindex >=0)&&(rindex < 280))||((rindex < 680)&&(rindex >=400)))
    phi = low[rindex];
  
  return phi;
}


// Whether the reverse map is valid
Bool_t StFgtGeom::mReverseNaiveMappingValid = 0;

// The reverse map data member
Int_t StFgtGeom::mReverseNaiveMapping[ 2*kFgtNumStrips ];

//  Initialize our physical coordinate database here. These are:
//  isPhi?, ordinate, lowerSpan, upperSpan
//  The index corresponds to (apv*128)+channel (assuming that the apv is in
//  [0,12).  If apv is in [12,24), then the index is (apv-12)*128+channel.
StFgtGeom::StFgtGeomData StFgtGeom::mStrips[] =
{
    { false, 38.1571, 0.8905733, 1.53935776 },
    { false, 38.0617, 0.88363036, 1.53927901 },
    { false, 37.9663, 0.87651359, 1.53919987 },
    { false, 37.8709, 0.86921173, 1.53912033 },
    { false, 37.7755, 0.86171144, 1.53904038 },
    { false, 37.6802, 0.85400674, 1.53896012 },
    { false, 37.5848, 0.84606502, 1.53887937 },
    { false, 37.4894, 0.83787596, 1.53879821 },
    { false, 37.394, 0.8294177, 1.53871663 },
    { false, 37.2986, 0.82066585, 1.53863463 },
    { false, 37.2033, 0.81160142, 1.5385523 },
    { false, 37.1079, 0.80217172, 1.53846947 },
    { false, 37.0125, 0.79234517, 1.5383862 },
    { false, 36.9171, 0.78207329, 1.53830251 },
    { false, 36.8217, 0.7712965, 1.53821838 },
    { false, 36.7264, 0.75995149, 1.5381339 },
    { false, 36.631, 0.74791883, 1.5380489 },
    { false, 36.5356, 0.73508408, 1.53796345 },
    { false, 36.4402, 0.72127599, 1.53787756 },
    { false, 36.3448, 0.70624922, 1.53779121 },
    { false, 36.2495, 0.68965309, 1.53770451 },
    { false, 36.1541, 0.67084417, 1.53761725 },
    { false, 36.0587, 0.64869399, 1.53752953 },
    { false, 35.9633, 0.62045077, 1.53744135 },
    { false, 35.8679, 0.57264494, 1.5373527 },
    { false, 35.7726, 0.7866672, 1.53726367 },
    { false, 35.6772, 0.7866672, 1.53717408 },
    { false, 35.5818, 0.7866672, 1.537084 },
    { false, 35.4864, 0.7866672, 1.53699344 },
    { false, 35.391, 0.7866672, 1.53690239 },
    { false, 35.2957, 0.7866672, 1.53681094 },
    { false, 35.2003, 0.7866672, 1.5367189 },
    { false, 35.1049, 0.7866672, 1.53662637 },
    { false, 35.0095, 0.7866672, 1.53653333 },
    { false, 34.9141, 0.7866672, 1.53643978 },
    { false, 34.8188, 0.7866672, 1.53634582 },
    { false, 34.7234, 0.7866672, 1.53625125 },
    { false, 34.628, 0.7866672, 1.53615615 },
    { false, 34.5326, 0.7866672, 1.53606053 },
    { false, 34.4372, 0.7866672, 1.53596438 },
    { false, 34.3419, 0.7866672, 1.5358678 },
    { false, 34.2465, 0.7866672, 1.53577058 },
    { false, 34.1511, 0.7866672, 1.53567282 },
    { false, 34.0557, 0.7866672, 1.53557451 },
    { false, 33.9603, 0.7866672, 1.53547565 },
    { false, 33.865, 0.7866672, 1.53537633 },
    { false, 33.7696, 0.7866672, 1.53527636 },
    { false, 33.6742, 0.7866672, 1.53517581 },
    { false, 33.5788, 0.7866672, 1.53507469 },
    { false, 33.4834, 0.7866672, 1.53497301 },
    { false, 33.3881, 0.7866672, 1.53487084 },
    { false, 33.2927, 0.7866672, 1.53476799 },
    { false, 33.1973, 0.7866672, 1.53466454 },
    { false, 33.1019, 0.7866672, 1.5345605 },
    { false, 33.0065, 0.7866672, 1.53445586 },
    { false, 32.9112, 0.7866672, 1.53435072 },
    { false, 32.8158, 0.7866672, 1.53424486 },
    { false, 32.7204, 0.7866672, 1.53413839 },
    { false, 32.625, 0.7866672, 1.53403129 },
    { false, 32.5296, 0.7866672, 1.53392357 },
    { false, 32.4343, 0.7866672, 1.53381532 },
    { false, 32.3389, 0.7866672, 1.53370633 },
    { false, 32.2435, 0.7866672, 1.53359669 },
    { false, 32.1481, 0.7866672, 1.5334864 },
    { false, 32.0527, 0.7866672, 1.53337546 },
    { false, 31.9574, 0.7866672, 1.53326397 },
    { false, 31.862, 0.7866672, 1.5331517 },
    { false, 31.7666, 0.7866672, 1.53303875 },
    { false, 31.6712, 0.7866672, 1.53292513 },
    { false, 31.5758, 0.7866672, 1.53281082 },
    { false, 31.4805, 0.7866672, 1.53269594 },
    { false, 31.3851, 0.7866672, 1.53258024 },
    { false, 31.2897, 0.7866672, 1.53246383 },
    { false, 31.1943, 0.7866672, 1.53234672 },
    { false, 31.0989, 0.7866672, 1.53222889 },
    { false, 31.0036, 0.7866672, 1.53211046 },
    { false, 30.9082, 0.7866672, 1.53199117 },
    { false, 30.8128, 0.7866672, 1.53187114 },
    { false, 30.7174, 0.7866672, 1.53175038 },
    { false, 30.622, 0.7866672, 1.53162886 },
    { false, 30.5267, 0.7866672, 1.53150671 },
    { false, 30.4313, 0.7866672, 1.53138366 },
    { false, 30.3359, 0.7866672, 1.53125985 },
    { false, 30.2405, 0.7866672, 1.53113525 },
    { false, 30.1451, 0.7866672, 1.53100987 },
    { false, 30.0498, 0.7866672, 1.53088382 },
    { false, 29.9544, 0.7866672, 1.53075685 },
    { false, 29.859, 0.7866672, 1.53062905 },
    { false, 29.7636, 0.7866672, 1.53050045 },
    { false, 29.6682, 0.7866672, 1.53037102 },
    { false, 29.5729, 0.7866672, 1.53024089 },
    { false, 29.4775, 0.7866672, 1.53010978 },
    { false, 29.3821, 0.7866672, 1.52997782 },
    { false, 29.2867, 0.7866672, 1.529845 },
    { false, 29.1913, 0.7866672, 1.52971132 },
    { false, 29.096, 0.7866672, 1.5295769 },
    { false, 29.0006, 0.7866672, 1.52944147 },
    { false, 28.9052, 0.7866672, 1.52930513 },
    { false, 28.8098, 0.7866672, 1.5291679 },
    { false, 28.7144, 0.7866672, 1.52902975 },
    { false, 28.6191, 0.7866672, 1.52889084 },
    { false, 28.5237, 0.7866672, 1.52875084 },
    { false, 28.4283, 0.7866672, 1.52860991 },
    { false, 28.3329, 0.7866672, 1.52846804 },
    { false, 28.2375, 0.7866672, 1.5283252 },
    { false, 28.1422, 0.7866672, 1.52818155 },
    { false, 28.0468, 0.7866672, 1.52803677 },
    { false, 27.9514, 0.7866672, 1.52789102 },
    { false, 27.856, 0.7866672, 1.52774425 },
    { false, 27.7606, 0.7866672, 1.52759649 },
    { false, 27.6653, 0.7866672, 1.52744786 },
    { false, 27.5699, 0.7866672, 1.52729805 },
    { false, 27.4745, 0.7866672, 1.5271472 },
    { false, 27.3791, 0.7866672, 1.52699531 },
    { false, 27.2837, 0.7866672, 1.52684235 },
    { false, 27.1884, 0.7866672, 1.52668848 },
    { false, 27.093, 0.7866672, 1.52653337 },
    { false, 26.9976, 0.7866672, 1.52637717 },
    { false, 26.9022, 0.7866672, 1.52621986 },
    { false, 26.8068, 0.7866672, 1.52606143 },
    { false, 26.7115, 0.7866672, 1.52590204 },
    { false, 26.6161, 0.7866672, 1.52574135 },
    { false, 26.5207, 0.7866672, 1.52557949 },
    { false, 26.4253, 0.7866672, 1.52541648 },
    { false, 26.3299, 0.7866672, 1.52525228 },
    { false, 26.2346, 0.7866672, 1.52508707 },
    { false, 26.1392, 0.7866672, 1.52492048 },
    { false, 26.0438, 0.7866672, 1.52475266 },
    { false, 25.9484, 0.7866672, 1.52458363 },
    { false, 25.853, 0.7866672, 1.52441334 },
    { false, 25.7577, 0.7866672, 1.52424198 },
    { false, 25.6623, 0.7866672, 1.52406916 },
    { false, 25.5669, 0.7866672, 1.52389506 },
    { false, 25.4715, 0.7866672, 1.52371966 },
    { false, 25.3761, 0.7866672, 1.52354294 },
    { false, 25.2808, 0.7866672, 1.52336508 },
    { false, 25.1854, 0.7866672, 1.52318568 },
    { false, 25.09, 0.7866672, 1.52300492 },
    { false, 24.9946, 0.7866672, 1.52282279 },
    { false, 24.8992, 0.7866672, 1.52263927 },
    { false, 24.8039, 0.7866672, 1.52245453 },
    { false, 24.7085, 0.7866672, 1.52226818 },
    { false, 24.6131, 0.7866672, 1.52208038 },
    { false, 24.5177, 0.7866672, 1.52189112 },
    { false, 24.4223, 0.7866672, 1.52170039 },
    { false, 24.327, 0.7866672, 1.52150837 },
    { false, 24.2316, 0.7866672, 1.52131464 },
    { false, 24.1362, 0.7866672, 1.52111938 },
    { false, 24.0408, 0.7866672, 1.52092258 },
    { false, 23.9454, 0.7866672, 1.52072421 },
    { false, 23.8501, 0.7866672, 1.52052447 },
    { false, 23.7547, 0.7866672, 1.52032291 },
    { false, 23.6593, 0.7866672, 1.52011974 },
    { false, 23.5639, 0.7866672, 1.51991493 },
    { false, 23.4685, 0.7866672, 1.51970845 },
    { false, 23.3732, 0.7866672, 1.51950051 },
    { false, 23.2778, 0.7866672, 1.51929066 },
    { false, 23.1824, 0.7866672, 1.51907908 },
    { false, 23.087, 0.7866672, 1.51886575 },
    { false, 22.9916, 0.7866672, 1.51865067 },
    { false, 22.8963, 0.7866672, 1.51843402 },
    { false, 22.8009, 0.7866672, 1.51821534 },
    { false, 22.7055, 0.7866672, 1.51799482 },
    { false, 22.6101, 0.7866672, 1.51777245 },
    { false, 22.5147, 0.7866672, 1.5175482 },
    { false, 22.4194, 0.7866672, 1.51732228 },
    { false, 22.324, 0.7866672, 1.5170942 },
    { false, 22.2286, 0.7866672, 1.51686417 },
    { false, 22.1332, 0.7866672, 1.51663216 },
    { false, 22.0378, 0.7866672, 1.51639815 },
    { false, 21.9425, 0.7866672, 1.51616236 },
    { false, 21.8471, 0.7866672, 1.51592427 },
    { false, 21.7517, 0.7866672, 1.51568409 },
    { false, 21.6563, 0.7866672, 1.51544181 },
    { false, 21.5609, 0.7866672, 1.51519738 },
    { false, 21.4656, 0.7866672, 1.51495106 },
    { false, 21.3702, 0.7866672, 1.51470227 },
    { false, 21.2748, 0.7866672, 1.51445127 },
    { false, 21.1794, 0.7866672, 1.51419801 },
    { false, 21.084, 0.7866672, 1.51394247 },
    { false, 20.9887, 0.7866672, 1.51368488 },
    { false, 20.8933, 0.7866672, 1.51342468 },
    { false, 20.7979, 0.7866672, 1.51316209 },
    { false, 20.7025, 0.7866672, 1.5128971 },
    { false, 20.6071, 0.7866672, 1.51262966 },
    { false, 20.5118, 0.7866672, 1.51236002 },
    { false, 20.4164, 0.7866672, 1.51208759 },
    { false, 20.321, 0.7866672, 1.51181262 },
    { false, 20.2256, 0.7866672, 1.51153505 },
    { false, 20.1302, 0.7866672, 1.51125486 },
    { false, 20.0349, 0.7866672, 1.51097231 },
    { false, 19.9395, 0.7866672, 1.51068678 },
    { false, 19.8441, 0.7866672, 1.5103985 },
    { false, 19.7487, 0.7866672, 1.51010745 },
    { false, 19.6533, 0.7866672, 1.50981359 },
    { false, 19.558, 0.7866672, 1.50951718 },
    { false, 19.4626, 0.7866672, 1.50921757 },
    { false, 19.3672, 0.7866672, 1.50891501 },
    { false, 19.2718, 0.7866672, 1.50860947 },
    { false, 19.1764, 0.7866672, 1.5083009 },
    { false, 19.0736, 0.7866672, 1.50796495 },
    { false, 18.9782, 0.7866672, 1.50764995 },
    { false, 18.8828, 0.7866672, 1.50733177 },
    { false, 18.7874, 0.7866672, 1.50701038 },
    { false, 18.692, 0.7866672, 1.50668572 },
    { false, 18.5967, 0.7866672, 1.50635808 },
    { false, 18.5013, 0.7866672, 1.50602674 },
    { false, 18.4059, 0.7866672, 1.50569198 },
    { false, 18.3105, 0.7866672, 1.50535374 },
    { false, 18.2151, 0.7866672, 1.50501198 },
    { false, 18.1198, 0.7866672, 1.50466699 },
    { false, 18.0244, 0.7866672, 1.50431801 },
    { false, 17.929, 0.7866672, 1.50396533 },
    { false, 17.8336, 0.7866672, 1.50360889 },
    { false, 17.7382, 0.7866672, 1.50324864 },
    { false, 17.6429, 0.7866672, 1.50288489 },
    { false, 17.5475, 0.7866672, 1.50251683 },
    { false, 17.4521, 0.7866672, 1.50214475 },
    { false, 17.3567, 0.7866672, 1.50176861 },
    { false, 17.2613, 0.7866672, 1.50138832 },
    { false, 17.166, 0.7866672, 1.50100424 },
    { false, 17.0706, 0.7866672, 1.50061547 },
    { false, 16.9752, 0.7866672, 1.50022237 },
    { false, 16.8798, 0.7866672, 1.49982484 },
    { false, 16.7844, 0.7866672, 1.49942281 },
    { false, 16.6891, 0.7866672, 1.49901664 },
    { false, 16.5937, 0.7866672, 1.49860539 },
    { false, 16.4983, 0.7866672, 1.49818942 },
    { false, 16.4029, 0.7866672, 1.49776863 },
    { false, 16.3075, 0.7866672, 1.49734294 },
    { false, 16.2122, 0.7866672, 1.49691272 },
    { false, 16.1168, 0.7866672, 1.49647699 },
    { false, 16.0214, 0.7866672, 1.4960361 },
    { false, 15.926, 0.7866672, 1.49558995 },
    { false, 15.8306, 0.7866672, 1.49513846 },
    { false, 15.7353, 0.7866672, 1.494682 },
    { false, 15.6399, 0.7866672, 1.49421953 },
    { false, 15.5445, 0.7866672, 1.49375141 },
    { false, 15.4491, 0.7866672, 1.49327755 },
    { false, 15.3537, 0.7866672, 1.49279783 },
    { false, 15.2584, 0.7866672, 1.49231266 },
    { false, 15.163, 0.7866672, 1.49182092 },
    { false, 15.0676, 0.7866672, 1.49132298 },
    { false, 14.9722, 0.7866672, 1.49081875 },
    { false, 14.8768, 0.7866672, 1.49030808 },
    { false, 14.7815, 0.7866672, 1.48979141 },
    { false, 14.6861, 0.7866672, 1.48926753 },
    { false, 14.5907, 0.7866672, 1.48873684 },
    { false, 14.4953, 0.7866672, 1.48819922 },
    { false, 14.3999, 0.7866672, 1.48765452 },
    { false, 14.3046, 0.7866672, 1.48710319 },
    { false, 14.2092, 0.7866672, 1.48654392 },
    { false, 14.1138, 0.7866672, 1.48597715 },
    { false, 14.0184, 0.7866672, 1.48540272 },
    { false, 13.923, 0.7866672, 1.48482047 },
    { false, 13.8277, 0.7866672, 1.48423087 },
    { false, 13.7323, 0.7866672, 1.48363253 },
    { false, 13.6369, 0.7866672, 1.48302587 },
    { false, 13.5415, 0.7866672, 1.48241073 },
    { false, 13.4461, 0.7866672, 1.48178693 },
    { false, 13.3508, 0.7866672, 1.48115495 },
    { false, 13.2554, 0.7866672, 1.48051329 },
    { false, 13.16, 0.7866672, 1.4798624 },
    { false, 13.0646, 0.7866672, 1.47920207 },
    { false, 12.9692, 0.7866672, 1.47853212 },
    { false, 12.8739, 0.7866672, 1.47785304 },
    { false, 12.7785, 0.7866672, 1.47716319 },
    { false, 12.6831, 0.7866672, 1.47646305 },
    { false, 12.5877, 0.7866672, 1.4757524 },
    { false, 12.4923, 0.7866672, 1.47503098 },
    { false, 12.397, 0.7866672, 1.47429935 },
    { false, 12.3016, 0.7866672, 1.4735557 },
    { false, 12.2062, 0.7866672, 1.47280053 },
    { false, 12.1108, 0.7866672, 1.47203359 },
    { false, 12.0154, 0.7866672, 1.47125458 },
    { false, 11.9201, 0.7866672, 1.47046406 },
    { false, 11.8247, 0.7866672, 1.46966008 },
    { false, 11.7293, 0.7866672, 1.46884315 },
    { false, 11.6339, 0.7866672, 1.46801297 },
    { false, 11.5385, 0.7866672, 1.46716921 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 38.1571, 0.03143857, 0.19153084 },
    { false, 38.0617, 0.03151732, 0.19847378 },
    { false, 37.9663, 0.03159646, 0.20559055 },
    { false, 37.8709, 0.031676, 0.21289241 },
    { false, 37.7755, 0.03175594, 0.2203927 },
    { false, 37.6802, 0.0318362, 0.2280974 },
    { false, 37.5848, 0.03191696, 0.23603912 },
    { false, 37.4894, 0.03199812, 0.24422818 },
    { false, 37.394, 0.0320797, 0.25268644 },
    { false, 37.2986, 0.0321617, 0.26143829 },
    { false, 37.2033, 0.03224402, 0.27050272 },
    { false, 37.1079, 0.03232686, 0.27993242 },
    { false, 37.0125, 0.03241013, 0.28975896 },
    { false, 36.9171, 0.03249382, 0.30003085 },
    { false, 36.8217, 0.03257795, 0.31080764 },
    { false, 36.7264, 0.03266242, 0.32215264 },
    { false, 36.631, 0.03274743, 0.33418531 },
    { false, 36.5356, 0.03283288, 0.34702006 },
    { false, 36.4402, 0.03291877, 0.36082815 },
    { false, 36.3448, 0.03300511, 0.37585491 },
    { false, 36.2495, 0.03309182, 0.39245105 },
    { false, 36.1541, 0.03317908, 0.41125997 },
    { false, 36.0587, 0.03326679, 0.43341014 },
    { false, 35.9633, 0.03335497, 0.46165336 },
    { false, 35.8679, 0.03344363, 0.50945919 },
    { false, 35.7726, 0.03353265, 0.78412913 },
    { false, 35.6772, 0.03362225, 0.78412913 },
    { false, 35.5818, 0.03371233, 0.78412913 },
    { false, 35.4864, 0.03380289, 0.78412913 },
    { false, 35.391, 0.03389394, 0.78412913 },
    { false, 35.2957, 0.03398539, 0.78412913 },
    { false, 35.2003, 0.03407742, 0.78412913 },
    { false, 35.1049, 0.03416996, 0.78412913 },
    { false, 35.0095, 0.034263, 0.78412913 },
    { false, 34.9141, 0.03435654, 0.78412913 },
    { false, 34.8188, 0.03445051, 0.78412913 },
    { false, 34.7234, 0.03454508, 0.78412913 },
    { false, 34.628, 0.03464018, 0.78412913 },
    { false, 34.5326, 0.0347358, 0.78412913 },
    { false, 34.4372, 0.03483194, 0.78412913 },
    { false, 34.3419, 0.03492853, 0.78412913 },
    { false, 34.2465, 0.03502575, 0.78412913 },
    { false, 34.1511, 0.03512351, 0.78412913 },
    { false, 34.0557, 0.03522182, 0.78412913 },
    { false, 33.9603, 0.03532068, 0.78412913 },
    { false, 33.865, 0.03542, 0.78412913 },
    { false, 33.7696, 0.03551997, 0.78412913 },
    { false, 33.6742, 0.03562052, 0.78412913 },
    { false, 33.5788, 0.03572163, 0.78412913 },
    { false, 33.4834, 0.03582332, 0.78412913 },
    { false, 33.3881, 0.03592548, 0.78412913 },
    { false, 33.2927, 0.03602834, 0.78412913 },
    { false, 33.1973, 0.03613179, 0.78412913 },
    { false, 33.1019, 0.03623583, 0.78412913 },
    { false, 33.0065, 0.03634047, 0.78412913 },
    { false, 32.9112, 0.03644561, 0.78412913 },
    { false, 32.8158, 0.03655147, 0.78412913 },
    { false, 32.7204, 0.03665794, 0.78412913 },
    { false, 32.625, 0.03676504, 0.78412913 },
    { false, 32.5296, 0.03687276, 0.78412913 },
    { false, 32.4343, 0.036981, 0.78412913 },
    { false, 32.3389, 0.03709, 0.78412913 },
    { false, 32.2435, 0.03719964, 0.78412913 },
    { false, 32.1481, 0.03730992, 0.78412913 },
    { false, 32.0527, 0.03742087, 0.78412913 },
    { false, 31.9574, 0.03753235, 0.78412913 },
    { false, 31.862, 0.03764463, 0.78412913 },
    { false, 31.7666, 0.03775757, 0.78412913 },
    { false, 31.6712, 0.0378712, 0.78412913 },
    { false, 31.5758, 0.03798551, 0.78412913 },
    { false, 31.4805, 0.03810039, 0.78412913 },
    { false, 31.3851, 0.03821609, 0.78412913 },
    { false, 31.2897, 0.03833249, 0.78412913 },
    { false, 31.1943, 0.03844961, 0.78412913 },
    { false, 31.0989, 0.03856744, 0.78412913 },
    { false, 31.0036, 0.03868587, 0.78412913 },
    { false, 30.9082, 0.03880516, 0.78412913 },
    { false, 30.8128, 0.03892518, 0.78412913 },
    { false, 30.7174, 0.03904595, 0.78412913 },
    { false, 30.622, 0.03916747, 0.78412913 },
    { false, 30.5267, 0.03928962, 0.78412913 },
    { false, 30.4313, 0.03941267, 0.78412913 },
    { false, 30.3359, 0.03953648, 0.78412913 },
    { false, 30.2405, 0.03966107, 0.78412913 },
    { false, 30.1451, 0.03978646, 0.78412913 },
    { false, 30.0498, 0.0399125, 0.78412913 },
    { false, 29.9544, 0.04003948, 0.78412913 },
    { false, 29.859, 0.04016727, 0.78412913 },
    { false, 29.7636, 0.04029588, 0.78412913 },
    { false, 29.6682, 0.04042531, 0.78412913 },
    { false, 29.5729, 0.04055544, 0.78412913 },
    { false, 29.4775, 0.04068655, 0.78412913 },
    { false, 29.3821, 0.04081851, 0.78412913 },
    { false, 29.2867, 0.04095132, 0.78412913 },
    { false, 29.1913, 0.04108501, 0.78412913 },
    { false, 29.096, 0.04121942, 0.78412913 },
    { false, 29.0006, 0.04135486, 0.78412913 },
    { false, 28.9052, 0.0414912, 0.78412913 },
    { false, 28.8098, 0.04162843, 0.78412913 },
    { false, 28.7144, 0.04176657, 0.78412913 },
    { false, 28.6191, 0.04190549, 0.78412913 },
    { false, 28.5237, 0.04204548, 0.78412913 },
    { false, 28.4283, 0.04218642, 0.78412913 },
    { false, 28.3329, 0.04232829, 0.78412913 },
    { false, 28.2375, 0.04247113, 0.78412913 },
    { false, 28.1422, 0.04261477, 0.78412913 },
    { false, 28.0468, 0.04275955, 0.78412913 },
    { false, 27.9514, 0.04290531, 0.78412913 },
    { false, 27.856, 0.04305207, 0.78412913 },
    { false, 27.7606, 0.04319984, 0.78412913 },
    { false, 27.6653, 0.04334846, 0.78412913 },
    { false, 27.5699, 0.04349827, 0.78412913 },
    { false, 27.4745, 0.04364912, 0.78412913 },
    { false, 27.3791, 0.04380102, 0.78412913 },
    { false, 27.2837, 0.04395398, 0.78412913 },
    { false, 27.1884, 0.04410784, 0.78412913 },
    { false, 27.093, 0.04426296, 0.78412913 },
    { false, 26.9976, 0.04441916, 0.78412913 },
    { false, 26.9022, 0.04457647, 0.78412913 },
    { false, 26.8068, 0.0447349, 0.78412913 },
    { false, 26.7115, 0.04489429, 0.78412913 },
    { false, 26.6161, 0.04505498, 0.78412913 },
    { false, 26.5207, 0.04521683, 0.78412913 },
    { false, 26.4253, 0.04537985, 0.78412913 },
    { false, 26.3299, 0.04554404, 0.78412913 },
    { false, 26.2346, 0.04570926, 0.78412913 },
    { false, 26.1392, 0.04587585, 0.78412913 },
    { false, 26.0438, 0.04604366, 0.78412913 },
    { false, 25.9484, 0.0462127, 0.78412913 },
    { false, 25.853, 0.04638299, 0.78412913 },
    { false, 25.7577, 0.04655435, 0.78412913 },
    { false, 25.6623, 0.04672717, 0.78412913 },
    { false, 25.5669, 0.04690126, 0.78412913 },
    { false, 25.4715, 0.04707667, 0.78412913 },
    { false, 25.3761, 0.04725339, 0.78412913 },
    { false, 25.2808, 0.04743125, 0.78412913 },
    { false, 25.1854, 0.04761064, 0.78412913 },
    { false, 25.09, 0.0477914, 0.78412913 },
    { false, 24.9946, 0.04797353, 0.78412913 },
    { false, 24.8992, 0.04815706, 0.78412913 },
    { false, 24.8039, 0.0483418, 0.78412913 },
    { false, 24.7085, 0.04852815, 0.78412913 },
    { false, 24.6131, 0.04871595, 0.78412913 },
    { false, 24.5177, 0.04890521, 0.78412913 },
    { false, 24.4223, 0.04909593, 0.78412913 },
    { false, 24.327, 0.04928796, 0.78412913 },
    { false, 24.2316, 0.04948169, 0.78412913 },
    { false, 24.1362, 0.04967695, 0.78412913 },
    { false, 24.0408, 0.04987375, 0.78412913 },
    { false, 23.9454, 0.05007212, 0.78412913 },
    { false, 23.8501, 0.05027186, 0.78412913 },
    { false, 23.7547, 0.05047341, 0.78412913 },
    { false, 23.6593, 0.05067659, 0.78412913 },
    { false, 23.5639, 0.0508814, 0.78412913 },
    { false, 23.4685, 0.05108788, 0.78412913 },
    { false, 23.3732, 0.05129581, 0.78412913 },
    { false, 23.2778, 0.05150567, 0.78412913 },
    { false, 23.1824, 0.05171725, 0.78412913 },
    { false, 23.087, 0.05193057, 0.78412913 },
    { false, 22.9916, 0.05214566, 0.78412913 },
    { false, 22.8963, 0.05236231, 0.78412913 },
    { false, 22.8009, 0.05258099, 0.78412913 },
    { false, 22.7055, 0.05280151, 0.78412913 },
    { false, 22.6101, 0.05302388, 0.78412913 },
    { false, 22.5147, 0.05324813, 0.78412913 },
    { false, 22.4194, 0.05347404, 0.78412913 },
    { false, 22.324, 0.05370212, 0.78412913 },
    { false, 22.2286, 0.05393216, 0.78412913 },
    { false, 22.1332, 0.05416416, 0.78412913 },
    { false, 22.0378, 0.05439817, 0.78412913 },
    { false, 21.9425, 0.05463397, 0.78412913 },
    { false, 21.8471, 0.05487206, 0.78412913 },
    { false, 21.7517, 0.05511224, 0.78412913 },
    { false, 21.6563, 0.05535452, 0.78412913 },
    { false, 21.5609, 0.05559894, 0.78412913 },
    { false, 21.4656, 0.05584527, 0.78412913 },
    { false, 21.3702, 0.05609405, 0.78412913 },
    { false, 21.2748, 0.05634506, 0.78412913 },
    { false, 21.1794, 0.05659831, 0.78412913 },
    { false, 21.084, 0.05685386, 0.78412913 },
    { false, 20.9887, 0.05711145, 0.78412913 },
    { false, 20.8933, 0.05737165, 0.78412913 },
    { false, 20.7979, 0.05763423, 0.78412913 },
    { false, 20.7025, 0.05789923, 0.78412913 },
    { false, 20.6071, 0.05816667, 0.78412913 },
    { false, 20.5118, 0.0584363, 0.78412913 },
    { false, 20.4164, 0.05870873, 0.78412913 },
    { false, 20.321, 0.05898371, 0.78412913 },
    { false, 20.2256, 0.05926128, 0.78412913 },
    { false, 20.1302, 0.05954146, 0.78412913 },
    { false, 20.0349, 0.05982401, 0.78412913 },
    { false, 19.9395, 0.06010955, 0.78412913 },
    { false, 19.8441, 0.06039783, 0.78412913 },
    { false, 19.7487, 0.06068887, 0.78412913 },
    { false, 19.6533, 0.06098274, 0.78412913 },
    { false, 19.558, 0.06127915, 0.78412913 },
    { false, 19.4626, 0.06157876, 0.78412913 },
    { false, 19.3672, 0.06188132, 0.78412913 },
    { false, 19.2718, 0.06218686, 0.78412913 },
    { false, 19.1764, 0.06249543, 0.78412913 },
    { false, 19.0736, 0.06283138, 0.78412913 },
    { false, 18.9782, 0.06314638, 0.78412913 },
    { false, 18.8828, 0.06346455, 0.78412913 },
    { false, 18.7874, 0.06378595, 0.78412913 },
    { false, 18.692, 0.06411061, 0.78412913 },
    { false, 18.5967, 0.06443824, 0.78412913 },
    { false, 18.5013, 0.06476958, 0.78412913 },
    { false, 18.4059, 0.06510434, 0.78412913 },
    { false, 18.3105, 0.06544258, 0.78412913 },
    { false, 18.2151, 0.06578435, 0.78412913 },
    { false, 18.1198, 0.06612933, 0.78412913 },
    { false, 18.0244, 0.06647831, 0.78412913 },
    { false, 17.929, 0.066831, 0.78412913 },
    { false, 17.8336, 0.06718743, 0.78412913 },
    { false, 17.7382, 0.06754768, 0.78412913 },
    { false, 17.6429, 0.06791143, 0.78412913 },
    { false, 17.5475, 0.0682795, 0.78412913 },
    { false, 17.4521, 0.06865158, 0.78412913 },
    { false, 17.3567, 0.06902772, 0.78412913 },
    { false, 17.2613, 0.069408, 0.78412913 },
    { false, 17.166, 0.06979209, 0.78412913 },
    { false, 17.0706, 0.07018085, 0.78412913 },
    { false, 16.9752, 0.07057396, 0.78412913 },
    { false, 16.8798, 0.07097149, 0.78412913 },
    { false, 16.7844, 0.07137352, 0.78412913 },
    { false, 16.6891, 0.07177968, 0.78412913 },
    { false, 16.5937, 0.07219093, 0.78412913 },
    { false, 16.4983, 0.07260691, 0.78412913 },
    { false, 16.4029, 0.07302769, 0.78412913 },
    { false, 16.3075, 0.07345339, 0.78412913 },
    { false, 16.2122, 0.0738836, 0.78412913 },
    { false, 16.1168, 0.07431933, 0.78412913 },
    { false, 16.0214, 0.07476023, 0.78412913 },
    { false, 15.926, 0.07520638, 0.78412913 },
    { false, 15.8306, 0.07565787, 0.78412913 },
    { false, 15.7353, 0.07611433, 0.78412913 },
    { false, 15.6399, 0.0765768, 0.78412913 },
    { false, 15.5445, 0.07704491, 0.78412913 },
    { false, 15.4491, 0.07751878, 0.78412913 },
    { false, 15.3537, 0.07799849, 0.78412913 },
    { false, 15.2584, 0.07848366, 0.78412913 },
    { false, 15.163, 0.07897541, 0.78412913 },
    { false, 15.0676, 0.07947334, 0.78412913 },
    { false, 14.9722, 0.07997758, 0.78412913 },
    { false, 14.8768, 0.08048824, 0.78412913 },
    { false, 14.7815, 0.08100491, 0.78412913 },
    { false, 14.6861, 0.0815288, 0.78412913 },
    { false, 14.5907, 0.08205948, 0.78412913 },
    { false, 14.4953, 0.08259711, 0.78412913 },
    { false, 14.3999, 0.08314181, 0.78412913 },
    { false, 14.3046, 0.08369314, 0.78412913 },
    { false, 14.2092, 0.08425241, 0.78412913 },
    { false, 14.1138, 0.08481918, 0.78412913 },
    { false, 14.0184, 0.0853936, 0.78412913 },
    { false, 13.923, 0.08597586, 0.78412913 },
    { false, 13.8277, 0.08656545, 0.78412913 },
    { false, 13.7323, 0.0871638, 0.78412913 },
    { false, 13.6369, 0.08777046, 0.78412913 },
    { false, 13.5415, 0.0883856, 0.78412913 },
    { false, 13.4461, 0.0890094, 0.78412913 },
    { false, 13.3508, 0.08964138, 0.78412913 },
    { false, 13.2554, 0.09028304, 0.78412913 },
    { false, 13.16, 0.09093393, 0.78412913 },
    { false, 13.0646, 0.09159426, 0.78412913 },
    { false, 12.9692, 0.09226421, 0.78412913 },
    { false, 12.8739, 0.09294329, 0.78412913 },
    { false, 12.7785, 0.09363314, 0.78412913 },
    { false, 12.6831, 0.09433328, 0.78412913 },
    { false, 12.5877, 0.09504393, 0.78412913 },
    { false, 12.4923, 0.09576534, 0.78412913 },
    { false, 12.397, 0.09649698, 0.78412913 },
    { false, 12.3016, 0.09724063, 0.78412913 },
    { false, 12.2062, 0.0979958, 0.78412913 },
    { false, 12.1108, 0.09876274, 0.78412913 },
    { false, 12.0154, 0.09954175, 0.78412913 },
    { false, 11.9201, 0.10033227, 0.78412913 },
    { false, 11.8247, 0.10113625, 0.78412913 },
    { false, 11.7293, 0.10195317, 0.78412913 },
    { false, 11.6339, 0.10278336, 0.78412913 },
    { false, 11.5385, 0.10362712, 0.78412913 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { false, 0.0, 0.0, 0.0 },
    { true, 1.53841996, 37.0706, 38.25 },
    { true, 1.53632998, 34.8235, 38.25 },
    { true, 1.53422999, 32.8244, 38.25 },
    { true, 1.53214002, 31.0505, 38.25 },
    { true, 1.53004003, 29.4515, 38.25 },
    { true, 1.52795005, 28.0157, 38.25 },
    { true, 1.52585006, 26.7075, 38.25 },
    { true, 1.52375996, 25.5216, 38.25 },
    { true, 1.52165997, 24.4317, 38.25 },
    { true, 1.51956999, 23.4357, 38.25 },
    { true, 1.51748002, 22.5179, 38.25 },
    { true, 1.51538002, 21.6654, 38.25 },
    { true, 1.51329005, 20.8788, 38.25 },
    { true, 1.51119006, 20.144, 38.25 },
    { true, 1.50909996, 19.4624, 38.25 },
    { true, 1.50699997, 19.125, 38.25 },
    { true, 1.50490999, 18.2264, 38.25 },
    { true, 1.50281, 19.125, 38.25 },
    { true, 1.50072002, 17.1382, 38.25 },
    { true, 1.49862003, 19.125, 38.25 },
    { true, 1.49653006, 16.1729, 38.25 },
    { true, 1.49442995, 19.125, 38.25 },
    { true, 1.49233997, 15.3108, 38.25 },
    { true, 1.49023998, 19.125, 38.25 },
    { true, 1.48815, 14.5362, 38.25 },
    { true, 1.48606002, 19.125, 38.25 },
    { true, 1.48396003, 13.8365, 38.25 },
    { true, 1.48187006, 19.125, 38.25 },
    { true, 1.47976995, 13.2012, 38.25 },
    { true, 1.47767997, 19.125, 38.25 },
    { true, 1.47557998, 12.6219, 38.25 },
    { true, 1.47349, 19.125, 38.25 },
    { true, 1.47139001, 12.0916, 38.25 },
    { true, 1.46930003, 19.125, 38.25 },
    { true, 1.46720004, 11.6042, 38.25 },
    { true, 1.46510994, 19.125, 38.25 },
    { true, 1.46300995, 11.5, 38.25 },
    { true, 1.46091998, 19.125, 38.25 },
    { true, 1.45883, 11.5, 38.25 },
    { true, 1.45673001, 19.125, 38.25 },
    { true, 1.45464003, 11.5, 38.25 },
    { true, 1.45254004, 19.125, 38.25 },
    { true, 1.45044994, 11.5, 38.25 },
    { true, 1.44834995, 19.125, 38.25 },
    { true, 1.44625998, 11.5, 38.25 },
    { true, 1.44415998, 19.125, 38.25 },
    { true, 1.44207001, 11.5, 38.25 },
    { true, 1.43997002, 19.125, 38.25 },
    { true, 1.43788004, 11.5, 38.25 },
    { true, 1.43578005, 19.125, 38.25 },
    { true, 1.43368995, 11.5, 38.25 },
    { true, 1.43158996, 19.125, 38.25 },
    { true, 1.42949998, 11.5, 38.25 },
    { true, 1.42741001, 19.125, 38.25 },
    { true, 1.42531002, 11.5, 38.25 },
    { true, 1.42322004, 19.125, 38.25 },
    { true, 1.42112005, 11.5, 38.25 },
    { true, 1.41902995, 19.125, 38.25 },
    { true, 1.41692996, 11.5, 38.25 },
    { true, 1.41483998, 19.125, 38.25 },
    { true, 1.41273999, 11.5, 38.25 },
    { true, 1.41065001, 19.125, 38.25 },
    { true, 1.40855002, 11.5, 38.25 },
    { true, 1.40646005, 19.125, 38.25 },
    { true, 1.40436006, 11.5, 38.25 },
    { true, 1.40226996, 19.125, 38.25 },
    { true, 1.40017998, 11.5, 38.25 },
    { true, 1.39807999, 19.125, 38.25 },
    { true, 1.39599001, 11.5, 38.25 },
    { true, 1.39389002, 19.125, 38.25 },
    { true, 1.39180005, 11.5, 38.25 },
    { true, 1.38970006, 19.125, 38.25 },
    { true, 1.38760996, 11.5, 38.25 },
    { true, 1.38550997, 19.125, 38.25 },
    { true, 1.38341999, 11.5, 38.25 },
    { true, 1.38132, 19.125, 38.25 },
    { true, 1.37923002, 11.5, 38.25 },
    { true, 1.37713003, 19.125, 38.25 },
    { true, 1.37504005, 11.5, 38.25 },
    { true, 1.37293994, 19.125, 38.25 },
    { true, 1.37084997, 11.5, 38.25 },
    { true, 1.36875999, 19.125, 38.25 },
    { true, 1.36666, 11.5, 38.25 },
    { true, 1.36457002, 19.125, 38.25 },
    { true, 1.36247003, 11.5, 38.25 },
    { true, 1.36038005, 19.125, 38.25 },
    { true, 1.35827994, 11.5, 38.25 },
    { true, 1.35618997, 19.125, 38.25 },
    { true, 1.35408998, 11.5, 38.25 },
    { true, 1.352, 19.125, 38.25 },
    { true, 1.34990001, 11.5, 38.25 },
    { true, 1.34781003, 19.125, 38.25 },
    { true, 1.34571004, 11.5, 38.25 },
    { true, 1.34361994, 19.125, 38.25 },
    { true, 1.34152997, 11.5, 38.25 },
    { true, 1.33942997, 19.125, 38.25 },
    { true, 1.33734, 11.5, 38.25 },
    { true, 1.33524001, 19.125, 38.25 },
    { true, 1.33315003, 11.5, 38.25 },
    { true, 1.33105004, 19.125, 38.25 },
    { true, 1.32895994, 11.5, 38.25 },
    { true, 1.32685995, 19.125, 38.25 },
    { true, 1.32476997, 11.5, 38.25 },
    { true, 1.32266998, 19.125, 38.25 },
    { true, 1.32058001, 11.5, 38.25 },
    { true, 1.31848001, 19.125, 38.25 },
    { true, 1.31639004, 11.5, 38.25 },
    { true, 1.31429005, 19.125, 38.25 },
    { true, 1.31219995, 11.5, 38.25 },
    { true, 1.31010997, 19.125, 38.25 },
    { true, 1.30800998, 11.5, 38.25 },
    { true, 1.30592, 19.125, 38.25 },
    { true, 1.30382001, 11.5, 38.25 },
    { true, 1.30173004, 19.125, 38.25 },
    { true, 1.29963005, 11.5, 38.25 },
    { true, 1.29753995, 19.125, 38.25 },
    { true, 1.29543996, 11.5, 38.25 },
    { true, 1.29334998, 19.125, 38.25 },
    { true, 1.29124999, 11.5, 38.25 },
    { true, 1.28916001, 19.125, 38.25 },
    { true, 1.28706002, 11.5, 38.25 },
    { true, 1.28497005, 19.125, 38.25 },
    { true, 1.28287995, 11.5, 38.25 },
    { true, 1.28077996, 19.125, 38.25 },
    { true, 1.27868998, 11.5, 38.25 },
    { true, 1.27658999, 19.125, 38.25 },
    { true, 1.27450001, 11.5, 38.25 },
    { true, 1.27240002, 19.125, 38.25 },
    { true, 1.27031004, 11.5, 38.25 },
    { true, 1.26821005, 19.125, 38.25 },
    { true, 1.26611996, 11.5, 38.25 },
    { true, 1.26401997, 19.125, 38.25 },
    { true, 1.26192999, 11.5, 38.25 },
    { true, 1.25983, 19.125, 38.25 },
    { true, 1.25774002, 11.5, 38.25 },
    { true, 1.25565004, 19.125, 38.25 },
    { true, 1.25355005, 11.5, 38.25 },
    { true, 1.25145996, 19.125, 38.25 },
    { true, 1.24935997, 11.5, 38.25 },
    { true, 1.24726999, 19.125, 38.25 },
    { true, 1.24517, 11.5, 38.25 },
    { true, 1.24308002, 19.125, 38.25 },
    { true, 1.24098003, 11.5, 38.25 },
    { true, 1.23889005, 19.125, 38.25 },
    { true, 1.23678994, 11.5, 38.25 },
    { true, 1.23469996, 19.125, 38.25 },
    { true, 1.23259997, 11.5, 38.25 },
    { true, 1.23051, 19.125, 38.25 },
    { true, 1.22841001, 11.5, 38.25 },
    { true, 1.22632003, 19.125, 38.25 },
    { true, 1.22423005, 11.5, 38.25 },
    { true, 1.22212994, 19.125, 38.25 },
    { true, 1.22003996, 11.5, 38.25 },
    { true, 1.21793997, 19.125, 38.25 },
    { true, 1.21585, 11.5, 38.25 },
    { true, 1.21375, 19.125, 38.25 },
    { true, 1.21166003, 11.5, 38.25 },
    { true, 1.20956004, 19.125, 38.25 },
    { true, 1.20747006, 11.5, 38.25 },
    { true, 1.20536995, 19.125, 38.25 },
    { true, 1.20327997, 11.5, 38.25 },
    { true, 1.20117998, 19.125, 38.25 },
    { true, 1.19909, 11.5, 38.25 },
    { true, 1.19700003, 19.125, 38.25 },
    { true, 1.19490004, 11.5, 38.25 },
    { true, 1.19281006, 19.125, 38.25 },
    { true, 1.19070995, 11.5, 38.25 },
    { true, 1.18861997, 19.125, 38.25 },
    { true, 1.18651998, 11.5, 38.25 },
    { true, 1.18443, 19.125, 38.25 },
    { true, 1.18233001, 11.5, 38.25 },
    { true, 1.18024004, 19.125, 38.25 },
    { true, 1.17814004, 11.5, 38.25 },
    { true, 1.17604995, 19.125, 38.25 },
    { true, 1.17394996, 11.5, 38.25 },
    { true, 1.17185998, 19.125, 38.25 },
    { true, 1.16975999, 11.5, 38.25 },
    { true, 1.16767001, 19.125, 38.25 },
    { true, 1.16558003, 11.5, 38.25 },
    { true, 1.16348004, 19.125, 38.25 },
    { true, 1.16138995, 11.5, 38.25 },
    { true, 1.15928996, 19.125, 38.25 },
    { true, 1.15719998, 11.5, 38.25 },
    { true, 1.15509999, 19.125, 38.25 },
    { true, 1.15301001, 11.5, 38.25 },
    { true, 1.15091002, 19.125, 38.25 },
    { true, 1.14882004, 11.5, 38.25 },
    { true, 1.14672005, 19.125, 38.25 },
    { true, 1.14462996, 11.5, 38.25 },
    { true, 1.14252996, 19.125, 38.25 },
    { true, 1.14043999, 11.5, 38.25 },
    { true, 1.13835001, 19.125, 38.25 },
    { true, 1.13625002, 11.5, 38.25 },
    { true, 1.13416004, 19.125, 38.25 },
    { true, 1.13206005, 11.5, 38.25 },
    { true, 1.12996995, 19.125, 38.25 },
    { true, 1.12786996, 11.5, 38.25 },
    { true, 1.12577999, 19.125, 38.25 },
    { true, 1.12368, 11.5, 38.25 },
    { true, 1.12159002, 19.125, 38.25 },
    { true, 1.11949003, 11.5, 38.25 },
    { true, 1.11740005, 19.125, 38.25 },
    { true, 1.11530006, 11.5, 38.25 },
    { true, 1.11320996, 19.125, 38.25 },
    { true, 1.11110997, 11.5, 38.25 },
    { true, 1.10901999, 19.125, 38.25 },
    { true, 1.10693002, 11.5, 38.25 },
    { true, 1.10483003, 19.125, 38.25 },
    { true, 1.10274005, 11.5, 38.25 },
    { true, 1.10064006, 19.125, 38.25 },
    { true, 1.09854996, 11.5, 38.25 },
    { true, 1.09644997, 19.125, 38.25 },
    { true, 1.09435999, 11.5, 38.25 },
    { true, 1.09226, 19.125, 38.25 },
    { true, 1.09017003, 11.5, 38.25 },
    { true, 1.08807003, 19.125, 38.25 },
    { true, 1.08598006, 11.5, 38.25 },
    { true, 1.08387995, 19.125, 38.25 },
    { true, 1.08178997, 11.5, 38.25 },
    { true, 1.07969999, 19.125, 38.25 },
    { true, 1.0776, 11.5, 38.25 },
    { true, 1.07551003, 19.125, 38.25 },
    { true, 1.07341003, 11.5, 38.25 },
    { true, 1.07132006, 19.125, 38.25 },
    { true, 1.06921995, 11.5, 38.25 },
    { true, 1.06712997, 19.125, 38.25 },
    { true, 1.06502998, 11.5, 38.25 },
    { true, 1.06294, 19.125, 38.25 },
    { true, 1.06084001, 11.5, 38.25 },
    { true, 1.05875003, 19.125, 38.25 },
    { true, 1.05665004, 11.5, 38.25 },
    { true, 1.05455995, 19.125, 38.25 },
    { true, 1.05245996, 11.5, 38.25 },
    { true, 1.05036998, 19.125, 38.25 },
    { true, 1.04828, 11.5, 38.25 },
    { true, 1.04618001, 19.125, 38.25 },
    { true, 1.04409003, 11.5, 38.25 },
    { true, 1.04199004, 19.125, 38.25 },
    { true, 1.03989995, 11.5, 38.25 },
    { true, 1.03779995, 19.125, 38.25 },
    { true, 1.03570998, 11.5, 38.25 },
    { true, 1.03360999, 19.125, 38.25 },
    { true, 1.03152001, 11.5, 38.25 },
    { true, 1.02942002, 19.125, 38.25 },
    { true, 1.02733004, 11.5, 38.25 },
    { true, 1.02523005, 19.125, 38.25 },
    { true, 1.02313995, 11.5, 38.25 },
    { true, 1.02104998, 19.125, 38.25 },
    { true, 1.01894999, 11.5, 38.25 },
    { true, 1.01686001, 19.125, 38.25 },
    { true, 1.01476002, 11.5, 38.25 },
    { true, 1.01267004, 19.125, 38.25 },
    { true, 1.01057005, 11.5, 38.25 },
    { true, 1.00847995, 19.125, 38.25 },
    { true, 1.00637996, 11.5, 38.25 },
    { true, 1.00428998, 19.125, 38.25 },
    { true, 1.00218999, 11.5, 38.25 },
    { true, 1.00010002, 19.125, 38.25 },
    { true, 0.99800402, 11.5, 38.25 },
    { true, 0.99590999, 19.125, 38.25 },
    { true, 0.993815, 11.5, 38.25 },
    { true, 0.99172002, 19.125, 38.25 },
    { true, 0.98962599, 11.5, 38.25 },
    { true, 0.98753101, 19.125, 38.25 },
    { true, 0.98543602, 11.5, 38.25 },
    { true, 0.98334199, 19.125, 38.25 },
    { true, 0.98124701, 11.5, 38.25 },
    { true, 0.97915202, 19.125, 38.25 },
    { true, 0.97705799, 11.5, 38.25 },
    { true, 0.97496301, 19.125, 38.25 },
    { true, 0.97286898, 11.5, 38.25 },
    { true, 0.97077399, 19.125, 38.25 },
    { true, 0.96867901, 11.5, 38.25 },
    { true, 0.96658498, 19.125, 38.25 },
    { true, 0.96449, 11.5, 38.25 },
    { true, 0.96239501, 19.125, 38.25 },
    { true, 0.96030098, 11.5, 38.25 },
    { true, 0.958206, 19.125, 38.25 },
    { true, 0.95611101, 11.5, 38.25 },
    { true, 0.95401698, 19.125, 38.25 },
    { true, 0.951922, 11.5, 38.25 },
    { true, 0.94982702, 19.125, 38.25 },
    { true, 0.94773299, 11.5, 38.25 },
    { true, 0.945638, 19.125, 38.25 },
    { true, 0.94354397, 11.5, 38.25 },
    { true, 0.94144899, 19.125, 38.25 },
    { true, 0.939354, 11.5, 38.25 },
    { true, 0.93725997, 19.125, 38.25 },
    { true, 0.93516499, 11.5, 38.25 },
    { true, 0.93307, 19.125, 38.25 },
    { true, 0.93097597, 11.5, 38.25 },
    { true, 0.92888099, 19.125, 38.25 },
    { true, 0.92678601, 11.5, 38.25 },
    { true, 0.92469198, 19.125, 38.25 },
    { true, 0.92259699, 11.5, 38.25 },
    { true, 0.92050302, 19.125, 38.25 },
    { true, 0.91840798, 11.5, 38.25 },
    { true, 0.91631299, 19.125, 38.25 },
    { true, 0.91421902, 11.5, 38.25 },
    { true, 0.91212398, 19.125, 38.25 },
    { true, 0.91002899, 11.5, 38.25 },
    { true, 0.90793502, 19.125, 38.25 },
    { true, 0.90583998, 11.5, 38.25 },
    { true, 0.903745, 19.125, 38.25 },
    { true, 0.90165102, 11.5, 38.25 },
    { true, 0.89955598, 19.125, 38.25 },
    { true, 0.897461, 11.5, 38.25 },
    { true, 0.89536703, 19.125, 38.2243 },
    { true, 0.89327198, 11.5, 38.1948 },
    { true, 0.89117801, 19.125, 38.1655 },
    { true, 0.88908303, 11.5, 38.1364 },
    { true, 0.88698798, 19.125, 38.1075 },
    { true, 0.88489401, 11.5, 38.0789 },
    { true, 0.88279903, 19.125, 38.0504 },
    { true, 0.88070399, 11.5, 38.0222 },
    { true, 0.87861001, 19.125, 37.9942 },
    { true, 0.87651497, 11.5, 37.9663 },
    { true, 0.87441999, 19.125, 37.9387 },
    { true, 0.87232602, 11.5, 37.9113 },
    { true, 0.87023097, 19.125, 37.8841 },
    { true, 0.86813599, 11.5, 37.8571 },
    { true, 0.86604202, 19.125, 37.8303 },
    { true, 0.86394697, 11.5, 37.8037 },
    { true, 0.861853, 19.125, 37.7773 },
    { true, 0.85975802, 11.5, 37.7511 },
    { true, 0.85766298, 19.125, 37.7251 },
    { true, 0.85556901, 11.5, 37.6993 },
    { true, 0.85347402, 19.125, 37.6737 },
    { true, 0.85137898, 11.5, 37.6483 },
    { true, 0.84928501, 19.125, 37.6231 },
    { true, 0.84719002, 11.5, 37.5981 },
    { true, 0.84509498, 19.125, 37.5733 },
    { true, 0.84300101, 11.5, 37.5488 },
    { true, 0.84090602, 19.125, 37.5244 },
    { true, 0.83881199, 11.5, 37.5002 },
    { true, 0.83671701, 19.125, 37.4761 },
    { true, 0.83462203, 11.5, 37.4523 },
    { true, 0.832528, 19.125, 37.4287 },
    { true, 0.83043301, 11.5, 37.4053 },
    { true, 0.82833803, 19.125, 37.382 },
    { true, 0.826244, 11.5, 37.359 },
    { true, 0.82414901, 19.125, 37.3362 },
    { true, 0.82205403, 11.5, 37.3135 },
    { true, 0.81996, 19.125, 37.2911 },
    { true, 0.81786501, 11.5, 37.2688 },
    { true, 0.81576997, 19.125, 37.2467 },
    { true, 0.813676, 11.5, 37.2248 },
    { true, 0.81158102, 19.125, 37.2031 },
    { true, 0.80948699, 11.5, 37.1816 },
    { true, 0.807392, 19.125, 37.1602 },
    { true, 0.80529702, 11.5, 37.1391 },
    { true, 0.80320299, 19.125, 37.1181 },
    { true, 0.801108, 11.5, 37.0974 },
    { true, 0.79901302, 19.125, 37.0768 },
    { true, 0.79691899, 11.5, 37.0564 },
    { true, 0.794824, 19.125, 37.0362 },
    { true, 0.79272902, 11.5, 37.0162 },
    { true, 0.79063499, 19.125, 36.9963 },
    { true, 0.78854001, 11.5, 36.9766 },
    { true, 0.78644502, 19.125, 36.9572 },
    { true, 0.78435099, 11.5, 36.9379 },
    { true, 0.78225601, 19.125, 36.9188 },
    { true, 0.78016198, 11.5, 36.8998 },
    { true, 0.77806699, 19.125, 36.8811 },
    { true, 0.77597201, 11.5, 36.8625 },
    { true, 0.77387798, 19.125, 36.8441 },
    { true, 0.77178299, 11.5, 36.8259 },
    { true, 0.76968801, 19.125, 36.8079 },
    { true, 0.76759398, 11.5, 36.79 },
    { true, 0.765499, 19.125, 36.7723 },
    { true, 0.76340401, 11.5, 36.7548 },
    { true, 0.76130998, 19.125, 36.7375 },
    { true, 0.759215, 11.5, 36.7204 },
    { true, 0.75712103, 19.125, 36.7034 },
    { true, 0.75502598, 11.5, 36.6866 },
    { true, 0.752931, 19.125, 36.67 },
    { true, 0.75083703, 11.5, 36.6536 },
    { true, 0.74874198, 19.125, 36.6373 },
    { true, 0.746647, 11.5, 36.6213 },
    { true, 0.74455303, 19.125, 36.6053 },
    { true, 0.74245799, 11.5, 36.5896 },
    { true, 0.740363, 19.125, 36.574 },
    { true, 0.73826897, 11.5, 36.5587 },
    { true, 0.73617399, 19.125, 36.5434 },
    { true, 0.734079, 11.5, 36.5284 },
    { true, 0.73198497, 19.125, 36.5135 },
    { true, 0.72988999, 11.5, 36.4988 },
    { true, 0.72779602, 19.125, 36.4843 },
    { true, 0.72570097, 11.5, 36.47 },
    { true, 0.72360599, 19.125, 36.4558 },
    { true, 0.72151202, 11.5, 36.4418 },
    { true, 0.71941698, 19.125, 36.4279 },
    { true, 0.71732199, 11.5, 36.4143 },
    { true, 0.71522802, 19.125, 36.4008 },
    { true, 0.71313298, 11.5, 36.3874 },
    { true, 0.71103799, 19.125, 36.3743 },
    { true, 0.70894402, 11.5, 36.3613 },
    { true, 0.70684898, 19.125, 36.3484 },
    { true, 0.70475399, 11.5, 36.3358 },
    { true, 0.70266002, 19.125, 36.3233 },
    { true, 0.70056498, 11.5, 36.311 },
    { true, 0.69847101, 19.125, 36.2988 },
    { true, 0.69637603, 11.5, 36.2868 },
    { true, 0.69428098, 19.125, 36.275 },
    { true, 0.69218701, 11.5, 36.2634 },
    { true, 0.69009203, 19.125, 36.2519 },
    { true, 0.68799698, 11.5, 36.2406 },
    { true, 0.68590301, 19.125, 36.2294 },
    { true, 0.68380803, 11.5, 36.2184 },
    { true, 0.68171299, 19.125, 36.2076 },
    { true, 0.67961901, 11.5, 36.1969 },
    { true, 0.67752397, 19.125, 36.1865 },
    { true, 0.67543, 11.5, 36.1761 },
    { true, 0.67333502, 19.125, 36.166 },
    { true, 0.67123997, 11.5, 36.156 },
    { true, 0.669146, 19.125, 36.1461 },
    { true, 0.66705102, 11.5, 36.1365 },
    { true, 0.66495597, 19.125, 36.127 },
    { true, 0.662862, 11.5, 36.1176 },
    { true, 0.66076702, 19.125, 36.1084 },
    { true, 0.65867198, 11.5, 36.0994 },
    { true, 0.656578, 19.125, 36.0906 },
    { true, 0.65448302, 11.5, 36.0819 },
    { true, 0.65238798, 19.125, 36.0733 },
    { true, 0.65029401, 11.5, 36.065 },
    { true, 0.64819902, 19.125, 36.0568 },
    { true, 0.64610499, 11.5, 36.0487 },
    { true, 0.64401001, 19.125, 36.0409 },
    { true, 0.64191502, 11.5, 36.0331 },
    { true, 0.63982099, 19.125, 36.0256 },
    { true, 0.63772601, 11.5, 36.0182 },
    { true, 0.63563102, 19.125, 36.0109 },
    { true, 0.63353699, 11.5, 36.0039 },
    { true, 0.63144201, 19.125, 35.997 },
    { true, 0.62934703, 11.5, 35.9902 },
    { true, 0.627253, 19.125, 35.9836 },
    { true, 0.62515801, 11.5, 35.9772 },
    { true, 0.62306303, 19.125, 35.9709 },
    { true, 0.620969, 11.5, 35.9648 },
    { true, 0.61887401, 19.125, 35.9588 },
    { true, 0.61677998, 11.5, 35.953 },
    { true, 0.614685, 19.125, 35.9474 },
    { true, 0.61259001, 11.5, 35.9419 },
    { true, 0.61049598, 19.125, 35.9366 },
    { true, 0.608401, 11.5, 35.9315 },
    { true, 0.60630602, 19.125, 35.9265 },
    { true, 0.60421199, 11.5, 35.9216 },
    { true, 0.602117, 19.125, 35.9169 },
    { true, 0.60002202, 11.5, 35.9124 },
    { true, 0.59792799, 19.125, 35.9081 },
    { true, 0.595833, 11.5, 35.9039 },
    { true, 0.59373897, 19.125, 35.8998 },
    { true, 0.59164399, 11.5, 35.8959 },
    { true, 0.58954901, 19.125, 35.8922 },
    { true, 0.58745497, 11.5, 35.8886 },
    { true, 0.58535999, 19.125, 35.8852 },
    { true, 0.58326501, 11.5, 35.882 },
    { true, 0.58117098, 19.125, 35.8789 },
    { true, 0.57907599, 11.5, 35.8759 },
    { true, 0.57698101, 19.125, 35.8732 },
    { true, 0.57488698, 11.5, 35.8705 },
    { true, 0.57279199, 19.125, 35.8681 },
    { true, 0.57069701, 11.5, 35.8658 },
    { true, 0.56860298, 19.125, 35.8636 },
    { true, 0.566508, 11.5, 35.8616 },
    { true, 0.56441402, 19.125, 35.8598 },
    { true, 0.56231898, 11.5, 35.8581 },
    { true, 0.560224, 19.125, 35.8566 },
    { true, 0.55813003, 11.5, 35.8552 },
    { true, 0.55603498, 19.125, 35.854 },
    { true, 0.55394, 11.5, 35.853 },
    { true, 0.55184603, 19.125, 35.8521 },
    { true, 0.54975098, 11.5, 35.8514 },
    { true, 0.547656, 19.125, 35.8508 },
    { true, 0.54556203, 11.5, 35.8504 },
    { true, 0.54346699, 19.125, 35.8501 },
    { true, 0.541372, 11.5, 35.85 },
    { true, 0.53927797, 19.125, 35.8501 },
    { true, 0.53718299, 11.5, 35.8503 },
    { true, 0.53508902, 19.125, 35.8506 },
    { true, 0.53299397, 11.5, 35.8512 },
    { true, 0.53089899, 19.125, 35.8518 },
    { true, 0.52880502, 11.5, 35.8527 },
    { true, 0.52670997, 19.125, 35.8537 },
    { true, 0.52461499, 11.5, 35.8548 },
    { true, 0.52252102, 19.125, 35.8562 },
    { true, 0.52042598, 11.5, 35.8576 },
    { true, 0.51833099, 19.125, 35.8593 },
    { true, 0.51623702, 11.5, 35.861 },
    { true, 0.51414198, 19.125, 35.863 },
    { true, 0.51204801, 11.5, 35.8651 },
    { true, 0.50995302, 19.125, 35.8673 },
    { true, 0.50785798, 11.5, 35.8698 },
    { true, 0.50576401, 19.125, 35.8723 },
    { true, 0.50366902, 11.5, 35.8751 },
    { true, 0.50157398, 19.125, 35.878 },
    { true, 0.49948001, 11.5, 35.881 },
    { true, 0.497385, 19.125, 35.8842 },
    { true, 0.49529001, 11.5, 35.8876 },
    { true, 0.49319601, 19.125, 35.8911 },
    { true, 0.491101, 11.5, 35.8948 },
    { true, 0.48900601, 19.125, 35.8986 },
    { true, 0.48691201, 11.5, 35.9026 },
    { true, 0.484817, 19.125, 35.9068 },
    { true, 0.482723, 11.5, 35.9111 },
    { true, 0.48062801, 19.125, 35.9155 },
    { true, 0.478533, 11.5, 35.9202 },
    { true, 0.476439, 19.125, 35.925 },
    { true, 0.47434399, 11.5, 35.9299 },
    { true, 0.472249, 19.125, 35.935 },
    { true, 0.470155, 11.5, 35.9403 },
    { true, 0.46805999, 19.125, 35.9457 },
    { true, 0.465965, 11.5, 35.9513 },
    { true, 0.463871, 19.125, 35.957 },
    { true, 0.46177599, 11.5, 35.9629 },
    { true, 0.45968199, 19.125, 35.969 },
    { true, 0.457587, 11.5, 35.9752 },
    { true, 0.45549199, 19.125, 35.9816 },
    { true, 0.45339799, 11.5, 35.9882 },
    { true, 0.45130301, 19.125, 35.9949 },
    { true, 0.44920799, 11.5, 36.0017 },
    { true, 0.44711399, 19.125, 36.0088 },
    { true, 0.44501901, 11.5, 36.0159 },
    { true, 0.44292399, 19.125, 36.0233 },
    { true, 0.44082999, 11.5, 36.0308 },
    { true, 0.43873501, 19.125, 36.0385 },
    { true, 0.43663999, 11.5, 36.0463 },
    { true, 0.43454599, 19.125, 36.0543 },
    { true, 0.43245101, 11.5, 36.0625 },
    { true, 0.43035701, 19.125, 36.0708 },
    { true, 0.428262, 11.5, 36.0792 },
    { true, 0.42616701, 19.125, 36.0879 },
    { true, 0.42407301, 11.5, 36.0967 },
    { true, 0.421978, 19.125, 36.1057 },
    { true, 0.41988301, 11.5, 36.1148 },
    { true, 0.41778901, 19.125, 36.1241 },
    { true, 0.415694, 11.5, 36.1335 },
    { true, 0.41359901, 19.125, 36.1432 },
    { true, 0.41150501, 11.5, 36.1529 },
    { true, 0.40941, 19.125, 36.1629 },
    { true, 0.40731499, 11.5, 36.173 },
    { true, 0.40522099, 19.125, 36.1833 },
    { true, 0.403126, 11.5, 36.1937 },
    { true, 0.401032, 19.125, 36.2043 },
    { true, 0.39893699, 11.5, 36.2151 },
    { true, 0.396842, 19.125, 36.226 },
    { true, 0.394748, 11.5, 36.2371 },
    { true, 0.39265299, 19.125, 36.2484 },
    { true, 0.390558, 11.5, 36.2598 },
    { true, 0.388464, 19.125, 36.2714 },
    { true, 0.38636899, 11.5, 36.2832 },
    { true, 0.38427401, 19.125, 36.2951 },
    { true, 0.38218001, 11.5, 36.3072 },
    { true, 0.38008499, 19.125, 36.3195 },
    { true, 0.37799099, 11.5, 36.3319 },
    { true, 0.37589601, 19.125, 36.3446 },
    { true, 0.37380099, 11.5, 36.3573 },
    { true, 0.37170699, 19.125, 36.3703 },
    { true, 0.36961201, 11.5, 36.3834 },
    { true, 0.36751699, 19.125, 36.3967 },
    { true, 0.36542299, 11.5, 36.4101 },
    { true, 0.36332801, 19.125, 36.4237 },
    { true, 0.361233, 11.5, 36.4375 },
    { true, 0.359139, 19.125, 36.4515 },
    { true, 0.35704401, 11.5, 36.4656 },
    { true, 0.354949, 19.125, 36.4799 },
    { true, 0.352855, 11.5, 36.4944 },
    { true, 0.35076001, 19.125, 36.509 },
    { true, 0.34866601, 11.5, 36.5238 },
    { true, 0.346571, 19.125, 36.5388 },
    { true, 0.34447601, 11.5, 36.554 },
    { true, 0.34238201, 19.125, 36.5693 },
    { true, 0.340287, 11.5, 36.5848 },
    { true, 0.33819199, 19.125, 36.6005 },
    { true, 0.33609799, 11.5, 36.6164 },
    { true, 0.334003, 19.125, 36.6324 },
    { true, 0.33190799, 11.5, 36.6486 },
    { true, 0.32981399, 19.125, 36.665 },
    { true, 0.327719, 11.5, 36.6815 },
    { true, 0.32562399, 19.125, 36.6983 },
    { true, 0.32352999, 11.5, 36.7152 },
    { true, 0.321435, 19.125, 36.7323 },
    { true, 0.319341, 11.5, 36.7495 },
    { true, 0.31724599, 19.125, 36.767 },
    { true, 0.31515101, 11.5, 36.7846 },
    { true, 0.31305701, 19.125, 36.8024 },
    { true, 0.31096199, 11.5, 36.8204 },
    { true, 0.30886701, 19.125, 36.8385 },
    { true, 0.30677301, 11.5, 36.8569 },
    { true, 0.30467799, 19.125, 36.8754 },
    { true, 0.30258301, 11.5, 36.8941 },
    { true, 0.30048901, 19.125, 36.9129 },
    { true, 0.29839399, 11.5, 36.932 },
    { true, 0.29629999, 19.125, 36.9512 },
    { true, 0.29420501, 11.5, 36.9707 },
    { true, 0.29211, 19.125, 36.9903 },
    { true, 0.290016, 11.5, 37.0101 },
    { true, 0.28792101, 19.125, 37.03 },
    { true, 0.285826, 11.5, 37.0502 },
    { true, 0.283732, 19.125, 37.0705 },
    { true, 0.28163701, 11.5, 37.0911 },
    { true, 0.279542, 19.125, 37.1118 },
    { true, 0.277448, 11.5, 37.1327 },
    { true, 0.27535301, 19.125, 37.1538 },
    { true, 0.273258, 11.5, 37.175 },
    { true, 0.271164, 19.125, 37.1965 },
    { true, 0.26906899, 11.5, 37.2181 },
    { true, 0.26697499, 19.125, 37.24 },
    { true, 0.26488, 11.5, 37.262 },
    { true, 0.26278499, 19.125, 37.2842 },
    { true, 0.26069099, 11.5, 37.3066 },
    { true, 0.258596, 19.125, 37.3292 },
    { true, 0.25650099, 11.5, 37.352 },
    { true, 0.25440699, 19.125, 37.375 },
    { true, 0.252312, 11.5, 37.3982 },
    { true, 0.25021699, 19.125, 37.4215 },
    { true, 0.24812301, 11.5, 37.4451 },
    { true, 0.24602801, 19.125, 37.4688 },
    { true, 0.24393301, 11.5, 37.4928 },
    { true, 0.24183901, 19.125, 37.5169 },
    { true, 0.23974399, 11.5, 37.5413 },
    { true, 0.23765001, 19.125, 37.5658 },
    { true, 0.23555499, 11.5, 37.5905 },
    { true, 0.23345999, 19.125, 37.6155 },
    { true, 0.23136599, 11.5, 37.6406 },
    { true, 0.22927099, 19.125, 37.6659 },
    { true, 0.227176, 11.5, 37.6915 },
    { true, 0.225082, 19.125, 37.7172 },
    { true, 0.222987, 11.5, 37.7431 },
    { true, 0.220892, 19.125, 37.7692 },
    { true, 0.218798, 11.5, 37.7956 },
    { true, 0.216703, 19.125, 37.8221 },
    { true, 0.214609, 11.5, 37.8488 },
    { true, 0.212514, 19.125, 37.8758 },
    { true, 0.210419, 11.5, 37.9029 },
    { true, 0.208325, 19.125, 37.9303 },
    { true, 0.20623, 11.5, 37.9578 },
    { true, 0.204135, 19.125, 37.9856 },
    { true, 0.202041, 11.5, 38.0136 },
    { true, 0.199946, 19.125, 38.0418 },
    { true, 0.197851, 11.5, 38.0702 },
    { true, 0.195757, 19.125, 38.0988 },
    { true, 0.193662, 11.5, 38.1276 },
    { true, 0.191567, 19.125, 38.1566 },
    { true, 0.189473, 11.5, 38.1858 },
    { true, 0.187378, 19.125, 38.2153 },
    { true, 0.185284, 11.5, 38.2449 },
    { true, 0.183189, 19.125, 38.25 },
    { true, 0.18109401, 11.5, 38.25 },
    { true, 0.17900001, 19.125, 38.25 },
    { true, 0.17690501, 11.5, 38.25 },
    { true, 0.17481001, 19.125, 38.25 },
    { true, 0.17271601, 11.5, 38.25 },
    { true, 0.17062099, 19.125, 38.25 },
    { true, 0.16852599, 11.5, 38.25 },
    { true, 0.16643199, 19.125, 38.25 },
    { true, 0.16433699, 11.5, 38.25 },
    { true, 0.162242, 19.125, 38.25 },
    { true, 0.16014799, 11.5, 38.25 },
    { true, 0.158053, 19.125, 38.25 },
    { true, 0.155959, 11.5, 38.25 },
    { true, 0.153864, 19.125, 38.25 },
    { true, 0.151769, 11.5, 38.25 },
    { true, 0.149675, 19.125, 38.25 },
    { true, 0.14758, 11.5, 38.25 },
    { true, 0.145485, 19.125, 38.25 },
    { true, 0.143391, 11.5, 38.25 },
    { true, 0.141296, 19.125, 38.25 },
    { true, 0.139201, 11.5, 38.25 },
    { true, 0.137107, 19.125, 38.25 },
    { true, 0.135012, 11.5, 38.25 },
    { true, 0.132918, 19.125, 38.25 },
    { true, 0.130823, 11.5, 38.25 },
    { true, 0.128728, 19.125, 38.25 },
    { true, 0.126634, 11.5, 38.25 },
    { true, 0.124539, 19.125, 38.25 },
    { true, 0.122444, 11.5, 38.25 },
    { true, 0.12035, 19.125, 38.25 },
    { true, 0.118255, 11.5, 38.25 },
    { true, 0.11616, 19.125, 38.25 },
    { true, 0.114066, 11.5, 38.25 },
    { true, 0.111971, 19.125, 38.25 },
    { true, 0.109876, 11.5, 38.25 },
    { true, 0.107782, 19.125, 38.25 },
    { true, 0.105687, 11.5, 38.25 },
    { true, 0.103593, 19.125, 38.25 },
    { true, 0.101498, 11.8432, 38.25 },
    { true, 0.0994033, 19.125, 38.25 },
    { true, 0.0973086, 12.3514, 38.25 },
    { true, 0.095214, 19.125, 38.25 },
    { true, 0.0931193, 12.9053, 38.25 },
    { true, 0.0910247, 19.125, 38.25 },
    { true, 0.0889301, 13.5115, 38.25 },
    { true, 0.0868354, 19.125, 38.25 },
    { true, 0.0847408, 14.1778, 38.25 },
    { true, 0.0826461, 19.125, 38.25 },
    { true, 0.0805515, 14.9134, 38.25 },
    { true, 0.0784569, 19.125, 38.25 },
    { true, 0.0763622, 15.7299, 38.25 },
    { true, 0.0742676, 19.125, 38.25 },
    { true, 0.0721729, 16.6412, 38.25 },
    { true, 0.0700783, 19.125, 38.25 },
    { true, 0.0679837, 17.6649, 38.25 },
    { true, 0.065889, 19.125, 38.25 },
    { true, 0.0637944, 18.8232, 38.25 },
    { true, 0.0616997, 19.4614, 38.25 },
    { true, 0.0596051, 20.1444, 38.25 },
    { true, 0.0575104, 20.8773, 38.25 },
    { true, 0.0554158, 21.6656, 38.25 },
    { true, 0.0533212, 22.5158, 38.25 },
    { true, 0.0512265, 23.4356, 38.25 },
    { true, 0.0491319, 24.4339, 38.25 },
    { true, 0.0470372, 25.5211, 38.25 },
    { true, 0.0449426, 26.7097, 38.25 },
    { true, 0.042848, 28.0145, 38.25 },
    { true, 0.0407533, 29.4536, 38.25 },
    { true, 0.0386587, 31.0486, 38.25 },
    { true, 0.036564, 32.8265, 38.25 },
    { true, 0.0344694, 34.8204, 38.25 },
    { true, 0.0323748, 37.0723, 38.25 }
};

//  This initialized an idealized mapping.
Int_t StFgtGeom::mNaiveMapping[] =
{
    9,
    720,
    733,
    723,
    740,
    722,
    721,
    724,
    11,
    731,
    13,
    8,
    15,
    726,
    745,
    743,
    4,
    746,
    5,
    14,
    19,
    738,
    732,
    735,
    748,
    18,
    17,
    747,
    749,
    10,
    752,
    16,
    753,
    751,
    727,
    742,
    737,
    739,
    21,
    750,
    756,
    754,
    736,
    12,
    725,
    734,
    757,
    20,
    23,
    22,
    0,
    730,
    3,
    744,
    6,
    755,
    729,
    741,
    728,
    2,
    7,
    1,
    760,
    758,
    761,
    796,
    791,
    759,
    45,
    24,
    25,
    789,
    764,
    762,
    47,
    42,
    46,
    763,
    765,
    779,
    27,
    26,
    797,
    784,
    794,
    766,
    28,
    38,
    29,
    768,
    782,
    767,
    30,
    776,
    31,
    774,
    43,
    770,
    790,
    769,
    786,
    772,
    32,
    781,
    33,
    771,
    34,
    780,
    787,
    777,
    35,
    775,
    783,
    773,
    41,
    788,
    36,
    785,
    37,
    40,
    39,
    44,
    778,
    792,
    48,
    793,
    49,
    795,
    60,
    798,
    62,
    801,
    815,
    800,
    799,
    802,
    63,
    808,
    814,
    806,
    818,
    804,
    69,
    68,
    54,
    821,
    55,
    816,
    826,
    813,
    61,
    811,
    823,
    824,
    822,
    70,
    71,
    809,
    827,
    820,
    73,
    72,
    805,
    817,
    65,
    66,
    830,
    825,
    831,
    829,
    64,
    812,
    803,
    810,
    75,
    828,
    834,
    832,
    50,
    807,
    53,
    819,
    56,
    74,
    59,
    67,
    58,
    52,
    57,
    51,
    835,
    833,
    77,
    875,
    100,
    76,
    101,
    836,
    838,
    863,
    839,
    837,
    103,
    860,
    102,
    78,
    79,
    91,
    80,
    840,
    877,
    858,
    871,
    841,
    843,
    854,
    844,
    81,
    93,
    842,
    82,
    852,
    83,
    850,
    98,
    846,
    99,
    845,
    96,
    848,
    84,
    856,
    85,
    847,
    86,
    855,
    97,
    853,
    87,
    851,
    94,
    849,
    95,
    861,
    88,
    859,
    89,
    857,
    92,
    865,
    90,
    867,
    104,
    869,
    105,
    873,
    116,
    879,
    118,
    885,
    123,
    883,
    881,
    887,
    119,
    899,
    122,
    895,
    125,
    891,
    127,
    917,
    110,
    921,
    111,
    913,
    131,
    909,
    117,
    905,
    129,
    925,
    128,
    923,
    130,
    901,
    132,
    919,
    133,
    929,
    893,
    915,
    121,
    911,
    134,
    927,
    135,
    933,
    120,
    907,
    889,
    903,
    136,
    931,
    137,
    937,
    106,
    897,
    109,
    126,
    112,
    935,
    115,
    124,
    114,
    108,
    113,
    107,
    138,
    939,
    139,
    1003,
    164,
    941,
    165,
    943,
    140,
    991,
    141,
    945,
    167,
    987,
    166,
    947,
    142,
    155,
    143,
    949,
    1005,
    983,
    999,
    951,
    144,
    975,
    145,
    955,
    157,
    953,
    146,
    971,
    147,
    967,
    162,
    959,
    163,
    957,
    160,
    963,
    148,
    979,
    149,
    961,
    150,
    977,
    161,
    973,
    151,
    969,
    158,
    965,
    159,
    989,
    152,
    985,
    153,
    981,
    156,
    993,
    154,
    995,
    168,
    997,
    169,
    1001,
    180,
    1007,
    182,
    1013,
    1043,
    1011,
    1009,
    1015,
    1033,
    1027,
    1041,
    1023,
    1049,
    1019,
    188,
    187,
    174,
    1055,
    175,
    1045,
    191,
    1039,
    181,
    183,
    1059,
    1061,
    1057,
    189,
    190,
    1029,
    192,
    1053,
    193,
    1065,
    1021,
    1047,
    184,
    185,
    194,
    1063,
    195,
    1069,
    1035,
    1037,
    1017,
    1031,
    196,
    1067,
    197,
    1073,
    170,
    1025,
    173,
    1051,
    176,
    1071,
    179,
    186,
    178,
    172,
    177,
    171,
    198,
    1075,
    199,
    928,
    220,
    1077,
    221,
    1079,
    200,
    916,
    866,
    862,
    223,
    912,
    222,
    864,
    868,
    211,
    202,
    201,
    930,
    908,
    924,
    870,
    874,
    900,
    876,
    203,
    213,
    872,
    204,
    896,
    882,
    207,
    218,
    880,
    219,
    878,
    216,
    886,
    884,
    904,
    206,
    205,
    890,
    902,
    217,
    898,
    892,
    894,
    214,
    888,
    215,
    914,
    208,
    910,
    209,
    906,
    212,
    918,
    210,
    920,
    224,
    922,
    225,
    926,
    233,
    932,
    962,
    940,
    976,
    228,
    934,
    942,
    235,
    958,
    237,
    232,
    239,
    948,
    986,
    982,
    229,
    988,
    944,
    238,
    243,
    972,
    960,
    966,
    992,
    242,
    241,
    990,
    994,
    234,
    1000,
    240,
    1002,
    998,
    950,
    980,
    970,
    974,
    245,
    996,
    1008,
    1004,
    968,
    236,
    230,
    964,
    1010,
    244,
    247,
    246,
    226,
    956,
    938,
    984,
    946,
    1006,
    954,
    978,
    952,
    936,
    231,
    227,
    248,
    1012,
    249,
    1076,
    274,
    1014,
    275,
    1016,
    250,
    1064,
    251,
    1018,
    277,
    1060,
    276,
    1020,
    252,
    265,
    253,
    1022,
    1078,
    1056,
    1072,
    1024,
    254,
    1048,
    255,
    1028,
    267,
    1026,
    256,
    1044,
    257,
    1040,
    272,
    1032,
    273,
    1030,
    270,
    1036,
    258,
    1052,
    259,
    1034,
    260,
    1050,
    271,
    1046,
    261,
    1042,
    268,
    1038,
    269,
    1062,
    262,
    1058,
    263,
    1054,
    266,
    1066,
    264,
    1068,
    278,
    1070,
    279,
    1074,
    669,
    1080,
    667,
    1086,
    662,
    1084,
    1082,
    1088,
    666,
    1100,
    663,
    1096,
    660,
    1092,
    1122,
    1118,
    675,
    1124,
    674,
    1114,
    656,
    1110,
    668,
    1106,
    1128,
    657,
    658,
    1126,
    1130,
    1102,
    1136,
    659,
    1138,
    1134,
    1094,
    1116,
    664,
    1112,
    654,
    1132,
    1144,
    1140,
    665,
    1108,
    1090,
    1104,
    1146,
    655,
    652,
    653,
    679,
    1098,
    676,
    1120,
    673,
    1142,
    670,
    661,
    671,
    677,
    672,
    678,
    1152,
    1148,
    1154,
    1224,
    1214,
    1150,
    630,
    651,
    650,
    1210,
    1160,
    1156,
    628,
    633,
    629,
    1158,
    1162,
    1190,
    648,
    649,
    1226,
    1200,
    1220,
    1164,
    647,
    637,
    646,
    1168,
    1196,
    1166,
    645,
    1184,
    644,
    1180,
    632,
    1172,
    1212,
    1170,
    1204,
    1176,
    643,
    1194,
    642,
    1174,
    641,
    1192,
    1206,
    1186,
    640,
    1182,
    1198,
    1178,
    634,
    1208,
    639,
    1202,
    638,
    635,
    636,
    631,
    1188,
    1216,
    627,
    1218,
    626,
    1222,
    615,
    1228,
    613,
    1234,
    609,
    1232,
    1230,
    1236,
    612,
    1248,
    1260,
    1244,
    1268,
    1240,
    1274,
    1270,
    621,
    606,
    620,
    1264,
    1284,
    610,
    614,
    1254,
    605,
    1280,
    1276,
    1278,
    1282,
    1250,
    603,
    1272,
    1290,
    1286,
    1242,
    608,
    1258,
    1262,
    1292,
    604,
    601,
    602,
    611,
    1256,
    1238,
    1252,
    1298,
    1288,
    1300,
    1296,
    625,
    1246,
    622,
    607,
    619,
    1294,
    616,
    1266,
    617,
    623,
    618,
    624,
    599,
    600,
    1085,
    1149,
    576,
    1081,
    575,
    1083,
    1087,
    1137,
    597,
    598,
    573,
    1133,
    574,
    1089,
    1093,
    585,
    1095,
    1091,
    1151,
    1129,
    1145,
    596,
    595,
    1121,
    1101,
    1099,
    583,
    1097,
    1103,
    1117,
    593,
    1113,
    578,
    1105,
    577,
    594,
    580,
    1109,
    592,
    1125,
    591,
    1107,
    590,
    1123,
    579,
    1119,
    589,
    1115,
    582,
    1111,
    581,
    1135,
    588,
    1131,
    587,
    1127,
    584,
    1139,
    586,
    1141,
    572,
    1143,
    571,
    1147,
    560,
    1153,
    558,
    1159,
    553,
    1157,
    1155,
    1161,
    557,
    1173,
    554,
    1169,
    551,
    1165,
    549,
    1191,
    566,
    1195,
    565,
    1187,
    545,
    1183,
    559,
    1179,
    547,
    1199,
    548,
    1197,
    546,
    1175,
    544,
    1193,
    543,
    1203,
    1167,
    1189,
    555,
    1185,
    542,
    1201,
    541,
    1207,
    556,
    1181,
    1163,
    1177,
    540,
    1205,
    539,
    1211,
    570,
    1171,
    567,
    550,
    564,
    1209,
    561,
    552,
    562,
    568,
    563,
    569,
    538,
    1213,
    537,
    1277,
    512,
    1215,
    511,
    1217,
    536,
    1265,
    535,
    1219,
    509,
    1261,
    510,
    1221,
    534,
    521,
    533,
    1223,
    1279,
    1257,
    1273,
    1225,
    532,
    1249,
    531,
    1229,
    519,
    1227,
    530,
    1245,
    529,
    1241,
    514,
    1233,
    513,
    1231,
    516,
    1237,
    528,
    1253,
    527,
    1235,
    526,
    1251,
    515,
    1247,
    525,
    1243,
    518,
    1239,
    517,
    1263,
    524,
    1259,
    523,
    1255,
    520,
    1267,
    522,
    1269,
    508,
    1271,
    507,
    1275,
    496,
    1281,
    494,
    1287,
    490,
    1285,
    1283,
    1289,
    493,
    1301,
    1307,
    1297,
    1311,
    1293,
    1314,
    1312,
    502,
    487,
    501,
    1309,
    1319,
    491,
    495,
    1304,
    486,
    1317,
    1315,
    1316,
    1318,
    1302,
    484,
    1313,
    483,
    1320,
    1295,
    489,
    1306,
    1308,
    482,
    485,
    481,
    1322,
    492,
    1305,
    1291,
    1303,
    480,
    1321,
    479,
    1324,
    506,
    1299,
    503,
    488,
    500,
    1323,
    497,
    1310,
    498,
    504,
    499,
    505,
    478,
    1325,
    477,
    1361,
    456,
    1326,
    455,
    1327,
    476,
    1355,
    475,
    1328,
    453,
    1353,
    454,
    1329,
    474,
    465,
    1332,
    1330,
    1362,
    1351,
    1359,
    1331,
    1333,
    1347,
    472,
    1334,
    463,
    473,
    1336,
    467,
    1337,
    1342,
    458,
    471,
    457,
    1335,
    460,
    1339,
    470,
    1349,
    1340,
    1338,
    1341,
    1348,
    459,
    1346,
    468,
    1343,
    462,
    469,
    461,
    1354,
    1344,
    1352,
    1345,
    1350,
    464,
    1356,
    466,
    1357,
    452,
    1358,
    451,
    1360,
    442,
    1363,
    1377,
    1366,
    1384,
    1365,
    1364,
    1367,
    440,
    1375,
    438,
    443,
    436,
    1370,
    1389,
    1387,
    446,
    1390,
    1368,
    437,
    432,
    1382,
    1376,
    1379,
    1392,
    433,
    434,
    1391,
    1393,
    441,
    1396,
    435,
    1397,
    1395,
    1371,
    1386,
    1381,
    1383,
    430,
    1394,
    1400,
    1398,
    1380,
    439,
    445,
    1378,
    1401,
    431,
    428,
    429,
    450,
    1374,
    447,
    1388,
    1369,
    1399,
    1373,
    1385,
    1372,
    448,
    444,
    449,
    427,
    1402,
    426,
    1438,
    405,
    1403,
    404,
    1404,
    425,
    1432,
    424,
    1405,
    402,
    1430,
    403,
    1406,
    423,
    1423,
    422,
    1407,
    1439,
    1428,
    1436,
    1408,
    421,
    413,
    420,
    1410,
    1426,
    1409,
    419,
    1420,
    418,
    1417,
    407,
    1412,
    406,
    1411,
    409,
    417,
    1414,
    1425,
    1415,
    1413,
    416,
    1424,
    408,
    1421,
    1418,
    415,
    1427,
    1416,
    410,
    1431,
    1419,
    1429,
    414,
    411,
    412,
    1433,
    1422,
    1434,
    401,
    1435,
    400,
    1437
};

// This gives center of quadrant XYZ in STAR coordinate
// This does NOT get modified by alignment parameters.
void StFgtGeom::getQuadCenterXYZ(Short_t disc, Short_t quad, TVector3 &xyz){
  char l;
  short d,q;  
  double p,p1,p2,r,r1,r2;
  int gidp=encodeGeoId(disc,quad,'P',(short)(kFgtNumStrips/2));
  getGlobalPhysicalCoordinate(gidp,d,q,l,p,p1,p2);
  int gidr=encodeGeoId(disc,quad,'R',(short)(kFgtNumRstripsPerOctant/2));
  getGlobalPhysicalCoordinate(gidr,d,q,l,r,r1,r2);
  xyz.SetXYZ(r*cos(p),r*sin(p),getDiscZ(disc));
}

/*
 *  $Id: StFgtGeom.cxx,v 1.38 2015/05/21 19:46:19 akio Exp $
 *  $Log: StFgtGeom.cxx,v $
 *  Revision 1.38  2015/05/21 19:46:19  akio
 *  fixes to pass cppcheck
 *
 *  Revision 1.37  2013/01/31 15:44:27  akio
 *  Adding getQuadCenterXYZ
 *
 *  Revision 1.36  2012/11/05 15:43:40  akio
 *  FgtSlowSimu related fixes for r/phi consistency & speed up
 *
 *  Revision 1.35  2012/08/25 17:23:34  avossen
 *  made getQuad compatible with angles between 0 and 2*pi
 *
 *  Revision 1.34  2012/08/15 18:03:36  rfatemi
 *  Bug fix for getGlobalPhysicalCoordinate by pnord, computation done in computeGlobalPhysicalCoordinate
 *
 *  Revision 1.33  2012/05/11 19:45:18  rfatemi
 *  added getGlobalPhysicalCoordinate
 *
 *  Revision 1.32  2012/03/15 17:17:44  rfatemi
 *  more INFO->DEBUG
 *
 *  Revision 1.31  2012/03/15 15:42:46  rfatemi
 *  changed layer tests to "R" and "P" instead of "S" and "L"
 *
 *  Revision 1.30  2012/03/15 00:18:12  wwitzke
 *  Added boundary conditions to StFgtGeom.
 *
 *  Revision 1.29  2012/02/09 18:23:24  wwitzke
 *  Fixed various minor issues, including nesting the StFgtGeomData, making the
 *  various "pi" variables use the TMath definions of pi, and removing the various
 *  asserts from the code.
 *
 *  Revision 1.26  2012/02/09 16:49:02  wwitzke
 *  Fixed naming convention problems (camel casing).
 *
 *  Revision 1.25  2012/02/09 16:16:39  wwitzke
 *  Made minor fix to StFgtGeom.h to eliminate warnings.
 *
 *  Revision 1.24  2012/01/27 13:21:55  rfatemi
 *  Use only constants in StFgtConsts.h
 *
 *  Revision 1.23  2012/01/26 13:13:12  sgliske
 *  Updated to use StFgtConsts, which
 *  replaces StFgtEnums and StFgtGeomDefs
 *
 *  Revision 1.22  2011/11/09 00:50:04  rfatemi
 *  Fix width phi strips with r < rMid()
 *
 *  Revision 1.21  2011/11/08 22:40:06  rfatemi
 *  Fixed flat part in phi2LocalStripId
 *
 *  Revision 1.20  2011/11/06 22:34:21  rfatemi
 *  implement phi2LocalStripId
 *
 *  Revision 1.19  2011/11/05 02:25:14  rfatemi
 *  add phi2LocalStripId
 *
 *  Revision 1.18  2011/11/03 16:18:51  balewski
 *  remove printout
 *
 *  Revision 1.17  2011/10/13 21:02:15  balewski
 *  cleanup of not needed intermediate methods
 *
 *  Revision 1.16  2011/10/13 15:41:36  rfatemi
 *  change acceptance area for hits to Rlast + 1/2 pitch or Rfirst - 1/2 pitch
 *
 *  Revision 1.15  2011/10/11 17:52:22  rfatemi
 *  Put back mRadStripOff and mPhiStripOff
 *
 *  Revision 1.14  2011/10/09 16:24:21  rfatemi
 *  Update rad2LocalStripId
 *
 *  Revision 1.13  2011/10/09 13:37:10  rfatemi
 *  Update rad2LocalStripId
 *
 *  Revision 1.12  2011/10/07 03:42:54  rfatemi
 *  Update to rad2LocalStripId
 *
 *  Revision 1.11  2011/09/29 21:35:47  balewski
 *  more functions, fixing phi error for quadrant recognision
 *
 *  Revision 1.10  2011/09/29 18:52:03  wwitzke
 *  Changed the ordering of the items in the file.  The big arrays are now towards
 *  the bottom. This was to facilitate editing and maintenance.
 *
 *  Revision 1.9  2011/09/29 18:45:49  wwitzke
 *  Fixed bug with calculation of pi.  Weird.
 *
 *  Revision 1.8  2011/09/29 18:34:53  sgliske
 *  Fixed phiQuadXaxis, added asserts to getQuad,and added reverse lookup: elec. coord. from geoId
 *
 *
 */
