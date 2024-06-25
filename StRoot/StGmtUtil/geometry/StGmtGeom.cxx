/*  StGmtGeom.cxx
 *
 *  GMT geometry class declaration.
 *
 *  \authors K.S. Engle and Richard Witt (witt@usna.edu)
 *  based on StFgtGeom
 */

#include <TMath.h>
//#include <assert.h>
#include <iostream>
#include <algorithm>
#include "StGmtGeom.h"
#include "StMessMgr.h"

//ClassImp(StGmtGeom)

double	StGmtGeom::mPi = TMath::Pi();
double	StGmtGeom::mHalfPi = TMath::PiOver2();


Int_t StGmtGeom::encodeGeoId    //// WORKING
(
//     Int_t module, Char_t layer, Int_t strip
    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
)   
{
    Short_t module = getModuleIdFromElecCoord( rdo, arm, apv );
    // locally map apv number into [0,1]
    if ( apv <= 3 )
    {
      apv = apv % 2;
    }
    else
    {
      apv = (apv - 12) % 2;
    }
    
    Char_t testS='S';
    Char_t testP='P';

    if ( module < 0 || module >= kGmtNumModules )
    {
	LOG_DEBUG << "Module " << module << " out of range in StGmtGeom::encodeGeoId." << endm;
	return kGmtError;
    }
    else if ( apv > 1 || apv < 0 )
    {
	LOG_DEBUG << "APV " << apv << " out of range in StGmtGeom::encodeGeoId." << endm;
	return kGmtError;
    }
    else if ( channel < 0 || channel >= kGmtNumStrips )
    {
	LOG_DEBUG << "Channel " << channel << " out of range in StGmtGeom::encodeGeoId." << endm;
	return kGmtError;
    }

    return
        ( module * kGmtNumLayers + apv ) * kGmtNumStrips + channel; // from 0 to 2047 since channel is from 0-127;
}

Int_t StGmtGeom::decodeGeoId      //// WORKING
(
//     Int_t geoId, Short_t & module, Char_t & layer, Short_t & strip
    Int_t geoId, Short_t & module, Int_t & layer, Short_t & strip
)
{
    if ( geoId < 0 || geoId >= kGmtNumGeoIds )
    {
	LOG_DEBUG << "GeoId " << geoId << " out of range in StGmtGeom::decodeGeoId." << endm;
	module = kGmtError;
// 	layer = kGmtErrorChar;
	layer = kGmtError;
	strip = kGmtError;

	return kGmtError;
    }

    strip = geoId % kGmtNumStrips;
    geoId /= kGmtNumStrips;

//     layer = ( geoId % kGmtNumLayers ) ? 'P' : 'S';
    Int_t apv = ( geoId % kGmtNumLayers ) ? 1 : 0;
    geoId /= kGmtNumLayers;

    StGmtGeomData stripdata = mStrips[ strip + apv*kGmtNumStrips ];
    layer = stripdata.isY;

    module = geoId;

    return 0;
}

////////////  FIX ME!!!!!!!!!!!!!!!!!!!!
std::string StGmtGeom::encodeGeoName
(
    Int_t module, Char_t layer, Int_t strip
)
{
   Char_t testS='S';
   Char_t testP='P';

    if ( module < 0 || module >= kGmtNumModules )
    {
	LOG_DEBUG << "Module " << module << " out of range in StGmtGeom::encodeGeoName." << endm;
	return kGmtErrorString;
    }
    else if (
	     layer != testS
	     && layer != testP
    )
    {
	LOG_DEBUG << "Layer " << layer << " out of range in StGmtGeom::encodeGeoName." << endm;
	return kGmtErrorString;
    }
    else if ( strip < 0 || strip >= kGmtNumStrips )
    {
	LOG_DEBUG << "Strip " << strip << " out of range in StGmtGeom::encodeGeoName." << endm;
	return kGmtErrorString;
    }

 ////////////  FIX ME!!!!!!!!!!!!!!!!!!!!
   std::stringstream buff;
//     buff << disc+1 << (Char_t)(quadrant+'A') << layer;
    buff << module+1 << layer;

    if ( strip < 10 )
        buff << "00";
    else if ( strip < 100 )
        buff << "0";

    buff << strip;
    return buff.str();
}

 ////////////  FIX ME!!!!!!!!!!!!!!!!!!!!
Int_t StGmtGeom::decodeGeoName 
(
//     const std::string & geoName,
//     Short_t & module, Char_t & layer, Short_t & strip
    const std::string & geoName,
    Short_t & module, Int_t & layer, Short_t & strip
)
{
    Char_t testS='S';
    Char_t testP='P';

    //assert( geoName.size() == 6 );
//     disc = geoName[0] - '1';
//     quadrant = geoName[1] - 'A';
//     layer = geoName[2];
//     strip = std::atoi( (geoName.substr(3)).c_str() );
    module = geoName[0] - '1';
    layer = geoName[2];
    strip = std::atoi( (geoName.substr(3)).c_str() );

    //	This is unlikely to catch all errors with the geoName, but it should
    //	do fairly well.
    if (
	   module < 0
	|| module >= kGmtNumModules 
	|| (
// 	     layer != testS
// 	     && layer != testP
	     layer < 0 || layer > 1
	   )
	|| strip < 0
	|| strip > kGmtNumStrips
    )
    {
	LOG_DEBUG << "Malformed geoName " << geoName << " in StGmtGeom::decodeGeoName." << endm;
	module = kGmtError;
	layer = kGmtErrorChar;
	strip = kGmtError;

	return kGmtError;
    }

    return 0;
}

 ////////////  FIX ME!!!!!!!!!!!!!!!!!!!!
std::string StGmtGeom::translateGeoIdToGeoName( Int_t geoId )
{
    Short_t module, strip;
//     Char_t layer;
    Int_t layer;
    
    if ( geoId < 0 || geoId >= kGmtNumGeoIds )
    {
	LOG_DEBUG << "GeoId " << geoId << " out of range in StGmtGeom::translateGeoIdToGeoName." << endm;
	return kGmtErrorString;
    }

    decodeGeoId( geoId, module, layer, strip );
    return encodeGeoName( module, layer, strip );
}

 ////////////  FIX ME!!!!!!!!!!!!!!!!!!!!
Int_t StGmtGeom::translateGeoNameToGeoId( const std::string & geoName )
{
    Short_t module, strip;
//     Char_t layer;
    Int_t layer;
    Int_t rdo, arm, apv, channel;

    //	Error message already taken care of in decodeGeoName.
    if ( decodeGeoName( geoName, module, layer, strip ) < 0 )
	return kGmtError;

//     return encodeGeoId( module, layer, strip );
    return encodeGeoId( rdo, arm, apv, channel );
}

Int_t StGmtGeom::getPhysicalCoordinate  //// WORKING
(
    Int_t geoId,
//     Short_t & module, Char_t & layer
    Short_t & module, Int_t & layer
)
{
    if ( geoId < 0 || geoId >= kGmtNumGeoIds )
    {
	LOG_DEBUG << "GeoId " << geoId << " out of range in StGmtGeom::getPhysicalCoordinate." << endm;
	module = kGmtError;
	layer = kGmtErrorChar;

	return kGmtError;
    }

    Short_t strip;

    decodeGeoId( geoId, module, layer, strip );

    return 0;
}

 ////////////  FIX ME!!!!!!!!!!!!!!!!!!!!
Int_t StGmtGeom::getPhysicalCoordinate
(
//     const std::string & geoName,
//     Short_t & module, Char_t & layer
    const std::string & geoName,
    Short_t & module, Int_t & layer
)
{
    Short_t strip;

    if ( decodeGeoName( geoName, module, layer, strip ) < 0 )
    {
	//  Error is mostly handled by the decodeGeoName call.
	module = kGmtError;
// 	layer = kGmtErrorChar;
	layer = kGmtError;

	return kGmtError;
    }

    return 0;
}

Short_t StGmtGeom::getModuleIdFromElecCoord
(
    Int_t rdo, Int_t arm, Int_t apv
)
{
    if ( (rdo - 1) < 0 || (rdo - 1) >= kGmtNumRdos )
    {
	LOG_DEBUG << "RDO " << rdo << " out of range in StGmtGeom::getModuleIdFromElecCoord." << endm;
	return kGmtError;
    }
    else if ( arm < 0 || arm >= kGmtNumArms )
    {
	LOG_DEBUG << "ARM " << arm << " out of range in StGmtGeom::getModuleIdFromElecCoord." << endm;
	return kGmtError;
    }
    else if ( apv < 0 || apv > kGmtMaxApvId || (apv > 3 && apv < 12)  )
    {
	LOG_DEBUG << "APV " << apv << " out of range in StGmtGeom::getModuleIdFromElecCoord." << endm;
	return kGmtError;
    }

    if ( arm == 0 )
    {
      if( apv == 0 || apv == 1 )
	return 0;
      else if( apv == 2 || apv == 3 )
	return 1;
      else if( apv == 12 || apv == 13 )
	return 2;
      else if( apv == 14 || apv == 15 )
	return 3;
      else
      {
	LOG_DEBUG << "Invalid electronics coordinates in StGmtGeom::getModuleIdFromElecCoord." << endm;
	return kGmtError;
      }
    }
    else
    {
      if( apv == 0 || apv == 1 )
	return 4;
      else if( apv == 2 || apv == 3 )
	return 5;
      else if( apv == 12 || apv == 13 )
	return 6;
      else if( apv == 14 || apv == 15 )
	return 7;
      else
      {
	LOG_DEBUG << "Invalid electronics coordinates in StGmtGeom::getModuleIdFromElecCoord." << endm;
	return kGmtError;
      }
    }
}

Int_t StGmtGeom::getCoordNumFromElecCoord
(
    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
)
{
    if ( (rdo - 1) < 0 || (rdo - 1) >= kGmtNumRdos )
    {
	LOG_DEBUG << "RDO " << rdo << " out of range in StGmtGeom::getCoordNumFromElecCoord." << endm;
	return kGmtError;
    }
    else if ( arm < 0 || arm >= kGmtNumArms )
    {
	LOG_DEBUG << "ARM " << arm << " out of range in StGmtGeom::getCoordNumFromElecCoord." << endm;
	return kGmtError;
    }
    else if ( apv < 0 || apv > kGmtMaxApvId || (apv > 3 && apv < 12)  )
    {
	LOG_DEBUG << "APV " << apv << " out of range in StGmtGeom::getCoordNumFromElecCoord." << endm;
	return kGmtError;
    }
    else if ( channel < 0 || channel >= kGmtNumChannels )
    {
	LOG_DEBUG << "Channel " << channel << " out of range in StGmtGeom::getCoordNumFromElecCoord." << endm;
	return kGmtError;
    }

//     // locally map apv number into [0,1]
//     if ( apv == 3 )
//     {
//       apv = apv/2;
//     }
//     else
//     {
//       apv = (apv - 12)/2;
//     }
      

    StGmtGeomData stripdata = mStrips[ channel + (apv % 2) * kGmtNumStrips ];
    
    return stripdata.coordinate;
    
}

Double_t StGmtGeom::getPositionFromElecCoord//here
(
 Int_t rdo, Int_t arm, Int_t apv, Int_t channel
 )
{
  Int_t apvi = apv;
  if ( (rdo - 1) < 0 || (rdo - 1) >= kGmtNumRdos )
    {
      LOG_DEBUG << "RDO " << rdo << " out of range in StGmtGeom::getCoordNumFromElecCoord." << endm;
      return kGmtError;
    }
  else if ( arm < 0 || arm >= kGmtNumArms )
    {
      LOG_DEBUG << "ARM " << arm << " out of range in StGmtGeom::getCoordNumFromElecCoord." << endm;
      return kGmtError;
    }
  else if ( apv < 0 || apv > kGmtMaxApvId || (apv > 3 && apv < 12)  )
    {
      LOG_DEBUG << "APV " << apv << " out of range in StGmtGeom::getCoordNumFromElecCoord." << endm;
      return kGmtError;
    }
  else if ( channel < 0 || channel >= kGmtNumChannels )
    {
      LOG_DEBUG << "Channel " << channel << " out of range in StGmtGeom::getCoordNumFromElecCoord." << endm;
      return kGmtError;
    }

  // locally map apv number into [0,1]
//   if ( apv <= 3 )
//     {
//       apv = apv/2;
//     }
//   else
//     {
//       apv = (apv - 12)/2;
//     }

  apv = apv%2;
  
  StGmtGeomData stripdata = mStrips[ channel + apv*kGmtNumStrips ];//0-257
  //LOG_INFO << "rdo=" << rdo << "\tarm="<< arm << "\tapv="<< apvi <<"\tchannel = "<< channel << endm;
  //LOG_INFO << "apv2= " << apv << "\tstr=" << kGmtNumStrips << "\t[]=" <<  channel + apv*kGmtNumStrips << "\t===> " <<stripdata.location << endm;
  return stripdata.location;//0-10, 0-127: 0 to 5.2; 128-256 5.2 to 10
  
}

////////////  FIX ME!!!!!!!!!!!!!!!!!!!!
//  Calculates coordinates of strip in global coordinate system
//  Units are in cm and radians, depending on the layer.
Int_t StGmtGeom::computeGlobalPhysicalCoordinate
(
//     Char_t & layer,
    Int_t & layer,
    Short_t & strip
)
{
    switch (layer) {
// 	case ('P') :
	case (1) :
//             ordinate =
//                 mStrips[kGmtNumStrips + strip].ordinate + StGmtGeom::phiQuadXaxis(quadrant);
//             lowerSpan =
//                 mStrips[kGmtNumStrips + strip].lowerSpan;
//             upperSpan =
//                 mStrips[kGmtNumStrips + strip].upperSpan;
            break;
// 	case ('S') :
	case (0) :
//             ordinate =
//                 mStrips[strip].ordinate;
//             lowerSpan =
//                 mStrips[strip].lowerSpan + StGmtGeom::phiQuadXaxis(quadrant);
//             upperSpan =
//                 mStrips[strip].upperSpan + StGmtGeom::phiQuadXaxis(quadrant);
            break;
        default:
	    return kGmtError;
	    break;
    }

    return 0;
}

 ////////////  FIX ME!!!!!!!!!!!!!!!!!!!!
Int_t StGmtGeom::getGlobalPhysicalCoordinate
(
//     Int_t geoId,
//     Short_t & module, Char_t & layer
    Int_t geoId, Short_t & module, Int_t & layer
)
{
  Short_t strip;
 
  decodeGeoId( geoId, module, layer, strip );


  if ( geoId < 0 || geoId >= kGmtNumGeoIds )
    {
	LOG_DEBUG << "GeoId " << geoId << " out of range in StGmtGeom::getPhysicalCoordinate." << endm;
	module = kGmtError;
// 	layer = kGmtErrorChar;
	layer = kGmtError;

	return kGmtError;
    }

    return computeGlobalPhysicalCoordinate( layer, strip);
}

 ////////////  FIX ME!!!!!!!!!!!!!!!!!!!!
//  The ordinate, lowerSpan and upperSpan are all in centimeters or
//  radians, depending on the layer.
Int_t StGmtGeom::getGlobalPhysicalCoordinate
(
    const std::string & geoName,
//     Short_t & module, Char_t & layer
    Short_t & module, Int_t & layer
)
{
    Short_t strip;

    if ( decodeGeoName( geoName, module, layer, strip ) < 0 )
    {
	//  Error is mostly handled by the decodeGeoName call.
	module = kGmtError;
// 	layer = kGmtErrorChar;
	layer = kGmtError;

	return kGmtError;
    }

    return computeGlobalPhysicalCoordinate( layer, strip);

}




//  Please note that the following functions do NOT access the STAR
//  database to find mapping information. They assume the most
//  straight-forward mapping scheme and use that.
//  For those functions that have them, currently rdo can only be 1, arm
//  can be 0-1, apv can be 0-23 (although 4-11 are not
//  technically valid) and channel is 0-127.
Int_t StGmtGeom::encodeElectronicId     //// WORKING
(
    Int_t rdo, Int_t arm, Int_t apv, Int_t channel
)
{
    if ( (rdo - 1) < 0 || (rdo - 1) >= kGmtNumRdos )
    {
	LOG_DEBUG << "RDO " << rdo << " out of range in StGmtGeom::encodeElectronicId." << endm;
	return kGmtError;
    }
    else if ( arm < 0 || arm >= kGmtNumArms )
    {
	LOG_DEBUG << "ARM " << arm << " out of range in StGmtGeom::encodeElectronicId." << endm;
	return kGmtError;
    }
    else if ( apv < 0 || apv > kGmtMaxApvId || (apv > 3 && apv < 12)  )
    {
	LOG_DEBUG << "APV " << apv << " out of range in StGmtGeom::encodeElectronicId." << endm;
	return kGmtError;
    }
    else if ( channel < 0 || channel >= kGmtNumChannels )
    {
	LOG_DEBUG << "Channel " << channel << " out of range in StGmtGeom::encodeElectronicId." << endm;
	return kGmtError;
    }

    return channel + kGmtNumStrips*(apv + 4*arm);
}

Int_t StGmtGeom::decodeElectronicId     //// WORKING
(
    Int_t elecId,
    Int_t &rdo, Int_t &arm, Int_t &apv, Int_t &channel
)
{
    if ( elecId < 0 || elecId >= kGmtNumElecIds )
    {
	LOG_DEBUG << "Electronic ID " << elecId << " out of range in StGmtGeom::decodeElectronicId." << endm;

	rdo = kGmtError;
	arm = kGmtError;
	apv = kGmtError;
	channel = kGmtError;

	return kGmtError;
    }

    channel = elecId % 128;
    elecId /= 128;

    apv = elecId % 4;
    elecId /= 4;

    arm = elecId;
    rdo = 1;

    return 0;
}



// Whether the reverse map is valid
Bool_t StGmtGeom::mReverseNaiveMappingValid = 0;

// The reverse map data member
Int_t StGmtGeom::mReverseNaiveMapping[ kGmtNumStripsPerModule ];


//  Initialize our physical coordinate database here. These are:
//  APV,Chan,Strip(0) or Pad(1),coordinate #,Location (cm),Signal *,Readout Order
//  The index corresponds to int(apv/2)+channel (assuming that the apv is in
//  [0,12).  If apv is in [12,24), then the index is int((apv-12)/2)+channel.
StGmtGeom::StGmtGeomData StGmtGeom::mStrips[] =
{
    {0,0,0,54,4.28,"S54",0},
    {0,1,0,37,2.92,"S37",1},
    {0,2,0,31,2.44,"S31",2},
    {0,3,0,8,0.6,"S8",3},
    {0,4,0,51,4.04,"S51",4},
    {0,5,0,35,2.76,"S35",5},
    {0,6,0,29,2.28,"S29",6},
    {0,7,0,21,1.64,"S21",7},
    {0,8,0,60,4.76,"S60",8},
    {0,9,0,33,2.6,"S33",9},
    {0,10,0,27,2.12,"S27",10},
    {0,11,0,12,0.92,"S12",11},
    {0,12,0,41,3.24,"S41",12},
    {0,13,0,55,4.36,"S55",13},
    {0,14,0,25,1.96,"S25",14},
    {0,15,0,14,1.08,"S14",15},
    {0,16,1,999,-999,"NC",16},     //  Not connected
    {0,17,1,39,3.08,"P39",17},
    {0,18,1,2,0.12,"P2",18},
    {0,19,1,24,1.88,"P24",19},
    {0,20,1,54,4.28,"P54",20},
    {0,21,1,37,2.92,"P37",21},
    {0,22,1,31,2.44,"P31",22},
    {0,23,1,23,1.8,"P23",23},
    {0,24,1,43,3.4,"P43",24},
    {0,25,1,35,2.76,"P35",25},
    {0,26,1,29,2.28,"P29",26},
    {0,27,1,21,1.64,"P21",27},
    {0,28,1,41,3.24,"P41",28},
    {0,29,0,46,3.64,"S46",29},
    {0,30,1,26,2.04,"P26",30},
    {0,31,1,7,0.52,"P7",31},
    {0,32,0,52,4.12,"S52",32},
    {0,33,1,57,4.52,"P57",33},
    {0,34,0,6,0.44,"S6",34},
    {0,35,0,7,0.52,"S7",35},
    {0,36,0,48,3.8,"S48",36},
    {0,37,0,50,3.96,"S50",37},
    {0,38,0,3,0.2,"S3",38},
    {0,39,0,20,1.56,"S20",39},
    {0,40,0,59,4.68,"S59",40},
    {0,41,1,999,-999,"NC",41},     //  Not connected
    {0,42,1,1,0.04,"P1",42},
    {0,43,0,11,0.84,"S11",43},
    {0,44,0,42,3.32,"S42",44},
    {0,45,0,56,4.44,"S56",45},
    {0,46,0,13,1,"S13",46},
    {0,47,0,16,1.24,"S16",47},
    {0,48,1,61,4.84,"P61",48},
    {0,49,1,46,3.64,"P46",49},
    {0,50,1,33,2.6,"P33",50},
    {0,51,1,25,1.96,"P25",51},
    {0,52,1,56,4.44,"P56",52},
    {0,53,1,50,3.96,"P50",53},
    {0,54,1,10,0.76,"P10",54},
    {0,55,1,15,1.16,"P15",55},
    {0,56,1,47,3.72,"P47",56},
    {0,57,1,55,4.36,"P55",57},
    {0,58,1,12,0.92,"P12",58},
    {0,59,1,9,0.68,"P9",59},
    {0,60,1,42,3.32,"P42",60},
    {0,61,0,62,4.92,"S62",61},
    {0,62,1,27,2.12,"P27",62},
    {0,63,1,6,0.44,"P6",63},
    {0,64,0,47,3.72,"S47",64},
    {0,65,0,49,3.88,"S49",65},
    {0,66,0,5,0.36,"S5",66},
    {0,67,0,10,0.76,"S10",67},
    {0,68,0,45,3.56,"S45",68},
    {0,69,1,59,4.68,"P59",69},
    {0,70,0,4,0.28,"S4",70},
    {0,71,0,9,0.68,"S9",71},
    {0,72,0,39,3.08,"S39",72},
    {0,73,0,61,4.84,"S61",73},
    {0,74,1,4,0.28,"P4",74},
    {0,75,0,18,1.4,"S18",75},
    {0,76,0,40,3.16,"S40",76},
    {0,77,0,57,4.52,"S57",77},
    {0,78,0,24,1.88,"S24",78},
    {0,79,0,2,0.12,"S2",79},
    {0,80,1,62,4.92,"P62",80},
    {0,81,1,48,3.8,"P48",81},
    {0,82,1,32,2.52,"P32",82},
    {0,83,1,22,1.72,"P22",83},
    {0,84,1,58,4.6,"P58",84},
    {0,85,1,52,4.12,"P52",85},
    {0,86,1,30,2.36,"P30",86},
    {0,87,1,17,1.32,"P17",87},
    {0,88,1,49,3.88,"P49",88},
    {0,89,0,44,3.48,"S44",89},
    {0,90,1,28,2.2,"P28",90},
    {0,91,1,11,0.84,"P11",91},
    {0,92,1,53,4.2,"P53",92},
    {0,93,0,63,5,"S63",93},
    {0,94,1,18,1.4,"P18",94},
    {0,95,1,5,0.36,"P5",95},
    {0,96,1,63,5,"P63",96},
    {0,97,0,36,2.84,"S36",97},
    {0,98,0,30,2.36,"S30",98},
    {0,99,0,22,1.72,"S22",99},
    {0,100,0,43,3.4,"S43",100},
    {0,101,0,34,2.68,"S34",101},
    {0,102,0,28,2.2,"S28",102},
    {0,103,0,19,1.48,"S19",103},
    {0,104,0,53,4.2,"S53",104},
    {0,105,0,58,4.6,"S58",105},
    {0,106,0,26,2.04,"S26",106},
    {0,107,0,17,1.32,"S17",107},
    {0,108,0,38,3,"S38",108},
    {0,109,0,32,2.52,"S32",109},
    {0,110,0,23,1.8,"S23",110},
    {0,111,0,1,0.04,"S1",111},
    {0,112,1,60,4.76,"P60",112},
    {0,113,1,40,3.16,"P40",113},
    {0,114,1,8,0.6,"P8",114},
    {0,115,1,14,1.08,"P14",115},
    {0,116,1,45,3.56,"P45",116},
    {0,117,1,38,3,"P38",117},
    {0,118,0,15,1.16,"S15",118},
    {0,119,1,19,1.48,"P19",119},
    {0,120,1,51,4.04,"P51",120},
    {0,121,1,36,2.84,"P36",121},
    {0,122,1,16,1.24,"P16",122},
    {0,123,1,13,1,"P13",123},
    {0,124,1,44,3.48,"P44",124},
    {0,125,1,34,2.68,"P34",125},
    {0,126,1,20,1.56,"P20",126},
    {0,127,1,3,0.2,"P3",127},
    {1,0,0,117,9.32,"S117",128},
    {1,1,0,100,7.96,"S100",129},
    {1,2,0,94,7.48,"S94",130},
    {1,3,0,71,5.64,"S71",131},
    {1,4,0,114,9.08,"S114",132},
    {1,5,0,98,7.8,"S98",133},
    {1,6,0,92,7.32,"S92",134},
    {1,7,0,84,6.68,"S84",135},
    {1,8,0,123,9.8,"S123",136},
    {1,9,0,96,7.64,"S96",137},
    {1,10,0,90,7.16,"S90",138},
    {1,11,0,75,5.96,"S75",139},
    {1,12,0,104,8.28,"S104",140},
    {1,13,0,118,9.4,"S118",141},
    {1,14,0,88,7,"S88",142},
    {1,15,0,77,6.12,"S77",143},
    {1,16,1,999,-999,"NC",144},     //  Not connected
    {1,17,1,102,8.12,"P102",145},
    {1,18,1,65,5.16,"P65",146},
    {1,19,1,87,6.92,"P87",147},
    {1,20,1,117,9.32,"P117",148},
    {1,21,1,100,7.96,"P100",149},
    {1,22,1,94,7.48,"P94",150},
    {1,23,1,86,6.84,"P86",151},
    {1,24,1,106,8.44,"P106",152},
    {1,25,1,98,7.8,"P98",153},
    {1,26,1,92,7.32,"P92",154},
    {1,27,1,84,6.68,"P84",155},
    {1,28,1,104,8.28,"P104",156},
    {1,29,0,109,8.68,"S109",157},
    {1,30,1,89,7.08,"P89",158},
    {1,31,1,70,5.56,"P70",159},
    {1,32,0,115,9.16,"S115",160},
    {1,33,1,120,9.56,"P120",161},
    {1,34,0,69,5.48,"S69",162},
    {1,35,0,70,5.56,"S70",163},
    {1,36,0,111,8.84,"S111",164},
    {1,37,0,113,9,"S113",165},
    {1,38,0,66,5.24,"S66",166},
    {1,39,0,83,6.6,"S83",167},
    {1,40,0,122,9.72,"S122",168},
    {1,41,1,999,-999,"NC",169},     //  Not connected
    {1,42,1,64,5.08,"P64",170},
    {1,43,0,74,5.88,"S74",171},
    {1,44,0,105,8.36,"S105",172},
    {1,45,0,119,9.48,"S119",173},
    {1,46,0,76,6.04,"S76",174},
    {1,47,0,79,6.28,"S79",175},
    {1,48,1,124,9.88,"P124",176},
    {1,49,1,109,8.68,"P109",177},
    {1,50,1,96,7.64,"P96",178},
    {1,51,1,88,7,"P88",179},
    {1,52,1,119,9.48,"P119",180},
    {1,53,1,113,9,"P113",181},
    {1,54,1,73,5.8,"P73",182},
    {1,55,1,78,6.2,"P78",183},
    {1,56,1,110,8.76,"P110",184},
    {1,57,1,118,9.4,"P118",185},
    {1,58,1,75,5.96,"P75",186},
    {1,59,1,72,5.72,"P72",187},
    {1,60,1,105,8.36,"P105",188},
    {1,61,0,125,9.96,"S125",189},
    {1,62,1,90,7.16,"P90",190},
    {1,63,1,69,5.48,"P69",191},
    {1,64,0,110,8.76,"S110",192},
    {1,65,0,112,8.92,"S112",193},
    {1,66,0,68,5.4,"S68",194},
    {1,67,0,73,5.8,"S73",195},
    {1,68,0,108,8.6,"S108",196},
    {1,69,1,122,9.72,"P122",197},
    {1,70,0,67,5.32,"S67",198},
    {1,71,0,72,5.72,"S72",199},
    {1,72,0,102,8.12,"S102",200},
    {1,73,0,124,9.88,"S124",201},
    {1,74,1,67,5.32,"P67",202},
    {1,75,0,81,6.44,"S81",203},
    {1,76,0,103,8.2,"S103",204},
    {1,77,0,120,9.56,"S120",205},
    {1,78,0,87,6.92,"S87",206},
    {1,79,0,65,5.16,"S65",207},
    {1,80,1,125,9.96,"P125",208},
    {1,81,1,111,8.84,"P111",209},
    {1,82,1,95,7.56,"P95",210},
    {1,83,1,85,6.76,"P85",211},
    {1,84,1,121,9.64,"P121",212},
    {1,85,1,115,9.16,"P115",213},
    {1,86,1,93,7.4,"P93",214},
    {1,87,1,80,6.36,"P80",215},
    {1,88,1,112,8.92,"P112",216},
    {1,89,0,107,8.52,"S107",217},
    {1,90,1,91,7.24,"P91",218},
    {1,91,1,74,5.88,"P74",219},
    {1,92,1,116,9.24,"P116",220},
    {1,93,0,126,10.04,"S126",221},
    {1,94,1,81,6.44,"P81",222},
    {1,95,1,68,5.4,"P68",223},
    {1,96,1,999,-999,"NC",224},     //  Not connected
    {1,97,0,99,7.88,"S99",225},
    {1,98,0,93,7.4,"S93",226},
    {1,99,0,85,6.76,"S85",227},
    {1,100,0,106,8.44,"S106",228},
    {1,101,0,97,7.72,"S97",229},
    {1,102,0,91,7.24,"S91",230},
    {1,103,0,82,6.52,"S82",231},
    {1,104,0,116,9.24,"S116",232},
    {1,105,0,121,9.64,"S121",233},
    {1,106,0,89,7.08,"S89",234},
    {1,107,0,80,6.36,"S80",235},
    {1,108,0,101,8.04,"S101",236},
    {1,109,0,95,7.56,"S95",237},
    {1,110,0,86,6.84,"S86",238},
    {1,111,0,64,5.08,"S64",239},
    {1,112,1,123,9.8,"P123",240},
    {1,113,1,103,8.2,"P103",241},
    {1,114,1,71,5.64,"P71",242},
    {1,115,1,77,6.12,"P77",243},
    {1,116,1,108,8.6,"P108",244},
    {1,117,1,101,8.04,"P101",245},
    {1,118,0,78,6.2,"S78",246},
    {1,119,1,82,6.52,"P82",247},
    {1,120,1,114,9.08,"P114",248},
    {1,121,1,99,7.88,"P99",249},
    {1,122,1,79,6.28,"P79",250},
    {1,123,1,76,6.04,"P76",251},
    {1,124,1,107,8.52,"P107",252},
    {1,125,1,97,7.72,"P97",253},
    {1,126,1,83,6.6,"P83",254},
    {1,127,1,66,5.24,"P66",255}  
};

// //  This initialized an idealized mapping.  The index of the array is the coordinate number (X coordinates are first then Y).
// //  The value in the array is the readout number of the coordinate (i.e. the index into mStrips[] above).
Int_t StGmtGeom::mNaiveMapping[] =
{
	111,
	79,
	38,
	70,
	66,
	34,
	35,
	3,
	71,
	67,
	43,
	11,
	46,
	15,
	118,
	47,
	107,
	75,
	103,
	39,
	7,
	99,
	110,
	78,
	14,
	106,
	10,
	102,
	6,
	98,
	2,
	109,
	9,
	101,
	5,
	97,
	1,
	108,
	72,
	76,
	12,
	44,
	100,
	89,
	68,
	29,
	64,
	36,
	65,
	37,
	4,
	32,
	104,
	0,
	13,
	45,
	77,
	105,
	40,
	8,
	73,
	61,
	93,
	239,
	207,
	166,
	198,
	194,
	162,
	163,
	131,
	199,
	195,
	171,
	139,
	174,
	143,
	246,
	175,
	235,
	203,
	231,
	167,
	135,
	227,
	238,
	206,
	142,
	234,
	138,
	230,
	134,
	226,
	130,
	237,
	137,
	229,
	133,
	225,
	129,
	236,
	200,
	204,
	140,
	172,
	228,
	217,
	196,
	157,
	192,
	164,
	193,
	165,
	132,
	160,
	232,
	128,
	141,
	173,
	205,
	233,
	168,
	136,
	201,
	189,
	221,
	42,
	18,
	127,
	74,
	95,
	63,
	31,
	114,
	59,
	54,
	91,
	58,
	123,
	115,
	55,
	122,
	87,
	94,
	119,
	126,
	27,
	83,
	23,
	19,
	51,
	30,
	62,
	90,
	26,
	86,
	22,
	82,
	50,
	125,
	25,
	121,
	21,
	117,
	17,
	113,
	28,
	60,
	24,
	124,
	116,
	49,
	56,
	81,
	88,
	53,
	120,
	85,
	92,
	20,
	57,
	52,
	33,
	84,
	69,
	112,
	48,
	80,
	96,
	170,
	146,
	255,
	202,
	223,
	191,
	159,
	242,
	187,
	182,
	219,
	186,
	251,
	243,
	183,
	250,
	215,
	222,
	247,
	254,
	155,
	211,
	151,
	147,
	179,
	158,
	190,
	218,
	154,
	214,
	150,
	210,
	178,
	253,
	153,
	249,
	149,
	245,
	145,
	241,
	156,
	188,
	152,
	252,
	244,
	177,
	184,
	209,
	216,
	181,
	248,
	213,
	220,
	148,
	185,
	180,
	161,
	212,
	197,
	240,
	176,
	208,
	16,
	41,
	144,
	169,
	224
};

// Module locations at the corner of the GEM,
// per email from W.J. Llope to stargmt-l@lists.bnl.gov
// on 2012/10/31

Double_t StGmtGeom::getModuleZ(int iModule) {
  switch (iModule) {
   case 4:
   case 0 : return 77.768 * 2.54; // inches => cm
   case 5:
   case 1 : return 2.729 * 2.54; // inches => cm
   case 7:
   case 3 : return -2.729 * 2.54; // inches => cm
   case 6:
   case 2 : return -77.768 * 2.54; // inches => cm
   default : return -999;
 }
}

Double_t StGmtGeom::getModulePhi(int iModule) {
  double R = 85.606 * 2.54; // inches => cm
  double deltaphi = 5./R; // crude radian conversion
  switch (iModule) {
   case 0:
   case 1 : return TMath::Pi()*(10./6.)-deltaphi;
   case 2:
   case 3 : return TMath::Pi()*(10./6.)+deltaphi;
   case 4:
   case 5 : return TMath::Pi()*(1./6.)-deltaphi;
   case 6:
   case 7 : return TMath::Pi()*(1./6.)+deltaphi;
   default : return 0;
 }
}

