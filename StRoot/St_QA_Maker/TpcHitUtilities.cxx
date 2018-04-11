///////////////////////////////////////////////////////////////////////////
// $Id: TpcHitUtilities.cxx,v 1.10 2018/04/11 02:43:22 smirnovd Exp $
//
// Author: M.L. Miller, Yale
//
///////////////////////////////////////////////////////////////////////////
//
// Description: TPC sector gains hit utilities class
//
///////////////////////////////////////////////////////////////////////////
//
// $Log: TpcHitUtilities.cxx,v $
// Revision 1.10  2018/04/11 02:43:22  smirnovd
// Enable TPC/iTPC switch via St_tpcPadConfig
//
// This is accomplished by substituting St_tpcPadPlanes with St_tpcPadConfig.
// A sector ID is passed to St_tpcPadConfig in order to extract parameters for
// either TPC or iTPC
//
// Revision 1.9  2011/01/18 14:40:32  fisyak
// Clean up TpcDb interfaces and Tpc coordinate transformation
//
// Revision 1.8  2006/05/22 18:27:34  genevb
// Remove patch to observe fast offline issues
//
// Revision 1.7  2006/05/20 03:17:21  genevb
// Changed MapKey to MapQAKey to make it unique for QA
//
// Revision 1.6  2006/05/18 03:27:41  genevb
// Patch to observe fast offline issues
//
// Revision 1.5  2003/09/19 21:23:37  genevb
// Extraneous semicolon
//
// Revision 1.4  2002/02/01 23:15:27  genevb
// Include float.h
//
// Revision 1.3  2001/04/25 18:14:12  perev
// HPcorrs
//
// Revision 1.2  2000/08/09 18:57:44  lansdell
// improvements in TPC gains code reduces CPU time per event by factor of 2
//
//
///////////////////////////////////////////////////////////////////////////
#include "float.h"
#include "TpcMapUtilities.h"
#include "TpcHitUtilities.h"
#include "StEventTypes.h" // StEvent
#include "StThreeVectorD.hh"
#include <algorithm> // STL
// StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StTpcPadCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StTpcDb/StTpcDb.h"

#ifndef ST_NO_NAMESPACES
using std::sort;
using std::pair;
#endif

TpcHitUtilities::TpcHitUtilities()
{
    buildMaps();
    clear();    
}

TpcHitUtilities::TpcHitUtilities(StTrack* tck, double magneticField)
{
    buildMaps();
    clear();
    m_StTrack = tck;
    m_BField = magneticField;
}

TpcHitUtilities::~TpcHitUtilities() {}

//Access--------------------------------------
void TpcHitUtilities::clear()
{
    m_StTrack = 0;
    m_BField = 0.;
    m_tpcHitVec.clear();
    return;
}

void TpcHitUtilities::setTrack(StTrack* tck)
{
    m_StTrack = tck;
    return;
}

void TpcHitUtilities::setBField(double b)
{
    m_BField = b;
    return;
}

const vector<StTpcHit*>& TpcHitUtilities::tpcHitVec() const { return m_tpcHitVec;}

//Methods---------------------------------------
void TpcHitUtilities::findHits()
{
    StPtrVecHit vec = m_StTrack->detectorInfo()->hits(kTpcId);
    StPtrVecHitIterator iter;
    StTpcHit* hit;
    
    for (iter = vec.begin(); iter != vec.end(); iter++) {
	if (*iter){
	    hit = dynamic_cast<StTpcHit*>(*iter);
	    if (hit) { m_tpcHitVec.push_back(hit); }
	}
    }
    return;
}


//Hit Filter, currently keep only flag==0 hits
bool TpcHitUtilities::keepHit(StTpcHit* tpcHit)
{
    if (tpcHit->flag()==0) {return true;}
    else {return false;}

}

//Outward pointing normal of the sector
const StThreeVectorD TpcHitUtilities::sectorNormal(int sector)
{
    int numSectors = gStTpcDb->Dimensions()->numberOfSectors();
    double beta = (sector > 12) ?(numSectors-sector)*2.*M_PI/(static_cast<double>(numSectors)/2.): sector*2.*M_PI/(static_cast<double>(numSectors)/2.);
    const StThreeVectorD vec(sin(beta), cos(beta), 0.);
    return vec;
}

//Caluclate Pathlength using StHelixD
double TpcHitUtilities::dx(StTpcHit* tpcHit)
{
    double ds=0.;
    HitMapQAKey mykey;    //Build a key to the map (sector, padrow)
    mykey.sector = tpcHit->sector();
    mykey.padrow = tpcHit->padrow();
    PadrowLocation padLoc = m_PadrowMap[mykey];
    const StThreeVectorD normal = m_SectorNormalMap[tpcHit->sector()];
    double s_out = m_StTrack->geometry()->helix().pathLength(padLoc.outsidePoint(), normal);
    double s_in  = m_StTrack->geometry()->helix().pathLength(padLoc.insidePoint(), normal);
    ds = s_out-s_in;
    if (ds < 0.) {ds = -1.*ds;}
    if (s_out==DBL_MAX || s_in==DBL_MAX) {ds = 0.;}
    return ds;
}

void TpcHitUtilities::buildMaps()
{
    StTpcCoordinateTransform transformer(gStTpcDb);
    //Build map with an outward pointing normal keyed by sector number
    {for (int sector=1; sector<=24; sector++) {
	m_SectorNormalMap[sector] = sectorNormal(sector);
    }}

    //Build map with a point on outside, center, and inside of padrow (this plus normal define a plane)
    //StTpcCoordinateTransform transformer(gStTpcDb);
    {for (int sector=1; sector<=24; sector++) {
	
	for (int padrow=1; padrow<=45; padrow++) {
	    double padlength;
	    if (padrow<14) {
		padlength = St_tpcPadConfigC::instance()->innerSectorPadLength(sector);}
	    else {
		padlength = St_tpcPadConfigC::instance()->outerSectorPadLength(sector);}
	    
	    //Get the position of the padrow center, transform to local sector coordinates
	    StTpcPadCoordinate padCoord(sector, padrow, 1, 1);
	    StTpcLocalSectorCoordinate lsMidCoord;
	    transformer(padCoord, lsMidCoord);

	    //Boost the local y value by +- padlength / 2.
	    const StThreeVector<double>& lsPos = lsMidCoord.position();
	    StTpcLocalSectorCoordinate lsTopCoord(lsPos.x(),
						  lsPos.y()+padlength/2.,
						  lsPos.z(),
						  sector);
	    StTpcLocalSectorCoordinate lsBotCoord(lsPos.x(),
						  lsPos.y()-padlength/2.,
						  lsPos.z(),
						  sector);
	    //Transform back to global coordinates
	    StGlobalCoordinate gBotCoord, gMidCoord, gTopCoord;
	    transformer(lsTopCoord, gTopCoord);
	    transformer(lsBotCoord, gBotCoord);
	    transformer(lsMidCoord, gMidCoord);
	    const StThreeVector<double>& gTopPos = gTopCoord.position();
	    const StThreeVector<double>& gMidPos = gMidCoord.position();
	    const StThreeVector<double>& gBotPos = gBotCoord.position();
	    const StThreeVectorD gTopPosD(gTopPos.x(),gTopPos.y(),gTopPos.z());
	    const StThreeVectorD gMidPosD(gMidPos.x(),gMidPos.y(),gMidPos.z());
	    const StThreeVectorD gBotPosD(gBotPos.x(),gBotPos.y(),gBotPos.z());

	    //Store in this form
	    PadrowLocation padLocation(gTopPosD, gMidPosD, gBotPosD);
	    HitMapQAKey myKey;
	    myKey.sector = sector;
	    myKey.padrow = padrow;
	    m_PadrowMap.insert(padrowMapValType(myKey,padLocation));
	    //cout <<sector<<"\t"<<padrow<<"\t"<<endl;
	    //padLocation.print();
	}
    }}

    cout<<"Done Building Maps"<<endl;
    return;
}
