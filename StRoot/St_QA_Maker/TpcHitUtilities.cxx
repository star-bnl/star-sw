// TpcHitUtilities.cxx
// M.L. Miller
// Yale Software
// 7/00

#include "TpcHitUtilities.h"
#include "StEventTypes.h" // StEvent
#include "StThreeVectorD.hh"
#include <algorithm> // STL
// StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StTpcDb/StTpcDb.h"
#include "StTpcDb/StTpcPadPlaneI.h"

TpcHitUtilities::TpcHitUtilities() {};

TpcHitUtilities::TpcHitUtilities(StTrack* tck, double magneticField)
{
    m_StTrack = tck;
    m_BField = magneticField;
}

TpcHitUtilities::~TpcHitUtilities() {};

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
    StTpcCoordinateTransform transformer(gStTpcDb);

    double padlength;
    if (tpcHit->padrow()<14) {
	padlength = gStTpcDb->PadPlaneGeometry()->innerSectorPadLength();}
    else {
	padlength = gStTpcDb->PadPlaneGeometry()->outerSectorPadLength();}

    //Get the position of the hit, transform to local sector coordinates
    const StGlobalCoordinate& gMidCoord(tpcHit->position());
    StTpcLocalSectorCoordinate lsMidCoord;
    transformer(gMidCoord, lsMidCoord);

    //Boost the local y value by +- padlength / 2.
    const StThreeVector<double>& lsPos = lsMidCoord.position();
    StTpcLocalSectorCoordinate lsTopCoord(lsPos.x(),
					  lsPos.y()+padlength/2.,
					  lsPos.z(),
					  tpcHit->sector());
    StTpcLocalSectorCoordinate lsBotCoord(lsPos.x(),
					  lsPos.y()-padlength/2.,
					  lsPos.z(),
					  tpcHit->sector());
    //Transform back to global coordinates
    StGlobalCoordinate gBotCoord, gTopCoord;
    transformer(lsTopCoord, gTopCoord);
    transformer(lsBotCoord, gBotCoord);
    const StThreeVector<double>& gTopPos = gTopCoord.position();
    const StThreeVector<double>& gMidPos = gMidCoord.position();
    const StThreeVector<double>& gBotPos = gBotCoord.position();
    const StThreeVectorD gTopPosD(gTopPos.x(),gTopPos.y(),gTopPos.z());
    const StThreeVectorD gMidPosD(gMidPos.x(),gMidPos.y(),gMidPos.z());
    const StThreeVectorD gBotPosD(gBotPos.x(),gBotPos.y(),gBotPos.z());

    //Now Calculate pathlength difference between inner and outer points
    const StThreeVectorD normal = sectorNormal(tpcHit->sector());
    double s_out = m_StTrack->geometry()->helix().pathLength(gTopPosD, normal);
    double s_in  = m_StTrack->geometry()->helix().pathLength(gBotPosD, normal);
    ds = s_out - s_in;
    if (ds < 0.) {ds = -1.*ds;}

    if (s_out==DBL_MAX || s_in==DBL_MAX) {ds = 0.;}
    return ds;
}

//Calculate Crossing Angle in degrees
double TpcHitUtilities::crossingAngle(StTpcHit* hit)
{
    const StThreeVectorD normal = sectorNormal(hit->sector());
    //Get the pathlength at the middle of the pad, a big mess because of no template support
    const StGlobalCoordinate& gMidCoord(hit->position());
    const StThreeVector<double>& gMidPos = gMidCoord.position();
    const StThreeVectorD gMidPosD(gMidPos.x(),gMidPos.y(),gMidPos.z());
    const double pathlength = m_StTrack->geometry()->helix().pathLength(gMidPosD);
    const StThreeVectorD momentum = m_StTrack->geometry()->helix().momentumAt(pathlength, m_BField);
    const StThreeVectorD xymomentum(momentum.x(), momentum.y(), 0.);
    const double crossingAngle = normal.angle(xymomentum);

    return 180./3.14159*crossingAngle;
}

//Utilities----------------------------------
//Print tpcHitVec to screen (debugging)
void TpcHitUtilities::printTpcHitVec()
{
    for (vector<StTpcHit*>::iterator it1 = m_tpcHitVec.begin(); it1 != m_tpcHitVec.end(); it1++) {
	cout <<(*it1)->sector()<<"\t"<<(*it1)->padrow()<<"\t"<<(*it1)->charge()<<"\t"<<endl;
    }
    return;
}
