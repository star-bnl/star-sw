/***************************************************************************
 *
 * $Id: StTrsChargeSegment.cc,v 1.1 1998/11/10 17:12:23 fisyak Exp $
 *
 * Author: brian May 18, 1998
 *
 ***************************************************************************
 *
 * Description: g2t wrapper with added functionality
 *
 ***************************************************************************
 *
 * $Log: StTrsChargeSegment.cc,v $
 * Revision 1.1  1998/11/10 17:12:23  fisyak
 * Put Brian trs versin into StRoot
 *
 *
 * Revision 1.3  1999/01/28 02:50:28  lasiuk
 * beta gamma for particle mass
 *
 * Revision 1.2  1999/01/15 10:59:11  lasiuk
 * remove g2t pointer
 * Revision 1.1  1998/11/10 17:12:23  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.7  1998/11/08 17:04:44  lasiuk
 * use built in types,
 * namespace macro
 * vector allocators
 *
 * Revision 1.6  1998/11/01 13:55:15  lasiuk
 * coordinate transform rename
 *
 * Revision 1.5  1998/10/22 00:24:20  lasiuk
 * Oct 22
 *
 * Revision 1.4  1998/06/30 23:08:56  lasiuk
 * g2t stored by pointer
 *
 * Revision 1.3  1998/06/04 23:23:37  lasiuk
 * remove transportToWire()
 *
 * Revision 1.2  1998/05/21 21:27:58  lasiuk
 * Initial revision
 *
 * Revision 1.1.1.1  1998/05/19 22:33:44  lasiuk
 * Initial Revision
#include "SystemOfUnits.h"
 *
 * add pid member; add systemofunits; mv access fcts to .hh

    : mPosition(0,0,0), mMomentum(0,0,0), mNumberOfElectrons(0), mSectorOfOrigin(0)

StTrsChargeSegment::StTrsChargeSegment()
    : mPosition(0,0,0), mMomentum(0,0,0)
{
StTrsChargeSegment::StTrsChargeSegment(StThreeVector<double>& pos, StThreeVector<double>& mom, g2t_tpc_hit* seg)
    : mPosition(pos), mMomentum(mom), mNumberOfElectrons(0), mSectorOfOrigin(0)
{
//     mG2tTpcHit = new g2t_tpc_hit;
    mG2tTpcHit = seg;
//     cout << "seg->de " << (mG2tTpcHit->de) << "\t" << mG2tTpcHit << endl;

    // this is temporary...will be unecessary when pointer to g2t structure
    // is stored properly
        mDE = seg->de;
        mDs = seg->ds;
				       float  ne)
    : mPosition(pos), mMomentum(mom)
StTrsChargeSegment::~StTrsChargeSegment()
{
    //delete mG2tTpcHit;
}
    mNumberOfElectrons = ne;   // default is -1
StThreeVector<double>& StTrsChargeSegment::position()
{
    return mPosition;
}

StThreeVector<double>& StTrsChargeSegment::momentum()
{
    return mMomentum;
}

double StTrsChargeSegment::dE()
{
    //return mDE;
    return mG2tTpcHit->de;
}

double StTrsChargeSegment::ds()
{
    //return mDs;
    return mG2tTpcHit->ds;
}
    mDE = de;
    mDs = ds;
    mPid = pid;                // default is -1
    mSectorOfOrigin = 0;
void StTrsChargeSegment::rotate(StTpcGeometry* geodb, StTpcSlowControl* SCdb)
}
    StTpcCoordinateTransform transformer(geodb, SCdb);
StTrsChargeSegment::~StTrsChargeSegment() {/* nopt */ }


    cout << "mSectorOfOrigin: " << mSectorOfOrigin << endl;
    cout << "mSector12Position: " << mSector12Position << endl;
    cout << endl;
    StTpcCoordinateTransform transformer(geodb, SCdb, elecDb);

    cout << "rotate() position= " << mPosition << endl;
void StTrsChargeSegment::split(StTrsDeDx* gasDb, StMagneticField* magDb,
			       int subSegments, double segmentLength,
			       list<StTrsMiniChargeSegment>* listOfMiniSegments
			       )
    PR(mSectorOfOrigin);
void StTrsChargeSegment::split(StTrsDeDx* gasDb, StMagneticField* magDb,
			       int subSegments, double segmentLength,
			       list<StTrsMiniChargeSegment,allocator<StTrsMiniChargeSegment> >* listOfMiniSegments
			       )
void StTrsChargeSegment::split(StTrsDeDx*       gasDb,
			       StMagneticField* magDb,
			       int subSegments, 
			       list<StTrsMiniChargeSegment>* listOfMiniSegments)
#else
    gasDb->setPadLength(segmentLength/centimeter);
    //cout << "StTrsDeDx::padLength() -->        " << gasDb->padLength() << endl;
    //cout << "StTrsChargeSegment::split() de--> " << this->dE() << endl;
    //cout << "StTrsChargeSegment::split() ds--> " << this->ds() << endl;
    //cout << "StTrsDeDx::W() -->                " << gasDb->W() << endl;
    //cout << "StMagneticField:at() -->          " << (magDb->at(mSector12Position)) << endl;
    
    // Maximum number of electrons in complete segment
    mNumberOfElectrons = (this->dE())/(gasDb->W()/eV);
    PR(mNumberOfElectrons);
//     cout << "StTrsDeDx::padLength() -->        "  << (gasDb->padLength()/millimeter) << " mm" << endl;
//     cout << "StTrsChargeSegment::split() de--> " << (this->dE()/eV)                 << " eV" << endl;
//     cout << "StTrsChargeSegment::split() ds--> " << (this->ds()/millimeter)         << " mm" << endl;
    // Need track info from g2t:
    // - momentum in current volume
    //   (NOT total track momentum  because this is defined at the vertex)!
    // - charge
    // At center of g2t volume!!
    
    int charge = 1;  // tmp only
    StPhysicalHelix
	track(mMomentum,
	      mSector12Position,
	      (magDb->at(mSector12Position)).z(),
	      charge);
    //
    // tmp: pion mass
    // Must get from g2t structures
    double particleMass = .1395*GeV;
    
    double betaGamma = abs(mMomentum)/particleMass;
    //PR(mMomentum);
    //PR(betaGamma);
	    break;
    // number of segments to split given by command line argument (default 1):
    //  should be related to mNumberOfElectrons
	double betaGamma = abs(mMomentum)/particleMass;
    int    ionizationLeft = mNumberOfElectrons;
    double deltaS         = mDs/static_cast<double>(subSegments);
//     PR(betaGamma);
    //
    // define the new origin
    //
    double newPosition = -((this->ds()) + deltaS)/2;

    //
    // loop over all subSegments and distribute charge
    //
	// calculate the offset to the start position
    vector<int> ionization(StTrsDeDx::numberOfElectrons);
	 double newPosition = -mDs/2. + deltaS/2.;
    vector<int, allocator<int> > ionization(StTrsDeDx::numberOfElectrons);
	
    //cout << "ionization.size() " << (ionization.size()) << endl;
    int numberOfElectronsOnMiniSegment;
	vector<int> ionization(StTrsDeDx::numberOfElectrons);
    for(int ii=0; ii<(subSegments-1); ii++) {
	newPosition += deltaS;
	//PR(newPosition);
	
	gasDb->electrons(ionization, betaGamma);

	if(!ionization[StTrsDeDx::total]) continue;
	
	//PR(ionization[StTrsDeDx::total]);
	if(ionization[StTrsDeDx::total] > ionizationLeft) {
	    numberOfElectronsOnMiniSegment = ionizationLeft;
	}
	else {
	    numberOfElectronsOnMiniSegment = ionization[StTrsDeDx::total];
	}
	    if(ionization[StTrsDeDx::total] > ionizationLeft) {
	//PR(numberOfElectronsOnMiniSegment);

	StTrsMiniChargeSegment aMiniSegment(track.at(newPosition),
					    numberOfElectronsOnMiniSegment,
					    deltaS);
	listOfMiniSegments->push_back(aMiniSegment);
// 	PR(track.at(newPosition));
	ionizationLeft -= numberOfElectronsOnMiniSegment;
	//PR(ionizationLeft);
						numberOfElectronsOnMiniSegment,
	if(!ionizationLeft) break;
    }
    //
    // last subsegment
    //
    if(ionizationLeft) {
	StTrsMiniChargeSegment aNewMiniSegment(track.at(newPosition),
					       ionizationLeft,
					       deltaS);
	listOfMiniSegments->push_back(aNewMiniSegment);
    } // if (subsegments > 1)  ---> allows us to skip the helix construction
    else {
	StTrsMiniChargeSegment aSingleMiniSegment(mPosition,
     }

}

// Non-member
ostream& operator<<(ostream& os, const StTrsChargeSegment& seg)
{
    return os << '(' << seg.position() << ", " << seg.momentum() << ", " << seg.dE() << ", " << seg.ds() << ')';
}
