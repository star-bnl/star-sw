/***************************************************************************
 *
 * $Id: StTrsChargeSegment.cc,v 1.3 1999/01/28 02:50:28 lasiuk Exp $
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
 * Revision 1.3  1999/01/28 02:50:28  lasiuk
 * beta gamma for particle mass
 *
 * Revision 1.5  1999/02/12 01:26:37  lasiuk
 * Limit debug output
 *
 * Revision 1.4  1999/02/10 18:02:24  lasiuk
 * verbose output and ostream
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
 *
 * add pid member; add systemofunits; mv access fcts to .hh

#include "StPhysicalHelix.hh"

#include "StTrsChargeSegment.hh"
#include "StPhysicalHelix.hh"
#include "StTpcCoordinateTransform.hh"
#include "StTrsDeDx.hh"


StTrsChargeSegment::StTrsChargeSegment()
    : mPosition(0,0,0), mMomentum(0,0,0)
{
    mNumberOfElectrons = -1;
    mDE = 0;
    mDs = 0;
    mPid = 0;
    mSectorOfOrigin = 0;
    mSector12Position = mPosition;
    //mG2tTpcHit = new g2t_tpc_hit;
}

StTrsChargeSegment::StTrsChargeSegment(StThreeVector<double>& pos,
				       StThreeVector<double>& mom,
				       double de,
				       double ds,
				       int    pid,
				       float  ne)
    : mPosition(pos), mMomentum(mom)
{
    mNumberOfElectrons = ne;   // default is -1
    mDE = de;
    mDs = ds;
    mPid = pid;                // default is -1
    mSectorOfOrigin = 0;
void StTrsChargeSegment::rotate(StTpcGeometry* geodb, StTpcSlowControl* SCdb)
}
    StTpcCoordinateTransform transformer(geodb, SCdb);
StTrsChargeSegment::~StTrsChargeSegment() {/* nopt */ }


void StTrsChargeSegment::rotate(StTpcGeometry* geodb, StTpcSlowControl* SCdb, StTpcElectronics* elecDb)
{ // rotate to sector 12 ---use a coordinate transform:
    StTpcCoordinateTransform transformer(geodb, SCdb, elecDb);

    cout << "rotate() position= " << mPosition << endl;

    mSector12Position =
	transformer.sector12Coordinate(mPosition, &mSectorOfOrigin);

    PR(mSectorOfOrigin);
    PR(mSector12Position);
}

#ifndef ST_NO_TEMPLATE_DEF_ARGS
void StTrsChargeSegment::split(StTrsDeDx*       gasDb,
			       StMagneticField* magDb,
			       int subSegments, 
			       list<StTrsMiniChargeSegment>* listOfMiniSegments)
#else
    // what is the subsegment length?
//     PR(gasDb->W());
    double deltaS = mDs/static_cast<double>(subSegments);
    PR(deltaS);
    PR(deltaS/millimeter);

	// what is the subsegment length?
    gasDb->setPadLength(deltaS*centimeter);
    cout << "StTrsDeDx::padLength() -->        "  << (gasDb->padLength()/millimeter) << " mm" << endl;
    cout << "StTrsChargeSegment::split() de--> " << (this->dE()/eV)                 << " eV" << endl;
    cout << "StTrsChargeSegment::split() ds--> " << (this->ds()/millimeter)         << " mm" << endl;
    cout << "StTrsDeDx::W() -->                "   << (gasDb->W()/eV)                 << " eV"  << endl;
    cout << "StMagneticField:at() -->          " << (magDb->at(mSector12Position))    << " T"  <<endl;
    PR(mSector12Position);
    
    // Number of electrons in complete segment
    if (mNumberOfElectrons<0)
	mNumberOfElectrons = (mDE)/(gasDb->W());
    PR(mNumberOfElectrons);
//     cout << "StTrsDeDx::padLength() -->        "  << (gasDb->padLength()/millimeter) << " mm" << endl;
//     cout << "StTrsChargeSegment::split() de--> " << (this->dE()/eV)                 << " eV" << endl;
//     cout << "StTrsChargeSegment::split() ds--> " << (this->ds()/millimeter)         << " mm" << endl;
//     cout << "StTrsDeDx::W() -->                "   << (gasDb->W()/eV)                 << " eV"  << endl;
    
    int charge = 1;  // tmp only
    StPhysicalHelix
	track(mMomentum,
	      mSector12Position,
	      (magDb->at(mSector12Position)).z()*tesla,
	      charge);
    //
    // tmp: pion mass
    // Must get from pid structures
    double particleMass;
    switch (mPid) {
    case 1:
	particleMass = .1395*GeV;
	break;
    case 2:
	particleMass = .493*GeV;
	break;
    case 3:
	particleMass = .939*GeV;
	break;
    default:
	particleMass = .1395*GeV;
	break;
    }
    
    double betaGamma = abs(mMomentum)/particleMass;
    PR(mMomentum);
    PR(betaGamma);
	    break;
    // number of segments to split given by command line argument (default 1):
    //  should be related to mNumberOfElectrons
	double betaGamma = abs(mMomentum)/particleMass;
    float ionizationLeft = mNumberOfElectrons;
//     PR(betaGamma);
    //
    // calculate the offset to the start position
    //
    double newPosition = -mDs/2. + deltaS/2.;
    PR(newPosition);
    
    //
    // loop over all subSegments and distribute charge
    //
	double newPosition = -mDs/2. + deltaS/2.;
	// calculate the offset to the start position
    vector<int> ionization(StTrsDeDx::numberOfElectrons);
	 double newPosition = -mDs/2. + deltaS/2.;
    vector<int, allocator<int> > ionization(StTrsDeDx::numberOfElectrons);
	
    //cout << "ionization.size() " << (ionization.size()) << endl;
    int numberOfElectronsOnMiniSegment;
	vector<int> ionization(StTrsDeDx::numberOfElectrons);
    for(int ii=0; ii<(subSegments-1); ii++) {
	// generate electrons
	gasDb->electrons(ionization, betaGamma);

// 	PR(ionization[StTrsDeDx::primaries]);
// 	PR(ionization[StTrsDeDx::secondaries]);
	PR(ionization[StTrsDeDx::total]);

	if(!ionization[StTrsDeDx::total]) continue;

	// Don't generate too much ionization
	if(ionization[StTrsDeDx::total] > ionizationLeft) {
	    numberOfElectronsOnMiniSegment = ionizationLeft;
	}
	else {
	    numberOfElectronsOnMiniSegment = ionization[StTrsDeDx::total];
	}
	    if(ionization[StTrsDeDx::total] > ionizationLeft) {
	PR(numberOfElectronsOnMiniSegment);
	PR(newPosition);
	PR(track.at(newPosition));
		numberOfElectronsOnMiniSegment = ionization[StTrsDeDx::total];
	StTrsMiniChargeSegment aMiniSegment(track.at(newPosition),
					    numberOfElectronsOnMiniSegment,
					    deltaS);
	listOfMiniSegments->push_back(aMiniSegment);
// 	PR(track.at(newPosition));
	ionizationLeft -= numberOfElectronsOnMiniSegment;
	PR(ionizationLeft);
						numberOfElectronsOnMiniSegment,
	if(!ionizationLeft) break;
		newPosition += deltaS;
	PR(newPosition);
    }
    //
    // last subsegment -- assign remaining ionizaiton
    // GEANT energy is conserved by this (exactly!)
    //
    PR(newPosition);
    PR(track.at(newPosition));
    if(ionizationLeft) {
	StTrsMiniChargeSegment aNewMiniSegment(track.at(newPosition),
					       ionizationLeft,
					       deltaS);
	listOfMiniSegments->push_back(aNewMiniSegment);
    } // if (subsegments > 1)  ---> allows us to skip the helix construction
    else {
	StTrsMiniChargeSegment aSingleMiniSegment(mPosition,
						  mNumberOfElectrons,
ostream& operator<<(ostream& os, StTrsChargeSegment& seg)
	listOfMiniSegments->push_back(aSingleMiniSegment);
    }
// 	PR(mNumberOfElectrons);
     }

}

// Non-member
ostream& operator<<(ostream& os, const StTrsChargeSegment& seg)
{
    return os << '(' << seg.position() << ", " << seg.momentum() << ", " << seg.dE() << ", " << seg.ds() << ')';
}
