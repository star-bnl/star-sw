/***************************************************************************
 *
 * $Id: StParticleDefinition.cc,v 1.4 2016/07/25 17:45:33 jwebb Exp $
 *
 * Author: Thomas Ullrich, May 99 (based on Geant4 code, see below) 
 ***************************************************************************
 *
 * The design of the StParticleDefinition class and all concrete
 * classes derived from it is largely based on the design of the 
 * G4ParticleDefinition class from Geant4 (RD44).
 * Although the code is in large parts different (modified or rewritten)
 * and adapted to the STAR framework the basic idea stays the same.
 *
 ***************************************************************************
 *
 * $Log: StParticleDefinition.cc,v $
 * Revision 1.4  2016/07/25 17:45:33  jwebb
 * Init members in ctor / coverity
 *
 * Revision 1.3  2009/10/13 18:31:39  perev
 * Unonimous update
 *
 * Revision 1.2  1999/12/07 23:43:04  ullrich
 * Modified to get rid of warnings on Linux.
 *
 * Revision 1.1  1999/05/14 18:48:13  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StParticleDefinition.hh"
#include "StParticleTable.hh"
#include "PhysicalConstants.h"
// #include <typeinfo>
#ifdef __ROOT__
ClassImp(StParticleDefinition)
#endif
 
StParticleDefinition::StParticleDefinition(
					   const string  &     aName,  
					   double              aMass,
					   double              aWidth,
					   double              aCharge,   
					   int                 aISpin,
					   int                 aIParity,    
					   int                 aIConjugation,
					   int                 aIIsospin,   
					   int                 aIIsospin3, 
					   int                 aGParity,
					   const string  &     aType,
					   int                 aLepton,      
					   int                 aBaryon,
					   int                 aEncoding,
					   bool                aIsStableFlag,
					   double              aLifetime)
    : mParticleName(aName), 
    mPDGMass(aMass),
    mPDGWidth(aWidth),
    mPDGCharge(aCharge),
    mPDGiSpin(aISpin),
    mPDGSpin(aISpin*0.5),
    mPDGiParity(aIParity), 
    mPDGiConjugation(aIConjugation),
    mPDGiIsospin(aIIsospin),
    mPDGiIsospin3(aIIsospin3),
    mPDGIsospin(aIIsospin*0.5),
    mPDGIsospin3(aIIsospin3*0.5),
    mPDGiGParity(aGParity),
    mLeptonNumber(aLepton),
    mBaryonNumber(aBaryon),
    mParticleType(aType), 
    mPDGEncoding(aEncoding),
    mAntiPDGEncoding(-1*aEncoding),
    mPDGStable(aIsStableFlag), 
  mPDGLifeTime(aLifetime) ,
  mParticleTable(0)
{
    //
    // check name and register this particle into ParticleTable
    //
    mParticleTable = StParticleTable::particleTable();
    mParticleTable->insert(this);
}

StParticleDefinition::StParticleDefinition(const StParticleDefinition&){/* private */}

StParticleDefinition::StParticleDefinition() :
  mParticleName(""), 
    mPDGMass(0),
    mPDGWidth(0),
    mPDGCharge(0),
    mPDGiSpin(0),
    mPDGSpin(0),
    mPDGiParity(0), 
    mPDGiConjugation(0),
    mPDGiIsospin(0),
    mPDGiIsospin3(0),
    mPDGIsospin(0),
    mPDGIsospin3(0),
    mPDGiGParity(0),
    mLeptonNumber(0),
    mBaryonNumber(0),
    mParticleType(0), 
    mPDGEncoding(0),
    mAntiPDGEncoding(0),
    mPDGStable(0), 
  mPDGLifeTime(0) ,
  mParticleTable(0)
 {/* private */}

StParticleDefinition::~StParticleDefinition() {/* noop */}

const StParticleDefinition&
StParticleDefinition::operator=(const StParticleDefinition& p) {/* private */ return p;}

int StParticleDefinition::operator==(const StParticleDefinition &p) const
{
    return (this->mParticleName == p.mParticleName);
}

int StParticleDefinition::operator!=(const StParticleDefinition &right) const
{
    return !(*this == right);
}

ostream& operator<<(ostream& os, const StParticleDefinition& p)
{
//    os << '<' << typeid(p).name() << '>' << endl;   // needs RTTI
    os << "Particle Name :         " << p.name().c_str() << endl;
    os << "PDG particle code :     " << p.pdgEncoding()  << endl;
    os << "Mass [GeV/c2] :         " << p.mass()/GeV     << endl;
    os << "Width [GeV/c2] :        " << p.width()/GeV    << endl;
    os << "Lifetime [nsec] :       ";
    if (p.lifeTime() < 0)
	os << "-";
    else
	os << p.lifeTime()/nanosecond;
    os << endl;
    os << "Charge [e] :            " << p.charge()/eplus << endl;
    os << "Spin :                  " << p.iSpin() << "/2" << endl;
    os << "Parity :                " << p.iParity()      << endl;
    os << "Charge conjugation :    " << p.iConjugation() << endl;
    os << "Isospin : (I,Iz):       (" << p.iIsospin() <<"/2" << " , " << p.iIsospin3() << "/2 ) " << endl;
    os << "GParity :               " << p.iGParity()     << endl;
    os << "Lepton number :         " << p.leptonNumber() << endl;
    os << "Baryon number :         " << p.baryonNumber() << endl;
    os << "Particle type :         " << p.type().c_str() << endl;
    os << "Is stable :             " << (p.stable() ? "yes" : "no") << endl;
    return os;
}





