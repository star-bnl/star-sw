/***************************************************************************
 *
 * $Id: StParticleDefinition.hh,v 1.4 2005/09/22 20:09:20 fisyak Exp $
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
 * $Log: StParticleDefinition.hh,v $
 * Revision 1.4  2005/09/22 20:09:20  fisyak
 * Make StLorentzVector persistent
 *
 * Revision 1.3  2003/09/02 17:59:35  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  1999/12/21 15:14:21  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.1  1999/05/14 18:49:58  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StParticleDefinition_hh
#define StParticleDefinition_hh
#ifdef __ROOT__
#include "Rtypes.h"
#endif

#include <string>
#include <Stiostream.h>
#if !defined(ST_NO_NAMESPACES)
using std::string;
#endif

class StParticleTable;

class StParticleDefinition {
public:
    StParticleDefinition(const string  &  aName,  
			 double           mass,     
			 double           width,
			 double           charge,   
			 int              iSpin,
			 int              iParity,
			 int              iConjugation,
			 int              iIsospin,   
			 int              iIsospinZ, 
			 int              gParity,
			 const string  &  pType,
			 int              lepton,
			 int              baryon,
			 int              encoding,
			 bool             stable,
			 double           lifetime);
    virtual ~StParticleDefinition();
    
    int   operator==(const StParticleDefinition &right) const;
    int   operator!=(const StParticleDefinition &right) const;
    
    string   name() const            {return mParticleName;}
    double   mass() const            {return mPDGMass;}
    double   width() const           {return mPDGWidth;} 
    double   charge() const          {return mPDGCharge;}
    double   spin() const            {return mPDGSpin;}
    int      iSpin() const           {return mPDGiSpin;}
    int      iParity() const         {return mPDGiParity;}
    int      iConjugation() const    {return mPDGiConjugation;}
    double   isospin() const         {return mPDGiIsospin;}
    double   isospin3() const        {return mPDGiIsospin3;}
    int      iIsospin() const        {return mPDGiIsospin;}
    int      iIsospin3() const       {return mPDGiIsospin3;}
    int      iGParity() const        {return mPDGiGParity;}
    string   type() const            {return mParticleType;}
    int      leptonNumber() const    {return mLeptonNumber;}
    int      baryonNumber() const    {return mBaryonNumber;}    
    int      pdgEncoding() const     {return mPDGEncoding;}
    int      antiPdgEncoding() const {return mAntiPDGEncoding;}
    bool     stable() const          {return mPDGStable;}    
    double   lifeTime() const        {return mPDGLifeTime;}

    StParticleTable* particleTable() const {return mParticleTable;}
    
private:
    StParticleDefinition(const StParticleDefinition &);
    StParticleDefinition();
    const StParticleDefinition & operator=(const StParticleDefinition &);

    string   mParticleName;       // name of the particle.
    double   mPDGMass;            // mass of the particle, in units of equivalent energy.
    double   mPDGWidth;           // decay width of the particle, usually the width of a
                                  // Breit-Wigner function, assuming that you are near the
                                  // mass center anyway. (in units of equivalent energy)
    double   mPDGCharge;          // charge of the particle.(in units of Coulomb)
    int      mPDGiSpin;           // total spin of the particle, also often denoted as
                                  // capital J, in units of 1/2.
    double   mPDGSpin;            // total spin of the particle, in units of 1.
    int      mPDGiParity;         // parity quantum number, in units of 1. If the parity
                                  // is not defined for this particle, we will set this to 0.
    int      mPDGiConjugation;    // charge conjugation quantum number in units of 1.
    int      mPDGiIsospin;        // isospin and its 3rd-component in units of 1/2.
    int      mPDGiIsospin3;         
    double   mPDGIsospin;
    double   mPDGIsospin3;        // isospin quantum number in units of 1.
    int      mPDGiGParity;        // value of the G-parity quantum number.
    int      mLeptonNumber;       // lepton quantum number.
    int      mBaryonNumber;       // baryon quantum number.
    string   mParticleType;       // general textual type description of the particle.
    int      mPDGEncoding;        // Particle Data Group integer identifier of this particle
    int      mAntiPDGEncoding;    // Particle Data Group integer identifier of the anti-particle
    bool     mPDGStable;
    double   mPDGLifeTime;        // related to the decay width of the particle. The mean
                                  // life time is given in seconds.
    StParticleTable *mParticleTable;
#ifdef __ROOT__
  ClassDef(StParticleDefinition,1)
#endif
};

ostream& operator<<(ostream&, const StParticleDefinition&);

#endif





















