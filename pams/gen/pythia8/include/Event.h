// Event.h is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Header file for the Particle and Event classes.
// Particle: information on an instance of a particle.
// Junction: information on a junction between three colours.
// Event: list of particles in the current event.

#ifndef Pythia8_Event_H
#define Pythia8_Event_H

#include "Basics.h"
#include "ParticleData.h"
#include "PythiaStdlib.h"
#include "Settings.h"

namespace Pythia8 {

//**************************************************************************

// Forward references to ParticleData and ResonanceWidths classes.
class ParticleDataEntry;
class ResonanceWidths;

//**************************************************************************

// Particle class.
// This class holds info on a particle in general.

class Particle {

public:

  // Constructors.
  Particle() : idSave(0), statusSave(0), mother1Save(0), mother2Save(0), 
    daughter1Save(0), daughter2Save(0), colSave(0), acolSave(0), 
    pSave(Vec4(0.,0.,0.,0.)), mSave(0.), scaleSave(0.), 
    hasVertexSave(false), vProdSave(Vec4(0.,0.,0.,0.)), tauSave(0.), 
    particlePtr(0) { }  
  Particle(int idIn, int statusIn = 0, int mother1In = 0, 
    int mother2In = 0, int daughter1In = 0, int daughter2In = 0,
    int colIn = 0, int acolIn = 0, double pxIn = 0., double pyIn = 0., 
    double pzIn = 0., double eIn = 0., double mIn = 0., double scaleIn = 0.) 
    : idSave(idIn), statusSave(statusIn), mother1Save(mother1In), 
    mother2Save(mother2In), daughter1Save(daughter1In), 
    daughter2Save(daughter2In), colSave(colIn), acolSave(acolIn), 
    pSave(Vec4(pxIn, pyIn, pzIn, eIn)), mSave(mIn), scaleSave(scaleIn), 
    hasVertexSave(false), vProdSave(Vec4(0.,0.,0.,0.)), tauSave(0.) 
    {setParticlePtr();}  
  Particle(int idIn, int statusIn, int mother1In, int mother2In, 
    int daughter1In, int daughter2In, int colIn, int acolIn, 
    Vec4 pIn, double mIn = 0., double scaleIn = 0.) 
    : idSave(idIn), statusSave(statusIn), mother1Save(mother1In), 
    mother2Save(mother2In), daughter1Save(daughter1In), 
    daughter2Save(daughter2In), colSave(colIn), acolSave(acolIn), 
    pSave(pIn), mSave(mIn), scaleSave(scaleIn), hasVertexSave(false), 
    vProdSave(Vec4(0.,0.,0.,0.)), tauSave(0.) {setParticlePtr();}  
  Particle(const Particle& pt) : idSave(pt.idSave), 
    statusSave(pt.statusSave), mother1Save(pt.mother1Save), 
    mother2Save(pt.mother2Save), daughter1Save(pt.daughter1Save), 
    daughter2Save(pt.daughter2Save), colSave(pt.colSave), 
    acolSave(pt.acolSave), pSave(pt.pSave), mSave(pt.mSave), 
    scaleSave(pt.scaleSave), hasVertexSave(pt.hasVertexSave), 
    vProdSave(pt.vProdSave), tauSave(pt.tauSave), 
    particlePtr(pt.particlePtr) { } 
  Particle& operator=(const Particle& pt) {if (this != &pt) {
    idSave = pt.idSave; statusSave = pt.statusSave; 
    mother1Save = pt.mother1Save; mother2Save = pt.mother2Save; 
    daughter1Save = pt.daughter1Save; daughter2Save = pt.daughter2Save; 
    colSave = pt.colSave; acolSave = pt.acolSave; pSave = pt.pSave; 
    mSave = pt.mSave; scaleSave = pt.scaleSave; 
    hasVertexSave = pt.hasVertexSave; vProdSave = pt.vProdSave; 
    tauSave = pt.tauSave; particlePtr = pt.particlePtr; } return *this; } 
      
  // Member functions for input.
  void id(int idIn) {idSave = idIn; setParticlePtr();}
  void status(int statusIn) {statusSave = statusIn;}
  void statusPos() {statusSave = abs(statusSave);}
  void statusNeg() {statusSave = -abs(statusSave);}
  void statusCode(int statusIn) {statusSave = 
    (statusSave > 0) ? abs(statusIn) : -abs(statusIn);}
  void mother1(int mother1In) {mother1Save = mother1In;}
  void mother2(int mother2In) {mother2Save = mother2In;}
  void mothers(int mother1In = 0, int mother2In = 0) 
    {mother1Save = mother1In; mother2Save = mother2In;}
  void daughter1(int daughter1In) {daughter1Save = daughter1In;}
  void daughter2(int daughter2In) {daughter2Save = daughter2In;}
  void daughters(int daughter1In = 0, int daughter2In = 0) 
    {daughter1Save = daughter1In; daughter2Save = daughter2In;}  
  void col(int colIn) {colSave = colIn;}
  void acol(int acolIn) {acolSave = acolIn;}
  void cols(int colIn = 0,int acolIn = 0) {colSave = colIn; 
    acolSave = acolIn;}  
  void p(Vec4 pIn) {pSave = pIn;}
  void p(double pxIn, double pyIn, double pzIn, double eIn) 
    {pSave.p(pxIn, pyIn, pzIn, eIn);}
  void px(double pxIn) {pSave.px(pxIn);}
  void py(double pyIn) {pSave.py(pyIn);}
  void pz(double pzIn) {pSave.pz(pzIn);}
  void e(double eIn) {pSave.e(eIn);}
  void m(double mIn) {mSave = mIn;}
  void scale(double scaleIn) {scaleSave = scaleIn;}
  void vProd(Vec4 vProdIn) {vProdSave = vProdIn; hasVertexSave = true;}
  void vProd(double xProdIn, double yProdIn, double zProdIn, double tProdIn)
    {vProdSave.p(xProdIn, yProdIn, zProdIn, tProdIn); hasVertexSave = true;}
  void xProd(double xProdIn) {vProdSave.px(xProdIn); hasVertexSave = true;} 
  void yProd(double yProdIn) {vProdSave.py(yProdIn); hasVertexSave = true;} 
  void zProd(double zProdIn) {vProdSave.pz(zProdIn); hasVertexSave = true;} 
  void tProd(double tProdIn) {vProdSave.e(tProdIn); hasVertexSave = true;} 
  void tau(double tauIn) {tauSave = tauIn;} 

  // Member functions for output.
  int    id()        const {return idSave;}
  int    status()    const {return statusSave;}
  int    mother1()   const {return mother1Save;}
  int    mother2()   const {return mother2Save;}
  int    daughter1() const {return daughter1Save;}
  int    daughter2() const {return daughter2Save;}
  int    col()       const {return colSave;}
  int    acol()      const {return acolSave;}
  Vec4   p()         const {return pSave;}
  double px()        const {return pSave.px();}
  double py()        const {return pSave.py();}
  double pz()        const {return pSave.pz();}
  double e()         const {return pSave.e();}
  double m()         const {return mSave;}
  double scale()     const {return scaleSave;}
  bool   hasVertex() const {return hasVertexSave;}
  Vec4   vProd()     const {return vProdSave;}
  double xProd()     const {return vProdSave.px();}
  double yProd()     const {return vProdSave.py();}
  double zProd()     const {return vProdSave.pz();}
  double tProd()     const {return vProdSave.e();}
  double tau()       const {return tauSave;}

  // Member functions for output; derived int and bool quantities.
  int    idAbs()     const {return abs(idSave);}
  int    statusAbs() const {return abs(statusSave);}
  bool   isFinal()   const {return (statusSave > 0);}

  // Member functions for output; derived double quantities.
  double m2()        const {return mSave*mSave;}
  double mCalc()     const {return pSave.mCalc();}
  double m2Calc()    const {return pSave.m2Calc();}
  double eCalc()     const {return sqrt(mSave*mSave + pSave.pAbs2());}
  double pT()        const {return pSave.pT();}
  double pT2()       const {return pSave.pT2();}
  double mT()        const {return sqrt(mSave*mSave + pSave.pT2());}
  double mT2()       const {return mSave*mSave + pSave.pT2();}
  double pAbs()      const {return pSave.pAbs();}
  double pAbs2()     const {return pSave.pAbs2();}
  double theta()     const {return pSave.theta();}
  double phi()       const {return pSave.phi();}
  double thetaXZ()   const {return pSave.thetaXZ();}
  double pPlus()     const {return pSave.pPlus();}
  double pMinus()    const {return pSave.pMinus();}
  double y()         const;
  double eta()       const; 
  Vec4   vDec()      const {return (tauSave > 0. && mSave > 0.) 
    ? vProdSave + tauSave * pSave / mSave : vProdSave;}
  double xDec()      const {return (tauSave > 0. && mSave > 0.) 
    ? vProdSave.px() + tauSave * pSave.px() / mSave : vProdSave.px();}
  double yDec()      const {return (tauSave > 0. && mSave > 0.)  
    ? vProdSave.py() + tauSave * pSave.py() / mSave : vProdSave.py();}
  double zDec()      const {return (tauSave > 0. && mSave > 0.)  
    ? vProdSave.pz() + tauSave * pSave.pz() / mSave : vProdSave.pz();}
  double tDec()      const {return (tauSave > 0. && mSave > 0.)  
    ? vProdSave.e()  + tauSave * pSave.e()  / mSave : vProdSave.e();}

  // Further output, based on a pointer to a ParticleDataEntry object.
  string name()      const {return particlePtr->name(idSave);}
  string nameWithStatus(int maxLen = 20) const;
  int    spinType()  const {return particlePtr->spinType();}
  int    chargeType() const {return particlePtr->chargeType(idSave);}
  double charge()    const {return  particlePtr->charge(idSave);}
  bool   isCharged() const {return (particlePtr->chargeType(idSave) != 0);}
  bool   isNeutral() const {return (particlePtr->chargeType(idSave) == 0);}
  int    colType()   const {return particlePtr->colType(idSave);}
  double m0()        const {return particlePtr->m0();}
  double mWidth()    const {return particlePtr->mWidth();}
  double mMin()      const {return particlePtr->mMin();}
  double mMax()      const {return particlePtr->mMax();}
  double mass()      const {return particlePtr->mass();}
  double constituentMass() const {return particlePtr->constituentMass();}
  double tau0()      const {return particlePtr->tau0();}
  bool   mayDecay()  const {return particlePtr->mayDecay();}
  bool   canDecay()  const {return particlePtr->canDecay();}
  bool   doExternalDecay() const {return particlePtr->doExternalDecay();}
  bool   isResonance() const {return particlePtr->isResonance();}
  bool   isVisible() const {return particlePtr->isVisible();}
  bool   isLepton()  const {return particlePtr->isLepton();}
  bool   isQuark()   const {return particlePtr->isQuark();}
  bool   isGluon()   const {return particlePtr->isGluon();}
  bool   isHadron()  const {return particlePtr->isHadron();}
  ParticleDataEntry& particleData() const {return *particlePtr;}

  // Member functions that perform operations.
  void rescale3(double fac) {pSave.rescale3(fac);}
  void rescale4(double fac) {pSave.rescale4(fac);}
  void rescale5(double fac) {pSave.rescale4(fac); mSave *= fac;}
  void rot(double thetaIn, double phiIn) {pSave.rot(thetaIn, phiIn);
    if (hasVertexSave) vProdSave.rot(thetaIn, phiIn);} 
  void bst(double betaX, double betaY, double betaZ) {
    pSave.bst(betaX, betaY, betaZ);
    if (hasVertexSave) vProdSave.bst(betaX, betaY, betaZ);}
  void bst(double betaX, double betaY, double betaZ, double gamma) {
    pSave.bst(betaX, betaY, betaZ, gamma);
    if (hasVertexSave) vProdSave.bst(betaX, betaY, betaZ, gamma);}
  void bst(const Vec4& pBst) {pSave.bst(pBst);
    if (hasVertexSave) vProdSave.bst(pBst);}
  void bst(const Vec4& pBst, double mBst) {pSave.bst(pBst, mBst);
    if (hasVertexSave) vProdSave.bst(pBst, mBst);}
  void bstback(const Vec4& pBst) {pSave.bstback(pBst);
    if (hasVertexSave) vProdSave.bstback(pBst);}
  void bstback(const Vec4& pBst, double mBst) {pSave.bstback(pBst, mBst);
    if (hasVertexSave) vProdSave.bstback(pBst, mBst);}
  void rotbst(const RotBstMatrix& M) {pSave.rotbst(M);
    if (hasVertexSave) vProdSave.rotbst(M);} 
  void offsetHistory( int minMother, int addMother, int minDaughter, 
    int addDaughter);
  void offsetCol( int addCol); 

  // Member function to set the ParticleDataEntry pointer, using idSave.
  void setParticlePtr();

private:

  // Constants: could only be changed in the code itself.
  static const double TINY;

  // Properties of the current particle.
  int idSave, statusSave, mother1Save, mother2Save, daughter1Save, 
    daughter2Save, colSave, acolSave;
  Vec4 pSave;
  double mSave, scaleSave;
  bool hasVertexSave;
  Vec4 vProdSave;
  double tauSave;

  // Pointer to properties of the particle species.
  // Should no be saved in a persistent copy of the event record.
  // The //! below is ROOT notation that this member should not be saved.
  // Event::restorePtrs() can be called to restore the missing information. 
  ParticleDataEntry* particlePtr;  //!

};

// Invariant mass of a pair and its square.
// (Not part of class proper, but tightly linked.)
double m(const Particle&, const Particle&); 
double m2(const Particle&, const Particle&); 

//**************************************************************************

// The juction class stores what kind of junction it is, the colour indices 
// of the legs at the junction and as far out as legs have been traced,
// and the status codes assigned for fragmentation of each leg.

class Junction {

public:

  // Constructors.
  Junction() : remainsSave(true), kindSave(0) { 
    for (int j = 0; j < 3; ++j) {
    colSave[j] = 0; endColSave[j] = 0; statusSave[j] = 0; } }
  Junction( int kindIn, int col0In, int col1In, int col2In) 
    : remainsSave(true), kindSave(kindIn) {colSave[0] = col0In; 
    colSave[1] = col1In; colSave[2] = col2In; 
    for (int j = 0; j < 3; ++j) {
    endColSave[j] = colSave[j]; statusSave[j] = 0; } }
  Junction(const Junction& ju) : remainsSave(ju.remainsSave), 
    kindSave(ju.kindSave) { for (int j = 0; j < 3; ++j) {
    colSave[j] = ju.colSave[j]; endColSave[j] = ju.endColSave[j]; 
    statusSave[j] = ju.statusSave[j]; } }
  Junction& operator=(const Junction& ju) {if (this != &ju) { 
    remainsSave = ju.remainsSave; kindSave =  ju.kindSave; 
    for (int j = 0; j < 3; ++j) { colSave[j] = ju.colSave[j]; 
    endColSave[j] = ju.endColSave[j]; statusSave[j] = ju.statusSave[j]; } } 
    return *this; }  

  // Set values.
  void remains(bool remainsIn) {remainsSave = remainsIn;}
  void col(int j, int colIn) {colSave[j] = colIn; endColSave[j] = colIn;}
  void cols(int j, int colIn, int endColIn) {colSave[j] = colIn; 
    endColSave[j] = endColIn;}
  void endCol(int j, int endColIn) {endColSave[j] = endColIn;}
  void status(int j, int statusIn) {statusSave[j] = statusIn;}

  // Read out value.
  bool   remains()     const {return remainsSave;}
  int    kind()        const {return kindSave;}
  int    col(int j)    const {return colSave[j];}
  int    endCol(int j) const {return endColSave[j];}
  int    status(int j) const {return statusSave[j];}
 
private:

  // Kind, positions of the three ends and their status codes.
  bool remainsSave;
  int kindSave, colSave[3], endColSave[3], statusSave[3];

};

//**************************************************************************

// The Event class holds all info on the generated event.

class Event {
    
public:

  // Constructors.
  Event(int capacity = 100) {entry.reserve(capacity); startColTag = 100; 
    headerList = "----------------------------------------";}
  Event& operator=(const Event& oldEvent);

  // Initialize colour and header specification for event listing.
  void init( string headerIn = "");

  // Clear event record.
  void clear() {entry.resize(0); maxColTag = startColTag; 
    clearJunctions(); clearSystems();}

  // Clear event record, and set first particle empty.
  void reset() {clear(); append(90, -11, 0, 0, 0., 0., 0., 0., 0.);}

  // Overload index operator to access element of event record.
  Particle& operator[](int i) {return entry[i];}
  const Particle& operator[](int i) const {return entry[i];}

  // Event record size.
  int size() const {return entry.size();}

  // Put a new particle at the end of the event record; return index.
  int append(Particle entryIn) {    
    entry.push_back(entryIn); 
    if (entryIn.col() > maxColTag) maxColTag = entryIn.col();   
    if (entryIn.acol() > maxColTag) maxColTag = entryIn.acol();
    return entry.size() - 1;
  }
  int append(int id, int status, int mother1, int mother2, int daughter1, 
    int daughter2, int col, int acol, double px, double py, double pz, 
    double e, double m = 0., double scaleIn = 0.) {entry.push_back( 
    Particle(id, status, mother1, mother2, daughter1, daughter2, col, acol, 
    px, py, pz, e, m, scaleIn) ); 
    if (col > maxColTag) maxColTag = col;   
    if (acol > maxColTag) maxColTag = acol;
    return entry.size() - 1;
  }
  int append(int id, int status, int mother1, int mother2, int daughter1, 
    int daughter2, int col, int acol, Vec4 p, double m = 0., 
    double scaleIn = 0.) {entry.push_back( Particle(id, status, mother1, 
    mother2, daughter1, daughter2, col, acol, p, m, scaleIn) ); 
    if (col > maxColTag) maxColTag = col;   
    if (acol > maxColTag) maxColTag = acol;
    return entry.size() - 1;
  }

  // Brief versions of append: no mothers and no daughters.
  int append(int id, int status, int col, int acol, double px, double py, 
    double pz, double e, double m = 0.) {entry.push_back( Particle(id, 
    status, 0, 0, 0, 0, col, acol, px, py, pz, e, m, 0.) ); 
    if (col > maxColTag) maxColTag = col;   
    if (acol > maxColTag) maxColTag = acol;
    return entry.size() - 1;
  }
  int append(int id, int status, int col, int acol, Vec4 p, double m = 0.) 
    {entry.push_back( Particle(id, status, 0, 0, 0, 0, col, acol, p, m, 0.) ); 
    if (col > maxColTag) maxColTag = col;   
    if (acol > maxColTag) maxColTag = acol;
    return entry.size() - 1;
  }

  // Add a copy of an existing particle at the end of the event record.
  int copy(int iCopy, int newStatus = 0);

  // Implement reference "back" to access last element.
  Particle& back() {return entry.back();}

  // List the particles in an event.
  void list(ostream& os = cout) {list(false, false, os);}  
  void list(bool showScaleAndVertex, bool showMothersAndDaughters = false, 
    ostream& os = cout);  

  // Remove last n entries.
  void popBack(int nRemove = 1) { if (nRemove ==1) entry.pop_back();
    else {int newSize = max( 0, size() - nRemove); 
    entry.resize(newSize);} } 

  // Restore all ParticleDataEntry* pointers in the Particle vector.
  // Useful when a persistent copy of the event record is read back in.
  void restorePtrs() { 
    for (int i = 0; i < size(); ++i) entry[i].setParticlePtr(); } 

  // Save or restore the size of the event record (throwing at the end).
  void saveSize() {savedSize = entry.size();}
  void restoreSize() {entry.resize(savedSize);}   

  // Initialize and access colour tag information.
  void initColTag(int colTag = 0) {maxColTag = max( colTag,startColTag);}
  int lastColTag() const {return maxColTag;}
  int nextColTag() {return ++maxColTag;}

  // Access scale for which event as a whole is defined.
  void scale( double scaleIn) {scaleSave = scaleIn;}
  double scale() const {return scaleSave;}

  // Need a second scale if two hard interactions in event.
  void scaleSecond( double scaleSecondIn) {scaleSecondSave = scaleSecondIn;}
  double scaleSecond() const {return scaleSecondSave;}

  // Find complete list of daughters and mothers.
  vector<int> motherList(int i) const;
  vector<int> daughterList(int i) const;
 
  // Trace the first and last copy of one and the same particle.
  int iTopCopy(int i) const;
  int iBotCopy(int i) const;

  // Trace the first and last copy of a particle, using flavour match.
  int iTopCopyId(int i) const;
  int iBotCopyId(int i) const;

  // Find list of sisters, also tracking up and down identical copies.
  vector<int> sisterList(int i) const;
  vector<int> sisterListTopBot(int i, bool widenSearch = true) const;

  // Check whether two particles have a direct mother-daughter relation.
  bool isAncestor(int i, int iAncestor) const;

  // Member functions for rotations and boosts of an event.
  void rot(double theta, double phi) 
    {for (int i = 0; i < size(); ++i) entry[i].rot(theta, phi);} 
  void bst(double betaX, double betaY, double betaZ) 
    {for (int i = 0; i < size(); ++i) entry[i].bst(betaX, betaY, betaZ);}
  void bst(double betaX, double betaY, double betaZ, double gamma) 
    {for (int i = 0; i < size(); ++i) entry[i].bst(betaX, betaY, betaZ, 
    gamma);}
  void bst(const Vec4& vec) 
    {for (int i = 0; i < size(); ++i) entry[i].bst(vec);}
  void rotbst(const RotBstMatrix& M) 
    {for (int i = 0; i < size(); ++i) entry[i].rotbst(M);}

  // Clear the list of junctions.
  void clearJunctions() {junction.resize(0);}
 
  // Add a junction to the list, study it or extra input.
  void appendJunction( int kind, int col0, int col1, int col2)  
    { junction.push_back( Junction( kind, col0, col1, col2) );} 
  void appendJunction(Junction junctionIn) {junction.push_back(junctionIn);} 
  int sizeJunction() const {return junction.size();}
  bool remainsJunction(int i) const {return junction[i].remains();}
  void remainsJunction(int i, bool remainsIn) {junction[i].remains(remainsIn);}
  int kindJunction(int i) const {return junction[i].kind();}
  int colJunction( int i, int j) const {return junction[i].col(j);}
  void colJunction( int i, int j, int colIn) {junction[i].col(j, colIn);}
  int endColJunction( int i, int j) const {return junction[i].endCol(j);}
  void endColJunction( int i, int j, int endColIn) 
    {junction[i].endCol(j, endColIn);}
  int statusJunction( int i, int j) const {return junction[i].status(j);}
  void statusJunction( int i, int j, int statusIn) 
    {junction[i].status(j, statusIn);}
  Junction& getJunction(int i) {return junction[i];}
  const Junction& getJunction(int i) const {return junction[i];}
  void eraseJunction(int i);

  // Save or restore the size of the junction list (throwing at the end).
  void saveJunctionSize() {savedJunctionSize = junction.size();}
  void restoreJunctionSize() {junction.resize(savedJunctionSize);}   

  // List any junctions in the event; for debug mainly.
  void listJunctions(ostream& os = cout) const;

  // Operations with grouped systems of partons for internal use only.
  // (Used by combined MI, ISR, FSR and BR machinery in PartonLevel.)

  // Reset all systems and system number to empty.
  void clearSystems() {beginSys.resize(0); sizeSys.resize(0); 
    memberSys.resize(0);}
  
  // Get number of systems or number of members in a system. 
  int sizeSystems() const {return beginSys.size();}
  int sizeSystem(int iSys) const {return sizeSys[iSys];}

  // New system or new parton in system.
  int newSystem() {beginSys.push_back(memberSys.size()); 
    sizeSys.push_back(0); return (beginSys.size() - 1);}
  void addToSystem(int iSys, int iPos);

  // Get or set value of given member in given system. Replace value by new.
  int getInSystem(int iSys, int iMem) const {
    return memberSys[beginSys[iSys] + iMem];}
  void setInSystem(int iSys, int iMem, int iPos) {
    memberSys[beginSys[iSys] + iMem] = iPos;}
  void replaceInSystem(int iSys, int iPosOld, int iPosNew);

  // List members in systems; for debug mainly.
  void listSystems(ostream& os = cout) const;

  // Operator overloading allows to append one event to an existing one.
  // Warning: particles should be OK, but some other information unreliable.
  Event& operator+=(const Event& addEvent);

private: 

  // Constants: could only be changed in the code itself.
  static const int IPERLINE;

  // Initialization data, normally only set once.
  int startColTag;

  // The event: a vector containing all particles (entries).
  vector<Particle> entry;

  // The list of junctions.
  vector<Junction> junction;

  // The maximum colour tag of the event so far.
  int maxColTag;

  // Saved entry and junction list sizes, for simple restoration.
  int savedSize, savedJunctionSize;

  // The scale of the event; linear quantity in GeV.
  double scaleSave, scaleSecondSave;

  // Header specification in event listing (at most 40 characters wide).
  string headerList;

  // Offsets, sizes and values of systems.
  vector<int> beginSys, sizeSys, memberSys;
  
};

//**************************************************************************

} // end namespace Pythia8

#endif // end Pythia8_Event_H
