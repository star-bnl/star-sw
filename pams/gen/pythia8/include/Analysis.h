// Analysis.h is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Header file for the Sphericity, Thrust, ClusterJet and CellJet classes.
// Sphericity: sphericity analysis of the event.
// Thrust: thrust analysis of the event.
// ClusterJet: clustering jet finder.
// CellJet: calorimetric cone jet finder. 

#ifndef Pythia8_Analysis_H
#define Pythia8_Analysis_H

#include "Basics.h"
#include "Event.h"
#include "PythiaStdlib.h"

namespace Pythia8 {

//**************************************************************************

// Sphericity class.
// This class performs (optionally modified) sphericity analysis on an event.

class Sphericity {

public: 

  // Constructor.
  Sphericity(double powerIn = 2., int selectIn = 2) : power(powerIn), 
    select(selectIn), nFew(0), nBack(0) {powerInt = 0; 
    if (abs(power - 1.) < 0.01) powerInt = 1;
    if (abs(power - 2.) < 0.01) powerInt = 2; 
    powerMod = 0.5 * power - 1.;}
  
  // Analyze event.
  bool analyze(const Event& event, ostream& os = cout);

  // Return info on results of analysis.
  double sphericity()      const {return 1.5 * (eVal2 + eVal3);}
  double aplanarity()      const {return 1.5 * eVal3;}
  double eigenValue(int i) const {return (i < 2) ? eVal1 :
    ( (i < 3) ? eVal2 : eVal3 ) ;}
  Vec4 eventAxis(int i)    const {return (i < 2) ? eVec1 :
    ( (i < 3) ? eVec2 : eVec3 ) ;}

  // Provide a listing of the info.
  void list(ostream& os = cout);

  // Tell how many events could not be analyzed.
  int nError() const {return nFew + nBack;}

private: 

  // Constants: could only be changed in the code itself.
  static const int    NSTUDYMIN, TIMESTOPRINT;
  static const double P2MIN, EIGENVALUEMIN;

  // Properties of analysis.
  double power;
  int    select, powerInt; 
  double powerMod;

  // Outcome of analysis.
  double eVal1, eVal2, eVal3; 
  Vec4   eVec1, eVec2, eVec3; 

  // Error statistics;
  int    nFew, nBack;

};  

//**************************************************************************

// Thrust class.
// This class performs thrust analysis on an event.

class Thrust {

public: 

  // Constructor.
  Thrust(int selectIn = 2) : select(selectIn), nFew(0) {}
  
  // Analyze event.
  bool analyze(const Event& event, ostream& os = cout);

  // Return info on results of analysis.
  double thrust()       const {return eVal1;}
  double tMajor()       const {return eVal2;}
  double tMinor()       const {return eVal3;}
  double oblateness()   const {return eVal2 - eVal3;}
  Vec4 eventAxis(int i) const {return (i < 2) ? eVec1 :
    ( (i < 3) ? eVec2 : eVec3 ) ;}

  // Provide a listing of the info.
  void list(ostream& os = cout);

  // Tell how many events could not be analyzed.
  int nError() const {return nFew;}

private: 

  // Constants: could only be changed in the code itself.
  static const int    NSTUDYMIN, TIMESTOPRINT;
  static const double MAJORMIN;

  // Properties of analysis.
  int    select; 

  // Outcome of analysis.
  double eVal1, eVal2, eVal3; 
  Vec4   eVec1, eVec2, eVec3; 

  // Error statistics;
  int    nFew;

};  

//**************************************************************************

// SingleClusterJet class.
// Simple helper class to ClusterJet for a jet and its contents. 

class SingleClusterJet {

public:

  // Constructors.
  SingleClusterJet(Vec4 pJetIn = 0., int motherIn = 0) : 
    pJet(pJetIn), mother(motherIn), daughter(0), multiplicity(1),    
    isAssigned(false) {pAbs = max( PABSMIN, pJet.pAbs());}
  SingleClusterJet& operator=(const SingleClusterJet& j) { if (this != &j)
    { pJet = j.pJet;  mother = j.mother; daughter = j.daughter; 
    multiplicity = j.multiplicity; pAbs = j.pAbs;
    isAssigned = j.isAssigned;} return *this; }

  // Properties of jet.
  // Note: mother, daughter and isAssigned only used for original 
  // particles, multiplicity and pTemp only for reconstructed jets.
  Vec4   pJet; 
  int    mother, daughter, multiplicity;
  bool   isAssigned;
  double pAbs; 
  Vec4   pTemp; 

  // Distance measures (Lund, JADE, Durham) with friend.
  friend double dist2Fun(int measure, const SingleClusterJet& j1, 
    const SingleClusterJet& j2);  

private: 

  // Constants: could only be changed in the code itself.
  static const double PABSMIN;

} ;

//**************************************************************************

// ClusterJet class.
// This class performs a jet clustering according to different
// distance measures: Lund, JADE or Durham.

class ClusterJet {

public: 

  // Constructor.
  ClusterJet(string measureIn = "Lund", int selectIn = 2, int massSetIn = 2, 
    bool preclusterIn = false, bool reassignIn = false) : measure(1), 
    select(selectIn), massSet(massSetIn), doPrecluster(preclusterIn), 
    doReassign(reassignIn), nFew(0) {
    char firstChar = toupper(measureIn[0]);
    if (firstChar == 'J') measure = 2;
    if (firstChar == 'D') measure = 3; 
    piMass = ParticleDataTable::m0(211);
  }
      
  // Analyze event.
  bool analyze(const Event& event, double yScaleIn, double pTscaleIn, 
    int nJetMinIn = 1, int nJetMaxIn = 0, ostream& os = cout);

  // Return info on jets produced.
  int    size() const {return jets.size();}
  Vec4 p(int j) const {return jets[j].pJet;}

  // Return belonging of particle to one of the jets (-1 if none).
  int jetAssignment(int i) const {
    for (int iP = 0; iP < int(particles.size()); ++iP)
    if (particles[iP].mother == i) return particles[iP].daughter;
    return -1;} 

  // Provide a listing of the info.
  void list(ostream& os = cout);

  // Tell how many events could not be analyzed.
  int nError() const {return nFew;}

private: 

  // Constants: could only be changed in the code itself.
  static const int    TIMESTOPRINT;
  static const double PABSMIN, PRECLUSTERFRAC, PRECLUSTERSTEP;

  // Properties of analysis.
  int    measure, select, massSet; 
  bool   doPrecluster, doReassign;
  double yScale, pTscale;
  int    nJetMin, nJetMax; 

  // Temporary results.
  double piMass, dist2Join, dist2BigMin, distPre, dist2Pre;
  vector<SingleClusterJet> particles;
  int    nParticles;

  // Error statistics;
  int    nFew;

  // Member functions for some operations (for clarity).
  void precluster();
  void reassign();

  // Outcome of analysis: ET-ordered list of jets. 
  vector<SingleClusterJet> jets;

};  

//**************************************************************************

// SingleCell class.
// Simple helper class to CellJet for a cell and its contents. 

class SingleCell {

public:

  // Constructor.
  SingleCell(int iCellIn = 0, double etaCellIn = 0., double phiCellIn = 0., 
    double eTcellIn = 0., int multiplicityIn = 0) : iCell(iCellIn), 
    etaCell(etaCellIn), phiCell(phiCellIn), eTcell(eTcellIn), 
    multiplicity(multiplicityIn), canBeSeed(true), isUsed(false),
    isAssigned(false) {}

  // Properties of cell.
  int    iCell;
  double etaCell, phiCell, eTcell;
  int    multiplicity;
  bool   canBeSeed, isUsed, isAssigned;

} ;

//**************************************************************************

// SingleCellJet class.
// Simple helper class to CellJet for a jet and its contents. 

class SingleCellJet {

public:

  // Constructor.
  SingleCellJet(double eTjetIn = 0., double etaCenterIn = 0., 
    double phiCenterIn = 0., double etaWeightedIn = 0.,
    double phiWeightedIn = 0., int multiplicityIn = 0,
    Vec4 pMassiveIn = 0.) : eTjet(eTjetIn), etaCenter(etaCenterIn), 
    phiCenter(phiCenterIn), etaWeighted(etaWeightedIn), 
    phiWeighted(phiWeightedIn), multiplicity(multiplicityIn),
    pMassive(pMassiveIn) {}

  // Properties of jet.
  double eTjet, etaCenter, phiCenter, etaWeighted, phiWeighted;
  int    multiplicity;
  Vec4   pMassive;  

} ;

//**************************************************************************

// CellJet class.
// This class performs a cone jet search in (eta, phi, E_T) space.

class CellJet {

public: 

  // Constructor.
  CellJet(double etaMaxIn = 5., int nEtaIn = 50, int nPhiIn = 32, 
    int selectIn = 2, int smearIn = 0, double resolutionIn = 0.5, 
    double upperCutIn = 2., double thresholdIn = 0.) : etaMax(etaMaxIn), 
    nEta(nEtaIn), nPhi(nPhiIn), select(selectIn), smear(smearIn),
    resolution(resolutionIn), upperCut(upperCutIn), 
    threshold(thresholdIn), nFew(0) { }
  
  // Analyze event.
  bool analyze(const Event& event, double eTjetMinIn = 20., 
    double coneRadiusIn = 0.7, double eTseedIn = 1.5, ostream& os = cout);

  // Return info on results of analysis.
  int    size()              const {return jets.size();}
  double eT(int i)           const {return jets[i].eTjet;}
  double etaCenter(int i)    const {return jets[i].etaCenter;}
  double phiCenter(int i)    const {return jets[i].phiCenter;}
  double etaWeighted(int i)  const {return jets[i].etaWeighted;}
  double phiWeighted(int i)  const {return jets[i].phiWeighted;}
  double multiplicity(int i) const {return jets[i].multiplicity;}
  Vec4 pMassless(int i)      const {return jets[i].eTjet * Vec4(
           cos(jets[i].phiWeighted),  sin(jets[i].phiWeighted),
          sinh(jets[i].etaWeighted), cosh(jets[i].etaWeighted) );}
  Vec4 pMassive(int i)       const {return jets[i].pMassive;}
  double m(int i)            const {return jets[i].pMassive.mCalc();}

  // Provide a listing of the info.
  void list(ostream& os = cout);

  // Tell how many events could not be analyzed: so far never.
  int nError() const {return nFew;}

private: 

  // Constants: could only be changed in the code itself.
  static const int    TIMESTOPRINT;

  // Properties of analysis.
  double etaMax; 
  int    nEta, nPhi, select, smear;
  double resolution, upperCut, threshold;
  double eTjetMin, coneRadius, eTseed; 

  // Error statistics;
  int    nFew;

  // Outcome of analysis: ET-ordered list of jets. 
  vector<SingleCellJet> jets;

};  

//**************************************************************************

} // end namespace Pythia8

#endif // end Pythia8_Analysis_H

