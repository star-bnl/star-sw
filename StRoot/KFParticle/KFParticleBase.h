/*
 * This file is part of KFParticle package
 * Copyright (C) 2007-2019 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2019 Goethe University of Frankfurt
 *               2007-2019 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Maksym Zyzak
 *               2007-2019 Sergey Gorbunov
 *
 * KFParticle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * KFParticle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */


#ifndef KFPARTICLEBASE_H
#define KFPARTICLEBASE_H

#ifdef __ROOT__ //for the STAR experiment
#define HomogeneousField
#endif

#ifdef HLTCA_STANDALONE
#include "RootTypesDef.h"
#else
#include "TObject.h"
#endif

#include <vector>

/** @class KFParticleBase
 ** @brief The base of KFParticle class, describes particle objects.
 ** @author  S.Gorbunov, I.Kisel, M.Zyzak
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** Contains the main mathematics of the KF Particle . Will be merged with the KFParticle class.
 **/

class KFParticleBase :public TObject {
  
 public:

  /**
   ** Abstract methods are defined in the KFParticle class
   **/ 

  /** Virtual method to access the magnetic field**/
  virtual void GetFieldValue(const float xyz[], float B[]) const = 0;
  
  /** Virtual method to get extrapolation parameter dS=l/p to . Is defined in KFParticle.**/
  virtual float GetDStoPoint( const float xyz[3], float dsdr[6] ) const = 0;
  
  float GetDStoPointLine( const float xyz[3], float dsdr[6] ) const;
  float GetDStoPointBz( float B, const float xyz[3], float dsdr[6], const float* param=0) const;
  float GetDStoPointBy( float By, const float xyz[3], float dsdr[6] ) const;
  float GetDStoPointB( const float* B, const float xyz[3], float dsdr[6] ) const;
  float GetDStoPointCBM( const float xyz[3], float dsdr[6] ) const;

  /** Virtual method to get extrapolation parameter dS=l/p to another particle. Is defined in KFParticle.**/
  virtual void GetDStoParticle( const KFParticleBase &p, float dS[2], float dsdr[4][6] ) const = 0;
  
  void GetDStoParticleLine( const KFParticleBase &p, float dS[2], float dsdr[4][6] ) const ;
  void GetDStoParticleBz( float Bz, const KFParticleBase &p, float dS[2], float dsdr[4][6], const float* param1=0, const float* param2=0 ) const ;
  void GetDStoParticleBy( float B,  const KFParticleBase &p, float dS[2], float dsdr[4][6] ) const ;
  void GetDStoParticleCBM( const KFParticleBase &p, float dS[2], float dsdr[4][6] ) const ;
  
  /** Virtual method to transport a particle on a certain distance along the trajectory. Is defined in KFParticle.**/
  virtual void Transport( float dS, const float dsdr[6], float P[], float C[], float* dsdr1=0, float* F=0, float* F1=0 ) const = 0;


  KFParticleBase();
  virtual ~KFParticleBase() { ; } ///< The default destructor.

  void Initialize( const float Param[], const float Cov[], Int_t Charge, float Mass );
  void Initialize();

  void SetConstructMethod(Int_t m) {fConstructMethod = m;} ///< Defines the construction method for the current particle (see description of fConstructMethod).
  void SetMassHypo(float m) { fMassHypo = m;} ///< Sets the mass hypothesis to the particle, is used when fConstructMethod = 2.
  const float& GetMassHypo() const { return fMassHypo; } ///< Returns the mass hypothesis.
  const float& GetSumDaughterMass() const {return SumDaughterMass;} ///< Returns the sum of masses of the daughters.

  //*
  //*  ACCESSORS
  //*

  //* Simple accessors 

  float GetX    () const { return fP[0]; } ///< Retruns X coordinate of the particle, fP[0].
  float GetY    () const { return fP[1]; } ///< Retruns Y coordinate of the particle, fP[1].
  float GetZ    () const { return fP[2]; } ///< Retruns Z coordinate of the particle, fP[2].
  float GetPx   () const { return fP[3]; } ///< Retruns X component of the momentum, fP[3].
  float GetPy   () const { return fP[4]; } ///< Retruns Y component of the momentum, fP[4].
  float GetPz   () const { return fP[5]; } ///< Retruns Z component of the momentum, fP[5].
  float GetE    () const { return fP[6]; } ///< Returns energy of the particle, fP[6].
  float GetS    () const { return fP[7]; } ///< Returns dS=l/p, l - decay length, fP[7], defined if production vertex is set.
  char  GetQ    () const { return fQ;    } ///< Returns charge of the particle.
  float GetChi2 () const { return fChi2; } ///< Returns Chi2 of the fit.
  Int_t GetNDF  () const { return fNDF;  } ///< Returns number of decrease of freedom.

  const float& X    () const { return fP[0]; } ///< Retruns X coordinate of the particle, fP[0].
  const float& Y    () const { return fP[1]; } ///< Retruns Y coordinate of the particle, fP[1].
  const float& Z    () const { return fP[2]; } ///< Retruns Z coordinate of the particle, fP[2].
  const float& Px   () const { return fP[3]; } ///< Retruns X component of the momentum, fP[3].
  const float& Py   () const { return fP[4]; } ///< Retruns Y component of the momentum, fP[4].
  const float& Pz   () const { return fP[5]; } ///< Retruns Z component of the momentum, fP[5].
  const float& E    () const { return fP[6]; } ///< Returns energy of the particle, fP[6].
  const float& S    () const { return fP[7]; } ///< Returns dS=l/p, l - decay length, fP[7], defined if production vertex is set.
  const char& Q     () const { return fQ;    } ///< Returns charge of the particle.
  const float& Chi2 () const { return fChi2; } ///< Returns Chi2 of the fit.
  const Int_t& NDF  () const { return fNDF;  } ///< Returns number of decrease of freedom.
  
  float GetParameter ( Int_t i )          const { return fP[i];       } ///< Returns P[i] parameter.
  float GetCovariance( Int_t i )          const { return fC[i];       } ///< Returns C[i] element of the covariance matrix in the lower triangular form.
  float GetCovariance( Int_t i, Int_t j ) const { return fC[IJ(i,j)]; } ///< Returns C[i,j] element of the covariance matrix.

  //* Accessors with calculations( &value, &estimated sigma )
  //* error flag returned (0 means no error during calculations) 

  Int_t GetMomentum      ( float &p,   float &error ) const ;
  Int_t GetPt            ( float &pt,  float &error ) const ;
  Int_t GetEta           ( float &eta, float &error ) const ;
  Int_t GetPhi           ( float &phi, float &error ) const ;
  Int_t GetMass          ( float &m,   float &error ) const ;
  Int_t GetDecayLength   ( float &l,   float &error ) const ;
  Int_t GetDecayLengthXY ( float &l,   float &error ) const ;
  Int_t GetLifeTime      ( float &ctau,float &error ) const ;
  Int_t GetR             ( float &r,   float &error ) const ;

  //*
  //*  MODIFIERS
  //*
  
  float & X    () { return fP[0]; } ///< Modifier of X coordinate of the particle, fP[0].
  float & Y    () { return fP[1]; } ///< Modifier of Y coordinate of the particle, fP[1].
  float & Z    () { return fP[2]; } ///< Modifier of Z coordinate of the particle, fP[2].
  float & Px   () { return fP[3]; } ///< Modifier of X component of the momentum, fP[3].
  float & Py   () { return fP[4]; } ///< Modifier of Y component of the momentum, fP[4].
  float & Pz   () { return fP[5]; } ///< Modifier of Z component of the momentum, fP[5].
  float & E    () { return fP[6]; } ///< Modifier of energy of the particle, fP[6].
  float & S    () { return fP[7]; } ///< Modifier of dS=l/p, l - decay length, fP[7], defined if production vertex is set.
  char  & Q    () { return fQ;    } ///< Modifier of charge of the particle.
  float & Chi2 () { return fChi2; } ///< Modifier of Chi2 of the fit.
  Int_t & NDF  () { return fNDF;  } ///< Modifier of number of decrease of freedom.

  float & Parameter ( Int_t i )          { return fP[i];       } ///< Modifier of P[i] parameter.
  float & Covariance( Int_t i )          { return fC[i];       } ///< Modifier of C[i] element of the covariance matrix in the lower triangular form.
  float & Covariance( Int_t i, Int_t j ) { return fC[IJ(i,j)]; } ///< Modifier of C[i,j] element of the covariance matrix.


  //* 
  //* CONSTRUCTION OF THE PARTICLE BY ITS DAUGHTERS AND MOTHER
  //* USING THE KALMAN FILTER METHOD
  //*


  //* Simple way to add daughter ex. D0+= Pion; 

  void operator +=( const KFParticleBase &Daughter );  

  //* Add daughter track to the particle 

  void AddDaughter( const KFParticleBase &Daughter );
  void SubtractDaughter( const KFParticleBase &Daughter );

  void AddDaughterWithEnergyFit( const KFParticleBase &Daughter );
  void AddDaughterWithEnergyFitMC( const KFParticleBase &Daughter );

  //* Set production vertex 

  void SetProductionVertex( const KFParticleBase &Vtx );

  //* Set mass constraint 

  void SetNonlinearMassConstraint( float Mass );
  void SetMassConstraint( float Mass, float SigmaMass = 0 );

  //* Set no decay length for resonances

  void SetNoDecayLength();


  //* Everything in one go  

  void Construct( const KFParticleBase *vDaughters[], Int_t nDaughters, const KFParticleBase *ProdVtx=0,   float Mass=-1 );

  //Transport functions
  void TransportToDecayVertex();
  void TransportToProductionVertex();
  void TransportToDS( float dS, const float* dsdr );
  void TransportBz( float Bz, float dS, const float* dsdr, float P[], float C[], float* dsdr1=0, float* F=0, float* F1=0 ) const;
  void TransportCBM( float dS, const float* dsdr, float P[], float C[], float* dsdr1=0, float* F=0, float* F1=0 ) const;  

  //* 
  //* OTHER UTILITIES
  //*

  //* Calculate distance from another object [cm]

  float GetDistanceFromVertex( const float vtx[] ) const;
  float GetDistanceFromVertex( const KFParticleBase &Vtx ) const;
  float GetDistanceFromParticle( const KFParticleBase &p ) const;

  //* Calculate sqrt(Chi2/ndf) deviation from vertex
  //* v = [xyz], Cv=[Cxx,Cxy,Cyy,Cxz,Cyz,Czz]-covariance matrix

  float GetDeviationFromVertex( const float v[], const float Cv[]=0 ) const;
  float GetDeviationFromVertex( const KFParticleBase &Vtx ) const;
  float GetDeviationFromParticle( const KFParticleBase &p ) const;  

  void SubtractFromVertex( KFParticleBase &Vtx ) const;
  void SubtractFromParticle( KFParticleBase &Vtx ) const;

  static void GetArmenterosPodolanski(KFParticleBase& positive, KFParticleBase& negative, float QtAlfa[2] );
  void RotateXY(float angle, float Vtx[3]);

  int Id() const { return fId; } ///< Returns Id of the particle.
  int NDaughters() const { return fDaughtersIds.size(); } ///< Returns number of daughter particles.
  const std::vector<int>& DaughterIds() const { return fDaughtersIds; } ///< Returns the vector with the indices of daughter particles.
  void CleanDaughtersId() { fDaughtersIds.clear(); } ///< Cleans the vector with the indices of daughter particles.
  
  void SetId( int id ) { fId = id; } ///< Sets the Id of the particle. After the construction of a particle should be set by user.
  void AddDaughterId( int id ) { fDaughtersIds.push_back(id); } ///< Adds index of the daughter particle. 

  void SetPDG ( int pdg ) { fPDG = pdg; } ///< Sets the PDG hypothesis.
  int GetPDG () const { return fPDG; } ///< Returns the PDG hypothesis.

#ifdef __ROOT__ //for the STAR experiment
  virtual void Print(Option_t *opt="") const;
  Int_t        IdTruth() const { return fIdTruth;}
  Int_t        QaTruth() const { return fQuality; }
  Int_t        IdParentMcVx() const {return fIdParentMcVx;}
  Int_t        IdParentVx()   const {return IdParentMcVx();}
  void         SetParentID(Int_t id=0) {fParentID = id;}
  Int_t        GetParentID() const {return fParentID;}
  void         SetIdParentMcVx(Int_t id) {fIdParentMcVx = id;}
  void         SetIdTruth(Int_t idtru,Int_t qatru=0) {fIdTruth = (UShort_t) idtru; fQuality = (UShort_t) qatru;}
  virtual void Clear(Option_t * /*option*/ ="");
#endif

  static void InvertCholetsky3(float a[6]);
  static void MultQSQt( const float Q[], const float S[], float SOut[], const int kN );

 protected:
  /** Converts a pair of indices {i,j} of the covariance matrix to one index corresponding to the triangular form. */
  static Int_t IJ( Int_t i, Int_t j ){ 
    return ( j<=i ) ? i*(i+1)/2+j :j*(j+1)/2+i;
  }
  /** Return an element of the covariance matrix with {i,j} indices. */
  float & Cij( Int_t i, Int_t j ){ return fC[IJ(i,j)]; }
  void TransportLine( float S, const float* dsdr, float P[], float C[], float* dsdr1, float* F, float* F1 ) const ;
  bool GetMeasurement( const KFParticleBase& daughter, float m[], float V[], float D[3][3] ) ;
  void SetMassConstraint( float *mP, float *mC, float mJ[7][7], float mass );

  float fP[8];           ///< Particle parameters { X, Y, Z, Px, Py, Pz, E, S[=DecayLength/P]}.
  float fC[36];          ///< Low-triangle covariance matrix of fP.
  float fChi2;           ///< Chi^2.
  float fSFromDecay;     ///< Distance from the decay vertex to the current position.
  float SumDaughterMass; ///< Sum of the daughter particles masses. Needed to set the constraint on the minimum mass during particle construction.
  float fMassHypo;       ///< The mass hypothesis, used for the constraints during particle construction.
  Int_t fNDF;            ///< Number of degrees of freedom.
  int   fId;             ///< Id of the particle.
#ifdef __ROOT__ //for the STAR experiment
  Short_t    fParentID;     ///< Id of the parent particle.
  Short_t    fIdTruth;      ///< MC track id.
  Short_t    fQuality;      ///< quality of this information (percentage of hits coming from the above MC track).
  Short_t    fIdParentMcVx; ///< for track and McTrack for vertex.
#endif
  Bool_t fAtProductionVertex; ///< Flag shows if particle is at the production point.
  char fQ; ///< The charge of the particle in the units of the elementary charge.
  
  /** \brief Determines the method for the particle construction. \n
   ** 0 - Energy considered as an independent veriable, fitted independently from momentum, without any constraints on mass \n
   ** 2 - Energy considered as an independent variable, fitted independently from momentum, with constraints on mass of daughter particle
   **/
  char fConstructMethod; 
  int fPDG; ///< The PDG hypothesis assigned to the particle.
  
  /** \brief A vector with ids of the daughter particles: \n
   ** 1) if particle is created from a track - the index of the track, in this case the size of the vector is always equal to one; \n
   ** 2) if particle is constructed from other particles - indices of these particles in the same array.
   **/
  std::vector<int> fDaughtersIds;
 
#ifndef KFParticleStandalone
  ClassDef( KFParticleBase, 3 )
#endif
};

#ifdef __ROOT__ //for the STAR experiment
std::ostream&  operator<<(std::ostream& os, KFParticleBase const & particle);
#endif

#endif 
