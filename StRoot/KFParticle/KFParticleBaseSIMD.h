/*
 * This file is part of KFParticle package
 * Copyright (C) 2007-2019 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2019 Goethe University of Frankfurt
 *               2007-2019 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Maksym Zyzak
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


#ifndef KFParticleBaseSIMD_H
#define KFParticleBaseSIMD_H

#ifdef HLTCA_STANDALONE
#include "RootTypesDef.h"
#endif

#include <vector>
#include "KFParticleDef.h"

/** @class KFParticleBaseSIMD
 ** @brief The base of KFParticleSIMD class.
 ** @author  M.Zyzak
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** Contains the main mathematics of the vectorised KF Particle. Will be merged with the KFParticleSIMD class. 
 ** The class stores all the data in a format of SIMD vectors, the mathematics is fully vectorised.
 ** THe mathematics is implemented in single precision.
 ** The functionality of the vectorised and scalar classes is the same.
 **/

class KFParticleBaseSIMD {
  
 public:
                  
  //*
  //* ABSTRACT METHODS HAVE TO BE DEFINED IN USER CLASS 
  //* 
#if  __GNUC__ && ( defined(ENVIRONMENT32) ||  GCC_VERSION < 40300 )

#else
                                            
  void *operator new(size_t size) { return _mm_malloc(size, sizeof(float_v)); }     ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new[](size_t size) { return _mm_malloc(size, sizeof(float_v)); }   ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new(size_t size, void *ptr) { return ::operator new(size, ptr);}   ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new[](size_t size, void *ptr) { return ::operator new(size, ptr);} ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void operator delete(void *ptr, size_t) { _mm_free(ptr); }                        ///< delete operator for the SIMD-alligned dynamic memory release
  void operator delete[](void *ptr, size_t) { _mm_free(ptr); }                      ///< delete operator for the SIMD-alligned dynamic memory release
 
#endif 
                  
  /** Virtual method to access the magnetic field**/
  virtual void GetFieldValue(const float_v xyz[], float_v B[]) const = 0;
  
  //* Virtual methods needed for particle transportation 
  //* One can use particular implementations for collider (only Bz component) 
  //* geometry and for fixed-target (CBM-like) geometry which are provided below 
  //* in TRANSPORT section
 
  //* Get dS to xyz[] space point 

   /** Virtual method to get extrapolation parameter dS=l/p to . Is defined in KFParticleSIMD.**/
  virtual float_v GetDStoPoint( const float_v xyz[3], float_v dsdr[6] ) const = 0;
  
  float_v GetDStoPointLine( const float_v xyz[3], float_v dsdr[6] ) const;
  float_v GetDStoPointBz( float_v Bz, const float_v xyz[3], float_v dsdr[6], const float_v* param = 0 ) const;
  float_v GetDStoPointBy( float_v By, const float_v xyz[3], float_v dsdr[6] ) const;
  float_v GetDStoPointCBM( const float_v xyz[3], float_v dsdr[6] ) const;

  /** Virtual method to get extrapolation parameter dS=l/p and ds/dr partial derivatives to another particle. Is defined in KFParticleSIMD.**/
  virtual void GetDStoParticle( const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] ) const = 0;
  /** Virtual method to get extrapolation parameter dS=l/p to another particle. Is defined in KFParticleSIMD.**/
  virtual void GetDStoParticleFast( const KFParticleBaseSIMD &p, float_v dS[2] ) const = 0;
  
  void GetDStoParticleLine( const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] ) const ;
  void GetDStoParticleLine( const KFParticleBaseSIMD &p, float_v dS[2]  ) const ;
  void GetDStoParticleBz( float_v Bz, const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6], const float_v* param1 =0, const float_v* param2 =0  ) const ;
  void GetDStoParticleBz( float_v Bz, const KFParticleBaseSIMD &p, float_v dS[2], const float_v* param1 =0, const float_v* param2 =0  ) const ;
  void GetDStoParticleBy( float_v B, const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] ) const ;
  void GetDStoParticleBy( float_v B, const KFParticleBaseSIMD &p, float_v dS[2] ) const ;
  void GetDStoParticleB( float_v B[3], const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] ) const;
  void GetDStoParticleB( float_v B[3], const KFParticleBaseSIMD &p, float_v dS[2] ) const;
  void GetDStoParticleCBM( const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] ) const ;
  void GetDStoParticleCBM( const KFParticleBaseSIMD &p, float_v dS[2] ) const ;
  
  /** Virtual method to transport a particle parameters and covariance matrix on a certain distance along the trajectory. Is defined in KFParticleSIMD.**/
  virtual void Transport( float_v dS, const float_v dsdr[6], float_v P[], float_v C[], float_v* dsdr1=0, float_v* F=0, float_v* F1=0 ) const = 0;
  /** Virtual method to transport a particle parameters on a certain distance along the trajectory. Is defined in KFParticleSIMD.**/
  virtual void TransportFast( float_v dS, float_v P[] ) const = 0;



  //*
  //*  INITIALIZATION
  //*

  //* Constructor 

  KFParticleBaseSIMD();

  //* Destructor 

  virtual ~KFParticleBaseSIMD() { ; } ///< The default destructor.

  void Initialize( const float_v Param[], const float_v Cov[], int_v Charge, float_v Mass );
  void Initialize();

  void SetConstructMethod(Int_t m) {fConstructMethod = m;} ///< Defines the construction method for the current particle (see description of fConstructMethod).
  void SetMassHypo(float_v m) { fMassHypo = m;} ///< Sets the mass hypothesis to the particle, is used when fConstructMethod = 2.
  const float_v& GetMassHypo() const { return fMassHypo; } ///< Returns the mass hypothesis.
  const float_v& GetSumDaughterMass() const {return SumDaughterMass;} ///< Returns the sum of masses of the daughters.

  //*
  //*  ACCESSORS
  //*

  //* Simple accessors 

  float_v GetX    () const { return fP[0]; } ///< Returns the sum of masses of the daughters
  float_v GetY    () const { return fP[1]; } ///< Returns the sum of masses of the daughters
  float_v GetZ    () const { return fP[2]; } ///< Returns the sum of masses of the daughters
  float_v GetPx   () const { return fP[3]; } ///< Returns the sum of masses of the daughters
  float_v GetPy   () const { return fP[4]; } ///< Returns the sum of masses of the daughters
  float_v GetPz   () const { return fP[5]; } ///< Returns the sum of masses of the daughters
  float_v GetE    () const { return fP[6]; } ///< Returns the sum of masses of the daughters
  float_v GetS    () const { return fP[7]; } ///< Returns the sum of masses of the daughters
  int_v   GetQ    () const { return fQ;    } ///< Returns the sum of masses of the daughters
  float_v GetChi2 () const { return fChi2; } ///< Returns the sum of masses of the daughters
  int_v GetNDF    () const { return fNDF;  } ///< Returns the sum of masses of the daughters

  const float_v& X    () const { return fP[0]; } ///< Retruns X coordinate of the particle, fP[0].
  const float_v& Y    () const { return fP[1]; } ///< Retruns Y coordinate of the particle, fP[1].
  const float_v& Z    () const { return fP[2]; } ///< Retruns Z coordinate of the particle, fP[2].
  const float_v& Px   () const { return fP[3]; } ///< Retruns X component of the momentum, fP[3].
  const float_v& Py   () const { return fP[4]; } ///< Retruns Y component of the momentum, fP[4].
  const float_v& Pz   () const { return fP[5]; } ///< Retruns Z component of the momentum, fP[5].
  const float_v& E    () const { return fP[6]; } ///< Returns energy of the particle, fP[6].
  const float_v& S    () const { return fP[7]; } ///< Returns dS=l/p, l - decay length, fP[7], defined if production vertex is set.
  const int_v&   Q    () const { return fQ;    } ///< Returns charge of the particle.
  const float_v& Chi2 () const { return fChi2; } ///< Returns Chi2 of the fit.
  const int_v& NDF    () const { return fNDF;  } ///< Returns number of decrease of freedom.
  
  float_v GetParameter ( Int_t i )          const { return fP[i];       } ///< Returns P[i] parameter.
  float_v GetCovariance( Int_t i )          const { return fC[i];       } ///< Returns C[i] element of the covariance matrix in the lower triangular form.
  float_v GetCovariance( Int_t i, Int_t j ) const { return fC[IJ(i,j)]; } ///< Returns C[i,j] element of the covariance matrix.

  //* Accessors with calculations( &value, &estimated sigma )
  //* error flag returned (0 means no error during calculations) 

  float_m GetMomentum      ( float_v &p,    float_v &error ) const ;
  float_m GetPt            ( float_v &pt,   float_v &error ) const ;
  float_m GetEta           ( float_v &eta,  float_v &error ) const ;
  float_m GetPhi           ( float_v &phi,  float_v &error ) const ;
  float_m GetMass          ( float_v &m,    float_v &error ) const ;
  float_m GetDecayLength   ( float_v &l,    float_v &error ) const ;
  float_m GetDecayLengthXY ( float_v &l,    float_v &error ) const ;
  float_m GetLifeTime      ( float_v &ctau, float_v &error ) const ;
  float_m GetR             ( float_v &r,    float_v &error ) const ;

  //*
  //*  MODIFIERS
  //*
  
  float_v & X    () { return fP[0]; } ///< Modifier of X coordinate of the particle, fP[0].
  float_v & Y    () { return fP[1]; } ///< Modifier of Y coordinate of the particle, fP[1].
  float_v & Z    () { return fP[2]; } ///< Modifier of Z coordinate of the particle, fP[2].
  float_v & Px   () { return fP[3]; } ///< Modifier of X component of the momentum, fP[3].
  float_v & Py   () { return fP[4]; } ///< Modifier of Y component of the momentum, fP[4].
  float_v & Pz   () { return fP[5]; } ///< Modifier of Z component of the momentum, fP[5].
  float_v & E    () { return fP[6]; } ///< Modifier of energy of the particle, fP[6].
  float_v & S    () { return fP[7]; } ///< Modifier of dS=l/p, l - decay length, fP[7], defined if production vertex is set.
  int_v   & Q    () { return fQ;    } ///< Modifier of charge of the particle.
  float_v & Chi2 () { return fChi2; } ///< Modifier of Chi2 of the fit.
  int_v & NDF    () { return fNDF;  } ///< Modifier of number of decrease of freedom.

  float_v & Parameter ( Int_t i )          { return fP[i];       } ///< Modifier of P[i] parameter.
  float_v & Covariance( Int_t i )          { return fC[i];       } ///< Modifier of C[i] element of the covariance matrix in the lower triangular form.
  float_v & Covariance( Int_t i, Int_t j ) { return fC[IJ(i,j)]; } ///< Modifier of C[i,j] element of the covariance matrix.


  //* 
  //* CONSTRUCTION OF THE PARTICLE BY ITS DAUGHTERS AND MOTHER
  //* USING THE KALMAN FILTER METHOD
  //*


  //* Simple way to add daughter ex. D0+= Pion; 

  void operator +=( const KFParticleBaseSIMD &Daughter );  

  //* Add daughter track to the particle 

  void AddDaughter( const KFParticleBaseSIMD &Daughter );
  void SubtractDaughter( const KFParticleBaseSIMD &Daughter );

  void AddDaughterWithEnergyFit( const KFParticleBaseSIMD &Daughter );
  void AddDaughterWithEnergyFitMC( const KFParticleBaseSIMD &Daughter ); 
  //with mass constrained

  //* Set production vertex 

  void SetProductionVertex( const KFParticleBaseSIMD &Vtx );

  //* Set mass constraint 

  void SetNonlinearMassConstraint( float_v Mass );
  void SetMassConstraint( float_v Mass, float_v SigmaMass = float_v(0.f) );

  //* Set no decay length for resonances

  void SetNoDecayLength();


  //* Everything in one go  

  void Construct( const KFParticleBaseSIMD *vDaughters[], Int_t nDaughters, const KFParticleBaseSIMD *ProdVtx=0,   Float_t Mass=-1 );


  //*
  //*                   TRANSPORT
  //* 
  //*  ( main transportation parameter is S = SignedPath/Momentum )
  //*  ( parameters of decay & production vertices are stored locally )
  //*


  //* Transport the particle to its decay vertex 

  void TransportToDecayVertex();

  //* Transport the particle to its production vertex 

  void TransportToProductionVertex();

  //* Transport the particle on dS parameter (SignedPath/Momentum) 

  void TransportToDS( float_v dS, const float_v* dsdr );
  void TransportToDSLine( float_v dS, const float_v* dsdr );
  //* Particular extrapolators one can use 
  void TransportBz( float_v Bz, float_v dS, const float_v* dsdr, float_v P[], float_v C[], float_v* dsdr1=0, float_v* F=0, float_v* F1=0 ) const;
  void TransportBz( float_v Bz, float_v dS, float_v P[] ) const;
  void TransportCBM( float_v dS, const float_v* dsdr, float_v P[], float_v C[], float_v* dsdr1=0, float_v* F=0, float_v* F1=0 ) const;
  void TransportCBM( float_v dS, float_v P[] ) const;    

  //* 
  //* OTHER UTILITIES
  //*

  //* Calculate distance from another object [cm]

  float_v GetDistanceFromVertex( const float_v vtx[] ) const;
  float_v GetDistanceFromVertex( const KFParticleBaseSIMD &Vtx ) const;
  float_v GetDistanceFromParticle( const KFParticleBaseSIMD &p ) const;

  //* Calculate CAMath::Sqrt(Chi2/ndf) deviation from vertex
  //* v = [xyz], Cv=[Cxx,Cxy,Cyy,Cxz,Cyz,Czz]-covariance matrix

  float_v GetDeviationFromVertex( const float_v v[], 
                                   const float_v Cv[]=0 ) const;
  float_v GetDeviationFromVertex( const KFParticleBaseSIMD &Vtx ) const;
  float_v GetDeviationFromParticle( const KFParticleBaseSIMD &p ) const;  

  //* Subtract the particle from the vertex  

  void SubtractFromVertex( KFParticleBaseSIMD &Vtx ) const;
  void SubtractFromParticle( KFParticleBaseSIMD &Vtx ) const;

  static void GetArmenterosPodolanski(KFParticleBaseSIMD& positive, KFParticleBaseSIMD& negative, float_v QtAlfa[2] );
  void RotateXY(float_v angle, float_v Vtx[3]);

  int_v Id() const { return fId; } ///< Returns Id of the particle.
  int NDaughters() const { return fDaughterIds.size(); } ///< Returns number of daughter particles.
  std::vector<int_v> & DaughterIds() { return fDaughterIds; } ///< Returns the vector with the indices of daughter particles.
  int_v GetDaughterId(int iD) const { return fDaughterIds[iD]; } ///< Returns the daughter Id with the index iD.
  
  void SetId( int_v id ){ fId = id; } ///< Sets the Id of the particle. After the construction of a particle should be set by user.
  void SetNDaughters( int n ) { fDaughterIds.reserve(n); } ///< Reserves the size of the vector with daughter Ids to n
  void AddDaughterId( int_v id ){ fDaughterIds.push_back(id); } ///< Adds index of the daughter particle. 
  void CleanDaughtersId() { fDaughterIds.clear(); } ///< Cleans the vector with the indices of daughter particles.

  void SetPDG ( int pdg ) { fPDG = pdg; } ///< Sets the PDG hypothesis common for all elements of the SIMD vector.
  void SetPDG ( int_v& pdg ) { fPDG = pdg; } ///< Sets the PDG hypothesis individual for each entry of the SIMD vector.
  const int_v& GetPDG () const { return fPDG; } ///< Returns the PDG hypothesis.
  const int_v& PDG () const { return fPDG; } ///< Returns the PDG hypothesis.

  void GetDistanceToVertexLine( const KFParticleBaseSIMD &Vertex, float_v &l, float_v &dl, float_m *isParticleFromVertex = 0 ) const;

  static void MultQSQt( const float_v Q[], const float_v S[], float_v SOut[], const int kN  );

 protected:
  /** Converts a pair of indices {i,j} of the covariance matrix to one index corresponding to the triangular form. */
  static Int_t IJ( Int_t i, Int_t j ){ 
    return ( j<=i ) ? i*(i+1)/2+j :j*(j+1)/2+i;
  }
  /** Return an element of the covariance matrix with {i,j} indices. */
  float_v & Cij( Int_t i, Int_t j ){ return fC[IJ(i,j)]; }

  void TransportLine( float_v S, const float_v* dsdr, float_v P[], float_v C[], float_v* dsdr1=0, float_v* F=0, float_v* F1=0 ) const ;
  void TransportLine( float_v S, float_v P[] ) const ;

  static void InvertCholetsky3(float_v a[6]);

  void GetMeasurement( const KFParticleBaseSIMD& daughter, float_v m[], float_v V[], float_v D[3][3] ) ;

  //* Mass constraint function. is needed for the nonlinear mass constraint and a fit with mass constraint
  void SetMassConstraint( float_v *mP, float_v *mC, float_v mJ[7][7], float_v mass, float_m mask );

  float_v fP[8];              ///< Particle parameters { X, Y, Z, Px, Py, Pz, E, S[=DecayLength/P]}.
  float_v fC[36];             ///< Low-triangle covariance matrix of fP.
  int_v fQ;                   ///< The charge of the particle in the units of the elementary charge.
  int_v fNDF;                 ///< Number of degrees of freedom.
  float_v fChi2;              ///< Chi^2.
  float_v fSFromDecay;        ///< Distance from the decay vertex to the current position.
  float_v SumDaughterMass;    ///< Sum of the daughter particles masses. Needed to set the constraint on the minimum mass during particle construction.
  float_v fMassHypo;          ///< The mass hypothesis, used for the constraints during particle construction.
  int_v fId;                  ///< Id of the particle.
  Bool_t fAtProductionVertex; ///< Flag shows if particle is at the production point.
  int_v fPDG;                 ///< The PDG hypothesis assigned to the particle.
  /** \brief Determines the method for the particle construction. \n
   ** 0 - Energy considered as an independent veriable, fitted independently from momentum, without any constraints on mass \n
   ** 2 - Energy considered as an independent variable, fitted independently from momentum, with constraints on mass of daughter particle
   **/
  Int_t fConstructMethod;
  /** \brief A vector with ids of the daughter particles: \n
   ** 1) if particle is created from a track - the index of the track, in this case the size of the vector is always equal to one; \n
   ** 2) if particle is constructed from other particles - indices of these particles in the same array.
   **/
  std::vector<int_v> fDaughterIds; // id of particles it created from. if size == 1 then this is id of track.

} __attribute__((aligned(sizeof(float_v))));

#endif 
