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


//#define NonhomogeneousField
// #define HomogeneousField

#ifndef KFPARTICLESIMD_H
#define KFPARTICLESIMD_H

#include "KFPTrack.h"
#include "KFPTrackVector.h"
#include "KFPEmcCluster.h"
#include "KFPVertex.h"
#include "KFParticleDatabase.h"
#include "KFParticleMath.h"

#ifdef NonhomogeneousField
#include "KFParticleField.h"
#endif

class KFParticle;

/** @class KFParticleSIMD
 ** @brief The main vectorised class of KF Particle pacakge, describes particle objects.
 ** @author  M.Zyzak
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** Vectorised version of the KF Particle class, describes particle objects.
 ** The particle is described with the state vector { X, Y, Z, Px, Py, Pz, E }
 ** and the corresponding covariance matrix.
 ** It contains functionality to create particle-object from track, to construct 
 ** short-lived particles from other tracks or particles. The mathematics is 
 ** based on the Kalman filter method. It also allows to subtract particles from 
 ** the already constructed object,
 ** to transport partices, get parameters together with their erros, get distance 
 ** to other particles and vertices, get deviations from them in terms of errors, etc.
 **/

class KFParticleSIMD
{
  
 public:

  void *operator new(size_t size) { return _mm_malloc(size, sizeof(float32_v)); }     ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new[](size_t size) { return _mm_malloc(size, sizeof(float32_v)); }   ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new(size_t size, void *ptr) { return ::operator new(size, ptr);}   ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new[](size_t size, void *ptr) { return ::operator new(size, ptr);} ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void operator delete(void *ptr, size_t) { _mm_free(ptr); }                        ///< delete operator for the SIMD-alligned dynamic memory release
  void operator delete[](void *ptr, size_t) { _mm_free(ptr); }                      ///< delete operator for the SIMD-alligned dynamic memory release
  //*
  //*  INITIALIZATION
  //*

  //* Set magnetic field for all particles
#ifdef HomogeneousField
  static void SetField( float32_v Bz ); ///< Set Bz field.
#endif
#ifdef NonhomogeneousField
  void SetField(const KFParticleFieldRegion &field)
  {
    /** Set the field approximation along the particle trajectory
     ** \param[in] field - approximation of each component of the magnetic field with the parabola alnog the particle trajectory
     **/
    fField = field;
  }
#endif
  //* Constructor (empty)

  KFParticleSIMD(): fQ(0), fNDF(-3), fChi2(0.f), SumDaughterMass(0.f), fMassHypo(-1.f), fId(-1), fPDG(0), fConstructMethod(0), fDaughterIds()
#ifdef NonhomogeneousField
  , fField() 
#endif
  {
    for( int i=0; i<8; i++) fP[i] = 0.f;
    for(int i=0;i<36;++i) fC[i]=0.f;
    fC[0] = fC[2] = fC[5] = 100.f;
    fC[35] = 1.f;
  }

 //* Initialisation from "cartesian" coordinates ( X Y Z Px Py Pz )
 //* Parameters, covariance matrix, charge and PID hypothesis should be provided 

  void Create( const float32_v Param[], const float32_v Cov[], int32_v Charge, float32_v mass /*int PID*/ );

  void SetOneEntry(int iEntry, KFParticleSIMD& part, int iEntryPart);

  KFParticleSIMD( const KFPTrack *track, int PID );
  KFParticleSIMD( KFPTrack* Track[], int NTracks, const int *pdg=0 );
  KFParticleSIMD( KFPTrackVector &track, int32_v& index, const int32_v& pdg );

  void Create(KFPTrack* Track[], int NTracks, const int *pdg=0);
  void Create(KFPTrackVector &track, int32_v& index, const int32_v& pdg);
  void Load(const KFPTrackVector &track, int index, const int32_v& pdg);
  void Load(const KFPTrackVector &track, int index);
  void Rotate();

  KFParticleSIMD(KFPTrack &Track, const int *pdg=0);
  KFParticleSIMD(KFPTrackVector &track, int n, const int *pdg=0);

  KFParticleSIMD( KFPEmcCluster &track, int32_v& index, const KFParticleSIMD& vertexGuess );
  KFParticleSIMD( KFPEmcCluster &track, int index, const KFParticleSIMD& vertexGuess );
  void Create( KFPEmcCluster &track, int32_v& index, const KFParticleSIMD& vertexGuess );
  void Load( KFPEmcCluster &track, int index, const KFParticleSIMD& vertexGuess );

    
  //* Initialisation from VVertex 

  KFParticleSIMD( const KFPVertex &vertex );
  KFParticleSIMD( KFParticle* part[], const int nPart = 0 );
  KFParticleSIMD( const KFParticle &part );

  //*
  //*  ACCESSORS
  //*

  //* Simple accessors 

  float32_v GetX    () const { return fP[0]; } ///< Returns the sum of masses of the daughters
  float32_v GetY    () const { return fP[1]; } ///< Returns the sum of masses of the daughters
  float32_v GetZ    () const { return fP[2]; } ///< Returns the sum of masses of the daughters
  float32_v GetPx   () const { return fP[3]; } ///< Returns the sum of masses of the daughters
  float32_v GetPy   () const { return fP[4]; } ///< Returns the sum of masses of the daughters
  float32_v GetPz   () const { return fP[5]; } ///< Returns the sum of masses of the daughters
  float32_v GetE    () const { return fP[6]; } ///< Returns the sum of masses of the daughters
  float32_v GetS    () const { return fP[7]; } ///< Returns the sum of masses of the daughters
  int32_v   GetQ    () const { return fQ;    } ///< Returns the sum of masses of the daughters
  float32_v GetChi2 () const { return fChi2; } ///< Returns the sum of masses of the daughters
  int32_v GetNDF    () const { return fNDF;  } ///< Returns the sum of masses of the daughters

  const float32_v& X    () const { return fP[0]; } ///< Retruns X coordinate of the particle, fP[0].
  const float32_v& Y    () const { return fP[1]; } ///< Retruns Y coordinate of the particle, fP[1].
  const float32_v& Z    () const { return fP[2]; } ///< Retruns Z coordinate of the particle, fP[2].
  const float32_v& Px   () const { return fP[3]; } ///< Retruns X component of the momentum, fP[3].
  const float32_v& Py   () const { return fP[4]; } ///< Retruns Y component of the momentum, fP[4].
  const float32_v& Pz   () const { return fP[5]; } ///< Retruns Z component of the momentum, fP[5].
  const float32_v& E    () const { return fP[6]; } ///< Returns energy of the particle, fP[6].
  const float32_v& S    () const { return fP[7]; } ///< Returns dS=l/p, l - decay length, fP[7], defined if production vertex is set.
  const int32_v&   Q    () const { return fQ;    } ///< Returns charge of the particle.
  const float32_v& Chi2 () const { return fChi2; } ///< Returns Chi2 of the fit.
  const int32_v& NDF    () const { return fNDF;  } ///< Returns number of decrease of freedom.
  
  float32_v GetParameter ( int i ) const { return fP[i]; }        ///< Returns P[i] parameter.
  float32_v GetCovariance( int i ) const { return fC[i]; }        ///< Returns C[i] element of the covariance matrix in the lower triangular form.
  float32_v GetCovariance( int i, int j ) const { return Cij(i, j); } ///< Returns C[i,j] element of the covariance matrix.

  //* Accessors with calculations, value returned w/o error flag
  
  float32_v GetP             () const; ///< Returns momentum
  float32_v GetPt            () const; ///< Returns transverse momentum
  float32_v GetEta           () const; ///< Returns pseudorapidity
  float32_v GetPhi           () const; ///< Returns the azimuthal angle phi 
  float32_v GetMomentum      () const; ///< Returns momentum
  float32_v GetMass          () const; ///< Returns mass
  float32_v GetDecayLength   () const; ///< Returns decay length
  float32_v GetDecayLengthXY () const; ///< Returns decay length in XY
  float32_v GetLifeTime      () const; ///< Returns life time ctau [cm]
  float32_v GetR             () const; ///< Returns distance to the origin of the coordinate system {0,0,0}
  float32_v GetRapidity() const { return float32_v(0.5f)*KFPMath::log((fP[6] + fP[5])/(fP[6] - fP[5])); }  ///< Returns rapidity of the particle
  
  //* Accessors to estimated errors

  float32_v GetErrX             () const ; ///< Returns the error of X of current position 
  float32_v GetErrY             () const ; ///< Returns the error of Y of current position
  float32_v GetErrZ             () const ; ///< Returns the error of Z of current position
  float32_v GetErrPx            () const ; ///< Returns the error of X-compoment of the particle momentum
  float32_v GetErrPy            () const ; ///< Returns the error of Y-compoment of the particle momentum
  float32_v GetErrPz            () const ; ///< Returns the error of Z-compoment of the particle momentum
  float32_v GetErrE             () const ; ///< Returns the error of energy
  float32_v GetErrS             () const ; ///< Returns the error of decay length / momentum
  float32_v GetErrP             () const ; ///< Returns the error of momentum
  float32_v GetErrPt            () const ; ///< Returns the error of transverse momentum
  float32_v GetErrEta           () const ; ///< Returns the error of pseudorapidity
  float32_v GetErrPhi           () const ; ///< Returns the error of the azimuthal angle phi 
  float32_v GetErrMomentum      () const ; ///< Returns the error of momentum
  float32_v GetErrMass          () const ; ///< Returns the error of mass
  float32_v GetErrDecayLength   () const ; ///< Returns the error of decay length
  float32_v GetErrDecayLengthXY () const ; ///< Returns the error of decay length in XY
  float32_v GetErrLifeTime      () const ; ///< Returns the error of life time
  float32_v GetErrR             () const ; ///< Returns the error of distance to the origin of the coordinate system {0,0,0}

  //* Accessors with calculations( &value, &estimated sigma )
  //* error flag returned (0 means no error during calculations) 

  mask32_v GetP           ( float32_v &P, float32_v &SigmaP ) const ;   //* momentum
  mask32_v GetPt          ( float32_v &Pt, float32_v &SigmaPt ) const ; //* transverse momentum
  mask32_v GetEta         ( float32_v &Eta, float32_v &SigmaEta ) const ; //* pseudorapidity
  mask32_v GetPhi         ( float32_v &Phi, float32_v &SigmaPhi ) const ; //* phi
  mask32_v GetMomentum    ( float32_v &P, float32_v &SigmaP ) const ;   //* momentum
  mask32_v GetMass        ( float32_v &M, float32_v &SigmaM ) const ;   //* mass
  mask32_v GetDecayLength ( float32_v &L, float32_v &SigmaL ) const ;   //* decay length
  mask32_v GetDecayLengthXY ( float32_v &L, float32_v &SigmaL ) const ;   //* decay length in XY
  mask32_v GetLifeTime    ( float32_v &T, float32_v &SigmaT ) const ;   //* life time
  mask32_v GetR           ( float32_v &R, float32_v &SigmaR ) const ; //* R

  //*
  //*  MODIFIERS
  //*
  
  float32_v & X    () { return fP[0]; } ///< Modifier of X coordinate of the particle, fP[0].
  float32_v & Y    () { return fP[1]; } ///< Modifier of Y coordinate of the particle, fP[1].
  float32_v & Z    () { return fP[2]; } ///< Modifier of Z coordinate of the particle, fP[2].
  float32_v & Px   () { return fP[3]; } ///< Modifier of X component of the momentum, fP[3].
  float32_v & Py   () { return fP[4]; } ///< Modifier of Y component of the momentum, fP[4].
  float32_v & Pz   () { return fP[5]; } ///< Modifier of Z component of the momentum, fP[5].
  float32_v & E    () { return fP[6]; } ///< Modifier of energy of the particle, fP[6].
  float32_v & S    () { return fP[7]; } ///< Modifier of dS=l/p, l - decay length, fP[7], defined if production vertex is set.
  int32_v   & Q    () { return fQ;    } ///< Modifier of charge of the particle.
  float32_v & Chi2 () { return fChi2; } ///< Modifier of Chi2 of the fit.
  int32_v & NDF    () { return fNDF;  } ///< Modifier of number of decrease of freedom.

  float32_v & Parameter ( int i )        { return fP[i];       } ///< Modifier of P[i] parameter.
  float32_v & Covariance( int i )        { return fC[i];       } ///< Modifier of C[i] element of the covariance matrix in the lower triangular form.
  float32_v & Covariance( int i, int j ) { return fC[IJ(i,j)]; } ///< Modifier of C[i,j] element of the covariance matrix.

  const float32_v * Parameters() const       { return fP; } ///< Returns pointer to the parameters fP
  const float32_v * CovarianceMatrix() const { return fC; } ///< Returns pointer to the covariance matrix fC

  void SetConstructMethod(int m) {fConstructMethod = m;} ///< Defines the construction method for the current particle (see description of fConstructMethod).
  void SetMassHypo(float32_v m) { fMassHypo = m;} ///< Sets the mass hypothesis to the particle, is used when fConstructMethod = 2.
  const float32_v& GetMassHypo() const { return fMassHypo; } ///< Returns the mass hypothesis.
  const float32_v& GetSumDaughterMass() const {return SumDaughterMass;} ///< Returns the sum of masses of the daughters.

  int32_v Id() const { return fId; } ///< Returns Id of the particle.
  int NDaughters() const { return fDaughterIds.size(); } ///< Returns number of daughter particles.
  std::vector<int32_v> & DaughterIds() { return fDaughterIds; } ///< Returns the vector with the indices of daughter particles.
  int32_v GetDaughterId(int iD) const { return fDaughterIds[iD]; } ///< Returns the daughter Id with the index iD.
  
  void SetId( int32_v id ){ fId = id; } ///< Sets the Id of the particle. After the construction of a particle should be set by user.
  void SetNDaughters( int n ) { fDaughterIds.reserve(n); } ///< Reserves the size of the vector with daughter Ids to n
  void AddDaughterId( int32_v id ){ fDaughterIds.push_back(id); } ///< Adds index of the daughter particle. 
  void CleanDaughtersId() { fDaughterIds.clear(); } ///< Cleans the vector with the indices of daughter particles.

  void SetPDG ( const int pdg ) { fPDG = pdg; } ///< Sets the PDG hypothesis common for all elements of the SIMD vector.
  void SetPDG ( const int32_v pdg ) { fPDG = pdg; } ///< Sets the PDG hypothesis individual for each entry of the SIMD vector.
  const int32_v& GetPDG () const { return fPDG; } ///< Returns the PDG hypothesis.
  const int32_v& PDG () const { return fPDG; } ///< Returns the PDG hypothesis.

  void GetKFParticle( KFParticle &Part, int iPart = 0);
  void GetKFParticle( KFParticle *Part, int nPart = 0);

  //* 
  //* CONSTRUCTION OF THE PARTICLE BY ITS DAUGHTERS AND MOTHER
  //* USING THE KALMAN FILTER METHOD
  //*

  //* Simple way to add daughter ex. D0+= Pion; 

  void operator +=( const KFParticleSIMD &Daughter );  

  //* Add daughter track to the particle 
  void AddDaughter( const KFParticleSIMD &Daughter );
  void SubtractDaughter( const KFParticleSIMD &Daughter );
  void ReconstructMissingMass(const KFParticleSIMD &Daughter, KFParticleSIMD &MotherFiltered, KFParticleSIMD &cDaughterFiltered, float32_v neutralmasshypo );

  void AddDaughterWithEnergyFit( const KFParticleSIMD &Daughter );
  void AddDaughterWithEnergyFitMC( const KFParticleSIMD &Daughter ); 

  //* Subtract a particle from a vertex  
  void SubtractFromVertex( KFParticleSIMD &Vtx ) const;
  void SubtractFromParticle( KFParticleSIMD &Vtx ) const;

  //* Set production vertex 

  void SetProductionVertex( const KFParticleSIMD &Vtx );

  //* Set mass constraint 

  void SetNonlinearMassConstraint( float32_v Mass );
  void SetMassConstraint( float32_v Mass, float32_v SigmaMass = float32_v(0.f) );

  //* Everything in one go  

  void Construct( const KFParticleSIMD *vDaughters[], int nDaughters, const KFParticleSIMD *ProdVtx=0,   Float_t Mass=-1 );

  //*
  //*                   TRANSPORT
  //* 
  //*  ( main transportation parameter is S = SignedPath/Momentum )
  //*  ( parameters of decay & production vertices are stored locally )
  //*

  //* Transport the particle close to xyz[] point 

  void TransportToPoint( const float32_v xyz[] );

  //* Transport the particle close to VVertex  
#ifdef HomogeneousField
  void TransportToVertex( const KFPVertex &v );
#endif
  //* Transport the particle close to another particle p 

  void TransportToParticle( const KFParticleSIMD &p );

  //* Transport the particle on dS parameter (SignedPath/Momentum) 

  void TransportToDS( float32_v dS, const float32_v* dsdr );
  void TransportToDSLine( float32_v dS, const float32_v* dsdr );
  //* Particular extrapolators one can use 
  void TransportBz( float32_v Bz, float32_v dS, const float32_v* dsdr, float32_v P[], float32_v C[], float32_v* dsdr1=0, float32_v* F=0, float32_v* F1=0, const bool fullC=true ) const;
  void TransportBz( float32_v Bz, float32_v dS, float32_v P[] ) const;
  void TransportCBM( float32_v dS, const float32_v* dsdr, float32_v P[], float32_v C[], float32_v* dsdr1=0, float32_v* F=0, float32_v* F1=0 ) const;
  void TransportCBM( float32_v dS, float32_v P[] ) const;    

  void TransportLine( float32_v S, const float32_v* dsdr, float32_v P[], float32_v C[], float32_v* dsdr1=0, float32_v* F=0, float32_v* F1=0 ) const ;
  void TransportLine( float32_v S, float32_v P[] ) const ;

  //* Get dS to a certain space point 

  float32_v GetDStoPoint( const float32_v xyz[3], float32_v dsdr[6] ) const ;
  float32_v GetDStoPointLine( const float32_v xyz[3], float32_v dsdr[6] ) const;
  float32_v GetDStoPointBz( float32_v Bz, const float32_v xyz[3], float32_v dsdr[6], const float32_v* param = 0 ) const;
  float32_v GetDStoPointBy( float32_v By, const float32_v xyz[3], float32_v dsdr[6] ) const;
  float32_v GetDStoPointCBM( const float32_v xyz[3], float32_v dsdr[6] ) const;

  float32_v GetDStoPointXYBz( float32_v B, const float32_v xy[2] ) const;
  float32_v GetDStoPointZBz( const float32_v z0 ) const;
  void GetDStoCylinderBz( const float32_v B, const float32_v R, float32_v dS[2] ) const;

#ifdef HomogeneousField
  float32_v GetDStoPointXY( const float32_v xyz[2] ) const {
    return GetDStoPointXYBz( GetFieldAlice(), xyz );
  }
  //* Get dS to a certain space point 
  void GetDStoCylinder( const float32_v R, float32_v dS[2] ) const {
    GetDStoCylinderBz( GetFieldAlice(), R, dS );
  }
#endif

  //* Get dS to other particle p (dSp for particle p also returned) 

  void GetDStoParticle( const KFParticleSIMD &p, float32_v dS[2], float32_v dsdr[4][6] ) const ;
  void GetDStoParticleFast( const KFParticleSIMD &p, float32_v dS[2] ) const ;
  void GetDStoParticleLine( const KFParticleSIMD &p, float32_v dS[2], float32_v dsdr[4][6] ) const ;
  void GetDStoParticleLine( const KFParticleSIMD &p, float32_v dS[2]  ) const ;
  void GetDStoParticleBz( float32_v Bz, const KFParticleSIMD &p, float32_v dS[2], float32_v dsdr[4][6], const float32_v* param1 =0, const float32_v* param2 =0  ) const ;
  void GetDStoParticleBz( float32_v Bz, const KFParticleSIMD &p, float32_v dS[2], const float32_v* param1 =0, const float32_v* param2 =0  ) const ;
  void GetDStoParticleBy( float32_v B, const KFParticleSIMD &p, float32_v dS[2], float32_v dsdr[4][6] ) const ;
  void GetDStoParticleBy( float32_v B, const KFParticleSIMD &p, float32_v dS[2] ) const ;
  void GetDStoParticleB( float32_v B[3], const KFParticleSIMD &p, float32_v dS[2], float32_v dsdr[4][6] ) const;
  void GetDStoParticleB( float32_v B[3], const KFParticleSIMD &p, float32_v dS[2] ) const;
  void GetDStoParticleCBM( const KFParticleSIMD &p, float32_v dS[2], float32_v dsdr[4][6] ) const ;
  void GetDStoParticleCBM( const KFParticleSIMD &p, float32_v dS[2] ) const ;

  void GetDistanceToVertexLine( const KFParticleSIMD &Vertex, float32_v &l, float32_v &dl, mask32_v *isParticleFromVertex = 0 ) const;

  //* 
  //* OTHER UTILITIES
  //*

  //* Calculate distance from another object [cm]

  float32_v GetDistanceFromVertex( const float32_v vtx[] ) const;
  float32_v GetDistanceFromVertex( const KFParticleSIMD &Vtx ) const;
  float32_v GetDistanceFromParticle( const KFParticleSIMD &p ) const;

  //* Calculate CAMath::Sqrt(Chi2/ndf) deviation from vertex
  //* v = [xyz], Cv=[Cxx,Cxy,Cyy,Cxz,Cyz,Czz]-covariance matrix

  float32_v GetDeviationFromVertex( const float32_v v[], 
                                   const float32_v Cv[]=0 ) const;
  float32_v GetDeviationFromVertex( const KFParticleSIMD &Vtx ) const;
  float32_v GetDeviationFromParticle( const KFParticleSIMD &p ) const;  

  //* Calculate distance from another object [cm] in XY-plane

  mask32_v GetDistanceFromVertexXY( const float32_v vtx[], float32_v &val, float32_v &err ) const ;
  mask32_v GetDistanceFromVertexXY( const float32_v vtx[], const float32_v Cv[], float32_v &val, float32_v &err ) const ;
  mask32_v GetDistanceFromVertexXY( const KFParticleSIMD &Vtx, float32_v &val, float32_v &err ) const ;
#ifdef HomogeneousField
  mask32_v GetDistanceFromVertexXY( const KFPVertex &Vtx, float32_v &val, float32_v &err ) const ;
#endif

  float32_v GetDistanceFromVertexXY( const float32_v vtx[] ) const ;
  float32_v GetDistanceFromVertexXY( const KFParticleSIMD &Vtx ) const ;
#ifdef HomogeneousField
  float32_v GetDistanceFromVertexXY( const KFPVertex &Vtx ) const ;
#endif
  float32_v GetDistanceFromParticleXY( const KFParticleSIMD &p ) const ;

  //* Calculate sqrt(Chi2/ndf) deviation from another object in XY plane
  //* ( v = [xyz]-vertex, Cv=[Cxx,Cxy,Cyy,Cxz,Cyz,Czz]-covariance matrix )

  float32_v GetDeviationFromVertexXY( const float32_v v[], const float32_v Cv[]=0 ) const ;
  float32_v GetDeviationFromVertexXY( const KFParticleSIMD &Vtx ) const ;
#ifdef HomogeneousField
  float32_v GetDeviationFromVertexXY( const KFPVertex &Vtx ) const ;
#endif
  float32_v GetDeviationFromParticleXY( const KFParticleSIMD &p ) const ;

  //* Calculate opennig angle between two particles

  float32_v GetAngle  ( const KFParticleSIMD &p ) const ;
  float32_v GetAngleXY( const KFParticleSIMD &p ) const ;
  float32_v GetAngleRZ( const KFParticleSIMD &p ) const ;

    // * Pseudo Proper Time of decay = (r*pt) / |pt| * M/|pt|
    // @primVertex - primary vertex
    // @mass - mass of the mother particle (in the case of "Hb -> JPsi" it would be JPsi mass)
    // @*timeErr2 - squared error of the decay time. If timeErr2 = 0 it isn't calculated
  float32_v GetPseudoProperDecayTime( const KFParticleSIMD &primVertex, const float32_v& mass, float32_v* timeErr2 = 0 ) const;

  void GetFieldValue( const float32_v xyz[], float32_v B[] ) const ;
  
  void Transport( float32_v dS, const float32_v* dsdr, float32_v P[], float32_v C[], float32_v* dsdr1=0, float32_v* F=0, float32_v* F1=0, const bool fullC=true ) const ;
  void TransportFast( float32_v dS, float32_v P[] ) const ;
  
  static void GetArmenterosPodolanski(KFParticleSIMD& positive, KFParticleSIMD& negative, float32_v QtAlfa[2] );
  void RotateXY(float32_v angle, float32_v Vtx[3]);
  static void MultQSQt( const float32_v Q[], const float32_v S[], float32_v SOut[], const int kN  );

 private:
  void GetMeasurement( const KFParticleSIMD& daughter, float32_v m[], float32_v V[], float32_v D[3][3] ) ;
  //* Mass constraint function. is needed for the nonlinear mass constraint and a fit with mass constraint
  void SetMassConstraint( float32_v *mP, float32_v *mC, float32_v mJ[7][7], float32_v mass, mask32_v mask );

  static void InvertCholetsky3(float32_v a[6]);

  //*
  //*  INTERNAL STUFF
  //* 

  /** Converts a pair of indices {i,j} of the covariance matrix to one index corresponding to the triangular form. */
  static int IJ( int i, int j ){ 
    return ( j<=i ) ? i*(i+1)/2+j :j*(j+1)/2+i;
  }
  /** Return an element of the covariance matrix with {i,j} indices. */
  float32_v Cij( int i, int j ) const { return fC[IJ(i,j)]; }

  float32_v fP[8];              ///< Particle parameters { X, Y, Z, Px, Py, Pz, E, S[=DecayLength/P]}.
  float32_v fC[36];             ///< Low-triangle covariance matrix of fP.
  int32_v fQ;                   ///< The charge of the particle in the units of the elementary charge.
  int32_v fNDF;                 ///< Number of degrees of freedom.
  float32_v fChi2;              ///< Chi^2.
  float32_v SumDaughterMass;    ///< Sum of the daughter particles masses. Needed to set the constraint on the minimum mass during particle construction.
  float32_v fMassHypo;          ///< The mass hypothesis, used for the constraints during particle construction.
  int32_v fId;                  ///< Id of the particle.
  int32_v fPDG;                 ///< The PDG hypothesis assigned to the particle.
  /** \brief Determines the method for the particle construction. \n
   ** 0 - Energy considered as an independent veriable, fitted independently from momentum, without any constraints on mass \n
   ** 2 - Energy considered as an independent variable, fitted independently from momentum, with constraints on mass of daughter particle
   **/
  int fConstructMethod;
  /** \brief A vector with ids of the daughter particles: \n
   ** 1) if particle is created from a track - the index of the track, in this case the size of the vector is always equal to one; \n
   ** 2) if particle is constructed from other particles - indices of these particles in the same array.
   **/
  std::vector<int32_v> fDaughterIds; // id of particles it created from. if size == 1 then this is id of track.

  //* Method to access ALICE field 
#ifdef HomogeneousField
  static float32_v GetFieldAlice();
#endif
  
 private:
#ifdef HomogeneousField
  static float32_v fgBz;  ///< Bz compoment of the magnetic field (is defined in case of #ifdef HomogeneousField)
#endif
#ifdef NonhomogeneousField
  /** \brief Approximation of the magnetic field along the track trajectory.
   ** Each component (Bx, By, Bz) is approximated with the parabola depending on Z coordinate. Is defined in case of #ifdef NonhomogeneousField.
   **/
  KFParticleFieldRegion fField;
#endif
} __attribute__((aligned(sizeof(float32_v))));

inline void KFParticleSIMD::InvertCholetsky3(float32_v a[6])
{
  /** Inverts symmetric 3x3 matrix a using modified Choletsky decomposition. The result is stored to the same matrix a.
   ** \param[in,out] a - 3x3 symmetric matrix
   **/

  const float32_v d0 = 1.f/a[0];
  const float32_v u01 = a[1]*d0;
  const float32_v u02 = a[3]*d0;
  
  const float32_v d1 = 1.f/(a[2] - u01*a[1]);
  const float32_v u12_d = a[4] - u01*a[3];
  const float32_v u12 = d1*u12_d;  
  const float32_v d2 = 1.f/(a[5] - u02*a[3] - u12*u12_d);
  
  //find V = -U^-1
  const float32_v v02 = u02 - u01*u12;
  
  //find A^-1 = U^-1 D^-1 Ut^-1
  a[5] =  d2;
  a[4] = -d2*u12;
  a[3] = -d2*v02;
  const float32_v d1u01 = -d1*u01;
  a[2] = d1    - a[4]*u12;
  a[1] = d1u01 - a[3]*u12;
  a[0] = d0 - d1u01*u01 - a[3]*v02;
}

//---------------------------------------------------------------------
//
//     Inline implementation of the KFParticleSIMD methods
//
//---------------------------------------------------------------------

#ifdef HomogeneousField
inline void KFParticleSIMD::SetField( float32_v Bz )
{ 
  /** Sets the constant homogemeous one-component magnetic field Bz (is defined in case of #ifdef HomogeneousField).
   ** \param[in] Bz - Z-component of the magnetic field
   **/
  fgBz = Bz;
}
#endif

inline float32_v KFParticleSIMD::GetP    () const
{
  float32_v par, err;
  GetMomentum( par, err );
  return par;
}

inline float32_v KFParticleSIMD::GetPt   () const
{
  float32_v par, err;
  GetPt( par, err ) ;
  return par;
}

inline float32_v KFParticleSIMD::GetEta   () const
{
  float32_v par, err;
  GetEta( par, err );
  return par;
}

inline float32_v KFParticleSIMD::GetPhi   () const
{
  float32_v par, err;
  KFParticleSIMD::GetPhi( par, err );
  return par;
}

inline float32_v KFParticleSIMD::GetMomentum    () const
{
  float32_v par, err;
  GetMomentum( par, err );
  return par;
}

inline float32_v KFParticleSIMD::GetMass        () const
{
  float32_v par, err;
  GetMass( par, err );
  return par;
}

inline float32_v KFParticleSIMD::GetDecayLength () const
{
  float32_v par, err;
  GetDecayLength( par, err );
  return par;
}

inline float32_v KFParticleSIMD::GetDecayLengthXY () const
{
  float32_v par, err;
  KFParticleSIMD::GetDecayLengthXY( par, err );
  return par;
}

inline float32_v KFParticleSIMD::GetLifeTime    () const
{
  float32_v par, err;
  KFParticleSIMD::GetLifeTime( par, err );
  return par;
}

inline float32_v KFParticleSIMD::GetR   () const
{
  float32_v par, err;
  KFParticleSIMD::GetR( par, err );
  return par;
}

inline float32_v KFParticleSIMD::GetErrX           () const 
{
  return sqrt(abs( GetCovariance(0,0) ));
}

inline float32_v KFParticleSIMD::GetErrY           () const 
{
  return sqrt(abs( GetCovariance(1,1) ));
}

inline float32_v KFParticleSIMD::GetErrZ           () const 
{
  return sqrt(abs( GetCovariance(2,2) ));
}

inline float32_v KFParticleSIMD::GetErrPx          () const 
{
  return sqrt(abs( GetCovariance(3,3) ));
}

inline float32_v KFParticleSIMD::GetErrPy          () const 
{
  return sqrt(abs( GetCovariance(4,4) ));
}

inline float32_v KFParticleSIMD::GetErrPz          () const 
{
  return sqrt(abs( GetCovariance(5,5) ));
}

inline float32_v KFParticleSIMD::GetErrE           () const 
{
  return sqrt(abs( GetCovariance(6,6) ));
}

inline float32_v KFParticleSIMD::GetErrS           () const 
{
  return sqrt(abs( GetCovariance(7,7) ));
}

inline float32_v KFParticleSIMD::GetErrP    () const
{
  float32_v par, err;
  mask32_v mask = GetMomentum( par, err );
  return select(mask, 1.e10f, err);
}

inline float32_v KFParticleSIMD::GetErrPt    () const
{
  float32_v par, err;
  mask32_v mask = GetPt( par, err );
  return select(mask, 1.e10f, err);
}

inline float32_v KFParticleSIMD::GetErrEta    () const
{
  float32_v par, err;
  mask32_v mask = GetEta( par, err );
  return select(mask, 1.e10f, err);
}

inline float32_v KFParticleSIMD::GetErrPhi    () const
{
  float32_v par, err;
  mask32_v mask = GetPhi( par, err );
  return select(mask, 1.e10f, err);
}

inline float32_v KFParticleSIMD::GetErrMomentum    () const
{
  float32_v par, err;
  mask32_v mask = GetMomentum( par, err );
  return select(mask, 1.e10f, err);
}

inline float32_v KFParticleSIMD::GetErrMass        () const
{
  float32_v par, err;
  mask32_v mask = KFParticleSIMD::GetMass( par, err );
  return select(mask, 1.e10f, err);
}

inline float32_v KFParticleSIMD::GetErrDecayLength () const
{
  float32_v par, err;
  mask32_v mask = KFParticleSIMD::GetDecayLength( par, err );
  return select(mask, 1.e10f, err);
}

inline float32_v KFParticleSIMD::GetErrDecayLengthXY () const
{
  float32_v par, err;
  mask32_v mask = KFParticleSIMD::GetDecayLengthXY( par, err );
  return select(mask, 1.e10f, err);
}

inline float32_v KFParticleSIMD::GetErrLifeTime    () const
{
  float32_v par, err;
  mask32_v mask = KFParticleSIMD::GetLifeTime( par, err );
  return select(mask, 1.e10f, err);
}

inline float32_v KFParticleSIMD::GetErrR    () const
{
  float32_v par, err;
  mask32_v mask = KFParticleSIMD::GetR( par, err );
  return select(mask, 1.e10f, err);
}

inline mask32_v KFParticleSIMD::GetP( float32_v &P, float32_v &SigmaP ) const 
{
  /** Calculates particle momentum and its error. If they are well defined returns 0, otherwise 1.
   ** \param[out] P - momentum of the particle
   ** \param[out] SigmaP - its error
   **/
  return GetMomentum( P, SigmaP );
}

inline void KFParticleSIMD::TransportToPoint( const float32_v xyz[] )
{
  /** Transports particle to the distance of closest approach to the point xyz.
   ** \param[in] xyz[3] - point, where particle should be transported
   **/
  
  float32_v dsdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  const float32_v dS = GetDStoPoint(xyz, dsdr);
  TransportToDS( dS, dsdr );
}
#ifdef HomogeneousField
inline void KFParticleSIMD::TransportToVertex( const KFPVertex &v )
{
  /** Transports particle to the distance of closest approach to the vertex v.
   ** \param[in] v - vertex, where particle should be transported
   **/
  TransportToPoint( KFParticleSIMD(v).fP );
}
#endif
inline void KFParticleSIMD::TransportToParticle( const KFParticleSIMD &p )
{ 
  /** Transports particle to the distance of closest approach to the particle p.
   ** \param[in] p - particle, to which the current particle should be transported.
   **/
  float32_v dsdr[4][6];
  float32_v dS[2];
  GetDStoParticle( p, dS, dsdr );
  TransportToDS( dS[0], dsdr[0] );
}

inline float32_v KFParticleSIMD::GetDStoPoint( const float32_v xyz[3], float32_v dsdr[6] ) const 
{
  /** Returns dS = l/p parameter, where \n
   ** 1) l - signed distance to the DCA point with the input xyz point;\n
   ** 2) p - momentum of the particle; \n
   ** Also calculates partial derivatives dsdr of the parameter dS over the state vector of the current particle.
   ** If "HomogeneousField" is defined GetDStoPointBz() is called,
   ** if "NonhomogeneousField" is defined - GetDStoPointCBM()
   ** \param[in] xyz[3] - point, to which particle should be transported
   ** \param[out] dsdr[6] = ds/dr partial derivatives of the parameter dS over the state vector of the current particle
   ** \param[in] param - optional parameter, is used in case if the parameters of the particle are rotated
   ** to other coordinate system (see GetDStoPointBy() function), otherwise fP are used
   **/
#ifdef HomogeneousField
  return GetDStoPointBz( GetFieldAlice(), xyz, dsdr );
#endif
#ifdef NonhomogeneousField
  return GetDStoPointCBM( xyz, dsdr );
#endif
}

#ifdef HomogeneousField
inline float32_v KFParticleSIMD::GetFieldAlice()
{ 
  /** Returns value of the constant homogemeous one-component magnetic field Bz, (is defined in case of #ifdef HomogeneousField). */
  return fgBz; 
}
#endif

#ifdef HomogeneousField
inline void KFParticleSIMD::GetFieldValue( const float32_v * /*xyz*/, float32_v B[] ) const 
{  
  /** Calculates the Bx, By, Bz components at the point xyz using approximation of the
   ** magnetic field along the particle trajectory.
   ** \param[in] xyz[3] - X, Y, Z coordiantes of the point where the magnetic field should be calculated
   ** \param[out] B[3] - value of X, Y, Z components of the calculated magnetic field at the given point
   **/  
  B[0] = B[1] = 0;
  B[2] = GetFieldAlice();
}
#endif

#ifdef NonhomogeneousField
inline void KFParticleSIMD::GetFieldValue( const float32_v xyz[], float32_v B[] ) const 
{
  /** Calculates the Bx, By, Bz components at the point xyz using approximation of the
   ** magnetic field along the particle trajectory.
   ** \param[in] xyz[3] - X, Y, Z coordiantes of the point where the magnetic field should be calculated
   ** \param[out] B[3] - value of X, Y, Z components of the calculated magnetic field at the given point
   **/
  KFParticleFieldValue mB = const_cast<KFParticleFieldRegion&>(fField).Get(xyz[2]);
  B[0] = mB.x;
  B[1] = mB.y;
  B[2] = mB.z;
}
#endif

inline void KFParticleSIMD::GetDStoParticle( const KFParticleSIMD &p, float32_v dS[2], float32_v dsdr[4][6] )const
{
  /** Calculates dS = l/p parameters for two particles, where \n
   ** 1) l - signed distance to the DCA point with the other particle;\n
   ** 2) p - momentum of the particleю \n
   ** dS[0] is the transport parameter for the current particle, dS[1] - for the particle "p".
   ** Also calculates partial derivatives dsdr of the parameters dS[0] and dS[1] over the state vectors of the particles:\n
   ** 1) dsdr[0][6] = d(dS[0])/d(param1);\n
   ** 2) dsdr[1][6] = d(dS[0])/d(param2);\n
   ** 3) dsdr[2][6] = d(dS[1])/d(param1);\n
   ** 4) dsdr[3][6] = d(dS[1])/d(param2);\n
   ** where param1 are parameters of the current particle fP and
   ** param2 are parameters of the second particle p.fP. If "HomogeneousField" is defined GetDStoParticleBz() is called,
   ** if "NonhomogeneousField" is defined - GetDStoParticleCBM()
   ** \param[in] p - second particle
   ** \param[out] dS[2] - transport parameters dS for the current particle (dS[0]) and the second particle "p" (dS[1])
   ** \param[out] dsdr[4][6] - partial derivatives of the parameters dS[0] and dS[1] over the state vectors of the both particles
   **/
#ifdef HomogeneousField
  GetDStoParticleBz( GetFieldAlice(), p, dS, dsdr ) ;
#endif
#ifdef NonhomogeneousField
  GetDStoParticleCBM( p, dS, dsdr ) ;
#endif
}

inline void KFParticleSIMD::GetDStoParticleFast( const KFParticleSIMD &p, float32_v dS[2] )const
{
  /** Calculates dS = l/p parameters for two particles, where \n
   ** 1) l - signed distance to the DCA point with the other particle;\n
   ** 2) p - momentum of the particleю \n
   ** dS[0] is the transport parameter for the current particle, dS[1] - for the particle "p".
   ** If "HomogeneousField" is defined GetDStoParticleBz() is called,
   ** if "NonhomogeneousField" is defined - GetDStoParticleCBM()
   ** \param[in] p - second particle
   ** \param[out] dS[2] - transport parameters dS for the current particle (dS[0]) and the second particle "p" (dS[1])
   **/
#ifdef HomogeneousField
  GetDStoParticleBz( GetFieldAlice(), p, dS ) ;
#endif
#ifdef NonhomogeneousField
  GetDStoParticleCBM( p, dS ) ;
#endif
}

inline void KFParticleSIMD::Transport( float32_v dS, const float32_v* dsdr, float32_v P[], float32_v C[], float32_v* dsdr1, float32_v* F, float32_v* F1, const bool fullC ) const 
{
  /** Transports the parameters and their covariance matrix of the current particle
   ** on a length defined by the transport parameter dS = l/p, where l is the signed distance and p is 
   ** the momentum of the current particle. If "HomogeneousField" is defined TransportBz()
   ** is called, if "NonhomogeneousField" - TransportCBM().
   ** The obtained parameters and covariance matrix are stored to the arrays P and 
   ** C respectively. P and C can be set to the parameters fP and covariance matrix fC of the current particle. In this
   ** case the particle parameters will be modified. Dependence of the transport parameter dS on the state vector of the
   ** current particle is taken into account in the covariance matrix using partial derivatives dsdr = d(dS)/d(fP). If
   ** a pointer to F is initialised the transport jacobian F = d(fP new)/d(fP old) is stored.
   ** Since dS can depend on the state vector r1 of other particle or vertex, the corelation matrix 
   ** F1 = d(fP new)/d(r1) can be optionally calculated if a pointer F1 is provided.
   *  Parameters F and F1 should be either both initialised or both set to null pointer.
   ** \param[in] dS - transport parameter which defines the distance to which particle should be transported
   ** \param[in] dsdr[6] = ds/dr - partial derivatives of the parameter dS over the state vector of the current particle
   ** \param[out] P[8] - array, where transported parameters should be stored
   ** \param[out] C[36] - array, where transported covariance matrix (8x8) should be stored in the lower triangular form 
   ** \param[in] dsdr1[6] = ds/dr - partial derivatives of the parameter dS over the state vector of another particle 
   ** or vertex
   ** \param[out] F[36] - optional parameter, transport jacobian, 6x6 matrix F = d(fP new)/d(fP old)
   ** \param[out] F1[36] - optional parameter, corelation 6x6 matrix betweeen the current particle and particle or vertex
   ** with the state vector r1, to which the current particle is being transported, F1 = d(fP new)/d(r1)
   **/ 
#ifdef HomogeneousField
  TransportBz( GetFieldAlice(), dS, dsdr, P, C, dsdr1, F, F1, fullC );
#endif
#ifdef NonhomogeneousField
  TransportCBM( dS, dsdr, P, C, dsdr1, F, F1 );
#endif
}

inline void KFParticleSIMD::TransportFast( float32_v dS, float32_v P[] ) const 
{
  /** Transports the parametersof the current particle
   ** on a length defined by the transport parameter dS = l/p, where l is the signed distance and p is 
   ** the momentum of the current particle. If "HomogeneousField" is defined TransportBz()
   ** is called, if "NonhomogeneousField" - TransportCBM().
   ** The obtained parameters are stored to the array P.
   ** P can be set to the parameters fP of the current particle. In this
   ** case the particle parameters will be modified. 
   ** \param[in] dS - transport parameter which defines the distance to which particle should be transported
   ** \param[out] P[8] - array, where transported parameters should be stored
   **/ 
#ifdef HomogeneousField
  TransportBz( GetFieldAlice(), dS, P );
#endif
#ifdef NonhomogeneousField
  TransportCBM( dS, P );
#endif
}

inline void KFParticleSIMD::Load(const KFPTrackVector &track, int index, const int32_v& pdg)
{
  /** Create a particle from a set of consequetive tracks stored in the KFPTrackVector format
   ** starting from the index "index".
   ** \param[in] track - an array with tracks in the KFPTrackVector format
   ** \param[in] index - index of the first track
   ** \param[in] pdg - a SIMD vector with an individual pdg hypothesis for each element
   **/
  
  fP[0] = reinterpret_cast<const float32_v&>(track.Parameter(0)[index]);
  fP[1] = reinterpret_cast<const float32_v&>(track.Parameter(1)[index]);
  fP[2] = reinterpret_cast<const float32_v&>(track.Parameter(2)[index]);
  fP[3] = reinterpret_cast<const float32_v&>(track.Parameter(3)[index]);
  fP[4] = reinterpret_cast<const float32_v&>(track.Parameter(4)[index]);
  fP[5] = reinterpret_cast<const float32_v&>(track.Parameter(5)[index]);
   
  fC[ 0] = reinterpret_cast<const float32_v&>(track.Covariance( 0)[index]);
  fC[ 1] = reinterpret_cast<const float32_v&>(track.Covariance( 1)[index]);
  fC[ 2] = reinterpret_cast<const float32_v&>(track.Covariance( 2)[index]);
  fC[ 3] = reinterpret_cast<const float32_v&>(track.Covariance( 3)[index]);
  fC[ 4] = reinterpret_cast<const float32_v&>(track.Covariance( 4)[index]);
  fC[ 5] = reinterpret_cast<const float32_v&>(track.Covariance( 5)[index]);
  fC[ 6] = reinterpret_cast<const float32_v&>(track.Covariance( 6)[index]);
  fC[ 7] = reinterpret_cast<const float32_v&>(track.Covariance( 7)[index]);
  fC[ 8] = reinterpret_cast<const float32_v&>(track.Covariance( 8)[index]);
  fC[ 9] = reinterpret_cast<const float32_v&>(track.Covariance( 9)[index]);
  fC[10] = reinterpret_cast<const float32_v&>(track.Covariance(10)[index]);
  fC[11] = reinterpret_cast<const float32_v&>(track.Covariance(11)[index]);
  fC[12] = reinterpret_cast<const float32_v&>(track.Covariance(12)[index]);
  fC[13] = reinterpret_cast<const float32_v&>(track.Covariance(13)[index]);
  fC[14] = reinterpret_cast<const float32_v&>(track.Covariance(14)[index]);
  fC[15] = reinterpret_cast<const float32_v&>(track.Covariance(15)[index]);
  fC[16] = reinterpret_cast<const float32_v&>(track.Covariance(16)[index]);
  fC[17] = reinterpret_cast<const float32_v&>(track.Covariance(17)[index]);
  fC[18] = reinterpret_cast<const float32_v&>(track.Covariance(18)[index]);
  fC[19] = reinterpret_cast<const float32_v&>(track.Covariance(19)[index]);
  fC[20] = reinterpret_cast<const float32_v&>(track.Covariance(20)[index]);
  
#ifdef NonhomogeneousField
  for(int i=0; i<10; i++)
    fField.fField[i] = reinterpret_cast<const float32_v&>(track.FieldCoefficient(i)[index]);
#endif
  
  //   fPDG.gather(&(track.PDG()[0]), index);
  fQ = reinterpret_cast<const int32_v&>(track.Q()[index]);

  float32_v mass = KFParticleDatabase::Instance()->GetMass(pdg);
  
  float32_v energy = sqrt( mass*mass + fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5]);
  fP[6] = energy;
  fP[7] = 0;
  fNDF = 0;
  fChi2 = 0;

  float32_v energyInv = 1.f/energy;
  float32_v 
    h0 = fP[3]*energyInv,
    h1 = fP[4]*energyInv,
    h2 = fP[5]*energyInv;

  fC[21] = h0*fC[ 6] + h1*fC[10] + h2*fC[15];
  fC[22] = h0*fC[ 7] + h1*fC[11] + h2*fC[16];
  fC[23] = h0*fC[ 8] + h1*fC[12] + h2*fC[17];
  fC[24] = h0*fC[ 9] + h1*fC[13] + h2*fC[18];
  fC[25] = h0*fC[13] + h1*fC[14] + h2*fC[19];
  fC[26] = h0*fC[18] + h1*fC[19] + h2*fC[20];
  fC[27] = ( h0*h0*fC[ 9] + h1*h1*fC[14] + h2*h2*fC[20] 
           + 2*(h0*h1*fC[13] + h0*h2*fC[18] + h1*h2*fC[19] ) );
  for( int i=28; i<36; i++ ) fC[i] = 0.f;
  fC[35] = 1.f;

  SumDaughterMass = mass;
  fMassHypo = mass;
}

inline void KFParticleSIMD::Load(const KFPTrackVector &track, int index)
{
  /** Create a particle from a set of consequetive tracks stored in the KFPTrackVector format
   ** starting from the index "index".
   ** \param[in] track - an array with tracks in the KFPTrackVector format
   ** \param[in] index - index of the first track
   **/
  
  fP[0] = reinterpret_cast<const float32_v&>(track.Parameter(0)[index]);
  fP[1] = reinterpret_cast<const float32_v&>(track.Parameter(1)[index]);
  fP[2] = reinterpret_cast<const float32_v&>(track.Parameter(2)[index]);
  fP[3] = reinterpret_cast<const float32_v&>(track.Parameter(3)[index]);
  fP[4] = reinterpret_cast<const float32_v&>(track.Parameter(4)[index]);
  fP[5] = reinterpret_cast<const float32_v&>(track.Parameter(5)[index]);
   
  fC[ 0] = reinterpret_cast<const float32_v&>(track.Covariance( 0)[index]);
  fC[ 1] = reinterpret_cast<const float32_v&>(track.Covariance( 1)[index]);
  fC[ 2] = reinterpret_cast<const float32_v&>(track.Covariance( 2)[index]);
  fC[ 3] = reinterpret_cast<const float32_v&>(track.Covariance( 3)[index]);
  fC[ 4] = reinterpret_cast<const float32_v&>(track.Covariance( 4)[index]);
  fC[ 5] = reinterpret_cast<const float32_v&>(track.Covariance( 5)[index]);
  fC[ 6] = reinterpret_cast<const float32_v&>(track.Covariance( 6)[index]);
  fC[ 7] = reinterpret_cast<const float32_v&>(track.Covariance( 7)[index]);
  fC[ 8] = reinterpret_cast<const float32_v&>(track.Covariance( 8)[index]);
  fC[ 9] = reinterpret_cast<const float32_v&>(track.Covariance( 9)[index]);
  fC[10] = reinterpret_cast<const float32_v&>(track.Covariance(10)[index]);
  fC[11] = reinterpret_cast<const float32_v&>(track.Covariance(11)[index]);
  fC[12] = reinterpret_cast<const float32_v&>(track.Covariance(12)[index]);
  fC[13] = reinterpret_cast<const float32_v&>(track.Covariance(13)[index]);
  fC[14] = reinterpret_cast<const float32_v&>(track.Covariance(14)[index]);
  fC[15] = reinterpret_cast<const float32_v&>(track.Covariance(15)[index]);
  fC[16] = reinterpret_cast<const float32_v&>(track.Covariance(16)[index]);
  fC[17] = reinterpret_cast<const float32_v&>(track.Covariance(17)[index]);
  fC[18] = reinterpret_cast<const float32_v&>(track.Covariance(18)[index]);
  fC[19] = reinterpret_cast<const float32_v&>(track.Covariance(19)[index]);
  fC[20] = reinterpret_cast<const float32_v&>(track.Covariance(20)[index]);
  
#ifdef NonhomogeneousField
  for(int i=0; i<10; i++)
    fField.fField[i] = reinterpret_cast<const float32_v&>(track.FieldCoefficient(i)[index]);
#endif
  
  fQ = reinterpret_cast<const int32_v&>(track.Q()[index]);
}

#endif 
