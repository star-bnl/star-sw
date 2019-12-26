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

#include "KFParticleBaseSIMD.h"

#include "KFPTrack.h"
#include "KFPTrackVector.h"
#include "KFPEmcCluster.h"
#include "KFPVertex.h"

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

class KFParticleSIMD :public KFParticleBaseSIMD
{
  
 public:

  void *operator new(size_t size) { return _mm_malloc(size, sizeof(float_v)); }     ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new[](size_t size) { return _mm_malloc(size, sizeof(float_v)); }   ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new(size_t size, void *ptr) { return ::operator new(size, ptr);}   ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new[](size_t size, void *ptr) { return ::operator new(size, ptr);} ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void operator delete(void *ptr, size_t) { _mm_free(ptr); }                        ///< delete operator for the SIMD-alligned dynamic memory release
  void operator delete[](void *ptr, size_t) { _mm_free(ptr); }                      ///< delete operator for the SIMD-alligned dynamic memory release
  //*
  //*  INITIALIZATION
  //*

  //* Set magnetic field for all particles
#ifdef HomogeneousField
  static void SetField( float_v Bz ); ///< Set Bz field.
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

  KFParticleSIMD():KFParticleBaseSIMD()
#ifdef NonhomogeneousField
  , fField() 
#endif
  { ; }

  //* Destructor (empty)

  virtual ~KFParticleSIMD(){ ; }

  //* Construction of mother particle by its 2-3-4 daughters

  KFParticleSIMD( const KFParticleSIMD &d1, const KFParticleSIMD &d2 );
  KFParticleSIMD( const KFParticleSIMD &d1, const KFParticleSIMD &d2,  const KFParticleSIMD &d3 );
  KFParticleSIMD( const KFParticleSIMD &d1, const KFParticleSIMD &d2, const KFParticleSIMD &d3, const KFParticleSIMD &d4 );

 //* Initialisation from "cartesian" coordinates ( X Y Z Px Py Pz )
 //* Parameters, covariance matrix, charge and PID hypothesis should be provided 

  void Create( const float_v Param[], const float_v Cov[], int_v Charge, float_v mass /*Int_t PID*/ );

  void SetOneEntry(int iEntry, KFParticleSIMD& part, int iEntryPart);

  KFParticleSIMD( const KFPTrack *track, Int_t PID );
  KFParticleSIMD( KFPTrack* Track[], int NTracks, const Int_t *pdg=0 );
  KFParticleSIMD( KFPTrackVector &track, uint_v& index, const int_v& pdg );

  void Create(KFPTrack* Track[], int NTracks, const Int_t *pdg=0);
  void Create(KFPTrackVector &track, uint_v& index, const int_v& pdg);
  void Load(KFPTrackVector &track, int index, const int_v& pdg);
  void Rotate();

  KFParticleSIMD(KFPTrack &Track, const Int_t *pdg=0);
  KFParticleSIMD(KFPTrackVector &track, int n, const Int_t *pdg=0);

  KFParticleSIMD( KFPEmcCluster &track, uint_v& index, const KFParticleSIMD& vertexGuess );
  KFParticleSIMD( KFPEmcCluster &track, int index, const KFParticleSIMD& vertexGuess );
  void Create( KFPEmcCluster &track, uint_v& index, const KFParticleSIMD& vertexGuess );
  void Load( KFPEmcCluster &track, int index, const KFParticleSIMD& vertexGuess );

    
  //* Initialisation from VVertex 

  KFParticleSIMD( const KFPVertex &vertex );
  KFParticleSIMD( KFParticle* part[], const int nPart = 0 );
  KFParticleSIMD( KFParticle &part );

  //*
  //*  ACCESSORS
  //*

  //* Simple accessors 

  float_v GetX    () const ; ///< Retruns X coordinate of the particle, fP[0].
  float_v GetY    () const ; ///< Retruns Y coordinate of the particle, fP[1].
  float_v GetZ    () const ; ///< Retruns Z coordinate of the particle, fP[2].
  float_v GetPx   () const ; ///< Retruns X component of the momentum, fP[3].
  float_v GetPy   () const ; ///< Retruns Y component of the momentum, fP[4].
  float_v GetPz   () const ; ///< Retruns Z component of the momentum, fP[5].
  float_v GetE    () const ; ///< Returns energy of the particle, fP[6].
  float_v GetS    () const ; ///< Returns dS=l/p, l - decay length, fP[7], defined if production vertex is set.
  int_v   GetQ    () const ; ///< Returns charge of the particle.
  float_v GetChi2 () const ; ///< Returns Chi2 of the fit.
  int_v   GetNDF  () const ; ///< Returns number of decrease of freedom.

  Bool_t GetAtProductionVertex() const { return fAtProductionVertex; }  ///< Returns a flag which shows if the particle is located at the production point

  const float_v& X    () const { return fP[0]; } ///< Retruns X coordinate of the particle, fP[0].
  const float_v& Y    () const { return fP[1]; } ///< Retruns Y coordinate of the particle, fP[1].
  const float_v& Z    () const { return fP[2]; } ///< Retruns Z coordinate of the particle, fP[2].
  const float_v& Px   () const { return fP[3]; } ///< Retruns X component of the momentum, fP[3].
  const float_v& Py   () const { return fP[4]; } ///< Retruns Y component of the momentum, fP[4].
  const float_v& Pz   () const { return fP[5]; } ///< Retruns Z component of the momentum, fP[5].
  const float_v& E    () const { return fP[6]; } ///< Returns energy of the particle, fP[6].
  const float_v& S    () const { return fP[7]; } ///< Returns dS=l/p, l - decay length, fP[7], defined if production vertex is set.
  const int_v  & Q    () const { return fQ;    } ///< Returns charge of the particle.
  const float_v& Chi2 () const { return fChi2; } ///< Returns Chi2 of the fit.
  const int_v& NDF  () const { return fNDF;  }   ///< Returns number of decrease of freedom.
  
  float_v GetParameter ( int i ) const ;        ///< Returns P[i] parameter.
  float_v GetCovariance( int i ) const ;        ///< Returns C[i] element of the covariance matrix in the lower triangular form.
  float_v GetCovariance( int i, int j ) const ; ///< Returns C[i,j] element of the covariance matrix.

  //* Accessors with calculations, value returned w/o error flag
  
  float_v GetP             () const; ///< Returns momentum
  float_v GetPt            () const; ///< Returns transverse momentum
  float_v GetEta           () const; ///< Returns pseudorapidity
  float_v GetPhi           () const; ///< Returns the azimuthal angle phi 
  float_v GetMomentum      () const; ///< Returns momentum
  float_v GetMass          () const; ///< Returns mass
  float_v GetDecayLength   () const; ///< Returns decay length
  float_v GetDecayLengthXY () const; ///< Returns decay length in XY
  float_v GetLifeTime      () const; ///< Returns life time ctau [cm]
  float_v GetR             () const; ///< Returns distance to the origin of the coordinate system {0,0,0}
  float_v GetRapidity() const { return float_v(0.5f)*log((fP[6] + fP[5])/(fP[6] - fP[5])); }  ///< Returns rapidity of the particle
  
  //* Accessors to estimated errors

  float_v GetErrX             () const ; ///< Returns the error of X of current position 
  float_v GetErrY             () const ; ///< Returns the error of Y of current position
  float_v GetErrZ             () const ; ///< Returns the error of Z of current position
  float_v GetErrPx            () const ; ///< Returns the error of X-compoment of the particle momentum
  float_v GetErrPy            () const ; ///< Returns the error of Y-compoment of the particle momentum
  float_v GetErrPz            () const ; ///< Returns the error of Z-compoment of the particle momentum
  float_v GetErrE             () const ; ///< Returns the error of energy
  float_v GetErrS             () const ; ///< Returns the error of decay length / momentum
  float_v GetErrP             () const ; ///< Returns the error of momentum
  float_v GetErrPt            () const ; ///< Returns the error of transverse momentum
  float_v GetErrEta           () const ; ///< Returns the error of pseudorapidity
  float_v GetErrPhi           () const ; ///< Returns the error of the azimuthal angle phi 
  float_v GetErrMomentum      () const ; ///< Returns the error of momentum
  float_v GetErrMass          () const ; ///< Returns the error of mass
  float_v GetErrDecayLength   () const ; ///< Returns the error of decay length
  float_v GetErrDecayLengthXY () const ; ///< Returns the error of decay length in XY
  float_v GetErrLifeTime      () const ; ///< Returns the error of life time
  float_v GetErrR             () const ; ///< Returns the error of distance to the origin of the coordinate system {0,0,0}

  //* Accessors with calculations( &value, &estimated sigma )
  //* error flag returned (0 means no error during calculations) 

  float_m GetP           ( float_v &P, float_v &SigmaP ) const ;   //* momentum
  float_m GetPt          ( float_v &Pt, float_v &SigmaPt ) const ; //* transverse momentum
  float_m GetEta         ( float_v &Eta, float_v &SigmaEta ) const ; //* pseudorapidity
  float_m GetPhi         ( float_v &Phi, float_v &SigmaPhi ) const ; //* phi
  float_m GetMomentum    ( float_v &P, float_v &SigmaP ) const ;   //* momentum
  float_m GetMass        ( float_v &M, float_v &SigmaM ) const ;   //* mass
  float_m GetDecayLength ( float_v &L, float_v &SigmaL ) const ;   //* decay length
  float_m GetDecayLengthXY ( float_v &L, float_v &SigmaL ) const ;   //* decay length in XY
  float_m GetLifeTime    ( float_v &T, float_v &SigmaT ) const ;   //* life time
  float_m GetR           ( float_v &R, float_v &SigmaR ) const ; //* R


  //*
  //*  MODIFIERS
  //*
  
  float_v & X    () ; ///< Modifier of X coordinate of the particle, fP[0].
  float_v & Y    () ; ///< Modifier of Y coordinate of the particle, fP[1].
  float_v & Z    () ; ///< Modifier of Z coordinate of the particle, fP[2].
  float_v & Px   () ; ///< Modifier of X component of the momentum, fP[3].
  float_v & Py   () ; ///< Modifier of Y component of the momentum, fP[4].
  float_v & Pz   () ; ///< Modifier of Z component of the momentum, fP[5].
  float_v & E    () ; ///< Modifier of energy of the particle, fP[6].
  float_v & S    () ; ///< Modifier of dS=l/p, l - decay length, fP[7], defined if production vertex is set.
  int_v   & Q    () ; ///< Modifier of charge of the particle.
  float_v & Chi2 () ; ///< Modifier of Chi2 of the fit.
  int_v & NDF  () ;   ///< Modifier of number of decrease of freedom.

  float_v & Parameter ( int i ) ;         ///< Modifier of P[i] parameter.
  float_v & Covariance( int i ) ;         ///< Modifier of C[i] element of the covariance matrix in the lower triangular form.
  float_v & Covariance( int i, int j ) ;  ///< Modifier of C[i,j] element of the covariance matrix.
  float_v * Parameters () ;               ///< Returns pointer to the parameters fP
  float_v * CovarianceMatrix() ;          ///< Returns pointer to the covariance matrix fC

  void GetKFParticle( KFParticle &Part, int iPart = 0);
  void GetKFParticle( KFParticle *Part, int nPart = 0);

  //* 
  //* CONSTRUCTION OF THE PARTICLE BY ITS DAUGHTERS AND MOTHER
  //* USING THE KALMAN FILTER METHOD
  //*


  //* Add daughter to the particle 

  void AddDaughter( const KFParticleSIMD &Daughter );

  //* Add daughter via += operator: ex.{ D0; D0+=Pion; D0+= Kaon; }

  void operator +=( const KFParticleSIMD &Daughter );  

  //* Everything in one go  

  void Construct( const KFParticleSIMD *vDaughters[], int nDaughters, 
                  const KFParticleSIMD *ProdVtx=0,   Float_t Mass=-1 );

  //*
  //*                   TRANSPORT
  //* 
  //*  ( main transportation parameter is S = SignedPath/Momentum )
  //*  ( parameters of decay & production vertices are stored locally )
  //*

  //* Transport the particle close to xyz[] point 

  void TransportToPoint( const float_v xyz[] );

  //* Transport the particle close to VVertex  
#ifdef HomogeneousField
  void TransportToVertex( const KFPVertex &v );
#endif
  //* Transport the particle close to another particle p 

  void TransportToParticle( const KFParticleSIMD &p );

  //* Get dS to a certain space point 

  float_v GetDStoPoint( const float_v xyz[3], float_v dsdr[6] ) const ;
  
  //* Get dS to other particle p (dSp for particle p also returned) 

  void GetDStoParticle( const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] ) const ;
  void GetDStoParticleFast( const KFParticleBaseSIMD &p, float_v dS[2] ) const ;
  //* 
  //* OTHER UTILITIES
  //*
 
  //* Calculate distance from another object [cm] in XY-plane

  float_m GetDistanceFromVertexXY( const float_v vtx[], float_v &val, float_v &err ) const ;
  float_m GetDistanceFromVertexXY( const float_v vtx[], const float_v Cv[], float_v &val, float_v &err ) const ;
  float_m GetDistanceFromVertexXY( const KFParticleSIMD &Vtx, float_v &val, float_v &err ) const ;
#ifdef HomogeneousField
  float_m GetDistanceFromVertexXY( const KFPVertex &Vtx, float_v &val, float_v &err ) const ;
#endif

  float_v GetDistanceFromVertexXY( const float_v vtx[] ) const ;
  float_v GetDistanceFromVertexXY( const KFParticleSIMD &Vtx ) const ;
#ifdef HomogeneousField
  float_v GetDistanceFromVertexXY( const KFPVertex &Vtx ) const ;
#endif
  float_v GetDistanceFromParticleXY( const KFParticleSIMD &p ) const ;

  //* Calculate sqrt(Chi2/ndf) deviation from another object in XY plane
  //* ( v = [xyz]-vertex, Cv=[Cxx,Cxy,Cyy,Cxz,Cyz,Czz]-covariance matrix )

  float_v GetDeviationFromVertexXY( const float_v v[], const float_v Cv[]=0 ) const ;
  float_v GetDeviationFromVertexXY( const KFParticleSIMD &Vtx ) const ;
#ifdef HomogeneousField
  float_v GetDeviationFromVertexXY( const KFPVertex &Vtx ) const ;
#endif
  float_v GetDeviationFromParticleXY( const KFParticleSIMD &p ) const ;

  //* Calculate opennig angle between two particles

  float_v GetAngle  ( const KFParticleSIMD &p ) const ;
  float_v GetAngleXY( const KFParticleSIMD &p ) const ;
  float_v GetAngleRZ( const KFParticleSIMD &p ) const ;

    // * Pseudo Proper Time of decay = (r*pt) / |pt| * M/|pt|
    // @primVertex - primary vertex
    // @mass - mass of the mother particle (in the case of "Hb -> JPsi" it would be JPsi mass)
    // @*timeErr2 - squared error of the decay time. If timeErr2 = 0 it isn't calculated
  float_v GetPseudoProperDecayTime( const KFParticleSIMD &primVertex, const float_v& mass, float_v* timeErr2 = 0 ) const;

  void GetFieldValue( const float_v xyz[], float_v B[] ) const ;
  
  void Transport( float_v dS, const float_v* dsdr, float_v P[], float_v C[], float_v* dsdr1=0, float_v* F=0, float_v* F1=0  ) const ;
  void TransportFast( float_v dS, float_v P[] ) const ;
  
 protected: 
  
  //*
  //*  INTERNAL STUFF
  //* 

  //* Method to access ALICE field 
#ifdef HomogeneousField
  static float_v GetFieldAlice();
#endif
  //* Other methods required by the abstract KFParticleBaseSIMD class 
  
 private:
#ifdef HomogeneousField
  static float_v fgBz;  ///< Bz compoment of the magnetic field (is defined in case of #ifdef HomogeneousField)
#endif
#ifdef NonhomogeneousField
  /** \brief Approximation of the magnetic field along the track trajectory.
   ** Each component (Bx, By, Bz) is approximated with the parabola depending on Z coordinate. Is defined in case of #ifdef NonhomogeneousField.
   **/
  KFParticleFieldRegion fField;
#endif
};



//---------------------------------------------------------------------
//
//     Inline implementation of the KFParticleSIMD methods
//
//---------------------------------------------------------------------

#ifdef HomogeneousField
inline void KFParticleSIMD::SetField( float_v Bz )
{ 
  /** Sets the constant homogemeous one-component magnetic field Bz (is defined in case of #ifdef HomogeneousField).
   ** \param[in] Bz - Z-component of the magnetic field
   **/
  fgBz = Bz;
}
#endif

inline KFParticleSIMD::KFParticleSIMD( const KFParticleSIMD &d1, 
                                       const KFParticleSIMD &d2, 
                                       const KFParticleSIMD &d3 ): KFParticleBaseSIMD()
#ifdef NonhomogeneousField
                                       , fField()
#endif
{
  /** Constructs a particle from three input daughter particles
   ** \param[in] d1 - the first daughter particle
   ** \param[in] d2 - the second daughter particle
   ** \param[in] d3 - the third daughter particle
   **/
    
  KFParticleSIMD mother;
  mother+= d1;
  mother+= d2;
  mother+= d3;
  *this = mother;
}

inline KFParticleSIMD::KFParticleSIMD( const KFParticleSIMD &d1, 
                               const KFParticleSIMD &d2, 
                               const KFParticleSIMD &d3, 
                               const KFParticleSIMD &d4 ): KFParticleBaseSIMD()
#ifdef NonhomogeneousField
                                                          , fField()
#endif
{
  /** Constructs a particle from four input daughter particles
   ** \param[in] d1 - the first daughter particle
   ** \param[in] d2 - the second daughter particle
   ** \param[in] d3 - the third daughter particle
   ** \param[in] d4 - the fourth daughter particle
   **/
  
  KFParticleSIMD mother;
  mother+= d1;
  mother+= d2;
  mother+= d3;
  mother+= d4;
  *this = mother;
}

inline float_v KFParticleSIMD::GetX    () const 
{ 
  return KFParticleBaseSIMD::GetX();    
}

inline float_v KFParticleSIMD::GetY    () const 
{ 
  return KFParticleBaseSIMD::GetY();    
}

inline float_v KFParticleSIMD::GetZ    () const 
{ 
  return KFParticleBaseSIMD::GetZ();    
}

inline float_v KFParticleSIMD::GetPx   () const 
{ 
  return KFParticleBaseSIMD::GetPx();   
}

inline float_v KFParticleSIMD::GetPy   () const 
{ 
  return KFParticleBaseSIMD::GetPy();   
}

inline float_v KFParticleSIMD::GetPz   () const 
{ 
  return KFParticleBaseSIMD::GetPz();   
}

inline float_v KFParticleSIMD::GetE    () const 
{ 
  return KFParticleBaseSIMD::GetE();    
}

inline float_v KFParticleSIMD::GetS    () const 
{ 
  return KFParticleBaseSIMD::GetS();    
}

inline int_v    KFParticleSIMD::GetQ    () const 
{ 
  return KFParticleBaseSIMD::GetQ();    
}

inline float_v KFParticleSIMD::GetChi2 () const 
{ 
  return KFParticleBaseSIMD::GetChi2(); 
}

inline int_v    KFParticleSIMD::GetNDF  () const 
{ 
  return KFParticleBaseSIMD::GetNDF();  
}

inline float_v KFParticleSIMD::GetParameter ( int i ) const 
{ 
  return KFParticleBaseSIMD::GetParameter(i);  
}

inline float_v KFParticleSIMD::GetCovariance( int i ) const 
{ 
  return KFParticleBaseSIMD::GetCovariance(i); 
}

inline float_v KFParticleSIMD::GetCovariance( int i, int j ) const 
{ 
  return KFParticleBaseSIMD::GetCovariance(i,j);
}


inline float_v KFParticleSIMD::GetP    () const
{
  float_v par, err;
  KFParticleBaseSIMD::GetMomentum( par, err );
  return par;
}

inline float_v KFParticleSIMD::GetPt   () const
{
  float_v par, err;
  KFParticleBaseSIMD::GetPt( par, err ) ;
  return par;
}

inline float_v KFParticleSIMD::GetEta   () const
{
  float_v par, err;
  KFParticleBaseSIMD::GetEta( par, err );
  return par;
}

inline float_v KFParticleSIMD::GetPhi   () const
{
  float_v par, err;
  KFParticleSIMD::GetPhi( par, err );
  return par;
}

inline float_v KFParticleSIMD::GetMomentum    () const
{
  float_v par, err;
  KFParticleSIMD::GetMomentum( par, err );
  return par;
}

inline float_v KFParticleSIMD::GetMass        () const
{
  float_v par, err;
  KFParticleSIMD::GetMass( par, err );
  return par;
}

inline float_v KFParticleSIMD::GetDecayLength () const
{
  float_v par, err;
  KFParticleSIMD::GetDecayLength( par, err );
  return par;
}

inline float_v KFParticleSIMD::GetDecayLengthXY () const
{
  float_v par, err;
  KFParticleSIMD::GetDecayLengthXY( par, err );
  return par;
}

inline float_v KFParticleSIMD::GetLifeTime    () const
{
  float_v par, err;
  KFParticleSIMD::GetLifeTime( par, err );
  return par;
}

inline float_v KFParticleSIMD::GetR   () const
{
  float_v par, err;
  KFParticleSIMD::GetR( par, err );
  return par;
}

inline float_v KFParticleSIMD::GetErrX           () const 
{
  return sqrt(abs( GetCovariance(0,0) ));
}

inline float_v KFParticleSIMD::GetErrY           () const 
{
  return sqrt(abs( GetCovariance(1,1) ));
}

inline float_v KFParticleSIMD::GetErrZ           () const 
{
  return sqrt(abs( GetCovariance(2,2) ));
}

inline float_v KFParticleSIMD::GetErrPx          () const 
{
  return sqrt(abs( GetCovariance(3,3) ));
}

inline float_v KFParticleSIMD::GetErrPy          () const 
{
  return sqrt(abs( GetCovariance(4,4) ));
}

inline float_v KFParticleSIMD::GetErrPz          () const 
{
  return sqrt(abs( GetCovariance(5,5) ));
}

inline float_v KFParticleSIMD::GetErrE           () const 
{
  return sqrt(abs( GetCovariance(6,6) ));
}

inline float_v KFParticleSIMD::GetErrS           () const 
{
  return sqrt(abs( GetCovariance(7,7) ));
}

inline float_v KFParticleSIMD::GetErrP    () const
{
  float_v par, err;
  float_m mask = KFParticleSIMD::GetMomentum( par, err );
  float_v ret(1.e10f);
  ret(!mask) = err;
  return ret;
}

inline float_v KFParticleSIMD::GetErrPt    () const
{
  float_v par, err;
  float_m mask = KFParticleSIMD::GetPt( par, err );
  float_v ret(1.e10f);
  ret(!mask) = err;
  return ret;
}

inline float_v KFParticleSIMD::GetErrEta    () const
{
  float_v par, err;
  float_m mask = KFParticleSIMD::GetEta( par, err );
  float_v ret(1.e10f);
  ret(!mask) = err;
  return ret;
}

inline float_v KFParticleSIMD::GetErrPhi    () const
{
  float_v par, err;
  float_m mask = KFParticleSIMD::GetPhi( par, err );
  float_v ret(1.e10f);
  ret(!mask) = err;
  return ret;
}

inline float_v KFParticleSIMD::GetErrMomentum    () const
{
  float_v par, err;
  float_m mask = KFParticleSIMD::GetMomentum( par, err );
  float_v ret(1.e10f);
  ret(!mask) = err;
  return ret;
}

inline float_v KFParticleSIMD::GetErrMass        () const
{
  float_v par, err;
  float_m mask = KFParticleSIMD::GetMass( par, err );
  float_v ret(1.e10f);
  ret(!mask) = err;
  return ret;
}

inline float_v KFParticleSIMD::GetErrDecayLength () const
{
  float_v par, err;
  float_m mask = KFParticleSIMD::GetDecayLength( par, err );
  float_v ret(1.e10f);
  ret(!mask) = err;
  return ret;
}

inline float_v KFParticleSIMD::GetErrDecayLengthXY () const
{
  float_v par, err;
  float_m mask = KFParticleSIMD::GetDecayLengthXY( par, err );
  float_v ret(1.e10f);
  ret(!mask) = err;
  return ret;
}

inline float_v KFParticleSIMD::GetErrLifeTime    () const
{
  float_v par, err;
  float_m mask = KFParticleSIMD::GetLifeTime( par, err );
  float_v ret(1.e10f);
  ret(!mask) = err;
  return ret;
}

inline float_v KFParticleSIMD::GetErrR    () const
{
  float_v par, err;
  float_m mask = KFParticleSIMD::GetR( par, err );
  float_v ret(1.e10f);
  ret(!mask) = err;
  return ret;
}


inline float_m KFParticleSIMD::GetP( float_v &P, float_v &SigmaP ) const 
{
  /** Calculates particle momentum and its error. If they are well defined returns 0, otherwise 1.
   ** \param[out] P - momentum of the particle
   ** \param[out] SigmaP - its error
   **/
  return KFParticleBaseSIMD::GetMomentum( P, SigmaP );
}

inline float_m KFParticleSIMD::GetPt( float_v &Pt, float_v &SigmaPt ) const 
{
  /** Calculates particle transverse  momentum and its error. If they are well defined returns 0, otherwise 1.
   ** \param[out] Pt - transverse momentum of the particle
   ** \param[out] SigmaPt - its error
   **/
  return KFParticleBaseSIMD::GetPt( Pt, SigmaPt );
}

inline float_m KFParticleSIMD::GetEta( float_v &Eta, float_v &SigmaEta ) const 
{
  /** Calculates particle pseudorapidity and its error. If they are well defined returns 0, otherwise 1.
   ** \param[out] Eta - pseudorapidity of the particle
   ** \param[out] SigmaEta - its error
   **/
  return KFParticleBaseSIMD::GetEta( Eta, SigmaEta );
}

inline float_m KFParticleSIMD::GetPhi( float_v &Phi, float_v &SigmaPhi ) const 
{
  /** Calculates particle polar angle at the current point and its error. If they are well defined returns 0, otherwise 1.
   ** \param[out] Phi - polar angle of the particle
   ** \param[out] SigmaPhi - its error
   **/
  return KFParticleBaseSIMD::GetPhi( Phi, SigmaPhi );
}

inline float_m KFParticleSIMD::GetMomentum( float_v &P, float_v &SigmaP ) const 
{
  /** Calculates particle momentum and its error. If they are well defined returns 0, otherwise 1.
   ** \param[out] P - momentum of the particle
   ** \param[out] SigmaP - its error
   **/
  return KFParticleBaseSIMD::GetMomentum( P, SigmaP );
}

inline float_m KFParticleSIMD::GetMass( float_v &M, float_v &SigmaM ) const 
{
  /** Calculates the mass of the particle and its error. If they are well defined returns 0, otherwise 1.
   ** \param[out] M - mass of the particle
   ** \param[out] SigmaM - its error
   **/
  return KFParticleBaseSIMD::GetMass( M, SigmaM );
}

inline float_m KFParticleSIMD::GetDecayLength( float_v &L, float_v &SigmaL ) const 
{
  /** Calculates the decay length of the particle in the laboratory system and its error. If they are well defined returns 0, otherwise 1.
   ** The production point should be set before calling this function.
   ** \param[out] L - the decay length
   ** \param[out] SigmaL - its error
   **/
  return KFParticleBaseSIMD::GetDecayLength( L, SigmaL );
}

inline float_m KFParticleSIMD::GetDecayLengthXY( float_v &L, float_v &SigmaL ) const 
{
  /** Calculates the projection in the XY plane of the decay length of the particle in the laboratory 
   ** system and its error. If they are well defined returns 0, otherwise 1.
   ** The production point should be set before calling this function.
   ** \param[out] L - the decay length
   ** \param[out] SigmaL - its error
   **/
  return KFParticleBaseSIMD::GetDecayLengthXY( L, SigmaL );
}

inline float_m KFParticleSIMD::GetLifeTime( float_v &T, float_v &SigmaT ) const 
{
  /** Calculates the lifetime times speed of life (ctau) [cm] of the particle in the  
   ** center of mass frame and its error. If they are well defined returns 0, otherwise 1.
   ** The production point should be set before calling this function.
   ** \param[out] T - lifetime of the particle [cm]
   ** \param[out] SigmaT - its error
   **/
  return KFParticleBaseSIMD::GetLifeTime( T, SigmaT );
}

inline float_m KFParticleSIMD::GetR( float_v &R, float_v &SigmaR ) const 
{
  /** Calculates the distance to the point {0,0,0} and its error. If they are well defined returns 0, otherwise 1.
   ** \param[out] R - polar angle of the particle
   ** \param[out] SigmaR - its error
   **/
  return KFParticleBaseSIMD::GetR( R, SigmaR );
}

inline float_v & KFParticleSIMD::X() 
{ 
  return KFParticleBaseSIMD::X();    
}

inline float_v & KFParticleSIMD::Y()
{ 
  return KFParticleBaseSIMD::Y();    
}

inline float_v & KFParticleSIMD::Z() 
{ 
  return KFParticleBaseSIMD::Z();    
}

inline float_v & KFParticleSIMD::Px() 
{ 
  return KFParticleBaseSIMD::Px();   
}

inline float_v & KFParticleSIMD::Py() 
{ 
  return KFParticleBaseSIMD::Py();   
}

inline float_v & KFParticleSIMD::Pz() 
{ 
  return KFParticleBaseSIMD::Pz();   
}

inline float_v & KFParticleSIMD::E() 
{ 
  return KFParticleBaseSIMD::E();    
}

inline float_v & KFParticleSIMD::S() 
{ 
  return KFParticleBaseSIMD::S();    
}

inline int_v    & KFParticleSIMD::Q() 
{ 
  return KFParticleBaseSIMD::Q();    
}

inline float_v & KFParticleSIMD::Chi2() 
{ 
  return KFParticleBaseSIMD::Chi2(); 
}

inline int_v    & KFParticleSIMD::NDF() 
{ 
  return KFParticleBaseSIMD::NDF();  
}

inline float_v & KFParticleSIMD::Parameter ( int i )        
{ 
  return KFParticleBaseSIMD::Parameter(i);
}

inline float_v & KFParticleSIMD::Covariance( int i )        
{ 
  return KFParticleBaseSIMD::Covariance(i);
}

inline float_v & KFParticleSIMD::Covariance( int i, int j ) 
{ 
  return KFParticleBaseSIMD::Covariance(i,j); 
}

inline float_v * KFParticleSIMD::Parameters ()
{
  return fP;
}

inline float_v * KFParticleSIMD::CovarianceMatrix()
{
  return fC;
}


inline void KFParticleSIMD::operator +=( const KFParticleSIMD &Daughter )
{
  /** Operator to add daughter to the current particle. Calls AddDaughter() function.
   ** \param[in] Daughter - the daughter particle
   **/
#ifdef NonhomogeneousField
  fField = Daughter.fField;
#endif
  KFParticleBaseSIMD::operator +=( Daughter );
}
  

inline void KFParticleSIMD::AddDaughter( const KFParticleSIMD &Daughter )
{
  /** Adds daughter to the current particle. Depending on the selected construction method uses: \n
   ** 1) Either simplifyed fast mathematics which consideres momentum and energy as
   ** independent variables and thus ignores constraint on the fixed mass (fConstructMethod = 0).
   ** In this case the mass of the daughter particle can be corrupted when the constructed vertex
   ** is added as the measurement and the mass of the output short-lived particle can become 
   ** unphysical - smaller then the threshold. Implemented in the 
   ** AddDaughterWithEnergyFit() function \n
   ** 2) Or slower but correct mathematics which requires that the masses of daughter particles 
   ** stays fixed in the construction process (fConstructMethod = 2). Implemented in the
   ** AddDaughterWithEnergyFitMC() function.
   ** \param[in] Daughter - the daughter particle
   **/
#ifdef NonhomogeneousField
  fField = Daughter.fField;
#endif
  KFParticleBaseSIMD::AddDaughter( Daughter );
}

inline void KFParticleSIMD::Construct( const KFParticleSIMD *vDaughters[], int nDaughters, 
                                       const KFParticleSIMD *ProdVtx,   Float_t Mass )
{
  /** Constructs a short-lived particle from a set of daughter particles:\n
   ** 1) all parameters of the "this" objects are initialised;\n
   ** 2) daughters are added one after another;\n
   ** 3) if Parent pointer is not null, the production vertex is set to it;\n
   ** 4) if Mass hypothesis >=0 the mass constraint is set.
   ** \param[in] vDaughters - array of daughter particles
   ** \param[in] nDaughters - number of daughter particles in the input array
   ** \param[in] Parent - optional parrent particle
   ** \param[in] Mass - optional mass hypothesis
   **/  
#ifdef NonhomogeneousField
  fField = vDaughters[0]->fField;
#endif
  KFParticleBaseSIMD::Construct( ( const KFParticleBaseSIMD**)vDaughters, nDaughters, 
                                 ( const KFParticleBaseSIMD*)ProdVtx, Mass );
}

inline void KFParticleSIMD::TransportToPoint( const float_v xyz[] )
{
  /** Transports particle to the distance of closest approach to the point xyz.
   ** \param[in] xyz[3] - point, where particle should be transported
   **/
  
  float_v dsdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  const float_v dS = GetDStoPoint(xyz, dsdr);
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
  float_v dsdr[4][6];
  float_v dS[2];
  GetDStoParticle( p, dS, dsdr );
  TransportToDS( dS[0], dsdr[0] );
}

inline float_v KFParticleSIMD::GetDStoPoint( const float_v xyz[3], float_v dsdr[6] ) const 
{
  /** Returns dS = l/p parameter, where \n
   ** 1) l - signed distance to the DCA point with the input xyz point;\n
   ** 2) p - momentum of the particle; \n
   ** Also calculates partial derivatives dsdr of the parameter dS over the state vector of the current particle.
   ** If "HomogeneousField" is defined KFParticleBaseSIMD::GetDStoPointBz() is called,
   ** if "NonhomogeneousField" is defined - KFParticleBaseSIMD::GetDStoPointCBM()
   ** \param[in] xyz[3] - point, to which particle should be transported
   ** \param[out] dsdr[6] = ds/dr partial derivatives of the parameter dS over the state vector of the current particle
   ** \param[in] param - optional parameter, is used in case if the parameters of the particle are rotated
   ** to other coordinate system (see GetDStoPointBy() function), otherwise fP are used
   **/
#ifdef HomogeneousField
  return KFParticleBaseSIMD::GetDStoPointBz( GetFieldAlice(), xyz, dsdr );
#endif
#ifdef NonhomogeneousField
  return KFParticleBaseSIMD::GetDStoPointCBM( xyz, dsdr );
#endif
}


#ifdef HomogeneousField
inline float_v KFParticleSIMD::GetFieldAlice()
{ 
  /** Returns value of the constant homogemeous one-component magnetic field Bz, (is defined in case of #ifdef HomogeneousField). */
  return fgBz; 
}
#endif

#ifdef HomogeneousField
inline void KFParticleSIMD::GetFieldValue( const float_v * /*xyz*/, float_v B[] ) const 
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
inline void KFParticleSIMD::GetFieldValue( const float_v xyz[], float_v B[] ) const 
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

inline void KFParticleSIMD::GetDStoParticle( const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] )const
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
   ** param2 are parameters of the second particle p.fP. If "HomogeneousField" is defined KFParticleBaseSIMD::GetDStoParticleBz() is called,
   ** if "NonhomogeneousField" is defined - KFParticleBaseSIMD::GetDStoParticleCBM()
   ** \param[in] p - second particle
   ** \param[out] dS[2] - transport parameters dS for the current particle (dS[0]) and the second particle "p" (dS[1])
   ** \param[out] dsdr[4][6] - partial derivatives of the parameters dS[0] and dS[1] over the state vectors of the both particles
   **/
#ifdef HomogeneousField
  KFParticleBaseSIMD::GetDStoParticleBz( GetFieldAlice(), p, dS, dsdr ) ;
#endif
#ifdef NonhomogeneousField
  KFParticleBaseSIMD::GetDStoParticleCBM( p, dS, dsdr ) ;
#endif
}

inline void KFParticleSIMD::GetDStoParticleFast( const KFParticleBaseSIMD &p, float_v dS[2] )const
{
  /** Calculates dS = l/p parameters for two particles, where \n
   ** 1) l - signed distance to the DCA point with the other particle;\n
   ** 2) p - momentum of the particleю \n
   ** dS[0] is the transport parameter for the current particle, dS[1] - for the particle "p".
   ** If "HomogeneousField" is defined KFParticleBaseSIMD::GetDStoParticleBz() is called,
   ** if "NonhomogeneousField" is defined - KFParticleBaseSIMD::GetDStoParticleCBM()
   ** \param[in] p - second particle
   ** \param[out] dS[2] - transport parameters dS for the current particle (dS[0]) and the second particle "p" (dS[1])
   **/
#ifdef HomogeneousField
  KFParticleBaseSIMD::GetDStoParticleBz( GetFieldAlice(), p, dS ) ;
#endif
#ifdef NonhomogeneousField
  KFParticleBaseSIMD::GetDStoParticleCBM( p, dS ) ;
#endif
}

inline void KFParticleSIMD::Transport( float_v dS, const float_v* dsdr, float_v P[], float_v C[], float_v* dsdr1, float_v* F, float_v* F1 ) const 
{
  /** Transports the parameters and their covariance matrix of the current particle
   ** on a length defined by the transport parameter dS = l/p, where l is the signed distance and p is 
   ** the momentum of the current particle. If "HomogeneousField" is defined KFParticleBaseSIMD::TransportBz()
   ** is called, if "NonhomogeneousField" - KFParticleBaseSIMD::TransportCBM().
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
  KFParticleBaseSIMD::TransportBz( GetFieldAlice(), dS, dsdr, P, C, dsdr1, F, F1 );
#endif
#ifdef NonhomogeneousField
  KFParticleBaseSIMD::TransportCBM( dS, dsdr, P, C, dsdr1, F, F1 );
#endif
}

inline void KFParticleSIMD::TransportFast( float_v dS, float_v P[] ) const 
{
  /** Transports the parametersof the current particle
   ** on a length defined by the transport parameter dS = l/p, where l is the signed distance and p is 
   ** the momentum of the current particle. If "HomogeneousField" is defined KFParticleBaseSIMD::TransportBz()
   ** is called, if "NonhomogeneousField" - KFParticleBaseSIMD::TransportCBM().
   ** The obtained parameters are stored to the array P.
   ** P can be set to the parameters fP of the current particle. In this
   ** case the particle parameters will be modified. 
   ** \param[in] dS - transport parameter which defines the distance to which particle should be transported
   ** \param[out] P[8] - array, where transported parameters should be stored
   **/ 
#ifdef HomogeneousField
  KFParticleBaseSIMD::TransportBz( GetFieldAlice(), dS, P );
#endif
#ifdef NonhomogeneousField
  KFParticleBaseSIMD::TransportCBM( dS, P );
#endif
}

#endif 
