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

//#define NonhomogeneousField
// #define HomogeneousField

#ifndef KFPARTICLE_H
#define KFPARTICLE_H

#ifdef __ROOT__ //for the STAR experiment
#define HomogeneousField
#endif

#ifdef HLTCA_STANDALONE
#include "RootTypesDef.h"
#else
#include "TObject.h"
#endif

#include <vector>
#include <cmath>

class KFPTrack;
class KFPVertex;

/** @class KFParticle
 ** @brief The main scalar class of KF Particle package, describes particle objects.
 ** @author  S.Gorbunov, I.Kisel, M.Zyzak
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The main scalar class of KF Particle pacakge, describes particle objects.
 ** The particle is described with the state vector { X, Y, Z, Px, Py, Pz, E }
 ** and the corresponding covariance matrix.
 ** It contains functionality to create particle-object from track, to construct 
 ** short-lived particles from other tracks or particles. The mathematics is 
 ** based on the Kalman filter method. It also allows to subtract particles from 
 ** the already constructed object,
 ** to transport particles, get parameters together with their errors, get distance 
 ** to other particles and vertices, get deviations from them in terms of errors, etc.
 **/
class KFParticle : public TObject
{
  
 public:

  //*
  //*  INITIALIZATION
  //*

  //* Set magnetic field for all particles
#ifdef HomogeneousField
  static void SetField( float Bz );
#endif
  //* Constructor (empty)

  KFParticle();

  //* Destructor (empty)

  virtual ~KFParticle() {}

 //* Initialisation from ALICE track, PID hypothesis shoould be provided 

  KFParticle( const KFPTrack &track, const int PID );


  //* Initialisation from VVertex 

  KFParticle( const KFPVertex &vertex );

  //* Initialise covariance matrix and set current parameters to 0.0 

  void Initialize();
  void Initialize( const float Param[], const float Cov[], Int_t Charge, float Mass );

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

#ifdef NonhomogeneousField
  const float* GetFieldCoeff() const { return fieldRegion; } ///< Returns the field approximation for the current particle
  void SetFieldCoeff(float c, int i) { fieldRegion[i] = c; } ///< Sets the field coefficient with index i.
#endif

  const float& X    () const { return fP[0]; } ///< Retruns X coordinate of the particle, fP[0].
  const float& Y    () const { return fP[1]; } ///< Retruns Y coordinate of the particle, fP[1].
  const float& Z    () const { return fP[2]; } ///< Retruns Z coordinate of the particle, fP[2].
  const float& Px   () const { return fP[3]; } ///< Retruns X component of the momentum, fP[3].
  const float& Py   () const { return fP[4]; } ///< Retruns Y component of the momentum, fP[4].
  const float& Pz   () const { return fP[5]; } ///< Retruns Z component of the momentum, fP[5].
  const float& E    () const { return fP[6]; } ///< Returns energy of the particle, fP[6].
  const float& S    () const { return fP[7]; } ///< Returns dS=l/p, l - decay length, fP[7], defined if production vertex is set.
  const char&  Q    () const { return fQ;    } ///< Returns charge of the particle.
  const float& Chi2 () const { return fChi2; } ///< Returns Chi2 of the fit.
  const Int_t& NDF  () const { return fNDF;  } ///< Returns number of decrease of freedom.
  
  float GetParameter ( int i )        const { return fP[i];       } ///< Returns P[i] parameter.
  float GetCovariance( int i )        const { return fC[i];       } ///< Returns C[i] element of the covariance matrix in the lower triangular form.
  float GetCovariance( int i, int j ) const { return fC[IJ(i,j)]; } ///< Returns C[i,j] element of the covariance matrix.

  //* Accessors with calculations, value returned w/o error flag
  
  float GetP             () const; ///< Returns momentum
  float GetPt            () const; ///< Returns transverse momentum
  float GetEta           () const; ///< Returns pseudorapidity
  float GetPhi           () const; ///< Returns the azimuthal angle phi 
  float GetMomentum      () const; ///< Returns momentum
  float GetMass          () const; ///< Returns mass
  float GetDecayLength   () const; ///< Returns decay length
  float GetDecayLengthXY () const; ///< Returns decay length in XY
  float GetLifeTime      () const; ///< Returns life time ctau [cm]
  float GetR             () const; ///< Returns distance to the origin of the coordinate system {0,0,0}

  //* Accessors to estimated errors

  float GetErrX             () const ; ///< Returns the error of X of current position 
  float GetErrY             () const ; ///< Returns the error of Y of current position
  float GetErrZ             () const ; ///< Returns the error of Z of current position
  float GetErrPx            () const ; ///< Returns the error of X-compoment of the particle momentum
  float GetErrPy            () const ; ///< Returns the error of Y-compoment of the particle momentum
  float GetErrPz            () const ; ///< Returns the error of Z-compoment of the particle momentum
  float GetErrE             () const ; ///< Returns the error of energy
  float GetErrS             () const ; ///< Returns the error of decay length / momentum
  float GetErrP             () const ; ///< Returns the error of momentum
  float GetErrPt            () const ; ///< Returns the error of transverse momentum
  float GetErrEta           () const ; ///< Returns the error of pseudorapidity
  float GetErrPhi           () const ; ///< Returns the error of the azimuthal angle phi 
  float GetErrMomentum      () const ; ///< Returns the error of momentum
  float GetErrMass          () const ; ///< Returns the error of mass
  float GetErrDecayLength   () const ; ///< Returns the error of decay length
  float GetErrDecayLengthXY () const ; ///< Returns the error of decay length in XY
  float GetErrLifeTime      () const ; ///< Returns the error of life time
  float GetErrR             () const ; ///< Returns the error of distance to the origin of the coordinate system {0,0,0}

  //* Accessors with calculations( &value, &estimated sigma )
  //* error flag returned (0 means no error during calculations) 

  int GetP             ( float &P, float &SigmaP ) const ;     //* momentum
  int GetPt            ( float &Pt, float &SigmaPt ) const ;   //* transverse momentum
  int GetEta           ( float &Eta, float &SigmaEta ) const ; //* pseudorapidity
  int GetPhi           ( float &Phi, float &SigmaPhi ) const ; //* phi
  int GetMomentum      ( float &P, float &SigmaP ) const ;     //* momentum
  int GetMass          ( float &M, float &SigmaM ) const ;     //* mass
  int GetDecayLength   ( float &L, float &SigmaL ) const ;     //* decay length
  int GetDecayLengthXY ( float &L, float &SigmaL ) const ;     //* decay length in XY
  int GetLifeTime      ( float &T, float &SigmaT ) const ;     //* life time
  int GetR             ( float &R, float &SigmaR ) const ;     //* R
  float GetRapidity() const { return 0.5*log((fP[6] + fP[5])/(fP[6] - fP[5])); } ///< Returns rapidity of the particle
  float GetTheta()    const { return atan2(GetPt(),fP[5]); } ///< Returns the polar angle in RZ


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

  float & Parameter ( int i )        { return fP[i];       }; ///< Modifier of P[i] parameter.
  float & Covariance( int i )        { return fC[i];       }; ///< Modifier of C[i] element of the covariance matrix in the lower triangular form.
  float & Covariance( int i, int j ) { return fC[IJ(i,j)]; }; ///< Modifier of C[i,j] element of the covariance matrix.

  const float * Parameters ()      const { return fP;}            ///< Returns pointer to the parameters fP
  const float * CovarianceMatrix() const { return fC;}            ///< Returns pointer to the covariance matrix fC

  void SetConstructMethod(Int_t m) {fConstructMethod = m;} ///< Defines the construction method for the current particle (see description of fConstructMethod).
  void SetMassHypo(float m) { fMassHypo = m;} ///< Sets the mass hypothesis to the particle, is used when fConstructMethod = 2.
  const float& GetMassHypo() const { return fMassHypo; } ///< Returns the mass hypothesis.
  const float& GetSumDaughterMass() const {return SumDaughterMass;} ///< Returns the sum of masses of the daughters.


  //* 
  //* CONSTRUCTION OF THE PARTICLE BY ITS DAUGHTERS AND MOTHER
  //* USING THE KALMAN FILTER METHOD
  //*


  //* Add daughter to the particle 
  bool GetMeasurement( const KFParticle& daughter, float m[], float V[], float D[3][3] ) ;

  void AddDaughter( const KFParticle &Daughter );
  void SubtractDaughter( const KFParticle &Daughter );
  void AddDaughterWithEnergyFit( const KFParticle &Daughter );
  void AddDaughterWithEnergyFitMC( const KFParticle &Daughter );

  void SubtractFromVertex( KFParticle &Vtx ) const;
  void SubtractFromParticle( KFParticle &Vtx ) const;

  //* Add daughter via += operator: ex.{ D0; D0+=Pion; D0+= Kaon; }

  void operator +=( const KFParticle &Daughter );  

  //* Set production vertex 

  void SetProductionVertex( const KFParticle &Vtx );

  //* Set mass constraint 

  void SetNonlinearMassConstraint( float Mass );
  void SetMassConstraint( float Mass, float SigmaMass = 0 );
  void SetMassConstraint( float *mP, float *mC, float mJ[7][7], float mass );

  //* Everything in one go  

  void Construct( const KFParticle *vDaughters[], int nDaughters, 
		  const KFParticle *ProdVtx=0,   float Mass=-1 );

  //*
  //*                   TRANSPORT
  //* 
  //*  ( main transportation parameter is S = SignedPath/Momentum )
  //*  ( parameters of decay & production vertices are stored locally )
  //*

  //* Transport the particle close to xyz[] point 

  void TransportToPoint( const float xyz[] );

  //* Transport the particle close to VVertex  
#ifdef HomogeneousField
  void TransportToVertex( const KFPVertex &v );
#endif
  //* Transport the particle close to another particle p 
  void TransportToParticle( const KFParticle &p );

  //* Get dS to a certain space point 
  float GetDStoPoint( const float xyz[3], float dsdr[6] ) const ;

  float GetDStoPointLine( const float xyz[3], float dsdr[6] ) const;
  float GetDStoPointBz( float B, const float xyz[3], float dsdr[6], const float* param=0) const;
  float GetDStoPointBy( float By, const float xyz[3], float dsdr[6] ) const;
  float GetDStoPointB( const float* B, const float xyz[3], float dsdr[6] ) const;
  float GetDStoPointCBM( const float xyz[3], float dsdr[6] ) const;

  //* Get dS to other particle p (dSp for particle p also returned) 
  void GetDStoParticle( const KFParticle &p, float dS[2], float dsdr[4][6] ) const ;
  
  void GetDStoParticleLine( const KFParticle &p, float dS[2], float dsdr[4][6] ) const ;
  void GetDStoParticleBz( float Bz, const KFParticle &p, float dS[2], float dsdr[4][6], const float* param1=0, const float* param2=0 ) const ;
  void GetDStoParticleBy( float B,  const KFParticle &p, float dS[2], float dsdr[4][6] ) const ;
  void GetDStoParticleCBM( const KFParticle &p, float dS[2], float dsdr[4][6] ) const ;

  void GetDStoCylinderBz( const float B, const float R, float dS[2]) const;

#ifdef HomogeneousField
  //* Get dS to a certain space point 
  void GetDStoCylinder( const float R, float dS[2]) const {
    GetDStoCylinderBz( GetFieldAlice(), R, dS);
  }
#endif

  //* 
  //* OTHER UTILITIES
  //*

  //* Calculate distance from another object [cm]

  void GetDistanceToVertexLine( const KFParticle &Vertex, float &l, float &dl) const;
  void GetDistanceToVertexLineWithDirection( const KFParticle &Vertex, float &l, float &dl) const;

  float GetDistanceFromVertex( const float vtx[] ) const;
  float GetDistanceFromVertex( const KFParticle &Vtx ) const;
  float GetDistanceFromParticle( const KFParticle &p ) const;

  //* Calculate sqrt(Chi2/ndf) deviation from vertex
  //* v = [xyz], Cv=[Cxx,Cxy,Cyy,Cxz,Cyz,Czz]-covariance matrix

  float GetDeviationFromVertex( const float v[], const float Cv[]=0 ) const;
  float GetDeviationFromVertex( const KFParticle &Vtx ) const;
  float GetDeviationFromParticle( const KFParticle &p ) const; 

  //* Calculate distance from another object [cm] in XY-plane

  Bool_t GetDistanceFromVertexXY( const float vtx[], float &val, float &err ) const ;
  Bool_t GetDistanceFromVertexXY( const float vtx[], const float Cv[], float &val, float &err ) const ;
  Bool_t GetDistanceFromVertexXY( const KFParticle &Vtx, float &val, float &err ) const ;
#ifdef HomogeneousField
  Bool_t GetDistanceFromVertexXY( const KFPVertex &Vtx, float &val, float &err ) const ;
#endif

  float GetDistanceFromVertexXY( const float vtx[] ) const ;
  float GetDistanceFromVertexXY( const KFParticle &Vtx ) const ;
#ifdef HomogeneousField
  float GetDistanceFromVertexXY( const KFPVertex &Vtx ) const ;
#endif
  float GetDistanceFromParticleXY( const KFParticle &p ) const ;

  //* Calculate sqrt(Chi2/ndf) deviation from another object in XY plane
  //* ( v = [xyz]-vertex, Cv=[Cxx,Cxy,Cyy,Cxz,Cyz,Czz]-covariance matrix )

  float GetDeviationFromVertexXY( const float v[], const float Cv[]=0 ) const ;
  float GetDeviationFromVertexXY( const KFParticle &Vtx ) const ;
#ifdef HomogeneousField
  float GetDeviationFromVertexXY( const KFPVertex &Vtx ) const ;
#endif
  float GetDeviationFromParticleXY( const KFParticle &p ) const ;

  //* Get parameters at an arbitrary reconstructed point taking into account its errors
  void GetParametersAtPoint(const float* point, const float* pointCov, float* m, float* mV);
  
  //* Calculate opennig angle between two particles

  float GetAngle  ( const KFParticle &p ) const ;
  float GetAngleXY( const KFParticle &p ) const ;
  float GetAngleRZ( const KFParticle &p ) const ;

  float GetPseudoProperDecayTime( const KFParticle &primVertex, const float& mass, float* timeErr2 = 0 ) const;

  void GetFieldValue( const float xyz[], float B[] ) const ;

  void Transport( float dS, const float* dsdr, float P[], float C[], float* dsdr1=0, float* F=0, float* F1=0, const bool fullC=true ) const ;

  //Transport functions
  void TransportToDS( float dS, const float* dsdr );
  void TransportBz( float Bz, float dS, const float* dsdr, float P[], float C[], float* dsdr1=0, float* F=0, float* F1=0, const bool fullC=true ) const;
  void TransportCBM( float dS, const float* dsdr, float P[], float C[], float* dsdr1=0, float* F=0, float* F1=0 ) const;  
  void TransportLine( float S, const float* dsdr, float P[], float C[], float* dsdr1, float* F, float* F1 ) const ;


  static void GetArmenterosPodolanski(const KFParticle& positive, const KFParticle& negative, float QtAlfa[2] );
  void RotateXY(float angle, float Vtx[3]);
  void Rotate(float angle, const KFParticle& axis);

  int Id() const { return fId; } ///< Returns Id of the particle.
  int NDaughters() const { return fDaughtersIds.size(); } ///< Returns number of daughter particles.
  const std::vector<int>& DaughterIds() const { return fDaughtersIds; } ///< Returns the vector with the indices of daughter particles.
  void CleanDaughtersId() { fDaughtersIds.clear(); } ///< Cleans the vector with the indices of daughter particles.
  
  void SetId( int id ) { fId = id; } ///< Sets the Id of the particle. After the construction of a particle should be set by user.
  void AddDaughterId( int id ) { fDaughtersIds.push_back(id); } ///< Adds index of the daughter particle. 

  void SetPDG ( int pdg ) { fPDG = pdg; } ///< Sets the PDG hypothesis.
  int GetPDG () const { return fPDG; } ///< Returns the PDG hypothesis.

#ifdef __ROOT__ //for the STAR experiment
  void  Print(Option_t *opt="") const;
  Int_t IdTruth() const { return fIdTruth;}
  Int_t QaTruth() const { return fQuality; }
  Int_t IdParentMcVx() const {return fIdParentMcVx;}
  Int_t IdParentVx()   const {return IdParentMcVx();}
  void  SetParentID(Int_t id=0) {fParentID = id;}
  Int_t GetParentID() const {return fParentID;}
  void  SetIdParentMcVx(Int_t id) {fIdParentMcVx = id;}
  void  SetIdTruth(Int_t idtru,Int_t qatru=0) {fIdTruth = (UShort_t) idtru; fQuality = (UShort_t) qatru;}
  void  Clear(Option_t * /*option*/ ="");
#endif

  static void InvertCholetsky3(float a[6]);

  /** Converts a pair of indices {i,j} of the covariance matrix to one index corresponding to the triangular form. */
  static Int_t IJ( Int_t i, Int_t j ){ 
    return ( j<=i ) ? i*(i+1)/2+j :j*(j+1)/2+i;
  }

  //*
  //*  INTERNAL STUFF
  //* 
 
 protected:
  //* Method to access ALICE field 
#ifdef HomogeneousField
  static float GetFieldAlice();
#endif

  static void MultQSQt( const float Q[], const float S[], float SOut[], const int kN );

  /** Return an element of the covariance matrix with {i,j} indices. */
  float & Cij( Int_t i, Int_t j ){ return fC[IJ(i,j)]; }

  float fP[8];           ///< Particle parameters { X, Y, Z, Px, Py, Pz, E, S[=DecayLength/P]}.
  float fC[36];          ///< Low-triangle covariance matrix of fP.
  float fChi2;           ///< Chi^2.
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

#ifdef HomogeneousField
  static float fgBz;  ///< Bz compoment of the magnetic field (is defined in case of #ifdef HomogeneousField)
#endif
#ifdef NonhomogeneousField
  /** \brief Approximation of the magnetic field along the track trajectory.
   ** Each component (Bx, By, Bz) is approximated with the parabola depending on Z coordinate. Is defined in case of #ifdef NonhomogeneousField.
   **/
  float fieldRegion[10];
#endif
  
#ifndef KFParticleStandalone
  ClassDef( KFParticle, 4 )
#endif
};





//---------------------------------------------------------------------
//
//     Inline implementation of the KFParticle methods
//
//---------------------------------------------------------------------

#ifdef HomogeneousField
inline void KFParticle::SetField( float Bz )
{ 
  /** Sets the constant homogemeous one-component magnetic field Bz (is defined in case of #ifdef HomogeneousField).
   ** \param[in] Bz - Z-component of the magnetic field
   **/
  fgBz = Bz;
}
#endif

inline void KFParticle::Initialize()
{ 
  /** Initialises the parameters by default: \n
   ** 1) all parameters are set to 0; \n
   ** 2) all elements of the covariance matrix are set to 0 except Cxx=Cyy=Czz=100; \n
   ** 3) Q = 0; \n
   ** 4) chi2 is set to 0; \n
   ** 5) NDF = -3, since 3 parameters should be fitted: X, Y, Z. 
   **/

  for( Int_t i=0; i<8; i++) fP[i] = 0;
  for(Int_t i=0;i<36;++i) fC[i]=0.;
  fC[0] = fC[2] = fC[5] = 100.;
  fC[35] = 1.;
  fNDF  = -3;
  fChi2 =  0.;
  fQ = 0;
  SumDaughterMass = 0;
  fMassHypo = -1;
}

inline void KFParticle::Initialize( const float Param[], const float Cov[], Int_t Charge, float Mass )
{
  /** Sets the parameters of the particle:
   **
   ** \param[in] Param[6] = { X, Y, Z, Px, Py, Pz } - position and momentum
   ** \param[in] Cov[21]  - lower-triangular part of the covariance matrix:@n
   ** \verbatim
             (  0  .  .  .  .  . )
             (  1  2  .  .  .  . )
   Cov[21] = (  3  4  5  .  .  . )
             (  6  7  8  9  .  . )
             ( 10 11 12 13 14  . )
             ( 15 16 17 18 19 20 )
   \endverbatim
   ** \param[in] Charge - charge of the particle in elementary charge units
   ** \param[in] mass - the mass hypothesis
   **/

  for( Int_t i=0; i<6 ; i++ ) fP[i] = Param[i];
  for( Int_t i=0; i<21; i++ ) fC[i] = Cov[i];

  float energy = sqrt( Mass*Mass + fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5]);
  fP[6] = energy;
  fP[7] = 0;
  fQ = Charge;
  fNDF = 0;
  fChi2 = 0;

  float energyInv = 1./energy;
  float 
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
  for( Int_t i=28; i<36; i++ ) fC[i] = 0;
  fC[35] = 1.;

  SumDaughterMass = Mass;
  fMassHypo = Mass;
}

inline float KFParticle::GetP    () const
{
  float par, err;
  if( GetMomentum( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetPt   () const
{
  float par, err;
  if( GetPt( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetEta   () const
{
  float par, err;
  if( GetEta( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetPhi   () const
{
  float par, err;
  if( GetPhi( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetMomentum    () const
{
  float par, err;
  if( GetMomentum( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetMass        () const
{
  float par, err;
  if( GetMass( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetDecayLength () const
{
  float par, err;
  if( GetDecayLength( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetDecayLengthXY () const
{
  float par, err;
  if( GetDecayLengthXY( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetLifeTime    () const
{
  float par, err;
  if( GetLifeTime( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetR   () const
{
  float par, err;
  if( GetR( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetErrX           () const 
{
  return sqrt(fabs( GetCovariance(0,0) ));
}

inline float KFParticle::GetErrY           () const 
{
  return sqrt(fabs( GetCovariance(1,1) ));
}

inline float KFParticle::GetErrZ           () const 
{
  return sqrt(fabs( GetCovariance(2,2) ));
}

inline float KFParticle::GetErrPx          () const 
{
  return sqrt(fabs( GetCovariance(3,3) ));
}

inline float KFParticle::GetErrPy          () const 
{
  return sqrt(fabs( GetCovariance(4,4) ));
}

inline float KFParticle::GetErrPz          () const 
{
  return sqrt(fabs( GetCovariance(5,5) ));
}

inline float KFParticle::GetErrE           () const 
{
  return sqrt(fabs( GetCovariance(6,6) ));
}

inline float KFParticle::GetErrS           () const 
{
  return sqrt(fabs( GetCovariance(7,7) ));
}

inline float KFParticle::GetErrP    () const
{
  float par, err;
  if( GetMomentum( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrPt    () const
{
  float par, err;
  if( GetPt( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrEta    () const
{
  float par, err;
  if( GetEta( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrPhi    () const
{
  float par, err;
  if( GetPhi( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrMomentum    () const
{
  float par, err;
  if( GetMomentum( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrMass        () const
{
  float par, err;
  if( GetMass( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrDecayLength () const
{
  float par, err;
  if( GetDecayLength( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrDecayLengthXY () const
{
  float par, err;
  if( GetDecayLengthXY( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrLifeTime    () const
{
  float par, err;
  if( GetLifeTime( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrR    () const
{
  float par, err;
  if( GetR( par, err ) ) return 1.e10;
  else return err;
}


inline int KFParticle::GetP( float &P, float &SigmaP ) const 
{
  /** Calculates particle momentum and its error. If they are well defined returns 0, otherwise 1.
   ** \param[out] P - momentum of the particle
   ** \param[out] SigmaP - its error
   **/
  return GetMomentum( P, SigmaP );
}

inline void KFParticle::TransportToPoint( const float xyz[] )
{ 
  /** Transports particle to the distance of closest approach to the point xyz.
   ** \param[in] xyz[3] - point, where particle should be transported
   **/
  float dsdr[6] = {0.f};
  float dS = GetDStoPoint(xyz, dsdr);
  TransportToDS( dS, dsdr );
}
#ifdef HomogeneousField
inline void KFParticle::TransportToVertex( const KFPVertex &v )
{ 
  /** Transports particle to the distance of closest approach to the vertex v.
   ** \param[in] v - vertex, where particle should be transported
   **/
  TransportToPoint( KFParticle(v).fP );
}
#endif
inline void KFParticle::TransportToParticle( const KFParticle &p )
{ 
  /** Transports particle to the distance of closest approach to the particle p.
   ** \param[in] p - particle, to which the current particle should be transported.
   **/
  float dsdr[4][6];
  float dS[2];
  GetDStoParticle( p, dS, dsdr );
  TransportToDS( dS[0], dsdr[0] );
}

inline float KFParticle::GetDStoPoint( const float xyz[], float* dsdr ) const 
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
inline float KFParticle::GetFieldAlice()
{ 
  /** Returns value of the constant homogemeous one-component magnetic field Bz, (is defined in case of #ifdef HomogeneousField). */
  return fgBz; 
}
#endif

#ifdef HomogeneousField
inline void KFParticle::GetFieldValue( const float * /*xyz*/, float B[] ) const 
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
inline void KFParticle::GetFieldValue( const float xyz[], float B[] ) const 
{
  /** Calculates the Bx, By, Bz components at the point xyz using approximation of the
   ** magnetic field along the particle trajectory.
   ** \param[in] xyz[3] - X, Y, Z coordiantes of the point where the magnetic field should be calculated
   ** \param[out] B[3] - value of X, Y, Z components of the calculated magnetic field at the given point
   **/
  
  const float dz = (xyz[2]-fieldRegion[9]);
  const float dz2 = dz*dz;

  B[0] = fieldRegion[0] + fieldRegion[1]*dz + fieldRegion[2]*dz2;
  B[1] = fieldRegion[3] + fieldRegion[4]*dz + fieldRegion[5]*dz2;
  B[2] = fieldRegion[6] + fieldRegion[7]*dz + fieldRegion[8]*dz2;
}
#endif

inline void KFParticle::GetDStoParticle( const KFParticle &p, float dS[2], float dsdr[4][6] ) const
{ 
  /** Calculates dS = l/p parameters for two particles, where \n
   ** 1) l - signed distance to the DCA point with the other particle;\n
   ** 2) p - momentum of the particle \n
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

inline void KFParticle::Transport( float dS, const float* dsdr, float P[], float C[], float* dsdr1, float* F, float* F1, const bool fullC ) const 
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

#ifdef __ROOT__ //for the STAR experiment
std::ostream&  operator<<(std::ostream& os, KFParticle const & particle);
#endif

inline void KFParticle::InvertCholetsky3(float a[6])
{
  /** Inverts symmetric 3x3 matrix a using modified Choletsky decomposition. The result is stored to the same matrix a.
   ** \param[in,out] a - 3x3 symmetric matrix
   **/

  const float d0 = 1.f/a[0];
  const float u01 = a[1]*d0;
  const float u02 = a[3]*d0;
  
  const float d1 = 1.f/(a[2] - u01*a[1]);
  const float u12_d = a[4] - u01*a[3];
  const float u12 = d1*u12_d;  
  const float d2 = 1.f/(a[5] - u02*a[3] - u12*u12_d);
  
  //find V = -U^-1
  const float v02 = u02 - u01*u12;
  
  //find A^-1 = U^-1 D^-1 Ut^-1
  a[5] =  d2;
  a[4] = -d2*u12;
  a[3] = -d2*v02;
  const float d1u01 = -d1*u01;
  a[2] = d1    - a[4]*u12;
  a[1] = d1u01 - a[3]*u12;
  a[0] = d0 - d1u01*u01 - a[3]*v02;
}






#endif 
