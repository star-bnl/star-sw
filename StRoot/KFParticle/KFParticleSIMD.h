//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   20.08.13
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________


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

class KFParticleSIMD :public KFParticleBaseSIMD
{
  
 public:

  void *operator new(size_t size) { return _mm_malloc(size, sizeof(float_v)); }
  void *operator new[](size_t size) { return _mm_malloc(size, sizeof(float_v)); }
  void *operator new(size_t size, void *ptr) { return ::operator new(size, ptr);}
  void *operator new[](size_t size, void *ptr) { return ::operator new(size, ptr);}
  void operator delete(void *ptr, size_t) { _mm_free(ptr); }
  void operator delete[](void *ptr, size_t) { _mm_free(ptr); }
  //*
  //*  INITIALIZATION
  //*

  //* Set magnetic field for all particles
#ifdef HomogeneousField
  static void SetField( float_v Bz );
#endif
#ifdef NonhomogeneousField
  void SetField(const KFParticleFieldRegion &field)
  {
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

  ~KFParticleSIMD(){ ; }

  //* Construction of mother particle by its 2-3-4 daughters

  KFParticleSIMD( const KFParticleSIMD &d1, const KFParticleSIMD &d2 );

  KFParticleSIMD( const KFParticleSIMD &d1, const KFParticleSIMD &d2,  const KFParticleSIMD &d3 );

  KFParticleSIMD( const KFParticleSIMD &d1, const KFParticleSIMD &d2, const KFParticleSIMD &d3, const KFParticleSIMD &d4 );

 //* Initialisation from "cartesian" coordinates ( X Y Z Px Py Pz )
 //* Parameters, covariance matrix, charge and PID hypothesis should be provided 

  void Create( const float_v Param[], const float_v Cov[], int_v Charge, float_v mass /*Int_t PID*/ );

  void SetOneEntry(int iEntry, KFParticleSIMD& part, int iEntryPart);

  KFParticleSIMD( const KFPTrack &track, Int_t PID );
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

  //* Initialise covariance matrix and set current parameters to 0.0 

  void Initialize();

  //*
  //*  ACCESSORS
  //*

  //* Simple accessors 

  float_v GetX    () const ; //* x of current position
  float_v GetY    () const ; //* y of current position
  float_v GetZ    () const ; //* z of current position
  float_v GetPx   () const ; //* x-compoment of 3-momentum
  float_v GetPy   () const ; //* y-compoment of 3-momentum
  float_v GetPz   () const ; //* z-compoment of 3-momentum
  float_v GetE    () const ; //* energy
  float_v GetS    () const ; //* decay length / momentum
  int_v   GetQ    () const ; //* charge
  float_v GetChi2 () const ; //* chi^2
  int_v GetNDF  () const ; //* Number of Degrees of Freedom

  Bool_t GetAtProductionVertex() const { return fAtProductionVertex; }

  const float_v& X    () const { return fP[0]; }
  const float_v& Y    () const { return fP[1]; }
  const float_v& Z    () const { return fP[2]; }
  const float_v& Px   () const { return fP[3]; }
  const float_v& Py   () const { return fP[4]; }
  const float_v& Pz   () const { return fP[5]; }
  const float_v& E    () const { return fP[6]; }
  const float_v& S    () const { return fP[7]; }
  const int_v  & Q    () const { return fQ;    }
  const float_v& Chi2 () const { return fChi2; }
  const int_v& NDF  () const { return fNDF;  }
  
  float_v GetParameter ( int i ) const ;
  float_v GetCovariance( int i ) const ;
  float_v GetCovariance( int i, int j ) const ;

  //* Accessors with calculations, value returned w/o error flag
  
  float_v GetP           () const; //* momentum
  float_v GetPt          () const; //* transverse momentum
  float_v GetEta         () const; //* pseudorapidity
  float_v GetPhi         () const; //* phi
  float_v GetMomentum    () const; //* momentum (same as GetP() )
  float_v GetMass        () const; //* mass
  float_v GetDecayLength () const; //* decay length
  float_v GetDecayLengthXY () const; //* decay length in XY
  float_v GetLifeTime    () const; //* life time
  float_v GetR           () const; //* distance to the origin
  float_v GetRapidity() const { return float_v(0.5f)*log((fP[6] + fP[5])/(fP[6] - fP[5])); }
  
  //* Accessors to estimated errors

  float_v GetErrX           () const ; //* x of current position 
  float_v GetErrY           () const ; //* y of current position
  float_v GetErrZ           () const ; //* z of current position
  float_v GetErrPx          () const ; //* x-compoment of 3-momentum
  float_v GetErrPy          () const ; //* y-compoment of 3-momentum
  float_v GetErrPz          () const ; //* z-compoment of 3-momentum
  float_v GetErrE           () const ; //* energy
  float_v GetErrS           () const ; //* decay length / momentum
  float_v GetErrP           () const ; //* momentum
  float_v GetErrPt          () const ; //* transverse momentum
  float_v GetErrEta         () const ; //* pseudorapidity
  float_v GetErrPhi         () const ; //* phi
  float_v GetErrMomentum    () const ; //* momentum
  float_v GetErrMass        () const ; //* mass
  float_v GetErrDecayLength () const ; //* decay length
  float_v GetErrDecayLengthXY () const ; //* decay length in XY
  float_v GetErrLifeTime    () const ; //* life time
  float_v GetErrR           () const ; //* distance to the origin

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
  
  float_v & X    () ;
  float_v & Y    () ;
  float_v & Z    () ;
  float_v & Px   () ;
  float_v & Py   () ;
  float_v & Pz   () ;
  float_v & E    () ;
  float_v & S    () ;
  int_v   & Q    () ;
  float_v & Chi2 () ;
  int_v & NDF  () ;

  float_v & Parameter ( int i ) ;
  float_v & Covariance( int i ) ;
  float_v & Covariance( int i, int j ) ;
  float_v * Parameters () ;
  float_v * CovarianceMatrix() ;

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

  //* Set production vertex 

  void SetProductionVertex( const KFParticleSIMD &Vtx );

  //* Set mass constraint 

  void SetMassConstraint( float_v Mass, float_v SigmaMass = 0.f  );
  
  //* Set no decay length for resonances

  void SetNoDecayLength();

  //* Everything in one go  

  void Construct( const KFParticleSIMD *vDaughters[], int nDaughters, 
                  const KFParticleSIMD *ProdVtx=0,   Float_t Mass=-1 );

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

  //* Transport the particle close to xyz[] point 

  void TransportToPoint( const float_v xyz[] );

  //* Transport the particle close to VVertex  
#ifdef HomogeneousField
  void TransportToVertex( const KFPVertex &v );
#endif
  //* Transport the particle close to another particle p 

  void TransportToParticle( const KFParticleSIMD &p );

  //* Transport the particle on dS parameter (SignedPath/Momentum) 

  void TransportToDS( float_v dS, const float_v* dsdr );
  void TransportToDSLine( float_v dS, const float_v* dsdr );

  //* Get dS to a certain space point 

  float_v GetDStoPoint( const float_v xyz[3], float_v dsdr[6] ) const ;
  
  //* Get dS to other particle p (dSp for particle p also returned) 

  void GetDStoParticle( const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] ) const ;
  void GetDStoParticleFast( const KFParticleBaseSIMD &p, float_v dS[2] ) const ;
  //* 
  //* OTHER UTILITIES
  //*


  //* Calculate distance from another object [cm]

  float_v GetDistanceFromVertex( const float_v vtx[] ) const ;
  float_v GetDistanceFromVertex( const KFParticleSIMD &Vtx ) const ;
#ifdef HomogeneousField
  float_v GetDistanceFromVertex( const KFPVertex &Vtx ) const ;
#endif
  float_v GetDistanceFromParticle( const KFParticleSIMD &p ) const ;

  //* Calculate sqrt(Chi2/ndf) deviation from another object
  //* ( v = [xyz]-vertex, Cv=[Cxx,Cxy,Cyy,Cxz,Cyz,Czz]-covariance matrix )

  float_v GetDeviationFromVertex( const float_v v[], const float_v Cv[]=0 ) const ;
  float_v GetDeviationFromVertex( const KFParticleSIMD &Vtx ) const ;
#ifdef HomogeneousField
  float_v GetDeviationFromVertex( const KFPVertex &Vtx ) const ;
#endif
  float_v GetDeviationFromParticle( const KFParticleSIMD &p ) const ;
 
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

  //* Subtract the particle from the vertex  

  void SubtractFromVertex( KFParticleSIMD &v ) const ;
  void SubtractFromParticle( KFParticleSIMD &v ) const ;

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
  static float_v fgBz;  //* Bz compoment of the magnetic field
#endif
#ifdef NonhomogeneousField
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
  KFParticleSIMD mother;
  mother+= d1;
  mother+= d2;
  mother+= d3;
  mother+= d4;
  *this = mother;
}


inline void KFParticleSIMD::Initialize()
{ 
  KFParticleBaseSIMD::Initialize(); 
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
  return KFParticleBaseSIMD::GetMomentum( P, SigmaP );
}

inline float_m KFParticleSIMD::GetPt( float_v &Pt, float_v &SigmaPt ) const 
{
  return KFParticleBaseSIMD::GetPt( Pt, SigmaPt );
}

inline float_m KFParticleSIMD::GetEta( float_v &Eta, float_v &SigmaEta ) const 
{
  return KFParticleBaseSIMD::GetEta( Eta, SigmaEta );
}

inline float_m KFParticleSIMD::GetPhi( float_v &Phi, float_v &SigmaPhi ) const 
{
  return KFParticleBaseSIMD::GetPhi( Phi, SigmaPhi );
}

inline float_m KFParticleSIMD::GetMomentum( float_v &P, float_v &SigmaP ) const 
{
  return KFParticleBaseSIMD::GetMomentum( P, SigmaP );
}

inline float_m KFParticleSIMD::GetMass( float_v &M, float_v &SigmaM ) const 
{
  return KFParticleBaseSIMD::GetMass( M, SigmaM );
}

inline float_m KFParticleSIMD::GetDecayLength( float_v &L, float_v &SigmaL ) const 
{
  return KFParticleBaseSIMD::GetDecayLength( L, SigmaL );
}

inline float_m KFParticleSIMD::GetDecayLengthXY( float_v &L, float_v &SigmaL ) const 
{
  return KFParticleBaseSIMD::GetDecayLengthXY( L, SigmaL );
}

inline float_m KFParticleSIMD::GetLifeTime( float_v &T, float_v &SigmaT ) const 
{
  return KFParticleBaseSIMD::GetLifeTime( T, SigmaT );
}

inline float_m KFParticleSIMD::GetR( float_v &R, float_v &SigmaR ) const 
{
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
#ifdef NonhomogeneousField
  fField = Daughter.fField;
#endif
  KFParticleBaseSIMD::operator +=( Daughter );
}
  

inline void KFParticleSIMD::AddDaughter( const KFParticleSIMD &Daughter )
{
#ifdef NonhomogeneousField
  fField = Daughter.fField;
#endif
  KFParticleBaseSIMD::AddDaughter( Daughter );
}

inline void KFParticleSIMD::SetProductionVertex( const KFParticleSIMD &Vtx )
{
  KFParticleBaseSIMD::SetProductionVertex( Vtx );
}

inline void KFParticleSIMD::SetMassConstraint( float_v Mass, float_v SigmaMass )
{
  KFParticleBaseSIMD::SetMassConstraint( Mass, SigmaMass );
}

inline void KFParticleSIMD::SetNoDecayLength()
{
  KFParticleBaseSIMD::SetNoDecayLength();
}

inline void KFParticleSIMD::Construct( const KFParticleSIMD *vDaughters[], int nDaughters, 
                                       const KFParticleSIMD *ProdVtx,   Float_t Mass )
{
#ifdef NonhomogeneousField
  fField = vDaughters[0]->fField;
#endif
  KFParticleBaseSIMD::Construct( ( const KFParticleBaseSIMD**)vDaughters, nDaughters, 
                                 ( const KFParticleBaseSIMD*)ProdVtx, Mass );

//   #ifdef NonhomogeneousField
//   // calculate a field region for the constructed particle
//   KFParticleFieldValue field[3];
//   float_v zField[3] = {0.f, fP[2]/2, fP[2]};
// 
//   for(int iPoint=0; iPoint<3; iPoint++)
//   {
//     for(int iD=0; iD<nDaughters; ++iD)
//     {
//       KFParticleFieldValue b = const_cast<KFParticleSIMD *>(vDaughters[iD])->fField.Get(zField[iPoint]);
//       field[iPoint].x += b.x;
//       field[iPoint].y += b.y;
//       field[iPoint].z += b.z;
//     }
//     field[iPoint].x /= nDaughters;
//     field[iPoint].y /= nDaughters;
//     field[iPoint].z /= nDaughters;
//   }
// 
//   fField.Set( field[2], zField[2], field[1], zField[1], field[0], zField[0] );
//   #endif
}

inline void KFParticleSIMD::TransportToDecayVertex()
{
  KFParticleBaseSIMD::TransportToDecayVertex(); 
}

inline void KFParticleSIMD::TransportToProductionVertex()
{
  KFParticleBaseSIMD::TransportToProductionVertex();
}

inline void KFParticleSIMD::TransportToPoint( const float_v xyz[] )
{
  float_v dsdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  const float_v dS = GetDStoPoint(xyz, dsdr);
  TransportToDS( dS, dsdr );
}
#ifdef HomogeneousField
inline void KFParticleSIMD::TransportToVertex( const KFPVertex &v )
{
  TransportToPoint( KFParticleSIMD(v).fP );
}
#endif
inline void KFParticleSIMD::TransportToParticle( const KFParticleSIMD &p )
{ 
  float_v dsdr[4][6];
  float_v dS[2];
  GetDStoParticle( p, dS, dsdr );
  TransportToDS( dS[0], dsdr[3] );
}

inline void KFParticleSIMD::TransportToDS( float_v dS, const float_v* dsdr )
{
  KFParticleBaseSIMD::TransportToDS( dS, dsdr );
} 

inline void KFParticleSIMD::TransportToDSLine( float_v dS, const float_v* dsdr )
{
  KFParticleBaseSIMD::TransportToDSLine( dS, dsdr );
} 

inline float_v KFParticleSIMD::GetDStoPoint( const float_v xyz[3], float_v dsdr[6] ) const 
{
#ifdef HomogeneousField
  return KFParticleBaseSIMD::GetDStoPointBz( GetFieldAlice(), xyz, dsdr );
#endif
#ifdef NonhomogeneousField
  return KFParticleBaseSIMD::GetDStoPointCBM( xyz, dsdr );
#endif
}

inline float_v KFParticleSIMD::GetDistanceFromVertex( const float_v vtx[] ) const
{
  return KFParticleBaseSIMD::GetDistanceFromVertex( vtx );
}

inline float_v KFParticleSIMD::GetDeviationFromVertex( const float_v v[], 
						       const float_v Cv[] ) const
{
  return KFParticleBaseSIMD::GetDeviationFromVertex( v, Cv);
}

inline float_v KFParticleSIMD::GetDistanceFromVertex( const KFParticleSIMD &Vtx ) const
{
  return KFParticleBaseSIMD::GetDistanceFromVertex( Vtx );
}

inline float_v KFParticleSIMD::GetDeviationFromVertex( const KFParticleSIMD &Vtx ) const
{
  return KFParticleBaseSIMD::GetDeviationFromVertex( Vtx );
}
#ifdef HomogeneousField
inline float_v KFParticleSIMD::GetDistanceFromVertex( const KFPVertex &Vtx ) const
{
  return GetDistanceFromVertex( KFParticleSIMD(Vtx) );
}

inline float_v KFParticleSIMD::GetDeviationFromVertex( const KFPVertex &Vtx ) const
{
  return GetDeviationFromVertex( KFParticleSIMD(Vtx) );
}
#endif
inline float_v KFParticleSIMD::GetDistanceFromParticle( const KFParticleSIMD &p ) const 
{
  return KFParticleBaseSIMD::GetDistanceFromParticle( p );
}

inline float_v KFParticleSIMD::GetDeviationFromParticle( const KFParticleSIMD &p ) const 
{
  return KFParticleBaseSIMD::GetDeviationFromParticle( p );
}

inline void KFParticleSIMD::SubtractFromVertex( KFParticleSIMD &v ) const 
{
  KFParticleBaseSIMD::SubtractFromVertex( v );
}

inline void KFParticleSIMD::SubtractFromParticle( KFParticleSIMD &v ) const 
{
  KFParticleBaseSIMD::SubtractFromParticle( v );
}

#ifdef HomogeneousField
inline float_v KFParticleSIMD::GetFieldAlice()
{ 
  return fgBz; 
}
#endif

#ifdef HomogeneousField
inline void KFParticleSIMD::GetFieldValue( const float_v * /*xyz*/, float_v B[] ) const 
{    
  B[0] = B[1] = 0;
  B[2] = GetFieldAlice();
}
#endif

#ifdef NonhomogeneousField
inline void KFParticleSIMD::GetFieldValue( const float_v xyz[], float_v B[] ) const 
{
  KFParticleFieldValue mB = const_cast<KFParticleFieldRegion&>(fField).Get(xyz[2]);
  B[0] = mB.x;
  B[1] = mB.y;
  B[2] = mB.z;
}
#endif

inline void KFParticleSIMD::GetDStoParticle( const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] )const
{
#ifdef HomogeneousField
  KFParticleBaseSIMD::GetDStoParticleBz( GetFieldAlice(), p, dS, dsdr ) ;
#endif
#ifdef NonhomogeneousField
  KFParticleBaseSIMD::GetDStoParticleCBM( p, dS, dsdr ) ;
#endif
}

inline void KFParticleSIMD::GetDStoParticleFast( const KFParticleBaseSIMD &p, float_v dS[2] )const
{
#ifdef HomogeneousField
  KFParticleBaseSIMD::GetDStoParticleBz( GetFieldAlice(), p, dS ) ;
#endif
#ifdef NonhomogeneousField
  KFParticleBaseSIMD::GetDStoParticleCBM( p, dS ) ;
#endif
}

inline void KFParticleSIMD::Transport( float_v dS, const float_v* dsdr, float_v P[], float_v C[], float_v* dsdr1, float_v* F, float_v* F1 ) const 
{
#ifdef HomogeneousField
  KFParticleBaseSIMD::TransportBz( GetFieldAlice(), dS, dsdr, P, C, dsdr1, F, F1 );
#endif
#ifdef NonhomogeneousField
  KFParticleBaseSIMD::TransportCBM( dS, dsdr, P, C, dsdr1, F, F1 );
#endif
}

inline void KFParticleSIMD::TransportFast( float_v dS, float_v P[] ) const 
{
#ifdef HomogeneousField
  KFParticleBaseSIMD::TransportBz( GetFieldAlice(), dS, P );
#endif
#ifdef NonhomogeneousField
  KFParticleBaseSIMD::TransportCBM( dS, P );
#endif
}

#endif 
