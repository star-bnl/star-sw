//---------------------------------------------------------------------------------
// The KFParticle class
// .
// @author  S.Gorbunov, I.Kisel
// @version 1.0
// @since   13.05.07
// 
// Class to reconstruct and store the decayed particle parameters.
// The method is described in CBM-SOFT note 2007-003, 
// ``Reconstruction of decayed particles based on the Kalman filter'', 
// http://www.gsi.de/documents/DOC-2007-May-14-1.pdf
//
// This class is ALICE interface to general mathematics in KFParticleBase
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//_________________________________________________________________________________

//#define NonhomogeneousField
// #define HomogeneousField

#ifndef KFPARTICLE_H
#define KFPARTICLE_H

#include "KFParticleBase.h"
// #include "TMath.h"
#include <cmath>

class KFPTrack;
class KFPVertex;

class KFParticle :public KFParticleBase
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

  KFParticle():KFParticleBase(){ ; }
  //* Destructor (empty)

  ~KFParticle(){ ; }

  //* Construction of mother particle by its 2-3-4 daughters

  KFParticle( const KFParticle &d1, const KFParticle &d2 );

  KFParticle( const KFParticle &d1, const KFParticle &d2, 
              const KFParticle &d3 );

  KFParticle( const KFParticle &d1, const KFParticle &d2, 
              const KFParticle &d3, const KFParticle &d4 );
 
 //* Initialisation from "cartesian" coordinates ( X Y Z Px Py Pz )
 //* Parameters, covariance matrix, charge and PID hypothesis should be provided 

  void Create( const float Param[], const float Cov[], Int_t Charge, float mass /*Int_t PID*/ );
  void Create( const Double_t Param[], const Double_t Cov[], Int_t Charge, float mass /*Int_t PID*/ );

 //* Initialisation from ALICE track, PID hypothesis shoould be provided 

  KFParticle( const KFPTrack &track, const int PID );


  //* Initialisation from VVertex 

  KFParticle( const KFPVertex &vertex );

  //* Initialise covariance matrix and set current parameters to 0.0 

  void Initialize();

  //*
  //*  ACCESSORS
  //*

  //* Simple accessors 

  float GetX    () const ; //* x of current position
  float GetY    () const ; //* y of current position
  float GetZ    () const ; //* z of current position
  float GetPx   () const ; //* x-compoment of 3-momentum
  float GetPy   () const ; //* y-compoment of 3-momentum
  float GetPz   () const ; //* z-compoment of 3-momentum
  float GetE    () const ; //* energy
  float GetS    () const ; //* decay length / momentum
  char    GetQ    () const ; //* charge
  float GetChi2 () const ; //* chi^2
  Int_t    GetNDF  () const ; //* Number of Degrees of Freedom

  Bool_t GetAtProductionVertex() const { return fAtProductionVertex; }
  void SetAtProductionVertex(Bool_t b) { fAtProductionVertex = b; }

#ifdef NonhomogeneousField
  const float* GetFieldCoeff() const { return fieldRegion; }
  void SetFieldCoeff(float c, int i) { fieldRegion[i] = c; }
#endif

  const float& X    () const { return fP[0]; }
  const float& Y    () const { return fP[1]; }
  const float& Z    () const { return fP[2]; }
  const float& Px   () const { return fP[3]; }
  const float& Py   () const { return fP[4]; }
  const float& Pz   () const { return fP[5]; }
  const float& E    () const { return fP[6]; }
  const float& S    () const { return fP[7]; }
  const char   & Q    () const { return fQ;    }
  const float& Chi2 () const { return fChi2; }
  const Int_t   & NDF  () const { return fNDF;  }
  
  float GetParameter ( int i ) const ;
  float GetCovariance( int i ) const ;
  float GetCovariance( int i, int j ) const ;

  //* Accessors with calculations, value returned w/o error flag
  
  float GetP           () const; //* momentum
  float GetPt          () const; //* transverse momentum
  float GetEta         () const; //* pseudorapidity
  float GetPhi         () const; //* phi
  float GetMomentum    () const; //* momentum (same as GetP() )
  float GetMass        () const; //* mass
  float GetDecayLength () const; //* decay length
  float GetDecayLengthXY () const; //* decay length in XY
  float GetLifeTime    () const; //* life time
  float GetR           () const; //* distance to the origin

  //* Accessors to estimated errors

  float GetErrX           () const ; //* x of current position 
  float GetErrY           () const ; //* y of current position
  float GetErrZ           () const ; //* z of current position
  float GetErrPx          () const ; //* x-compoment of 3-momentum
  float GetErrPy          () const ; //* y-compoment of 3-momentum
  float GetErrPz          () const ; //* z-compoment of 3-momentum
  float GetErrE           () const ; //* energy
  float GetErrS           () const ; //* decay length / momentum
  float GetErrP           () const ; //* momentum
  float GetErrPt          () const ; //* transverse momentum
  float GetErrEta         () const ; //* pseudorapidity
  float GetErrPhi         () const ; //* phi
  float GetErrMomentum    () const ; //* momentum
  float GetErrMass        () const ; //* mass
  float GetErrDecayLength () const ; //* decay length
  float GetErrDecayLengthXY () const ; //* decay length in XY
  float GetErrLifeTime    () const ; //* life time
  float GetErrR           () const ; //* distance to the origin

  //* Accessors with calculations( &value, &estimated sigma )
  //* error flag returned (0 means no error during calculations) 

  int GetP           ( float &P, float &SigmaP ) const ;   //* momentum
  int GetPt          ( float &Pt, float &SigmaPt ) const ; //* transverse momentum
  int GetEta         ( float &Eta, float &SigmaEta ) const ; //* pseudorapidity
  int GetPhi         ( float &Phi, float &SigmaPhi ) const ; //* phi
  int GetMomentum    ( float &P, float &SigmaP ) const ;   //* momentum
  int GetMass        ( float &M, float &SigmaM ) const ;   //* mass
  int GetDecayLength ( float &L, float &SigmaL ) const ;   //* decay length
  int GetDecayLengthXY ( float &L, float &SigmaL ) const ;   //* decay length in XY
  int GetLifeTime    ( float &T, float &SigmaT ) const ;   //* life time
  int GetR           ( float &R, float &SigmaR ) const ; //* R
  float GetRapidity() const { return 0.5*log((fP[6] + fP[5])/(fP[6] - fP[5])); }
  float GetTheta()    const { return atan2(GetPt(),fP[5]); }


  //*
  //*  MODIFIERS
  //*
  
  float & X    () ;
  float & Y    () ;
  float & Z    () ;
  float & Px   () ;
  float & Py   () ;
  float & Pz   () ;
  float & E    () ;
  float & S    () ;
  char    & Q    () ;
  float & Chi2 () ;
  Int_t    & NDF  () ;

  float & Parameter ( int i ) ;
  float & Covariance( int i ) ;
  float & Covariance( int i, int j ) ;
  float * Parameters () ;
  float * CovarianceMatrix() ;

  //* 
  //* CONSTRUCTION OF THE PARTICLE BY ITS DAUGHTERS AND MOTHER
  //* USING THE KALMAN FILTER METHOD
  //*


  //* Add daughter to the particle 

  void AddDaughter( const KFParticle &Daughter );

  //* Add daughter via += operator: ex.{ D0; D0+=Pion; D0+= Kaon; }

  void operator +=( const KFParticle &Daughter );  

  //* Set production vertex 

  void SetProductionVertex( const KFParticle &Vtx );

  //* Set mass constraint 

  void SetMassConstraint( float Mass, float SigmaMass = 0  );
  
  //* Set no decay length for resonances

  void SetNoDecayLength();

  //* Everything in one go  

  void Construct( const KFParticle *vDaughters[], int nDaughters, 
		  const KFParticle *ProdVtx=0,   float Mass=-1 );

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

  void TransportToPoint( const float xyz[] );

  //* Transport the particle close to VVertex  
#ifdef HomogeneousField
  void TransportToVertex( const KFPVertex &v );
#endif
  //* Transport the particle close to another particle p 

  void TransportToParticle( const KFParticle &p );

  //* Transport the particle on dS parameter (SignedPath/Momentum) 

  void TransportToDS( float dS, const float* dsdr );

  //* Get dS to a certain space point 

  float GetDStoPoint( const float xyz[3], float dsdr[6] ) const ;
  
  //* Get dS to other particle p (dSp for particle p also returned) 

  void GetDStoParticle( const KFParticleBase &p, float dS[2], float dsdr[4][6] ) const ;
  
  
  //* 
  //* OTHER UTILITIES
  //*


  //* Calculate distance from another object [cm]

  float GetDistanceFromVertex( const float vtx[] ) const ;
  float GetDistanceFromVertex( const KFParticle &Vtx ) const ;
#ifdef HomogeneousField
  float GetDistanceFromVertex( const KFPVertex &Vtx ) const ;
#endif
  float GetDistanceFromParticle( const KFParticle &p ) const ;

  //* Calculate sqrt(Chi2/ndf) deviation from another object
  //* ( v = [xyz]-vertex, Cv=[Cxx,Cxy,Cyy,Cxz,Cyz,Czz]-covariance matrix )

  float GetDeviationFromVertex( const float v[], const float Cv[]=0 ) const ;
  float GetDeviationFromVertex( const KFParticle &Vtx ) const ;
#ifdef HomogeneousField
  float GetDeviationFromVertex( const KFPVertex &Vtx ) const ;
#endif
  float GetDeviationFromParticle( const KFParticle &p ) const ;
 
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

  //* Subtract the particle from the vertex  

  void SubtractFromVertex( KFParticle &v ) const ;
  void SubtractFromParticle( KFParticle &v ) const;

    // * Pseudo Proper Time of decay = (r*pt) / |pt| * M/|pt|
    // @primVertex - primary vertex
    // @mass - mass of the mother particle (in the case of "Hb -> JPsi" it would be JPsi mass)
    // @*timeErr2 - squared error of the decay time. If timeErr2 = 0 it isn't calculated
  float GetPseudoProperDecayTime( const KFParticle &primVertex, const float& mass, float* timeErr2 = 0 ) const;

  void GetFieldValue( const float xyz[], float B[] ) const ;

  void Transport( float dS, const float* dsdr, float P[], float C[], float* dsdr1=0, float* F=0, float* F1=0 ) const ;


 protected: 
  
  //*
  //*  INTERNAL STUFF
  //* 
  //* Method to access ALICE field 
#ifdef HomogeneousField
  static float GetFieldAlice();
#endif
  
 private:
#ifdef HomogeneousField
  static float fgBz;  //* Bz compoment of the magnetic field
#endif
#ifdef NonhomogeneousField
  float fieldRegion[10];
#endif
  
#ifndef KFParticleStandalone
  ClassDef( KFParticle, 3 )
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
  fgBz = Bz;
}
#endif

inline KFParticle::KFParticle( const KFParticle &d1, 
                               const KFParticle &d2, 
                               const KFParticle &d3 )
{
  KFParticle mother;
  mother+= d1;
  mother+= d2;
  mother+= d3;
  *this = mother;
}

inline KFParticle::KFParticle( const KFParticle &d1, 
                               const KFParticle &d2, 
                               const KFParticle &d3, 
                               const KFParticle &d4 )
{
  KFParticle mother;
  mother+= d1;
  mother+= d2;
  mother+= d3;
  mother+= d4;
  *this = mother;
}


inline void KFParticle::Initialize()
{ 
  KFParticleBase::Initialize(); 
}

inline float KFParticle::GetX    () const 
{ 
  return KFParticleBase::GetX();    
}

inline float KFParticle::GetY    () const 
{ 
  return KFParticleBase::GetY();    
}

inline float KFParticle::GetZ    () const 
{ 
  return KFParticleBase::GetZ();    
}

inline float KFParticle::GetPx   () const 
{ 
  return KFParticleBase::GetPx();   
}

inline float KFParticle::GetPy   () const 
{ 
  return KFParticleBase::GetPy();   
}

inline float KFParticle::GetPz   () const 
{ 
  return KFParticleBase::GetPz();   
}

inline float KFParticle::GetE    () const 
{ 
  return KFParticleBase::GetE();    
}

inline float KFParticle::GetS    () const 
{ 
  return KFParticleBase::GetS();    
}

inline char    KFParticle::GetQ    () const 
{ 
  return KFParticleBase::GetQ();    
}

inline float KFParticle::GetChi2 () const 
{ 
  return KFParticleBase::GetChi2(); 
}

inline Int_t    KFParticle::GetNDF  () const 
{ 
  return KFParticleBase::GetNDF();  
}

inline float KFParticle::GetParameter ( int i ) const 
{ 
  return KFParticleBase::GetParameter(i);  
}

inline float KFParticle::GetCovariance( int i ) const 
{ 
  return KFParticleBase::GetCovariance(i); 
}

inline float KFParticle::GetCovariance( int i, int j ) const 
{ 
  return KFParticleBase::GetCovariance(i,j);
}


inline float KFParticle::GetP    () const
{
  float par, err;
  if( KFParticleBase::GetMomentum( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetPt   () const
{
  float par, err;
  if( KFParticleBase::GetPt( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetEta   () const
{
  float par, err;
  if( KFParticleBase::GetEta( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetPhi   () const
{
  float par, err;
  if( KFParticleBase::GetPhi( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetMomentum    () const
{
  float par, err;
  if( KFParticleBase::GetMomentum( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetMass        () const
{
  float par, err;
  if( KFParticleBase::GetMass( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetDecayLength () const
{
  float par, err;
  if( KFParticleBase::GetDecayLength( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetDecayLengthXY () const
{
  float par, err;
  if( KFParticleBase::GetDecayLengthXY( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetLifeTime    () const
{
  float par, err;
  if( KFParticleBase::GetLifeTime( par, err ) ) return 0;
  else return par;
}

inline float KFParticle::GetR   () const
{
  float par, err;
  if( KFParticleBase::GetR( par, err ) ) return 0;
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
  if( KFParticleBase::GetMomentum( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrPt    () const
{
  float par, err;
  if( KFParticleBase::GetPt( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrEta    () const
{
  float par, err;
  if( KFParticleBase::GetEta( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrPhi    () const
{
  float par, err;
  if( KFParticleBase::GetPhi( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrMomentum    () const
{
  float par, err;
  if( KFParticleBase::GetMomentum( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrMass        () const
{
  float par, err;
  if( KFParticleBase::GetMass( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrDecayLength () const
{
  float par, err;
  if( KFParticleBase::GetDecayLength( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrDecayLengthXY () const
{
  float par, err;
  if( KFParticleBase::GetDecayLengthXY( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrLifeTime    () const
{
  float par, err;
  if( KFParticleBase::GetLifeTime( par, err ) ) return 1.e10;
  else return err;
}

inline float KFParticle::GetErrR    () const
{
  float par, err;
  if( KFParticleBase::GetR( par, err ) ) return 1.e10;
  else return err;
}


inline int KFParticle::GetP( float &P, float &SigmaP ) const 
{
  return KFParticleBase::GetMomentum( P, SigmaP );
}

inline int KFParticle::GetPt( float &Pt, float &SigmaPt ) const 
{
  return KFParticleBase::GetPt( Pt, SigmaPt );
}

inline int KFParticle::GetEta( float &Eta, float &SigmaEta ) const 
{
  return KFParticleBase::GetEta( Eta, SigmaEta );
}

inline int KFParticle::GetPhi( float &Phi, float &SigmaPhi ) const 
{
  return KFParticleBase::GetPhi( Phi, SigmaPhi );
}

inline int KFParticle::GetMomentum( float &P, float &SigmaP ) const 
{
  return KFParticleBase::GetMomentum( P, SigmaP );
}

inline int KFParticle::GetMass( float &M, float &SigmaM ) const 
{
  return KFParticleBase::GetMass( M, SigmaM );
}

inline int KFParticle::GetDecayLength( float &L, float &SigmaL ) const 
{
  return KFParticleBase::GetDecayLength( L, SigmaL );
}

inline int KFParticle::GetDecayLengthXY( float &L, float &SigmaL ) const 
{
  return KFParticleBase::GetDecayLengthXY( L, SigmaL );
}

inline int KFParticle::GetLifeTime( float &T, float &SigmaT ) const 
{
  return KFParticleBase::GetLifeTime( T, SigmaT );
}

inline int KFParticle::GetR( float &R, float &SigmaR ) const 
{
  return KFParticleBase::GetR( R, SigmaR );
}

inline float & KFParticle::X() 
{ 
  return KFParticleBase::X();    
}

inline float & KFParticle::Y()
{ 
  return KFParticleBase::Y();    
}

inline float & KFParticle::Z() 
{ 
  return KFParticleBase::Z();    
}

inline float & KFParticle::Px() 
{ 
  return KFParticleBase::Px();   
}

inline float & KFParticle::Py() 
{ 
  return KFParticleBase::Py();   
}

inline float & KFParticle::Pz() 
{ 
  return KFParticleBase::Pz();   
}

inline float & KFParticle::E() 
{ 
  return KFParticleBase::E();    
}

inline float & KFParticle::S() 
{ 
  return KFParticleBase::S();    
}

inline char    & KFParticle::Q() 
{ 
  return KFParticleBase::Q();    
}

inline float & KFParticle::Chi2() 
{ 
  return KFParticleBase::Chi2(); 
}

inline Int_t    & KFParticle::NDF() 
{ 
  return KFParticleBase::NDF();  
}

inline float & KFParticle::Parameter ( int i )        
{ 
  return KFParticleBase::Parameter(i);
}

inline float & KFParticle::Covariance( int i )        
{ 
  return KFParticleBase::Covariance(i);
}

inline float & KFParticle::Covariance( int i, int j ) 
{ 
  return KFParticleBase::Covariance(i,j); 
}

inline float * KFParticle::Parameters ()
{
  return fP;
}

inline float * KFParticle::CovarianceMatrix()
{
  return fC;
}


inline void KFParticle::operator +=( const KFParticle &Daughter )
{
#ifdef NonhomogeneousField
  for(int i=0; i<10; i++)
    SetFieldCoeff(Daughter.GetFieldCoeff()[i], i);
#endif
  KFParticleBase::operator +=( Daughter );
}
  

inline void KFParticle::AddDaughter( const KFParticle &Daughter )
{
#ifdef NonhomogeneousField
  for(int i=0; i<10; i++)
    SetFieldCoeff(Daughter.GetFieldCoeff()[i], i);
#endif
  KFParticleBase::AddDaughter( Daughter );
}

inline void KFParticle::SetProductionVertex( const KFParticle &Vtx )
{
  KFParticleBase::SetProductionVertex( Vtx );
}

inline void KFParticle::SetMassConstraint( float Mass, float SigmaMass )
{
  KFParticleBase::SetMassConstraint( Mass, SigmaMass );
}
    
inline void KFParticle::SetNoDecayLength()
{
  KFParticleBase::SetNoDecayLength();
}

inline void KFParticle::Construct( const KFParticle *vDaughters[], int nDaughters, 
			       const KFParticle *ProdVtx,   float Mass )
{    
#ifdef NonhomogeneousField
  for(int i=0; i<10; i++)
    SetFieldCoeff(vDaughters[0]->GetFieldCoeff()[i], i);
#endif
  KFParticleBase::Construct( ( const KFParticleBase**)vDaughters, nDaughters, 
			 ( const KFParticleBase*)ProdVtx, Mass );
}

inline void KFParticle::TransportToDecayVertex()
{ 
  KFParticleBase::TransportToDecayVertex(); 
}

inline void KFParticle::TransportToProductionVertex()
{
  KFParticleBase::TransportToProductionVertex();
}

inline void KFParticle::TransportToPoint( const float xyz[] )
{ 
  float dsdr[6] = {0.f};
  float dS = GetDStoPoint(xyz, dsdr);
  TransportToDS( dS, dsdr );
}
#ifdef HomogeneousField
inline void KFParticle::TransportToVertex( const KFPVertex &v )
{       
  TransportToPoint( KFParticle(v).fP );
}
#endif
inline void KFParticle::TransportToParticle( const KFParticle &p )
{ 
  float dsdr[4][6];
  float dS[2];
  GetDStoParticle( p, dS, dsdr );
  TransportToDS( dS[0], dsdr[3] );
}

inline void KFParticle::TransportToDS( float dS, const float* dsdr )
{
  KFParticleBase::TransportToDS( dS, dsdr );
} 

inline float KFParticle::GetDStoPoint( const float xyz[], float* dsdr ) const 
{
#ifdef HomogeneousField
  return KFParticleBase::GetDStoPointBz( GetFieldAlice(), xyz, dsdr );
#endif
#ifdef NonhomogeneousField
  return KFParticleBase::GetDStoPointCBM( xyz, dsdr );
#endif
}


inline float KFParticle::GetDistanceFromVertex( const float vtx[] ) const
{
  return KFParticleBase::GetDistanceFromVertex( vtx );
}

inline float KFParticle::GetDeviationFromVertex( const float v[], 
						       const float Cv[] ) const
{
  return KFParticleBase::GetDeviationFromVertex( v, Cv);
}

inline float KFParticle::GetDistanceFromVertex( const KFParticle &Vtx ) const
{
  return KFParticleBase::GetDistanceFromVertex( Vtx );
}

inline float KFParticle::GetDeviationFromVertex( const KFParticle &Vtx ) const
{
  return KFParticleBase::GetDeviationFromVertex( Vtx );
}
#ifdef HomogeneousField
inline float KFParticle::GetDistanceFromVertex( const KFPVertex &Vtx ) const
{
  return GetDistanceFromVertex( KFParticle(Vtx) );
}

inline float KFParticle::GetDeviationFromVertex( const KFPVertex &Vtx ) const
{
  return GetDeviationFromVertex( KFParticle(Vtx) );
}
#endif
inline float KFParticle::GetDistanceFromParticle( const KFParticle &p ) const 
{
  return KFParticleBase::GetDistanceFromParticle( p );
}

inline float KFParticle::GetDeviationFromParticle( const KFParticle &p ) const 
{
  return KFParticleBase::GetDeviationFromParticle( p );
}

inline void KFParticle::SubtractFromVertex( KFParticle &v ) const 
{
  KFParticleBase::SubtractFromVertex( v );
}

inline void KFParticle::SubtractFromParticle( KFParticle &v ) const 
{
  KFParticleBase::SubtractFromParticle( v );
}

#ifdef HomogeneousField
inline float KFParticle::GetFieldAlice()
{ 
  return fgBz; 
}
#endif

#ifdef HomogeneousField
inline void KFParticle::GetFieldValue( const float * /*xyz*/, float B[] ) const 
{    
  B[0] = B[1] = 0;
  B[2] = GetFieldAlice();
}
#endif

#ifdef NonhomogeneousField

// #ifndef KFParticleStandalone
// #include "CbmKF.h"
// #endif

inline void KFParticle::GetFieldValue( const float xyz[], float B[] ) const 
{
// #ifndef KFParticleStandalone
//   FairField *MF = CbmKF::Instance()->GetMagneticField();
//   const Double_t xyzDouble[3] = {xyz[0], xyz[1], xyz[2]};
//   Double_t BDouble[3]={0.};
//   MF->GetFieldValue( xyzDouble, BDouble );
//   B[0] = BDouble[0]; B[1] = BDouble[1]; B[2] = BDouble[2];
// #else
  const float dz = (xyz[2]-fieldRegion[9]);
  const float dz2 = dz*dz;

  B[0] = fieldRegion[0] + fieldRegion[1]*dz + fieldRegion[2]*dz2;
  B[1] = fieldRegion[3] + fieldRegion[4]*dz + fieldRegion[5]*dz2;
  B[2] = fieldRegion[6] + fieldRegion[7]*dz + fieldRegion[8]*dz2;
// #endif
}
#endif

inline void KFParticle::GetDStoParticle( const KFParticleBase &p, float dS[2], float dsdr[4][6] ) const
{ 
#ifdef HomogeneousField
  KFParticleBase::GetDStoParticleBz( GetFieldAlice(), p, dS, dsdr ) ;
#endif
#ifdef NonhomogeneousField
  KFParticleBase::GetDStoParticleCBM( p, dS, dsdr ) ;
#endif
}

inline void KFParticle::Transport( float dS, const float* dsdr, float P[], float C[], float* dsdr1, float* F, float* F1 ) const 
{
#ifdef HomogeneousField
  KFParticleBase::TransportBz( GetFieldAlice(), dS, dsdr, P, C, dsdr1, F, F1 );
#endif
#ifdef NonhomogeneousField
  KFParticleBase::TransportCBM( dS, dsdr, P, C, dsdr1, F, F1 );
#endif
}

#endif 
