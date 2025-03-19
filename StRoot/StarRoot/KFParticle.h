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
//  -= Copyright &copy ALICE HLT Group =-
//_________________________________________________________________________________

#ifndef KFParticle_H
#define KFParticle_H

#include "KFParticleBase.h"
#include "TMath.h"

class MTrack;
class MVertex;

class KFParticle :public KFParticleBase
{
  
 public:

  //*
  //*  INITIALIZATION
  //*

  //* Set magnetic field for all particles
  
  static void SetField( Double_t Bz );

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

  void Create( const Double_t Param[], const Double_t Cov[], Int_t Charge, Int_t PID );

 //* Initialisation from ALICE track, PID hypothesis shoould be provided 

  KFParticle( const MTrack &track, Int_t PID );
  virtual void        Clear(Option_t *option ="") {KFParticleBase::Clear(option);}

  //* Initialisation from VVertex 
  KFParticle( const MVertex &vertex );
  //* Initialise covariance matrix and set current parameters to 0.0 
#if 0
  void Initialize();
#endif
  
  //* Set decay vertex parameters for linearisation 

  void SetVtxGuess( Double_t x, Double_t y, Double_t z );

  //*
  //*  ACCESSORS
  //*

  //* Simple accessors 

  Double_t GetX    () const ; //* x of current position
  Double_t GetY    () const ; //* y of current position
  Double_t GetZ    () const ; //* z of current position
  Double_t GetPx   () const ; //* x-compoment of 3-momentum
  Double_t GetPy   () const ; //* y-compoment of 3-momentum
  Double_t GetPz   () const ; //* z-compoment of 3-momentum
  Double_t GetE    () const ; //* energy
  Double_t GetS    () const ; //* decay length / momentum
  Short_t  GetQ    () const ; //* charge
  Double_t GetChi2 () const ; //* chi^2
  Short_t  GetNDF  () const ; //* Number of Degrees of Freedom
  Double_t GetParameter ( Int_t i ) const ;
  Double_t GetCovariance( Int_t i ) const ;
  Double_t GetCovariance( Int_t i, Int_t j ) const ;
  //* Accessors with calculations, value returned w/o error flag
  
  Double_t GetP           () const; //* momentum
  Double_t GetPt          () const; //* transverse momentum
  Double_t GetEta         () const; //* pseudorapidity
  Double_t GetPhi         () const; //* phi
  Double_t GetMomentum    () const; //* momentum (same as GetP() )
  Double_t GetMass        () const; //* mass
  Double_t GetDecayLength () const; //* decay length
  Double_t GetDecayLengthXY () const; //* decay length in XY
  Double_t GetLifeTime    () const; //* life time
  Double_t GetR           () const; //* distance to the origin

  //* Accessors to estimated errors

  Double_t GetErrX           () const ; //* x of current position 
  Double_t GetErrY           () const ; //* y of current position
  Double_t GetErrZ           () const ; //* z of current position
  Double_t GetErrPx          () const ; //* x-compoment of 3-momentum
  Double_t GetErrPy          () const ; //* y-compoment of 3-momentum
  Double_t GetErrPz          () const ; //* z-compoment of 3-momentum
  Double_t GetErrE           () const ; //* energy
  Double_t GetErrS           () const ; //* decay length / momentum
  Double_t GetErrP           () const ; //* momentum
  Double_t GetErrPt          () const ; //* transverse momentum
  Double_t GetErrEta         () const ; //* pseudorapidity
  Double_t GetErrPhi         () const ; //* phi
  Double_t GetErrMomentum    () const ; //* momentum
  Double_t GetErrMass        () const ; //* mass
  Double_t GetErrDecayLength () const ; //* decay length
  Double_t GetErrDecayLengthXY () const ; //* decay length in XY
  Double_t GetErrLifeTime    () const ; //* life time
  Double_t GetErrR           () const ; //* distance to the origin

  //* Accessors with calculations( &value, &estimated sigma )
  //* error flag returned (0 means no error during calculations) 

  Int_t GetP           ( Double_t &P, Double_t &SigmaP ) const ;   //* momentum
  Int_t GetPt          ( Double_t &Pt, Double_t &SigmaPt ) const ; //* transverse momentum
  Int_t GetEta         ( Double_t &Eta, Double_t &SigmaEta ) const ; //* pseudorapidity
  Int_t GetPhi         ( Double_t &Phi, Double_t &SigmaPhi ) const ; //* phi
  Int_t GetMomentum    ( Double_t &P, Double_t &SigmaP ) const ;   //* momentum
  Int_t GetMass        ( Double_t &M, Double_t &SigmaM ) const ;   //* mass
  Int_t GetDecayLength ( Double_t &L, Double_t &SigmaL ) const ;   //* decay length
  Int_t GetDecayLengthXY ( Double_t &L, Double_t &SigmaL ) const ;   //* decay length in XY
  Int_t GetLifeTime    ( Double_t &T, Double_t &SigmaT ) const ;   //* life time
  Int_t GetR           ( Double_t &R, Double_t &SigmaR ) const ; //* R


  //*
  //*  MODIFIERS
  //*
  
  Double_t & X    () ;
  Double_t & Y    () ;
  Double_t & Z    () ;
  Double_t & Px   () ;
  Double_t & Py   () ;
  Double_t & Pz   () ;
  Double_t & E    () ;
  Double_t & S    () ;
  Short_t  & Q    () ;
  Double_t & Chi2 () ;
  Short_t  & NDF  () ;

  Double_t & Parameter ( Int_t i ) ;
  Double_t & Covariance( Int_t i ) ;
  Double_t & Covariance( Int_t i, Int_t j ) ;
  Double_t * Parameters () ;
  Double_t * CovarianceMatrix() ;

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

  void SetMassConstraint( Double_t Mass, Double_t SigmaMass = 0  );
  
  //* Set no decay length for resonances

  void SetNoDecayLength();

  //* Everything in one go  

  void Construct( const KFParticle *vDaughters[], Int_t NDaughters, 
		  const KFParticle *ProdVtx=0,   Double_t Mass=-1, Bool_t IsConstrained=0  );

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

  void TransportToPoint( const Double_t xyz[] );

  //* Transport the particle close to VVertex  

  void TransportToVertex( const MVertex &v );

  //* Transport the particle close to another particle p 

  void TransportToParticle( const KFParticle &p );

  //* Transport the particle on dS parameter (SignedPath/Momentum) 

  void TransportToDS( Double_t dS );

  //* Get dS to a certain space point 

  Double_t GetDStoPoint( const Double_t xyz[] ) const ;
  
  //* Get dS to other particle p (dSp for particle p also returned) 

  void GetDStoParticle( const KFParticle &p, 
			Double_t &DS, Double_t &DSp ) const ;
  
  //* Get dS to other particle p in XY-plane

  void GetDStoParticleXY( const KFParticleBase &p, 
			  Double_t &DS, Double_t &DSp ) const ;
  
  //* 
  //* OTHER UTILITIES
  //*


  //* Calculate distance from another object [cm]

  Double_t GetDistanceFromVertex( const Double_t vtx[] ) const ;
  Double_t GetDistanceFromVertex( const KFParticle &Vtx ) const ;
  Double_t GetDistanceFromVertex( const MVertex &Vtx ) const ;
  Double_t GetDistanceFromParticle( const KFParticle &p ) const ;
  //* Calculate sqrt(Chi2/ndf) deviation from another object
  //* ( v = [xyz]-vertex, Cv=[Cxx,Cxy,Cyy,Cxz,Cyz,Czz]-covariance matrix )

  Double_t GetDeviationFromVertex( const Double_t v[], const Double_t Cv[]=0 ) const ;
  Double_t GetDeviationFromVertex( const KFParticle &Vtx ) const ;
  Double_t GetDeviationFromVertex( const MVertex &Vtx ) const ;
  Double_t GetDeviationFromParticle( const KFParticle &p ) const ;
 
  //* Calculate distance from another object [cm] in XY-plane

  Bool_t GetDistanceFromVertexXY( const Double_t vtx[], Double_t &val, Double_t &err ) const ;
  Bool_t GetDistanceFromVertexXY( const Double_t vtx[], const Double_t Cv[], Double_t &val, Double_t &err ) const ;
  Bool_t GetDistanceFromVertexXY( const KFParticle &Vtx, Double_t &val, Double_t &err ) const ;
  Bool_t GetDistanceFromVertexXY( const MVertex &Vtx, Double_t &val, Double_t &err ) const ;

  Double_t GetDistanceFromVertexXY( const Double_t vtx[] ) const ;
  Double_t GetDistanceFromVertexXY( const KFParticle &Vtx ) const ;
  Double_t GetDistanceFromVertexXY( const MVertex &Vtx ) const ;
  Double_t GetDistanceFromParticleXY( const KFParticle &p ) const ;

  //* Calculate sqrt(Chi2/ndf) deviation from another object in XY plane
  //* ( v = [xyz]-vertex, Cv=[Cxx,Cxy,Cyy,Cxz,Cyz,Czz]-covariance matrix )

  Double_t GetDeviationFromVertexXY( const Double_t v[], const Double_t Cv[]=0 ) const ;
  Double_t GetDeviationFromVertexXY( const KFParticle &Vtx ) const ;
  Double_t GetDeviationFromVertexXY( const MVertex &Vtx ) const ;
  Double_t GetDeviationFromParticleXY( const KFParticle &p ) const ;

  //* Calculate opennig angle between two particles

  Double_t GetAngle  ( const KFParticle &p ) const ;
  Double_t GetAngleXY( const KFParticle &p ) const ;
  Double_t GetAngleRZ( const KFParticle &p ) const ;

  //* Subtract the particle from the vertex  

  void SubtractFromVertex( KFParticle &v ) const ;

  //* Special method for creating gammas

  void ConstructGamma( const KFParticle &daughter1,
		       const KFParticle &daughter2  );
 protected: 
  
  //*
  //*  INTERNAL STUFF
  //* 

  //* Method to access ALICE field 
 
  static Double_t GetFieldAlice();
  
  //* Other methods required by the abstract KFParticleBase class 
  
  void GetFieldValue( const Double_t xyz[], Double_t B[] ) const ;
  void GetDStoParticle( const KFParticleBase &p, Double_t &DS, Double_t &DSp )const ;
  void Transport( Double_t dS, Double_t P[], Double_t C[] ) const ;
  static void GetExternalTrackParam( const KFParticleBase &p, Double_t &X, Double_t &Alpha, Double_t P[5]  ) ;

  //void GetDStoParticleALICE( const KFParticleBase &p, Double_t &DS, Double_t &DS1 ) const;


 private:

  static Double32_t fgBz;  //! * Bz compoment of the magnetic field


  ClassDef(KFParticle,1)
};
#ifndef __CINT__


//---------------------------------------------------------------------
//
//     Inline implementation of the KFParticle methods
//
//---------------------------------------------------------------------


inline void KFParticle::SetField( Double_t Bz )
{ 
  fgBz = Bz;//!!!
}


inline KFParticle::KFParticle( const KFParticle &d1, 
				     const KFParticle &d2 )
{
  KFParticle mother;
  mother+= d1;
  mother+= d2;
  *this = mother;
}

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
#if 0
inline void KFParticle::Initialize()
{ 
  KFParticleBase::Initialize(); 
}
#endif
inline void KFParticle::SetVtxGuess( Double_t x, Double_t y, Double_t z )
{
  KFParticleBase::SetVtxGuess(x,y,z);
}  

inline Double_t KFParticle::GetX    () const 
{ 
  return KFParticleBase::GetX();    
}

inline Double_t KFParticle::GetY    () const 
{ 
  return KFParticleBase::GetY();    
}

inline Double_t KFParticle::GetZ    () const 
{ 
  return KFParticleBase::GetZ();    
}

inline Double_t KFParticle::GetPx   () const 
{ 
  return KFParticleBase::GetPx();   
}

inline Double_t KFParticle::GetPy   () const 
{ 
  return KFParticleBase::GetPy();   
}

inline Double_t KFParticle::GetPz   () const 
{ 
  return KFParticleBase::GetPz();   
}

inline Double_t KFParticle::GetE    () const 
{ 
  return KFParticleBase::GetE();    
}

inline Double_t KFParticle::GetS    () const 
{ 
  return KFParticleBase::GetS();    
}

inline Short_t    KFParticle::GetQ    () const 
{ 
  return KFParticleBase::GetQ();    
}

inline Double_t KFParticle::GetChi2 () const 
{ 
  return KFParticleBase::GetChi2(); 
}

inline Short_t    KFParticle::GetNDF  () const 
{ 
  return KFParticleBase::GetNDF();  
}
inline Double_t KFParticle::GetParameter ( Int_t i ) const 
{ 
  return KFParticleBase::GetParameter(i);  
}

inline Double_t KFParticle::GetCovariance( Int_t i ) const 
{ 
  return KFParticleBase::GetCovariance(i); 
}

inline Double_t KFParticle::GetCovariance( Int_t i, Int_t j ) const 
{ 
  return KFParticleBase::GetCovariance(i,j);
}

inline Double_t KFParticle::GetP    () const
{
  Double_t par, err;
  if( KFParticleBase::GetMomentum( par, err ) ) return 0;
  else return par;
}

inline Double_t KFParticle::GetPt   () const
{
  Double_t par, err;
  if( KFParticleBase::GetPt( par, err ) ) return 0;
  else return par;
}

inline Double_t KFParticle::GetEta   () const
{
  Double_t par, err;
  if( KFParticleBase::GetEta( par, err ) ) return 0;
  else return par;
}

inline Double_t KFParticle::GetPhi   () const
{
  Double_t par, err;
  if( KFParticleBase::GetPhi( par, err ) ) return 0;
  else return par;
}

inline Double_t KFParticle::GetMomentum    () const
{
  Double_t par, err;
  if( KFParticleBase::GetMomentum( par, err ) ) return 0;
  else return par;
}

inline Double_t KFParticle::GetMass        () const
{
  Double_t par, err;
  if( KFParticleBase::GetMass( par, err ) ) return 0;
  else return par;
}

inline Double_t KFParticle::GetDecayLength () const
{
  Double_t par, err;
  if( KFParticleBase::GetDecayLength( par, err ) ) return 0;
  else return par;
}

inline Double_t KFParticle::GetDecayLengthXY () const
{
  Double_t par, err;
  if( KFParticleBase::GetDecayLengthXY( par, err ) ) return 0;
  else return par;
}

inline Double_t KFParticle::GetLifeTime    () const
{
  Double_t par, err;
  if( KFParticleBase::GetLifeTime( par, err ) ) return 0;
  else return par;
}

inline Double_t KFParticle::GetR   () const
{
  Double_t par, err;
  if( KFParticleBase::GetR( par, err ) ) return 0;
  else return par;
}

inline Double_t KFParticle::GetErrX           () const 
{
  return TMath::Sqrt(TMath::Abs( GetCovariance(0,0) ));
}

inline Double_t KFParticle::GetErrY           () const 
{
  return TMath::Sqrt(TMath::Abs( GetCovariance(1,1) ));
}

inline Double_t KFParticle::GetErrZ           () const 
{
  return TMath::Sqrt(TMath::Abs( GetCovariance(2,2) ));
}

inline Double_t KFParticle::GetErrPx          () const 
{
  return TMath::Sqrt(TMath::Abs( GetCovariance(3,3) ));
}

inline Double_t KFParticle::GetErrPy          () const 
{
  return TMath::Sqrt(TMath::Abs( GetCovariance(4,4) ));
}

inline Double_t KFParticle::GetErrPz          () const 
{
  return TMath::Sqrt(TMath::Abs( GetCovariance(5,5) ));
}

inline Double_t KFParticle::GetErrE           () const 
{
  return TMath::Sqrt(TMath::Abs( GetCovariance(6,6) ));
}

inline Double_t KFParticle::GetErrS           () const 
{
  return TMath::Sqrt(TMath::Abs( GetCovariance(7,7) ));
}

inline Double_t KFParticle::GetErrP    () const
{
  Double_t par, err;
  if( KFParticleBase::GetMomentum( par, err ) ) return 1.e10;
  else return err;
}

inline Double_t KFParticle::GetErrPt    () const
{
  Double_t par, err;
  if( KFParticleBase::GetPt( par, err ) ) return 1.e10;
  else return err;
}

inline Double_t KFParticle::GetErrEta    () const
{
  Double_t par, err;
  if( KFParticleBase::GetEta( par, err ) ) return 1.e10;
  else return err;
}

inline Double_t KFParticle::GetErrPhi    () const
{
  Double_t par, err;
  if( KFParticleBase::GetPhi( par, err ) ) return 1.e10;
  else return err;
}

inline Double_t KFParticle::GetErrMomentum    () const
{
  Double_t par, err;
  if( KFParticleBase::GetMomentum( par, err ) ) return 1.e10;
  else return err;
}

inline Double_t KFParticle::GetErrMass        () const
{
  Double_t par, err;
  if( KFParticleBase::GetMass( par, err ) ) return 1.e10;
  else return err;
}

inline Double_t KFParticle::GetErrDecayLength () const
{
  Double_t par, err;
  if( KFParticleBase::GetDecayLength( par, err ) ) return 1.e10;
  else return err;
}

inline Double_t KFParticle::GetErrDecayLengthXY () const
{
  Double_t par, err;
  if( KFParticleBase::GetDecayLengthXY( par, err ) ) return 1.e10;
  else return err;
}

inline Double_t KFParticle::GetErrLifeTime    () const
{
  Double_t par, err;
  if( KFParticleBase::GetLifeTime( par, err ) ) return 1.e10;
  else return err;
}

inline Double_t KFParticle::GetErrR    () const
{
  Double_t par, err;
  if( KFParticleBase::GetR( par, err ) ) return 1.e10;
  else return err;
}

inline Int_t KFParticle::GetP( Double_t &P, Double_t &SigmaP ) const 
{
  return KFParticleBase::GetMomentum( P, SigmaP );
}
inline Int_t KFParticle::GetPt( Double_t &Pt, Double_t &SigmaPt ) const 
{
  return KFParticleBase::GetPt( Pt, SigmaPt );
}

inline Int_t KFParticle::GetEta( Double_t &Eta, Double_t &SigmaEta ) const 
{
  return KFParticleBase::GetEta( Eta, SigmaEta );
}

inline Int_t KFParticle::GetPhi( Double_t &Phi, Double_t &SigmaPhi ) const 
{
  return KFParticleBase::GetPhi( Phi, SigmaPhi );
}

inline Int_t KFParticle::GetMomentum( Double_t &P, Double_t &SigmaP ) const 
{
  return KFParticleBase::GetMomentum( P, SigmaP );
}

inline Int_t KFParticle::GetMass( Double_t &M, Double_t &SigmaM ) const 
{
  return KFParticleBase::GetMass( M, SigmaM );
}

inline Int_t KFParticle::GetDecayLength( Double_t &L, Double_t &SigmaL ) const 
{
  return KFParticleBase::GetDecayLength( L, SigmaL );
}

inline Int_t KFParticle::GetDecayLengthXY( Double_t &L, Double_t &SigmaL ) const 
{
  return KFParticleBase::GetDecayLengthXY( L, SigmaL );
}

inline Int_t KFParticle::GetLifeTime( Double_t &T, Double_t &SigmaT ) const 
{
  return KFParticleBase::GetLifeTime( T, SigmaT );
}

inline Int_t KFParticle::GetR( Double_t &R, Double_t &SigmaR ) const 
{
  return KFParticleBase::GetR( R, SigmaR );
}
inline Double_t & KFParticle::X() 
{ 
  return KFParticleBase::X();    
}

inline Double_t & KFParticle::Y()
{ 
  return KFParticleBase::Y();    
}

inline Double_t & KFParticle::Z() 
{ 
  return KFParticleBase::Z();    
}

inline Double_t & KFParticle::Px() 
{ 
  return KFParticleBase::Px();   
}

inline Double_t & KFParticle::Py() 
{ 
  return KFParticleBase::Py();   
}

inline Double_t & KFParticle::Pz() 
{ 
  return KFParticleBase::Pz();   
}

inline Double_t & KFParticle::E() 
{ 
  return KFParticleBase::E();    
}

inline Double_t & KFParticle::S() 
{ 
  return KFParticleBase::S();    
}

inline Short_t    & KFParticle::Q() 
{ 
  return KFParticleBase::Q();    
}

inline Double_t & KFParticle::Chi2() 
{ 
  return KFParticleBase::Chi2(); 
}

inline Short_t    & KFParticle::NDF() 
{ 
  return KFParticleBase::NDF();  
}

inline Double_t & KFParticle::Parameter ( Int_t i )        
{ 
  return KFParticleBase::Parameter(i);
}

inline Double_t & KFParticle::Covariance( Int_t i )        
{ 
  return KFParticleBase::Covariance(i);
}

inline Double_t & KFParticle::Covariance( Int_t i, Int_t j ) 
{ 
  return KFParticleBase::Covariance(i,j); 
}
inline Double_t * KFParticle::Parameters ()
{
  return fP;
}

inline Double_t * KFParticle::CovarianceMatrix()
{
  return fC;
}

inline void KFParticle::operator +=( const KFParticle &Daughter )
{
  KFParticleBase::operator +=( Daughter );
}
  

inline void KFParticle::AddDaughter( const KFParticle &Daughter )
{
  KFParticleBase::AddDaughter( Daughter );
}

inline void KFParticle::SetProductionVertex( const KFParticle &Vtx )
{
  KFParticleBase::SetProductionVertex( Vtx );
}

inline void KFParticle::SetMassConstraint( Double_t Mass, Double_t SigmaMass )
{
  KFParticleBase::SetMassConstraint( Mass, SigmaMass );
}
    
inline void KFParticle::SetNoDecayLength()
{
  KFParticleBase::SetNoDecayLength();
}

inline void KFParticle::Construct( const KFParticle *vDaughters[], Int_t NDaughters, 
			       const KFParticle *ProdVtx,   Double_t Mass, Bool_t IsConstrained  )
{    
  KFParticleBase::Construct( ( const KFParticleBase**)vDaughters, NDaughters, 
			 ( const KFParticleBase*)ProdVtx, Mass, IsConstrained );
}

inline void KFParticle::TransportToDecayVertex()
{ 
  KFParticleBase::TransportToDecayVertex(); 
}

inline void KFParticle::TransportToProductionVertex()
{
  KFParticleBase::TransportToProductionVertex();
}
inline void KFParticle::TransportToPoint( const Double_t xyz[] )
{ 
  TransportToDS( GetDStoPoint(xyz) );
}

inline void KFParticle::TransportToVertex( const MVertex &v )
{       
  TransportToPoint( KFParticle(v).fP );
}

inline void KFParticle::TransportToParticle( const KFParticle &p )
{ 
  Double_t dS, dSp;
  GetDStoParticle( p, dS, dSp );
  TransportToDS( dS );
}

inline void KFParticle::TransportToDS( Double_t dS )
{
  KFParticleBase::TransportToDS( dS );
} 

inline Double_t KFParticle::GetDStoPoint( const Double_t xyz[] ) const 
{
  return KFParticleBase::GetDStoPointBz( GetFieldAlice(), xyz );
}

  
inline void KFParticle::GetDStoParticle( const KFParticle &p, 
					    Double_t &DS, Double_t &DSp ) const 
{
  GetDStoParticleXY( p, DS, DSp );
}


inline Double_t KFParticle::GetDistanceFromVertex( const Double_t vtx[] ) const
{
  return KFParticleBase::GetDistanceFromVertex( vtx );
}

inline Double_t KFParticle::GetDeviationFromVertex( const Double_t v[], 
						       const Double_t Cv[] ) const
{
  return KFParticleBase::GetDeviationFromVertex( v, Cv);
}

inline Double_t KFParticle::GetDistanceFromVertex( const KFParticle &Vtx ) const
{
  return KFParticleBase::GetDistanceFromVertex( Vtx );
}

inline Double_t KFParticle::GetDeviationFromVertex( const KFParticle &Vtx ) const
{
  return KFParticleBase::GetDeviationFromVertex( Vtx );
}

inline Double_t KFParticle::GetDistanceFromVertex( const MVertex &Vtx ) const
{
  return GetDistanceFromVertex( KFParticle(Vtx) );
}

inline Double_t KFParticle::GetDeviationFromVertex( const MVertex &Vtx ) const
{
  return GetDeviationFromVertex( KFParticle(Vtx) );
}
inline Double_t KFParticle::GetDistanceFromParticle( const KFParticle &p ) const 
{
  return KFParticleBase::GetDistanceFromParticle( p );
}
inline Double_t KFParticle::GetDeviationFromParticle( const KFParticle &p ) const 
{
  return KFParticleBase::GetDeviationFromParticle( p );
}

inline void KFParticle::SubtractFromVertex( KFParticle &v ) const 
{
  KFParticleBase::SubtractFromVertex( v );
}

inline Double_t KFParticle::GetFieldAlice()
{ 
  return fgBz; 
}

inline void KFParticle::GetFieldValue( const Double_t * /*xyz*/, Double_t B[] ) const 
{    
  B[0] = B[1] = 0;
  B[2] = GetFieldAlice();
}

inline void KFParticle::GetDStoParticle( const KFParticleBase &p, 
					    Double_t &DS, Double_t &DSp )const
{
  GetDStoParticleXY( p, DS, DSp );
}

inline void KFParticle::GetDStoParticleXY( const KFParticleBase &p, 
				       Double_t &DS, Double_t &DSp ) const
{ 
  KFParticleBase::GetDStoParticleBz( GetFieldAlice(), p, DS, DSp ) ;
  //GetDStoParticleALICE( p, DS, DSp ) ;
}

inline void KFParticle::Transport( Double_t dS, Double_t P[], Double_t C[] ) const 
{
  KFParticleBase::TransportBz( GetFieldAlice(), dS, P, C );
}

inline void KFParticle::ConstructGamma( const KFParticle &daughter1,
					   const KFParticle &daughter2  )
{
  KFParticleBase::ConstructGammaBz( daughter1, daughter2, GetFieldAlice() );
}
#endif /* ! __CINT__ */
#endif 
