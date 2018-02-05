//---------------------------------------------------------------------------------
// The KFParticleBaseSIMD class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   13.05.07
// 
// Class to reconstruct and store the decayed particle parameters.
// The method is described in CBM-SOFT note 2007-003, 
// ``Reconstruction of decayed particles based on the Kalman filter'', 
// http://www.gsi.de/documents/DOC-2007-May-14-1.pdf
//
// This class describes general mathematics which is used by KFParticle class
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//_________________________________________________________________________________


#ifndef KFParticleBaseSIMD_H
#define KFParticleBaseSIMD_H

#ifdef HLTCA_STANDALONE
#include "RootTypesDef.h"
#endif

#include <vector>
#include "KFParticleDef.h"

class KFParticleBaseSIMD {
  
 public:
                  
  //*
  //* ABSTRACT METHODS HAVE TO BE DEFINED IN USER CLASS 
  //* 
#if  __GNUC__ && ( defined(ENVIRONMENT32) ||  GCC_VERSION < 40300 )

#else
                                            
  void *operator new(size_t size) { return _mm_malloc(size, sizeof(float_v)); }
  void *operator new[](size_t size) { return _mm_malloc(size, sizeof(float_v)); }
  void *operator new(size_t size, void *ptr) { return ::operator new(size, ptr);}
  void *operator new[](size_t size, void *ptr) { return ::operator new(size, ptr);}
  void operator delete(void *ptr, size_t) { _mm_free(ptr); }
  void operator delete[](void *ptr, size_t) { _mm_free(ptr); }
 
#endif 
                  
  //* Virtual method to access the magnetic field

  virtual void GetFieldValue(const float_v xyz[], float_v B[]) const = 0;
  
  //* Virtual methods needed for particle transportation 
  //* One can use particular implementations for collider (only Bz component) 
  //* geometry and for fixed-target (CBM-like) geometry which are provided below 
  //* in TRANSPORT section
 
  //* Get dS to xyz[] space point 

  virtual float_v GetDStoPoint( const float_v xyz[3], float_v dsdr[6] ) const = 0;
  
  float_v GetDStoPointLine( const float_v xyz[3], float_v dsdr[6] ) const;
  float_v GetDStoPointBz( float_v Bz, const float_v xyz[3], float_v dsdr[6], const float_v* param = 0 ) const;
  float_v GetDStoPointBy( float_v By, const float_v xyz[3], float_v dsdr[6] ) const;
  float_v GetDStoPointCBM( const float_v xyz[3], float_v dsdr[6] ) const;

  //* Get dS to other particle p (dSp for particle p also returned) 

  virtual void GetDStoParticle( const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] ) const = 0;
  virtual void GetDStoParticleFast( const KFParticleBaseSIMD &p, float_v dS[2] ) const = 0;
  
  void GetDStoParticleLine( const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] ) const ;
  void GetDStoParticleLine( const KFParticleBaseSIMD &p, float_v dS[2]  ) const ;
  void GetDStoParticleBz( float_v Bz, const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6], const float_v* param1 =0, const float_v* param2 =0  ) const ;
  void GetDStoParticleBz( float_v Bz, const KFParticleBaseSIMD &p, float_v dS[2], const float_v* param1 =0, const float_v* param2 =0  ) const ;
  void GetDStoParticleBy( float_v B, const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] ) const ;
//   void GetDStoParticleBy( float_v B, const KFParticleBaseSIMD &p, float_v dS[2] ) const ;
  void GetDStoParticleB( float_v B[3], const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] ) const;
//   void GetDStoParticleB( float_v B[3], const KFParticleBaseSIMD &p, float_v dS[2] ) const;
  void GetDStoParticleCBM( const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] ) const ;
//   void GetDStoParticleCBM( const KFParticleBaseSIMD &p, float_v dS[2] ) const ;
  
  //* Transport on dS value along trajectory, output to P,C

  virtual void Transport( float_v dS, const float_v dsdr[6], float_v P[], float_v C[], float_v* dsdr1=0, float_v* F=0, float_v* F1=0 ) const = 0;
  virtual void TransportFast( float_v dS, float_v P[] ) const = 0;



  //*
  //*  INITIALIZATION
  //*

  //* Constructor 

  KFParticleBaseSIMD();

  //* Destructor 

  virtual ~KFParticleBaseSIMD() { ; }

 //* Initialisation from "cartesian" coordinates ( X Y Z Px Py Pz )
 //* Parameters, covariance matrix, charge, and mass hypothesis should be provided 

  void Initialize( const float_v Param[], const float_v Cov[], int_v Charge, float_v Mass );

  //* Initialise covariance matrix and set current parameters to 0.0 

  void Initialize();

  //* Set consruction method

  void SetConstructMethod(Int_t m) {fConstructMethod = m;}

  //* Set and get mass hypothesis of the particle
  void SetMassHypo(float_v m) { fMassHypo = m;}
  const float_v& GetMassHypo() const { return fMassHypo; }

  //* Returns the sum of masses of the daughters
  const float_v& GetSumDaughterMass() const {return SumDaughterMass;}

  //*
  //*  ACCESSORS
  //*

  //* Simple accessors 

  float_v GetX    () const { return fP[0]; }
  float_v GetY    () const { return fP[1]; }
  float_v GetZ    () const { return fP[2]; }
  float_v GetPx   () const { return fP[3]; }
  float_v GetPy   () const { return fP[4]; }
  float_v GetPz   () const { return fP[5]; }
  float_v GetE    () const { return fP[6]; }
  float_v GetS    () const { return fP[7]; }
  int_v   GetQ    () const { return fQ;    }
  float_v GetChi2 () const { return fChi2; }
  int_v GetNDF  () const { return fNDF;  }

  const float_v& X    () const { return fP[0]; }
  const float_v& Y    () const { return fP[1]; }
  const float_v& Z    () const { return fP[2]; }
  const float_v& Px   () const { return fP[3]; }
  const float_v& Py   () const { return fP[4]; }
  const float_v& Pz   () const { return fP[5]; }
  const float_v& E    () const { return fP[6]; }
  const float_v& S    () const { return fP[7]; }
  const int_v&   Q    () const { return fQ;    }
  const float_v& Chi2 () const { return fChi2; }
  const int_v& NDF  () const { return fNDF;  }
  
  float_v GetParameter ( Int_t i )        const { return fP[i];       }
  float_v GetCovariance( Int_t i )        const { return fC[i];       }
  float_v GetCovariance( Int_t i, Int_t j ) const { return fC[IJ(i,j)]; }

  //* Accessors with calculations( &value, &estimated sigma )
  //* error flag returned (0 means no error during calculations) 

  float_m GetMomentum    ( float_v &P, float_v &SigmaP ) const ;
  float_m GetPt          ( float_v &Pt, float_v &SigmaPt ) const ;
  float_m GetEta         ( float_v &Eta, float_v &SigmaEta ) const ;
  float_m GetPhi         ( float_v &Phi, float_v &SigmaPhi ) const ;
  float_m GetMass        ( float_v &M, float_v &SigmaM ) const ;
  float_m GetDecayLength ( float_v &L, float_v &SigmaL ) const ;
  float_m GetDecayLengthXY ( float_v &L, float_v &SigmaL ) const ;
  float_m GetLifeTime    ( float_v &T, float_v &SigmaT ) const ;
  float_m GetR           ( float_v &R, float_v &SigmaR ) const ;

  //*
  //*  MODIFIERS
  //*
  
  float_v & X    () { return fP[0]; }
  float_v & Y    () { return fP[1]; }
  float_v & Z    () { return fP[2]; }
  float_v & Px   () { return fP[3]; }
  float_v & Py   () { return fP[4]; }
  float_v & Pz   () { return fP[5]; }
  float_v & E    () { return fP[6]; }
  float_v & S    () { return fP[7]; }
  int_v   & Q    () { return fQ;    }
  float_v & Chi2 () { return fChi2; }
  int_v & NDF  () { return fNDF;  }

  float_v & Parameter ( Int_t i )        { return fP[i];       }
  float_v & Covariance( Int_t i )        { return fC[i];       }
  float_v & Covariance( Int_t i, Int_t j ) { return fC[IJ(i,j)]; }


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

  void GetMaxDistanceToParticleBz(const float_v& B, const KFParticleBaseSIMD &p/*, float_v &r*/ ) const;

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

  //* return parameters for the Armenteros-Podolanski plot
  static void GetArmenterosPodolanski(KFParticleBaseSIMD& positive, KFParticleBaseSIMD& negative, float_v QtAlfa[2] );

  //* Rotates the KFParticle object around OZ axis, OZ axis is set by the vertex position
  void RotateXY(float_v angle, float_v Vtx[3]);

  int_v Id() const { return fId; };
  int NDaughters() const { return fDaughterIds.size(); };
  std::vector<int_v> & DaughterIds() { return fDaughterIds; };
  int_v GetDaughterId(int iD) const { return fDaughterIds[iD]; }
  
  void SetId( int_v id ){ fId = id; }; // should be always used (manualy)
  void SetNDaughters( int n ) { fDaughterIds.reserve(n); }
  void AddDaughterId( int_v id ){ fDaughterIds.push_back(id); };
  void CleanDaughtersId() { fDaughterIds.clear(); }

  void SetPDG ( int pdg ) { fPDG = pdg; }
  void SetPDG ( int_v& pdg ) { fPDG = pdg; }
  const int_v& GetPDG () const { return fPDG; }
  const int_v& PDG () const { return fPDG; }

  void GetDistanceToVertexLine( const KFParticleBaseSIMD &Vertex, float_v &l, float_v &dl, float_m *isParticleFromVertex = 0 ) const;

  static void MultQSQt( const float_v Q[], const float_v S[], float_v SOut[], const int kN  );

 protected:

  static Int_t IJ( Int_t i, Int_t j ){ 
    return ( j<=i ) ? i*(i+1)/2+j :j*(j+1)/2+i;
  }

  float_v & Cij( Int_t i, Int_t j ){ return fC[IJ(i,j)]; }

  void TransportLine( float_v S, const float_v* dsdr, float_v P[], float_v C[], float_v* dsdr1=0, float_v* F=0, float_v* F1=0 ) const ;

  static void InvertCholetsky3(float_v a[6]);

  static void multQSQt1( const float_v J[11], float_v S[] );

  void GetMeasurement( const KFParticleBaseSIMD& daughter, float_v m[], float_v V[], float_v D[3][3] ) ;

  //* Mass constraint function. is needed for the nonlinear mass constraint and a fit with mass constraint
  void SetMassConstraint( float_v *mP, float_v *mC, float_v mJ[7][7], float_v mass, float_m mask );

  float_v fP[8];  //* Main particle parameters {X,Y,Z,Px,Py,Pz,E,S[=DecayLength/P]}
  float_v fC[36]; //* Low-triangle covariance matrix of fP
  int_v fQ;     //* Particle charge 
  int_v fNDF;   //* Number of degrees of freedom 
  float_v fChi2;  //* Chi^2

  float_v fSFromDecay; //* Distance from decay vertex to current position

  float_v SumDaughterMass;  //* sum of the daughter particles masses
  float_v fMassHypo;  //* sum of the daughter particles masses
  
  int_v fId;                   // id of particle

  Bool_t fAtProductionVertex; //* Flag shows that the particle error along
                              //* its trajectory is taken from production vertex    

  int_v fPDG; // pdg hypothesis

  Int_t fConstructMethod; //* Determines the method for the particle construction. 
  //* 0 - Energy considered as an independent veriable, fitted independently from momentum, without any constraints on mass
  //* 1 - Energy considered as a dependent variable, calculated from the momentum and mass hypothesis
  //* 2 - Energy considered as an independent variable, fitted independently from momentum, with constraints on mass of daughter particle


  std::vector<int_v> fDaughterIds; // id of particles it created from. if size == 1 then this is id of track.

} __attribute__((aligned(sizeof(float_v))));

#endif 
