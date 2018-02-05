//---------------------------------------------------------------------------------
// The KFParticleBase class
// .
// @author  S.Gorbunov, I.Kisel, I.Kulakov, M.Zyzak
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

class KFParticleBase :public TObject {
  
 public:

  //*
  //* ABSTRACT METHODS HAVE TO BE DEFINED IN USER CLASS 
  //* 

  //* Virtual method to access the magnetic field

  virtual void GetFieldValue(const float xyz[], float B[]) const = 0;
  
  //* Virtual methods needed for particle transportation 
  //* One can use particular implementations for collider (only Bz component) 
  //* geometry and for fixed-target (CBM-like) geometry which are provided below 
  //* in TRANSPORT section
 
  //* Get dS to xyz[] space point 

  virtual float GetDStoPoint( const float xyz[3], float dsdr[6] ) const = 0;
  
  float GetDStoPointLine( const float xyz[3], float dsdr[6] ) const;
  float GetDStoPointBz( float B, const float xyz[3], float dsdr[6], const float* param=0) const;
  float GetDStoPointBy( float By, const float xyz[3], float dsdr[6] ) const;
  float GetDStoPointB( const float* B, const float xyz[3], float dsdr[6] ) const;
  float GetDStoPointCBM( const float xyz[3], float dsdr[3] ) const;

  //* Get dS to other particle p (dSp for particle p also returned) 

  virtual void GetDStoParticle( const KFParticleBase &p, float dS[2], float dsdr[4][6] ) const = 0;
  
  void GetDStoParticleLine( const KFParticleBase &p, float dS[2], float dsdr[4][6] ) const ;
  void GetDStoParticleBz( float Bz, const KFParticleBase &p, float dS[2], float dsdr[4][6], const float* param1=0, const float* param2=0 ) const ;
  void GetDStoParticleBy( float B,  const KFParticleBase &p, float dS[2], float dsdr[4][6] ) const ;
  void GetDStoParticleCBM( const KFParticleBase &p, float dS[2], float dsdr[4][6] ) const ;
  
  //* Transport on dS value along trajectory, output to P,C

  virtual void Transport( float dS, const float dsdr[6], float P[], float C[], float* dsdr1=0, float* F=0, float* F1=0 ) const = 0;


  //*
  //*  INITIALIZATION
  //*

  //* Constructor 

  KFParticleBase();

  //* Destructor 

  virtual ~KFParticleBase() { ; }

 //* Initialisation from "cartesian" coordinates ( X Y Z Px Py Pz )
 //* Parameters, covariance matrix, charge, and mass hypothesis should be provided 

  void Initialize( const float Param[], const float Cov[], Int_t Charge, float Mass );

  //* Initialise covariance matrix and set current parameters to 0.0 

  void Initialize();

  //* Set consruction method

  void SetConstructMethod(Int_t m) {fConstructMethod = m;}

  //* Set and get mass hypothesis of the particle
  void SetMassHypo(float m) { fMassHypo = m;}
  const float& GetMassHypo() const { return fMassHypo; }

  //* Returns the sum of masses of the daughters
  const float& GetSumDaughterMass() const {return SumDaughterMass;}

  //*
  //*  ACCESSORS
  //*

  //* Simple accessors 

  float GetX    () const { return fP[0]; }
  float GetY    () const { return fP[1]; }
  float GetZ    () const { return fP[2]; }
  float GetPx   () const { return fP[3]; }
  float GetPy   () const { return fP[4]; }
  float GetPz   () const { return fP[5]; }
  float GetE    () const { return fP[6]; }
  float GetS    () const { return fP[7]; }
  char    GetQ    () const { return fQ;    }
  float GetChi2 () const { return fChi2; }
  Int_t    GetNDF  () const { return fNDF;  }

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
  
  float GetParameter ( Int_t i )        const { return fP[i];       }
  float GetCovariance( Int_t i )        const { return fC[i];       }
  float GetCovariance( Int_t i, Int_t j ) const { return fC[IJ(i,j)]; }

  //* Accessors with calculations( &value, &estimated sigma )
  //* error flag returned (0 means no error during calculations) 

  Int_t GetMomentum    ( float &P, float &SigmaP ) const ;
  Int_t GetPt          ( float &Pt, float &SigmaPt ) const ;
  Int_t GetEta         ( float &Eta, float &SigmaEta ) const ;
  Int_t GetPhi         ( float &Phi, float &SigmaPhi ) const ;
  Int_t GetMass        ( float &M, float &SigmaM ) const ;
  Int_t GetDecayLength ( float &L, float &SigmaL ) const ;
  Int_t GetDecayLengthXY ( float &L, float &SigmaL ) const ;
  Int_t GetLifeTime    ( float &T, float &SigmaT ) const ;
  Int_t GetR           ( float &R, float &SigmaR ) const ;

  //*
  //*  MODIFIERS
  //*
  
  float & X    () { return fP[0]; }
  float & Y    () { return fP[1]; }
  float & Z    () { return fP[2]; }
  float & Px   () { return fP[3]; }
  float & Py   () { return fP[4]; }
  float & Pz   () { return fP[5]; }
  float & E    () { return fP[6]; }
  float & S    () { return fP[7]; }
  char    & Q    () { return fQ;    }
  float & Chi2 () { return fChi2; }
  Int_t    & NDF  () { return fNDF;  }

  float & Parameter ( Int_t i )        { return fP[i];       }
  float & Covariance( Int_t i )        { return fC[i];       }
  float & Covariance( Int_t i, Int_t j ) { return fC[IJ(i,j)]; }


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
  void AddDaughterWithEnergyFitMC( const KFParticleBase &Daughter ); //with mass constrained

  //* Set production vertex 

  void SetProductionVertex( const KFParticleBase &Vtx );

  //* Set mass constraint 

  void SetNonlinearMassConstraint( float Mass );
  void SetMassConstraint( float Mass, float SigmaMass = 0 );

  //* Set no decay length for resonances

  void SetNoDecayLength();


  //* Everything in one go  

  void Construct( const KFParticleBase *vDaughters[], Int_t nDaughters, 
		  const KFParticleBase *ProdVtx=0,   float Mass=-1 );


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

  void TransportToDS( float dS, const float* dsdr );

  //* Particular extrapolators one can use 

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

  float GetDeviationFromVertex( const float v[], 
                                   const float Cv[]=0 ) const;
  float GetDeviationFromVertex( const KFParticleBase &Vtx ) const;
  float GetDeviationFromParticle( const KFParticleBase &p ) const;  

  //* Subtract the particle from the vertex  

  void SubtractFromVertex( KFParticleBase &Vtx ) const;
  void SubtractFromParticle( KFParticleBase &Vtx ) const;

  //* return parameters for the Armenteros-Podolanski plot
  static void GetArmenterosPodolanski(KFParticleBase& positive, KFParticleBase& negative, float QtAlfa[2] );

  //* Rotates the KFParticle object around OZ axis, OZ axis is set by the vertex position
  void RotateXY(float angle, float Vtx[3]);

  int Id() const { return fId; };
  int NDaughters() const { return fDaughtersIds.size(); };
  const std::vector<int>& DaughterIds() const { return fDaughtersIds; };
  void CleanDaughtersId() { fDaughtersIds.clear(); }
  
  void SetId( int id ){ fId = id; }; // should be always used (manualy)
  void AddDaughterId( int id ){ fDaughtersIds.push_back(id); };

  void SetPDG ( int pdg ) { fPDG = pdg; }
  int GetPDG () const { return fPDG; }

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

  static Int_t IJ( Int_t i, Int_t j ){ 
    return ( j<=i ) ? i*(i+1)/2+j :j*(j+1)/2+i;
  }

  float & Cij( Int_t i, Int_t j ){ return fC[IJ(i,j)]; }

  void TransportLine( float S, const float* dsdr, float P[], float C[], float* dsdr1, float* F, float* F1 ) const ;

  static Bool_t InvertSym3( const float A[], float Ainv[] );

  bool GetMeasurement( const KFParticleBase& daughter, float m[], float V[], float D[3][3] ) ;

  //* Mass constraint function. is needed for the nonlinear mass constraint and a fit with mass constraint
  void SetMassConstraint( float *mP, float *mC, float mJ[7][7], float mass );

  float fP[8];  //* Main particle parameters {X,Y,Z,Px,Py,Pz,E,S[=DecayLength/P]}
  float fC[36]; //* Low-triangle covariance matrix of fP
  float fChi2;  //* Chi^2
  float fSFromDecay; //* Distance from decay vertex to current position
  float SumDaughterMass;  //* sum of the daughter particles masses
  float fMassHypo;  //* sum of the daughter particles masse
  Int_t fNDF;   //* Number of degrees of freedom 
  int   fId;                   //* id of particle

#ifdef __ROOT__ //for the STAR experiment
  Short_t    fParentID;
  Short_t    fIdTruth; // MC track id 
  Short_t    fQuality; // quality of this information (percentage of hits coming from the above MC track)
  Short_t    fIdParentMcVx; // for track and McTrack for vertex
#endif

  Bool_t fAtProductionVertex; //* Flag shows that the particle error along
                              //* its trajectory is taken from production vertex    

                          //* ( used for linearisation of equations )

  Bool_t fIsLinearized;   //* Flag shows that the guess is present

  char    fQ;     //* Particle charge 
  char fConstructMethod; //* Determines the method for the particle construction. 
  //* 0 - Energy considered as an independent veriable, fitted independently from momentum, without any constraints on mass
  //* 1 - Energy considered as a dependent variable, calculated from the momentum and mass hypothesis
  //* 2 - Energy considered as an independent variable, fitted independently from momentum, with constraints on mass of daughter particle

  int fPDG; // pdg hypothesis
  std::vector<int> fDaughtersIds; // id of particles it created from. if size == 1 then this is id of track. TODO use in functions. why unsigned short int doesn't work???
 
#ifndef KFParticleStandalone
  ClassDef( KFParticleBase, 2 )
#endif
};

#ifdef __ROOT__ //for the STAR experiment
std::ostream&  operator<<(std::ostream& os, KFParticleBase const & particle);
#endif

#endif 
