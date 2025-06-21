//---------------------------------------------------------------------------------
// The KFParticleBase class
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
// This class describes general mathematics which is used by KFParticle class
// 
//  -= Copyright &copy ALICE HLT Group =-
//_________________________________________________________________________________
#ifndef KFParticleBASE_H
#define KFParticleBASE_H
#include "TObject.h"
class KFParticleBase : public TObject {
 public:
  //  ABSTRACT METHODS HAVE TO BE DEFINED IN USER CLASS 
  //  Virtual method to access the magnetic field
  virtual void GetFieldValue(const Double_t xyz[], Double_t B[]) const = 0;
  //  Virtual methods needed for particle transportation 
  //  One can use particular implementations for collider (only Bz component) 
  //  geometry and for fixed-target (CBM-like) geometry which are provided below 
  //  in TRANSPORT section
 
  //  Get dS to xyz[] space point 
  virtual Double_t GetDStoPoint( const Double_t xyz[] ) const = 0;
  //  Get dS to other particle p (dSp for particle p also returned) 
  virtual void GetDStoParticle( const KFParticleBase &p, 
				Double_t &DS, Double_t &DSp ) const = 0;
  //  Transport on dS value along trajectory, output to P,C
  virtual void Transport( Double_t dS, Double_t P[], Double_t C[] ) const = 0;
  KFParticleBase();
  virtual ~KFParticleBase() {}
  //  Initialisation from "cartesian" coordinates ( X Y Z Px Py Pz )
  //  Parameters, covariance matrix, charge, and mass hypothesis should be provided 
  void Initialize( const Double_t Param[], const Double_t Cov[], Int_t Charge, Double_t Mass, Int_t PID = 0);
  //  Initialise covariance matrix and set current parameters to 0.0 
  void Initialize();
  virtual void        Clear(Option_t * /*option*/ ="");
  //  Set decay vertex parameters for linearisation 
  void SetVtxGuess( Double_t x, Double_t y, Double_t z );
  void SetID(Int_t id=0) {fID = id;}
  void SetParentID(Int_t id=0) {fParentID = id;}
  //   ACCESSORS
  Int_t    GetID() const {return fID;}
  Int_t    GetParentID() const {return fParentID;}
  Double_t GetX    () const { return fP[0]; }
  Double_t GetY    () const { return fP[1]; }
  Double_t GetZ    () const { return fP[2]; }
  Double_t GetPx   () const { return fP[3]; }
  Double_t GetPy   () const { return fP[4]; }
  Double_t GetPz   () const { return fP[5]; }
  Double_t GetE    () const { return fP[6]; }
  Double_t GetS    () const { return fP[7]; }
  Short_t  GetQ    () const { return fQ;    }
  Double_t GetChi2 () const { return fChi2; }
  Short_t  GetNDF  () const { return fNDF;  }
  
  const Double_t *GetParameter()           const { return fP;}
  Double_t GetParameter ( Int_t i )        const { return fP[i];}
  const Double_t *GetCovariance()          const { return fC;}
  Double_t GetCovariance( Int_t i )        const { return fC[i];       }
  Double_t GetCovariance( Int_t i, Int_t j ) const { return fC[IJ(i,j)]; }
  
  //  Accessors with calculations( &value, &estimated sigma )
  //  error flag returned (0 means no error during calculations) 
  
  Int_t GetMomentum    ( Double_t &P, Double_t &SigmaP ) const ;
  Int_t GetPt          ( Double_t &Pt, Double_t &SigmaPt ) const ;
  Int_t GetEta         ( Double_t &Eta, Double_t &SigmaEta ) const ;
  Int_t GetPhi         ( Double_t &Phi, Double_t &SigmaPhi ) const ;
  Int_t GetMass        ( Double_t &M, Double_t &SigmaM ) const ;
  Int_t GetDecayLength ( Double_t &L, Double_t &SigmaL ) const ;
  Int_t GetDecayLengthXY ( Double_t &L, Double_t &SigmaL ) const ;
  Int_t GetLifeTime    ( Double_t &T, Double_t &SigmaT ) const ;
  Int_t GetR           ( Double_t &R, Double_t &SigmaR ) const ;
  
  // 
  //   MODIFIERS
  // 
  
  Double_t & X    () { return fP[0]; }
  Double_t & Y    () { return fP[1]; }
  Double_t & Z    () { return fP[2]; }
  Double_t & Px   () { return fP[3]; }
  Double_t & Py   () { return fP[4]; }
  Double_t & Pz   () { return fP[5]; }
  Double_t & E    () { return fP[6]; }
  Double_t & S    () { return fP[7]; }
  Short_t  & Q    () { return fQ;    }
  Double_t & Chi2 () { return fChi2; }
  Short_t  & NDF  () { return fNDF;  }

  Double_t & Parameter ( Int_t i )        { return fP[i];       }
  Double_t & Covariance( Int_t i )        { return fC[i];       }
  Double_t & Covariance( Int_t i, Int_t j ) { return fC[IJ(i,j)]; }


  //  
  //  CONSTRUCTION OF THE PARTICLE BY ITS DAUGHTERS AND MOTHER
  //  USING THE KALMAN FILTER METHOD
  // 


  //  Simple way to add daughter ex. D0+= Pion; 

  void operator +=( const KFParticleBase &Daughter );  

  //  Add daughter track to the particle 

  void AddDaughter( const KFParticleBase &Daughter );

  //  Set production vertex 

  void SetProductionVertex( const KFParticleBase &Vtx );

  //  Set mass constraint 

  void SetMassConstraint( Double_t Mass, Double_t SigmaMass = 0 );
  
  //  Set no decay length for resonances

  void SetNoDecayLength();


  //  Everything in one go  

  void Construct( const KFParticleBase *vDaughters[], Int_t NDaughters, 
		  const KFParticleBase *ProdVtx=0,   Double_t Mass=-1, Bool_t IsConstrained=0  );
  

  // 
  //                    TRANSPORT
  //  
  //   ( main transportation parameter is S = SignedPath/Momentum )
  //   ( parameters of decay & production vertices are stored locally )
  // 


  //  Transport the particle to its decay vertex 

  void TransportToDecayVertex();

  //  Transport the particle to its production vertex 

  void TransportToProductionVertex();

  //  Transport the particle on dS parameter (SignedPath/Momentum) 

  void TransportToDS( Double_t dS );

  //  Particular extrapolators one can use 

  Double_t GetDStoPointBz( Double_t Bz, const Double_t xyz[] ) const;
  
  void GetDStoParticleBz( Double_t Bz, const KFParticleBase &p, 
			  Double_t &dS, Double_t &dS1       ) const ;
 
  // Double_t GetDStoPointCBM( const Double_t xyz[] ) const;
 
   void TransportBz( Double_t Bz, Double_t dS, Double_t P[], Double_t C[] ) const;
   void TransportCBM( Double_t dS, Double_t P[], Double_t C[] ) const;  


  //  
  //  OTHER UTILITIES
  // 

  //  Calculate distance from another object [cm]

  Double_t GetDistanceFromVertex( const Double_t vtx[] ) const;
  Double_t GetDistanceFromVertex( const KFParticleBase &Vtx ) const;
  Double_t GetDistanceFromParticle( const KFParticleBase &p ) const;

  //  Calculate sqrt(Chi2/ndf) deviation from vertex
  //  v = [xyz], Cv=[Cxx,Cxy,Cyy,Cxz,Cyz,Czz]-covariance matrix

  Double_t GetDeviationFromVertex( const Double_t v[], 
				   const Double_t Cv[]=0 ) const;
  Double_t GetDeviationFromVertex( const KFParticleBase &Vtx ) const;
  Double_t GetDeviationFromParticle( const KFParticleBase &p ) const;  

  //  Subtract the particle from the vertex  

  void SubtractFromVertex( KFParticleBase &Vtx ) const ;
  void SubtractFromParticle( KFParticleBase &Vtx ) const;

    //  Special method for creating gammas

  void ConstructGammaBz( const KFParticleBase &daughter1,
			 const KFParticleBase &daughter2, Double_t Bz  );
  virtual void Print(Option_t *opt="") const;
  Int_t           IdTruth() const { return fIdTruth;}
  Int_t           QaTruth() const { return fQuality; }
  Int_t           IdParentMcVx() const {return fIdParentMcVx;}
  Int_t           IdParentVx()   const {return IdParentMcVx();}
  
  void         SetIdTruth(Int_t idtru,Int_t qatru=0) {fIdTruth = (UShort_t) idtru; fQuality = (UShort_t) qatru;}
  void         SetIdParentMcVx(Int_t id) {fIdParentMcVx = id;}
  void  SetPDG ( Int_t pdg ) { fPDG = pdg; }
  Int_t GetPDG () const { return fPDG; }
 protected:

  static Int_t IJ( Int_t i, Int_t j ){ 
    return ( j<=i ) ? i*(i+1)/2+j :j*(j+1)/2+i;
  }

  Double_t & Cij( Int_t i, Int_t j ){ return fC[IJ(i,j)]; }

  void Convert( Bool_t ToProduction );
  void TransportLine( Double_t S, Double_t P[], Double_t C[] ) const ;
  Double_t GetDStoPointLine( const Double_t xyz[] ) const;

  static Bool_t InvertSym3( const Double_t A[], Double_t Ainv[] );
  static void InvertCholetsky3(Double_t a[6]);
  
  static void MultQSQt( const Double_t Q[], const Double_t S[], 
			Double_t SOut[] );

  static Double_t GetSCorrection( const Double_t Part[], const Double_t XYZ[] );

  void  GetMeasurement( const Double_t XYZ[], Double_t m[], Double_t V[] ) const ;
  
  Char_t     fBeg[1];        //!
  Int_t      fID;
  Int_t      fParentID;
  Double32_t fP[8];  //  Main particle parameters {X,Y,Z,Px,Py,Pz,E,S[=DecayLength/P]}
  Double32_t fC[36]; //  Low-triangle covariance matrix of fP
  Short_t    fQ;     //  Particle charge 
  Short_t    fNDF;   //  Number of degrees of freedom 
  Double32_t fChi2;  //  Chi^2

  Double32_t fSFromDecay; //  Distance from decay vertex to current position
  Bool_t     fAtProductionVertex; //!  Flag shows that the particle error along  its trajectory is taken from production vertex    

  Double32_t fVtxGuess[3];  //!  Guess for the position of the decay vertex ( used for linearisation of equations )
  
  Bool_t     fIsLinearized;   //!  Flag shows that the guess is present
  UShort_t   fIdTruth; // MC track id 
  UShort_t   fQuality; // quality of this information (percentage of hits coming from the above MC track)
  UShort_t   fIdParentMcVx; // for track and McTrack for vertex
  Short_t    fPDG;     // pdg hypothesis
  Char_t     fEnd[1];        //!
  ClassDef(KFParticleBase,4)			    
};
std::ostream&  operator<<(std::ostream& os, KFParticleBase const & particle);
#endif 
