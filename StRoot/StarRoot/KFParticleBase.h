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



#ifndef ALIKFPARTICLEBASE_H
#define ALIKFPARTICLEBASE_H

//#include "TObject.h"

//class KFParticleBase :public TObject {
class KFParticleBase{
  
 public:

  //*
  //* ABSTRACT METHODS HAVE TO BE DEFINED IN USER CLASS 
  //* 

  //* Virtual method to access the magnetic field

  virtual void GetFieldValue(const double xyz[], double B[]) const = 0;
  
  //* Virtual methods needed for particle transportation 
  //* One can use particular implementations for collider (only Bz component) 
  //* geometry and for fixed-target (CBM-like) geometry which are provided below 
  //* in TRANSPORT section
 
  //* Get dS to xyz[] space point 

  virtual double GetDStoPoint( const double xyz[] ) const = 0;

  //* Get dS to other particle p (dSp for particle p also returned) 

  virtual void GetDStoParticle( const KFParticleBase &p, 
				double &DS, double &DSp ) const = 0;
  
  //* Transport on dS value along trajectory, output to P,C

  virtual void Transport( double dS, double P[], double C[] ) const = 0;



  //*
  //*  INITIALIZATION
  //*

  //* Constructor 

  KFParticleBase();

  //* Destructor 

  virtual ~KFParticleBase() { ; }

 //* Initialisation from "cartesian" coordinates ( X Y Z Px Py Pz )
 //* Parameters, covariance matrix, charge, and mass hypothesis should be provided 

  void Initialize( const double Param[], const double Cov[], int Charge, double Mass );

  //* Initialise covariance matrix and set current parameters to 0.0 

  void Initialize();

  //* Set decay vertex parameters for linearisation 

  void SetVtxGuess( double x, double y, double z );

  //*
  //*  ACCESSORS
  //*

  //* Simple accessors 

  double GetX    () const { return fP[0]; }
  double GetY    () const { return fP[1]; }
  double GetZ    () const { return fP[2]; }
  double GetPx   () const { return fP[3]; }
  double GetPy   () const { return fP[4]; }
  double GetPz   () const { return fP[5]; }
  double GetE    () const { return fP[6]; }
  double GetS    () const { return fP[7]; }
  int    GetQ    () const { return fQ;    }
  double GetChi2 () const { return fChi2; }
  int    GetNDF  () const { return fNDF;  }
  
  double GetParameter ( int i )        const { return fP[i];       }
  double GetCovariance( int i )        const { return fC[i];       }
  double GetCovariance( int i, int j ) const { return fC[IJ(i,j)]; }

  //* Accessors with calculations( &value, &estimated sigma )
  //* error flag returned (0 means no error during calculations) 

  int GetMomentum    ( double &P, double &SigmaP ) const ;
  int GetPt          ( double &Pt, double &SigmaPt ) const ;
  int GetEta         ( double &Eta, double &SigmaEta ) const ;
  int GetPhi         ( double &Phi, double &SigmaPhi ) const ;
  int GetMass        ( double &M, double &SigmaM ) const ;
  int GetDecayLength ( double &L, double &SigmaL ) const ;
  int GetLifeTime    ( double &T, double &SigmaT ) const ;
  int GetR           ( double &R, double &SigmaR ) const ;

  //*
  //*  MODIFIERS
  //*
  
  double & X    () { return fP[0]; }
  double & Y    () { return fP[1]; }
  double & Z    () { return fP[2]; }
  double & Px   () { return fP[3]; }
  double & Py   () { return fP[4]; }
  double & Pz   () { return fP[5]; }
  double & E    () { return fP[6]; }
  double & S    () { return fP[7]; }
  int    & Q    () { return fQ;    }
  double & Chi2 () { return fChi2; }
  int    & NDF  () { return fNDF;  }

  double & Parameter ( int i )        { return fP[i];       }
  double & Covariance( int i )        { return fC[i];       }
  double & Covariance( int i, int j ) { return fC[IJ(i,j)]; }


  //* 
  //* CONSTRUCTION OF THE PARTICLE BY ITS DAUGHTERS AND MOTHER
  //* USING THE KALMAN FILTER METHOD
  //*


  //* Simple way to add daughter ex. D0+= Pion; 

  void operator +=( const KFParticleBase &Daughter );  

  //* Add daughter track to the particle 

  void AddDaughter( const KFParticleBase &Daughter );

  //* Set production vertex 

  void SetProductionVertex( const KFParticleBase &Vtx );

  //* Set mass constraint 

  void SetMassConstraint( double Mass, double SigmaMass = 0 );
  
  //* Set no decay length for resonances

  void SetNoDecayLength();


  //* Everything in one go  

  void Construct( const KFParticleBase *vDaughters[], int NDaughters, 
		  const KFParticleBase *ProdVtx=0,   double Mass=-1, bool IsConstrained=0  );


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

  void TransportToDS( double dS );

  //* Particular extrapolators one can use 

  double GetDStoPointBz( double Bz, const double xyz[] ) const;
  
  void GetDStoParticleBz( double Bz, const KFParticleBase &p, 
			  double &dS, double &dS1       ) const ;
 
  // double GetDStoPointCBM( const double xyz[] ) const;
 
   void TransportBz( double Bz, double dS, double P[], double C[] ) const;
   void TransportCBM( double dS, double P[], double C[] ) const;  


  //* 
  //* OTHER UTILITIES
  //*

  //* Calculate distance from another object [cm]

  double GetDistanceFromVertex( const double vtx[] ) const;
  double GetDistanceFromVertex( const KFParticleBase &Vtx ) const;
  double GetDistanceFromParticle( const KFParticleBase &p ) const;

  //* Calculate sqrt(Chi2/ndf) deviation from vertex
  //* v = [xyz], Cv=[Cxx,Cxy,Cyy,Cxz,Cyz,Czz]-covariance matrix

  double GetDeviationFromVertex( const double v[], 
				   const double Cv[]=0 ) const;
  double GetDeviationFromVertex( const KFParticleBase &Vtx ) const;
  double GetDeviationFromParticle( const KFParticleBase &p ) const;  

  //* Subtract the particle from the vertex  

  void SubtractFromVertex( double v[], double Cv[], 
			   double &vChi2, int vNDF ) const ;
  
 protected:

  static int IJ( int i, int j ){ 
    return ( j<=i ) ? i*(i+1)/2+j :j*(j+1)/2+i;
  }

  double & Cij( int i, int j ){ return fC[IJ(i,j)]; }

  void Convert( bool ToProduction );
  void TransportLine( double S, double P[], double C[] ) const ;
  double GetDStoPointLine( const double xyz[] ) const;

  static void MultQSQt( const double Q[], const double S[], 
			double SOut[] );

  static double GetSCorrection( const double Part[], const double XYZ[] );

  void GetMeasurement( const double XYZ[], double m[], double V[] ) const ;

  double fP[8];  //* Main particle parameters {X,Y,Z,Px,Py,Pz,E,S[=DecayLength/P]}
  double fC[36]; //* Low-triangle covariance matrix of fP
  int    fQ;     //* Particle charge 
  int    fNDF;   //* Number of degrees of freedom 
  double fChi2;  //* Chi^2

  double fSFromDecay; //* Distance from decay vertex to current position

  bool fAtProductionVertex; //* Flag shows that the particle error along
                              //* its trajectory is taken from production vertex    

  double fVtxGuess[3];  //* Guess for the position of the decay vertex 
                          //* ( used for linearisation of equations )

  bool fIsLinearized;   //* Flag shows that the guess is present

};

#endif 
