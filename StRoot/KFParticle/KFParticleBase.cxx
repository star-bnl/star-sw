//---------------------------------------------------------------------------------
// Implementation of the KFParticleBase class
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


#include "KFParticleBase.h"
#include <cmath>

#include <iostream>

#ifndef KFParticleStandalone
ClassImp(KFParticleBase)
#endif

#ifdef __ROOT__
#include "TClass.h"
#include "TRSymMatrix.h"
#include "TRVector.h"
KFParticleBase::KFParticleBase() :fChi2(0), fSFromDecay(0), SumDaughterMass(0), fMassHypo(-1), fNDF(-3), 
                                  fId(-1), fParentID(0), fIdTruth(0), fQuality(0), fIdParentMcVx(0), fAtProductionVertex(0),  
                                  fIsLinearized(0), fQ(0), fConstructMethod(0), fPDG(0), fDaughtersIds()
{ 
  static Bool_t first = kTRUE;
  if (first) {
    first = kFALSE;
    KFParticleBase::Class()->IgnoreTObjectStreamer();
  }
  //* Constructor 
  Clear();
}

void KFParticleBase::Clear(Option_t *option) {
  Initialize();
  fIdTruth = 0;
  fQuality = 0;
  fIdParentMcVx = 0;
  fParentID = 0;
}

void KFParticleBase::Print(Option_t *opt) const {
  std::cout << *this << std::endl;
  if (opt && (opt[0] == 'a' || opt[0] == 'A')) {
    TRVector P(8,fP); std::cout << "par. " << P << std::endl;
    TRSymMatrix C(8,fC); std::cout << "cov. " << C << std::endl;
    
  }
}

std::ostream&  operator<<(std::ostream& os, const KFParticleBase& particle) {
  static const Char_t *vn[14] = {"x","y","z","px","py","pz","E","S","M","t","p","Q","Chi2","NDF"};
  os << Form("p(%4i,%4i,%4i)",particle.Id(),particle.GetParentID(),particle.IdParentMcVx());
  for (Int_t i = 0; i < 8; i++) {
    if (i == 6) continue;                                    // E
    if (i == 7 && particle.GetParameter(i) <= 0.0) continue; // S
    if (particle.GetParameter(i) == 0. && particle.GetCovariance(i,i) == 0) continue;
    if (particle.GetCovariance(i,i) > 0) 
      os << Form(" %s:%8.3f+/-%6.3f", vn[i], particle.GetParameter(i), TMath::Sqrt(particle.GetCovariance(i,i)));
    else 
      os << Form(" %s:%8.3f", vn[i], particle.GetParameter(i));
  }
  float Mtp[3] = {0.f, 0.f, 0.f}, MtpErr[3] = {0.f, 0.f, 0.f};
  particle.GetMass(Mtp[0], MtpErr[0]);     if (MtpErr[0] < 1e-7 || MtpErr[0] > 1e10) MtpErr[0] = -13;
  particle.GetLifeTime(Mtp[1], MtpErr[1]); if (MtpErr[1] <=   0 || MtpErr[1] > 1e10) MtpErr[1] = -13;
  particle.GetMomentum(Mtp[2], MtpErr[2]); if (MtpErr[2] <=   0 || MtpErr[2] > 1e10) MtpErr[2] = -13;
  for (Int_t i = 8; i < 11; i++) {
    if (i == 9 && Mtp[i-8] <= 0.0) continue; // t
    if (MtpErr[i-8] > 0 && MtpErr[i-8] <  9e2) os << Form(" %s:%8.3f+/-%7.3f", vn[i],Mtp[i-8],MtpErr[i-8]);
    else                                       os << Form(" %s:%8.3f", vn[i],Mtp[i-8]);
  }
  os << Form(" pdg:%5i Q:%2i  chi2/NDF :%8.2f/%2i",particle.GetPDG(),particle.GetQ(),particle.GetChi2(),particle.GetNDF());
  if (particle.IdTruth()) os << Form(" IdT:%4i/%3i",particle.IdTruth(),particle.QaTruth());
  int nd = particle.NDaughters();
  if (nd > 1) {
    os << " ND: " << nd << ":";
    if (nd > 3) nd = 3;
    for (int d = 0; d < nd; d++) {
      os << particle.DaughterIds()[d];
      if (d < nd-1) os << ",";
    }
  }
  return os;
}
#endif

#ifndef __ROOT__
KFParticleBase::KFParticleBase() : fChi2(0), fSFromDecay(0), 
   SumDaughterMass(0), fMassHypo(-1), fNDF(-3), fId(-1), fAtProductionVertex(0),  fIsLinearized(0), fQ(0), fConstructMethod(0), fPDG(0), fDaughtersIds()
{ 
  //* Constructor 

  Initialize();
}
#endif

void KFParticleBase::Initialize( const float Param[], const float Cov[], Int_t Charge, float Mass )
{
  // Constructor from "cartesian" track, particle mass hypothesis should be provided
  //
  // Param[6] = { X, Y, Z, Px, Py, Pz } - position and momentum
  // Cov [21] = lower-triangular part of the covariance matrix:
  //
  //                (  0  .  .  .  .  . )
  //                (  1  2  .  .  .  . )
  //  Cov. matrix = (  3  4  5  .  .  . ) - numbering of covariance elements in Cov[]
  //                (  6  7  8  9  .  . )
  //                ( 10 11 12 13 14  . )
  //                ( 15 16 17 18 19 20 )


  for( Int_t i=0; i<6 ; i++ ) fP[i] = Param[i];
  for( Int_t i=0; i<21; i++ ) fC[i] = Cov[i];

  float energy = sqrt( Mass*Mass + fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5]);
  fP[6] = energy;
  fP[7] = 0;
  fQ = Charge;
  fNDF = 0;
  fChi2 = 0;
  fAtProductionVertex = 0;
  fIsLinearized = 0;
  fSFromDecay = 0;

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

void KFParticleBase::Initialize()
{
  //* Initialise covariance matrix and set current parameters to 0.0 

  for( Int_t i=0; i<8; i++) fP[i] = 0;
  for(Int_t i=0;i<36;++i) fC[i]=0.;
  fC[0] = fC[2] = fC[5] = 100.;
  fC[35] = 1.;
  fNDF  = -3;
  fChi2 =  0.;
  fQ = 0;
  fSFromDecay = 0;
  fAtProductionVertex = 0;
  fIsLinearized = 0;
  SumDaughterMass = 0;
  fMassHypo = -1;
}

Int_t KFParticleBase::GetMomentum( float &p, float &error )  const 
{
  //* Calculate particle momentum

  float x = fP[3];
  float y = fP[4];
  float z = fP[5];
  float x2 = x*x;
  float y2 = y*y;
  float z2 = z*z;
  float p2 = x2+y2+z2;
  p = sqrt(p2);
  error = (x2*fC[9]+y2*fC[14]+z2*fC[20] + 2*(x*y*fC[13]+x*z*fC[18]+y*z*fC[19]) );
  if( error>1.e-16 && p>1.e-4 ){
    error = sqrt(error)/p;
    return 0;
  }
  error = 1.e8;
  return 1;
}

Int_t KFParticleBase::GetPt( float &pt, float &error )  const 
{
  //* Calculate particle transverse momentum

  float px = fP[3];
  float py = fP[4];
  float px2 = px*px;
  float py2 = py*py;
  float pt2 = px2+py2;
  pt = sqrt(pt2);
  error = (px2*fC[9] + py2*fC[14] + 2*px*py*fC[13] );
  if( error>0 && pt>1.e-4 ){
    error = sqrt(error)/pt;
    return 0;
  }
  error = 1.e10;
  return 1;
}

Int_t KFParticleBase::GetEta( float &eta, float &error )  const 
{
  //* Calculate particle pseudorapidity

  float px = fP[3];
  float py = fP[4];
  float pz = fP[5];
  float pt2 = px*px + py*py;
  float p2 = pt2 + pz*pz;
  float p = sqrt(p2);
  float a = p + pz;
  float b = p - pz;
  eta = 1.e10;
  if( b > 1.e-8 ){
    float c = a/b;
    if( c>1.e-8 ) eta = 0.5*log(c);
  }
  float h3 = -px*pz;
  float h4 = -py*pz;  
  float pt4 = pt2*pt2;
  float p2pt4 = p2*pt4;
  error = (h3*h3*fC[9] + h4*h4*fC[14] + pt4*fC[20] + 2*( h3*(h4*fC[13] + fC[18]*pt2) + pt2*h4*fC[19] ) );

  if( error>0 && p2pt4>1.e-10 ){
    error = sqrt(error/p2pt4);
    return 0;
  }

  error = 1.e10;
  return 1;
}

Int_t KFParticleBase::GetPhi( float &phi, float &error )  const 
{
  //* Calculate particle polar angle

  float px = fP[3];
  float py = fP[4];
  float px2 = px*px;
  float py2 = py*py;
  float pt2 = px2 + py2;
  phi = atan2(py,px);
  error = (py2*fC[9] + px2*fC[14] - 2*px*py*fC[13] );
  if( error>0 && pt2>1.e-4 ){
    error = sqrt(error)/pt2;
    return 0;
  }
  error = 1.e10;
  return 1;
}

Int_t KFParticleBase::GetR( float &r, float &error )  const 
{
  //* Calculate distance to the origin

  float x = fP[0];
  float y = fP[1];
  float x2 = x*x;
  float y2 = y*y;
  r = sqrt(x2 + y2);
  error = (x2*fC[0] + y2*fC[2] - 2*x*y*fC[1] );
  if( error>0 && r>1.e-4 ){
    error = sqrt(error)/r;
    return 0;
  }
  error = 1.e10;
  return 1;
}

Int_t KFParticleBase::GetMass( float &m, float &error ) const 
{
  //* Calculate particle mass
  
  // s = sigma^2 of m2/2

  float s = (  fP[3]*fP[3]*fC[9] + fP[4]*fP[4]*fC[14] + fP[5]*fP[5]*fC[20] 
		  + fP[6]*fP[6]*fC[27] 
		+2*( + fP[3]*fP[4]*fC[13] + fP[5]*(fP[3]*fC[18] + fP[4]*fC[19]) 
		     - fP[6]*( fP[3]*fC[24] + fP[4]*fC[25] + fP[5]*fC[26] )   )
		 ); 
//   float m2 = fabs(fP[6]*fP[6] - fP[3]*fP[3] - fP[4]*fP[4] - fP[5]*fP[5]);
//   m  = sqrt(m2);
//   if( m>1.e-10 ){
//     if( s>=0 ){
//       error = sqrt(s)/m;
//       return 0;
//     }
//   }
//   error = 1.e20;
  float m2 = (fP[6]*fP[6] - fP[3]*fP[3] - fP[4]*fP[4] - fP[5]*fP[5]);

  if(m2<0.)
  {
    error = 1.e3;
    m = -sqrt(-m2);
    return 1;
  }

  m  = sqrt(m2);
  if( m>1.e-6 ){
    if( s >= 0 ) {
      error = sqrt(s)/m;
      return 0;
    }
  }
  else {
    error = 0.;
    return 0;
  }
  error = 1.e3;

  return 1;
}


Int_t KFParticleBase::GetDecayLength( float &l, float &error ) const 
{
  //* Calculate particle decay length [cm]

  float x = fP[3];
  float y = fP[4];
  float z = fP[5];
  float t = fP[7];
  float x2 = x*x;
  float y2 = y*y;
  float z2 = z*z;
  float p2 = x2+y2+z2;
  l = t*sqrt(p2);
  if( p2>1.e-4){
    error = p2*fC[35] + t*t/p2*(x2*fC[9]+y2*fC[14]+z2*fC[20]
				+ 2*(x*y*fC[13]+x*z*fC[18]+y*z*fC[19]) )
      + 2*t*(x*fC[31]+y*fC[32]+z*fC[33]);
    error = sqrt(fabs(error));
    return 0;
  }
  error = 1.e20;
  return 1;
}

Int_t KFParticleBase::GetDecayLengthXY( float &l, float &error ) const 
{
  //* Calculate particle decay length in XY projection [cm]

  float x = fP[3];
  float y = fP[4];
  float t = fP[7];
  float x2 = x*x;
  float y2 = y*y;
  float pt2 = x2+y2;
  l = t*sqrt(pt2);
  if( pt2>1.e-4){
    error = pt2*fC[35] + t*t/pt2*(x2*fC[9]+y2*fC[14] + 2*x*y*fC[13] )
      + 2*t*(x*fC[31]+y*fC[32]);
    error = sqrt(fabs(error));
    return 0;
  }
  error = 1.e20;
  return 1;
}


Int_t KFParticleBase::GetLifeTime( float &tauC, float &error ) const 
{
  //* Calculate particle decay time [s]

  float m, dm;
  GetMass( m, dm );
  float cTM = (-fP[3]*fC[31] - fP[4]*fC[32] - fP[5]*fC[33] + fP[6]*fC[34]);
  tauC = fP[7]*m;
  error = m*m*fC[35] + 2*fP[7]*cTM + fP[7]*fP[7]*dm*dm;
  if( error > 0 ){
    error = sqrt( error );
    return 0;
  }
  error = 1.e20;
  return 1;
}


void KFParticleBase::operator +=( const KFParticleBase &Daughter )
{
  //* Add daughter via operator+=

  AddDaughter( Daughter );
}

bool KFParticleBase::GetMeasurement( const KFParticleBase& daughter, float m[], float V[], float D[3][3] )
{
  //* Get additional covariances V used during measurement

  if(fNDF == -1)
  {
    float ds[2] = {0.f,0.f};
    float dsdr[4][6];
    float F1[36], F2[36], F3[36], F4[36];
    for(int i1=0; i1<36; i1++)
    {
      F1[i1] = 0;
      F2[i1] = 0;
      F3[i1] = 0;
      F4[i1] = 0;
    }
    GetDStoParticle( daughter, ds, dsdr );
    
    if( fabs(ds[0]*fP[5]) > 1000.f || fabs(ds[1]*daughter.fP[5]) > 1000.f)
      return 0;

    float V0Tmp[36] = {0.};
    float V1Tmp[36] = {0.};

    float C[36];
    for(int iC=0; iC<36; iC++)
      C[iC] = fC[iC];
          
             Transport(ds[0], dsdr[0], fP, fC, dsdr[1], F1, F2);
    daughter.Transport(ds[1], dsdr[3],  m,  V, dsdr[2], F4, F3);
    
    MultQSQt(F2, daughter.fC, V0Tmp, 6);
    MultQSQt(F3, C, V1Tmp, 6);
        
    for(int iC=0; iC<21; iC++)
    {
      fC[iC] += V0Tmp[iC];
      V[iC]  += V1Tmp[iC];
    }
    
    float C1F1T[6][6];
    for(int i=0; i<6; i++)
      for(int j=0; j<6; j++)
      {
        C1F1T[i][j] = 0;
        for(int k=0; k<6; k++)
        {
          C1F1T[i][j] +=  C[IJ(i,k)] * F1[j*6+k];
        }
      }
    float F3C1F1T[6][6];
    for(int i=0; i<6; i++)
      for(int j=0; j<6; j++)
      {
        F3C1F1T[i][j] = 0;
        for(int k=0; k<6; k++)
        {
          F3C1F1T[i][j] += F3[i*6+k] * C1F1T[k][j];
        }
      }
    float C2F2T[6][6];
    for(int i=0; i<6; i++)
      for(int j=0; j<6; j++)
      {
        C2F2T[i][j] = 0;
        for(int k=0; k<6; k++)
        {
          C2F2T[i][j] +=  daughter.fC[IJ(i,k)] * F2[j*6+k];
        }
      }
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        D[i][j] = F3C1F1T[i][j];
        for(int k=0; k<6; k++)
        {
          D[i][j] += F4[i*6+k] * C2F2T[k][j];
        }
      }    
  }
  else
  {
    float dsdr[6];
    float dS = daughter.GetDStoPoint(fP, dsdr);
    
    float dsdp[6] = {-dsdr[0], -dsdr[1], -dsdr[2], 0, 0, 0};
    
    float F[36], F1[36];
    for(int i2=0; i2<36; i2++)
    {
      F[i2]  = 0;
      F1[i2] = 0;
    }
    daughter.Transport(dS, dsdr, m, V, dsdp, F, F1);
    
//     float V1Tmp[36] = {0.};
//     MultQSQt(F1, fC, V1Tmp, 6);
    
//     for(int iC=0; iC<21; iC++)
//       V[iC] += V1Tmp[iC];
    
    float VFT[3][6];
    for(int i=0; i<3; i++)
      for(int j=0; j<6; j++)
      {
        VFT[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          VFT[i][j] +=  fC[IJ(i,k)] * F1[j*6+k];
        }
      }
    
    float FVFT[6][6];
    for(int i=0; i<6; i++)
      for(int j=0; j<6; j++)
      {
        FVFT[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          FVFT[i][j] += F1[i*6+k] * VFT[k][j];
        }
      }
      
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        D[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          D[i][j] +=  fC[IJ(j,k)] * F1[i*6+k];
        }
      }
      
    V[0] += FVFT[0][0];
    V[1] += FVFT[1][0];
    V[2] += FVFT[1][1];
    V[3] += FVFT[2][0];
    V[4] += FVFT[2][1];
    V[5] += FVFT[2][2];
    
//     if(fNDF > 100)
//     {
//       float dx = fP[0] - m[0];
//       float dy = fP[1] - m[1];
//       float dz = fP[2] - m[2];
//       float sigmaS = 3.f*sqrt( (dx*dx + dy*dy + dz*dz) / (m[3]*m[3] + m[4]*m[4] + m[5]*m[5]) );
//     
//       float h[3] = { m[3]*sigmaS, m[4]*sigmaS, m[5]*sigmaS };
//       V[0]+= h[0]*h[0];
//       V[1]+= h[1]*h[0];
//       V[2]+= h[1]*h[1];
//       V[3]+= h[2]*h[0];
//       V[4]+= h[2]*h[1];
//       V[5]+= h[2]*h[2];
//     }
  }
  
  return 1;
}

void KFParticleBase::AddDaughter( const KFParticleBase &Daughter )
{
  if( fNDF<-1 ){ // first daughter -> just copy
    fNDF   = -1;
    fQ     =  Daughter.GetQ();
    for( Int_t i=0; i<7; i++) fP[i] = Daughter.fP[i];
    for( Int_t i=0; i<28; i++) fC[i] = Daughter.fC[i];
    fSFromDecay = 0;
    fMassHypo = Daughter.fMassHypo;
    SumDaughterMass = Daughter.SumDaughterMass;
    return;
  }

  if(static_cast<int>(fConstructMethod) == 0)
    AddDaughterWithEnergyFit(Daughter);
  else if(static_cast<int>(fConstructMethod) == 2)
    AddDaughterWithEnergyFitMC(Daughter);

  SumDaughterMass += Daughter.SumDaughterMass;
  fMassHypo = -1;
}

void KFParticleBase::AddDaughterWithEnergyFit( const KFParticleBase &Daughter )
{
  //* Energy considered as an independent veriable, fitted independently from momentum, without any constraints on mass
  //* Add daughter 

  Int_t maxIter = 1;

  for( Int_t iter=0; iter<maxIter; iter++ ){

    float m[8], mV[36];

    float D[3][3];
    if(! GetMeasurement(Daughter, m, mV, D) )
      return;
    
    float mS[6]= { fC[0]+mV[0], 
                   fC[1]+mV[1], fC[2]+mV[2], 
                   fC[3]+mV[3], fC[4]+mV[4], fC[5]+mV[5] };
                   
    InvertCholetsky3(mS);

    //* Residual (measured - estimated)

    float zeta[3] = { m[0]-fP[0], m[1]-fP[1], m[2]-fP[2] };    

    float dChi2 = (mS[0]*zeta[0] + mS[1]*zeta[1] + mS[3]*zeta[2])*zeta[0]
           +      (mS[1]*zeta[0] + mS[2]*zeta[1] + mS[4]*zeta[2])*zeta[1]
           +      (mS[3]*zeta[0] + mS[4]*zeta[1] + mS[5]*zeta[2])*zeta[2]; 
    if (dChi2 > 1e9) return;
//     if(fNDF > 100 && dChi2 > 9) return;
    
    float K[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        K[i][j] = 0;
        for(int k=0; k<3; k++)
          K[i][j] += fC[IJ(i,k)] * mS[IJ(k,j)];
      }
    
    //* CHt = CH' - D'
    float mCHt0[7], mCHt1[7], mCHt2[7];

    mCHt0[0]=fC[ 0] ;       mCHt1[0]=fC[ 1] ;       mCHt2[0]=fC[ 3] ;
    mCHt0[1]=fC[ 1] ;       mCHt1[1]=fC[ 2] ;       mCHt2[1]=fC[ 4] ;
    mCHt0[2]=fC[ 3] ;       mCHt1[2]=fC[ 4] ;       mCHt2[2]=fC[ 5] ;
    mCHt0[3]=fC[ 6]-mV[ 6]; mCHt1[3]=fC[ 7]-mV[ 7]; mCHt2[3]=fC[ 8]-mV[ 8];
    mCHt0[4]=fC[10]-mV[10]; mCHt1[4]=fC[11]-mV[11]; mCHt2[4]=fC[12]-mV[12];
    mCHt0[5]=fC[15]-mV[15]; mCHt1[5]=fC[16]-mV[16]; mCHt2[5]=fC[17]-mV[17];
    mCHt0[6]=fC[21]-mV[21]; mCHt1[6]=fC[22]-mV[22]; mCHt2[6]=fC[23]-mV[23];
  
    //* Kalman gain K = mCH'*S
    
    float k0[7], k1[7], k2[7];
    
    for(Int_t i=0;i<7;++i){
      k0[i] = mCHt0[i]*mS[0] + mCHt1[i]*mS[1] + mCHt2[i]*mS[3];
      k1[i] = mCHt0[i]*mS[1] + mCHt1[i]*mS[2] + mCHt2[i]*mS[4];
      k2[i] = mCHt0[i]*mS[3] + mCHt1[i]*mS[4] + mCHt2[i]*mS[5];
    }

    //* Add the daughter momentum to the particle momentum
    
    fP[ 3] += m[ 3];
    fP[ 4] += m[ 4];
    fP[ 5] += m[ 5];
    fP[ 6] += m[ 6];
  
    fC[ 9] += mV[ 9];
    fC[13] += mV[13];
    fC[14] += mV[14];
    fC[18] += mV[18];
    fC[19] += mV[19];
    fC[20] += mV[20];
    fC[24] += mV[24];
    fC[25] += mV[25];
    fC[26] += mV[26];
    fC[27] += mV[27];
    
 
   //* New estimation of the vertex position r += K*zeta
    
    for(Int_t i=0;i<7;++i) 
      fP[i] = fP[i] + k0[i]*zeta[0] + k1[i]*zeta[1] + k2[i]*zeta[2];
    
    //* New covariance matrix C -= K*(mCH')'

    for(Int_t i=0, k=0;i<7;++i){
      for(Int_t j=0;j<=i;++j,++k){
	fC[k] = fC[k] - (k0[i]*mCHt0[j] + k1[i]*mCHt1[j] + k2[i]*mCHt2[j] );
      }
    }

    float K2[3][3];
    for(int i=0; i<3; i++)
    {
      for(int j=0; j<3; j++)
        K2[i][j] = -K[j][i];
      K2[i][i] += 1;
    }

    float A[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        A[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          A[i][j] += D[i][k] * K2[k][j];
        }
      }
    
    double M[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        M[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          M[i][j] += K[i][k] * A[k][j];
        }
      }

    fC[0] += 2*M[0][0];
    fC[1] += M[0][1] + M[1][0];
    fC[2] += 2*M[1][1];
    fC[3] += M[0][2] + M[2][0];
    fC[4] += M[1][2] + M[2][1];
    fC[5] += 2*M[2][2];
  
    //* Calculate Chi^2 

    fNDF  += 2;
    fQ    +=  Daughter.GetQ();
    fSFromDecay = 0;    
    fChi2 += dChi2;    

  }
}

void KFParticleBase::SubtractDaughter( const KFParticleBase &Daughter )
{
  //* Energy considered as an independent variable, fitted independently from momentum, without any constraints on mass
  //* Add daughter 

  float m[8], mV[36];

  float D[3][3];
  if(! GetMeasurement(Daughter, m, mV, D) )
    return;

//     std::cout << "X: " << fC[0] << " " << mV[0] << " Y: " << fC[2] << " "<< mV[2] << " Z: "<< fC[5] << " "<< mV[5] << std::endl;
  
  float mS[6]= { fC[0]+mV[0], 
		  fC[1]+mV[1], fC[2]+mV[2], 
		  fC[3]+mV[3], fC[4]+mV[4], fC[5]+mV[5] };
		  
  InvertCholetsky3(mS);

  //* Residual (measured - estimated)

  float zeta[3] = { m[0]-fP[0], m[1]-fP[1], m[2]-fP[2] };    

  float dChi2 = (mS[0]*zeta[0] + mS[1]*zeta[1] + mS[3]*zeta[2])*zeta[0]
	  +      (mS[1]*zeta[0] + mS[2]*zeta[1] + mS[4]*zeta[2])*zeta[1]
	  +      (mS[3]*zeta[0] + mS[4]*zeta[1] + mS[5]*zeta[2])*zeta[2]; 
  
  float K[3][3];
  for(int i=0; i<3; i++)
    for(int j=0; j<3; j++)
    {
      K[i][j] = 0;
      for(int k=0; k<3; k++)
	K[i][j] += fC[IJ(i,k)] * mS[IJ(k,j)];
    }
  
  //* CHt = CH' - D'
  float mCHt0[7], mCHt1[7], mCHt2[7];

  mCHt0[0]=fC[ 0] ;       mCHt1[0]=fC[ 1] ;       mCHt2[0]=fC[ 3] ;
  mCHt0[1]=fC[ 1] ;       mCHt1[1]=fC[ 2] ;       mCHt2[1]=fC[ 4] ;
  mCHt0[2]=fC[ 3] ;       mCHt1[2]=fC[ 4] ;       mCHt2[2]=fC[ 5] ;
  mCHt0[3]=fC[ 6]+mV[ 6]; mCHt1[3]=fC[ 7]+mV[ 7]; mCHt2[3]=fC[ 8]+mV[ 8];
  mCHt0[4]=fC[10]+mV[10]; mCHt1[4]=fC[11]+mV[11]; mCHt2[4]=fC[12]+mV[12];
  mCHt0[5]=fC[15]+mV[15]; mCHt1[5]=fC[16]+mV[16]; mCHt2[5]=fC[17]+mV[17];
  mCHt0[6]=fC[21]+mV[21]; mCHt1[6]=fC[22]+mV[22]; mCHt2[6]=fC[23]+mV[23];

  //* Kalman gain K = mCH'*S
  
  float k0[7], k1[7], k2[7];
  
  for(Int_t i=0;i<7;++i){
    k0[i] = mCHt0[i]*mS[0] + mCHt1[i]*mS[1] + mCHt2[i]*mS[3];
    k1[i] = mCHt0[i]*mS[1] + mCHt1[i]*mS[2] + mCHt2[i]*mS[4];
    k2[i] = mCHt0[i]*mS[3] + mCHt1[i]*mS[4] + mCHt2[i]*mS[5];
  }

  //* Add the daughter momentum to the particle momentum
  
  fP[ 3] -= m[ 3];
  fP[ 4] -= m[ 4];
  fP[ 5] -= m[ 5];
  fP[ 6] -= m[ 6];

  fC[ 9] += mV[ 9];
  fC[13] += mV[13];
  fC[14] += mV[14];
  fC[18] += mV[18];
  fC[19] += mV[19];
  fC[20] += mV[20];
  fC[24] += mV[24];
  fC[25] += mV[25];
  fC[26] += mV[26];
  fC[27] += mV[27];
  

  //* New estimation of the vertex position r += K*zeta
  
  for(Int_t i=0;i<7;++i) 
    fP[i] = fP[i] + k0[i]*zeta[0] + k1[i]*zeta[1] + k2[i]*zeta[2];
  
  //* New covariance matrix C -= K*(mCH')'

  for(Int_t i=0, k=0;i<7;++i){
    for(Int_t j=0;j<=i;++j,++k){
      fC[k] = fC[k] - (k0[i]*mCHt0[j] + k1[i]*mCHt1[j] + k2[i]*mCHt2[j] );
    }
  }

  float K2[3][3];
  for(int i=0; i<3; i++)
  {
    for(int j=0; j<3; j++)
      K2[i][j] = -K[j][i];
    K2[i][i] += 1;
  }

  float A[3][3];
  for(int i=0; i<3; i++)
    for(int j=0; j<3; j++)
    {
      A[i][j] = 0;
      for(int k=0; k<3; k++)
      {
	A[i][j] += D[i][k] * K2[k][j];
      }
    }
  
  double M[3][3];
  for(int i=0; i<3; i++)
    for(int j=0; j<3; j++)
    {
      M[i][j] = 0;
      for(int k=0; k<3; k++)
      {
	M[i][j] += K[i][k] * A[k][j];
      }
    }

  fC[0] += 2*M[0][0];
  fC[1] += M[0][1] + M[1][0];
  fC[2] += 2*M[1][1];
  fC[3] += M[0][2] + M[2][0];
  fC[4] += M[1][2] + M[2][1];
  fC[5] += 2*M[2][2];

  //* Calculate Chi^2 

  fNDF  += 2;
  fQ    +=  Daughter.GetQ();
  fSFromDecay = 0;    
  fChi2 += dChi2;    
}


void KFParticleBase::AddDaughterWithEnergyFitMC( const KFParticleBase &Daughter )
{
  //* Energy considered as an independent variable, fitted independently from momentum, without any constraints on mass
  //* Add daughter 

  Int_t maxIter = 1;

  for( Int_t iter=0; iter<maxIter; iter++ ){

    float m[8], mV[36];

    float D[3][3];
    GetMeasurement(Daughter, m, mV, D);
    
    float mS[6]= { fC[0]+mV[0], 
                   fC[1]+mV[1], fC[2]+mV[2], 
                   fC[3]+mV[3], fC[4]+mV[4], fC[5]+mV[5] };
    InvertCholetsky3(mS);
    //* Residual (measured - estimated)

    float zeta[3] = { m[0]-fP[0], m[1]-fP[1], m[2]-fP[2] };    

    float K[3][6];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        K[i][j] = 0;
        for(int k=0; k<3; k++)
          K[i][j] += fC[IJ(i,k)] * mS[IJ(k,j)];
      }

    
    //* CHt = CH'
    
    float mCHt0[7], mCHt1[7], mCHt2[7];
    
    mCHt0[0]=fC[ 0] ; mCHt1[0]=fC[ 1] ; mCHt2[0]=fC[ 3] ;
    mCHt0[1]=fC[ 1] ; mCHt1[1]=fC[ 2] ; mCHt2[1]=fC[ 4] ;
    mCHt0[2]=fC[ 3] ; mCHt1[2]=fC[ 4] ; mCHt2[2]=fC[ 5] ;
    mCHt0[3]=fC[ 6] ; mCHt1[3]=fC[ 7] ; mCHt2[3]=fC[ 8] ;
    mCHt0[4]=fC[10] ; mCHt1[4]=fC[11] ; mCHt2[4]=fC[12] ;
    mCHt0[5]=fC[15] ; mCHt1[5]=fC[16] ; mCHt2[5]=fC[17] ;
    mCHt0[6]=fC[21] ; mCHt1[6]=fC[22] ; mCHt2[6]=fC[23] ;
  
    //* Kalman gain K = mCH'*S
    
    float k0[7], k1[7], k2[7];
    
    for(Int_t i=0;i<7;++i){
      k0[i] = mCHt0[i]*mS[0] + mCHt1[i]*mS[1] + mCHt2[i]*mS[3];
      k1[i] = mCHt0[i]*mS[1] + mCHt1[i]*mS[2] + mCHt2[i]*mS[4];
      k2[i] = mCHt0[i]*mS[3] + mCHt1[i]*mS[4] + mCHt2[i]*mS[5];
    }

    // last itearation -> update the particle

    //* VHt = VH'
    
    float mVHt0[7], mVHt1[7], mVHt2[7];
    
    mVHt0[0]=mV[ 0] ; mVHt1[0]=mV[ 1] ; mVHt2[0]=mV[ 3] ;
    mVHt0[1]=mV[ 1] ; mVHt1[1]=mV[ 2] ; mVHt2[1]=mV[ 4] ;
    mVHt0[2]=mV[ 3] ; mVHt1[2]=mV[ 4] ; mVHt2[2]=mV[ 5] ;
    mVHt0[3]=mV[ 6] ; mVHt1[3]=mV[ 7] ; mVHt2[3]=mV[ 8] ;
    mVHt0[4]=mV[10] ; mVHt1[4]=mV[11] ; mVHt2[4]=mV[12] ;
    mVHt0[5]=mV[15] ; mVHt1[5]=mV[16] ; mVHt2[5]=mV[17] ;
    mVHt0[6]=mV[21] ; mVHt1[6]=mV[22] ; mVHt2[6]=mV[23] ;
  
    //* Kalman gain Km = mCH'*S
    
    float km0[7], km1[7], km2[7];
    
    for(Int_t i=0;i<7;++i){
      km0[i] = mVHt0[i]*mS[0] + mVHt1[i]*mS[1] + mVHt2[i]*mS[3];
      km1[i] = mVHt0[i]*mS[1] + mVHt1[i]*mS[2] + mVHt2[i]*mS[4];
      km2[i] = mVHt0[i]*mS[3] + mVHt1[i]*mS[4] + mVHt2[i]*mS[5];
    }

    for(Int_t i=0;i<7;++i) 
      fP[i] = fP[i] + k0[i]*zeta[0] + k1[i]*zeta[1] + k2[i]*zeta[2];

    for(Int_t i=0;i<7;++i) 
      m[i] = m[i] - km0[i]*zeta[0] - km1[i]*zeta[1] - km2[i]*zeta[2];

    for(Int_t i=0, k=0;i<7;++i){
      for(Int_t j=0;j<=i;++j,++k){
	fC[k] = fC[k] - (k0[i]*mCHt0[j] + k1[i]*mCHt1[j] + k2[i]*mCHt2[j] );
      }
    }

    for(Int_t i=0, k=0;i<7;++i){
      for(Int_t j=0;j<=i;++j,++k){
	mV[k] = mV[k] - (km0[i]*mVHt0[j] + km1[i]*mVHt1[j] + km2[i]*mVHt2[j] );
      }
    }

    float mDf[7][7];

    for(Int_t i=0;i<7;++i){
      for(Int_t j=0;j<7;++j){
	mDf[i][j] = (km0[i]*mCHt0[j] + km1[i]*mCHt1[j] + km2[i]*mCHt2[j] );
      }
    }

    float mJ1[7][7], mJ2[7][7];
    for(Int_t iPar1=0; iPar1<7; iPar1++)
    {
      for(Int_t iPar2=0; iPar2<7; iPar2++)
      {
        mJ1[iPar1][iPar2] = 0;
        mJ2[iPar1][iPar2] = 0;
      }
    }

    float mMassParticle  = fP[6]*fP[6] - (fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5]);
    float mMassDaughter  = m[6]*m[6] - (m[3]*m[3] + m[4]*m[4] + m[5]*m[5]);
    if(mMassParticle > 0) mMassParticle = sqrt(mMassParticle);
    if(mMassDaughter > 0) mMassDaughter = sqrt(mMassDaughter);

    if( fMassHypo > -0.5)
      SetMassConstraint(fP,fC,mJ1,fMassHypo);
    else if((mMassParticle < SumDaughterMass) || (fP[6]<0) )
      SetMassConstraint(fP,fC,mJ1,SumDaughterMass);

    if(Daughter.fMassHypo > -0.5)
      SetMassConstraint(m,mV,mJ2,Daughter.fMassHypo);
    else if((mMassDaughter < Daughter.SumDaughterMass) || (m[6] < 0) )
      SetMassConstraint(m,mV,mJ2,Daughter.SumDaughterMass);

    float mDJ[7][7];

    for(Int_t i=0; i<7; i++) {
      for(Int_t j=0; j<7; j++) {
        mDJ[i][j] = 0;
        for(Int_t k=0; k<7; k++) {
          mDJ[i][j] += mDf[i][k]*mJ1[j][k];
        }
      }
    }

    for(Int_t i=0; i<7; ++i){
      for(Int_t j=0; j<7; ++j){
        mDf[i][j]=0;
        for(Int_t l=0; l<7; l++){
          mDf[i][j] += mJ2[i][l]*mDJ[l][j];
        }
      }
    }

    //* Add the daughter momentum to the particle momentum

    fP[ 3] += m[ 3];
    fP[ 4] += m[ 4];
    fP[ 5] += m[ 5];
    fP[ 6] += m[ 6];

    fC[ 9] += mV[ 9];
    fC[13] += mV[13];
    fC[14] += mV[14];
    fC[18] += mV[18];
    fC[19] += mV[19];
    fC[20] += mV[20];
    fC[24] += mV[24];
    fC[25] += mV[25];
    fC[26] += mV[26];
    fC[27] += mV[27];

    fC[6 ] += mDf[3][0]; fC[7 ] += mDf[3][1]; fC[8 ] += mDf[3][2];
    fC[10] += mDf[4][0]; fC[11] += mDf[4][1]; fC[12] += mDf[4][2];
    fC[15] += mDf[5][0]; fC[16] += mDf[5][1]; fC[17] += mDf[5][2];
    fC[21] += mDf[6][0]; fC[22] += mDf[6][1]; fC[23] += mDf[6][2];

    fC[9 ] += mDf[3][3] + mDf[3][3];
    fC[13] += mDf[4][3] + mDf[3][4]; fC[14] += mDf[4][4] + mDf[4][4];
    fC[18] += mDf[5][3] + mDf[3][5]; fC[19] += mDf[5][4] + mDf[4][5]; fC[20] += mDf[5][5] + mDf[5][5];
    fC[24] += mDf[6][3] + mDf[3][6]; fC[25] += mDf[6][4] + mDf[4][6]; fC[26] += mDf[6][5] + mDf[5][6]; fC[27] += mDf[6][6] + mDf[6][6];

    
    float K2[3][3];
    for(int i=0; i<3; i++)
    {
      for(int j=0; j<3; j++)
        K2[i][j] = -K[j][i];
      K2[i][i] += 1;
    }

    float A[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        A[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          A[i][j] += D[i][k] * K2[k][j];
        }
      }
    
    double M[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        M[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          M[i][j] += K[i][k] * A[k][j];
        }
      }

    fC[0] += 2*M[0][0];
    fC[1] += M[0][1] + M[1][0];
    fC[2] += 2*M[1][1];
    fC[3] += M[0][2] + M[2][0];
    fC[4] += M[1][2] + M[2][1];
    fC[5] += 2*M[2][2];
    
    //* Calculate Chi^2 

    fNDF  += 2;
    fQ    +=  Daughter.GetQ();
    fSFromDecay = 0;    
    fChi2 += (mS[0]*zeta[0] + mS[1]*zeta[1] + mS[3]*zeta[2])*zeta[0]
      +      (mS[1]*zeta[0] + mS[2]*zeta[1] + mS[4]*zeta[2])*zeta[1]
      +      (mS[3]*zeta[0] + mS[4]*zeta[1] + mS[5]*zeta[2])*zeta[2];
  }
}

void KFParticleBase::SetProductionVertex( const KFParticleBase &Vtx )
{
  //* Set production vertex for the particle, when the particle was not used in the vertex fit

  const float *m = Vtx.fP, *mV = Vtx.fC;

  float decayPoint[3] = {fP[0], fP[1], fP[2]};
  float decayPointCov[6] = { fC[0], fC[1], fC[2], fC[3], fC[4], fC[5] };

  float D[6][6];
  for(int iD1=0; iD1<6; iD1++)
    for(int iD2=0; iD2<6; iD2++)
      D[iD1][iD2] = 0.f;

  Bool_t noS = ( fC[35]<=0 ); // no decay length allowed

  if( noS ){ 
    TransportToDecayVertex();
    fP[7] = 0;
    fC[28] = fC[29] = fC[30] = fC[31] = fC[32] = fC[33] = fC[34] = fC[35] = 0;
  }
  else
  {
    float dsdr[6] = {0.f, 0.f, 0.f, 0.f, 0.f, 0.f};
    float dS = GetDStoPoint(Vtx.fP, dsdr);
    
    float dsdp[6] = {-dsdr[0], -dsdr[1], -dsdr[2], 0, 0, 0};
    
    float F[36], F1[36];
    for(int i2=0; i2<36; i2++)
    {
      F[i2]  = 0;
      F1[i2] = 0;
    }
    Transport( dS, dsdr, fP, fC, dsdp, F, F1 );
    
    float CTmp[36] = {0.};
    MultQSQt(F1, mV, CTmp, 6);
    
    for(int iC=0; iC<6; iC++)
      fC[iC] += CTmp[iC];
          
    for(int i=0; i<6; i++)
      for(int j=0; j<3; j++)
      {
        D[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          D[i][j] +=  mV[IJ(j,k)] * F1[i*6+k];
        }
      }
  }

  float mS[6] = { fC[0] + mV[0],
                  fC[1] + mV[1], fC[2] + mV[2],
                  fC[3] + mV[3], fC[4] + mV[4], fC[5] + mV[5] };                 
  InvertCholetsky3(mS);
  
  float res[3] = { m[0] - X(), m[1] - Y(), m[2] - Z() };
  
  float K[3][6];  
  for(int i=0; i<3; i++)
    for(int j=0; j<3; j++)
    {
      K[i][j] = 0;
      for(int k=0; k<3; k++)
        K[i][j] += fC[IJ(i,k)] * mS[IJ(k,j)];
    }
  
  float mCHt0[7], mCHt1[7], mCHt2[7];
  mCHt0[0]=fC[ 0];        mCHt1[0]=fC[ 1];        mCHt2[0]=fC[ 3];
  mCHt0[1]=fC[ 1];        mCHt1[1]=fC[ 2];        mCHt2[1]=fC[ 4];
  mCHt0[2]=fC[ 3];        mCHt1[2]=fC[ 4];        mCHt2[2]=fC[ 5];
  mCHt0[3]=fC[ 6];        mCHt1[3]=fC[ 7];        mCHt2[3]=fC[ 8];
  mCHt0[4]=fC[10];        mCHt1[4]=fC[11];        mCHt2[4]=fC[12];
  mCHt0[5]=fC[15];        mCHt1[5]=fC[16];        mCHt2[5]=fC[17];
  mCHt0[6]=fC[21];        mCHt1[6]=fC[22];        mCHt2[6]=fC[23];
  
  float k0[7], k1[7], k2[7];
  for(Int_t i=0;i<7;++i){
    k0[i] = mCHt0[i]*mS[0] + mCHt1[i]*mS[1] + mCHt2[i]*mS[3];
    k1[i] = mCHt0[i]*mS[1] + mCHt1[i]*mS[2] + mCHt2[i]*mS[4];
    k2[i] = mCHt0[i]*mS[3] + mCHt1[i]*mS[4] + mCHt2[i]*mS[5];
  }
  
  for(Int_t i=0;i<7;++i) 
    fP[i] = fP[i] + k0[i]*res[0] + k1[i]*res[1] + k2[i]*res[2];

  for(Int_t i=0, k=0;i<7;++i){
    for(Int_t j=0;j<=i;++j,++k){
      fC[k] = fC[k] - (k0[i]*mCHt0[j] + k1[i]*mCHt1[j] + k2[i]*mCHt2[j] );
    }
  }

  float K2[3][3];
  for(int i=0; i<3; i++)
  {
    for(int j=0; j<3; j++)
      K2[i][j] = -K[j][i];
    K2[i][i] += 1;
  }

  float A[3][3];
  for(int i=0; i<3; i++)
    for(int j=0; j<3; j++)
    {
      A[i][j] = 0;
      for(int k=0; k<3; k++)
      {
        A[i][j] += D[k][i] * K2[k][j];
      }
    }
  
  double M[3][3];
  for(int i=0; i<3; i++)
    for(int j=0; j<3; j++)
    {
      M[i][j] = 0;
      for(int k=0; k<3; k++)
      {
        M[i][j] += K[i][k] * A[k][j];
      }
    }
    
  fC[0] += 2*M[0][0];
  fC[1] += M[0][1] + M[1][0];
  fC[2] += 2*M[1][1];
  fC[3] += M[0][2] + M[2][0];
  fC[4] += M[1][2] + M[2][1];
  fC[5] += 2*M[2][2];
  
  fChi2 += (mS[0]*res[0] + mS[1]*res[1] + mS[3]*res[2])*res[0]
        +  (mS[1]*res[0] + mS[2]*res[1] + mS[4]*res[2])*res[1]
        +  (mS[3]*res[0] + mS[4]*res[1] + mS[5]*res[2])*res[2];
  fNDF += 2;
   
  if( noS ){ 
    fP[7] = 0;
    fC[28] = fC[29] = fC[30] = fC[31] = fC[32] = fC[33] = fC[34] = fC[35] = 0;
    fSFromDecay = 0;
  }
  else
  {
    float dsdr[6] = {0.f, 0.f, 0.f, 0.f, 0.f, 0.f};
    fP[7] = GetDStoPoint(decayPoint, dsdr);   
    
    float dsdp[6] = {-dsdr[0], -dsdr[1], -dsdr[2], 0, 0, 0};

    float F[36], F1[36];
    for(int i2=0; i2<36; i2++)
    {
      F[i2]  = 0;
      F1[i2] = 0;
    }
    float tmpP[8], tmpC[36]; 
    Transport( fP[7], dsdr, tmpP, tmpC, dsdp, F, F1 );
          
    fC[35] = 0;
    for(int iDsDr=0; iDsDr<6; iDsDr++)
    {
      float dsdrC = 0, dsdpV = 0;
      
      for(int k=0; k<6; k++)
        dsdrC += dsdr[k] * fC[IJ(k,iDsDr)]; // (-dsdr[k])*fC[k,j]
      
      fC[iDsDr+28] = dsdrC;
      fC[35] += dsdrC*dsdr[iDsDr] ;
      if(iDsDr < 3)
      {
        for(int k=0; k<3; k++)
          dsdpV -= dsdr[k] * decayPointCov[IJ(k,iDsDr)];  //
        fC[35] -= dsdpV*dsdr[iDsDr];
      }
    }  
    fSFromDecay = -fP[7];
  }
  
  fAtProductionVertex = 1;
}

void KFParticleBase::SetMassConstraint( float *mP, float *mC, float mJ[7][7], float mass )
{
  //* Set nonlinear mass constraint (Mass) on the state vector mP with a covariance matrix mC.
  
  const float energy2 = mP[6]*mP[6], p2 = mP[3]*mP[3]+mP[4]*mP[4]+mP[5]*mP[5], mass2 = mass*mass;

  const float a = energy2 - p2 + 2.*mass2;
  const float b = -2.*(energy2 + p2);
  const float c = energy2 - p2 - mass2;

  float lambda = 0;
  if(fabs(b) > 1.e-10) lambda = -c / b;

  float d = 4.*energy2*p2 - mass2*(energy2-p2-2.*mass2);
  if(d>=0 && fabs(a) > 1.e-10) lambda = (energy2 + p2 - sqrt(d))/a;

  if(mP[6] < 0) //If energy < 0 we need a lambda < 0
    lambda = -1000000.; //Empirical, a better solution should be found

  Int_t iIter=0;
  for(iIter=0; iIter<100; iIter++)
  {
    float lambda2 = lambda*lambda;
    float lambda4 = lambda2*lambda2;

    float lambda0 = lambda;

    float f  = -mass2 * lambda4 + a*lambda2 + b*lambda + c;
    float df = -4.*mass2 * lambda2*lambda + 2.*a*lambda + b;
    if(fabs(df) < 1.e-10) break;
    lambda -= f/df;
    if(fabs(lambda0 - lambda) < 1.e-8) break;
  }

  const float lpi = 1./(1. + lambda);
  const float lmi = 1./(1. - lambda);
  const float lp2i = lpi*lpi;
  const float lm2i = lmi*lmi;

  float lambda2 = lambda*lambda;

  float dfl  = -4.*mass2 * lambda2*lambda + 2.*a*lambda + b;
  float dfx[7] = {0};//,0,0,0};
  dfx[0] = -2.*(1. + lambda)*(1. + lambda)*mP[3];
  dfx[1] = -2.*(1. + lambda)*(1. + lambda)*mP[4];
  dfx[2] = -2.*(1. + lambda)*(1. + lambda)*mP[5];
  dfx[3] = 2.*(1. - lambda)*(1. - lambda)*mP[6];
  float dlx[4] = {1,1,1,1};
  if(fabs(dfl) > 1.e-10 )
  {
    for(int i=0; i<4; i++)
      dlx[i] = -dfx[i] / dfl;
  }

  float dxx[4] = {mP[3]*lm2i, mP[4]*lm2i, mP[5]*lm2i, -mP[6]*lp2i};

  for(Int_t i=0; i<7; i++)
    for(Int_t j=0; j<7; j++)
      mJ[i][j]=0;
  mJ[0][0] = 1.;
  mJ[1][1] = 1.;
  mJ[2][2] = 1.;

  for(Int_t i=3; i<7; i++)
    for(Int_t j=3; j<7; j++)
      mJ[i][j] = dlx[j-3]*dxx[i-3];

  for(Int_t i=3; i<6; i++)
    mJ[i][i] += lmi;
  mJ[6][6] += lpi;

  float mCJ[7][7];

  for(Int_t i=0; i<7; i++) {
    for(Int_t j=0; j<7; j++) {
      mCJ[i][j] = 0;
      for(Int_t k=0; k<7; k++) {
        mCJ[i][j] += mC[IJ(i,k)]*mJ[j][k];
      }
    }
  }

  for(Int_t i=0; i<7; ++i){
    for(Int_t j=0; j<=i; ++j){
      mC[IJ(i,j)]=0;
      for(Int_t l=0; l<7; l++){
        mC[IJ(i,j)] += mJ[i][l]*mCJ[l][j];
      }
    }
  }

  mP[3] *= lmi;
  mP[4] *= lmi;
  mP[5] *= lmi;
  mP[6] *= lpi;
}

void KFParticleBase::SetNonlinearMassConstraint( float mass )
{
  //* Set nonlinear mass constraint (mass)

  const float& px = fP[3];
  const float& py = fP[4];
  const float& pz = fP[5];
  const float& energy  = fP[6];
  
  const float residual = (energy*energy - px*px - py*py - pz*pz) - mass*mass;
  const float dm2 = float(4.f) * ( fC[9]*px*px + fC[14]*py*py + fC[20]*pz*pz + fC[27]*energy*energy +
                      float(2.f) * ( (fC[13]*py + fC[18]*pz - fC[24]*energy)*px + (fC[19]*pz - fC[25]*energy)*py - fC[26]*pz*energy) );
  const float dChi2 = residual*residual / dm2;
  fChi2 += dChi2;
  fNDF  += 1;
  
  float mJ[7][7];
  SetMassConstraint( fP, fC, mJ, mass );
  fMassHypo = mass;
  SumDaughterMass = mass;
}

void KFParticleBase::SetMassConstraint( float Mass, float SigmaMass )
{  
  //* Set hard( SigmaMass=0 ) or soft (SigmaMass>0) mass constraint 

  fMassHypo = Mass;
  SumDaughterMass = Mass;

  float m2 = Mass*Mass;            // measurement, weighted by Mass 
  float s2 = m2*SigmaMass*SigmaMass; // sigma^2

  float p2 = fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5]; 
  float e0 = sqrt(m2+p2);

  float mH[8];
  mH[0] = mH[1] = mH[2] = 0.;
  mH[3] = -2*fP[3]; 
  mH[4] = -2*fP[4]; 
  mH[5] = -2*fP[5]; 
  mH[6] =  2*fP[6];//e0;
  mH[7] = 0; 

  float zeta = e0*e0 - e0*fP[6];
  zeta = m2 - (fP[6]*fP[6]-p2);
  
  float mCHt[8], s2_est=0;
  for( Int_t i=0; i<8; ++i ){
    mCHt[i] = 0.0;
    for (Int_t j=0;j<8;++j) mCHt[i] += Cij(i,j)*mH[j];
    s2_est += mH[i]*mCHt[i];
  }
  
  if( s2_est<1.e-20 ) return; // calculated mass error is already 0, 
                              // the particle can not be constrained on mass

  float w2 = 1./( s2 + s2_est );
  fChi2 += zeta*zeta*w2;
  fNDF  += 1;
  for( Int_t i=0, ii=0; i<8; ++i ){
    float ki = mCHt[i]*w2;
    fP[i]+= ki*zeta;
    for(Int_t j=0;j<=i;++j) fC[ii++] -= ki*mCHt[j];    
  }
}


void KFParticleBase::SetNoDecayLength()
{  
  //* Set no decay length for resonances

  TransportToDecayVertex();

  float h[8];
  h[0] = h[1] = h[2] = h[3] = h[4] = h[5] = h[6] = 0;
  h[7] = 1; 

  float zeta = 0 - fP[7];
  for(Int_t i=0;i<8;++i) zeta -= h[i]*(fP[i]-fP[i]);
  
  float s = fC[35];   
  if( s>1.e-20 ){
    s = 1./s;
    fChi2 += zeta*zeta*s;
    fNDF  += 1;
    for( Int_t i=0, ii=0; i<7; ++i ){
      float ki = fC[28+i]*s;
      fP[i]+= ki*zeta;
      for(Int_t j=0;j<=i;++j) fC[ii++] -= ki*fC[28+j];    
    }
  }
  fP[7] = 0;
  fC[28] = fC[29] = fC[30] = fC[31] = fC[32] = fC[33] = fC[34] = fC[35] = 0;
}


void KFParticleBase::Construct( const KFParticleBase* vDaughters[], Int_t nDaughters,
				   const KFParticleBase *Parent,  float Mass )
{ 
  //* Full reconstruction in one go
  const int maxIter = 1;
  for( Int_t iter=0; iter<maxIter; iter++ ){
    fAtProductionVertex = 0;
    fSFromDecay = 0;
    SumDaughterMass = 0;

    for(Int_t i=0;i<36;++i) fC[i]=0.;
    fC[35] = 1.;
    
    fNDF  = -3;
    fChi2 =  0.;
    fQ = 0;
// std::cout << " nDaughters " << nDaughters << std::endl;
// int ui;
// std::cin >>ui;
//     nDaughters = nDaughters < 200 ? nDaughters : 200;
    
    for( Int_t itr =0; itr<nDaughters; itr++ ){
      AddDaughter( *vDaughters[itr] );    
    }
// std::cout << "Vse! "<<std::endl;
// std::cin>>ui;
    
  }

  if( Mass>=0 ) SetMassConstraint( Mass );
  if( Parent ) SetProductionVertex( *Parent );
}

void KFParticleBase::TransportToDecayVertex()
{
  //* Transport the particle to its decay vertex 
  float dsdr[6] = {0.f}; //TODO doesn't take into account errors of the previous extrapolation
  if( fSFromDecay != 0 ) TransportToDS( -fSFromDecay, dsdr );
  fAtProductionVertex = 0;
}

void KFParticleBase::TransportToProductionVertex()
{
  //* Transport the particle to its production vertex 
  float dsdr[6] = {0.f}; //TODO doesn't take into account errors of the previous extrapolation
  if( fSFromDecay != -fP[7] ) TransportToDS( -fSFromDecay-fP[7], dsdr );
  fAtProductionVertex = 1;
}


void KFParticleBase::TransportToDS( float dS, const float* dsdr )
{ 
  //* Transport the particle on dS parameter (SignedPath/Momentum) 
 
  Transport( dS, dsdr, fP, fC );
  fSFromDecay+= dS;
}


float KFParticleBase::GetDStoPointLine( const float xyz[3], float dsdr[6] ) const 
{
  //* Get dS to a certain space point without field

  float p2 = fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5];  
  if( p2<1.e-4 ) p2 = 1;
  
  const float& a = fP[3]*(xyz[0]-fP[0]) + fP[4]*(xyz[1]-fP[1]) + fP[5]*(xyz[2]-fP[2]);
  dsdr[0] = -fP[3]/p2;
  dsdr[1] = -fP[4]/p2;
  dsdr[2] = -fP[5]/p2;
  dsdr[3] = ((xyz[0]-fP[0])*p2 - 2.f* fP[3]*a)/(p2*p2);
  dsdr[4] = ((xyz[1]-fP[1])*p2 - 2.f* fP[4]*a)/(p2*p2);
  dsdr[5] = ((xyz[2]-fP[2])*p2 - 2.f* fP[5]*a)/(p2*p2);
  
  return a/p2;
}


float KFParticleBase::GetDStoPointBz( float B, const float xyz[3], float dsdr[6], const float* param) const
{ 
  
  if(!param)
    param = fP;
  //* Get dS to a certain space point for Bz field
  
  const float& x  = param[0];
  const float& y  = param[1];
  const float& z  = param[2];
  const float& px = param[3];
  const float& py = param[4];
  const float& pz = param[5];
  
  const float kCLight = 0.000299792458f;
  float bq = B*fQ*kCLight;
  float pt2 = px*px + py*py;
  float p2 = pt2 + pz*pz;  
  
  float dx = xyz[0] - x;
  float dy = xyz[1] - y; 
  float dz = xyz[2] - z; 
  float a = dx*px+dy*py;
  float dS(0.f);
  
  float abq = bq*a;

  const float LocalSmall = 1.e-8f;
  bool mask = ( fabs(bq)<LocalSmall );
  if(mask && p2>1.e-4f)
  {
    dS = (a + dz*pz)/p2;
    
    dsdr[0] = -px/p2;
    dsdr[1] = -py/p2;
    dsdr[2] = -pz/p2;
    dsdr[3] = (dx*p2 - 2.f* px *(a + dz *pz))/(p2*p2);
    dsdr[4] = (dy*p2 - 2.f* py *(a + dz *pz))/(p2*p2);
    dsdr[5] = (dz*p2 - 2.f* pz *(a + dz *pz))/(p2*p2);
  }
  if(mask)
  { 
    return dS;
  }
  
  dS = atan2( abq, pt2 + bq*(dy*px -dx*py) )/bq;

  float bs= bq*dS;

  float s = sin(bs), c = cos(bs);

  if(fabs(bq) < LocalSmall)
    bq = LocalSmall;
  float bbq = bq*(dx*py - dy*px) - pt2;
  
  dsdr[0] = (px*bbq - py*abq)/(abq*abq + bbq*bbq);
  dsdr[1] = (px*abq + py*bbq)/(abq*abq + bbq*bbq);
  dsdr[2] = 0;
  dsdr[3] = -(dx*bbq + dy*abq + 2.f*px*a)/(abq*abq + bbq*bbq);
  dsdr[4] = (dx*abq - dy*bbq - 2.f*py*a)/(abq*abq + bbq*bbq);
  dsdr[5] = 0;
  
  float sz(0.f);
  float cCoeff =  (bbq*c - abq*s) - pz*pz ;
  if(fabs(cCoeff) > 1.e-8f)
    sz = (dS*pz - dz)*pz / cCoeff;

  float dcdr[6] = {0.f};
  dcdr[0] = -bq*py*c - bbq*s*bq*dsdr[0] + px*bq*s - abq*c*bq*dsdr[0];
  dcdr[1] =  bq*px*c - bbq*s*bq*dsdr[1] + py*bq*s - abq*c*bq*dsdr[1];
  dcdr[3] = (-bq*dy-2*px)*c - bbq*s*bq*dsdr[3] - dx*bq*s - abq*c*bq*dsdr[3];
  dcdr[4] = ( bq*dx-2*py)*c - bbq*s*bq*dsdr[4] - dy*bq*s - abq*c*bq*dsdr[4];
  dcdr[5] = -2*pz;
  
  for(int iP=0; iP<6; iP++)
    dsdr[iP] += pz*pz/cCoeff*dsdr[iP] - sz/cCoeff*dcdr[iP];
  dsdr[2] += pz/cCoeff;
  dsdr[5] += (2.f*pz*dS - dz)/cCoeff;
  
  dS += sz;
  
  bs= bq*dS;
  s = sin(bs), c = cos(bs);
  
  float sB, cB;
  const float kOvSqr6 = 1.f/sqrt(float(6.f));

  if(LocalSmall < fabs(bs))
  {
    sB = s/bq;
    cB = (1.f-c)/bq;
  }
  else
  {
    sB = (1.f-bs*kOvSqr6)*(1.f+bs*kOvSqr6)*dS;
    cB = .5f*sB*bs;
  }

  float p[5];
  p[0] = x + sB*px + cB*py;
  p[1] = y - cB*px + sB*py;
  p[2] = z +  dS*pz;
  p[3] =          c*px + s*py;
  p[4] =         -s*px + c*py;

  dx = xyz[0] - p[0];
  dy = xyz[1] - p[1];
  dz = xyz[2] - p[2];
  a = dx*p[3]+dy*p[4] + dz*pz;
  abq = bq*a;

  dS += atan2( abq, p2 + bq*(dy*p[3] -dx*p[4]) )/bq;
  
  return dS;
}

float KFParticleBase::GetDStoPointBy( float By, const float xyz[3], float dsdr[6] ) const
{ 
  
  //* Get dS to a certain space point for By field

  const float param[6] = { fP[0], -fP[2], fP[1], fP[3], -fP[5], fP[4] };
  const float point[3] = { xyz[0], -xyz[2], xyz[1] };
  
  float dsdrBz[6] = {0.f};
  
  const float dS = GetDStoPointBz(By, point, dsdrBz, param);
  dsdr[0] =  dsdrBz[0];
  dsdr[1] =  dsdrBz[2];
  dsdr[2] = -dsdrBz[1];
  dsdr[3] =  dsdrBz[3];
  dsdr[4] =  dsdrBz[5];
  dsdr[5] = -dsdrBz[4];
  
  return dS;
}

float KFParticleBase::GetDStoPointB( const float* B, const float xyz[3], float dsdr[6] ) const
{ 
  //* Get dS to a certain space point for By field

  const float& Bx = B[0];
  const float& By = B[1];
  const float& Bz = B[2];
  
  const float& Bxz = sqrt(Bx*Bx + Bz*Bz);
  const float& Br = sqrt(Bx*Bx + By*By + Bz*Bz);
    
  float cosA = 1;
  float sinA = 0;
  if(fabs(Bxz) > 1.e-8f)
  {
    cosA = Bz/Bxz;
    sinA = Bx/Bxz;
  }
  
  const float& sinP = By/Br;
  const float& cosP = Bxz/Br;
  
  const float param[6] = { cosA*fP[0] - sinA*fP[2], 
                          -sinA*sinP*fP[0] + cosP*fP[1] - cosA*sinP*fP[2], 
                           cosP*sinA*fP[0] + sinP*fP[1] + cosA*cosP*fP[2],
                           cosA*fP[3] - sinA*fP[5], 
                          -sinA*sinP*fP[3] + cosP*fP[4] - cosA*sinP*fP[5], 
                           cosP*sinA*fP[3] + sinP*fP[4] + cosA*cosP*fP[5]};
  const float point[3] = { cosA*xyz[0] - sinA*xyz[2], 
                          -sinA*sinP*xyz[0] + cosP*xyz[1] - cosA*sinP*xyz[2], 
                           cosP*sinA*xyz[0] + sinP*xyz[1] + cosA*cosP*xyz[2] };
  
  float dsdrBz[6] = {0.f};
  
  const float dS = GetDStoPointBz(Br, point, dsdrBz, param);
  dsdr[0] =  dsdrBz[0]*cosA - dsdrBz[1]*sinA*sinP + dsdrBz[2]*sinA*cosP;
  dsdr[1] =                   dsdrBz[1]*cosP      + dsdrBz[2]*sinP;
  dsdr[2] = -dsdrBz[0]*sinA - dsdrBz[1]*cosA*sinP + dsdrBz[2]*cosA*cosP;
  dsdr[3] =  dsdrBz[3]*cosA - dsdrBz[4]*sinA*sinP + dsdrBz[5]*sinA*cosP;
  dsdr[4] =                   dsdrBz[4]*cosP      + dsdrBz[5]*sinP;
  dsdr[5] = -dsdrBz[3]*sinA - dsdrBz[4]*cosA*sinP + dsdrBz[5]*cosA*cosP;
  
  return dS;
}

float KFParticleBase::GetDStoPointCBM( const float xyz[3], float dsdr[3] ) const
{
  //* Transport the particle on dS, output to P[],C[], for CBM field

  float dS = 0;
  //Linear approximation

  float fld[3];
  GetFieldValue( fP, fld );
  
//   dS = GetDStoPointB( fld, xyz, dsdr);
  dS = GetDStoPointBy( fld[1], xyz, dsdr );
//   dS = GetDStoPointLine( xyz, dsdr );
// 
//   if( fQ==0 ){
//     return dS;
//   }
// 
//   dS = GetDStoPointBy( fld[1], xyz, dsdr );

  return dS;
}

void KFParticleBase::GetDStoParticleBz( float Bz, const KFParticleBase &p, float dS[2], float dsdr[4][6], const float* param1, const float* param2 )  const
{ 
  if(!param1)
  {
    param1 = fP;
    param2 = p.fP;
  }

  //* Get dS to another particle for Bz field
  const float kOvSqr6 = 1.f/sqrt(float(6.f));
  const float kCLight = 0.000299792458f;

  //in XY plane
  //first root    
  const float& bq1 = Bz*fQ*kCLight;
  const float& bq2 = Bz*p.fQ*kCLight;

  const bool& isStraight1 = fabs(bq1) < 1.e-8f;
  const bool& isStraight2 = fabs(bq2) < 1.e-8f;
  
  if( isStraight1 && isStraight2 )
  {
    GetDStoParticleLine(p, dS, dsdr);
    return;
  }
    
  const float& px1 = param1[3];
  const float& py1 = param1[4];
  const float& pz1 = param1[5];

  const float& px2 = param2[3];
  const float& py2 = param2[4];
  const float& pz2 = param2[5];

  const float& pt12 = px1*px1 + py1*py1;
  const float& pt22 = px2*px2 + py2*py2;

  const float& x01 = param1[0];
  const float& y01 = param1[1];
  const float& z01 = param1[2];

  const float& x02 = param2[0];
  const float& y02 = param2[1];
  const float& z02 = param2[2];

  float dS1[2] = {0.f}, dS2[2]={0.f};
  
  const float& dx0 = (x01 - x02);
  const float& dy0 = (y01 - y02);
  const float& dr02 = dx0*dx0 + dy0*dy0;
  const float& drp1  = dx0*px1 + dy0*py1;
  const float& dxyp1 = dx0*py1 - dy0*px1;
  const float& drp2  = dx0*px2 + dy0*py2;
  const float& dxyp2 = dx0*py2 - dy0*px2;
  const float& p1p2 = px1*px2 + py1*py2;
  const float& dp1p2 = px1*py2 - px2*py1;
  
  const float& k11 = (bq2*drp1 - dp1p2);
  const float& k21 = (bq1*(bq2*dxyp1 - p1p2) + bq2*pt12);
  const float& k12 = ((bq1*drp2 - dp1p2));
  const float& k22 = (bq2*(bq1*dxyp2 + p1p2) - bq1*pt22);
  
  const float& kp = (dxyp1*bq2 - dxyp2*bq1 - p1p2);
  const float& kd = dr02/2.f*bq1*bq2 + kp;
  const float& c1 = -(bq1*kd + pt12*bq2);
  const float& c2 = bq2*kd + pt22*bq1; 
  
  float d1 = pt12*pt22 - kd*kd;
  if(d1<0.f)
    d1 = float(0.f);
  d1 = sqrt( d1 );
  float d2 = pt12*pt22 - kd*kd;
  if(d2<0.f)
    d2 = float(0.f);
  d2 = sqrt( d2 );
  
  // find two points of closest approach in XY plane
  
  float dS1dR1[2][6];
  float dS2dR2[2][6];

  float dS1dR2[2][6];
  float dS2dR1[2][6];

  float dk11dr1[6] = {bq2*px1, bq2*py1, 0, bq2*dx0 - py2, bq2*dy0 + px2, 0};
  float dk11dr2[6] = {-bq2*px1, -bq2*py1, 0, py1, -px1, 0};
  float dk12dr1[6] = {bq1*px2, bq1*py2, 0, -py2, px2, 0};
  float dk12dr2[6] = {-bq1*px2, -bq1*py2, 0, bq1*dx0 + py1, bq1*dy0 - px1, 0};
  float dk21dr1[6] = {bq1*bq2*py1, -bq1*bq2*px1, 0, 2*bq2*px1 + bq1*(-(bq2*dy0) - px2), 2*bq2*py1 + bq1*(bq2*dx0 - py2), 0};
  float dk21dr2[6] = {-(bq1*bq2*py1), bq1*bq2*px1, 0, -(bq1*px1), -(bq1*py1), 0};
  float dk22dr1[6] = {bq1*bq2*py2, -(bq1*bq2*px2), 0, bq2*px2, bq2*py2, 0};
  float dk22dr2[6] = {-(bq1*bq2*py2), bq1*bq2*px2, 0, bq2*(-(bq1*dy0) + px1) - 2*bq1*px2, bq2*(bq1*dx0 + py1) - 2*bq1*py2, 0};
  
  float dkddr1[6] = {bq1*bq2*dx0 + bq2*py1 - bq1*py2, bq1*bq2*dy0 - bq2*px1 + bq1*px2, 0, -bq2*dy0 - px2, bq2*dx0 - py2, 0};
  float dkddr2[6] = {-bq1*bq2*dx0 - bq2*py1 + bq1*py2, -bq1*bq2*dy0 + bq2*px1 - bq1*px2, 0, bq1*dy0 - px1, -bq1*dx0 - py1, 0};
  
  float dc1dr1[6] = {-(bq1*(bq1*bq2*dx0 + bq2*py1 - bq1*py2)), -(bq1*(bq1*bq2*dy0 - bq2*px1 + bq1*px2)), 0, -2*bq2*px1 - bq1*(-(bq2*dy0) - px2), -2*bq2*py1 - bq1*(bq2*dx0 - py2), 0};
  float dc1dr2[6] = {-(bq1*(-(bq1*bq2*dx0) - bq2*py1 + bq1*py2)), -(bq1*(-(bq1*bq2*dy0) + bq2*px1 - bq1*px2)), 0, -(bq1*(bq1*dy0 - px1)), -(bq1*(-(bq1*dx0) - py1)), 0};
  
  float dc2dr1[6] = {bq2*(bq1*bq2*dx0 + bq2*py1 - bq1*py2), bq2*(bq1*bq2*dy0 - bq2*px1 + bq1*px2), 0, bq2*(-(bq2*dy0) - px2), bq2*(bq2*dx0 - py2), 0};
  float dc2dr2[6] = {bq2*(-(bq1*bq2*dx0) - bq2*py1 + bq1*py2), bq2*(-(bq1*bq2*dy0) + bq2*px1 - bq1*px2), 0, bq2*(bq1*dy0 - px1) + 2*bq1*px2, bq2*(-(bq1*dx0) - py1) + 2*bq1*py2, 0};
  
  float dd1dr1[6] = {0,0,0,0,0,0};
  float dd1dr2[6] = {0,0,0,0,0,0};
  if(d1>0)
  {
    for(int i=0; i<6; i++)
    {
      dd1dr1[i] = -kd/d1*dkddr1[i];
      dd1dr2[i] = -kd/d1*dkddr2[i];
    }
    dd1dr1[3] += px1/d1*pt22; dd1dr1[4] += py1/d1*pt22;
    dd1dr2[3] += px2/d1*pt12; dd1dr2[4] += py2/d1*pt12;
  }

  if(!isStraight1)
  {    
    dS1[0] = atan2( bq1*(k11*c1 + k21*d1), (bq1*k11*d1*bq1 - k21*c1) )/bq1;
    dS1[1] = atan2( bq1*(k11*c1 - k21*d1), (-bq1*k11*d1*bq1 - k21*c1) )/bq1;
    
    float a = bq1*(k11*c1 + k21*d1);
    float b = bq1*k11*d1*bq1 - k21*c1;
    for(int iP=0; iP<6; iP++)
    {
      if(( b*b + a*a ) > 0)
      {
        const float dadr1 = bq1*( dk11dr1[iP]*c1 + k11*dc1dr1[iP] + dk21dr1[iP]*d1 + k21*dd1dr1[iP] );
        const float dadr2 = bq1*( dk11dr2[iP]*c1 + k11*dc1dr2[iP] + dk21dr2[iP]*d1 + k21*dd1dr2[iP] );
        const float dbdr1 = bq1*bq1*( dk11dr1[iP]*d1 + k11*dd1dr1[iP] ) - ( dk21dr1[iP]*c1 + k21*dc1dr1[iP] );
        const float dbdr2 = bq1*bq1*( dk11dr2[iP]*d1 + k11*dd1dr2[iP] ) - ( dk21dr2[iP]*c1 + k21*dc1dr2[iP] );
      
        dS1dR1[0][iP] = 1/bq1 * 1/( b*b + a*a ) * ( dadr1*b - dbdr1*a );
        dS1dR2[0][iP] = 1/bq1 * 1/( b*b + a*a ) * ( dadr2*b - dbdr2*a );
      }
      else
      {
        dS1dR1[0][iP] = 0;
        dS1dR2[0][iP] = 0;
      }
    }
    
    a = bq1*(k11*c1 - k21*d1);
    b = -bq1*k11*d1*bq1 - k21*c1;
    for(int iP=0; iP<6; iP++)
    {
      if(( b*b + a*a ) > 0)
      {
        const float dadr1 = bq1*( dk11dr1[iP]*c1 + k11*dc1dr1[iP] - (dk21dr1[iP]*d1 + k21*dd1dr1[iP]) );
        const float dadr2 = bq1*( dk11dr2[iP]*c1 + k11*dc1dr2[iP] - (dk21dr2[iP]*d1 + k21*dd1dr2[iP]) );
        const float dbdr1 = -bq1*bq1*( dk11dr1[iP]*d1 + k11*dd1dr1[iP] ) - ( dk21dr1[iP]*c1 + k21*dc1dr1[iP] );
        const float dbdr2 = -bq1*bq1*( dk11dr2[iP]*d1 + k11*dd1dr2[iP] ) - ( dk21dr2[iP]*c1 + k21*dc1dr2[iP] );
      
        dS1dR1[1][iP] = 1/bq1 * 1/( b*b + a*a ) * ( dadr1*b - dbdr1*a );
        dS1dR2[1][iP] = 1/bq1 * 1/( b*b + a*a ) * ( dadr2*b - dbdr2*a );
      }
      else
      {
        dS1dR1[1][iP] = 0;
        dS1dR2[1][iP] = 0;
      }
    }
  }
  if(!isStraight2)
  {
    dS2[0] = atan2( (bq2*k12*c2 + k22*d2*bq2), (bq2*k12*d2*bq2 - k22*c2) )/bq2;
    dS2[1] = atan2( (bq2*k12*c2 - k22*d2*bq2), (-bq2*k12*d2*bq2 - k22*c2) )/bq2;
    
    float a = bq2*(k12*c2 + k22*d2);
    float b = bq2*k12*d2*bq2 - k22*c2;
    for(int iP=0; iP<6; iP++)
    {
      if(( b*b + a*a ) > 0)
      {
        const float dadr1 = bq2*( dk12dr1[iP]*c2 + k12*dc2dr1[iP] + dk22dr1[iP]*d1 + k22*dd1dr1[iP] );
        const float dadr2 = bq2*( dk12dr2[iP]*c2 + k12*dc2dr2[iP] + dk22dr2[iP]*d1 + k22*dd1dr2[iP] );
        const float dbdr1 = bq2*bq2*( dk12dr1[iP]*d1 + k12*dd1dr1[iP] ) - (dk22dr1[iP]*c2 + k22*dc2dr1[iP]);
        const float dbdr2 = bq2*bq2*( dk12dr2[iP]*d1 + k12*dd1dr2[iP] ) - (dk22dr2[iP]*c2 + k22*dc2dr2[iP]);
      
        dS2dR1[0][iP] = 1/bq2 * 1/( b*b + a*a ) * ( dadr1*b - dbdr1*a );
        dS2dR2[0][iP] = 1/bq2 * 1/( b*b + a*a ) * ( dadr2*b - dbdr2*a );
      }
      else
      {
        dS2dR1[0][iP] = 0;
        dS2dR2[0][iP] = 0;
      }
    }
    
    a = bq2*(k12*c2 - k22*d2);
    b = -bq2*k12*d2*bq2 - k22*c2;
    for(int iP=0; iP<6; iP++)
    {
      if(( b*b + a*a ) > 0)
      {
        const float dadr1 = bq2*( dk12dr1[iP]*c2 + k12*dc2dr1[iP] - (dk22dr1[iP]*d1 + k22*dd1dr1[iP]) );
        const float dadr2 = bq2*( dk12dr2[iP]*c2 + k12*dc2dr2[iP] - (dk22dr2[iP]*d1 + k22*dd1dr2[iP]) );
        const float dbdr1 = -bq2*bq2*( dk12dr1[iP]*d1 + k12*dd1dr1[iP] ) - (dk22dr1[iP]*c2 + k22*dc2dr1[iP]);
        const float dbdr2 = -bq2*bq2*( dk12dr2[iP]*d1 + k12*dd1dr2[iP] ) - (dk22dr2[iP]*c2 + k22*dc2dr2[iP]);
      
        dS2dR1[1][iP] = 1/bq2 * 1/( b*b + a*a ) * ( dadr1*b - dbdr1*a );
        dS2dR2[1][iP] = 1/bq2 * 1/( b*b + a*a ) * ( dadr2*b - dbdr2*a );
      }
      else
      {
        dS2dR1[1][iP] = 0;
        dS2dR2[1][iP] = 0;
      }
    }
  }
  if(isStraight1 && (pt12>0.f) )
  {
    dS1[0] = (k11*c1 + k21*d1)/(- k21*c1);
    dS1[1] = (k11*c1 - k21*d1)/(- k21*c1);
    
    float a = k11*c1 + k21*d1;
    float b = -k21*c1;
    
    for(int iP=0; iP<6; iP++)
    {
      if(b*b > 0)
      {
        const float dadr1 = ( dk11dr1[iP]*c1 + k11*dc1dr1[iP] + dk21dr1[iP]*d1 + k21*dd1dr1[iP] );
        const float dadr2 = ( dk11dr2[iP]*c1 + k11*dc1dr2[iP] + dk21dr2[iP]*d1 + k21*dd1dr2[iP] );
        const float dbdr1 = -( dk21dr1[iP]*c1 + k21*dc1dr1[iP] );
        const float dbdr2 = -( dk21dr2[iP]*c1 + k21*dc1dr2[iP] );
    
        dS1dR1[0][iP] = dadr1/b - dbdr1*a/(b*b) ;
        dS1dR2[0][iP] = dadr2/b - dbdr2*a/(b*b) ;
      }
      else
      {
        dS1dR1[0][iP] = 0;
        dS1dR2[0][iP] = 0;
      }
    }
    
    a = k11*c1 - k21*d1;
    for(int iP=0; iP<6; iP++)
    {
      if(b*b > 0)
      {
        const float dadr1 = ( dk11dr1[iP]*c1 + k11*dc1dr1[iP] - dk21dr1[iP]*d1 - k21*dd1dr1[iP] );
        const float dadr2 = ( dk11dr2[iP]*c1 + k11*dc1dr2[iP] - dk21dr2[iP]*d1 - k21*dd1dr2[iP] );
        const float dbdr1 = -( dk21dr1[iP]*c1 + k21*dc1dr1[iP] );
        const float dbdr2 = -( dk21dr2[iP]*c1 + k21*dc1dr2[iP] );
       
        dS1dR1[1][iP] = dadr1/b - dbdr1*a/(b*b) ;
        dS1dR2[1][iP] = dadr2/b - dbdr2*a/(b*b) ;
      }
      else
      {
        dS1dR1[1][iP] = 0;
        dS1dR2[1][iP] = 0;
      }
    }
  }
  if(isStraight2 && (pt22>0.f) )
  {
    dS2[0] = (k12*c2 + k22*d2)/(- k22*c2);
    dS2[1] = (k12*c2 - k22*d2)/(- k22*c2);  
    
    float a = k12*c2 + k22*d1;
    float b = -k22*c2;
    
    for(int iP=0; iP<6; iP++)
    {
      if(b*b > 0)
      {
        const float dadr1 = ( dk12dr1[iP]*c2 + k12*dc2dr1[iP] + dk22dr1[iP]*d1 + k22*dd1dr1[iP] );
        const float dadr2 = ( dk12dr2[iP]*c2 + k12*dc2dr2[iP] + dk22dr2[iP]*d1 + k22*dd1dr2[iP] );
        const float dbdr1 = -( dk22dr1[iP]*c2 + k22*dc2dr1[iP] );
        const float dbdr2 = -( dk22dr2[iP]*c2 + k22*dc2dr2[iP] );
    
        dS2dR1[0][iP] = dadr1/b - dbdr1*a/(b*b) ;
        dS2dR2[0][iP] = dadr2/b - dbdr2*a/(b*b) ;
      }
      else
      {
        dS2dR1[0][iP] = 0;
        dS2dR2[0][iP] = 0;
      }
    }
    
    a = k12*c2 - k22*d1;
    for(int iP=0; iP<6; iP++)
    {
      if(b*b > 0)
      {
        const float dadr1 = ( dk12dr1[iP]*c2 + k12*dc2dr1[iP] - dk22dr1[iP]*d1 - k22*dd1dr1[iP] );
        const float dadr2 = ( dk12dr2[iP]*c2 + k12*dc2dr2[iP] - dk22dr2[iP]*d1 - k22*dd1dr2[iP] );
        const float dbdr1 = -( dk22dr1[iP]*c2 + k22*dc2dr1[iP] );
        const float dbdr2 = -( dk22dr2[iP]*c2 + k22*dc2dr2[iP] );
    
        dS2dR1[1][iP] = dadr1/b - dbdr1*a/(b*b) ;
        dS2dR2[1][iP] = dadr2/b - dbdr2*a/(b*b) ;
      }
      else
      {
        dS2dR1[1][iP] = 0;
        dS2dR2[1][iP] = 0;
      }
    }
  }
  
  //select a point which is close to the primary vertex (with the smallest r)
  
  float dr2[2];
  for(int iP = 0; iP<2; iP++)
  {
    const float& bs1 = bq1*dS1[iP];
    const float& bs2 = bq2*dS2[iP];
    float sss = sin(bs1), ccc = cos(bs1);
    
    const bool& bs1Big = fabs(bs1) > 1.e-8f;
    const bool& bs2Big = fabs(bs2) > 1.e-8f;
    
    float sB(0.f), cB(0.f);
    if(bs1Big)
    {
      sB = sss/bq1;
      cB = (1.f-ccc)/bq1;
    }
    else
    {
      sB = ((1.f-bs1*kOvSqr6)*(1.f+bs1*kOvSqr6)*dS1[iP]);
      cB = .5f*sB*bs1;
    }
  
    const float& x1 = param1[0] + sB*px1 + cB*py1;
    const float& y1 = param1[1] - cB*px1 + sB*py1;
    const float& z1 = param1[2] + dS1[iP]*param1[5];

    sss = sin(bs2), ccc = cos(bs2);

    if(bs2Big)
    {
      sB = sss/bq2;
      cB = (1.f-ccc)/bq2;
    }
    else
    {
      sB = ((1.f-bs2*kOvSqr6)*(1.f+bs2*kOvSqr6)*dS2[iP]);
      cB = .5f*sB*bs2;
    }

    const float& x2 = param2[0] + sB*px2 + cB*py2;
    const float& y2 = param2[1] - cB*px2 + sB*py2;
    const float& z2 = param2[2] + dS2[iP]*param2[5];

    float dx = (x1-x2);
    float dy = (y1-y2);
    float dz = (z1-z2);
    
    dr2[iP] = dx*dx + dy*dy + dz*dz;
  }
  
  const bool isFirstRoot = dr2[0] < dr2[1];
  if(isFirstRoot)
  {
    dS[0]  = dS1[0];
    dS[1] = dS2[0];
    
    for(int iP=0; iP<6; iP++)
    {
      dsdr[0][iP] = dS1dR1[0][iP];
      dsdr[1][iP] = dS1dR2[0][iP];
      dsdr[2][iP] = dS2dR1[0][iP];
      dsdr[3][iP] = dS2dR2[0][iP];
    }
  }
  else
  {
    dS[0]  = dS1[1];
    dS[1] = dS2[1];
    
    for(int iP=0; iP<6; iP++)
    {
      dsdr[0][iP] = dS1dR1[1][iP];
      dsdr[1][iP] = dS1dR2[1][iP];
      dsdr[2][iP] = dS2dR1[1][iP];      
      dsdr[3][iP] = dS2dR2[1][iP];
    }    
  }
    
  //find correct parts of helices
  int n1(0);
  int n2(0);
  float dzMin = fabs( (z01-z02) + dS[0]*pz1 - dS[1]*pz2 );
  const float pi2(6.283185307f);
  
  //TODO optimise for loops for neutral particles
  const float& i1Float = -bq1/pi2*(z01/pz1+dS[0]);
  for(int di1=-1; di1<=1; di1++)
  {
    int i1(0);
    if(!isStraight1)
      i1 = int(i1Float) + di1;
    
    const float& i2Float = ( ((z01-z02) + (dS[0]+pi2*i1/bq1)*pz1)/pz2 - dS[1]) * bq2/pi2;
    for(int di2 = -1; di2<=1; di2++)
    {
      int i2(0);
      if(!isStraight2)
        i2 = int(i2Float) + di2;
      
      const float& z1 = z01 + (dS[0]+pi2*i1/bq1)*pz1;
      const float& z2 = z02 + (dS[1]+pi2*i2/bq2)*pz2;
      const float& dz = fabs( z1-z2 );
    
      if(dz < dzMin)
      {
        n1 = i1;
        n2 = i2;
        dzMin = dz;
      }
    }
  }

  if(!isStraight1)
    dS[0] += float(n1)*pi2/bq1;
  if(!isStraight2)
    dS[1] += float(n2)*pi2/bq2;

#if 0
  //z correction
  {
    const float& bs1 = bq1*dS[0];
    const float& bs2 = bq2*dS[1];
    
    float sss = sin(bs1), ccc = cos(bs1);
    const float& xr1 = sss*px1 - ccc*py1;
    const float& yr1 = ccc*px1 + sss*py1;

    float sss1 = sin(bs2), ccc1 = cos(bs2);
    const float& xr2 = sss1*px2 - ccc1*py2;
    const float& yr2 = ccc1*px2 + sss1*py2;
    
    const float& br = xr1*xr2 + yr1*yr2;
    const float& dx0mod = dx0*bq1*bq2 + py1*bq2 - py2*bq1;
    const float& dy0mod = dy0*bq1*bq2 - px1*bq2 + px2*bq1;
    const float& ar1 = dx0mod*xr1 + dy0mod*yr1;
    const float& ar2 = dx0mod*xr2 + dy0mod*yr2;
    const float& cz = (z01 - z02) + dS[0]*pz1 - dS[1]*pz2;
    
    const float& kz11 =  - ar1 + bq1*br + bq2*pz1*pz1;
    const float& kz12 =  -bq2*(br+pz1*pz2);
    const float& kz21 =   bq1*(br-pz1*pz2);
    const float& kz22 =  ar2 - bq2*br - bq1*pz2*pz2;
    
    const float& delta = kz11*kz22 - kz12*kz21;
    float sz1(0.f);
    float sz2(0.f);
    if( fabs(delta) > 1.e-16f )
    {
      const float aaa1 = -cz*(pz1*bq2*kz22 - pz2*bq1*kz12);
      const float aaa2 = -cz*(pz2*bq1*kz11 - pz1*bq2*kz21);
      sz1 = aaa1 / delta;
      sz2 = aaa2 / delta;
      
      const float dkz11dr1[6] = {-(bq1*bq2*xr1), -(bq1*bq2*yr1), 0, 
                                 -ccc*dy0mod - dx0mod*sss + bq2*yr1 + bq1*(sss*xr2 + ccc*yr2), 
                                  ccc*dx0mod - dy0mod*sss - bq2*xr1 + bq1*(-ccc*xr2 + sss*yr2), 2*bq2*pz1};
      const float dkz11dr2[6] = {bq1*bq2*xr1, bq1*bq2*yr1, 0, -bq1*yr1 + bq1*(sss1*xr1 + ccc1*yr1), bq1*xr1 + bq1*(-ccc1*xr1 + sss1*yr1), 0};
      
      const float dkz12dr1[6] = {0, 0, 0, -bq2*(sss*xr2 + ccc*yr2), -bq2*(-ccc*xr2 + sss*yr2), -bq2*pz2};
      const float dkz12dr2[6] = {0, 0, 0, -bq2*(sss1*xr1 + ccc1*yr1), -bq2*(-ccc1*xr1 + sss1*yr1), -bq2*pz1};
      
      const float dkz21dr1[6] = {0, 0, 0, bq1*(sss*xr2 + ccc*yr2), bq1*(-ccc*xr2 + sss*yr2), -bq1*pz2};
      const float dkz21dr2[6] = {0, 0, 0, bq1*(sss1*xr1 + ccc1*yr1), bq1*(-ccc1*xr1 + sss1*yr1), -bq1*pz1};
      
      const float dkz22dr1[6] = {bq1*bq2*xr2, bq1*bq2*yr2, 0, -bq2*yr2 - bq2*(sss*xr2 + ccc*yr2), bq2*xr2 - bq2*(-ccc*xr2 + sss*yr2), 0};
      const float dkz22dr2[6] = {-bq1*bq2*xr2, -bq1*bq2*yr2, 0, 
                                  ccc1*dy0mod + dx0mod*sss1 - bq2*(sss1*xr1 + ccc1*yr1) + bq1*yr2, 
                                 -ccc1*dx0mod + dy0mod*sss1 - bq1*xr2 - bq2*(-ccc1*xr1 + sss1*yr1), -2*bq1*pz2};
      
      const float dczdr1[6] = {0, 0, 1, 0, 0, dS[0]};
      const float dczdr2[6] = {0, 0, -1, 0, 0, -dS[1]};
      
      float daaa1dr1[6];
      float daaa1dr2[6];
      float daaa2dr2[6];
      float daaa2dr1[6];
      float dDeltadr1[6];
      float dDeltadr2[6];
      for(int iP=0; iP<6; iP++)
      {
        daaa1dr1[iP] = -( dczdr1[iP]*(pz1*bq2*kz22 - pz2*bq1*kz12) + cz*( bq2*pz1*dkz22dr1[iP] - bq1*pz2*dkz12dr1[iP] ) );
        daaa1dr2[iP] = -( dczdr2[iP]*(pz1*bq2*kz22 - pz2*bq1*kz12) + cz*( bq2*pz1*dkz22dr2[iP] - bq1*pz2*dkz12dr2[iP] ) );

        daaa2dr2[iP] = -( dczdr2[iP]*(pz2*bq1*kz11 - pz1*bq2*kz21) + cz*( bq1*pz2*dkz11dr2[iP] - bq2*pz1*dkz21dr2[iP] ) );
        daaa2dr1[iP] = -( dczdr1[iP]*(pz2*bq1*kz11 - pz1*bq2*kz21) + cz*( bq1*pz2*dkz11dr1[iP] - bq2*pz1*dkz21dr1[iP] ) );
        
        dDeltadr1[iP] = kz11*dkz22dr1[iP] + dkz11dr1[iP]*kz11 - kz12*dkz21dr1[iP] - dkz12dr1[iP]*kz21;
        dDeltadr2[iP] = kz11*dkz22dr2[iP] + dkz11dr2[iP]*kz11 - kz12*dkz21dr2[iP] - dkz12dr2[iP]*kz21;
      }
      daaa1dr1[5] -= cz*bq2*kz22;
      daaa1dr2[5] += cz*bq1*kz12;
      daaa2dr2[5] -= cz*bq1*kz11;
      daaa2dr1[5] += cz*bq2*kz21;
      
      //derivatives by s0 and s1
      const float dkz11ds0 = bq1*(dy0mod*xr1 - dx0mod*yr1 +  bq1*(xr2*yr1 - xr1*yr2));
      const float dkz11ds1 = bq1*bq2*( xr1*yr2 - xr2*yr1 );
      const float dkz12ds0 = bq2*bq1*( xr1*yr2 - xr2*yr1 );
      const float dkz12ds1 = bq2*bq2*( xr2*yr1 - xr1*yr2 );
      const float dkz21ds0 = bq1*bq1*( xr2*yr1 - xr1*yr2 );
      const float dkz21ds1 = bq1*bq2*( xr1*yr2 - xr2*yr1 );
      const float dkz22ds0 = bq1*bq2*( xr1*yr2 - xr2*yr1 );      
      const float dkz22ds1 = -bq2*( dy0mod*xr2 - dx0mod*yr2 - bq2*(xr2*yr1 - xr1*yr2) );
      const float dczds0   = pz1;
      const float dczds1   = -pz2;
      const float da1ds0   = -( dczds0*(pz1*bq2*kz22 - pz2*bq1*kz12) + cz*(pz1*bq2*dkz22ds0 - pz2*bq1*dkz12ds0));
      const float da1ds1   = -( dczds1*(pz1*bq2*kz22 - pz2*bq1*kz12) + cz*(pz1*bq2*dkz22ds1 - pz2*bq1*dkz12ds1));
      const float da2ds0   = -( dczds0*(pz2*bq1*kz11 - pz1*bq2*kz21) + cz*(pz2*bq1*dkz11ds0 - pz1*bq2*dkz21ds0));
      const float da2ds1   = -( dczds1*(pz2*bq1*kz11 - pz1*bq2*kz21) + cz*(pz2*bq1*dkz11ds1 - pz1*bq2*dkz21ds1));
      const float dDeltads0 = kz11*dkz22ds0 + dkz11ds0*kz11 - kz12*dkz21ds0 - dkz12ds0*kz21;
      const float dDeltads1 = kz11*dkz22ds1 + dkz11ds1*kz11 - kz12*dkz21ds1 - dkz12ds1*kz21;
      
      const float dsz1ds0 = da1ds0/delta - aaa1*dDeltads0/(delta*delta);
      const float dsz1ds1 = da1ds1/delta - aaa1*dDeltads1/(delta*delta);
      const float dsz2ds0 = da2ds0/delta - aaa2*dDeltads0/(delta*delta);
      const float dsz2ds1 = da2ds1/delta - aaa2*dDeltads1/(delta*delta);
      
      float dszdr[4][6];
      for(int iP=0; iP<6; iP++)
      {
        dszdr[0][iP] = dsz1ds0*dsdr[0][iP] + dsz1ds1*dsdr[2][iP];
        dszdr[1][iP] = dsz1ds0*dsdr[1][iP] + dsz1ds1*dsdr[3][iP];
        dszdr[2][iP] = dsz2ds0*dsdr[0][iP] + dsz2ds1*dsdr[2][iP];
        dszdr[3][iP] = dsz2ds0*dsdr[1][iP] + dsz2ds1*dsdr[3][iP];
      }
      
      for(int iP=0; iP<6; iP++)
      {
        dsdr[0][iP] += daaa1dr1[iP]/delta - aaa1*dDeltadr1[iP]/(delta*delta) + dszdr[0][iP];
        dsdr[1][iP] += daaa1dr2[iP]/delta - aaa1*dDeltadr2[iP]/(delta*delta) + dszdr[1][iP];
        dsdr[2][iP] += daaa2dr1[iP]/delta - aaa2*dDeltadr1[iP]/(delta*delta) + dszdr[2][iP];
        dsdr[3][iP] += daaa2dr2[iP]/delta - aaa2*dDeltadr2[iP]/(delta*delta) + dszdr[3][iP];
      }
    }
    
    dS[0]  += sz1;
    dS[1] += sz2;
  }
#endif
  //Line correction
  {
    const float& bs1 = bq1*dS[0];
    const float& bs2 = bq2*dS[1];
    float sss = sin(bs1), ccc = cos(bs1);
    
    const bool& bs1Big = fabs(bs1) > 1.e-8f;
    const bool& bs2Big = fabs(bs2) > 1.e-8f;
    
    float sB(0.f), cB(0.f);
    if(bs1Big)
    {
      sB = sss/bq1;
      cB = (1.f-ccc)/bq1;
    }
    else
    {
      sB = ((1.f-bs1*kOvSqr6)*(1.f+bs1*kOvSqr6)*dS[0]);
      cB = .5f*sB*bs1;
    }
  
    const float& x1 = x01 + sB*px1 + cB*py1;
    const float& y1 = y01 - cB*px1 + sB*py1;
    const float& z1 = z01 + dS[0]*pz1;
    const float& ppx1 =  ccc*px1 + sss*py1;
    const float& ppy1 = -sss*px1 + ccc*py1;
    const float& ppz1 = pz1;
    
    float sss1 = sin(bs2), ccc1 = cos(bs2);

    float sB1(0.f), cB1(0.f);
    if(bs2Big)
    {
      sB1 = sss1/bq2;
      cB1 = (1.f-ccc1)/bq2;
    }
    else
    {
      sB1 = ((1.f-bs2*kOvSqr6)*(1.f+bs2*kOvSqr6)*dS[1]);
      cB1 = .5f*sB1*bs2;
    }

    const float& x2 = x02 + sB1*px2 + cB1*py2;
    const float& y2 = y02 - cB1*px2 + sB1*py2;
    const float& z2 = z02 + dS[1]*pz2;
    const float& ppx2 =  ccc1*px2 + sss1*py2;
    const float& ppy2 = -sss1*px2 + ccc1*py2;    
    const float& ppz2 = pz2;

    const float& p12  = ppx1*ppx1 + ppy1*ppy1 + ppz1*ppz1;
    const float& p22  = ppx2*ppx2 + ppy2*ppy2 + ppz2*ppz2;
    const float& lp1p2 = ppx1*ppx2 + ppy1*ppy2 + ppz1*ppz2;

    const float& dx = (x2 - x1);
    const float& dy = (y2 - y1);
    const float& dz = (z2 - z1);
    
    const float& ldrp1 = ppx1*dx + ppy1*dy + ppz1*dz;
    const float& ldrp2 = ppx2*dx + ppy2*dy + ppz2*dz;

    float detp =  lp1p2*lp1p2 - p12*p22;
    if( fabs(detp)<1.e-4 ) detp = 1; //TODO correct!!!
    
    //dsdr calculation
    const float a1 = ldrp2*lp1p2 - ldrp1*p22;
    const float a2 = ldrp2*p12 - ldrp1*lp1p2;
    const float lp1p2_ds0 = bq1*( ppx2*ppy1 - ppy2*ppx1);
    const float lp1p2_ds1 = bq2*( ppx1*ppy2 - ppy1*ppx2);
    const float ldrp1_ds0 = -p12 + bq1*(ppy1*dx - ppx1*dy);
    const float ldrp1_ds1 =  lp1p2;
    const float ldrp2_ds0 = -lp1p2;
    const float ldrp2_ds1 =  p22 + bq2*(ppy2*dx - ppx2*dy);
    const float detp_ds0 = 2*lp1p2*lp1p2_ds0;
    const float detp_ds1 = 2*lp1p2*lp1p2_ds1;
    const float a1_ds0 = ldrp2_ds0*lp1p2 + ldrp2*lp1p2_ds0 - ldrp1_ds0*p22;
    const float a1_ds1 = ldrp2_ds1*lp1p2 + ldrp2*lp1p2_ds1 - ldrp1_ds1*p22;
    const float a2_ds0 = ldrp2_ds0*p12 - ldrp1_ds0*lp1p2 - ldrp1*lp1p2_ds0;
    const float a2_ds1 = ldrp2_ds1*p12 - ldrp1_ds1*lp1p2 - ldrp1*lp1p2_ds1;
    
    const float dsl1ds0 = a1_ds0/detp - a1*detp_ds0/(detp*detp);
    const float dsl1ds1 = a1_ds1/detp - a1*detp_ds1/(detp*detp);
    const float dsl2ds0 = a2_ds0/detp - a2*detp_ds0/(detp*detp);
    const float dsl2ds1 = a2_ds1/detp - a2*detp_ds1/(detp*detp);
    
    float dsldr[4][6];
    for(int iP=0; iP<6; iP++)
    {
      dsldr[0][iP] = dsl1ds0*dsdr[0][iP] + dsl1ds1*dsdr[2][iP];
      dsldr[1][iP] = dsl1ds0*dsdr[1][iP] + dsl1ds1*dsdr[3][iP];
      dsldr[2][iP] = dsl2ds0*dsdr[0][iP] + dsl2ds1*dsdr[2][iP];
      dsldr[3][iP] = dsl2ds0*dsdr[1][iP] + dsl2ds1*dsdr[3][iP];
    }
    
    for(int iDS=0; iDS<4; iDS++)
      for(int iP=0; iP<6; iP++)
        dsdr[iDS][iP] += dsldr[iDS][iP];
      
    const float lp1p2_dr0[6] = {0, 0, 0, ccc*ppx2 - ppy2*sss, ccc*ppy2 + ppx2*sss, pz2};
    const float lp1p2_dr1[6] = {0, 0, 0, ccc1*ppx1 - ppy1*sss1, ccc1*ppy1 + ppx1*sss1, pz1};
    const float ldrp1_dr0[6] = {-ppx1, -ppy1, -pz1,  cB*ppy1 - ppx1*sB + ccc*dx - sss*dy, -cB*ppx1-ppy1*sB + sss*dx + ccc*dy, -dS[0]*pz1 + dz};
    const float ldrp1_dr1[6] = { ppx1,  ppy1,  pz1, -cB1*ppy1 + ppx1*sB1, cB1*ppx1 + ppy1*sB1, dS[1]*pz1};
    const float ldrp2_dr0[6] = {-ppx2, -ppy2, -pz2, cB*ppy2 - ppx2*sB, -cB*ppx2-ppy2*sB, -dS[0]*pz2};
    const float ldrp2_dr1[6] = {ppx2, ppy2, pz2, -cB1*ppy2 + ppx2*sB1 + ccc1*dx- sss1*dy, cB1*ppx2 + ppy2*sB1 + sss1*dx + ccc1*dy, dz + dS[1]*pz2};
    const float p12_dr0[6] = {0, 0, 0, 2*px1, 2*py1, 2*pz1};
    const float p22_dr1[6] = {0, 0, 0, 2*px2, 2*py2, 2*pz2};
    float a1_dr0[6], a1_dr1[6], a2_dr0[6], a2_dr1[6], detp_dr0[6], detp_dr1[6];
    for(int iP=0; iP<6; iP++)
    {
      a1_dr0[iP] = ldrp2_dr0[iP]*lp1p2 + ldrp2*lp1p2_dr0[iP] - ldrp1_dr0[iP]*p22;
      a1_dr1[iP] = ldrp2_dr1[iP]*lp1p2 + ldrp2*lp1p2_dr1[iP] - ldrp1_dr1[iP]*p22 - ldrp1*p22_dr1[iP];
      a2_dr0[iP] = ldrp2_dr0[iP]*p12 + ldrp2*p12_dr0[iP] - ldrp1_dr0[iP]*lp1p2 - ldrp1*lp1p2_dr0[iP];
      a2_dr1[iP] = ldrp2_dr1[iP]*p12 - ldrp1_dr1[iP]*lp1p2 - ldrp1*lp1p2_dr1[iP];
      detp_dr0[iP] = 2*lp1p2*lp1p2_dr0[iP] - p12_dr0[iP]*p22;
      detp_dr1[iP] = 2*lp1p2*lp1p2_dr1[iP] - p12*p22_dr1[iP];
      
      dsdr[0][iP] += a1_dr0[iP]/detp - a1*detp_dr0[iP]/(detp*detp);
      dsdr[1][iP] += a1_dr1[iP]/detp - a1*detp_dr1[iP]/(detp*detp);
      dsdr[2][iP] += a2_dr0[iP]/detp - a2*detp_dr0[iP]/(detp*detp);
      dsdr[3][iP] += a2_dr1[iP]/detp - a2*detp_dr1[iP]/(detp*detp);
    }
    
    dS[0] += (ldrp2*lp1p2 - ldrp1*p22) /detp;
    dS[1] += (ldrp2*p12 - ldrp1*lp1p2)/detp;    
  }  
}

void KFParticleBase::GetDStoParticleBy( float B,  const KFParticleBase &p, float dS[2], float dsdr[4][6] ) const
{ 
  const float param1[6] = { fP[0], -fP[2], fP[1], fP[3], -fP[5], fP[4] };
  const float param2[6] = { p.fP[0], -p.fP[2], p.fP[1], p.fP[3], -p.fP[5], p.fP[4] };
  
  float dsdrBz[4][6];
  for(int i1=0; i1<4; i1++)
    for(int i2=0; i2<6; i2++)
      dsdrBz[i1][i2] = 0;
  
  GetDStoParticleBz(B, p, dS, dsdrBz, param1, param2);
  
  for(int iDs=0; iDs<4; iDs++)
  {
    dsdr[iDs][0] =  dsdrBz[iDs][0];
    dsdr[iDs][1] =  dsdrBz[iDs][2];
    dsdr[iDs][2] = -dsdrBz[iDs][1];
    dsdr[iDs][3] =  dsdrBz[iDs][3];
    dsdr[iDs][4] =  dsdrBz[iDs][5];
    dsdr[iDs][5] = -dsdrBz[iDs][4];
  }
}

void KFParticleBase::GetDStoParticleLine( const KFParticleBase &p, float dS[2], float dsdr[4][6] ) const
{
  float p12 = fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5];
  float p22 = p.fP[3]*p.fP[3] + p.fP[4]*p.fP[4] + p.fP[5]*p.fP[5];
  float p1p2 = fP[3]*p.fP[3] + fP[4]*p.fP[4] + fP[5]*p.fP[5];

  float drp1 = fP[3]*(p.fP[0]-fP[0]) + fP[4]*(p.fP[1]-fP[1]) + fP[5]*(p.fP[2]-fP[2]);
  float drp2 = p.fP[3]*(p.fP[0]-fP[0]) + p.fP[4]*(p.fP[1]-fP[1]) + p.fP[5]*(p.fP[2]-fP[2]);

  float detp =  p1p2*p1p2 - p12*p22;
  if( fabs(detp)<1.e-4 ) detp = 1; //TODO correct!!!

  dS[0]  = (drp2*p1p2 - drp1*p22) /detp;
  dS[1] = (drp2*p12  - drp1*p1p2)/detp;
  
  const float x01 = fP[0];
  const float y01 = fP[1];
  const float z01 = fP[2];
  const float px1 = fP[3];
  const float py1 = fP[4];
  const float pz1 = fP[5];

  const float x02 = p.fP[0];
  const float y02 = p.fP[1];
  const float z02 = p.fP[2];
  const float px2 = p.fP[3];
  const float py2 = p.fP[4];
  const float pz2 = p.fP[5];
  
  const float drp1_dr1[6] = {-px1, -py1, -pz1, -x01 + x02, -y01 + y02, -z01 + z02};
  const float drp1_dr2[6] = {px1, py1, pz1, 0, 0, 0};
  const float drp2_dr1[6] = {-px2, -py2, -pz2, 0, 0, 0};
  const float drp2_dr2[6] = {px2, py2, pz2, -x01 + x02, -y01 + y02, -z01 + z02};
  const float dp1p2_dr1[6] = {0, 0, 0, px2, py2, pz2};
  const float dp1p2_dr2[6] = {0, 0, 0, px1, py1, pz1};
  const float dp12_dr1[6] = {0, 0, 0, 2*px1, 2*py1, 2*pz1};
  const float dp12_dr2[6] = {0, 0, 0, 0, 0, 0};
  const float dp22_dr1[6] = {0, 0, 0, 0, 0, 0};
  const float dp22_dr2[6] = {0, 0, 0, 2*px2, 2*py2, 2*pz2};
  const float ddetp_dr1[6] = {0, 0, 0, -2*p22*px1 + 2*p1p2*px2, -2*p22*py1 + 2*p1p2*py2, -2*p22*pz1 + 2*p1p2*pz2};
  const float ddetp_dr2[6] = {0, 0, 0, 2*p1p2*px1 - 2*p12*px2,   2*p1p2*py1 - 2*p12*py2, 2*p1p2*pz1 - 2*p12*pz2};
  
  
  float da1_dr1[6], da1_dr2[6], da2_dr1[6], da2_dr2[6];
  
  const float a1 = drp2*p1p2 - drp1*p22;
  const float a2 = drp2*p12  - drp1*p1p2;
  for(int i=0; i<6; i++)
  {
    da1_dr1[i] = drp2_dr1[i]*p1p2 + drp2*dp1p2_dr1[i] - drp1_dr1[i]*p22 - drp1*dp22_dr1[i];
    da1_dr2[i] = drp2_dr2[i]*p1p2 + drp2*dp1p2_dr2[i] - drp1_dr2[i]*p22 - drp1*dp22_dr2[i];
    
    da2_dr1[i] = drp2_dr1[i]*p12 + drp2*dp12_dr1[i] - drp1_dr1[i]*p1p2 - drp1*dp1p2_dr1[i];
    da2_dr2[i] = drp2_dr2[i]*p12 + drp2*dp12_dr2[i] - drp1_dr2[i]*p1p2 - drp1*dp1p2_dr2[i];
    
    dsdr[0][i] = da1_dr1[i]/detp - a1/(detp*detp)*ddetp_dr1[i];
    dsdr[1][i] = da1_dr2[i]/detp - a1/(detp*detp)*ddetp_dr2[i];
    
    dsdr[2][i] = da2_dr1[i]/detp - a2/(detp*detp)*ddetp_dr1[i];
    dsdr[3][i] = da2_dr2[i]/detp - a2/(detp*detp)*ddetp_dr2[i];
  }
}

void KFParticleBase::GetDStoParticleCBM( const KFParticleBase &p, float dS[2], float dsdr[4][6] ) const
{
  //* Transport the particle on dS, output to P[],C[], for CBM field

  float fld[3];
  GetFieldValue( fP, fld );
  
  const float& bq1 = fld[1]*fQ;
  const float& bq2 = fld[1]*p.fQ;
  const bool& isStraight1 = fabs(bq1) < 1.e-8f;
  const bool& isStraight2 = fabs(bq2) < 1.e-8f;
  
  if( isStraight1 && isStraight2 )
    GetDStoParticleLine(p, dS, dsdr);
  else
    GetDStoParticleBy(fld[1], p, dS, dsdr);
}

void KFParticleBase::TransportCBM( float dS, const float* dsdr, float P[], float C[], float* dsdr1, float* F, float* F1) const
{  
  //* Transport the particle on dS, output to P[],C[], for CBM field
 
  if( fQ==0 ){
    TransportLine( dS, dsdr, P, C, dsdr1, F, F1 );
    return;
  }

  if( fabs(dS*fP[5]) > 1000.f ) dS = 0;
  
  const float kCLight = 0.000299792458;

  float c = fQ*kCLight;

  // construct coefficients 

  float 
    px   = fP[3],
    py   = fP[4],
    pz   = fP[5];
      
  float sx=0, sy=0, sz=0, syy=0, syz=0, syyy=0, ssx=0, ssy=0, ssz=0, ssyy=0, ssyz=0, ssyyy=0;

  { // get field integrals

    float fld[3][3];   
    float p0[3], p1[3], p2[3];

    // line track approximation

    p0[0] = fP[0];
    p0[1] = fP[1];
    p0[2] = fP[2];
  
    p2[0] = fP[0] + px*dS;
    p2[1] = fP[1] + py*dS;
    p2[2] = fP[2] + pz*dS;
  
    p1[0] = 0.5*(p0[0]+p2[0]);
    p1[1] = 0.5*(p0[1]+p2[1]);
    p1[2] = 0.5*(p0[2]+p2[2]);

    // first order track approximation
    {
      GetFieldValue( p0, fld[0] );
      GetFieldValue( p1, fld[1] );
      GetFieldValue( p2, fld[2] );

      float ssy1 = ( 7*fld[0][1] + 6*fld[1][1]-fld[2][1] )*c*dS*dS/96.;
      float ssy2 = (   fld[0][1] + 2*fld[1][1]         )*c*dS*dS/6.;

      p1[0] -= ssy1*pz;
      p1[2] += ssy1*px;
      p2[0] -= ssy2*pz;
      p2[2] += ssy2*px;   
    }

    GetFieldValue( p0, fld[0] );
    GetFieldValue( p1, fld[1] );
    GetFieldValue( p2, fld[2] );

    sx = c*( fld[0][0] + 4*fld[1][0] + fld[2][0] )*dS/6.;
    sy = c*( fld[0][1] + 4*fld[1][1] + fld[2][1] )*dS/6.;
    sz = c*( fld[0][2] + 4*fld[1][2] + fld[2][2] )*dS/6.;

    ssx = c*( fld[0][0] + 2*fld[1][0])*dS*dS/6.;
    ssy = c*( fld[0][1] + 2*fld[1][1])*dS*dS/6.;
    ssz = c*( fld[0][2] + 2*fld[1][2])*dS*dS/6.;

    float c2[3][3]    =   { {  5, -4, -1},{  44,  80,  -4},{ 11, 44, 5} }; // /=360.    
    float cc2[3][3]    =   { { 38,  8, -4},{ 148, 208, -20},{  3, 36, 3} }; // /=2520.
    for(Int_t n=0; n<3; n++)
      for(Int_t m=0; m<3; m++) 
	{
	  syz += c2[n][m]*fld[n][1]*fld[m][2];
	  ssyz += cc2[n][m]*fld[n][1]*fld[m][2];
	}
 
    syz  *= c*c*dS*dS/360.;
    ssyz  *= c*c*dS*dS*dS/2520.;
    
    syy  = c*( fld[0][1] + 4*fld[1][1] + fld[2][1] )*dS;
    syyy = syy*syy*syy / 1296;
    syy  = syy*syy/72;

    ssyy = ( fld[0][1]*( 38*fld[0][1] + 156*fld[1][1]  -   fld[2][1] )+
	    fld[1][1]*(              208*fld[1][1]  +16*fld[2][1] )+
	    fld[2][1]*(                             3*fld[2][1] )  
	    )*dS*dS*dS*c*c/2520.;
    ssyyy = 
      (
       fld[0][1]*( fld[0][1]*( 85*fld[0][1] + 526*fld[1][1]  - 7*fld[2][1] )+
		 fld[1][1]*(             1376*fld[1][1]  +84*fld[2][1] )+
		 fld[2][1]*(                            19*fld[2][1] )  )+
       fld[1][1]*( fld[1][1]*(             1376*fld[1][1] +256*fld[2][1] )+
		 fld[2][1]*(                            62*fld[2][1] )  )+
       fld[2][1]*fld[2][1]  *(                             3*fld[2][1] )       
       )*dS*dS*dS*dS*c*c*c/90720.;    
 
  }

  float mJ[8][8];
  for( Int_t i=0; i<8; i++ ) for( Int_t j=0; j<8; j++) mJ[i][j]=0;

  mJ[0][0]=1; mJ[0][1]=0; mJ[0][2]=0; mJ[0][3]=dS-ssyy;  mJ[0][4]=ssx;  mJ[0][5]=ssyyy-ssy;
  mJ[1][0]=0; mJ[1][1]=1; mJ[1][2]=0; mJ[1][3]=-ssz;     mJ[1][4]=dS;  mJ[1][5]=ssx+ssyz;
  mJ[2][0]=0; mJ[2][1]=0; mJ[2][2]=1; mJ[2][3]=ssy-ssyyy; mJ[2][4]=-ssx; mJ[2][5]=dS-ssyy;
  
  mJ[3][0]=0; mJ[3][1]=0; mJ[3][2]=0; mJ[3][3]=1-syy;   mJ[3][4]=sx;  mJ[3][5]=syyy-sy;
  mJ[4][0]=0; mJ[4][1]=0; mJ[4][2]=0; mJ[4][3]=-sz;     mJ[4][4]=1;   mJ[4][5]=sx+syz;
  mJ[5][0]=0; mJ[5][1]=0; mJ[5][2]=0; mJ[5][3]=sy-syyy; mJ[5][4]=-sx; mJ[5][5]=1-syy;
  mJ[6][6] = mJ[7][7] = 1;
  
  P[0] = fP[0] + mJ[0][3]*px + mJ[0][4]*py + mJ[0][5]*pz;
  P[1] = fP[1] + mJ[1][3]*px + mJ[1][4]*py + mJ[1][5]*pz;
  P[2] = fP[2] + mJ[2][3]*px + mJ[2][4]*py + mJ[2][5]*pz;
  P[3] =         mJ[3][3]*px + mJ[3][4]*py + mJ[3][5]*pz;
  P[4] =         mJ[4][3]*px + mJ[4][4]*py + mJ[4][5]*pz;
  P[5] =         mJ[5][3]*px + mJ[5][4]*py + mJ[5][5]*pz;
  P[6] = fP[6];
  P[7] = fP[7];

  float mJds[6][6];
  for( Int_t i=0; i<6; i++ ) for( Int_t j=0; j<6; j++) mJds[i][j]=0;

  if(fabs(dS)>0)
  {
    mJds[0][3]= 1 - 3*ssyy/dS;          mJds[0][4]= 2*ssx/dS; mJds[0][5]= (4.f*ssyyy-2*ssy)/dS;
    mJds[1][3]= -2.f*ssz/dS;            mJds[1][4]= 1;        mJds[1][5]= (2.f*ssx + 3.*ssyz)/dS;
    mJds[2][3]= (2.f*ssy-4.f*ssyyy)/dS; mJds[2][4]=-2*ssx/dS; mJds[2][5]= 1 - 3.f*ssyy/dS;
  
    mJds[3][3]= -2.f*syy/dS;            mJds[3][4]= sx/dS;    mJds[3][5]= 3.f*syyy/dS - sy/dS;
    mJds[4][3]= -sz/dS;                 mJds[4][4]=0;         mJds[4][5] = sx/dS + 2.f*syz/dS;
    mJds[5][3]= sy/dS - 3.f*syyy/dS;    mJds[5][4]=-sx/dS;    mJds[5][5]= -2.f*syy/dS;
  }
  
  for(int i1=0; i1<6; i1++)
    for(int i2=0; i2<6; i2++)
      mJ[i1][i2] += mJds[i1][3]*px*dsdr[i2] + mJds[i1][4]*py*dsdr[i2] + mJds[i1][5]*pz*dsdr[i2];
  
  MultQSQt( mJ[0], fC, C, 8);
  
  if(F)
  {
    for(int i=0; i<6; i++)
      for(int j=0; j<6; j++)
        F[i*6+j] = mJ[i][j];

    for(int i1=0; i1<6; i1++)
      for(int i2=0; i2<6; i2++)
        F1[i1*6 + i2] = mJds[i1][3]*px*dsdr1[i2] + mJds[i1][4]*py*dsdr1[i2] + mJds[i1][5]*pz*dsdr1[i2];
  }
}


void KFParticleBase::TransportBz( float b, float t, const float* dsdr, float p[], float e[], float* dsdr1, float* F, float* F1 ) const 
{ 
  //* Transport the particle on dS, output to P[],C[], for Bz field
 
  const float kCLight = 0.000299792458;
  b = b*fQ*kCLight;
  float bs= b*t;
  float s = sin(bs), c = cos(bs);
  float sB, cB;
  if( fabs(bs)>1.e-10){
    sB= s/b;
    cB= (1-c)/b;
  }else{
    const float kOvSqr6 = 1./sqrt(6.);
    sB = (1.-bs*kOvSqr6)*(1.+bs*kOvSqr6)*t;
    cB = .5*sB*bs;
  }
  
  float px = fP[3];
  float py = fP[4];
  float pz = fP[5];

  p[0] = fP[0] + sB*px + cB*py;
  p[1] = fP[1] - cB*px + sB*py;
  p[2] = fP[2] +  t*pz;
  p[3] =          c*px + s*py;
  p[4] =         -s*px + c*py;
  p[5] = fP[5];
  p[6] = fP[6];
  p[7] = fP[7];

  /* 
  float mJ[8][8] = { {1,0,0,   sB, cB,  0, 0, 0 },
			{0,1,0,  -cB, sB,  0, 0, 0 },
			{0,0,1,    0,  0,  t, 0, 0 },
			{0,0,0,    c,  s,  0, 0, 0 },
			{0,0,0,   -s,  c,  0, 0, 0 },
			{0,0,0,    0,  0,  1, 0, 0 },
			{0,0,0,    0,  0,  0, 1, 0 },
			{0,0,0,    0,  0,  0, 0, 1 }  };
  float mA[8][8];
  for( Int_t k=0,i=0; i<8; i++)
    for( Int_t j=0; j<=i; j++, k++ ) mA[i][j] = mA[j][i] = fC[k]; 

  float mJC[8][8];
  for( Int_t i=0; i<8; i++ )
    for( Int_t j=0; j<8; j++ ){
      mJC[i][j]=0;
      for( Int_t k=0; k<8; k++ ) mJC[i][j]+=mJ[i][k]*mA[k][j];
    }
  
  for( Int_t k=0,i=0; i<8; i++)
    for( Int_t j=0; j<=i; j++, k++ ){
      e[k] = 0;
      for( Int_t l=0; l<8; l++ ) e[k]+=mJC[i][l]*mJ[j][l];
    }
  
  return;
  */

/*  float 
    c6=fC[6], c7=fC[7], c8=fC[8], c17=fC[17], c18=fC[18],
    c24 = fC[24], c31 = fC[31];

  float 
    cBC13 = cB*fC[13],
    mJC13 = c7 - cB*fC[9] + sB*fC[13],
    mJC14 = fC[11] - cBC13 + sB*fC[14],
    mJC23 = c8 + t*c18,
    mJC24 = fC[12] + t*fC[19],
    mJC33 = c*fC[9] + s*fC[13],
    mJC34 = c*fC[13] + s*fC[14],
    mJC43 = -s*fC[9] + c*fC[13],
    mJC44 = -s*fC[13] + c*fC[14];


  e[0]= fC[0] + 2*(sB*c6 + cB*fC[10]) + (sB*fC[9] + 2*cBC13)*sB + cB*cB*fC[14];
  e[1]= fC[1] - cB*c6 + sB*fC[10] + mJC13*sB + mJC14*cB;
  e[2]= fC[2] - cB*c7 + sB*fC[11] - mJC13*cB + mJC14*sB;
  e[3]= fC[3] + t*fC[15] + mJC23*sB + mJC24*cB;
  e[4]= fC[4] + t*fC[16] - mJC23*cB + mJC24*sB;

  e[15]= fC[15] + c18*sB + fC[19]*cB;
  e[16]= fC[16] - c18*cB + fC[19]*sB;
  e[17]= c17 + fC[20]*t;
  e[18]= c18*c + fC[19]*s;
  e[19]= -c18*s + fC[19]*c;

  e[5]= fC[5] + (c17 + e[17] )*t;

  e[6]= c*c6 + s*fC[10] + mJC33*sB + mJC34*cB;
  e[7]= c*c7 + s*fC[11] - mJC33*cB + mJC34*sB;
  e[8]= c*c8 + s*fC[12] + e[18]*t;
  e[9]= mJC33*c + mJC34*s;
  e[10]= -s*c6 + c*fC[10] + mJC43*sB + mJC44*cB;

    
  e[11]= -s*c7 + c*fC[11] - mJC43*cB + mJC44*sB;
  e[12]= -s*c8 + c*fC[12] + e[19]*t;
  e[13]= mJC43*c + mJC44*s;
  e[14]= -mJC43*s + mJC44*c;
  e[20]= fC[20];
  e[21]= fC[21] + fC[25]*cB + c24*sB;
  e[22]= fC[22] - c24*cB + fC[25]*sB;
  e[23]= fC[23] + fC[26]*t;
  e[24]= c*c24 + s*fC[25];
  e[25]= c*fC[25] - c24*s;
  e[26]= fC[26];
  e[27]= fC[27];
  e[28]= fC[28] + fC[32]*cB + c31*sB;
  e[29]= fC[29] - c31*cB + fC[32]*sB;
  e[30]= fC[30] + fC[33]*t;
  e[31]= c*c31 + s*fC[32];
  e[32]= c*fC[32] - s*c31;
  e[33]= fC[33];
  e[34]= fC[34];
  e[35]= fC[35];   */  

  float mJ[8][8];
  for( Int_t i=0; i<8; i++ ) for( Int_t j=0; j<8; j++) mJ[i][j]=0;

  for(int i=0; i<8; i++) mJ[i][i]=1;
  mJ[0][3] =  sB; mJ[0][4] = cB;
  mJ[1][3] = -cB; mJ[1][4] = sB;
  mJ[2][5] = t;
  mJ[3][3] =  c; mJ[3][4] = s;
  mJ[4][3] = -s; mJ[4][4] = c;
  
  
  float mJds[6][6];
  for( Int_t i=0; i<6; i++ ) for( Int_t j=0; j<6; j++) mJds[i][j]=0;
  mJds[0][3] =  c; mJds[0][4] = s;
  mJds[1][3] = -s; mJds[1][4] = c;
  mJds[2][5] = 1;
  mJds[3][3] = -b*s; mJds[3][4] =  b*c;
  mJds[4][3] = -b*c; mJds[4][4] = -b*s;
  
  for(int i1=0; i1<6; i1++)
    for(int i2=0; i2<6; i2++)
      mJ[i1][i2] += mJds[i1][3]*px*dsdr[i2] + mJds[i1][4]*py*dsdr[i2] + mJds[i1][5]*pz*dsdr[i2];
  
  MultQSQt( mJ[0], fC, e, 8);
  
  if(F)
  {
    for(int i=0; i<6; i++)
      for(int j=0; j<6; j++)
        F[i*6+j] = mJ[i][j];

    for(int i1=0; i1<6; i1++)
      for(int i2=0; i2<6; i2++)
        F1[i1*6 + i2] = mJds[i1][3]*px*dsdr1[i2] + mJds[i1][4]*py*dsdr1[i2] + mJds[i1][5]*pz*dsdr1[i2];
  }
}


float KFParticleBase::GetDistanceFromVertex( const KFParticleBase &Vtx ) const
{
  //* Calculate distance from vertex [cm]

  return GetDistanceFromVertex( Vtx.fP );
}

float KFParticleBase::GetDistanceFromVertex( const float vtx[] ) const
{
  //* Calculate distance from vertex [cm]

  float mP[8], mC[36];  
  
  float dsdr[6] = {0.f};
  const float dS = GetDStoPoint(vtx, dsdr);
  
  Transport( dS, dsdr, mP, mC );
  float d[3]={ vtx[0]-mP[0], vtx[1]-mP[1], vtx[2]-mP[2]};
  return sqrt( d[0]*d[0]+d[1]*d[1]+d[2]*d[2] );
}

float KFParticleBase::GetDistanceFromParticle( const KFParticleBase &p ) 
  const
{ 
  //* Calculate distance to other particle [cm]
  
  float dsdr[4][6];
  float dS[2];
  GetDStoParticle( p, dS, dsdr );   
  float mP[8], mC[36], mP1[8], mC1[36];
  Transport( dS[0], dsdr[0], mP, mC ); 
  p.Transport( dS[1], dsdr[3], mP1, mC1 ); 
  float dx = mP[0]-mP1[0]; 
  float dy = mP[1]-mP1[1]; 
  float dz = mP[2]-mP1[2]; 
  return sqrt(dx*dx+dy*dy+dz*dz);
}

float KFParticleBase::GetDeviationFromVertex( const KFParticleBase &Vtx ) const
{
  //* Calculate Chi2 deviation from vertex

  return GetDeviationFromVertex( Vtx.fP, Vtx.fC );
}


float KFParticleBase::GetDeviationFromVertex( const float v[], const float Cv[] ) const
{
  //* Calculate Chi2 deviation from vertex
  //* v = [xyz], Cv=[Cxx,Cxy,Cyy,Cxz,Cyz,Czz]-covariance matrix

  float mP[8];
  float mC[36];
  float dsdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  const float dS = GetDStoPoint(v, dsdr);
  float dsdp[6] = {-dsdr[0], -dsdr[1], -dsdr[2], 0, 0, 0};
  float F[36], F1[36];
  for(int i2=0; i2<36; i2++)
  {
    F[i2]  = 0;
    F1[i2] = 0;
  }
  Transport( dS, dsdr, mP, mC, dsdp, F, F1 );  

  if(Cv)
  {
    float VFT[3][6];
    for(int i=0; i<3; i++)
      for(int j=0; j<6; j++)
      {
        VFT[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          VFT[i][j] +=  Cv[IJ(i,k)] * F1[j*6+k];
        }
      }
  
    float FVFT[6][6];
    for(int i=0; i<6; i++)
      for(int j=0; j<6; j++)
      {
        FVFT[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          FVFT[i][j] += F1[i*6+k] * VFT[k][j];
        }
      }
    mC[0] += FVFT[0][0] + Cv[0];
    mC[1] += FVFT[1][0] + Cv[1];
    mC[2] += FVFT[1][1] + Cv[2];
    mC[3] += FVFT[2][0] + Cv[3];
    mC[4] += FVFT[2][1] + Cv[4];
    mC[5] += FVFT[2][2] + Cv[5];
  }
  
  InvertCholetsky3(mC);
  
  float d[3]={ v[0]-mP[0], v[1]-mP[1], v[2]-mP[2]};

  return ( ( mC[0]*d[0] + mC[1]*d[1] + mC[3]*d[2])*d[0]
           +(mC[1]*d[0] + mC[2]*d[1] + mC[4]*d[2])*d[1]
           +(mC[3]*d[0] + mC[4]*d[1] + mC[5]*d[2])*d[2] );
}


float KFParticleBase::GetDeviationFromParticle( const KFParticleBase &p ) const
{ 
  //* Calculate Chi2 deviation from other particle
  
  float ds[2] = {0.f,0.f};
  float dsdr[4][6];
  float F1[36], F2[36], F3[36], F4[36];
  for(int i1=0; i1<36; i1++)
  {
    F1[i1] = 0;
    F2[i1] = 0;
    F3[i1] = 0;
    F4[i1] = 0;
  }
  GetDStoParticle( p, ds, dsdr );
  
  float V0Tmp[36] = {0.};
  float V1Tmp[36] = {0.};

  
  float mP1[8], mC1[36];
  float mP2[8], mC2[36]; 
  
    Transport(ds[0], dsdr[0], mP1, mC1, dsdr[1], F1, F2);
  p.Transport(ds[1], dsdr[3], mP2, mC2, dsdr[2], F4, F3);
  
  MultQSQt(F2, p.fC, V0Tmp, 6);
  MultQSQt(F3,   fC, V1Tmp, 6);
      
  for(int iC=0; iC<6; iC++)
    mC1[iC] += V0Tmp[iC] + mC2[iC] + V1Tmp[iC];

  float d[3]={ mP2[0]-mP1[0], mP2[1]-mP1[1], mP2[2]-mP1[2]};
  
  return ( ( mC1[0]*d[0] + mC1[1]*d[1] + mC1[3]*d[2])*d[0]
           +(mC1[1]*d[0] + mC1[2]*d[1] + mC1[4]*d[2])*d[1]
           +(mC1[3]*d[0] + mC1[4]*d[1] + mC1[5]*d[2])*d[2] );
}



void KFParticleBase::SubtractFromVertex(  KFParticleBase &Vtx ) const
{
  //* Subtract the particle from the vertex  

  //TODO this solution is an approximation, find exact solution
  float m[8];
  float mCm[36];
  float D[3][3];
  Vtx.GetMeasurement( *this, m, mCm, D );
  //* 
	    
  float mS[6] = { mCm[0] - Vtx.fC[0] + (D[0][0] + D[0][0]), 
                  mCm[1] - Vtx.fC[1] + (D[1][0] + D[0][1]), mCm[2] - Vtx.fC[2] + (D[1][1] + D[1][1]), 
                  mCm[3] - Vtx.fC[3] + (D[2][0] + D[0][2]), mCm[4] - Vtx.fC[4] + (D[1][2] + D[2][1]), mCm[5] - Vtx.fC[5] + (D[2][2] + D[2][2]) };
  InvertCholetsky3(mS);   
    
  //* Residual (measured - estimated)
    
  float zeta[3] = { m[0]-Vtx.fP[0], m[1]-Vtx.fP[1], m[2]-Vtx.fP[2] };
        
  //* mCHt = mCH' - D'
    
  float mCHt0[3], mCHt1[3], mCHt2[3];
    
  mCHt0[0]=Vtx.fC[ 0] ;      mCHt1[0]=Vtx.fC[ 1] ;      mCHt2[0]=Vtx.fC[ 3] ;
  mCHt0[1]=Vtx.fC[ 1] ;      mCHt1[1]=Vtx.fC[ 2] ;      mCHt2[1]=Vtx.fC[ 4] ;
  mCHt0[2]=Vtx.fC[ 3] ;      mCHt1[2]=Vtx.fC[ 4] ;      mCHt2[2]=Vtx.fC[ 5] ;
  
  //* Kalman gain K = mCH'*S
    
  float k0[3], k1[3], k2[3];
    
  for(Int_t i=0;i<3;++i){
    k0[i] = mCHt0[i]*mS[0] + mCHt1[i]*mS[1] + mCHt2[i]*mS[3];
    k1[i] = mCHt0[i]*mS[1] + mCHt1[i]*mS[2] + mCHt2[i]*mS[4];
    k2[i] = mCHt0[i]*mS[3] + mCHt1[i]*mS[4] + mCHt2[i]*mS[5];
  }
    
  //* New estimation of the vertex position r += K*zeta
    
  float dChi2 = ((mS[0]*zeta[0] + mS[1]*zeta[1] + mS[3]*zeta[2])*zeta[0]
              +  (mS[1]*zeta[0] + mS[2]*zeta[1] + mS[4]*zeta[2])*zeta[1]
              +  (mS[3]*zeta[0] + mS[4]*zeta[1] + mS[5]*zeta[2])*zeta[2]);

  for(Int_t i=0;i<3;++i) 
    Vtx.fP[i] -= k0[i]*zeta[0] + k1[i]*zeta[1] + k2[i]*zeta[2];       
    
  //* New covariance matrix C -= K*(mCH')'
    
  for(Int_t i=0, k=0;i<3;++i){
    for(Int_t j=0;j<=i;++j,++k) 
      Vtx.fC[k] += k0[i]*mCHt0[j] + k1[i]*mCHt1[j] + k2[i]*mCHt2[j];
  }
    
  //* Calculate Chi^2 

  Vtx.fNDF  -= 2;
  Vtx.fChi2 -= dChi2;
}

void KFParticleBase::SubtractFromParticle(  KFParticleBase &Vtx ) const
{
  //* Subtract the particle from the mother particle  
  //TODO this solution is an approximation, find exact solution

  float m[8];
  float mV[36];

  float D[3][3];
  Vtx.GetMeasurement( *this, m, mV, D );

  float mS[6] = { mV[0] - Vtx.fC[0] + (D[0][0] + D[0][0]), 
                  mV[1] - Vtx.fC[1] + (D[1][0] + D[0][1]), mV[2] - Vtx.fC[2] + (D[1][1] + D[1][1]), 
                  mV[3] - Vtx.fC[3] + (D[2][0] + D[0][2]), mV[4] - Vtx.fC[4] + (D[1][2] + D[2][1]), mV[5] - Vtx.fC[5] + (D[2][2] + D[2][2]) };
  InvertCholetsky3(mS);

  //* Residual (measured - estimated)

  float zeta[3] = { m[0]-Vtx.fP[0], m[1]-Vtx.fP[1], m[2]-Vtx.fP[2] };    

  //* CHt = CH' - D'

  float mCHt0[7], mCHt1[7], mCHt2[7];

  mCHt0[0]=mV[ 0] ;           mCHt1[0]=mV[ 1] ;           mCHt2[0]=mV[ 3] ;
  mCHt0[1]=mV[ 1] ;           mCHt1[1]=mV[ 2] ;           mCHt2[1]=mV[ 4] ;
  mCHt0[2]=mV[ 3] ;           mCHt1[2]=mV[ 4] ;           mCHt2[2]=mV[ 5] ;
  mCHt0[3]=Vtx.fC[ 6]-mV[ 6]; mCHt1[3]=Vtx.fC[ 7]-mV[ 7]; mCHt2[3]=Vtx.fC[ 8]-mV[ 8];
  mCHt0[4]=Vtx.fC[10]-mV[10]; mCHt1[4]=Vtx.fC[11]-mV[11]; mCHt2[4]=Vtx.fC[12]-mV[12];
  mCHt0[5]=Vtx.fC[15]-mV[15]; mCHt1[5]=Vtx.fC[16]-mV[16]; mCHt2[5]=Vtx.fC[17]-mV[17];
  mCHt0[6]=Vtx.fC[21]-mV[21]; mCHt1[6]=Vtx.fC[22]-mV[22]; mCHt2[6]=Vtx.fC[23]-mV[23];

  //* Kalman gain K = mCH'*S
    
  float k0[7], k1[7], k2[7];
    
  for(Int_t i=0;i<7;++i){
    k0[i] = mCHt0[i]*mS[0] + mCHt1[i]*mS[1] + mCHt2[i]*mS[3];
    k1[i] = mCHt0[i]*mS[1] + mCHt1[i]*mS[2] + mCHt2[i]*mS[4];
    k2[i] = mCHt0[i]*mS[3] + mCHt1[i]*mS[4] + mCHt2[i]*mS[5];
  }

    //* Add the daughter momentum to the particle momentum
    
  Vtx.fP[ 3] -= m[ 3];
  Vtx.fP[ 4] -= m[ 4];
  Vtx.fP[ 5] -= m[ 5];
  Vtx.fP[ 6] -= m[ 6];
  
  Vtx.fC[ 9] -= mV[ 9];
  Vtx.fC[13] -= mV[13];
  Vtx.fC[14] -= mV[14];
  Vtx.fC[18] -= mV[18];
  Vtx.fC[19] -= mV[19];
  Vtx.fC[20] -= mV[20];
  Vtx.fC[24] -= mV[24];
  Vtx.fC[25] -= mV[25];
  Vtx.fC[26] -= mV[26];
  Vtx.fC[27] -= mV[27];

   //* New estimation of the vertex position r += K*zeta
    
  for(Int_t i=0;i<3;++i) 
    Vtx.fP[i] = m[i] - (k0[i]*zeta[0] + k1[i]*zeta[1] + k2[i]*zeta[2]);
  for(Int_t i=3;i<7;++i) 
    Vtx.fP[i] = Vtx.fP[i] - (k0[i]*zeta[0] + k1[i]*zeta[1] + k2[i]*zeta[2]);

    //* New covariance matrix C -= K*(mCH')'

  float ffC[28] = {-mV[ 0],
                   -mV[ 1], -mV[ 2],
                   -mV[ 3], -mV[ 4], -mV[ 5],
                    mV[ 6],  mV[ 7],  mV[ 8], Vtx.fC[ 9],
                    mV[10],  mV[11],  mV[12], Vtx.fC[13], Vtx.fC[14],
                    mV[15],  mV[16],  mV[17], Vtx.fC[18], Vtx.fC[19], Vtx.fC[20],
                    mV[21],  mV[22],  mV[23], Vtx.fC[24], Vtx.fC[25], Vtx.fC[26], Vtx.fC[27] };

  for(Int_t i=0, k=0;i<7;++i){
    for(Int_t j=0;j<=i;++j,++k){
      Vtx.fC[k] = ffC[k] + (k0[i]*mCHt0[j] + k1[i]*mCHt1[j] + k2[i]*mCHt2[j] );
    }
  }

    //* Calculate Chi^2 
  Vtx.fNDF  -= 2;
  Vtx.fQ    -= GetQ();
  Vtx.fSFromDecay = 0;    
  Vtx.fChi2 -= ((mS[0]*zeta[0] + mS[1]*zeta[1] + mS[3]*zeta[2])*zeta[0]
             +  (mS[1]*zeta[0] + mS[2]*zeta[1] + mS[4]*zeta[2])*zeta[1]
             +  (mS[3]*zeta[0] + mS[4]*zeta[1] + mS[5]*zeta[2])*zeta[2]);     
}

void KFParticleBase::TransportLine( float dS, const float* dsdr, float P[], float C[], float* dsdr1, float* F, float* F1 ) const 
{
  //* Transport the particle as a straight line 
  
  float mJ[8][8];
  for( Int_t i=0; i<8; i++ ) for( Int_t j=0; j<8; j++) mJ[i][j]=0;

  mJ[0][0]=1; mJ[0][1]=0; mJ[0][2]=0; mJ[0][3]=dS;  mJ[0][4]=0;  mJ[0][5]=0;
  mJ[1][0]=0; mJ[1][1]=1; mJ[1][2]=0; mJ[1][3]=0;     mJ[1][4]=dS;  mJ[1][5]=0;
  mJ[2][0]=0; mJ[2][1]=0; mJ[2][2]=1; mJ[2][3]=0; mJ[2][4]=0; mJ[2][5]=dS;
  
  mJ[3][0]=0; mJ[3][1]=0; mJ[3][2]=0; mJ[3][3]=1;   mJ[3][4]=0;  mJ[3][5]=0;
  mJ[4][0]=0; mJ[4][1]=0; mJ[4][2]=0; mJ[4][3]=0;     mJ[4][4]=1;   mJ[4][5]=0;
  mJ[5][0]=0; mJ[5][1]=0; mJ[5][2]=0; mJ[5][3]=0; mJ[5][4]=0; mJ[5][5]=1;
  mJ[6][6] = mJ[7][7] = 1;
  
  float px = fP[3], py = fP[4], pz = fP[5];
  
  P[0] = fP[0] + dS*fP[3];
  P[1] = fP[1] + dS*fP[4];
  P[2] = fP[2] + dS*fP[5];
  P[3] = fP[3];
  P[4] = fP[4];
  P[5] = fP[5];
  P[6] = fP[6];
  P[7] = fP[7];
  
  float mJds[6][6];
  for( Int_t i=0; i<6; i++ ) for( Int_t j=0; j<6; j++) mJds[i][j]=0;
  
  mJds[0][3]= 1; 
  mJds[1][4]= 1;
  mJds[2][5]= 1;
  
  for(int i1=0; i1<6; i1++)
    for(int i2=0; i2<6; i2++)
      mJ[i1][i2] += mJds[i1][3]*px*dsdr[i2] + mJds[i1][4]*py*dsdr[i2] + mJds[i1][5]*pz*dsdr[i2];
  MultQSQt( mJ[0], fC, C, 8);
  
  if(F)
  {
    for(int i=0; i<6; i++)
      for(int j=0; j<6; j++)
        F[i*6+j] = mJ[i][j];

    for(int i1=0; i1<6; i1++)
      for(int i2=0; i2<6; i2++)
        F1[i1*6 + i2] = mJds[i1][3]*px*dsdr1[i2] + mJds[i1][4]*py*dsdr1[i2] + mJds[i1][5]*pz*dsdr1[i2];
  }
}

void KFParticleBase::GetArmenterosPodolanski(KFParticleBase& positive, KFParticleBase& negative, float QtAlfa[2] )
{
// example:
//       KFParticle PosParticle(...)
//       KFParticle NegParticle(...)
//       Gamma.ConstructGamma(PosParticle, NegParticle);
//       float VertexGamma[3] = {Gamma.GetX(), Gamma.GetY(), Gamma.GetZ()};
//       PosParticle.TransportToPoint(VertexGamma);
//       NegParticle.TransportToPoint(VertexGamma);
//       float armenterosQtAlfa[2] = {0.};
//       KFParticle::GetArmenterosPodolanski(PosParticle, NegParticle, armenterosQtAlfa );

  float alpha = 0., qt = 0.;
  float spx = positive.GetPx() + negative.GetPx();
  float spy = positive.GetPy() + negative.GetPy();
  float spz = positive.GetPz() + negative.GetPz();
  float sp  = sqrt(spx*spx + spy*spy + spz*spz);
  if( sp == 0.0) return;
  float pn, pln, plp;

  pn = sqrt(negative.GetPx()*negative.GetPx() + negative.GetPy()*negative.GetPy() + negative.GetPz()*negative.GetPz());
//   pp = sqrt(positive.GetPx()*positive.GetPx() + positive.GetPy()*positive.GetPy() + positive.GetPz()*positive.GetPz());
  pln  = (negative.GetPx()*spx+negative.GetPy()*spy+negative.GetPz()*spz)/sp;
  plp  = (positive.GetPx()*spx+positive.GetPy()*spy+positive.GetPz()*spz)/sp;

  if( pn == 0.0) return;
  float ptm  = (1.-((pln/pn)*(pln/pn)));
  qt= (ptm>=0.)?  pn*sqrt(ptm) :0;
  alpha = (plp-pln)/(plp+pln);

  QtAlfa[0] = qt;
  QtAlfa[1] = alpha;
}

void KFParticleBase::RotateXY(float angle, float Vtx[3])
{
  // Rotates the KFParticle object around OZ axis, OZ axis is set by the vertex position
  // float angle - angle of rotation in XY plane in [rad]
  // float Vtx[3] - position of the vertex in [cm]

  // Before rotation the center of the coordinat system should be moved to the vertex position; move back after rotation
  X() = X() - Vtx[0];
  Y() = Y() - Vtx[1];
  Z() = Z() - Vtx[2];

  // Rotate the kf particle
  float c = cos(angle);
  float s = sin(angle);

  float mA[8][ 8];
  for( Int_t i=0; i<8; i++ ){
    for( Int_t j=0; j<8; j++){
      mA[i][j] = 0;
    }
  }
  for( int i=0; i<8; i++ ){
    mA[i][i] = 1;
  }
  mA[0][0] =  c;  mA[0][1] = s;
  mA[1][0] = -s;  mA[1][1] = c;
  mA[3][3] =  c;  mA[3][4] = s;
  mA[4][3] = -s;  mA[4][4] = c;

  float mAC[8][8];
  float mAp[8];

  for( Int_t i=0; i<8; i++ ){
    mAp[i] = 0;
    for( Int_t k=0; k<8; k++){
      mAp[i]+=mA[i][k] * fP[k];
    }
  }

  for( Int_t i=0; i<8; i++){
    fP[i] = mAp[i];
  }

  for( Int_t i=0; i<8; i++ ){
    for( Int_t j=0; j<8; j++ ){
      mAC[i][j] = 0;
      for( Int_t k=0; k<8; k++ ){
        mAC[i][j]+= mA[i][k] * GetCovariance(k,j);
      }
    }
  }

  for( Int_t i=0; i<8; i++ ){
    for( Int_t j=0; j<=i; j++ ){
      float xx = 0;
      for( Int_t k=0; k<8; k++){
        xx+= mAC[i][k]*mA[j][k];
      }
      Covariance(i,j) = xx;
    }
  }

  X() = GetX() + Vtx[0];
  Y() = GetY() + Vtx[1];
  Z() = GetZ() + Vtx[2];
}

Bool_t KFParticleBase::InvertSym3( const float A[], float Ai[] )
{
  //* Invert symmetric matric stored in low-triagonal form 

  bool ret = 0;
  float a0 = A[0], a1 = A[1], a2 = A[2], a3 = A[3];

  Ai[0] = a2*A[5] - A[4]*A[4];
  Ai[1] = a3*A[4] - a1*A[5];
  Ai[3] = a1*A[4] - a2*a3;
  float det = (a0*Ai[0] + a1*Ai[1] + a3*Ai[3]);
  if( fabs(det)>1.e-20 ) det = 1./det;    
  else{ 
    det = 0;
    ret = 1;
  }
  Ai[0] *= det;
  Ai[1] *= det;
  Ai[3] *= det;
  Ai[2] = ( a0*A[5] - a3*a3 )*det;
  Ai[4] = ( a1*a3 - a0*A[4] )*det;
  Ai[5] = ( a0*a2 - a1*a1 )*det;
  return ret;
}

void KFParticleBase::InvertCholetsky3(float a[6])
{
  float d[3], uud, u[3][3];
  for(int i=0; i<3; i++) 
  {
    d[i]=0;
    for(int j=0; j<3; j++) 
      u[i][j]=0;
  }

  for(int i=0; i<3 ; i++)
  {
    uud=0;
    for(int j=0; j<i; j++) 
      uud += u[j][i]*u[j][i]*d[j];
    uud = a[i*(i+3)/2] - uud;

    if(fabs(uud)<1.e-12f) uud = 1.e-12f;

    d[i] = uud/fabs(uud);
    u[i][i] = sqrt(fabs(uud));

    for(int j=i+1; j<3; j++) 
    {
      uud = 0;
      for(int k=0; k<i; k++)
        uud += u[k][i]*u[k][j]*d[k];
      uud = a[j*(j+1)/2+i] - uud;
      u[i][j] = d[i]/u[i][i]*uud;
    }
  }

  float u1[3];

  for(int i=0; i<3; i++)
  {
    u1[i] = u[i][i];
    u[i][i] = 1/u[i][i];
  }
  for(int i=0; i<2; i++)
  {
    u[i][i+1] = - u[i][i+1]*u[i][i]*u[i+1][i+1];
  }
  for(int i=0; i<1; i++)
  {
    u[i][i+2] = u[i][i+1]*u1[i+1]*u[i+1][i+2]-u[i][i+2]*u[i][i]*u[i+2][i+2];
  }

  for(int i=0; i<3; i++)
    a[i+3] = u[i][2]*u[2][2]*d[2];
  for(int i=0; i<2; i++)
    a[i+1] = u[i][1]*u[1][1]*d[1] + u[i][2]*u[1][2]*d[2];
  a[0] = u[0][0]*u[0][0]*d[0] + u[0][1]*u[0][1]*d[1] + u[0][2]*u[0][2]*d[2];
}

void KFParticleBase::MultQSQt( const float Q[], const float S[], float SOut[], const int kN )
{
  //* Matrix multiplication Q*S*Q^T, Q - square matrix, S - symmetric

  float* mA = new float[kN*kN];
  
  for( Int_t i=0, ij=0; i<kN; i++ ){
    for( Int_t j=0; j<kN; j++, ++ij ){
      mA[ij] = 0 ;
      for( Int_t k=0; k<kN; ++k ) mA[ij]+= S[( k<=i ) ? i*(i+1)/2+k :k*(k+1)/2+i] * Q[ j*kN+k];
    }
  }
    
  for( Int_t i=0; i<kN; i++ ){
    for( Int_t j=0; j<=i; j++ ){
      Int_t ij = ( j<=i ) ? i*(i+1)/2+j :j*(j+1)/2+i;
      SOut[ij] = 0 ;
      for( Int_t k=0; k<kN; k++ )  SOut[ij] += Q[ i*kN+k ] * mA[ k*kN+j ];
    }
  }
  
  if(mA) delete [] mA;
}

// 72-charachters line to define the printer border
//3456789012345678901234567890123456789012345678901234567890123456789012

