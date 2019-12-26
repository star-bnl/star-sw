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


#include "KFParticleBaseSIMD.h"
#include <iostream>

static const float_v small = 1.e-20f;

KFParticleBaseSIMD::KFParticleBaseSIMD() :fQ(0), fNDF(-3), fChi2(0.f), fSFromDecay(0.f),
  SumDaughterMass(0.f), fMassHypo(-1.f), fId(-1), fAtProductionVertex(0), fPDG(0), fConstructMethod(0), fDaughterIds()
{ 
  /** The default constructor, initialises the parameters by: \n
   ** 1) all parameters are set to 0; \n
   ** 2) all elements of the covariance matrix are set to 0 except Cxx=Cyy=Czz=100; \n
   ** 3) Q = 0; \n
   ** 4) chi2 is set to 0; \n
   ** 5) NDF = -3, since 3 parameters should be fitted: X, Y, Z. 
   **/
  Initialize();
}

void KFParticleBaseSIMD::Initialize( const float_v Param[], const float_v Cov[], int_v Charge, float_v Mass )
{
  /** Sets the parameters of the particle:
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

  float_v energy = sqrt( Mass*Mass + fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5]);
  fP[6] = energy;
  fP[7] = 0;
  fQ = Charge;
  fNDF = 0;
  fChi2 = 0;
  fAtProductionVertex = 0;
  fSFromDecay = 0;

  float_v energyInv = 1.f/energy;
  float_v 
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
  for( Int_t i=28; i<36; i++ ) fC[i] = 0.f;
  fC[35] = 1.f;

  SumDaughterMass = Mass;
  fMassHypo = Mass;
}

void KFParticleBaseSIMD::Initialize()
{
  /** Initialises the parameters by default: \n
   ** 1) all parameters are set to 0; \n
   ** 2) all elements of the covariance matrix are set to 0 except Cxx=Cyy=Czz=100; \n
   ** 3) Q = 0; \n
   ** 4) chi2 is set to 0; \n
   ** 5) NDF = -3, since 3 parameters should be fitted: X, Y, Z. 
   **/
  
  for( Int_t i=0; i<8; i++) fP[i] = 0.f;
  for(Int_t i=0;i<36;++i) fC[i]=0.f;
  fC[0] = fC[2] = fC[5] = 100.f;
  fC[35] = 1.f;
  fNDF  = -3;
  fChi2 =  0.f;
  fQ = 0;
  fSFromDecay = 0.f;
  fAtProductionVertex = 0;
  SumDaughterMass = 0.f;
  fMassHypo = -1;
}

float_m KFParticleBaseSIMD::GetMomentum( float_v &p, float_v &error )  const 
{
  /** Calculates particle momentum and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** \param[out] p - momentum of the particle
   ** \param[out] error - its error
   **/
  
  float_v x = fP[3];
  float_v y = fP[4];
  float_v z = fP[5];
  float_v x2 = x*x;
  float_v y2 = y*y;
  float_v z2 = z*z;
  float_v p2 = x2+y2+z2;
  p = sqrt(p2);
  error = (x2*fC[9]+y2*fC[14]+z2*fC[20] + 2*(x*y*fC[13]+x*z*fC[18]+y*z*fC[19]) );
  const float_v LocalSmall = 1.e-4f;
  float_m mask = (0.f < error) && (LocalSmall < abs(p));
  error(!mask) = 1.e20f;
  error = sqrt(error);
  return (!mask);
}

float_m KFParticleBaseSIMD::GetPt( float_v &pt, float_v &error )  const 
{
  /** Calculates particle transverse  momentum and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** \param[out] pt - transverse momentum of the particle
   ** \param[out] error - its error
   **/

  float_v px = fP[3];
  float_v py = fP[4];
  float_v px2 = px*px;
  float_v py2 = py*py;
  float_v pt2 = px2+py2;
  pt = sqrt(pt2);
  error = (px2*fC[9] + py2*fC[14] + 2*px*py*fC[13] );
  const float_v LocalSmall = 1.e-4f;
  float_m mask = ( (0.f < error) && (LocalSmall < abs(pt)));
  error(!mask) = 1.e20f;
  error = sqrt(error);
  return (!mask);
}

float_m KFParticleBaseSIMD::GetEta( float_v &eta, float_v &error )  const 
{
  /** Calculates particle pseudorapidity and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** \param[out] eta - pseudorapidity of the particle
   ** \param[out] error - its error
   **/
  
  const float_v BIG = 1.e8f;
  const float_v LocalSmall = 1.e-8f;

  float_v px = fP[3];
  float_v py = fP[4];
  float_v pz = fP[5];
  float_v pt2 = px*px + py*py;
  float_v p2 = pt2 + pz*pz;
  float_v p = sqrt(p2);
  float_v a = p + pz;
  float_v b = p - pz;
  eta = BIG;
  float_v c = 0.f;
  c(b > LocalSmall) = (a/b);
  float_v logc = 0.5f*KFPMath::Log(c);
  eta(LocalSmall<abs(c)) = logc;

  float_v h3 = -px*pz;
  float_v h4 = -py*pz;  
  float_v pt4 = pt2*pt2;
  float_v p2pt4 = p2*pt4;
  error = (h3*h3*fC[9] + h4*h4*fC[14] + pt4*fC[20] + 2*( h3*(h4*fC[13] + fC[18]*pt2) + pt2*h4*fC[19] ) );

  float_m mask = ((LocalSmall < abs(p2pt4)) && (0.f < error));
  error(mask) = sqrt(error/p2pt4);
  error(!mask) = BIG;

  return (!mask);
}

float_m KFParticleBaseSIMD::GetPhi( float_v &phi, float_v &error )  const 
{
  /** Calculates particle polar angle at the current point and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** \param[out] phi - polar angle of the particle
   ** \param[out] error - its error
   **/
  
  float_v px = fP[3];
  float_v py = fP[4];
  float_v px2 = px*px;
  float_v py2 = py*py;
  float_v pt2 = px2 + py2;
  phi = KFPMath::ATan2(py,px);
  error = (py2*fC[9] + px2*fC[14] - float_v(2.f)*px*py*fC[13] );

  float_m mask = (0.f < error) && (1.e-4f < pt2);
  error(mask) = sqrt(error)/pt2;
  error(!mask) = 1.e10f;
  return !mask;
}

float_m KFParticleBaseSIMD::GetR( float_v &r, float_v &error )  const 
{
  /** Calculates the distance to the point {0,0,0} and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** \param[out] r - polar angle of the particle
   ** \param[out] error - its error
   **/
  
  float_v x = fP[0];
  float_v y = fP[1];
  float_v x2 = x*x;
  float_v y2 = y*y;
  r = sqrt(x2 + y2);
  error = (x2*fC[0] + y2*fC[2] - float_v(2.f)*x*y*fC[1] );

  float_m mask = (0.f < error) && (1.e-4f < r);
  error(mask) = sqrt(error)/r;
  error(!mask ) = 1.e10f;
  return !mask;
}

float_m KFParticleBaseSIMD::GetMass( float_v &m, float_v &error ) const 
{
  /** Calculates the mass of the particle and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** \param[out] m - mass of the particle
   ** \param[out] error - its error
   **/  

  const float_v BIG = 1.e8f;
  const float_v LocalSmall = 1.e-8f;

  float_v s = (  fP[3]*fP[3]*fC[9] + fP[4]*fP[4]*fC[14] + fP[5]*fP[5]*fC[20] 
               + fP[6]*fP[6]*fC[27] 
               + float_v(2.f)*( + fP[3]*fP[4]*fC[13] + fP[5]*(fP[3]*fC[18] + fP[4]*fC[19]) 
               - fP[6]*( fP[3]*fC[24] + fP[4]*fC[25] + fP[5]*fC[26] )   )
              ); 

  float_v m2 = (fP[6]*fP[6] - fP[3]*fP[3] - fP[4]*fP[4] - fP[5]*fP[5]);

  float_m mask = 0.f <= m2;
  m(mask) = sqrt(m2);
  m(!mask) = -sqrt(-m2);

  mask = (mask && (0.f <= s) && (LocalSmall < m));
  error(mask) = sqrt(s)/m;
  error(!mask) = BIG;

  return !mask;
}


float_m KFParticleBaseSIMD::GetDecayLength( float_v &l, float_v &error ) const 
{
  /** Calculates the decay length of the particle in the laboratory system and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** The production point should be set before calling this function.
   ** \param[out] l - the decay length
   ** \param[out] error - its error
   **/
  
  const float_v BIG = 1.e20f;

  float_v x = fP[3];
  float_v y = fP[4];
  float_v z = fP[5];
  float_v t = fP[7];
  float_v x2 = x*x;
  float_v y2 = y*y;
  float_v z2 = z*z;
  float_v p2 = x2+y2+z2;
  l = t*sqrt(p2);

  error = p2*fC[35] + t*t/p2*(x2*fC[9]+y2*fC[14]+z2*fC[20]
        + float_v(2.f)*(x*y*fC[13]+x*z*fC[18]+y*z*fC[19]) )
        + float_v(2.f)*t*(x*fC[31]+y*fC[32]+z*fC[33]);

  float_m mask = ((1.e-4f) < p2);
  error(mask) = sqrt(abs(error));
  error(!mask) = BIG;
  return !mask;
}

float_m KFParticleBaseSIMD::GetDecayLengthXY( float_v &l, float_v &error ) const 
{
  /** Calculates the projection in the XY plane of the decay length of the particle in the laboratory 
   ** system and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** The production point should be set before calling this function.
   ** \param[out] l - the decay length
   ** \param[out] error - its error
   **/
  
  const float_v BIG = 1.e8f;
  float_v x = fP[3];
  float_v y = fP[4];
  float_v t = fP[7];
  float_v x2 = x*x;
  float_v y2 = y*y;
  float_v pt2 = x2+y2;
  l = t*sqrt(pt2);

  error = pt2*fC[35] + t*t/pt2*(x2*fC[9]+y2*fC[14] + 2*x*y*fC[13] )
        + float_v(2.f)*t*(x*fC[31]+y*fC[32]);
  float_m mask = ((1.e-4f) < pt2);
  error(mask) = sqrt(abs(error));
  error(!mask) = BIG;
  return !mask;
}


float_m KFParticleBaseSIMD::GetLifeTime( float_v &tauC, float_v &error ) const 
{
  /** Calculates the lifetime times speed of life (ctau) [cm] of the particle in the  
   ** center of mass frame and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** The production point should be set before calling this function.
   ** \param[out] ctau - lifetime of the particle [cm]
   ** \param[out] error - its error
   **/
  
  const float_v BIG = 1.e20f;

  float_v m, dm;
  GetMass( m, dm );
  float_v cTM = (-fP[3]*fC[31] - fP[4]*fC[32] - fP[5]*fC[33] + fP[6]*fC[34]);
  tauC = fP[7]*m;
  error = m*m*fC[35] + 2*fP[7]*cTM + fP[7]*fP[7]*dm*dm;
  float_m mask = (0.f < error);
  error(mask) = sqrt(error);
  error(!mask) = BIG;
  return !mask;
}


void KFParticleBaseSIMD::operator +=( const KFParticleBaseSIMD &Daughter )
{
  /** Operator to add daughter to the current particle. Calls AddDaughter() function.
   ** \param[in] Daughter - the daughter particle
   **/
  
  AddDaughter( Daughter );
}

void KFParticleBaseSIMD::GetMeasurement( const KFParticleBaseSIMD& daughter, float_v m[], float_v V[], float_v D[3][3] )
{
  /** Obtains the measurements from the current particle and the daughter to be added for the Kalman filter
   ** mathematics. If these are two first daughters they are transported to the point of the closest approach,
   ** if the third or higher daughter is added it is transported to the DCA point of the already constructed
   ** vertex. The correlations are taken into account in the covariance matrices of both measurements,
   ** the correlation matrix of two measurements is also calculated. Parameters of the current particle are
   ** modified by this function, the daughter is not changed, its parameters are stored to the output arrays
   ** after modifications.
   ** \param[in] daughter - the daughter particle to be added, stays unchanged
   ** \param[out] m[8] - the output parameters of the daughter particle at the DCA point
   ** \param[out] V[36] - the output covariance matrix of the daughter parameters, takes into account the correlation
   ** \param[out] D[3][3] - the correlation matrix between the current and daughter particles
   **/
  
  if(fNDF[0] == -1)
  {
    float_v ds[2] = {0.f,0.f};
    float_v dsdr[4][6];
    float_v F1[36], F2[36], F3[36], F4[36];
    for(int i1=0; i1<36; i1++)
    {
      F1[i1] = 0;
      F2[i1] = 0;
      F3[i1] = 0;
      F4[i1] = 0;
    }
    GetDStoParticle( daughter, ds, dsdr );
    
    float_v V0Tmp[36] ;
    float_v V1Tmp[36] ;

    float_v C[36];
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
    
    float_v C1F1T[6][6];
    for(int i=0; i<6; i++)
      for(int j=0; j<6; j++)
      {
        C1F1T[i][j] = 0;
        for(int k=0; k<6; k++)
        {
          C1F1T[i][j] +=  C[IJ(i,k)] * F1[j*6+k];
        }
      }
    float_v F3C1F1T[6][6];
    for(int i=0; i<6; i++)
      for(int j=0; j<6; j++)
      {
        F3C1F1T[i][j] = 0;
        for(int k=0; k<6; k++)
        {
          F3C1F1T[i][j] += F3[i*6+k] * C1F1T[k][j];
        }
      }
    float_v C2F2T[6][6];
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
    float_v dsdr[6];
    float_v dS = daughter.GetDStoPoint(fP, dsdr);
    
    float_v dsdp[6] = {-dsdr[0], -dsdr[1], -dsdr[2], 0.f, 0.f, 0.f};
    
    float_v F[36], F1[36];
    for(int i2=0; i2<36; i2++)
    {
      F[i2]  = 0;
      F1[i2] = 0;
    }
    daughter.Transport(dS, dsdr, m, V, dsdp, F, F1);
    
//     float_v V1Tmp[36] = {0.};
//     MultQSQt(F1, fC, V1Tmp, 6);
    
//     for(int iC=0; iC<21; iC++)
//       V[iC] += V1Tmp[iC];
    
    float_v VFT[3][6];
    for(int i=0; i<3; i++)
      for(int j=0; j<6; j++)
      {
        VFT[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          VFT[i][j] +=  fC[IJ(i,k)] * F1[j*6+k];
        }
      }
    
    float_v FVFT[6][6];
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
  }
}

void KFParticleBaseSIMD::AddDaughter( const KFParticleBaseSIMD &Daughter )
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
  
  AddDaughterId( Daughter.Id() );

  if( int(fNDF[0])<-1 ){ // first daughter -> just copy
    fNDF   = -1;
    fQ     =  Daughter.GetQ();
    for( Int_t i=0; i<7; i++ ) fP[i] = Daughter.fP[i];
    for( Int_t i=0; i<28; i++ ) fC[i] = Daughter.fC[i];
    fSFromDecay = 0;
    fMassHypo = Daughter.fMassHypo;
    SumDaughterMass = Daughter.SumDaughterMass;
    return;
  }

  if(fConstructMethod == 0)
    AddDaughterWithEnergyFit(Daughter);
  else if(fConstructMethod == 2)
    AddDaughterWithEnergyFitMC(Daughter);

  SumDaughterMass += Daughter.SumDaughterMass;
  fMassHypo = -1.f;
}

void KFParticleBaseSIMD::AddDaughterWithEnergyFit( const KFParticleBaseSIMD &Daughter )
{
  /** Adds daughter to the current particle. Uses simplifyed fast mathematics which consideres momentum 
   ** and energy as independent variables and thus ignores constraint on the fixed mass.
   ** In this case the mass of the daughter particle can be corrupted when the constructed vertex
   ** is added as the measurement and the mass of the output short-lived particle can become 
   ** unphysical - smaller then the threshold.
   ** \param[in] Daughter - the daughter particle
   **/

  Int_t maxIter = 1;

  for( Int_t iter=0; iter<maxIter; iter++ ){

    float_v m[8], mV[36];

    float_v D[3][3];
    GetMeasurement(Daughter, m, mV, D);

    float_v mS[6]= { fC[0]+mV[0], 
                     fC[1]+mV[1], fC[2]+mV[2], 
                     fC[3]+mV[3], fC[4]+mV[4], fC[5]+mV[5] };    
    InvertCholetsky3(mS);
    //* Residual (measured - estimated)
    
    float_v zeta[3] = { m[0]-fP[0], m[1]-fP[1], m[2]-fP[2] };    

    float_v K[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        K[i][j] = 0;
        for(int k=0; k<3; k++)
          K[i][j] += fC[IJ(i,k)] * mS[IJ(k,j)];
      }
    
    //* CHt = CH' - D'
    float_v mCHt0[7], mCHt1[7], mCHt2[7];

    mCHt0[0]=fC[ 0] ;       mCHt1[0]=fC[ 1] ;       mCHt2[0]=fC[ 3] ;
    mCHt0[1]=fC[ 1] ;       mCHt1[1]=fC[ 2] ;       mCHt2[1]=fC[ 4] ;
    mCHt0[2]=fC[ 3] ;       mCHt1[2]=fC[ 4] ;       mCHt2[2]=fC[ 5] ;
    mCHt0[3]=fC[ 6]-mV[ 6]; mCHt1[3]=fC[ 7]-mV[ 7]; mCHt2[3]=fC[ 8]-mV[ 8];
    mCHt0[4]=fC[10]-mV[10]; mCHt1[4]=fC[11]-mV[11]; mCHt2[4]=fC[12]-mV[12];
    mCHt0[5]=fC[15]-mV[15]; mCHt1[5]=fC[16]-mV[16]; mCHt2[5]=fC[17]-mV[17];
    mCHt0[6]=fC[21]-mV[21]; mCHt1[6]=fC[22]-mV[22]; mCHt2[6]=fC[23]-mV[23];
  
    //* Kalman gain K = mCH'*S
    
    float_v k0[7], k1[7], k2[7];
    
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

    float_v K2[3][3];
    for(int i=0; i<3; i++)
    {
      for(int j=0; j<3; j++)
        K2[i][j] = -K[j][i];
      K2[i][i] += 1;
    }

    float_v A[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        A[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          A[i][j] += D[i][k] * K2[k][j];
        }
      }
    
    float_v M[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        M[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          M[i][j] += K[i][k] * A[k][j];
        }
      }

    fC[0] += 2.f*M[0][0];
    fC[1] += M[0][1] + M[1][0];
    fC[2] += 2.f*M[1][1];
    fC[3] += M[0][2] + M[2][0];
    fC[4] += M[1][2] + M[2][1];
    fC[5] += 2.f*M[2][2];
  
    //* Calculate Chi^2 

    fNDF  += 2;
    fQ    +=  Daughter.GetQ();
    fSFromDecay = 0;    
    fChi2 += (mS[0]*zeta[0] + mS[1]*zeta[1] + mS[3]*zeta[2])*zeta[0]
      +      (mS[1]*zeta[0] + mS[2]*zeta[1] + mS[4]*zeta[2])*zeta[1]
      +      (mS[3]*zeta[0] + mS[4]*zeta[1] + mS[5]*zeta[2])*zeta[2];
  }
}

void KFParticleBaseSIMD::AddDaughterWithEnergyFitMC( const KFParticleBaseSIMD &Daughter )
{
  /** Adds daughter to the current particle. Uses slower but correct mathematics 
   ** which requires that the masses of daughter particles 
   ** stays fixed in the construction process.
   ** \param[in] Daughter - the daughter particle
   **/

  Int_t maxIter = 1;

  for( Int_t iter=0; iter<maxIter; iter++ ){

    float_v m[8], mV[36];

    float_v D[3][3];
    GetMeasurement(Daughter, m, mV, D);
    
    float_v mS[6]= { fC[0]+mV[0], 
                   fC[1]+mV[1], fC[2]+mV[2], 
                   fC[3]+mV[3], fC[4]+mV[4], fC[5]+mV[5] };
    InvertCholetsky3(mS);
    //* Residual (measured - estimated)

    float_v zeta[3] = { m[0]-fP[0], m[1]-fP[1], m[2]-fP[2] };    

    float_v K[3][6];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        K[i][j] = 0;
        for(int k=0; k<3; k++)
          K[i][j] += fC[IJ(i,k)] * mS[IJ(k,j)];
      }

    
    //* CHt = CH'
    
    float_v mCHt0[7], mCHt1[7], mCHt2[7];
    
    mCHt0[0]=fC[ 0] ; mCHt1[0]=fC[ 1] ; mCHt2[0]=fC[ 3] ;
    mCHt0[1]=fC[ 1] ; mCHt1[1]=fC[ 2] ; mCHt2[1]=fC[ 4] ;
    mCHt0[2]=fC[ 3] ; mCHt1[2]=fC[ 4] ; mCHt2[2]=fC[ 5] ;
    mCHt0[3]=fC[ 6] ; mCHt1[3]=fC[ 7] ; mCHt2[3]=fC[ 8] ;
    mCHt0[4]=fC[10] ; mCHt1[4]=fC[11] ; mCHt2[4]=fC[12] ;
    mCHt0[5]=fC[15] ; mCHt1[5]=fC[16] ; mCHt2[5]=fC[17] ;
    mCHt0[6]=fC[21] ; mCHt1[6]=fC[22] ; mCHt2[6]=fC[23] ;
  
    //* Kalman gain K = mCH'*S
    
    float_v k0[7], k1[7], k2[7];
    
    for(Int_t i=0;i<7;++i){
      k0[i] = mCHt0[i]*mS[0] + mCHt1[i]*mS[1] + mCHt2[i]*mS[3];
      k1[i] = mCHt0[i]*mS[1] + mCHt1[i]*mS[2] + mCHt2[i]*mS[4];
      k2[i] = mCHt0[i]*mS[3] + mCHt1[i]*mS[4] + mCHt2[i]*mS[5];
    }

    // last itearation -> update the particle

    //* VHt = VH'
    
    float_v mVHt0[7], mVHt1[7], mVHt2[7];
    
    mVHt0[0]=mV[ 0] ; mVHt1[0]=mV[ 1] ; mVHt2[0]=mV[ 3] ;
    mVHt0[1]=mV[ 1] ; mVHt1[1]=mV[ 2] ; mVHt2[1]=mV[ 4] ;
    mVHt0[2]=mV[ 3] ; mVHt1[2]=mV[ 4] ; mVHt2[2]=mV[ 5] ;
    mVHt0[3]=mV[ 6] ; mVHt1[3]=mV[ 7] ; mVHt2[3]=mV[ 8] ;
    mVHt0[4]=mV[10] ; mVHt1[4]=mV[11] ; mVHt2[4]=mV[12] ;
    mVHt0[5]=mV[15] ; mVHt1[5]=mV[16] ; mVHt2[5]=mV[17] ;
    mVHt0[6]=mV[21] ; mVHt1[6]=mV[22] ; mVHt2[6]=mV[23] ;
  
    //* Kalman gain Km = mCH'*S
    
    float_v km0[7], km1[7], km2[7];
    
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

    float_v mDf[7][7];

    for(Int_t i=0;i<7;++i){
      for(Int_t j=0;j<7;++j){
        mDf[i][j] = (km0[i]*mCHt0[j] + km1[i]*mCHt1[j] + km2[i]*mCHt2[j] );
      }
    }

    float_v mJ1[7][7], mJ2[7][7];
    for(Int_t iPar1=0; iPar1<7; iPar1++)
    {
      for(Int_t iPar2=0; iPar2<7; iPar2++)
      {
        mJ1[iPar1][iPar2] = 0;
        mJ2[iPar1][iPar2] = 0;
      }
    }

    float_v mMassParticle  = fP[6]*fP[6] - (fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5]);
    float_v mMassDaughter  = m[6]*m[6] - (m[3]*m[3] + m[4]*m[4] + m[5]*m[5]);
    mMassParticle(mMassParticle > 0.f) = sqrt(mMassParticle);
    mMassParticle(mMassParticle <= 0.f) = 0.f;
    mMassDaughter(mMassDaughter > 0.f) = sqrt(mMassDaughter);
    mMassDaughter(mMassDaughter <= 0.f) = 0.f;

    float_m mask1 = fMassHypo > -0.5f;
    float_m mask2 = (!mask1) && ( (mMassParticle < SumDaughterMass) || (fP[6]<0.f)) ;
    SetMassConstraint(fP,fC,mJ1,fMassHypo, mask1);
    SetMassConstraint(fP,fC,mJ1,SumDaughterMass, mask2);

    float_m mask3 = Daughter.fMassHypo > -0.5f;
    float_m mask4 = ( (!mask3) && ( (mMassDaughter<Daughter.SumDaughterMass) || (m[6]<0.f)) );
    SetMassConstraint(m,mV,mJ2,Daughter.fMassHypo, mask3);
    SetMassConstraint(m,mV,mJ2,Daughter.SumDaughterMass, mask4);

    float_v mDJ[7][7];

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

    
    float_v K2[3][3];
    for(int i=0; i<3; i++)
    {
      for(int j=0; j<3; j++)
        K2[i][j] = -K[j][i];
      K2[i][i] += 1;
    }

    float_v A[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        A[i][j] = 0.f;
        for(int k=0; k<3; k++)
        {
          A[i][j] += D[i][k] * K2[k][j];
        }
      }
    
    float_v M[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        M[i][j] = 0.f;
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

void KFParticleBaseSIMD::SubtractDaughter( const KFParticleBaseSIMD &Daughter )
{
  /** Subtracts a daughter particle from the mother particle. The mathematics is
   ** similar to AddDaughterWithEnergyFit() but momentum is subtracted.
   ** \param[in] Daughter - the daughter particle
   **/
  
  AddDaughterId( Daughter.Id() );

  float_v m[8], mV[36];

  float_v D[3][3];
  GetMeasurement(Daughter, m, mV, D);
    
  float_v mS[6]= { fC[0]+mV[0], 
                     fC[1]+mV[1], fC[2]+mV[2], 
                     fC[3]+mV[3], fC[4]+mV[4], fC[5]+mV[5] };    
    InvertCholetsky3(mS);
    //* Residual (measured - estimated)
    
    float_v zeta[3] = { m[0]-fP[0], m[1]-fP[1], m[2]-fP[2] };    

    float_v K[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        K[i][j] = 0;
        for(int k=0; k<3; k++)
          K[i][j] += fC[IJ(i,k)] * mS[IJ(k,j)];
      }
    
    //* CHt = CH' - D'
    float_v mCHt0[7], mCHt1[7], mCHt2[7];

    mCHt0[0]=fC[ 0] ;       mCHt1[0]=fC[ 1] ;       mCHt2[0]=fC[ 3] ;
    mCHt0[1]=fC[ 1] ;       mCHt1[1]=fC[ 2] ;       mCHt2[1]=fC[ 4] ;
    mCHt0[2]=fC[ 3] ;       mCHt1[2]=fC[ 4] ;       mCHt2[2]=fC[ 5] ;
    mCHt0[3]=fC[ 6]+mV[ 6]; mCHt1[3]=fC[ 7]+mV[ 7]; mCHt2[3]=fC[ 8]+mV[ 8];
    mCHt0[4]=fC[10]+mV[10]; mCHt1[4]=fC[11]+mV[11]; mCHt2[4]=fC[12]+mV[12];
    mCHt0[5]=fC[15]+mV[15]; mCHt1[5]=fC[16]+mV[16]; mCHt2[5]=fC[17]+mV[17];
    mCHt0[6]=fC[21]+mV[21]; mCHt1[6]=fC[22]+mV[22]; mCHt2[6]=fC[23]+mV[23];
  
    //* Kalman gain K = mCH'*S
    
    float_v k0[7], k1[7], k2[7];
    
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

    float_v K2[3][3];
    for(int i=0; i<3; i++)
    {
      for(int j=0; j<3; j++)
        K2[i][j] = -K[j][i];
      K2[i][i] += 1;
    }

    float_v A[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        A[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          A[i][j] += D[i][k] * K2[k][j];
        }
      }
    
    float_v M[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        M[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          M[i][j] += K[i][k] * A[k][j];
        }
      }

    fC[0] += 2.f*M[0][0];
    fC[1] += M[0][1] + M[1][0];
    fC[2] += 2.f*M[1][1];
    fC[3] += M[0][2] + M[2][0];
    fC[4] += M[1][2] + M[2][1];
    fC[5] += 2.f*M[2][2];
  
    //* Calculate Chi^2 

    fNDF  += 2;
    fQ    -=  Daughter.GetQ();
    fSFromDecay = 0;    
    fChi2 += (mS[0]*zeta[0] + mS[1]*zeta[1] + mS[3]*zeta[2])*zeta[0]
      +      (mS[1]*zeta[0] + mS[2]*zeta[1] + mS[4]*zeta[2])*zeta[1]
      +      (mS[3]*zeta[0] + mS[4]*zeta[1] + mS[5]*zeta[2])*zeta[2];
}

void KFParticleBaseSIMD::SetProductionVertex( const KFParticleBaseSIMD &Vtx )
{
  /** Adds a vertex as a point-like measurement to the current particle.
   ** The eights parameter of the state vector is filled with the decay
   ** length to the momentum ratio (s = l/p). The corresponding covariances
   ** are calculated as well. The parameters of the particle are stored
   ** at the position of the production vertex.
   ** \param[in] Vtx - the assumed producation vertex
   **/
  
  const float_v *m = Vtx.fP, *mV = Vtx.fC;

  float_v decayPoint[3] = {fP[0], fP[1], fP[2]};
  float_v decayPointCov[6] = { fC[0], fC[1], fC[2], fC[3], fC[4], fC[5] };

  float_v D[6][6];
  for(int iD1=0; iD1<6; iD1++)
    for(int iD2=0; iD2<6; iD2++)
      D[iD1][iD2] = 0.f;

  Bool_t noS = ( fC[35][0]<=0.f ); // no decay length allowed

  if( noS ){ 
    TransportToDecayVertex();
    fP[7] = 0.f;
    fC[28] = fC[29] = fC[30] = fC[31] = fC[32] = fC[33] = fC[34] = fC[35] = 0.f;
  }
  else
  {
    float_v dsdr[6] = {0.f, 0.f, 0.f, 0.f, 0.f, 0.f};
    float_v dS = GetDStoPoint(Vtx.fP, dsdr);
    
    float_v dsdp[6] = {-dsdr[0], -dsdr[1], -dsdr[2], 0.f, 0.f, 0.f };
    
    float_v F[36], F1[36];
    for(int i2=0; i2<36; i2++)
    {
      F[i2]  = 0.f;
      F1[i2] = 0.f;
    }
    Transport( dS, dsdr, fP, fC, dsdp, F, F1 );
    
    float_v CTmp[36];
    MultQSQt(F1, mV, CTmp, 6);
    
    for(int iC=0; iC<21; iC++)
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

  float_v mS[6] = { fC[0] + mV[0],
                    fC[1] + mV[1], fC[2] + mV[2],
                    fC[3] + mV[3], fC[4] + mV[4], fC[5] + mV[5] };                 
  InvertCholetsky3(mS);
  
  float_v res[3] = { m[0] - X(), m[1] - Y(), m[2] - Z() };
  
  float_v K[3][6];  
  for(int i=0; i<3; i++)
    for(int j=0; j<3; j++)
    {
      K[i][j] = 0;
      for(int k=0; k<3; k++)
        K[i][j] += fC[IJ(i,k)] * mS[IJ(k,j)];
    }
  
  float_v mCHt0[7], mCHt1[7], mCHt2[7];
  mCHt0[0]=fC[ 0];        mCHt1[0]=fC[ 1];        mCHt2[0]=fC[ 3];
  mCHt0[1]=fC[ 1];        mCHt1[1]=fC[ 2];        mCHt2[1]=fC[ 4];
  mCHt0[2]=fC[ 3];        mCHt1[2]=fC[ 4];        mCHt2[2]=fC[ 5];
  mCHt0[3]=fC[ 6];        mCHt1[3]=fC[ 7];        mCHt2[3]=fC[ 8];
  mCHt0[4]=fC[10];        mCHt1[4]=fC[11];        mCHt2[4]=fC[12];
  mCHt0[5]=fC[15];        mCHt1[5]=fC[16];        mCHt2[5]=fC[17];
  mCHt0[6]=fC[21];        mCHt1[6]=fC[22];        mCHt2[6]=fC[23];
  
  float_v k0[7], k1[7], k2[7];
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

  float_v K2[3][3];
  for(int i=0; i<3; i++)
  {
    for(int j=0; j<3; j++)
      K2[i][j] = -K[j][i];
    K2[i][i] += 1;
  }

  float_v A[3][3];
  for(int i=0; i<3; i++)
    for(int j=0; j<3; j++)
    {
      A[i][j] = 0;
      for(int k=0; k<3; k++)
      {
        A[i][j] += D[k][i] * K2[k][j];
      }
    }
  
  float_v M[3][3];
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
    fC[28] = fC[29] = fC[30] = fC[31] = fC[32] = fC[33] = fC[34] = fC[35] = 0.f;
    fSFromDecay = 0;
  }
  else
  {
    float_v dsdr[6] = {0.f, 0.f, 0.f, 0.f, 0.f, 0.f};
    fP[7] = GetDStoPoint(decayPoint, dsdr);   
    
    float_v dsdp[6] = {-dsdr[0], -dsdr[1], -dsdr[2], 0.f, 0.f, 0.f};

    float_v F[36], F1[36];
    for(int i2=0; i2<36; i2++)
    {
      F[i2]  = 0.f;
      F1[i2] = 0.f;
    }
    float_v tmpP[8], tmpC[36]; 
    Transport( fP[7], dsdr, tmpP, tmpC, dsdp, F, F1 );
          
    fC[35] = 0.f;
    for(int iDsDr=0; iDsDr<6; iDsDr++)
    {
      float_v dsdrC = 0.f, dsdpV = 0.f;
      
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

void KFParticleBaseSIMD::SetMassConstraint( float_v *mP, float_v *mC, float_v mJ[7][7], float_v mass, float_m mask )
{
  /** Sets the exact nonlinear mass constraint on the state vector mP with the covariance matrix mC.
   ** \param[in,out] mP - the state vector to be modified
   ** \param[in,out] mC - the corresponding covariance matrix
   ** \param[in,out] mJ - the Jacobian between initial and modified parameters
   ** \param[in] mass - the mass to be set on the state vector mP
   ** \param[in] mask - mask defines entries of the SIMD vector, for which the constraint should be applied
   **/
  
  const float_v energy2 = mP[6]*mP[6], p2 = mP[3]*mP[3]+mP[4]*mP[4]+mP[5]*mP[5], mass2 = mass*mass;
  
  const float_v a = energy2 - p2 + 2.f*mass2;
  const float_v b = -2.f*(energy2 + p2);
  const float_v c = energy2 - p2 - mass2;

  float_v lambda(Vc::Zero);
  lambda(abs(b) > float_v(1.e-10f)) = -c/b ;

  float_v d = 4.f*energy2*p2 - mass2*(energy2-p2-2.f*mass2);
  float_m qMask = (d >= 0.f) && (abs(a) > (1.e-10f)) ;
  lambda(qMask) = (energy2 + p2 - sqrt(d))/a;

  lambda(mP[6]<0.f) = -1000000.f;

  Int_t iIter=0;
  for(iIter=0; iIter<100; iIter++)
  {
    float_v lambda2 = lambda*lambda;
    float_v lambda4 = lambda2*lambda2;

//    float_v lambda0 = lambda;

    float_v f  = -mass2 * lambda4 + a*lambda2 + b*lambda + c;
    float_v df = -4.f*mass2 * lambda2*lambda + 2.f*a*lambda + b;
    lambda(abs(df) > float_v(1.e-10f)) -= f/df;
//    if(TMath::Abs(lambda0 - lambda) < 1.e-8) break;
  }

  const float_v lpi = 1.f/(1.f + lambda);
  const float_v lmi = 1.f/(1.f - lambda);
  const float_v lp2i = lpi*lpi;
  const float_v lm2i = lmi*lmi;

  float_v lambda2 = lambda*lambda;

  float_v dfl  = -4.f*mass2 * lambda2*lambda + 2.f*a*lambda + b;
  float_v dfx[4] = {0.f,0.f,0.f,0.f};
  dfx[0] = -2.f*(1.f + lambda)*(1.f + lambda)*mP[3];
  dfx[1] = -2.f*(1.f + lambda)*(1.f + lambda)*mP[4];
  dfx[2] = -2.f*(1.f + lambda)*(1.f + lambda)*mP[5];
  dfx[3] = 2.f*(1.f - lambda)*(1.f - lambda)*mP[6];
  float_v dlx[4] = {1.f,1.f,1.f,1.f};

  for(int i=0; i<4; i++)
    dlx[i](abs(dfl) > float_v(1.e-10f)) = -dfx[i] / dfl;

  float_v dxx[4] = {mP[3]*lm2i, mP[4]*lm2i, mP[5]*lm2i, -mP[6]*lp2i};

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

  float_v mCJ[7][7];

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
      mC[IJ(i,j)](mask) = 0.f;
      for(Int_t l=0; l<7; l++){
        mC[IJ(i,j)](mask) += mJ[i][l]*mCJ[l][j];
      }
    }
  }

  mP[3](mask) *= lmi;
  mP[4](mask) *= lmi;
  mP[5](mask) *= lmi;
  mP[6](mask) *= lpi;
}

void KFParticleBaseSIMD::SetNonlinearMassConstraint( float_v mass )
{
  /** Sets the exact nonlinear mass constraint on the current particle.
   ** \param[in] mass - the mass to be set on the particle
   **/
  
  const float_v& px = fP[3];
  const float_v& py = fP[4];
  const float_v& pz = fP[5];
  const float_v& energy  = fP[6];
  
  const float_v residual = (energy*energy - px*px - py*py - pz*pz) - mass*mass;
  const float_v dm2 = float_v(4.f) * ( fC[9]*px*px + fC[14]*py*py + fC[20]*pz*pz + fC[27]*energy*energy +
                      float_v(2.f) * ( (fC[13]*py + fC[18]*pz - fC[24]*energy)*px + (fC[19]*pz - fC[25]*energy)*py - fC[26]*pz*energy) );
  const float_v dChi2 = residual*residual / dm2;
  fChi2 += dChi2;
  fNDF  += 1;
  
  float_v mJ[7][7];
  SetMassConstraint( fP, fC, mJ, mass, float_m(true) );
  fMassHypo = mass;
  SumDaughterMass = mass;
}

void KFParticleBaseSIMD::SetMassConstraint( float_v Mass, float_v SigmaMass )
{  
  /** Sets linearised mass constraint on the current particle. The constraint can be set with
   ** an uncertainty.
   ** \param[in] Mass - the mass to be set on the state vector mP
   ** \param[in] SigmaMass - uncertainty of the constraint
   **/
  
  fMassHypo = Mass;
  SumDaughterMass = Mass;

  float_v m2 = Mass*Mass;            // measurement, weighted by Mass 
  float_v s2 = m2*SigmaMass*SigmaMass; // sigma^2

  float_v p2 = fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5]; 
  float_v e0 = sqrt(m2+p2);

  float_v mH[8];
  mH[0] = mH[1] = mH[2] = 0.f;
  mH[3] = -2.f*fP[3]; 
  mH[4] = -2.f*fP[4]; 
  mH[5] = -2.f*fP[5]; 
  mH[6] =  2.f*fP[6];//e0;
  mH[7] = 0.f; 

  float_v zeta = e0*e0 - e0*fP[6];
  zeta = m2 - (fP[6]*fP[6]-p2);
  
  float_v mCHt[8], s2_est=0.f;
  for( Int_t i=0; i<8; ++i ){
    mCHt[i] = 0.0f;
    for (Int_t j=0;j<8;++j) mCHt[i] += Cij(i,j)*mH[j];
    s2_est += mH[i]*mCHt[i];
  }
  
//TODO add protection
//  if( s2_est<1.e-20 ) return; // calculated mass error is already 0, 
                              // the particle can not be constrained on mass

  float_v w2 = 1.f/( s2 + s2_est );
  fChi2 += zeta*zeta*w2;
  fNDF  += 1;
  for( Int_t i=0, ii=0; i<8; ++i ){
    float_v ki = mCHt[i]*w2;
    fP[i]+= ki*zeta;
    for(Int_t j=0;j<=i;++j) fC[ii++] -= ki*mCHt[j];    
  }
}


void KFParticleBaseSIMD::SetNoDecayLength()
{  
  /** Sets constraint on the zero decay length. When the production point is set 
   ** the measurement from this particle is created at the decay point.
   **/
  
  TransportToDecayVertex();

  float_v h[8];
  h[0] = h[1] = h[2] = h[3] = h[4] = h[5] = h[6] = 0.f;
  h[7] = 1.f; 

  float_v zeta = 0.f - fP[7];
  for(Int_t i=0;i<8;++i) zeta -= h[i]*(fP[i]-fP[i]);
  
  float_v s = fC[35];   
//  if( s>1.e-20 ) //TODO add protection
  {
    s = 1.f/s;
    fChi2 += zeta*zeta*s;
    fNDF  += 1;
    for( Int_t i=0, ii=0; i<7; ++i ){
      float_v ki = fC[28+i]*s;
      fP[i]+= ki*zeta;
      for(Int_t j=0;j<=i;++j) fC[ii++] -= ki*fC[28+j];    
    }
  }
  fP[7] = 0.f;
  fC[28] = fC[29] = fC[30] = fC[31] = fC[32] = fC[33] = fC[34] = fC[35] = 0.f;
}

void KFParticleBaseSIMD::Construct( const KFParticleBaseSIMD* vDaughters[], Int_t nDaughters, 
                                    const KFParticleBaseSIMD *Parent,  Float_t Mass )
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
  
  const int maxIter = 1;
  for( Int_t iter=0; iter<maxIter; iter++ ){
    
    CleanDaughtersId();
    SetNDaughters(nDaughters);
    
    fAtProductionVertex = 0;
    fSFromDecay = float_v(Vc::Zero);
    SumDaughterMass = float_v(Vc::Zero);

    for(Int_t i=0;i<36;++i) fC[i]=0.;
    fC[35] = 1.;
    
    fNDF  = -3;
    fChi2 =  0.;
    fQ = 0;

    for( Int_t itr =0; itr<nDaughters; itr++ ){
      AddDaughter( *vDaughters[itr] );    
    }
  }

  if( Mass>=0 ) SetMassConstraint( Mass );
  if( Parent ) SetProductionVertex( *Parent );
}

void KFParticleBaseSIMD::TransportToDecayVertex()
{
  /** Transports the particle to its decay vertex */ 
  float_v dsdr[6] = {float_v(Vc::Zero),float_v(Vc::Zero),float_v(Vc::Zero),float_v(Vc::Zero),float_v(Vc::Zero),float_v(Vc::Zero)};
  if( !( (abs(fSFromDecay) < float_v(1.e-6f)).isFull() ) ) TransportToDS( -fSFromDecay, dsdr );
  fAtProductionVertex = 0;
}

void KFParticleBaseSIMD::TransportToProductionVertex()
{
  /** Transports the particle to its production vertex */
  float_v dsdr[6] = {float_v(Vc::Zero),float_v(Vc::Zero),float_v(Vc::Zero),float_v(Vc::Zero),float_v(Vc::Zero),float_v(Vc::Zero)}; 
  if( !( (abs(fSFromDecay + fP[7]) < float_v(1.e-6f)).isFull() )  ) TransportToDS( -fSFromDecay-fP[7], dsdr );
  fAtProductionVertex = 1;
}


void KFParticleBaseSIMD::TransportToDS( float_v dS, const float_v* dsdr )
{ 
  /** Transport the particle on a certain distane. The distance is defined by the dS=l/p parameter, where \n
   ** 1) l - signed distance;\n
   ** 2) p - momentum of the particle. \n
   ** \param[in] dS = l/p - distance normalised to the momentum of the particle to be transported on
   ** \param[in] dsdr[6] = ds/dr partial derivatives of the parameter dS over the state vector of the current particle
   **/
  
  Transport( dS, dsdr, fP, fC );
  fSFromDecay+= dS;
}

void KFParticleBaseSIMD::TransportToDSLine( float_v dS, const float_v* dsdr )
{ 
  /** Transport the particle on a certain distane assuming the linear trajectory. 
   ** The distance is defined by the dS=l/p parameter, where \n
   ** 1) l - signed distance;\n
   ** 2) p - momentum of the particle. \n
   ** \param[in] dS = l/p - distance normalised to the momentum of the particle to be transported on
   ** \param[in] dsdr[6] = ds/dr partial derivatives of the parameter dS over the state vector of the current particle
   **/
  
  TransportLine( dS, dsdr, fP, fC );
  fSFromDecay+= dS;
}

void KFParticleBaseSIMD::GetDistanceToVertexLine( const KFParticleBaseSIMD &Vertex, float_v &l, float_v &dl, float_m *isParticleFromVertex ) const 
{
  /** Calculates the distance between the particle position and the vertex together with the error.
   ** Errors of both particle and vertex are taken into account. Also optionally checks if partcile
   ** is pointing flying from the vertex, not in the direction to the vertex if the pointer to the
   ** mask isParticleFromVertex is provided.
   ** \param[in] Vertex - vertex to which the distance should be calculated
   ** \param[out] l - distance between the current position of the particle and a vertex
   ** \param[out] dl - the error of the calculated distance
   ** \param[out] isParticleFromVertex - mask which shows if particle is flying in the direction from the vertex
   **/

  float_v c[6] = {Vertex.fC[0]+fC[0], Vertex.fC[1]+fC[1], Vertex.fC[2]+fC[2],
               Vertex.fC[3]+fC[3], Vertex.fC[4]+fC[4], Vertex.fC[5]+fC[5]};

  float_v dx = (Vertex.fP[0]-fP[0]);
  float_v dy = (Vertex.fP[1]-fP[1]);
  float_v dz = (Vertex.fP[2]-fP[2]);

  l = sqrt( dx*dx + dy*dy + dz*dz );
  dl = c[0]*dx*dx + c[2]*dy*dy + c[5]*dz*dz + 2*(c[1]*dx*dy + c[3]*dx*dz + c[4]*dy*dz);

  l(abs(l) < 1.e-8f) = 1.e-8f;
  float_m ok = float_v(Vc::Zero)<=dl;
  dl(!ok) = 1.e8f;
  dl(ok) = sqrt( dl )/l;

  if(isParticleFromVertex)
  {
    *isParticleFromVertex = ok && ( l<float_v(3.f*dl) );
    float_v cosV = dx*fP[3] + dy*fP[4] + dz*fP[5];
//     float_v dCos = dy*dy*fC[14] + dz*dz*fC[20] + dx*dx*fC[9] + 2*dz*fC[15]*fP[3] + c[0]* fP[3]*fP[3] + 
//             2*dz*fC[16]* fP[4] + 2 *c[1] *fP[3] *fP[4] + c[2] *fP[4]*fP[4] + 2 *dz *fC[17]* fP[5] + 
//             2*c[3] *fP[3]* fP[5] + 2 *c[4] *fP[4] *fP[5] + c[5]*fP[5] *fP[5] + 
//             2*dy *(dz *fC[19] + fC[10] *fP[3] + fC[11]* fP[4] + fC[12]* fP[5]) + 
//             2*dx *(dy *fC[13] + dz *fC[18] + fC[6]* fP[3] + fC[7]* fP[4] + fC[8]* fP[5]);
//     ok = float_v(float_v(0)<dCos);
//     dCos = float_v(ok & ( dCos ));
//     dCos = sqrt(dCos);
    *isParticleFromVertex = (*isParticleFromVertex) || (!(*isParticleFromVertex) && (cosV<0.f) ) ;
  }
}

float_v KFParticleBaseSIMD::GetDStoPointLine( const float_v xyz[3], float_v dsdr[6] ) const 
{
  /** Returns dS = l/p parameter, where \n
   ** 1) l - signed distance to the DCA point with the input xyz point;\n
   ** 2) p - momentum of the particle; \n
   ** assuming the straigth line trajectory. Is used for particles with charge 0 or in case of zero magnetic field.
   ** Also calculates partial derivatives dsdr of the parameter dS over the state vector of the current particle.
   ** \param[in] xyz[3] - point where particle should be transported
   ** \param[out] dsdr[6] = ds/dr partial derivatives of the parameter dS over the state vector of the current particle
   **/
  
  float_v p2 = fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5];  
  p2( p2 < float_v(1.e-4f) )  = float_v(1.f);
  
  const float_v& a = fP[3]*(xyz[0]-fP[0]) + fP[4]*(xyz[1]-fP[1]) + fP[5]*(xyz[2]-fP[2]);
  dsdr[0] = -fP[3]/p2;
  dsdr[1] = -fP[4]/p2;
  dsdr[2] = -fP[5]/p2;
  dsdr[3] = ((xyz[0]-fP[0])*p2 - 2.f* fP[3]*a)/(p2*p2);
  dsdr[4] = ((xyz[1]-fP[1])*p2 - 2.f* fP[4]*a)/(p2*p2);
  dsdr[5] = ((xyz[2]-fP[2])*p2 - 2.f* fP[5]*a)/(p2*p2);
  
  return a/p2;
}

float_v KFParticleBaseSIMD::GetDStoPointBz( float_v B, const float_v xyz[3], float_v dsdr[6], const float_v* param) const
{ 
  /** Returns dS = l/p parameter, where \n
   ** 1) l - signed distance to the DCA point with the input xyz point;\n
   ** 2) p - momentum of the particle; \n
   ** under the assumption of the constant homogeneous field Bz.
   ** Also calculates partial derivatives dsdr of the parameter dS over the state vector of the current particle.
   ** \param[in] B - magnetic field Bz
   ** \param[in] xyz[3] - point, to which particle should be transported
   ** \param[out] dsdr[6] = ds/dr partial derivatives of the parameter dS over the state vector of the current particle
   ** \param[in] param - optional parameter, is used in case if the parameters of the particle are rotated
   ** to other coordinate system (see GetDStoPointBy() function), otherwise fP are used
   **/

  if(!param)
    param = fP;
  //* Get dS to a certain space point for Bz field
  
  const float_v& x  = param[0];
  const float_v& y  = param[1];
  const float_v& z  = param[2];
  const float_v& px = param[3];
  const float_v& py = param[4];
  const float_v& pz = param[5];
  
  const float_v kCLight = 0.000299792458f;
  float_v bq = B*simd_cast<float_v>(fQ)*kCLight;
  float_v pt2 = param[3]*param[3] + param[4]*param[4];
  float_v p2 = pt2 + param[5]*param[5];  
  
  float_v dx = xyz[0] - param[0];
  float_v dy = xyz[1] - param[1]; 
  float_v dz = xyz[2] - param[2]; 
  float_v a = dx*param[3]+dy*param[4];
  float_v dS(Vc::Zero);
  
  float_v abq = bq*a;

  const float_v LocalSmall = 1.e-8f;
  float_m mask = ( abs(bq)<LocalSmall );
  if( !( (!mask).isFull() ) )
  {
    dS(mask && float_m(p2>1.e-4f)) = (a + dz*pz)/p2;
    
    dsdr[0](mask && float_m(p2>1.e-4f)) = -px/p2;
    dsdr[1](mask && float_m(p2>1.e-4f)) = -py/p2;
    dsdr[2](mask && float_m(p2>1.e-4f)) = -pz/p2;
    dsdr[3](mask && float_m(p2>1.e-4f)) = (dx*p2 - 2.f* px *(a + dz *pz))/(p2*p2);
    dsdr[4](mask && float_m(p2>1.e-4f)) = (dy*p2 - 2.f* py *(a + dz *pz))/(p2*p2);
    dsdr[5](mask && float_m(p2>1.e-4f)) = (dz*p2 - 2.f* pz *(a + dz *pz))/(p2*p2);
    
    if(mask.isFull())
      return dS;
  }
  
  dS(!mask) = KFPMath::ATan2( abq, pt2 + bq*(dy*px -dx*py) )/bq;

  float_v bs= bq*dS;

  float_v s = sin(bs), c = cos(bs);

  bq(abs(bq) < LocalSmall) = LocalSmall;
  float_v bbq = bq*(dx*py - dy*px) - pt2;
  
  dsdr[0](!mask) = (px*bbq - py*abq)/(abq*abq + bbq*bbq);
  dsdr[1](!mask) = (px*abq + py*bbq)/(abq*abq + bbq*bbq);
  dsdr[2](!mask) = 0;
  dsdr[3](!mask) = -(dx*bbq + dy*abq + 2.f*px*a)/(abq*abq + bbq*bbq);
  dsdr[4](!mask) = (dx*abq - dy*bbq - 2.f*py*a)/(abq*abq + bbq*bbq);
  dsdr[5](!mask) = 0;
  
  float_v sz(Vc::Zero);
  float_v cCoeff = (bbq*c - abq*s) - pz*pz ;
  sz(abs(cCoeff) > 1.e-8f) = (dS*pz - dz)*pz / cCoeff;
  
  float_v dcdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  dcdr[0] = -bq*py*c - bbq*s*bq*dsdr[0] + px*bq*s - abq*c*bq*dsdr[0];
  dcdr[1] =  bq*px*c - bbq*s*bq*dsdr[1] + py*bq*s - abq*c*bq*dsdr[1];
  dcdr[3] = (-bq*dy-2*px)*c - bbq*s*bq*dsdr[3] - dx*bq*s - abq*c*bq*dsdr[3];
  dcdr[4] = ( bq*dx-2*py)*c - bbq*s*bq*dsdr[4] - dy*bq*s - abq*c*bq*dsdr[4];
  dcdr[5] = -2*pz;
  
  for(int iP=0; iP<6; iP++)
    dsdr[iP](!mask) += pz*pz/cCoeff*dsdr[iP] - sz/cCoeff*dcdr[iP];

  dsdr[2](!mask) += pz/cCoeff;
  dsdr[5](!mask) += (2.f*pz*dS - dz)/cCoeff;
  
  dS(!mask) += sz;
  
  bs= bq*dS;
  s = sin(bs);
  c = cos(bs);
  
  float_v sB, cB;
  const float_v kOvSqr6 = 1.f/sqrt(float_v(6.f));

  sB(LocalSmall < abs(bs)) = s/bq;
  sB(LocalSmall >= abs(bs)) = (1.f-bs*kOvSqr6)*(1.f+bs*kOvSqr6)*dS;
  cB(LocalSmall < abs(bs)) = (1.f-c)/bq;
  cB(LocalSmall >= abs(bs)) = .5f*sB*bs;

  float_v p[5];
  p[0] = x + sB*px + cB*py;
  p[1] = y - cB*px + sB*py;
  p[2] = z +  dS*pz;
  p[3] =      c*px + s*py;
  p[4] =     -s*px + c*py;

  dx = xyz[0] - p[0];
  dy = xyz[1] - p[1];
  dz = xyz[2] - p[2];
  a = dx*p[3]+dy*p[4] + dz*pz;

  abq = bq*a;

  dS(!mask) += KFPMath::ATan2( abq, p2 + bq*(dy*p[3] -dx*p[4]) )/bq;
  
  return dS;
}

float_v KFParticleBaseSIMD::GetDStoPointBy( float_v By, const float_v xyz[3], float_v dsdr[6] ) 
  const
{ 
  /** Returns dS = l/p parameter, where \n
   ** 1) l - signed distance to the DCA point with the input xyz point;\n
   ** 2) p - momentum of the particle; \n
   ** under the assumption of the constant homogeneous field By.
   ** Also calculates partial derivatives dsdr of the parameter dS over the state vector of the current particle.
   ** The particle parameters are transformed to the coordinate system, where the main component of the magnetic field
   ** By is directed along the Z axis: x->x, y->-z, z->y, and the function GetDStoPointBz() is called. Derivatives dsdr are transformed back
   ** to the coordinate system of the particle.
   ** \param[in] By - magnetic field By
   ** \param[in] xyz[3] - point, to which particle should be transported
   ** \param[out] dsdr[6] = ds/dr - partial derivatives of the parameter dS over the state vector of the current particle
   **/
  
  const float_v param[6] = { fP[0], -fP[2], fP[1], fP[3], -fP[5], fP[4] };
  const float_v point[3] = { xyz[0], -xyz[2], xyz[1] };
  
  float_v dsdrBz[6] = {0.f,0.f,0.f,0.f,0.f,0.f};

  const float_v dS = GetDStoPointBz(By, point, dsdrBz, param);
  dsdr[0] =  dsdrBz[0];
  dsdr[1] =  dsdrBz[2];
  dsdr[2] = -dsdrBz[1];
  dsdr[3] =  dsdrBz[3];
  dsdr[4] =  dsdrBz[5];
  dsdr[5] = -dsdrBz[4];
  
  return dS;
}

float_v KFParticleBaseSIMD::GetDStoPointCBM( const float_v xyz[3], float_v dsdr[6] ) const
{
  /** Returns dS = l/p parameter, where \n
   ** 1) l - signed distance to the DCA point with the input xyz point;\n
   ** 2) p - momentum of the particle; \n
   ** in case of the CBM-like nonhomogeneous magnetic field.
   ** Also calculates partial derivatives dsdr of the parameter dS over the state vector of the current particle.
   ** For this the y-component of the magnetic field at the current position of the particle is obtained and
   ** the GetDStoPointBy() is called.
   ** \param[in] xyz[3] - point, to which particle should be transported
   ** \param[out] dsdr[6] = ds/dr partial derivatives of the parameter dS over the state vector of the current particle
   **/
  
  float_v dS(Vc::Zero);

  float_v fld[3];
  GetFieldValue( fP, fld );
  dS = GetDStoPointBy( fld[1],xyz, dsdr );
  
  dS(abs(dS)>1.E3f) = 0.f;

  return dS;
}

void KFParticleBaseSIMD::GetDStoParticleBz( float_v B, const KFParticleBaseSIMD &p, 
                                            float_v dS[2], float_v dsdr[4][6], const float_v* param1, const float_v* param2 ) const
{ 
  /** Calculates dS = l/p parameters for two particles, where \n
   ** 1) l - signed distance to the DCA point with the other particle;\n
   ** 2) p - momentum of the particle; \n
   ** under the assumption of the constant homogeneous field Bz. dS[0] is the transport parameter for the current particle,
   ** dS[1] - for the particle "p".
   ** Also calculates partial derivatives dsdr of the parameters dS[0] and dS[1] over the state vectors of the particles:\n
   ** 1) dsdr[0][6] = d(dS[0])/d(param1);\n
   ** 2) dsdr[1][6] = d(dS[0])/d(param2);\n
   ** 3) dsdr[2][6] = d(dS[1])/d(param1);\n
   ** 4) dsdr[3][6] = d(dS[1])/d(param2);\n
   ** where param1 are parameters of the current particle (if the pointer is not provided it is initialised with fP) and
   ** param2 are parameters of the second particle "p" (if the pointer is not provided it is initialised with p.fP). Parameters
   ** param1 and param2 should be either provided both or both set to null pointers.
   ** \param[in] B - magnetic field Bz
   ** \param[in] p - second particle
   ** \param[out] dS[2] - transport parameters dS for the current particle (dS[0]) and the second particle "p" (dS[1])
   ** \param[out] dsdr[4][6] - partial derivatives of the parameters dS[0] and dS[1] over the state vectors of the both particles
   ** \param[in] param1 - optional parameter, is used in case if the parameters of the current particles are rotated
   ** to other coordinate system (see GetDStoParticleBy() function), otherwise fP are used
   ** \param[in] param2 - optional parameter, is used in case if the parameters of the second particles are rotated
   ** to other coordinate system (see GetDStoParticleBy() function), otherwise p.fP are used
   **/
  
  if(!param1)
  {
    param1 = fP;
    param2 = p.fP;
  }

  //* Get dS to another particle for Bz field
  const float_v kOvSqr6 = 1.f/sqrt(float_v(6.f));
  const float_v kCLight = 0.000299792458f;

  //in XY plane
  //first root    
  const float_v& bq1 = B*simd_cast<float_v>(fQ)*kCLight;
  const float_v& bq2 = B*simd_cast<float_v>(p.fQ)*kCLight;

  const float_m& isStraight1 = abs(bq1) < float_v(1.e-8f);
  const float_m& isStraight2 = abs(bq2) < float_v(1.e-8f);
  
  if( isStraight1.isFull() && isStraight2.isFull() )
  {
    GetDStoParticleLine(p, dS, dsdr);
    return;
  }
    
  const float_v& px1 = param1[3];
  const float_v& py1 = param1[4];
  const float_v& pz1 = param1[5];

  const float_v& px2 = param2[3];
  const float_v& py2 = param2[4];
  const float_v& pz2 = param2[5];

  const float_v& pt12 = px1*px1 + py1*py1;
  const float_v& pt22 = px2*px2 + py2*py2;

  const float_v& x01 = param1[0];
  const float_v& y01 = param1[1];
  const float_v& z01 = param1[2];

  const float_v& x02 = param2[0];
  const float_v& y02 = param2[1];
  const float_v& z02 = param2[2];

  float_v dS1[2] = {0.f, 0.f}, dS2[2]={0.f, 0.f};
  
  const float_v& dx0 = (x01 - x02);
  const float_v& dy0 = (y01 - y02);
  const float_v& dr02 = dx0*dx0 + dy0*dy0;
  const float_v& drp1  = dx0*px1 + dy0*py1;
  const float_v& dxyp1 = dx0*py1 - dy0*px1;
  const float_v& drp2  = dx0*px2 + dy0*py2;
  const float_v& dxyp2 = dx0*py2 - dy0*px2;
  const float_v& p1p2 = px1*px2 + py1*py2;
  const float_v& dp1p2 = px1*py2 - px2*py1;
  
  const float_v& k11 = (bq2*drp1 - dp1p2);
  const float_v& k21 = (bq1*(bq2*dxyp1 - p1p2) + bq2*pt12);
  const float_v& k12 = ((bq1*drp2 - dp1p2));
  const float_v& k22 = (bq2*(bq1*dxyp2 + p1p2) - bq1*pt22);
  
  const float_v& kp = (dxyp1*bq2 - dxyp2*bq1 - p1p2);
  const float_v& kd = dr02/2.f*bq1*bq2 + kp;
  const float_v& c1 = -(bq1*kd + pt12*bq2);
  const float_v& c2 = bq2*kd + pt22*bq1; 
  
  float_v d1 = pt12*pt22 - kd*kd;
  d1(d1 < float_v(Vc::Zero)) = float_v(Vc::Zero);
  d1 = sqrt( d1 );
  float_v d2 = pt12*pt22 - kd*kd;
  d2(d2 < float_v(Vc::Zero)) = float_v(Vc::Zero);
  d2 = sqrt( d2 );
    
  float_v dS1dR1[2][6];
  float_v dS2dR2[2][6];

  float_v dS1dR2[2][6];
  float_v dS2dR1[2][6];

  float_v dk11dr1[6] = {bq2*px1, bq2*py1, 0.f, bq2*dx0 - py2, bq2*dy0 + px2, 0.f};
  float_v dk11dr2[6] = {-bq2*px1, -bq2*py1, 0.f, py1, -px1, 0.f};
  float_v dk12dr1[6] = {bq1*px2, bq1*py2, 0.f, -py2, px2, 0.f};
  float_v dk12dr2[6] = {-bq1*px2, -bq1*py2, 0.f, bq1*dx0 + py1, bq1*dy0 - px1, 0.f};
  float_v dk21dr1[6] = {bq1*bq2*py1, -bq1*bq2*px1, 0.f, 2.f*bq2*px1 + bq1*(-(bq2*dy0) - px2), 2.f*bq2*py1 + bq1*(bq2*dx0 - py2), 0.f};
  float_v dk21dr2[6] = {-(bq1*bq2*py1), bq1*bq2*px1, 0.f, -(bq1*px1), -(bq1*py1), 0.f};
  float_v dk22dr1[6] = {bq1*bq2*py2, -(bq1*bq2*px2), 0.f, bq2*px2, bq2*py2, 0.f};
  float_v dk22dr2[6] = {-(bq1*bq2*py2), bq1*bq2*px2, 0.f, bq2*(-(bq1*dy0) + px1) - 2.f*bq1*px2, bq2*(bq1*dx0 + py1) - 2.f*bq1*py2, 0.f};
  
  float_v dkddr1[6] = {bq1*bq2*dx0 + bq2*py1 - bq1*py2, bq1*bq2*dy0 - bq2*px1 + bq1*px2, 0.f, -bq2*dy0 - px2, bq2*dx0 - py2, 0.f};
  float_v dkddr2[6] = {-bq1*bq2*dx0 - bq2*py1 + bq1*py2, -bq1*bq2*dy0 + bq2*px1 - bq1*px2, 0.f, bq1*dy0 - px1, -bq1*dx0 - py1, 0.f};
  
  float_v dc1dr1[6] = {-(bq1*(bq1*bq2*dx0 + bq2*py1 - bq1*py2)), -(bq1*(bq1*bq2*dy0 - bq2*px1 + bq1*px2)), 0.f, -2.f*bq2*px1 - bq1*(-(bq2*dy0) - px2), -2.f*bq2*py1 - bq1*(bq2*dx0 - py2), 0.f};
  float_v dc1dr2[6] = {-(bq1*(-(bq1*bq2*dx0) - bq2*py1 + bq1*py2)), -(bq1*(-(bq1*bq2*dy0) + bq2*px1 - bq1*px2)), 0.f, -(bq1*(bq1*dy0 - px1)), -(bq1*(-(bq1*dx0) - py1)), 0.f};
  
  float_v dc2dr1[6] = {bq2*(bq1*bq2*dx0 + bq2*py1 - bq1*py2), bq2*(bq1*bq2*dy0 - bq2*px1 + bq1*px2), 0.f, bq2*(-(bq2*dy0) - px2), bq2*(bq2*dx0 - py2), 0.f};
  float_v dc2dr2[6] = {bq2*(-(bq1*bq2*dx0) - bq2*py1 + bq1*py2), bq2*(-(bq1*bq2*dy0) + bq2*px1 - bq1*px2), 0.f, bq2*(bq1*dy0 - px1) + 2.f*bq1*px2, bq2*(-(bq1*dx0) - py1) + 2.f*bq1*py2, 0.f};
  
  float_v dd1dr1[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  float_v dd1dr2[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  {
    for(int i=0; i<6; i++)
    {
      dd1dr1[i](d1>0.f) = -kd/d1*dkddr1[i];
      dd1dr2[i](d1>0.f) = -kd/d1*dkddr2[i];
    }
    dd1dr1[3](d1>0.f) += px1/d1*pt22; dd1dr1[4](d1>0.f) += py1/d1*pt22;
    dd1dr2[3](d1>0.f) += px2/d1*pt12; dd1dr2[4](d1>0.f) += py2/d1*pt12;
  }

  // find two points of closest approach in XY plane
  if( ! ( (!isStraight1).isEmpty() ) )
  {
    dS1[0](!isStraight1) = KFPMath::ATan2( (bq1*k11*c1 + k21*d1*bq1), (bq1*k11*d1*bq1 - k21*c1) )/bq1;
    dS1[1](!isStraight1) = KFPMath::ATan2( (bq1*k11*c1 - k21*d1*bq1), (-bq1*k11*d1*bq1 - k21*c1) )/bq1;
    
    float_v a = bq1*(k11*c1 + k21*d1);
    float_v b = bq1*k11*d1*bq1 - k21*c1;
    for(int iP=0; iP<6; iP++)
    {
      const float_v dadr1 = bq1*( dk11dr1[iP]*c1 + k11*dc1dr1[iP] + dk21dr1[iP]*d1 + k21*dd1dr1[iP] );
      const float_v dadr2 = bq1*( dk11dr2[iP]*c1 + k11*dc1dr2[iP] + dk21dr2[iP]*d1 + k21*dd1dr2[iP] );
      const float_v dbdr1 = bq1*bq1*( dk11dr1[iP]*d1 + k11*dd1dr1[iP] ) - ( dk21dr1[iP]*c1 + k21*dc1dr1[iP] );
      const float_v dbdr2 = bq1*bq1*( dk11dr2[iP]*d1 + k11*dd1dr2[iP] ) - ( dk21dr2[iP]*c1 + k21*dc1dr2[iP] );
      
      dS1dR1[0][iP](!isStraight1) = 1/bq1 * 1/( b*b + a*a ) * ( dadr1*b - dbdr1*a );
      dS1dR2[0][iP](!isStraight1) = 1/bq1 * 1/( b*b + a*a ) * ( dadr2*b - dbdr2*a );
    }
    
    a = bq1*(k11*c1 - k21*d1);
    b = -bq1*k11*d1*bq1 - k21*c1;
    for(int iP=0; iP<6; iP++)
    {
      const float_v dadr1 = bq1*( dk11dr1[iP]*c1 + k11*dc1dr1[iP] - (dk21dr1[iP]*d1 + k21*dd1dr1[iP]) );
      const float_v dadr2 = bq1*( dk11dr2[iP]*c1 + k11*dc1dr2[iP] - (dk21dr2[iP]*d1 + k21*dd1dr2[iP]) );
      const float_v dbdr1 = -bq1*bq1*( dk11dr1[iP]*d1 + k11*dd1dr1[iP] ) - ( dk21dr1[iP]*c1 + k21*dc1dr1[iP] );
      const float_v dbdr2 = -bq1*bq1*( dk11dr2[iP]*d1 + k11*dd1dr2[iP] ) - ( dk21dr2[iP]*c1 + k21*dc1dr2[iP] );
      
      dS1dR1[1][iP](!isStraight1) = 1/bq1 * 1/( b*b + a*a ) * ( dadr1*b - dbdr1*a );
      dS1dR2[1][iP](!isStraight1) = 1/bq1 * 1/( b*b + a*a ) * ( dadr2*b - dbdr2*a );
    }
  }
  if( ! ( (!isStraight2).isEmpty() ) )
  {
    dS2[0](!isStraight2) = KFPMath::ATan2( (bq2*k12*c2 + k22*d2*bq2), (bq2*k12*d2*bq2 - k22*c2) )/bq2;
    dS2[1](!isStraight2) = KFPMath::ATan2( (bq2*k12*c2 - k22*d2*bq2), (-bq2*k12*d2*bq2 - k22*c2) )/bq2;
    
    float_v a = bq2*(k12*c2 + k22*d2);
    float_v b = bq2*k12*d2*bq2 - k22*c2;
    for(int iP=0; iP<6; iP++)
    {
      const float_v dadr1 = bq2*( dk12dr1[iP]*c2 + k12*dc2dr1[iP] + dk22dr1[iP]*d1 + k22*dd1dr1[iP] );
      const float_v dadr2 = bq2*( dk12dr2[iP]*c2 + k12*dc2dr2[iP] + dk22dr2[iP]*d1 + k22*dd1dr2[iP] );
      const float_v dbdr1 = bq2*bq2*( dk12dr1[iP]*d1 + k12*dd1dr1[iP] ) - (dk22dr1[iP]*c2 + k22*dc2dr1[iP]);
      const float_v dbdr2 = bq2*bq2*( dk12dr2[iP]*d1 + k12*dd1dr2[iP] ) - (dk22dr2[iP]*c2 + k22*dc2dr2[iP]);
      
      dS2dR1[0][iP](!isStraight2) = 1/bq2 * 1/( b*b + a*a ) * ( dadr1*b - dbdr1*a );
      dS2dR2[0][iP](!isStraight2) = 1/bq2 * 1/( b*b + a*a ) * ( dadr2*b - dbdr2*a );
    }
    
    a = bq2*(k12*c2 - k22*d2);
    b = -bq2*k12*d2*bq2 - k22*c2;
    for(int iP=0; iP<6; iP++)
    {
      const float_v dadr1 = bq2*( dk12dr1[iP]*c2 + k12*dc2dr1[iP] - (dk22dr1[iP]*d1 + k22*dd1dr1[iP]) );
      const float_v dadr2 = bq2*( dk12dr2[iP]*c2 + k12*dc2dr2[iP] - (dk22dr2[iP]*d1 + k22*dd1dr2[iP]) );
      const float_v dbdr1 = -bq2*bq2*( dk12dr1[iP]*d1 + k12*dd1dr1[iP] ) - (dk22dr1[iP]*c2 + k22*dc2dr1[iP]);
      const float_v dbdr2 = -bq2*bq2*( dk12dr2[iP]*d1 + k12*dd1dr2[iP] ) - (dk22dr2[iP]*c2 + k22*dc2dr2[iP]);
      
      dS2dR1[1][iP](!isStraight2) = 1/bq2 * 1/( b*b + a*a ) * ( dadr1*b - dbdr1*a );
      dS2dR2[1][iP](!isStraight2) = 1/bq2 * 1/( b*b + a*a ) * ( dadr2*b - dbdr2*a );
    }
  }
  if( ! ( isStraight1.isEmpty() ) )
  {
    dS1[0](isStraight1 && (pt12>float_v(Vc::Zero)) ) = (k11*c1 + k21*d1)/(- k21*c1);
    dS1[1](isStraight1 && (pt12>float_v(Vc::Zero)) ) = (k11*c1 - k21*d1)/(- k21*c1);
    
    float_v a = k11*c1 + k21*d1;
    float_v b = -k21*c1;
    
    for(int iP=0; iP<6; iP++)
    {
      const float_v dadr1 = ( dk11dr1[iP]*c1 + k11*dc1dr1[iP] + dk21dr1[iP]*d1 + k21*dd1dr1[iP] );
      const float_v dadr2 = ( dk11dr2[iP]*c1 + k11*dc1dr2[iP] + dk21dr2[iP]*d1 + k21*dd1dr2[iP] );
      const float_v dbdr1 = -( dk21dr1[iP]*c1 + k21*dc1dr1[iP] );
      const float_v dbdr2 = -( dk21dr2[iP]*c1 + k21*dc1dr2[iP] );
    
      dS1dR1[0][iP](isStraight1 && (pt12>float_v(Vc::Zero)) ) = dadr1/b - dbdr1*a/(b*b) ;
      dS1dR2[0][iP](isStraight1 && (pt12>float_v(Vc::Zero)) ) = dadr2/b - dbdr2*a/(b*b) ;
    }
    
    a = k11*c1 - k21*d1;
    for(int iP=0; iP<6; iP++)
    {
      const float_v dadr1 = ( dk11dr1[iP]*c1 + k11*dc1dr1[iP] - dk21dr1[iP]*d1 - k21*dd1dr1[iP] );
      const float_v dadr2 = ( dk11dr2[iP]*c1 + k11*dc1dr2[iP] - dk21dr2[iP]*d1 - k21*dd1dr2[iP] );
      const float_v dbdr1 = -( dk21dr1[iP]*c1 + k21*dc1dr1[iP] );
      const float_v dbdr2 = -( dk21dr2[iP]*c1 + k21*dc1dr2[iP] );
      
      dS1dR1[1][iP](isStraight1 && (pt12>float_v(Vc::Zero)) ) = dadr1/b - dbdr1*a/(b*b) ;
      dS1dR2[1][iP](isStraight1 && (pt12>float_v(Vc::Zero)) ) = dadr2/b - dbdr2*a/(b*b) ;
    }
  }
  if( ! ( isStraight2.isEmpty() ) )
  {
    dS2[0](isStraight2 && (pt22>float_v(Vc::Zero)) ) = (k12*c2 + k22*d2)/(- k22*c2);
    dS2[1](isStraight2 && (pt22>float_v(Vc::Zero)) ) = (k12*c2 - k22*d2)/(- k22*c2);  
    
    float_v a = k12*c2 + k22*d1;
    float_v b = -k22*c2;
    
    for(int iP=0; iP<6; iP++)
    {
      const float_v dadr1 = ( dk12dr1[iP]*c2 + k12*dc2dr1[iP] + dk22dr1[iP]*d1 + k22*dd1dr1[iP] );
      const float_v dadr2 = ( dk12dr2[iP]*c2 + k12*dc2dr2[iP] + dk22dr2[iP]*d1 + k22*dd1dr2[iP] );
      const float_v dbdr1 = -( dk22dr1[iP]*c2 + k22*dc2dr1[iP] );
      const float_v dbdr2 = -( dk22dr2[iP]*c2 + k22*dc2dr2[iP] );
    
      dS2dR1[0][iP](isStraight2 && (pt22>float_v(Vc::Zero)) ) = dadr1/b - dbdr1*a/(b*b) ;
      dS2dR2[0][iP](isStraight2 && (pt22>float_v(Vc::Zero)) ) = dadr2/b - dbdr2*a/(b*b) ;
    }
    
    a = k12*c2 - k22*d1;
    for(int iP=0; iP<6; iP++)
    {
      const float_v dadr1 = ( dk12dr1[iP]*c2 + k12*dc2dr1[iP] - dk22dr1[iP]*d1 - k22*dd1dr1[iP] );
      const float_v dadr2 = ( dk12dr2[iP]*c2 + k12*dc2dr2[iP] - dk22dr2[iP]*d1 - k22*dd1dr2[iP] );
      const float_v dbdr1 = -( dk22dr1[iP]*c2 + k22*dc2dr1[iP] );
      const float_v dbdr2 = -( dk22dr2[iP]*c2 + k22*dc2dr2[iP] );
    
      dS2dR1[1][iP](isStraight2 && (pt22>float_v(Vc::Zero)) ) = dadr1/b - dbdr1*a/(b*b) ;
      dS2dR2[1][iP](isStraight2 && (pt22>float_v(Vc::Zero)) ) = dadr2/b - dbdr2*a/(b*b) ;
    }
  }
  
  //select a point which is close to the primary vertex (with the smallest r)
  
  float_v dr2[2];
  for(int iP = 0; iP<2; iP++)
  {
    const float_v& bs1 = bq1*dS1[iP];
    const float_v& bs2 = bq2*dS2[iP];
    float_v sss = sin(bs1), ccc = cos(bs1);
    
    const float_m& bs1Big = abs(bs1) > 1.e-8f;
    const float_m& bs2Big = abs(bs2) > 1.e-8f;
    
    float_v sB(Vc::Zero), cB(Vc::Zero);
    sB(bs1Big) = sss/bq1;
    sB(!bs1Big) = ((1.f-bs1*kOvSqr6)*(1.f+bs1*kOvSqr6)*dS1[iP]);
    cB(bs1Big) = (1.f-ccc)/bq1;
    cB(!bs1Big) = .5f*sB*bs1;
  
    const float_v& x1 = param1[0] + sB*px1 + cB*py1;
    const float_v& y1 = param1[1] - cB*px1 + sB*py1;
    const float_v& z1 = param1[2] + dS1[iP]*param1[5];

    sss = sin(bs2); ccc = cos(bs2);

    sB(bs2Big) = sss/bq2;
    sB(!bs2Big) = ((1.f-bs2*kOvSqr6)*(1.f+bs2*kOvSqr6)*dS2[iP]);
    cB(bs2Big) = (1.f-ccc)/bq2;
    cB(!bs2Big) = .5f*sB*bs2;

    const float_v& x2 = param2[0] + sB*px2 + cB*py2;
    const float_v& y2 = param2[1] - cB*px2 + sB*py2;
    const float_v& z2 = param2[2] + dS2[iP]*param2[5];

    float_v dx = (x1-x2);
    float_v dy = (y1-y2);
    float_v dz = (z1-z2);
    
    dr2[iP] = dx*dx + dy*dy + dz*dz;
  }
  
  float_v pointParam[2][8];
  float_v pointCov[2][36];
  
  float_v dsdrM1[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  for(int iP=0; iP<6; iP++)
    dsdrM1[iP] = (dS1dR1[0][iP] + dS1dR1[1][iP])/2.f;
  Transport((dS1[0] + dS1[1]) / 2.f, dsdrM1, pointParam[0], pointCov[0]);
  float_v dsdrM2[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  for(int iP=0; iP<6; iP++)
    dsdrM2[iP] = (dS2dR2[0][iP] + dS2dR2[1][iP])/2.f;
  p.Transport((dS2[0] + dS2[1]) / 2.f, dsdrM2, pointParam[1], pointCov[1]);
  
  const float_v drPoint[3] = { pointParam[0][0] - pointParam[1][0], pointParam[0][1] - pointParam[1][1], pointParam[0][2] - pointParam[1][2] } ;
  const float_v covPoint[6] = { pointCov[0][0] + pointCov[1][0],
                                pointCov[0][1] + pointCov[1][1],
                                pointCov[0][2] + pointCov[1][2],
                                pointCov[0][3] + pointCov[1][3],
                                pointCov[0][4] + pointCov[1][4],
                                pointCov[0][5] + pointCov[1][5]  };
  float_v dr2Points = drPoint[0]*drPoint[0] + drPoint[1]*drPoint[1] + drPoint[2]*drPoint[2];
  float_v dr2PointCov = drPoint[0]*drPoint[0]*covPoint[0] + drPoint[1]*drPoint[1]*covPoint[2] + drPoint[2]*drPoint[2]*covPoint[5] +
                        2.f*( drPoint[0]*drPoint[1]*covPoint[1] + drPoint[0]*drPoint[2]*covPoint[3] + drPoint[1]*drPoint[2]*covPoint[4] );
  const float_m isMiddlePoint = (dr2Points*dr2Points < 25.f*dr2PointCov);// && (abs(fPDG)==int_v(11)) && (abs(p.fPDG)==int_v(11));
  const float_m isFirstRoot = (dr2[0] < dr2[1]) & (!isMiddlePoint);
  
 // if(!(isFirstRoot.isEmpty()))
  {
    dS[0](isFirstRoot)  = dS1[0];
    dS[1](isFirstRoot) = dS2[0];
    
    for(int iP=0; iP<6; iP++)
    {
      dsdr[0][iP](isFirstRoot) = dS1dR1[0][iP];
      dsdr[1][iP](isFirstRoot) = dS1dR2[0][iP];
      dsdr[2][iP](isFirstRoot) = dS2dR1[0][iP];
      dsdr[3][iP](isFirstRoot) = dS2dR2[0][iP];
    }
  }
 // if( !( (!isFirstRoot).isEmpty() ) )
  {
    dS[0](!isFirstRoot)  = dS1[1];
    dS[1](!isFirstRoot) = dS2[1];    
    
    for(int iP=0; iP<6; iP++)
    {
      dsdr[0][iP](!isFirstRoot) = dS1dR1[1][iP];
      dsdr[1][iP](!isFirstRoot) = dS1dR2[1][iP];
      dsdr[2][iP](!isFirstRoot) = dS2dR1[1][iP];      
      dsdr[3][iP](!isFirstRoot) = dS2dR2[1][iP];
    }    
  }
 // if(!(isMiddlePoint.isEmpty()))
  {
    dS[0](isMiddlePoint) = (dS1[0] + dS1[1]) / 2.f;
    dS[1](isMiddlePoint) = (dS2[0] + dS2[1]) / 2.f;
    
    for(int iP=0; iP<6; iP++)
    {
      dsdr[0][iP](isMiddlePoint) = (dS1dR1[1][iP] + dS1dR1[0][iP])/2.f;
      dsdr[1][iP](isMiddlePoint) = (dS1dR2[1][iP] + dS1dR2[0][iP])/2.f;
      dsdr[2][iP](isMiddlePoint) = (dS2dR1[1][iP] + dS2dR1[0][iP])/2.f;      
      dsdr[3][iP](isMiddlePoint) = (dS2dR2[1][iP] + dS2dR2[0][iP])/2.f;
    }  
  }
        
  //find correct parts of helices
  int_v n1(Vc::Zero);
  int_v n2(Vc::Zero);
//  float_v dzMin = abs( (z01-z02) + dS[0]*pz1 - dS[1]*pz2 );
  const float_v pi2(6.283185307f);
  
//   //TODO optimise for loops for neutral particles
//   const float_v& i1Float = -bq1/pi2*(z01/pz1+dS[0]);
//   for(int di1=-1; di1<=1; di1++)
//   {
//     int_v i1(Vc::Zero);
//     i1(int_m(!isStraight1)) = int_v(i1Float) + di1;
//     
//     const float_v& i2Float = ( ((z01-z02) + (dS[0]+pi2*i1/bq1)*pz1)/pz2 - dS[1]) * bq2/pi2;
//     for(int di2 = -1; di2<=1; di2++)
//     {
//       int_v i2(Vc::Zero);
//       i2(int_m(!isStraight2)) = int_v(i2Float) + di2;
//       
//       const float_v& z1 = z01 + (dS[0]+pi2*i1/bq1)*pz1;
//       const float_v& z2 = z02 + (dS[1]+pi2*i2/bq2)*pz2;
//       const float_v& dz = abs( z1-z2 );
//     
//       n1(int_m(dz < dzMin)) = i1;
//       n2(int_m(dz < dzMin)) = i2;
//       dzMin(dz < dzMin) = dz;
//     }
//   }
// 
//   dS[0](!isStraight1) += float_v(n1)*pi2/bq1;
//   dS[1](!isStraight2) += float_v(n2)*pi2/bq2;

  //add a correction on z-coordinate
#if 0
  {
    const float_v& bs1 = bq1*dS[0];
    const float_v& bs2 = bq2*dS[1];
    
    float_v sss = KFPMath::Sin(bs1), ccc = KFPMath::Cos(bs1);
    const float_v& xr1 = sss*px1 - ccc*py1;
    const float_v& yr1 = ccc*px1 + sss*py1;

    float_v sss1 = KFPMath::Sin(bs2), ccc1 = KFPMath::Cos(bs2);
    const float_v& xr2 = sss1*px2 - ccc1*py2;
    const float_v& yr2 = ccc1*px2 + sss1*py2;
    
    const float_v& br = xr1*xr2 + yr1*yr2;
    const float_v& dx0mod = dx0*bq1*bq2 + py1*bq2 - py2*bq1;
    const float_v& dy0mod = dy0*bq1*bq2 - px1*bq2 + px2*bq1;
    const float_v& ar1 = dx0mod*xr1 + dy0mod*yr1;
    const float_v& ar2 = dx0mod*xr2 + dy0mod*yr2;
    const float_v& cz = (z01 - z02) + dS[0]*pz1 - dS[1]*pz2;
    
    float_v kz11 =  - ar1 + bq1*br + bq2*pz1*pz1;
    float_v kz12 =  -bq2*(br+pz1*pz2);
    float_v kz21 =   bq1*(br+pz1*pz2);
    float_v kz22 =  - ar2 - bq2*br - bq1*pz2*pz2;
    
    kz11(isStraight2) = pz1*pz1 + (px1*px1+py1*py1)*ccc + bq1*( (px2*dS[1] - dx0)*xr1 + (py2*dS[1] - dy0)*yr1 );
    kz12(isStraight2) = -(br + pz1*pz2);
    kz21(isStraight1) = (br + pz1*pz2);
    kz22(isStraight1) = -pz2*pz2 - (px2*px2+py2*py2)*ccc1 - bq2*( (px1*dS[0] + dx0)*xr2 + (py1*dS[0] + dy0)*yr2 );

    const float_v& delta = kz11*kz22 - kz12*kz21;
    float_v sz1(Vc::Zero);
    float_v sz2(Vc::Zero);
    
    {
      float_v aaa1 = -cz*(pz1*bq2*kz22 - pz2*bq1*kz12);
      float_v aaa2 = -cz*(pz2*bq1*kz11 - pz1*bq2*kz21);
      
      aaa1(isStraight2) = -cz*(pz1*kz22 - pz2*bq1*kz12);
      aaa2(isStraight2) = -cz*(pz2*bq1*kz11 - pz1*kz21);

      aaa1(isStraight1) = -cz*(pz1*bq2*kz22 - pz2*kz12);
      aaa2(isStraight1) = -cz*(pz2*kz11 - pz1*bq2*kz21);
      
      sz1( abs(delta) > 1.e-16f ) = aaa1 / delta;
      sz2( abs(delta) > 1.e-16f ) = aaa2 / delta;
      
      float_v dkz11dr1[6] = {-(bq1*bq2*xr1), -(bq1*bq2*yr1), 0.f, 
                                   -ccc*dy0mod - dx0mod*sss + bq2*yr1 + bq1*(sss*xr2 + ccc*yr2), 
                                    ccc*dx0mod - dy0mod*sss - bq2*xr1 + bq1*(-ccc*xr2 + sss*yr2), 2.f*bq2*pz1};
              dkz11dr1[0](isStraight2) = -bq1*xr1;
              dkz11dr1[1](isStraight2) = -bq1*yr1;
              dkz11dr1[3](isStraight2) = 2.f*ccc*px1 + bq1*(sss*(dS[1]*px2 - x01 + x02) + ccc*(dS[1]*py2 - y01 + y02));
              dkz11dr1[4](isStraight2) = 2.f*ccc*py1 + bq1*(-(ccc*(dS[1]*px2 - x01 + x02)) + sss*(dS[1]*py2 - y01 + y02));
              dkz11dr1[5](isStraight2) = 2.f*pz1;
      float_v dkz11dr2[6] = {bq1*bq2*xr1, bq1*bq2*yr1, 0.f, -bq1*yr1 + bq1*(sss1*xr1 + ccc1*yr1), bq1*xr1 + bq1*(-ccc1*xr1 + sss1*yr1), 0.f};
              dkz11dr2[0](isStraight2) = bq1*xr1;
              dkz11dr2[1](isStraight2) = bq1*yr1;
              dkz11dr2[3](isStraight2) = bq1*dS[1]*xr1;
              dkz11dr2[4](isStraight2) = bq1*dS[1]*yr1;

      float_v dkz12dr1[6] = {0.f, 0.f, 0.f, -bq2*(sss*xr2 + ccc*yr2), -bq2*(-ccc*xr2 + sss*yr2), -bq2*pz2};
              dkz12dr1[3](isStraight2) = -(sss*xr2 + ccc*yr2);
              dkz12dr1[4](isStraight2) = -(-ccc*xr2 + sss*yr2);
              dkz12dr1[5](isStraight2) = -pz2;
      float_v dkz12dr2[6] = {0.f, 0.f, 0.f, -bq2*(sss1*xr1 + ccc1*yr1), -bq2*(-ccc1*xr1 + sss1*yr1), -bq2*pz1};
              dkz12dr2[3](isStraight2) = -(sss1*xr1 + ccc1*yr1);
              dkz12dr2[4](isStraight2) = -(-ccc1*xr1 + sss1*yr1);
              dkz12dr2[5](isStraight2) = -pz1;
      float_v dkz21dr1[6] = {0.f, 0.f, 0.f, bq1*(sss*xr2 + ccc*yr2), bq1*(-ccc*xr2 + sss*yr2), bq1*pz2};
              dkz21dr1[3](isStraight1) = yr2;
              dkz21dr1[4](isStraight1) = -xr2;
              dkz21dr1[5](isStraight1) =  pz2;
      float_v dkz21dr2[6] = {0.f, 0.f, 0.f, bq1*(sss1*xr1 + ccc1*yr1), bq1*(-ccc1*xr1 + sss1*yr1), bq1*pz1};
              dkz21dr2[3](isStraight1) = (sss1*xr1 + ccc1*yr1);
              dkz21dr2[4](isStraight1) = (-ccc1*xr1 + sss1*yr1);
              dkz21dr2[5](isStraight1) = pz1;
      float_v dkz22dr1[6] = {-bq1*bq2*xr2, -bq1*bq2*yr2, 0.f, bq2*yr2 - bq2*(sss*xr2 + ccc*yr2), -bq2*xr2 - bq2*(-ccc*xr2 + sss*yr2), 0.f};
              dkz22dr1[0](isStraight1) = -(bq2*xr2);
              dkz22dr1[1](isStraight1) = -(bq2*yr2);
              dkz22dr1[3](isStraight1) = -(bq2*dS[0]*xr2);
              dkz22dr1[4](isStraight1) = -(bq2*dS[0]*yr2);
      float_v dkz22dr2[6] = {bq1*bq2*xr2, bq1*bq2*yr2, 0.f, 
                             -ccc1*dy0mod - dx0mod*sss1 - bq2*(sss1*xr1 + ccc1*yr1) - bq1*yr2, 
                              ccc1*dx0mod - dy0mod*sss1 + bq1*xr2 - bq2*(-ccc1*xr1 + sss1*yr1), -2.f*bq1*pz2};
              dkz22dr2[0](isStraight1) = bq2*xr2;
              dkz22dr2[1](isStraight1) = bq2*yr2;
              dkz22dr2[3](isStraight1) = -2.f*ccc1*px2 - bq2*(ccc1*(dy0 + dS[0]*py1) + (dx0 + dS[0]*px1)*sss1);
              dkz22dr2[4](isStraight1) = -2.f*ccc1*py2 - bq2*(-(ccc1*(dx0 + dS[0]*px1)) + (dy0 + dS[0]*py1)*sss1);
              dkz22dr2[5](isStraight1) = -2.f*pz2;
      
      float_v dczdr1[6] = {0.f, 0.f, 1.f, 0.f, 0.f, dS[0]};
      float_v dczdr2[6] = {0.f, 0.f, -1.f, 0.f, 0.f, -dS[1]};
      
      float_v daaa1dr1[6];
      float_v daaa1dr2[6];
      float_v daaa2dr2[6];
      float_v daaa2dr1[6];
      float_v dDeltadr1[6];
      float_v dDeltadr2[6];
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
      float_v dkz11ds0 = bq1*(dy0mod*xr1 - dx0mod*yr1 +  bq1*(xr2*yr1 - xr1*yr2));
      dkz11ds0(isStraight2) = -(bq1*(px1*px1 + py1*py1)*sss) + bq1*(bq1*yr1*(dS[1]*px2 - x01 + x02) -bq1*xr1*(dS[1]*py2 - y01 + y02));
      float_v dkz11ds1 = bq1*bq2*( xr1*yr2 - xr2*yr1 );
      dkz11ds1(isStraight2) = bq1*(px2*xr1 + py2*yr1);
      float_v dkz12ds0 = bq2*bq1*( xr1*yr2 - xr2*yr1 );
      dkz12ds0(isStraight2) = bq1*( xr1*yr2 - xr2*yr1 );
      float_v dkz12ds1 = bq2*bq2*( xr2*yr1 - xr1*yr2 );
      dkz12ds1(isStraight2) = 0;
      float_v dkz21ds0 = bq1*bq1*( xr2*yr1 - xr1*yr2 );
      dkz21ds0(isStraight1) = 0.f;
      float_v dkz21ds1 = bq1*bq2*( xr1*yr2 - xr2*yr1 );
      dkz21ds1(isStraight1) = px1*(bq2*ccc1*py2 - bq2*px2*sss1) - py1*(bq2*ccc1*px2 + bq2*py2*sss1);
      float_v dkz22ds0 = bq1*bq2*( xr1*yr2 - xr2*yr1 );      
      dkz22ds0(isStraight1) = -bq2*(px1*xr2 + py1*yr2);
      float_v dkz22ds1 = -bq2*( dy0mod*xr2 - dx0mod*yr2 - bq2*(xr2*yr1 - xr1*yr2) );
      dkz22ds1(isStraight1) = bq2*(px2*px2 + py2*py2)*sss1 - bq2*((dy0 + dS[0]*py1)*(bq2*ccc1*py2 - bq2*px2*sss1) + (dx0 + dS[0]*px1)*(bq2*ccc1*px2 + bq2*py2*sss1));
      const float_v dczds0   = pz1;
      const float_v dczds1   = -pz2;
      const float_v da1ds0   = -( dczds0*(pz1*bq2*kz22 - pz2*bq1*kz12) + cz*(pz1*bq2*dkz22ds0 - pz2*bq1*dkz12ds0));
      const float_v da1ds1   = -( dczds1*(pz1*bq2*kz22 - pz2*bq1*kz12) + cz*(pz1*bq2*dkz22ds1 - pz2*bq1*dkz12ds1));
      const float_v da2ds0   = -( dczds0*(pz2*bq1*kz11 - pz1*bq2*kz21) + cz*(pz2*bq1*dkz11ds0 - pz1*bq2*dkz21ds0));
      const float_v da2ds1   = -( dczds1*(pz2*bq1*kz11 - pz1*bq2*kz21) + cz*(pz2*bq1*dkz11ds1 - pz1*bq2*dkz21ds1));
      const float_v dDeltads0 = kz11*dkz22ds0 + dkz11ds0*kz11 - kz12*dkz21ds0 - dkz12ds0*kz21;
      const float_v dDeltads1 = kz11*dkz22ds1 + dkz11ds1*kz11 - kz12*dkz21ds1 - dkz12ds1*kz21;
      
      const float_v dsz1ds0 = da1ds0/delta - aaa1*dDeltads0/(delta*delta);
      const float_v dsz1ds1 = da1ds1/delta - aaa1*dDeltads1/(delta*delta);
      const float_v dsz2ds0 = da2ds0/delta - aaa2*dDeltads0/(delta*delta);
      const float_v dsz2ds1 = da2ds1/delta - aaa2*dDeltads1/(delta*delta);
      
      float_v dszdr[4][6];
      for(int iP=0; iP<6; iP++)
      {
        dszdr[0][iP] = dsz1ds0*dsdr[0][iP] + dsz1ds1*dsdr[2][iP];
        dszdr[1][iP] = dsz1ds0*dsdr[1][iP] + dsz1ds1*dsdr[3][iP];
        dszdr[2][iP] = dsz2ds0*dsdr[0][iP] + dsz2ds1*dsdr[2][iP];
        dszdr[3][iP] = dsz2ds0*dsdr[1][iP] + dsz2ds1*dsdr[3][iP];
      }
      
      for(int iP=0; iP<6; iP++)
      {
        dsdr[0][iP]( abs(delta) > 1.e-16f ) += daaa1dr1[iP]/delta - aaa1*dDeltadr1[iP]/(delta*delta) + dszdr[0][iP];
        dsdr[1][iP]( abs(delta) > 1.e-16f ) += daaa1dr2[iP]/delta - aaa1*dDeltadr2[iP]/(delta*delta) + dszdr[1][iP];
        dsdr[2][iP]( abs(delta) > 1.e-16f ) += daaa2dr1[iP]/delta - aaa2*dDeltadr1[iP]/(delta*delta) + dszdr[2][iP];
        dsdr[3][iP]( abs(delta) > 1.e-16f ) += daaa2dr2[iP]/delta - aaa2*dDeltadr2[iP]/(delta*delta) + dszdr[3][iP];
      }
    }

    dS[0] += sz1;
    dS[1] += sz2;
  }
#endif

  //Line correction
  {
    const float_v& bs1 = bq1*dS[0];
    const float_v& bs2 = bq2*dS[1];
    float_v sss = sin(bs1), ccc = cos(bs1);
    
    const float_m& bs1Big = abs(bs1) > 1.e-8f;
    const float_m& bs2Big = abs(bs2) > 1.e-8f;
    
    float_v sB(0.f), cB(0.f);
    sB(bs1Big) = sss/bq1;
    cB(bs1Big) = (1.f-ccc)/bq1;
    sB(!bs1Big) = ((1.f-bs1*kOvSqr6)*(1.f+bs1*kOvSqr6)*dS[0]);
    cB(!bs1Big) = .5f*sB*bs1;
  
    const float_v& x1 = x01 + sB*px1 + cB*py1;
    const float_v& y1 = y01 - cB*px1 + sB*py1;
    const float_v& z1 = z01 + dS[0]*pz1;
    const float_v& ppx1 =  ccc*px1 + sss*py1;
    const float_v& ppy1 = -sss*px1 + ccc*py1;
    const float_v& ppz1 = pz1;
    
    float_v sss1 = sin(bs2), ccc1 = cos(bs2);

    float_v sB1(0.f), cB1(0.f);
    sB1(bs2Big) = sss1/bq2;
    cB1(bs2Big) = (1.f-ccc1)/bq2;
    sB1(!bs2Big) = ((1.f-bs2*kOvSqr6)*(1.f+bs2*kOvSqr6)*dS[1]);
    cB1(!bs2Big) = .5f*sB1*bs2;

    const float_v& x2 = x02 + sB1*px2 + cB1*py2;
    const float_v& y2 = y02 - cB1*px2 + sB1*py2;
    const float_v& z2 = z02 + dS[1]*pz2;
    const float_v& ppx2 =  ccc1*px2 + sss1*py2;
    const float_v& ppy2 = -sss1*px2 + ccc1*py2;    
    const float_v& ppz2 = pz2;

    const float_v& p12  = ppx1*ppx1 + ppy1*ppy1 + ppz1*ppz1;
    const float_v& p22  = ppx2*ppx2 + ppy2*ppy2 + ppz2*ppz2;
    const float_v& lp1p2 = ppx1*ppx2 + ppy1*ppy2 + ppz1*ppz2;

    const float_v& dx = (x2 - x1);
    const float_v& dy = (y2 - y1);
    const float_v& dz = (z2 - z1);
    
    const float_v& ldrp1 = ppx1*dx + ppy1*dy + ppz1*dz;
    const float_v& ldrp2 = ppx2*dx + ppy2*dy + ppz2*dz;

    float_v detp =  lp1p2*lp1p2 - p12*p22;
    detp(abs(detp)<1.e-4f) = 1; //TODO correct!!!
    
    //dsdr calculation
    const float_v a1 = ldrp2*lp1p2 - ldrp1*p22;
    const float_v a2 = ldrp2*p12 - ldrp1*lp1p2;
    const float_v lp1p2_ds0 = bq1*( ppx2*ppy1 - ppy2*ppx1);
    const float_v lp1p2_ds1 = bq2*( ppx1*ppy2 - ppy1*ppx2);
    const float_v ldrp1_ds0 = -p12 + bq1*(ppy1*dx - ppx1*dy);
    const float_v ldrp1_ds1 =  lp1p2;
    const float_v ldrp2_ds0 = -lp1p2;
    const float_v ldrp2_ds1 =  p22 + bq2*(ppy2*dx - ppx2*dy);
    const float_v detp_ds0 = 2.f*lp1p2*lp1p2_ds0;
    const float_v detp_ds1 = 2.f*lp1p2*lp1p2_ds1;
    const float_v a1_ds0 = ldrp2_ds0*lp1p2 + ldrp2*lp1p2_ds0 - ldrp1_ds0*p22;
    const float_v a1_ds1 = ldrp2_ds1*lp1p2 + ldrp2*lp1p2_ds1 - ldrp1_ds1*p22;
    const float_v a2_ds0 = ldrp2_ds0*p12 - ldrp1_ds0*lp1p2 - ldrp1*lp1p2_ds0;
    const float_v a2_ds1 = ldrp2_ds1*p12 - ldrp1_ds1*lp1p2 - ldrp1*lp1p2_ds1;
    
    const float_v dsl1ds0 = a1_ds0/detp - a1*detp_ds0/(detp*detp);
    const float_v dsl1ds1 = a1_ds1/detp - a1*detp_ds1/(detp*detp);
    const float_v dsl2ds0 = a2_ds0/detp - a2*detp_ds0/(detp*detp);
    const float_v dsl2ds1 = a2_ds1/detp - a2*detp_ds1/(detp*detp);
    
    float_v dsldr[4][6];
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
      
    const float_v lp1p2_dr0[6] = {0.f, 0.f, 0.f, ccc*ppx2 - ppy2*sss, ccc*ppy2 + ppx2*sss, pz2};
    const float_v lp1p2_dr1[6] = {0.f, 0.f, 0.f, ccc1*ppx1 - ppy1*sss1, ccc1*ppy1 + ppx1*sss1, pz1};
    const float_v ldrp1_dr0[6] = {-ppx1, -ppy1, -pz1,  cB*ppy1 - ppx1*sB + ccc*dx - sss*dy, -cB*ppx1-ppy1*sB + sss*dx + ccc*dy, -dS[0]*pz1 + dz};
    const float_v ldrp1_dr1[6] = { ppx1,  ppy1,  pz1, -cB1*ppy1 + ppx1*sB1, cB1*ppx1 + ppy1*sB1, dS[1]*pz1};
    const float_v ldrp2_dr0[6] = {-ppx2, -ppy2, -pz2, cB*ppy2 - ppx2*sB, -cB*ppx2-ppy2*sB, -dS[0]*pz2};
    const float_v ldrp2_dr1[6] = { ppx2, ppy2, pz2, -cB1*ppy2 + ppx2*sB1 + ccc1*dx- sss1*dy, cB1*ppx2 + ppy2*sB1 + sss1*dx + ccc1*dy, dz + dS[1]*pz2};
    const float_v p12_dr0[6] = {0.f, 0.f, 0.f, 2.f*px1, 2.f*py1, 2.f*pz1};
    const float_v p22_dr1[6] = {0.f, 0.f, 0.f, 2.f*px2, 2.f*py2, 2.f*pz2};
    float_v a1_dr0[6], a1_dr1[6], a2_dr0[6], a2_dr1[6], detp_dr0[6], detp_dr1[6];
    for(int iP=0; iP<6; iP++)
    {
      a1_dr0[iP] = ldrp2_dr0[iP]*lp1p2 + ldrp2*lp1p2_dr0[iP] - ldrp1_dr0[iP]*p22;
      a1_dr1[iP] = ldrp2_dr1[iP]*lp1p2 + ldrp2*lp1p2_dr1[iP] - ldrp1_dr1[iP]*p22 - ldrp1*p22_dr1[iP];
      a2_dr0[iP] = ldrp2_dr0[iP]*p12 + ldrp2*p12_dr0[iP] - ldrp1_dr0[iP]*lp1p2 - ldrp1*lp1p2_dr0[iP];
      a2_dr1[iP] = ldrp2_dr1[iP]*p12 - ldrp1_dr1[iP]*lp1p2 - ldrp1*lp1p2_dr1[iP];
      detp_dr0[iP] = 2.f*lp1p2*lp1p2_dr0[iP] - p12_dr0[iP]*p22;
      detp_dr1[iP] = 2.f*lp1p2*lp1p2_dr1[iP] - p12*p22_dr1[iP];
      
      dsdr[0][iP] += a1_dr0[iP]/detp - a1*detp_dr0[iP]/(detp*detp);
      dsdr[1][iP] += a1_dr1[iP]/detp - a1*detp_dr1[iP]/(detp*detp);
      dsdr[2][iP] += a2_dr0[iP]/detp - a2*detp_dr0[iP]/(detp*detp);
      dsdr[3][iP] += a2_dr1[iP]/detp - a2*detp_dr1[iP]/(detp*detp);
    }
    
    dS[0] += (ldrp2*lp1p2 - ldrp1*p22) /detp;
    dS[1] += (ldrp2*p12 - ldrp1*lp1p2)/detp;    
  }
}

void KFParticleBaseSIMD::GetDStoParticleBz( float_v B, const KFParticleBaseSIMD &p, 
                                            float_v dS[2], const float_v* param1, const float_v* param2 ) const
{ 
  /** Calculates dS = l/p parameters for two particles, where \n
   ** 1) l - signed distance to the DCA point with the other particle;\n
   ** 2) p - momentum of the particle; \n
   ** under the assumption of the constant homogeneous field Bz. dS[0] is the transport parameter for the current particle,
   ** dS[1] - for the particle "p".
   ** Parameters param1 and param2 should be either provided both or both set to null pointers.
   ** \param[in] B - magnetic field Bz
   ** \param[in] p - second particle
   ** \param[out] dS[2] - transport parameters dS for the current particle (dS[0]) and the second particle "p" (dS[1])
   ** \param[in] param1 - optional parameter, is used in case if the parameters of the current particles are rotated
   ** to other coordinate system (see GetDStoParticleBy() function), otherwise fP are used
   ** \param[in] param2 - optional parameter, is used in case if the parameters of the second particles are rotated
   ** to other coordinate system (see GetDStoParticleBy() function), otherwise p.fP are used
   **/
  
  if(!param1)
  {
    param1 = fP;
    param2 = p.fP;
  }

  //* Get dS to another particle for Bz field
  const float_v kOvSqr6 = 1.f/sqrt(float_v(6.f));
  const float_v kCLight = 0.000299792458f;

  //in XY plane
  //first root    
  const float_v& bq1 = B*simd_cast<float_v>(fQ)*kCLight;
  const float_v& bq2 = B*simd_cast<float_v>(p.fQ)*kCLight;

  const float_m& isStraight1 = abs(bq1) < float_v(1.e-8f);
  const float_m& isStraight2 = abs(bq2) < float_v(1.e-8f);
  
  if( isStraight1.isFull() && isStraight2.isFull() )
  {
    GetDStoParticleLine(p, dS);
    return;
  }
    
  const float_v& px1 = param1[3];
  const float_v& py1 = param1[4];
  const float_v& pz1 = param1[5];

  const float_v& px2 = param2[3];
  const float_v& py2 = param2[4];
  const float_v& pz2 = param2[5];

  const float_v& pt12 = px1*px1 + py1*py1;
  const float_v& pt22 = px2*px2 + py2*py2;

  const float_v& x01 = param1[0];
  const float_v& y01 = param1[1];
  const float_v& z01 = param1[2];

  const float_v& x02 = param2[0];
  const float_v& y02 = param2[1];
  const float_v& z02 = param2[2];

  float_v dS1[2] = {0.f, 0.f}, dS2[2]={0.f, 0.f};
  
  const float_v& dx0 = (x01 - x02);
  const float_v& dy0 = (y01 - y02);
  const float_v& dr02 = dx0*dx0 + dy0*dy0;
  const float_v& drp1  = dx0*px1 + dy0*py1;
  const float_v& dxyp1 = dx0*py1 - dy0*px1;
  const float_v& drp2  = dx0*px2 + dy0*py2;
  const float_v& dxyp2 = dx0*py2 - dy0*px2;
  const float_v& p1p2 = px1*px2 + py1*py2;
  const float_v& dp1p2 = px1*py2 - px2*py1;
  
  const float_v& k11 = (bq2*drp1 - dp1p2);
  const float_v& k21 = (bq1*(bq2*dxyp1 - p1p2) + bq2*pt12);
  const float_v& k12 = ((bq1*drp2 - dp1p2));
  const float_v& k22 = (bq2*(bq1*dxyp2 + p1p2) - bq1*pt22);
  
  const float_v& kp = (dxyp1*bq2 - dxyp2*bq1 - p1p2);
  const float_v& kd = dr02/2.f*bq1*bq2 + kp;
  const float_v& c1 = -(bq1*kd + pt12*bq2);
  const float_v& c2 = bq2*kd + pt22*bq1; 
  
  float_v d1 = pt12*pt22 - kd*kd;
  d1(d1 < float_v(Vc::Zero)) = float_v(Vc::Zero);
  d1 = sqrt( d1 );
  float_v d2 = pt12*pt22 - kd*kd;
  d2(d2 < float_v(Vc::Zero)) = float_v(Vc::Zero);
  d2 = sqrt( d2 );

  // find two points of closest approach in XY plane
  if( ! ( (!isStraight1).isEmpty() ) )
  {
    dS1[0](!isStraight1) = KFPMath::ATan2( (bq1*k11*c1 + k21*d1*bq1), (bq1*k11*d1*bq1 - k21*c1) )/bq1;
    dS1[1](!isStraight1) = KFPMath::ATan2( (bq1*k11*c1 - k21*d1*bq1), (-bq1*k11*d1*bq1 - k21*c1) )/bq1;    
  }
  if( ! ( (!isStraight2).isEmpty() ) )
  {
    dS2[0](!isStraight2) = KFPMath::ATan2( (bq2*k12*c2 + k22*d2*bq2), (bq2*k12*d2*bq2 - k22*c2) )/bq2;
    dS2[1](!isStraight2) = KFPMath::ATan2( (bq2*k12*c2 - k22*d2*bq2), (-bq2*k12*d2*bq2 - k22*c2) )/bq2;
  }
  if( ! ( isStraight1.isEmpty() ) )
  {
    dS1[0](isStraight1 && (pt12>float_v(Vc::Zero)) ) = (k11*c1 + k21*d1)/(- k21*c1);
    dS1[1](isStraight1 && (pt12>float_v(Vc::Zero)) ) = (k11*c1 - k21*d1)/(- k21*c1);
  }
  if( ! ( isStraight2.isEmpty() ) )
  {
    dS2[0](isStraight2 && (pt22>float_v(Vc::Zero)) ) = (k12*c2 + k22*d2)/(- k22*c2);
    dS2[1](isStraight2 && (pt22>float_v(Vc::Zero)) ) = (k12*c2 - k22*d2)/(- k22*c2);      
  }
  
  //select a point which is close to the primary vertex (with the smallest r)
  
  float_v dr2[2];
  for(int iP = 0; iP<2; iP++)
  {
    const float_v& bs1 = bq1*dS1[iP];
    const float_v& bs2 = bq2*dS2[iP];
    float_v sss = KFPMath::Sin(bs1), ccc = KFPMath::Cos(bs1);
    
    const float_m& bs1Big = abs(bs1) > 1.e-8f;
    const float_m& bs2Big = abs(bs2) > 1.e-8f;
    
    float_v sB(Vc::Zero), cB(Vc::Zero);
    sB(bs1Big) = sss/bq1;
    sB(!bs1Big) = ((1.f-bs1*kOvSqr6)*(1.f+bs1*kOvSqr6)*dS1[iP]);
    cB(bs1Big) = (1.f-ccc)/bq1;
    cB(!bs1Big) = .5f*sB*bs1;
  
    const float_v& x1 = param1[0] + sB*px1 + cB*py1;
    const float_v& y1 = param1[1] - cB*px1 + sB*py1;
    const float_v& z1 = param1[2] + dS1[iP]*param1[5];

    sss = KFPMath::Sin(bs2); ccc = KFPMath::Cos(bs2);

    sB(bs2Big) = sss/bq2;
    sB(!bs2Big) = ((1.f-bs2*kOvSqr6)*(1.f+bs2*kOvSqr6)*dS2[iP]);
    cB(bs2Big) = (1.f-ccc)/bq2;
    cB(!bs2Big) = .5f*sB*bs2;

    const float_v& x2 = param2[0] + sB*px2 + cB*py2;
    const float_v& y2 = param2[1] - cB*px2 + sB*py2;
    const float_v& z2 = param2[2] + dS2[iP]*param2[5];

    float_v dx = (x1-x2);
    float_v dy = (y1-y2);
    float_v dz = (z1-z2);
    
    dr2[iP] = dx*dx + dy*dy + dz*dz;
  }
  
  
  const float_m isFirstRoot = (dr2[0] < dr2[1]);
  
 // if(!(isFirstRoot.isEmpty()))
  {
    dS[0](isFirstRoot)  = dS1[0];
    dS[1](isFirstRoot) = dS2[0];    
  }
 // if( !( (!isFirstRoot).isEmpty() ) )
  {
    dS[0](!isFirstRoot)  = dS1[1];
    dS[1](!isFirstRoot) = dS2[1];    
  }
        
  //find correct parts of helices
  int_v n1(Vc::Zero);
  int_v n2(Vc::Zero);
//  float_v dzMin = abs( (z01-z02) + dS[0]*pz1 - dS[1]*pz2 );
  const float_v pi2(6.283185307f);
  
//   //TODO optimise for loops for neutral particles
//   const float_v& i1Float = -bq1/pi2*(z01/pz1+dS[0]);
//   for(int di1=-1; di1<=1; di1++)
//   {
//     int_v i1(Vc::Zero);
//     i1(int_m(!isStraight1)) = int_v(i1Float) + di1;
//     
//     const float_v& i2Float = ( ((z01-z02) + (dS[0]+pi2*i1/bq1)*pz1)/pz2 - dS[1]) * bq2/pi2;
//     for(int di2 = -1; di2<=1; di2++)
//     {
//       int_v i2(Vc::Zero);
//       i2(int_m(!isStraight2)) = int_v(i2Float) + di2;
//       
//       const float_v& z1 = z01 + (dS[0]+pi2*i1/bq1)*pz1;
//       const float_v& z2 = z02 + (dS[1]+pi2*i2/bq2)*pz2;
//       const float_v& dz = abs( z1-z2 );
//     
//       n1(int_m(dz < dzMin)) = i1;
//       n2(int_m(dz < dzMin)) = i2;
//       dzMin(dz < dzMin) = dz;
//     }
//   }
// 
//   dS[0](!isStraight1) += float_v(n1)*pi2/bq1;
//   dS[1](!isStraight2) += float_v(n2)*pi2/bq2;

  //Line correction
  {
    const float_v& bs1 = bq1*dS[0];
    const float_v& bs2 = bq2*dS[1];
    float_v sss = KFPMath::Sin(bs1), ccc = KFPMath::Cos(bs1);
    
    const float_m& bs1Big = abs(bs1) > 1.e-8f;
    const float_m& bs2Big = abs(bs2) > 1.e-8f;
    
    float_v sB(0.f), cB(0.f);
    sB(bs1Big) = sss/bq1;
    cB(bs1Big) = (1.f-ccc)/bq1;
    sB(!bs1Big) = ((1.f-bs1*kOvSqr6)*(1.f+bs1*kOvSqr6)*dS[0]);
    cB(!bs1Big) = .5f*sB*bs1;
  
    const float_v& x1 = x01 + sB*px1 + cB*py1;
    const float_v& y1 = y01 - cB*px1 + sB*py1;
    const float_v& z1 = z01 + dS[0]*pz1;
    const float_v& ppx1 =  ccc*px1 + sss*py1;
    const float_v& ppy1 = -sss*px1 + ccc*py1;
    const float_v& ppz1 = pz1;
    
    float_v sss1 = KFPMath::Sin(bs2), ccc1 = KFPMath::Cos(bs2);

    float_v sB1(0.f), cB1(0.f);
    sB1(bs2Big) = sss1/bq2;
    cB1(bs2Big) = (1.f-ccc1)/bq2;
    sB1(!bs2Big) = ((1.f-bs2*kOvSqr6)*(1.f+bs2*kOvSqr6)*dS[1]);
    cB1(!bs2Big) = .5f*sB1*bs2;

    const float_v& x2 = x02 + sB1*px2 + cB1*py2;
    const float_v& y2 = y02 - cB1*px2 + sB1*py2;
    const float_v& z2 = z02 + dS[1]*pz2;
    const float_v& ppx2 =  ccc1*px2 + sss1*py2;
    const float_v& ppy2 = -sss1*px2 + ccc1*py2;    
    const float_v& ppz2 = pz2;

    const float_v& p12  = ppx1*ppx1 + ppy1*ppy1 + ppz1*ppz1;
    const float_v& p22  = ppx2*ppx2 + ppy2*ppy2 + ppz2*ppz2;
    const float_v& lp1p2 = ppx1*ppx2 + ppy1*ppy2 + ppz1*ppz2;

    const float_v& dx = (x2 - x1);
    const float_v& dy = (y2 - y1);
    const float_v& dz = (z2 - z1);
    
    const float_v& ldrp1 = ppx1*dx + ppy1*dy + ppz1*dz;
    const float_v& ldrp2 = ppx2*dx + ppy2*dy + ppz2*dz;

    float_v detp =  lp1p2*lp1p2 - p12*p22;
    detp(abs(detp)<1.e-4f) = 1; //TODO correct!!!
    
    dS[0] += (ldrp2*lp1p2 - ldrp1*p22) /detp;
    dS[1] += (ldrp2*p12 - ldrp1*lp1p2)/detp;    
  }
}

void KFParticleBaseSIMD::GetDStoParticleBy( float_v B, const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] ) const
{
  /** Calculates dS = l/p parameters for two particles, where \n
   ** 1) l - signed distance to the DCA point with the other particle;\n
   ** 2) p - momentum of the particle; \n
   ** under the assumption of the constant homogeneous field By. dS[0] is the transport parameter for the current particle,
   ** dS[1] - for the particle "p".
   ** Also calculates partial derivatives dsdr of the parameters dS[0] and dS[1] over the state vectors of the particles:\n
   ** 1) dsdr[0][6] = d(dS[0])/d(param1);\n
   ** 2) dsdr[1][6] = d(dS[0])/d(param2);\n
   ** 3) dsdr[2][6] = d(dS[1])/d(param1);\n
   ** 4) dsdr[3][6] = d(dS[1])/d(param2);\n
   ** where param1 are parameters of the current particle fP and
   ** param2 are parameters of the second particle p.fP.
   ** The particle parameters are transformed to the coordinate system, where the main component of the magnetic field
   ** By is directed along the Z axis: x->x, y->-z, z->y, and the function GetDStoPointBz() is called. Derivatives dsdr are transformed back
   ** to the coordinate system of the particle.
   ** \param[in] B - magnetic field By
   ** \param[in] p - second particle
   ** \param[out] dS[2] - transport parameters dS for the current particle (dS[0]) and the second particle "p" (dS[1])
   ** \param[out] dsdr[4][6] - partial derivatives of the parameters dS[0] and dS[1] over the state vectors of the both particles
   **/
  
  const float_v param1[6] = { fP[0], -fP[2], fP[1], fP[3], -fP[5], fP[4] };
  const float_v param2[6] = { p.fP[0], -p.fP[2], p.fP[1], p.fP[3], -p.fP[5], p.fP[4] };
  
  float_v dsdrBz[4][6];
  for(int i1=0; i1<4; i1++)
    for(int i2=0; i2<6; i2++)
      dsdrBz[i1][i2] = 0.f;
  
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

void KFParticleBaseSIMD::GetDStoParticleBy( float_v B, const KFParticleBaseSIMD &p, float_v dS[2] ) const
{
  /** Calculates dS = l/p parameters for two particles, where \n
   ** 1) l - signed distance to the DCA point with the other particle;\n
   ** 2) p - momentum of the particle; \n
   ** under the assumption of the constant homogeneous field By. dS[0] is the transport parameter for the current particle,
   ** dS[1] - for the particle "p".
   ** The particle parameters are transformed to the coordinate system, where the main component of the magnetic field
   ** By is directed along the Z axis: x->x, y->-z, z->y, and the function GetDStoPointBz() is called. 
   ** \param[in] B - magnetic field By
   ** \param[in] p - second particle
   ** \param[out] dS[2] - transport parameters dS for the current particle (dS[0]) and the second particle "p" (dS[1])
   **/
  
  const float_v param1[6] = { fP[0], -fP[2], fP[1], fP[3], -fP[5], fP[4] };
  const float_v param2[6] = { p.fP[0], -p.fP[2], p.fP[1], p.fP[3], -p.fP[5], p.fP[4] };
  
  GetDStoParticleBz(B, p, dS, param1, param2);
}

void KFParticleBaseSIMD::GetDStoParticleB( float_v B[3], const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] ) const
{
  /** Calculates dS = l/p parameters for two particles, where \n
   ** 1) l - signed distance to the DCA point with the other particle;\n
   ** 2) p - momentum of the particle; \n
   ** under the assumption of the constant homogeneous field By. dS[0] is the transport parameter for the current particle,
   ** dS[1] - for the particle "p".
   ** Also calculates partial derivatives dsdr of the parameters dS[0] and dS[1] over the state vectors of the particles:\n
   ** 1) dsdr[0][6] = d(dS[0])/d(param1);\n
   ** 2) dsdr[1][6] = d(dS[0])/d(param2);\n
   ** 3) dsdr[2][6] = d(dS[1])/d(param1);\n
   ** 4) dsdr[3][6] = d(dS[1])/d(param2);\n
   ** where param1 are parameters of the current particle fP and
   ** param2 are parameters of the second particle p.fP.
   ** The particle parameters are transformed to the coordinate system, where the magnetic field B
   ** is directed along the Z axis and the function GetDStoPointBz() is called. Derivatives dsdr are transformed back
   ** to the coordinate system of the particle.
   ** \param[in] B - magnetic field By
   ** \param[in] p - second particle
   ** \param[out] dS[2] - transport parameters dS for the current particle (dS[0]) and the second particle "p" (dS[1])
   ** \param[out] dsdr[4][6] - partial derivatives of the parameters dS[0] and dS[1] over the state vectors of the both particles
   **/
  
  const float_v& Bx = B[0];
  const float_v& By = B[1];
  const float_v& Bz = B[2];
  
  const float_v& Bxz = sqrt(Bx*Bx + Bz*Bz);
  const float_v& Br = sqrt(Bx*Bx + By*By + Bz*Bz);
    
  float_v cosA = 1.f;
  float_v sinA = 0.f;

  cosA( abs(Bxz) > 1.e-8f ) = Bz/Bxz;
  sinA( abs(Bxz) > 1.e-8f ) = Bx/Bxz;
  
  const float_v& sinP = By/Br;
  const float_v& cosP = Bxz/Br;

  
  const float_v param1[6] = { cosA*fP[0] - sinA*fP[2], 
                             -sinA*sinP*fP[0] + cosP*fP[1] - cosA*sinP*fP[2], 
                              cosP*sinA*fP[0] + sinP*fP[1] + cosA*cosP*fP[2],
                              cosA*fP[3] - sinA*fP[5], 
                             -sinA*sinP*fP[3] + cosP*fP[4] - cosA*sinP*fP[5], 
                              cosP*sinA*fP[3] + sinP*fP[4] + cosA*cosP*fP[5]};
  const float_v param2[6] = { cosA*p.fP[0] - sinA*p.fP[2], 
                             -sinA*sinP*p.fP[0] + cosP*p.fP[1] - cosA*sinP*p.fP[2], 
                              cosP*sinA*p.fP[0] + sinP*p.fP[1] + cosA*cosP*p.fP[2],
                              cosA*p.fP[3] - sinA*p.fP[5], 
                             -sinA*sinP*p.fP[3] + cosP*p.fP[4] - cosA*sinP*p.fP[5], 
                              cosP*sinA*p.fP[3] + sinP*p.fP[4] + cosA*cosP*p.fP[5]};

  float_v dsdrBz[4][6];
  for(int i1=0; i1<4; i1++)
    for(int i2=0; i2<6; i2++)
      dsdrBz[i1][i2] = 0.f;
    
  GetDStoParticleBz(Br, p, dS, dsdrBz, param1, param2);
  
  for(int iDs=0; iDs<4; iDs++)
  {
    dsdr[iDs][0] =  dsdrBz[iDs][0]*cosA - dsdrBz[iDs][1]*sinA*sinP + dsdrBz[iDs][2]*sinA*cosP;
    dsdr[iDs][1] =                        dsdrBz[iDs][1]*cosP      + dsdrBz[iDs][2]*sinP;
    dsdr[iDs][2] = -dsdrBz[iDs][0]*sinA - dsdrBz[iDs][1]*cosA*sinP + dsdrBz[iDs][2]*cosA*cosP;
    dsdr[iDs][3] =  dsdrBz[iDs][3]*cosA - dsdrBz[iDs][4]*sinA*sinP + dsdrBz[iDs][5]*sinA*cosP;
    dsdr[iDs][4] =                        dsdrBz[iDs][4]*cosP      + dsdrBz[iDs][5]*sinP;
    dsdr[iDs][5] = -dsdrBz[iDs][3]*sinA - dsdrBz[iDs][4]*cosA*sinP + dsdrBz[iDs][5]*cosA*cosP;
  }
}

void KFParticleBaseSIMD::GetDStoParticleB( float_v B[3], const KFParticleBaseSIMD &p, float_v dS[2] ) const
{
  /** Calculates dS = l/p parameters for two particles, where \n
   ** 1) l - signed distance to the DCA point with the other particle;\n
   ** 2) p - momentum of the particle; \n
   ** under the assumption of the constant homogeneous field By. dS[0] is the transport parameter for the current particle,
   ** dS[1] - for the particle "p".
   ** The particle parameters are transformed to the coordinate system, where the magnetic field B
   ** is directed along the Z axis and the function GetDStoPointBz() is called.
   ** \param[in] B - magnetic field By
   ** \param[in] p - second particle
   ** \param[out] dS[2] - transport parameters dS for the current particle (dS[0]) and the second particle "p" (dS[1])
   **/
  
  const float_v& Bx = B[0];
  const float_v& By = B[1];
  const float_v& Bz = B[2];
  
  const float_v& Bxz = sqrt(Bx*Bx + Bz*Bz);
  const float_v& Br = sqrt(Bx*Bx + By*By + Bz*Bz);
    
  float_v cosA = 1.f;
  float_v sinA = 0.f;

  cosA( abs(Bxz) > 1.e-8f ) = Bz/Bxz;
  sinA( abs(Bxz) > 1.e-8f ) = Bx/Bxz;
  
  const float_v& sinP = By/Br;
  const float_v& cosP = Bxz/Br;

  
  const float_v param1[6] = { cosA*fP[0] - sinA*fP[2], 
                             -sinA*sinP*fP[0] + cosP*fP[1] - cosA*sinP*fP[2], 
                              cosP*sinA*fP[0] + sinP*fP[1] + cosA*cosP*fP[2],
                              cosA*fP[3] - sinA*fP[5], 
                             -sinA*sinP*fP[3] + cosP*fP[4] - cosA*sinP*fP[5], 
                              cosP*sinA*fP[3] + sinP*fP[4] + cosA*cosP*fP[5]};
  const float_v param2[6] = { cosA*p.fP[0] - sinA*p.fP[2], 
                             -sinA*sinP*p.fP[0] + cosP*p.fP[1] - cosA*sinP*p.fP[2], 
                              cosP*sinA*p.fP[0] + sinP*p.fP[1] + cosA*cosP*p.fP[2],
                              cosA*p.fP[3] - sinA*p.fP[5], 
                             -sinA*sinP*p.fP[3] + cosP*p.fP[4] - cosA*sinP*p.fP[5], 
                              cosP*sinA*p.fP[3] + sinP*p.fP[4] + cosA*cosP*p.fP[5]};

  GetDStoParticleBz(Br, p, dS, param1, param2);
}

void KFParticleBaseSIMD::GetDStoParticleLine( const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] ) const
{
  /** Calculates dS = l/p parameters for two particles, where \n
   ** 1) l - signed distance to the DCA point with the other particle;\n
   ** 2) p - momentum of the particle; \n
   ** under the assumption of the straight line trajectory. Is used for particles with charge 0 or in case of zero magnetic field.
   ** dS[0] is the transport parameter for the current particle, dS[1] - for the particle "p".
   ** Also calculates partial derivatives dsdr of the parameters dS[0] and dS[1] over the state vectors of the particles:\n
   ** 1) dsdr[0][6] = d(dS[0])/d(param1);\n
   ** 2) dsdr[1][6] = d(dS[0])/d(param2);\n
   ** 3) dsdr[2][6] = d(dS[1])/d(param1);\n
   ** 4) dsdr[3][6] = d(dS[1])/d(param2);\n
   ** where param1 are parameters of the current particle fP and
   ** param2 are parameters of the second particle p.fP.
   ** \param[in] p - second particle
   ** \param[out] dS[2] - transport parameters dS for the current particle (dS[0]) and the second particle "p" (dS[1])
   ** \param[out] dsdr[4][6] - partial derivatives of the parameters dS[0] and dS[1] over the state vectors of the both particles
   **/
  
  float_v p12 = fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5];
  float_v p22 = p.fP[3]*p.fP[3] + p.fP[4]*p.fP[4] + p.fP[5]*p.fP[5];
  float_v p1p2 = fP[3]*p.fP[3] + fP[4]*p.fP[4] + fP[5]*p.fP[5];

  float_v drp1 = fP[3]*(p.fP[0]-fP[0]) + fP[4]*(p.fP[1]-fP[1]) + fP[5]*(p.fP[2]-fP[2]);
  float_v drp2 = p.fP[3]*(p.fP[0]-fP[0]) + p.fP[4]*(p.fP[1]-fP[1]) + p.fP[5]*(p.fP[2]-fP[2]);

  float_v detp =  p1p2*p1p2 - p12*p22;
  detp( abs(detp)<float_v(1.e-4f) ) = float_v(1.f); //TODO correct!!!

  dS[0]  = (drp2*p1p2 - drp1*p22) /detp;
  dS[1] = (drp2*p12  - drp1*p1p2)/detp;
  
  const float_v x01 = fP[0];
  const float_v y01 = fP[1];
  const float_v z01 = fP[2];
  const float_v px1 = fP[3];
  const float_v py1 = fP[4];
  const float_v pz1 = fP[5];

  const float_v x02 = p.fP[0];
  const float_v y02 = p.fP[1];
  const float_v z02 = p.fP[2];
  const float_v px2 = p.fP[3];
  const float_v py2 = p.fP[4];
  const float_v pz2 = p.fP[5];
  
  const float_v drp1_dr1[6]  = {-px1, -py1, -pz1, -x01 + x02, -y01 + y02, -z01 + z02};
  const float_v drp1_dr2[6]  = {px1, py1, pz1, 0.f, 0.f, 0.f};
  const float_v drp2_dr1[6]  = {-px2, -py2, -pz2, 0.f, 0.f, 0.f};
  const float_v drp2_dr2[6]  = {px2, py2, pz2, -x01 + x02, -y01 + y02, -z01 + z02};
  const float_v dp1p2_dr1[6] = {0.f, 0.f, 0.f, px2, py2, pz2};
  const float_v dp1p2_dr2[6] = {0.f, 0.f, 0.f, px1, py1, pz1};
  const float_v dp12_dr1[6]  = {0.f, 0.f, 0.f, 2.f*px1, 2.f*py1, 2.f*pz1};
  const float_v dp12_dr2[6]  = {0.f, 0.f, 0.f, 0.f, 0.f, 0.f};
  const float_v dp22_dr1[6]  = {0.f, 0.f, 0.f, 0.f, 0.f, 0.f};
  const float_v dp22_dr2[6]  = {0.f, 0.f, 0.f, 2.f*px2, 2.f*py2, 2.f*pz2};
  const float_v ddetp_dr1[6] = {0.f, 0.f, 0.f, -2.f*p22*px1 + 2.f*p1p2*px2, -2.f*p22*py1 + 2.f*p1p2*py2, -2.f*p22*pz1 + 2.f*p1p2*pz2};
  const float_v ddetp_dr2[6] = {0.f, 0.f, 0.f, 2.f*p1p2*px1 - 2.f*p12*px2,   2.f*p1p2*py1 - 2.f*p12*py2, 2.f*p1p2*pz1 - 2.f*p12*pz2};
  
  
  float_v da1_dr1[6], da1_dr2[6], da2_dr1[6], da2_dr2[6];
  
  const float_v a1 = drp2*p1p2 - drp1*p22;
  const float_v a2 = drp2*p12  - drp1*p1p2;
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

void KFParticleBaseSIMD::GetDStoParticleLine( const KFParticleBaseSIMD &p, float_v dS[2] ) const
{
  /** Calculates dS = l/p parameters for two particles, where \n
   ** 1) l - signed distance to the DCA point with the other particle;\n
   ** 2) p - momentum of the particle; \n
   ** under the assumption of the straight line trajectory. Is used for particles with charge 0 or in case of zero magnetic field.
   ** dS[0] is the transport parameter for the current particle, dS[1] - for the particle "p".
   ** \param[in] p - second particle
   ** \param[out] dS[2] - transport parameters dS for the current particle (dS[0]) and the second particle "p" (dS[1])
   **/
  
  float_v p12 = fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5];
  float_v p22 = p.fP[3]*p.fP[3] + p.fP[4]*p.fP[4] + p.fP[5]*p.fP[5];
  float_v p1p2 = fP[3]*p.fP[3] + fP[4]*p.fP[4] + fP[5]*p.fP[5];

  float_v drp1 = fP[3]*(p.fP[0]-fP[0]) + fP[4]*(p.fP[1]-fP[1]) + fP[5]*(p.fP[2]-fP[2]);
  float_v drp2 = p.fP[3]*(p.fP[0]-fP[0]) + p.fP[4]*(p.fP[1]-fP[1]) + p.fP[5]*(p.fP[2]-fP[2]);

  float_v detp =  p1p2*p1p2 - p12*p22;
  detp( abs(detp)<float_v(1.e-4f) ) = float_v(1.f); //TODO correct!!!

  dS[0]  = (drp2*p1p2 - drp1*p22) /detp;
  dS[1] = (drp2*p12  - drp1*p1p2)/detp;
}

void KFParticleBaseSIMD::GetDStoParticleCBM( const KFParticleBaseSIMD &p, float_v dS[2], float_v dsdr[4][6] ) const
{
  /** Calculates dS = l/p parameters for two particles, where \n
   ** 1) l - signed distance to the DCA point with the other particle;\n
   ** 2) p - momentum of the particle; \n
   ** in case of the CBM-like nonhomogeneous magnetic field. 
   ** dS[0] is the transport parameter for the current particle, dS[1] - for the particle "p".
   ** Also calculates partial derivatives dsdr of the parameters dS[0] and dS[1] over the state vectors of the particles:\n
   ** 1) dsdr[0][6] = d(dS[0])/d(param1);\n
   ** 2) dsdr[1][6] = d(dS[0])/d(param2);\n
   ** 3) dsdr[2][6] = d(dS[1])/d(param1);\n
   ** 4) dsdr[3][6] = d(dS[1])/d(param2);\n
   ** where param1 are parameters of the current particle fP and
   ** param2 are parameters of the second particle p.fP.
   ** For this the y-component of the magnetic field at the position of the current particle is obtained and
   ** the GetDStoParticleBy() is called. It is assumed that particles are already close to each other and that the difference 
   ** in magnetic field approximation between two particles can be neglected. If the charge of both particles 
   ** is zero or if the magnetic field is zero the function GetDStoParticleLine() is called.
   ** \param[in] p - second particle
   ** \param[out] dS[2] - transport parameters dS for the current particle (dS[0]) and the second particle "p" (dS[1])
   ** \param[out] dsdr[4][6] - partial derivatives of the parameters dS[0] and dS[1] over the state vectors of the both particles
   **/
  
  float_v fld[3];
  GetFieldValue( fP, fld );

  const float_v& bq1 = fld[1]*simd_cast<float_v>(fQ);
  const float_v& bq2 = fld[1]*simd_cast<float_v>(p.fQ);
  const float_m& isStraight1 = abs(bq1) < float_v(1.e-8f);
  const float_m& isStraight2 = abs(bq2) < float_v(1.e-8f);
  
  if( isStraight1.isFull() && isStraight2.isFull() )
    GetDStoParticleLine(p, dS, dsdr);
  else
    GetDStoParticleBy(fld[1], p, dS, dsdr);
}

void KFParticleBaseSIMD::GetDStoParticleCBM( const KFParticleBaseSIMD &p, float_v dS[2] ) const
{
  /** Calculates dS = l/p parameters for two particles, where \n
   ** 1) l - signed distance to the DCA point with the other particle;\n
   ** 2) p - momentum of the particle; \n
   ** in case of the CBM-like nonhomogeneous magnetic field. 
   ** dS[0] is the transport parameter for the current particle, dS[1] - for the particle "p".
   ** For this the y-component of the magnetic field at the position of the current particle is obtained and
   ** the GetDStoParticleBy() is called. It is assumed that particles are already close to each other and that the difference 
   ** in magnetic field approximation between two particles can be neglected. If the charge of both particles 
   ** is zero or if the magnetic field is zero the function GetDStoParticleLine() is called.
   ** \param[in] p - second particle
   ** \param[out] dS[2] - transport parameters dS for the current particle (dS[0]) and the second particle "p" (dS[1])
   **/
  
  float_v fld[3];
  GetFieldValue( fP, fld );

  const float_v& bq1 = fld[1]*simd_cast<float_v>(fQ);
  const float_v& bq2 = fld[1]*simd_cast<float_v>(p.fQ);
  const float_m& isStraight1 = abs(bq1) < float_v(1.e-8f);
  const float_m& isStraight2 = abs(bq2) < float_v(1.e-8f);
  
  if( isStraight1.isFull() && isStraight2.isFull() )
    GetDStoParticleLine(p, dS);
  else
    GetDStoParticleBy(fld[1], p, dS);
}

void KFParticleBaseSIMD::TransportCBM( float_v dS, const float_v* dsdr, float_v P[], float_v C[], float_v* dsdr1, float_v* F, float_v* F1 ) const
{  
  /** Transports the parameters and their covariance matrix of the current particle assuming CBM-like nonhomogeneous 
   ** magnetic field on the length defined by the transport parameter dS = l/p, where l is the signed distance and p is 
   ** the momentum of the current particle. The obtained parameters and covariance matrix are stored to the arrays P and 
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
  
  if( (fQ == int_v(Vc::Zero)).isFull() ){
    TransportLine( dS, dsdr, P, C, dsdr1, F, F1 );
    return;
  }

  const float_v kCLight = 0.000299792458f;

  float_v c = simd_cast<float_v>(fQ)*kCLight;

  // construct coefficients 

  float_v 
    px   = fP[3],
    py   = fP[4],
    pz   = fP[5];
      
  float_v sx=0.f, sy=0.f, sz=0.f, syy=0.f, syz=0.f, syyy=0.f, ssx=0.f, ssy=0.f, ssz=0.f, ssyy=0.f, ssyz=0.f, ssyyy=0.f;

  { // get field integrals

    float_v fld[3][3];   
    float_v p0[3], p1[3], p2[3];

    // line track approximation

    p0[0] = fP[0];
    p0[1] = fP[1];
    p0[2] = fP[2];
  
    p2[0] = fP[0] + px*dS;
    p2[1] = fP[1] + py*dS;
    p2[2] = fP[2] + pz*dS;
  
    p1[0] = 0.5f*(p0[0]+p2[0]);
    p1[1] = 0.5f*(p0[1]+p2[1]);
    p1[2] = 0.5f*(p0[2]+p2[2]);

    // first order track approximation
    {
      GetFieldValue( p0, fld[0] );
      GetFieldValue( p1, fld[1] );
      GetFieldValue( p2, fld[2] );

      float_v ssy1 = ( 7.f*fld[0][1] + 6.f*fld[1][1]-fld[2][1] )*c*dS*dS/96.f;
      float_v ssy2 = (   fld[0][1] + 2.f*fld[1][1]         )*c*dS*dS/6.f;

      p1[0] -= ssy1*pz;
      p1[2] += ssy1*px;
      p2[0] -= ssy2*pz;
      p2[2] += ssy2*px;   
    }

    GetFieldValue( p0, fld[0] );
    GetFieldValue( p1, fld[1] );
    GetFieldValue( p2, fld[2] );

    for(int iF1=0; iF1<3; iF1++)
      for(int iF2=0; iF2<3; iF2++)
        fld[iF1][iF2](abs(fld[iF1][iF2]) > float_v(100.f)) = 0.f;

    sx = c*( fld[0][0] + 4*fld[1][0] + fld[2][0] )*dS/6.f;
    sy = c*( fld[0][1] + 4*fld[1][1] + fld[2][1] )*dS/6.f;
    sz = c*( fld[0][2] + 4*fld[1][2] + fld[2][2] )*dS/6.f;

    ssx = c*( fld[0][0] + 2*fld[1][0])*dS*dS/6.f;
    ssy = c*( fld[0][1] + 2*fld[1][1])*dS*dS/6.f;
    ssz = c*( fld[0][2] + 2*fld[1][2])*dS*dS/6.f;

    float_v c2[3][3]    =   { {  5.f, -4.f, -1.f},{  44.f,  80.f,  -4.f},{ 11.f, 44.f, 5.f} }; // /=360.    
    float_v cc2[3][3]    =   { { 38.f,  8.f, -4.f},{ 148.f, 208.f, -20.f},{  3.f, 36.f, 3.f} }; // /=2520.
    for(Int_t n=0; n<3; n++)
      for(Int_t m=0; m<3; m++) 
	{
	  syz += c2[n][m]*fld[n][1]*fld[m][2];
	  ssyz += cc2[n][m]*fld[n][1]*fld[m][2];
	}
 
    syz  *= c*c*dS*dS/360.f;
    ssyz  *= c*c*dS*dS*dS/2520.f;
    
    syy  = c*( fld[0][1] + 4.f*fld[1][1] + fld[2][1] )*dS;
    syyy = syy*syy*syy / 1296.f;
    syy  = syy*syy/72.f;

    ssyy = ( fld[0][1]*( 38.f*fld[0][1] + 156.f*fld[1][1]  -   fld[2][1] )+
	    fld[1][1]*(              208.f*fld[1][1]  +16.f*fld[2][1] )+
	    fld[2][1]*(                             3.f*fld[2][1] )  
	    )*dS*dS*dS*c*c/2520.f;
    ssyyy = 
      (
       fld[0][1]*( fld[0][1]*( 85.f*fld[0][1] + 526.f*fld[1][1]  - 7.f*fld[2][1] )+
		 fld[1][1]*(             1376.f*fld[1][1]  +84.f*fld[2][1] )+
		 fld[2][1]*(                            19.f*fld[2][1] )  )+
       fld[1][1]*( fld[1][1]*(             1376.f*fld[1][1] +256.f*fld[2][1] )+
		 fld[2][1]*(                            62.f*fld[2][1] )  )+
       fld[2][1]*fld[2][1]  *(                             3.f*fld[2][1] )       
       )*dS*dS*dS*dS*c*c*c/90720.f;    
 
  }

//   float_v mJ[11];
// 
//   mJ[0]=dS-ssyy;  mJ[1]=ssx;  mJ[2]=ssyyy-ssy;
//   mJ[3]=-ssz;     mJ[4]=dS;  mJ[5]=ssx+ssyz;
// 
//   mJ[6]=1-syy;   mJ[7]=sx;  mJ[8]=syyy-sy;
//   mJ[9]=-sz;                mJ[10]=sx+syz;
// 
// 
// 
//   P[0] = fP[0] + mJ[0]*px + mJ[1]*py + mJ[2]*pz;
//   P[1] = fP[1] + mJ[3]*px + mJ[4]*py + mJ[5]*pz;
//   P[2] = fP[2] - mJ[2]*px - mJ[1]*py + mJ[0]*pz;
//   P[3] =  mJ[6]*px + mJ[7]*py + mJ[8]*pz;
//   P[4] =  mJ[9]*px +       py + mJ[10]*pz;
//   P[5] = -mJ[8]*px - mJ[7]*py + mJ[6]*pz;
//   P[6] = fP[6];
//   P[7] = fP[7];

//   if(C!=fC)
//   {
//     for(int iC=0; iC<36; iC++)
//       C[iC] = fC[iC];
//   }
// 
//   multQSQt1( mJ, C);

  float_v mJ[8][8];
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

  float_v mJds[6][6];
  for( Int_t i=0; i<6; i++ ) for( Int_t j=0; j<6; j++) mJds[i][j]=0;

  mJds[0][3]= 1.f;
  mJds[1][4]= 1.f;
  mJds[2][5]= 1.f;
  
  mJds[0][3](abs(dS)>0.f)= 1.f - 3.f*ssyy/dS;      mJds[0][4](abs(dS)>0.f)= 2.f*ssx/dS; mJds[0][5](abs(dS)>0.f)= (4.f*ssyyy-2.f*ssy)/dS;
  mJds[1][3](abs(dS)>0.f)= -2.f*ssz/dS;            mJds[1][4](abs(dS)>0.f)= 1.f;        mJds[1][5](abs(dS)>0.f)= (2.f*ssx + 3.f*ssyz)/dS;
  mJds[2][3](abs(dS)>0.f)= (2.f*ssy-4.f*ssyyy)/dS; mJds[2][4](abs(dS)>0.f)=-2.f*ssx/dS; mJds[2][5](abs(dS)>0.f)= 1.f - 3.f*ssyy/dS;
  
  mJds[3][3](abs(dS)>0.f)= -2.f*syy/dS;         mJds[3][4](abs(dS)>0.f)= sx/dS;  mJds[3][5](abs(dS)>0.f)= 3.f*syyy/dS - sy/dS;
  mJds[4][3](abs(dS)>0.f)= -sz/dS;              mJds[4][4](abs(dS)>0.f)=0.f;     mJds[4][5](abs(dS)>0.f) = sx/dS + 2.f*syz/dS;
  mJds[5][3](abs(dS)>0.f)= sy/dS - 3.f*syyy/dS; mJds[5][4](abs(dS)>0.f)=-sx/dS;  mJds[5][5](abs(dS)>0.f)= -2.f*syy/dS;
  
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

void KFParticleBaseSIMD::TransportCBM( float_v dS, float_v P[] ) const
{  
  /** Transports the parameters of the current particle assuming CBM-like nonhomogeneous 
   ** magnetic field on the length defined by the transport parameter dS = l/p, where l is the signed distance and p is 
   ** the momentum of the current particle. The obtained parameters and covariance matrix are stored to the array P. 
   ** P can be set to the parameters fP. In this
   ** case the particle parameters will be modified.
   ** \param[in] dS - transport parameter which defines the distance to which particle should be transported
   ** \param[out] P[8] - array, where transported parameters should be stored
   **/ 
  if( (fQ == int_v(Vc::Zero)).isFull() ){
    TransportLine( dS, P );
    return;
  }

  const float_v kCLight = 0.000299792458f;

  float_v c = simd_cast<float_v>(fQ)*kCLight;

  // construct coefficients 

  float_v 
    px   = fP[3],
    py   = fP[4],
    pz   = fP[5];
      
  float_v sx=0.f, sy=0.f, sz=0.f, syy=0.f, syz=0.f, syyy=0.f, ssx=0.f, ssy=0.f, ssz=0.f, ssyy=0.f, ssyz=0.f, ssyyy=0.f;

  { // get field integrals

    float_v fld[3][3];   
    float_v p0[3], p1[3], p2[3];

    // line track approximation

    p0[0] = fP[0];
    p0[1] = fP[1];
    p0[2] = fP[2];
  
    p2[0] = fP[0] + px*dS;
    p2[1] = fP[1] + py*dS;
    p2[2] = fP[2] + pz*dS;
  
    p1[0] = 0.5f*(p0[0]+p2[0]);
    p1[1] = 0.5f*(p0[1]+p2[1]);
    p1[2] = 0.5f*(p0[2]+p2[2]);

    // first order track approximation
    {
      GetFieldValue( p0, fld[0] );
      GetFieldValue( p1, fld[1] );
      GetFieldValue( p2, fld[2] );

      float_v ssy1 = ( 7.f*fld[0][1] + 6.f*fld[1][1]-fld[2][1] )*c*dS*dS/96.f;
      float_v ssy2 = (   fld[0][1] + 2.f*fld[1][1]         )*c*dS*dS/6.f;

      p1[0] -= ssy1*pz;
      p1[2] += ssy1*px;
      p2[0] -= ssy2*pz;
      p2[2] += ssy2*px;   
    }

    GetFieldValue( p0, fld[0] );
    GetFieldValue( p1, fld[1] );
    GetFieldValue( p2, fld[2] );

    for(int iF1=0; iF1<3; iF1++)
      for(int iF2=0; iF2<3; iF2++)
        fld[iF1][iF2](abs(fld[iF1][iF2]) > float_v(100.f)) = 0.f;

    sx = c*( fld[0][0] + 4*fld[1][0] + fld[2][0] )*dS/6.f;
    sy = c*( fld[0][1] + 4*fld[1][1] + fld[2][1] )*dS/6.f;
    sz = c*( fld[0][2] + 4*fld[1][2] + fld[2][2] )*dS/6.f;

    ssx = c*( fld[0][0] + 2*fld[1][0])*dS*dS/6.f;
    ssy = c*( fld[0][1] + 2*fld[1][1])*dS*dS/6.f;
    ssz = c*( fld[0][2] + 2*fld[1][2])*dS*dS/6.f;

    float_v c2[3][3]    =   { {  5.f, -4.f, -1.f},{  44.f,  80.f,  -4.f},{ 11.f, 44.f, 5.f} }; // /=360.    
    float_v cc2[3][3]    =   { { 38.f,  8.f, -4.f},{ 148.f, 208.f, -20.f},{  3.f, 36.f, 3.f} }; // /=2520.
    for(Int_t n=0; n<3; n++)
      for(Int_t m=0; m<3; m++) 
        {
          syz += c2[n][m]*fld[n][1]*fld[m][2];
          ssyz += cc2[n][m]*fld[n][1]*fld[m][2];
        }
 
    syz  *= c*c*dS*dS/360.f;
    ssyz  *= c*c*dS*dS*dS/2520.f;
    
    syy  = c*( fld[0][1] + 4.f*fld[1][1] + fld[2][1] )*dS;
    syyy = syy*syy*syy / 1296.f;
    syy  = syy*syy/72.f;

    ssyy = ( fld[0][1]*( 38.f*fld[0][1] + 156.f*fld[1][1]  -   fld[2][1] )+
            fld[1][1]*(              208.f*fld[1][1]  +16.f*fld[2][1] )+
            fld[2][1]*(                             3.f*fld[2][1] )  
            )*dS*dS*dS*c*c/2520.f;
    ssyyy = 
      (
       fld[0][1]*( fld[0][1]*( 85.f*fld[0][1] + 526.f*fld[1][1]  - 7.f*fld[2][1] )+
                 fld[1][1]*(             1376.f*fld[1][1]  +84.f*fld[2][1] )+
                 fld[2][1]*(                            19.f*fld[2][1] )  )+
       fld[1][1]*( fld[1][1]*(             1376.f*fld[1][1] +256.f*fld[2][1] )+
                 fld[2][1]*(                            62.f*fld[2][1] )  )+
       fld[2][1]*fld[2][1]  *(                             3.f*fld[2][1] )       
       )*dS*dS*dS*dS*c*c*c/90720.f;    
 
  }

  float_v mJ[8][8];
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
}

void KFParticleBaseSIMD::TransportBz( float_v Bz, float_v dS, const float_v* dsdr, float_v P[], float_v C[], float_v* dsdr1, float_v* F, float_v* F1 ) const 
{ 
  /** Transports the parameters and their covariance matrix of the current particle assuming constant homogeneous 
   ** magnetic field Bz on the length defined by the transport parameter dS = l/p, where l is the signed distance and p is 
   ** the momentum of the current particle. The obtained parameters and covariance matrix are stored to the arrays P and 
   ** C respectively. P and C can be set to the parameters fP and covariance matrix fC of the current particle. In this
   ** case the particle parameters will be modified. Dependence of the transport parameter dS on the state vector of the
   ** current particle is taken into account in the covariance matrix using partial derivatives dsdr = d(dS)/d(fP). If
   ** a pointer to F is initialised the transport jacobian F = d(fP new)/d(fP old) is stored.
   ** Since dS can depend on the state vector r1 of other particle or vertex, the corelation matrix 
   ** F1 = d(fP new)/d(r1) can be optionally calculated if a pointer F1 is provided.
   *  Parameters F and F1 should be either both initialised or both set to null pointer.
   ** \param[in] Bz - z-component of the constant homogeneous magnetic field Bz
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
  
  const float_v kCLight = 0.000299792458f;
  Bz = Bz*simd_cast<float_v>(fQ)*kCLight;
  float_v bs= Bz*dS;
  float_v s = sin(bs), c = cos(bs);

  float_v sB(Vc::Zero), cB(Vc::Zero);

  const float_v kOvSqr6 = 1.f/sqrt(float_v(6.f));
  const float_v LocalSmall = 1.e-10f;

  Bz(abs(bs) <= LocalSmall) = LocalSmall;
  sB(LocalSmall < abs(bs)) = s/Bz;
  sB(LocalSmall >= abs(bs)) = (1.f-bs*kOvSqr6)*(1.f+bs*kOvSqr6)*dS;
  cB(LocalSmall < abs(bs)) = (1.f-c)/Bz;
  cB(LocalSmall >= abs(bs)) = .5f*sB*bs;
  
  float_v px = fP[3];
  float_v py = fP[4];
  float_v pz = fP[5];

  P[0] = fP[0] + sB*px + cB*py;
  P[1] = fP[1] - cB*px + sB*py;
  P[2] = fP[2] +  dS*pz;
  P[3] =          c*px + s*py;
  P[4] =         -s*px + c*py;
  P[5] = fP[5];
  P[6] = fP[6];
  P[7] = fP[7];

  float_v mJ[8][8];
  for( Int_t i=0; i<8; i++ ) for( Int_t j=0; j<8; j++) mJ[i][j]=0;

  for(int i=0; i<8; i++) mJ[i][i]=1;
  mJ[0][3] =  sB; mJ[0][4] = cB;
  mJ[1][3] = -cB; mJ[1][4] = sB;
  mJ[2][5] = dS;
  mJ[3][3] =  c; mJ[3][4] = s;
  mJ[4][3] = -s; mJ[4][4] = c;
  
  
  float_v mJds[6][6];
  for( Int_t i=0; i<6; i++ ) for( Int_t j=0; j<6; j++) mJds[i][j]=0;
  mJds[0][3] =  c; mJds[0][4] = s;
  mJds[1][3] = -s; mJds[1][4] = c;
  mJds[2][5] = 1;
  mJds[3][3] = -Bz*s; mJds[3][4] =  Bz*c;
  mJds[4][3] = -Bz*c; mJds[4][4] = -Bz*s;
  
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

void KFParticleBaseSIMD::TransportBz( float_v Bz, float_v dS, float_v P[] ) const 
{ 
  /** Transports the parameters of the current particle assuming constant homogeneous 
   ** magnetic field Bz on the length defined by the transport parameter dS = l/p, where l is the signed distance and p is 
   ** the momentum of the current particle. The obtained parameters are stored to the array P. 
   ** P be set to the parameters fP of the current particle. In this
   ** case the particle parameters will be modified. 
   ** \param[in] Bz - z-component of the constant homogeneous magnetic field Bz
   ** \param[in] dS - transport parameter which defines the distance to which particle should be transported
   ** \param[out] P[8] - array, where transported parameters should be stored
   **/ 
  
  const float_v kCLight = 0.000299792458f;
  Bz = Bz*simd_cast<float_v>(fQ)*kCLight;
  float_v bs= Bz*dS;
  float_v s = KFPMath::Sin(bs), c = KFPMath::Cos(bs);

  float_v sB(Vc::Zero), cB(Vc::Zero);

  const float_v kOvSqr6 = 1.f/sqrt(float_v(6.f));
  const float_v LocalSmall = 1.e-10f;

  Bz(abs(bs) <= LocalSmall) = LocalSmall;
  sB(LocalSmall < abs(bs)) = s/Bz;
  sB(LocalSmall >= abs(bs)) = (1.f-bs*kOvSqr6)*(1.f+bs*kOvSqr6)*dS;
  cB(LocalSmall < abs(bs)) = (1.f-c)/Bz;
  cB(LocalSmall >= abs(bs)) = .5f*sB*bs;
  
  float_v px = fP[3];
  float_v py = fP[4];
  float_v pz = fP[5];

  P[0] = fP[0] + sB*px + cB*py;
  P[1] = fP[1] - cB*px + sB*py;
  P[2] = fP[2] +  dS*pz;
  P[3] =          c*px + s*py;
  P[4] =         -s*px + c*py;
  P[5] = fP[5];
  P[6] = fP[6];
  P[7] = fP[7];
}



float_v KFParticleBaseSIMD::GetDistanceFromVertex( const KFParticleBaseSIMD &Vtx ) const
{
  /** Returns the DCA distance from vertex in the KFParticle format in 3D.
   ** \param[in] Vtx - the vertex in the KFParticle format
   **/
  
  return GetDistanceFromVertex( Vtx.fP );
}

float_v KFParticleBaseSIMD::GetDistanceFromVertex( const float_v vtx[] ) const
{
  /** Returns the DCA distance from vertex in 3D.
   ** \param[in] vtx[3] - the vertex coordinates {X, Y, Z}
   **/
  
  float_v mP[8], mC[36];  
  float_v dsdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  const float_v dS = GetDStoPoint(vtx, dsdr);
  Transport( dS, dsdr, mP, mC );
  float_v d[3]={ vtx[0]-mP[0], vtx[1]-mP[1], vtx[2]-mP[2]};
  return sqrt( d[0]*d[0]+d[1]*d[1]+d[2]*d[2] );
}

float_v KFParticleBaseSIMD::GetDistanceFromParticle( const KFParticleBaseSIMD &p ) const
{ 
  /** Returns the DCA distance from another particle p.
   ** \param[in] p - the second particle
   **/
  
  float_v dS[2];
  GetDStoParticleFast( p, dS );   
  float_v mP[8], mP1[8];
  TransportFast( dS[0], mP ); 
  p.TransportFast( dS[1], mP1 ); 
  float_v dx = mP[0]-mP1[0]; 
  float_v dy = mP[1]-mP1[1]; 
  float_v dz = mP[2]-mP1[2]; 
  return sqrt(dx*dx+dy*dy+dz*dz);  
}

float_v KFParticleBaseSIMD::GetDeviationFromVertex( const KFParticleBaseSIMD &Vtx ) const
{
  /** Returns Chi2 deviation of the current particle from the vertex in the KFParticle format in 3D.
   ** \param[in] Vtx - the vertex in KFPartcile format
   **/
  
  return GetDeviationFromVertex( Vtx.fP, Vtx.fC );
}


float_v KFParticleBaseSIMD::GetDeviationFromVertex( const float_v v[], const float_v Cv[] ) const
{
  /** Returns Chi2 deviation of the current particle from the vertex v with the covariance matrix Cv in 3D.
   ** \param[in] v[3] - coordinates of the vertex {X, Y, Z}
   ** \param[in] Cv[6] - covariance matrix of the vertex {Cxx, Cxy, Cyy, Cxz, Czy, Czz}
   **/

  float_v mP[8];
  float_v mC[36];
  float_v dsdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  const float_v dS = GetDStoPoint(v, dsdr);
  float_v dsdp[6] = {-dsdr[0], -dsdr[1], -dsdr[2], 0.f, 0.f, 0.f};
  float_v F[36], F1[36];
  for(int i2=0; i2<36; i2++)
  {
    F[i2]  = 0.f;
    F1[i2] = 0.f;
  }
  Transport( dS, dsdr, mP, mC, dsdp, F, F1 );  

  if(Cv)
  {
    float_v VFT[3][6];
    for(int i=0; i<3; i++)
      for(int j=0; j<6; j++)
      {
        VFT[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          VFT[i][j] +=  Cv[IJ(i,k)] * F1[j*6+k];
        }
      }
  
    float_v FVFT[6][6];
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
  
  float_v d[3]={ v[0]-mP[0], v[1]-mP[1], v[2]-mP[2]};

  return ( ( mC[0]*d[0] + mC[1]*d[1] + mC[3]*d[2])*d[0]
           +(mC[1]*d[0] + mC[2]*d[1] + mC[4]*d[2])*d[1]
           +(mC[3]*d[0] + mC[4]*d[1] + mC[5]*d[2])*d[2] );
}

float_v KFParticleBaseSIMD::GetDeviationFromParticle( const KFParticleBaseSIMD &p ) const
{ 
  /** Returns Chi2 deviation of the current particle from another particle in 3D.
   ** \param[in] p - the second particle
   **/
  
  float_v ds[2] = {0.f,0.f};
  float_v dsdr[4][6];
  float_v F1[36], F2[36], F3[36], F4[36];
  for(int i1=0; i1<36; i1++)
  {
    F1[i1] = 0;
    F2[i1] = 0;
    F3[i1] = 0;
    F4[i1] = 0;
  }
  GetDStoParticle( p, ds, dsdr );
  
  float_v V0Tmp[36] ;
  float_v V1Tmp[36] ;

  
  float_v mP1[8], mC1[36];
  float_v mP2[8], mC2[36]; 
  
    Transport(ds[0], dsdr[0], mP1, mC1, dsdr[1], F1, F2);
  p.Transport(ds[1], dsdr[3], mP2, mC2, dsdr[2], F4, F3);
  
  MultQSQt(F2, p.fC, V0Tmp, 6);
  MultQSQt(F3,   fC, V1Tmp, 6);
      
  for(int iC=0; iC<6; iC++)
    mC1[iC] += V0Tmp[iC] + mC2[iC] + V1Tmp[iC];

  float_v d[3]={ mP2[0]-mP1[0], mP2[1]-mP1[1], mP2[2]-mP1[2]};
  
  return ( ( mC1[0]*d[0] + mC1[1]*d[1] + mC1[3]*d[2])*d[0]
           +(mC1[1]*d[0] + mC1[2]*d[1] + mC1[4]*d[2])*d[1]
           +(mC1[3]*d[0] + mC1[4]*d[1] + mC1[5]*d[2])*d[2] );
}

void KFParticleBaseSIMD::SubtractFromVertex(  KFParticleBaseSIMD &Vtx ) const
{
  /** Subtract the current particle from vertex Vtx using the Kalman filter mathematics.
   ** \param[in] Vtx - vertex from which particle should be subtracted
   **/
  
  float_v m[8];
  float_v mCm[36];
  float_v D[3][3];
  Vtx.GetMeasurement( *this, m, mCm, D );
  //* 
            
  float_v mS[6] = { mCm[0] - Vtx.fC[0] + (D[0][0] + D[0][0]), 
                  mCm[1] - Vtx.fC[1] + (D[1][0] + D[0][1]), mCm[2] - Vtx.fC[2] + (D[1][1] + D[1][1]), 
                  mCm[3] - Vtx.fC[3] + (D[2][0] + D[0][2]), mCm[4] - Vtx.fC[4] + (D[1][2] + D[2][1]), mCm[5] - Vtx.fC[5] + (D[2][2] + D[2][2]) };
  InvertCholetsky3(mS);   
    
  //* Residual (measured - estimated)
    
  float_v zeta[3] = { m[0]-Vtx.fP[0], m[1]-Vtx.fP[1], m[2]-Vtx.fP[2] };
        
  //* mCHt = mCH' - D'
    
  float_v mCHt0[3], mCHt1[3], mCHt2[3];
    
  mCHt0[0]=Vtx.fC[ 0] ;      mCHt1[0]=Vtx.fC[ 1] ;      mCHt2[0]=Vtx.fC[ 3] ;
  mCHt0[1]=Vtx.fC[ 1] ;      mCHt1[1]=Vtx.fC[ 2] ;      mCHt2[1]=Vtx.fC[ 4] ;
  mCHt0[2]=Vtx.fC[ 3] ;      mCHt1[2]=Vtx.fC[ 4] ;      mCHt2[2]=Vtx.fC[ 5] ;
  
  //* Kalman gain K = mCH'*S
    
  float_v k0[3], k1[3], k2[3];
    
  for(Int_t i=0;i<3;++i){
    k0[i] = mCHt0[i]*mS[0] + mCHt1[i]*mS[1] + mCHt2[i]*mS[3];
    k1[i] = mCHt0[i]*mS[1] + mCHt1[i]*mS[2] + mCHt2[i]*mS[4];
    k2[i] = mCHt0[i]*mS[3] + mCHt1[i]*mS[4] + mCHt2[i]*mS[5];
  }
    
  //* New estimation of the vertex position r += K*zeta
    
  float_v dChi2 = ((mS[0]*zeta[0] + mS[1]*zeta[1] + mS[3]*zeta[2])*zeta[0]
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

void KFParticleBaseSIMD::SubtractFromParticle(  KFParticleBaseSIMD &Vtx ) const
{
  /** Subtract the current particle from another particle Vtx using the Kalman filter mathematics. 
   ** The function is depricated and is kept for compatibility reasons. Should be replaced with SubtractDaughter().
   ** \param[in] Vtx - particle from which the current particle should be subtracted
   **/
  
  float_v m[8];
  float_v mV[36];

  float_v D[3][3];
  Vtx.GetMeasurement( *this, m, mV, D );

  float_v mS[6] = { mV[0] - Vtx.fC[0] + (D[0][0] + D[0][0]), 
                  mV[1] - Vtx.fC[1] + (D[1][0] + D[0][1]), mV[2] - Vtx.fC[2] + (D[1][1] + D[1][1]), 
                  mV[3] - Vtx.fC[3] + (D[2][0] + D[0][2]), mV[4] - Vtx.fC[4] + (D[1][2] + D[2][1]), mV[5] - Vtx.fC[5] + (D[2][2] + D[2][2]) };
  InvertCholetsky3(mS);

  //* Residual (measured - estimated)

  float_v zeta[3] = { m[0]-Vtx.fP[0], m[1]-Vtx.fP[1], m[2]-Vtx.fP[2] };    

  //* CHt = CH' - D'

  float_v mCHt0[7], mCHt1[7], mCHt2[7];

  mCHt0[0]=mV[ 0] ;           mCHt1[0]=mV[ 1] ;           mCHt2[0]=mV[ 3] ;
  mCHt0[1]=mV[ 1] ;           mCHt1[1]=mV[ 2] ;           mCHt2[1]=mV[ 4] ;
  mCHt0[2]=mV[ 3] ;           mCHt1[2]=mV[ 4] ;           mCHt2[2]=mV[ 5] ;
  mCHt0[3]=Vtx.fC[ 6]-mV[ 6]; mCHt1[3]=Vtx.fC[ 7]-mV[ 7]; mCHt2[3]=Vtx.fC[ 8]-mV[ 8];
  mCHt0[4]=Vtx.fC[10]-mV[10]; mCHt1[4]=Vtx.fC[11]-mV[11]; mCHt2[4]=Vtx.fC[12]-mV[12];
  mCHt0[5]=Vtx.fC[15]-mV[15]; mCHt1[5]=Vtx.fC[16]-mV[16]; mCHt2[5]=Vtx.fC[17]-mV[17];
  mCHt0[6]=Vtx.fC[21]-mV[21]; mCHt1[6]=Vtx.fC[22]-mV[22]; mCHt2[6]=Vtx.fC[23]-mV[23];

  //* Kalman gain K = mCH'*S
    
  float_v k0[7], k1[7], k2[7];
    
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

  float_v ffC[28] = {-mV[ 0],
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

void KFParticleBaseSIMD::TransportLine( float_v dS, const float_v* dsdr, float_v P[], float_v C[], float_v* dsdr1, float_v* F, float_v* F1 ) const 
{
  /** Transports the parameters and their covariance matrix of the current particle assuming the straight line trajectory
   ** on the length defined by the transport parameter dS = l/p, where l is the signed distance and p is 
   ** the momentum of the current particle. The obtained parameters and covariance matrix are stored to the arrays P and 
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
  
  float_v mJ[8][8];
  for( Int_t i=0; i<8; i++ ) for( Int_t j=0; j<8; j++) mJ[i][j]=0;

  mJ[0][0]=1; mJ[0][1]=0; mJ[0][2]=0; mJ[0][3]=dS;  mJ[0][4]=0;  mJ[0][5]=0;
  mJ[1][0]=0; mJ[1][1]=1; mJ[1][2]=0; mJ[1][3]=0;     mJ[1][4]=dS;  mJ[1][5]=0;
  mJ[2][0]=0; mJ[2][1]=0; mJ[2][2]=1; mJ[2][3]=0; mJ[2][4]=0; mJ[2][5]=dS;
  
  mJ[3][0]=0; mJ[3][1]=0; mJ[3][2]=0; mJ[3][3]=1;   mJ[3][4]=0;  mJ[3][5]=0;
  mJ[4][0]=0; mJ[4][1]=0; mJ[4][2]=0; mJ[4][3]=0;     mJ[4][4]=1;   mJ[4][5]=0;
  mJ[5][0]=0; mJ[5][1]=0; mJ[5][2]=0; mJ[5][3]=0; mJ[5][4]=0; mJ[5][5]=1;
  mJ[6][6] = mJ[7][7] = 1;
  
  float_v px = fP[3], py = fP[4], pz = fP[5];
  
  P[0] = fP[0] + dS*fP[3];
  P[1] = fP[1] + dS*fP[4];
  P[2] = fP[2] + dS*fP[5];
  P[3] = fP[3];
  P[4] = fP[4];
  P[5] = fP[5];
  P[6] = fP[6];
  P[7] = fP[7];
  
  float_v mJds[6][6];
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

void KFParticleBaseSIMD::TransportLine( float_v dS, float_v P[] ) const 
{
  /** Transports the parameters of the current particle assuming the straight line trajectory
   ** on the length defined by the transport parameter dS = l/p, where l is the signed distance and p is 
   ** the momentum of the current particle. The obtained parameters are stored to the array P 
   ** P can be set to the parameters fP of the current particle. In this
   ** case the particle parameters will be modified. 
   ** \param[in] dS - transport parameter which defines the distance to which particle should be transported
   ** \param[out] P[8] - array, where transported parameters should be stored
   **/
  
  P[0] = fP[0] + dS*fP[3];
  P[1] = fP[1] + dS*fP[4];
  P[2] = fP[2] + dS*fP[5];
  P[3] = fP[3];
  P[4] = fP[4];
  P[5] = fP[5];
  P[6] = fP[6];
  P[7] = fP[7];  
}

void KFParticleBaseSIMD::GetArmenterosPodolanski(KFParticleBaseSIMD& positive, KFParticleBaseSIMD& negative, float_v QtAlfa[2] )
{
  /** Calculates parameters for the Armenteros-Podolanski plot for two particles. 
   ** Example how to use:\n
   ** KFParticle PosParticle(...) \n
   ** KFParticle NegParticle(...) \n
   ** Gamma.ConstructGamma(PosParticle, NegParticle); \n
   ** float VertexGamma[3] = {Gamma.GetX(), Gamma.GetY(), Gamma.GetZ()}; \n
   ** PosParticle.TransportToPoint(VertexGamma); \n
   ** NegParticle.TransportToPoint(VertexGamma); \n
   ** float armenterosQtAlfa[2] = {0.}; \n
   ** KFParticle::GetArmenterosPodolanski(PosParticle, NegParticle, armenterosQtAlfa ); \n
   ** \param[in] positive - first particle, positive or neutral
   ** \param[in] negative - second particle, negative or neutral
   ** \param[out] QtAlfa[2] - parameters for the Armenteros-Podolanski plot: QtAlfa[0] = qt - projection of the 
   ** momenta of the particles on the transverse direction with respect to the total momentum, same for both particles;
   ** QtAlfa[1] = (Pl+ - Pl-)/(Pl+ + Pl-) - combination of the longitudinal components.
   **/

  float_v alpha = 0.f, qt = 0.f;
  float_v spx = positive.GetPx() + negative.GetPx();
  float_v spy = positive.GetPy() + negative.GetPy();
  float_v spz = positive.GetPz() + negative.GetPz();
  float_v sp  = sqrt(spx*spx + spy*spy + spz*spz);
  float_m mask = float_m(  abs(sp) < float_v(1.e-10f));
  float_v pn, pp, pln, plp;

  pn = sqrt(negative.GetPx()*negative.GetPx() + negative.GetPy()*negative.GetPy() + negative.GetPz()*negative.GetPz());
  pp = sqrt(positive.GetPx()*positive.GetPx() + positive.GetPy()*positive.GetPy() + positive.GetPz()*positive.GetPz());
  pln  = (negative.GetPx()*spx+negative.GetPy()*spy+negative.GetPz()*spz)/sp;
  plp  = (positive.GetPx()*spx+positive.GetPy()*spy+positive.GetPz()*spz)/sp;

  mask = float_m(mask & float_m(  abs(pn) < float_v(1.E-10f)));
  float_v ptm  = (1.f-((pln/pn)*(pln/pn)));
  qt(ptm >= 0.f) =  pn*sqrt(ptm);
  alpha = (plp-pln)/(plp+pln);

  QtAlfa[0](mask) = qt;
  QtAlfa[1](mask) = alpha;
}

void KFParticleBaseSIMD::RotateXY(float_v angle, float_v Vtx[3])
{
  /** Rotates the KFParticle object around OZ axis, OZ axis is set by the vertex position.
   ** \param[in] angle - angle of rotation in XY plane in [rad]
   ** \param[in] Vtx[3] - position of the vertex in [cm]
   **/

  // Before rotation the center of the coordinat system should be moved to the vertex position; move back after rotation
  X() = X() - Vtx[0];
  Y() = Y() - Vtx[1];
  Z() = Z() - Vtx[2];

  // Rotate the kf particle
  float_v s = sin(angle);
  float_v c = cos(angle);
  
  float_v mA[8][ 8];
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

  float_v mAC[8][8];
  float_v mAp[8];

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
      float_v xx = 0.f;
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

void KFParticleBaseSIMD::InvertCholetsky3(float_v a[6])
{
  /** Inverts symmetric 3x3 matrix a using modified Choletsky decomposition. The result is stored to the same matrix a.
   ** \param[in,out] a - 3x3 symmetric matrix
   **/

  float_v d[3], uud, u[3][3];
  for(int i=0; i<3; i++) 
  {
    d[i]=0.f;
    for(int j=0; j<3; j++) 
      u[i][j]=0.f;
  }

  for(int i=0; i<3 ; i++)
  {
    uud=0.f;
    for(int j=0; j<i; j++) 
      uud += u[j][i]*u[j][i]*d[j];
    uud = a[i*(i+3)/2] - uud;

    uud(abs(uud)<1.e-8f) = 1.e-8f;
    d[i] = uud/abs(uud);
    u[i][i] = sqrt(abs(uud));

    for(int j=i+1; j<3; j++) 
    {
      uud = 0.f;
      for(int k=0; k<i; k++)
        uud += u[k][i]*u[k][j]*d[k];
      uud = a[j*(j+1)/2+i] - uud;
      u[i][j] = d[i]/u[i][i]*uud;
    }
  }

  float_v u1[3];

  for(int i=0; i<3; i++)
  {
    u1[i] = u[i][i];
    u[i][i] = 1.f/u[i][i];
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

void KFParticleBaseSIMD::MultQSQt( const float_v Q[], const float_v S[], float_v SOut[], const int kN )
{
  /** Matrix multiplication SOut = Q*S*Q^T, where Q - square matrix, S - symmetric matrix.
   ** \param[in] Q - square matrix
   ** \param[in] S - input symmetric matrix
   ** \param[out] SOut - output symmetric matrix
   ** \param[in] kN - dimensionality of the matrices
   **/
  
  float_v* mA = new float_v[kN*kN];
  
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
  
  if(mA) delete[] mA;
}


// 72-charachters line to define the printer border
//3456789012345678901234567890123456789012345678901234567890123456789012

