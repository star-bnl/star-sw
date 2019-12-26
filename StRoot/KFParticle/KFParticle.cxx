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


#include "KFParticle.h"
#include "KFParticleDatabase.h"

#include "KFPTrack.h"
#include "KFPVertex.h"

#ifndef KFParticleStandalone
ClassImp(KFParticle);
#endif


#ifdef HomogeneousField
float KFParticle::fgBz = -5.;  //* Bz compoment of the magnetic field
#endif

KFParticle::KFParticle( const KFParticle &d1, const KFParticle &d2 ): KFParticleBase()
{
  /** Constructs a particle from two input daughter particles
   ** \param[in] d1 - the first daughter particle
   ** \param[in] d2 - the second daughter particle
   **/
  KFParticle mother;
  mother+= d1;
  mother+= d2;
  *this = mother;
}

void KFParticle::Create( const float Param[], const float Cov[], Int_t Charge, float mass )
{
  /** Constructor from a "cartesian" track, mass hypothesis should be provided
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
  float C[21];
  for( int i=0; i<21; i++ ) C[i] = Cov[i];
  
  KFParticleBase::Initialize( Param, C, Charge, mass );
}

void KFParticle::Create( const Double_t Param[], const Double_t Cov[], Int_t Charge, float mass )
{
  /** Constructor from a "cartesian" track, mass hypothesis should be provided
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
  float P[6];
  for(int i=0; i<6; i++ ) P[i] = Param[i];
  float C[21];
  for( int i=0; i<21; i++ ) C[i] = Cov[i];
    
  KFParticleBase::Initialize( P, C, Charge, mass );
}

KFParticle::KFParticle( const KFPTrack &track, const int PID ): KFParticleBase()
{
  /** Constructor from a track in the KF Particle format, PID hypothesis should be provided
   ** \param[in] track - KFPTrack containing 6 parameters: { X, Y, Z, Px, Py, Pz } and their errors
   ** \param[in] PID - PID hypothesis, needed to assign mass to a particle and calculate the energy
   **/

  track.XvYvZv(fP);
  track.PxPyPz(fP+3);
  fQ = track.Charge();
  track.GetCovarianceXYZPxPyPz( fC );

  float mass = KFParticleDatabase::Instance()->GetMass(PID);

  Create(fP,fC,fQ,mass);
  fChi2 = track.GetChi2();
  fNDF = track.GetNDF();
  SetPDG(PID);
#ifdef NonhomogeneousField
  for(int iF=0; iF<10; iF++)
    SetFieldCoeff( track.GetFieldCoeff()[iF], iF);
#endif
}

KFParticle::KFParticle( const KFPVertex &vertex ): KFParticleBase()
{
  /** Constructor from a vertex in the KF Particle format
   ** \param[in] vertex - KFPVertex containing 3 parameters: { X, Y, Z } and their errors
   **/

  vertex.GetXYZ( fP );
  vertex.GetCovarianceMatrix( fC );  
  fChi2 = vertex.GetChi2();
  fNDF = 2*vertex.GetNContributors() - 3;
  fQ = 0;
  fAtProductionVertex = 0;
  fSFromDecay = 0;
}

Bool_t KFParticle::GetDistanceFromVertexXY( const float vtx[], const float Cv[], float &val, float &err ) const
{
  /** Calculates the DCA distance from a vertex together with the error in the XY plane.
   ** Returns "true" if calculation is failed, "false" if both value and the error are well defined.
   ** \param[in] vtx[2] - { X, Y } coordinates of the vertex
   ** \param[in] Cv[3] - lower-triangular part of the covariance matrix of the vertex
   ** \param[out] val - the distance in the XY plane to the vertex
   ** \param[out] err - the error of the calculated distance, takes into account errors of the particle and vertex
   **/

  Bool_t ret = 0;
  
  float mP[8];
  float mC[36];
  float dsdr[6] = {0.f};
  const float dS = GetDStoPoint(vtx, dsdr);
  Transport( dS, dsdr, mP, mC );  

  float dx = mP[0] - vtx[0];
  float dy = mP[1] - vtx[1];
  float px = mP[3];
  float py = mP[4];
  float pt = sqrt(px*px + py*py);
  float ex=0, ey=0;
  if( pt<1.e-4 ){
    ret = 1;
    pt = 1.;
    val = 1.e4;
  } else{
    ex = px/pt;
    ey = py/pt;
    val = dy*ex - dx*ey;
  }

  float h0 = -ey;
  float h1 = ex;
  float h3 = (dy*ey + dx*ex)*ey/pt;
  float h4 = -(dy*ey + dx*ex)*ex/pt;
  
  err = 
    h0*(h0*GetCovariance(0,0) + h1*GetCovariance(0,1) + h3*GetCovariance(0,3) + h4*GetCovariance(0,4) ) +
    h1*(h0*GetCovariance(1,0) + h1*GetCovariance(1,1) + h3*GetCovariance(1,3) + h4*GetCovariance(1,4) ) +
    h3*(h0*GetCovariance(3,0) + h1*GetCovariance(3,1) + h3*GetCovariance(3,3) + h4*GetCovariance(3,4) ) +
    h4*(h0*GetCovariance(4,0) + h1*GetCovariance(4,1) + h3*GetCovariance(4,3) + h4*GetCovariance(4,4) );

  if( Cv ){
    err+= h0*(h0*Cv[0] + h1*Cv[1] ) + h1*(h0*Cv[1] + h1*Cv[2] ); 
  }

  err = sqrt(fabs(err));

  return ret;
}

Bool_t KFParticle::GetDistanceFromVertexXY( const float vtx[], float &val, float &err ) const
{
  /** Calculates the DCA distance from a vertex together with the error in the XY plane.
   ** Returns "true" if calculation is failed, "false" if both value and the error are well defined.
   ** \param[in] vtx[2] - { X, Y } coordinates of the vertex
   ** \param[out] val - the distance in the XY plane to the vertex
   ** \param[out] err - the error of the calculated distance, takes into account errors of the particle only
   **/
  return GetDistanceFromVertexXY( vtx, 0, val, err );
}


Bool_t KFParticle::GetDistanceFromVertexXY( const KFParticle &Vtx, float &val, float &err ) const 
{
  /** Calculates the DCA distance from a vertex in the KFParticle format together with the error in the XY plane.
   ** Returns "true" if calculation is failed, "false" if both value and the error are well defined.
   ** \param[in] Vtx - the vertex in the KFParticle format
   ** \param[out] val - the distance in the XY plane to the vertex
   ** \param[out] err - the error of the calculated distance, takes into account errors of the particle and vertex
   **/

  return GetDistanceFromVertexXY( Vtx.fP, Vtx.fC, val, err );
}

#ifdef HomogeneousField
Bool_t KFParticle::GetDistanceFromVertexXY( const KFPVertex &Vtx, float &val, float &err ) const 
{
  /** Calculates the DCA distance from a vertex in the KFPVertex format together with the error in the XY plane.
   ** Returns "true" if calculation is failed, "false" if both value and the error are well defined.
   ** \param[in] Vtx - the vertex in the KFPVertex format
   ** \param[out] val - the distance in the XY plane to the vertex
   ** \param[out] err - the error of the calculated distance, takes into account errors of the particle and vertex
   **/

  return GetDistanceFromVertexXY( KFParticle(Vtx), val, err );
}
#endif

float KFParticle::GetDistanceFromVertexXY( const float vtx[] ) const
{
  /** Returns the DCA distance from a vertex in the XY plane.
   ** \param[in] vtx[2] - { X, Y } coordinates of the vertex
   **/

  float val, err;
  GetDistanceFromVertexXY( vtx, 0, val, err );
  return val;
}

float KFParticle::GetDistanceFromVertexXY( const KFParticle &Vtx ) const 
{
  /** Returns the DCA distance from a vertex in the KFParticle format in the XY plane.
   ** \param[in] Vtx - the vertex in the KFParticle format
   **/

  return GetDistanceFromVertexXY( Vtx.fP );
}

#ifdef HomogeneousField
float KFParticle::GetDistanceFromVertexXY( const KFPVertex &Vtx ) const 
{
  /** Returns the DCA distance from a vertex in the KFParticle format in the XY plane.
   ** \param[in] Vtx - the vertex in the KFPVertex format
   **/

  return GetDistanceFromVertexXY( KFParticle(Vtx).fP );
}
#endif

float KFParticle::GetDistanceFromParticleXY( const KFParticle &p ) const 
{
  /** Returns the DCA distance between the current and the second particles in the XY plane.
   ** \param[in] p - the second particle
   **/
  
  float dsdr[4][6];
  float dS[2];
  GetDStoParticle( p, dS, dsdr );
  float mP[8], mC[36], mP1[8], mC1[36];
  Transport( dS[0], dsdr[0], mP, mC ); 
  p.Transport( dS[1], dsdr[3], mP1, mC1 ); 
  float dx = mP[0]-mP1[0]; 
  float dy = mP[1]-mP1[1]; 
  return sqrt(dx*dx+dy*dy);
}

float KFParticle::GetDeviationFromParticleXY( const KFParticle &p ) const 
{
  /** Returns sqrt(Chi2/ndf) deviation from other particle in the XY plane.
   ** \param[in] p - the second particle
   **/
  
  float dsdr[4][6];
  float dS[2];
  GetDStoParticle( p, dS, dsdr );
  float mP[8], mC[36], mP1[8], mC1[36];
  Transport( dS[0], dsdr[0], mP, mC ); 
  p.Transport( dS[1], dsdr[3], mP1, mC1 ); 

  float d[2]={ mP[0]-mP1[0], mP[1]-mP1[1] };

  float sigmaS = .1+10.*sqrt( (d[0]*d[0]+d[1]*d[1] )/
					(mP1[3]*mP1[3]+mP1[4]*mP1[4] )  );

  float h[2] = { mP1[3]*sigmaS, mP1[4]*sigmaS };       
  
  mC1[0] +=h[0]*h[0];
  mC1[1] +=h[1]*h[0]; 
  mC1[2] +=h[1]*h[1]; 

  return GetDeviationFromVertexXY( mP1, mC1 )*sqrt(2./1.);
}


float KFParticle::GetDeviationFromVertexXY( const float vtx[], const float Cv[] ) const 
{
  /** Returns sqrt(Chi2/ndf) deviation from the vertex in the XY plane.
   ** \param[in] vtx[2] - { X, Y } coordinates of the vertex
   ** \param[in] Cv[3] - lower-triangular part of the covariance matrix of the vertex
   **/

  float val, err;
  Bool_t problem = GetDistanceFromVertexXY( vtx, Cv, val, err );
  if( problem || err<1.e-20 ) return 1.e4;
  else return val/err;
}


float KFParticle::GetDeviationFromVertexXY( const KFParticle &Vtx ) const  
{
  /** Returns sqrt(Chi2/ndf) deviation from the vertex in the KFParticle format in the XY plane.
   ** \param[in] Vtx - the vertex in the KFParticle format
   **/

  return GetDeviationFromVertexXY( Vtx.fP, Vtx.fC );
}

#ifdef HomogeneousField
float KFParticle::GetDeviationFromVertexXY( const KFPVertex &Vtx ) const 
{
  /** Returns sqrt(Chi2/ndf) deviation from the vertex in the KFPVertex format in the XY plane.
   ** \param[in] Vtx - the vertex in the KFPVertex format
   **/

  KFParticle v(Vtx);
  return GetDeviationFromVertexXY( v.fP, v.fC );
}
#endif

void KFParticle::GetParametersAtPoint(const float* point, const float* pointCov, float* m, float* mV)
{
  /** Transports particle to the DCA of the given point and stores obtained parameters and covariance matrix
   ** \param[in] point[3] - the point to which particle is transported
   ** \param[in] pointCov[6] - the covariance matrix of the point
   ** \param[out] m[8] - the parameters of the particle at the DCA point
   ** \param[out] mV[36] - the covariance matrix of the particle at the DCA point, accounts the covariance matrix of the point as well
   **/
    
  float dsdr[6] = {0.f, 0.f, 0.f, 0.f, 0.f, 0.f};
  float dS = GetDStoPoint(point, dsdr);
  float dsdp[6] = {-dsdr[0], -dsdr[1], -dsdr[2], 0, 0, 0};
    
  float F[36], F1[36];
  for(int i2=0; i2<36; i2++){
    mV[i2] = 0.f;
    F[i2]  = 0.f;
    F1[i2] = 0.f;
  }
  Transport(dS, dsdr, m, mV, dsdp, F, F1);
    
  float V1Tmp[36];
  for(int i=0; i<36; i++)
    V1Tmp[i] = 0.f;
  KFParticle::MultQSQt(F1, pointCov, V1Tmp, 6);
    
  for(int iC=0; iC<21; iC++)
    mV[iC] += V1Tmp[iC];
}

float KFParticle::GetAngle  ( const KFParticle &p ) const 
{
  /** Returns the opening angle between the current and the second particle in 3D.
   ** \param[in] p - the second particle
   **/
  
  float dsdr[4][6];
  float dS[2];
  GetDStoParticle( p, dS, dsdr );   
  float mP[8], mC[36], mP1[8], mC1[36];
  Transport( dS[0], dsdr[0], mP, mC ); 
  p.Transport( dS[1], dsdr[3], mP1, mC1 ); 
  float n = sqrt( mP[3]*mP[3] + mP[4]*mP[4] + mP[5]*mP[5] );
  float n1= sqrt( mP1[3]*mP1[3] + mP1[4]*mP1[4] + mP1[5]*mP1[5] );
  n*=n1;
  float a = 0;
  if( n>1.e-8 ) a = ( mP[3]*mP1[3] + mP[4]*mP1[4] + mP[5]*mP1[5] )/n;
  if (fabs(a)<1.) a = acos(a);
  else a = (a>=0) ?0 :3.14;
  return a;
}

float KFParticle::GetAngleXY( const KFParticle &p ) const 
{
  /** Returns the opening angle between the current and the second particle in the XY plane.
   ** \param[in] p - the second particle
   **/
  
  float dsdr[4][6];
  float dS[2];
  GetDStoParticle( p, dS, dsdr );   
  float mP[8], mC[36], mP1[8], mC1[36];
  Transport( dS[0], dsdr[0], mP, mC ); 
  p.Transport( dS[1], dsdr[3], mP1, mC1 ); 
  float n = sqrt( mP[3]*mP[3] + mP[4]*mP[4] );
  float n1= sqrt( mP1[3]*mP1[3] + mP1[4]*mP1[4] );
  n*=n1;
  float a = 0;
  if( n>1.e-8 ) a = ( mP[3]*mP1[3] + mP[4]*mP1[4] )/n;
  if (fabs(a)<1.) a = acos(a);
  else a = (a>=0) ?0 :3.14;
  return a;
}

float KFParticle::GetAngleRZ( const KFParticle &p ) const 
{
  /** Returns the opening angle between the current and the second particle in the RZ plane, R = sqrt(X*X+Y*Y).
   ** \param[in] p - the second particle
   **/

  float dsdr[4][6];
  float dS[2];
  GetDStoParticle( p, dS, dsdr );   
  float mP[8], mC[36], mP1[8], mC1[36];
  Transport( dS[0], dsdr[0], mP, mC ); 
  p.Transport( dS[1], dsdr[3], mP1, mC1 );  
  float nr = sqrt( mP[3]*mP[3] + mP[4]*mP[4] );
  float n1r= sqrt( mP1[3]*mP1[3] + mP1[4]*mP1[4]  );
  float n = sqrt( nr*nr + mP[5]*mP[5] );
  float n1= sqrt( n1r*n1r + mP1[5]*mP1[5] );
  n*=n1;
  float a = 0;
  if( n>1.e-8 ) a = ( nr*n1r +mP[5]*mP1[5])/n; 
  if (fabs(a)<1.) a = acos(a);
  else a = (a>=0) ?0 :3.14;
  return a;
}

float KFParticle::GetPseudoProperDecayTime( const KFParticle &pV, const float& mass, float* timeErr2 ) const
{ 
  /** Returns the Pseudo Proper Time of the decay = (r*pt) / |pt| * M/|pt|
   ** \param[in] pV - the creation point of the particle
   ** \param[in] mass - the mass of the particle
   ** \param[out] timeErr2 - error of the returned value, if null pointer is provided - is not calculated
   **/
  
  const float ipt2 = 1/( Px()*Px() + Py()*Py() );
  const float mipt2 = mass*ipt2;
  const float dx = X() - pV.X();
  const float dy = Y() - pV.Y();

  if ( timeErr2 ) {
      // -- calculate error = sigma(f(r)) = f'Cf'
      // r = {x,y,px,py,x_pV,y_pV}
      // df/dr = { px*m/pt^2,
      //     py*m/pt^2,
      //    ( x - x_pV )*m*(1/pt^2 - 2(px/pt^2)^2),
      //    ( y - y_pV )*m*(1/pt^2 - 2(py/pt^2)^2),
      //     -px*m/pt^2,
      //     -py*m/pt^2 }
    const float f0 = Px()*mipt2;
    const float f1 = Py()*mipt2;
    const float mipt2derivative = mipt2*(1-2*Px()*Px()*ipt2);
    const float f2 = dx*mipt2derivative;
    const float f3 = -dy*mipt2derivative;
    const float f4 = -f0;
    const float f5 = -f1;

    const float& mC00 =    GetCovariance(0,0);
    const float& mC10 =    GetCovariance(0,1);
    const float& mC11 =    GetCovariance(1,1);
    const float& mC20 =    GetCovariance(3,0);
    const float& mC21 =    GetCovariance(3,1);
    const float& mC22 =    GetCovariance(3,3);
    const float& mC30 =    GetCovariance(4,0);
    const float& mC31 =    GetCovariance(4,1);
    const float& mC32 =    GetCovariance(4,3);
    const float& mC33 =    GetCovariance(4,4);
    const float& mC44 = pV.GetCovariance(0,0);
    const float& mC54 = pV.GetCovariance(1,0);
    const float& mC55 = pV.GetCovariance(1,1);

    *timeErr2 =
      f5*mC55*f5 +
      f5*mC54*f4 +
      f4*mC44*f4 +
      f3*mC33*f3 +
      f3*mC32*f2 +
      f3*mC31*f1 +
      f3*mC30*f0 +
      f2*mC22*f2 +
      f2*mC21*f1 +
      f2*mC20*f0 +
      f1*mC11*f1 +
      f1*mC10*f0 +
      f0*mC00*f0;
  }
  return ( dx*Px() + dy*Py() )*mipt2;
}
