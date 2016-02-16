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



#include "KFParticleSIMD.h"
#include "KFParticle.h"
#include "KFParticleDatabase.h"

#ifdef HomogeneousField
float_v KFParticleSIMD::fgBz = -5.f;  //* Bz compoment of the magnetic field
#endif

KFParticleSIMD::KFParticleSIMD( const KFParticleSIMD &d1, const KFParticleSIMD &d2 ): KFParticleBaseSIMD()
#ifdef NonhomogeneousField
, fField()
#endif
{
  KFParticleSIMD mother;
  mother+= d1;
  mother+= d2;
  *this = mother;
}

void KFParticleSIMD::Create( const float_v Param[], const float_v Cov[], int_v Charge, float_v mass /*Int_t PID*/ )
{
  // Constructor from "cartesian" track, PID hypothesis should be provided
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
  float_v C[21];
  for( int i=0; i<21; i++ ) C[i] = Cov[i];
  
//  TParticlePDG* particlePDG = TDatabasePDG::Instance()->GetParticle(PID);
//  float_v mass = (particlePDG) ? particlePDG->Mass() :0.13957;
  
  KFParticleBaseSIMD::Initialize( Param, C, Charge, mass );
}

KFParticleSIMD::KFParticleSIMD( const KFPTrack *track, Int_t PID ): KFParticleBaseSIMD()
#ifdef NonhomogeneousField
, fField()
#endif
{
  Double_t r[3];
  Double_t C[21];

  for(Int_t iPart = 0; iPart<float_vLen; iPart++)
  {
    track[iPart].XvYvZv(r);
    for(Int_t i=0; i<3; i++)
      fP[i][iPart] = r[i];
    track[iPart].PxPyPz(r);
    for(Int_t i=0; i<3; i++)
      fP[i+3][iPart] = r[i];
    fQ[iPart] = track[iPart].Charge();
    track[iPart].GetCovarianceXYZPxPyPz( C );
    for(Int_t i=0; i<21; i++)
      fC[i][iPart] = C[i];
  }

  float_v mass = KFParticleDatabase::Instance()->GetMass(PID);
  Create(fP,fC,fQ,mass);

  for(Int_t iPart = 0; iPart<float_vLen; iPart++)
  {
    fChi2[iPart] = track[iPart].GetChi2();
    fNDF[iPart] = track[iPart].GetNDF();
  }
}

KFParticleSIMD::KFParticleSIMD(KFPTrack &Track, const Int_t *pdg): KFParticleBaseSIMD()
#ifdef NonhomogeneousField
, fField()
#endif
{
  Double_t r[3];
  Double_t C[21];

  Track.XvYvZv(r);
  for(Int_t i=0; i<3; i++)
    fP[i] = r[i];
  Track.PxPyPz(r);
  for(Int_t i=0; i<3; i++)
    fP[i+3] = r[i];
  fQ = Track.Charge();
  Track.GetCovarianceXYZPxPyPz( C );
  for(Int_t i=0; i<21; i++)
    fC[i] = C[i];

  float_v mass = KFParticleDatabase::Instance()->GetMass(*pdg);
  Create(fP,fC,fQ,mass);

  fChi2 = Track.GetChi2();
  fNDF = Track.GetNDF();
}

KFParticleSIMD::KFParticleSIMD(KFPTrackVector &track, int n, const Int_t *pdg): KFParticleBaseSIMD()
#ifdef NonhomogeneousField
, fField()
#endif
{
  for(int i=0; i<6; i++)
    fP[i] = track.Parameter(i)[n];
  for(int i=0; i<21; i++)
    fC[i] = track.Covariance(i)[n];
#ifdef NonhomogeneousField
  for(int i=0; i<10; i++)
    fField.fField[i] = track.FieldCoefficient(i)[n];
#endif
  fQ = track.Q()[n];

  float_v mass = KFParticleDatabase::Instance()->GetMass(*pdg);
  Create(fP,fC,fQ,mass);
}

  
KFParticleSIMD::KFParticleSIMD(KFPTrack* Track[], int NTracks, const Int_t *pdg): KFParticleBaseSIMD()
#ifdef NonhomogeneousField
, fField()
#endif
{
  Create(Track, NTracks, pdg);
}

void KFParticleSIMD::Create(KFPTrack* Track[], int NTracks, const Int_t *pdg)
{
  Double_t r[3];
  Double_t C[21];

  for(Int_t iPart = 0; iPart<float_vLen; iPart++)
  {
    Int_t iEntry = (iPart < NTracks) ? iPart : 0; 
    Track[iEntry]->XvYvZv(r);
    for(Int_t i=0; i<3; i++)
      fP[i][iEntry] = r[i];
    Track[iEntry]->PxPyPz(r);
    for(Int_t i=0; i<3; i++)
      fP[i+3][iEntry] = r[i];
    fQ[iEntry] = Track[iEntry]->Charge();
    Track[iEntry]->GetCovarianceXYZPxPyPz( C );
    for(Int_t i=0; i<21; i++)
      fC[i][iEntry] = C[i];
  }

  float_v mass = KFParticleDatabase::Instance()->GetMass(*pdg);
  Create(fP,fC,fQ,mass);

  for(Int_t iPart = 0; iPart<float_vLen; iPart++)
  {
    Int_t iEntry = (iPart < NTracks) ? iPart : 0; 
    fChi2[iEntry] = Track[iEntry]->GetChi2();
    fNDF[iEntry] = Track[iEntry]->GetNDF();
  }
}

KFParticleSIMD::KFParticleSIMD(KFPTrackVector &track, uint_v& index, const int_v& pdg): KFParticleBaseSIMD()
#ifdef NonhomogeneousField
, fField()
#endif
{
  Create(track, index, pdg);
}

void KFParticleSIMD::Create(KFPTrackVector &track, uint_v& index, const int_v& pdg)
{
  for(int i=0; i<6; i++)
    fP[i].gather(&(track.Parameter(i)[0]), index);
  for(int i=0; i<21; i++)
    fC[i].gather(&(track.Covariance(i)[0]), index);
#ifdef NonhomogeneousField
  for(int i=0; i<10; i++)
    fField.fField[i].gather(&(track.FieldCoefficient(i)[0]), index);
#endif
  
  //   fPDG.gather(&(track.PDG()[0]), index);
  fQ.gather(&(track.Q()[0]), index);

  float_v mass = KFParticleDatabase::Instance()->GetMass(pdg);
  Create(fP,fC,fQ,mass);
}

void KFParticleSIMD::Load(KFPTrackVector &track, int index, const int_v& pdg)
{
  for(int i=0; i<6; i++)
    fP[i] = reinterpret_cast<const float_v&>(track.Parameter(i)[index]);
  for(int i=0; i<21; i++)
    fC[i] = reinterpret_cast<const float_v&>(track.Covariance(i)[index]);
#ifdef NonhomogeneousField
  for(int i=0; i<10; i++)
    fField.fField[i] = reinterpret_cast<const float_v&>(track.FieldCoefficient(i)[index]);
#endif
  
  //   fPDG.gather(&(track.PDG()[0]), index);
  fQ = reinterpret_cast<const int_v&>(track.Q()[index]);

  float_v mass = KFParticleDatabase::Instance()->GetMass(pdg);
  Create(fP,fC,fQ,mass);
}

void KFParticleSIMD::Rotate()
{
  for(int i=0; i<7; i++)
    fP[i] = fP[i].rotated(1);
  for(int i=0; i<27; i++)
    fC[i] = fC[i].rotated(1);
#ifdef NonhomogeneousField
  for(int i=0; i<10; i++)
    fField.fField[i] = fField.fField[i].rotated(1);
#endif
  fQ = fQ.rotated(1);
  fId = fId.rotated(1);
}

KFParticleSIMD::KFParticleSIMD(KFPEmcCluster &track, uint_v& index, const KFParticleSIMD& vertexGuess): KFParticleBaseSIMD()
#ifdef NonhomogeneousField
, fField()
#endif
{
  Create(track, index, vertexGuess);
}

void KFParticleSIMD::Create(KFPEmcCluster &track, uint_v& index, const KFParticleSIMD& vertexGuess)
{
  for(int i=0; i<3; i++)
    fP[i].gather(&(track.Parameter(i)[0]), index);
  fP[6].gather(&(track.Parameter(3)[0]), index);
  
  const float_v& dx = fP[0] - vertexGuess.fP[0];
  const float_v& dy = fP[1] - vertexGuess.fP[1];
  const float_v& dz = fP[2] - vertexGuess.fP[2];
  const float_v& dl2 = dx*dx + dy*dy + dz*dz;
  const float_v& dl = sqrt(dl2);

  fP[0] = vertexGuess.fP[0];
  fP[1] = vertexGuess.fP[1];
  fP[2] = vertexGuess.fP[2];
  
  fP[3] = dx/dl * fP[6];
  fP[4] = dy/dl * fP[6];
  fP[5] = dz/dl * fP[6];
  
  float_v V[10];
  for(int i=0; i<10; i++)
    V[i].gather(&(track.Covariance(i)[0]), index);
  
  float_v J[7][4];
  for(int i=0; i<7; i++)
    for(int j=0; j<4; j++)
      J[i][j] = 0.f;
  J[0][0] = 1.f; J[1][1] = 1.f; J[2][2] = 1.f; J[6][3] = 1.f;
  J[3][0] = fP[6]/dl * (1.f - dx*dx/dl2); J[3][1] = -dx*dy*fP[6]/(dl*dl2); J[3][2] = -dx*dz*fP[6]/(dl*dl2); J[3][3] = dx/dl;
  J[4][0] = -dx*dy*fP[6]/(dl*dl2); J[4][1] = fP[6]/dl * (1.f - dy*dy/dl2); J[4][2] = -dy*dz*fP[6]/(dl*dl2); J[4][3] = dy/dl;
  J[5][0] = -dx*dz*fP[6]/(dl*dl2); J[5][1] = -dy*dz*fP[6]/(dl*dl2); J[5][2] = fP[6]/dl * (1.f - dz*dz/dl2); J[5][3] = dz/dl;
  
  float_v VJT[4][7]; // V*J^T
  for(Int_t i=0; i<4; i++)
  {
    for(Int_t j=0; j<7; j++)
    {
      VJT[i][j] = 0.f;
      for(Int_t k=0; k<4; k++)
        VJT[i][j] += V[IJ(i,k)]*J[j][k];
    }
  }
  //Calculate the covariance matrix of the particle fC
  for( Int_t i=0; i<36; i++ ) fC[i] = 0.f;
  
  for(Int_t i=0; i<7; ++i)
    for(Int_t j=0; j<=i; ++j)
      for(Int_t l=0; l<4; l++)
        fC[IJ(i,j)]+= J[i][l]*VJT[l][j];
  fC[35] = 1.f;
  
  fQ = int_v(Vc::Zero);
  fNDF = 0;
  fChi2 = 0;
  
  SumDaughterMass = float_v(Vc::Zero);
  fMassHypo = float_v(Vc::Zero);
}

KFParticleSIMD::KFParticleSIMD(KFPEmcCluster &track, int index, const KFParticleSIMD& vertexGuess): KFParticleBaseSIMD()
#ifdef NonhomogeneousField
, fField()
#endif
{
  Load(track, index, vertexGuess);
}

void KFParticleSIMD::Load(KFPEmcCluster &track, int index, const KFParticleSIMD& vertexGuess)
{
  for(int i=0; i<3; i++)
    fP[i] = reinterpret_cast<const float_v&>(track.Parameter(i)[index]);
  fP[6] = reinterpret_cast<const float_v&>(track.Parameter(3)[index]);
  const float_v& dx = fP[0] - vertexGuess.fP[0];
  const float_v& dy = fP[1] - vertexGuess.fP[1];
  const float_v& dz = fP[2] - vertexGuess.fP[2];
  const float_v& dl2 = dx*dx + dy*dy + dz*dz;
  const float_v& dl = sqrt(dl2);

  fP[0] = vertexGuess.fP[0];
  fP[1] = vertexGuess.fP[1];
  fP[2] = vertexGuess.fP[2];
  
  fP[3] = dx/dl * fP[6];
  fP[4] = dy/dl * fP[6];
  fP[5] = dz/dl * fP[6];
  
  float_v V[10];
  for(int i=0; i<10; i++)
    V[i] = reinterpret_cast<const float_v&>(track.Covariance(i)[index]);
  
  float_v J[7][4];
  for(int i=0; i<7; i++)
    for(int j=0; j<4; j++)
      J[i][j] = 0.f;
  J[0][0] = 1.f; J[1][1] = 1.f; J[2][2] = 1.f; J[6][3] = 1.f;
  J[3][0] = fP[6]/dl * (1.f - dx*dx/dl2); J[3][1] = -dx*dy*fP[6]/(dl*dl2); J[3][2] = -dx*dz*fP[6]/(dl*dl2); J[3][3] = dx/dl;
  J[4][0] = -dx*dy*fP[6]/(dl*dl2); J[4][1] = fP[6]/dl * (1.f - dy*dy/dl2); J[4][2] = -dy*dz*fP[6]/(dl*dl2); J[4][3] = dy/dl;
  J[5][0] = -dx*dz*fP[6]/(dl*dl2); J[5][1] = -dy*dz*fP[6]/(dl*dl2); J[5][2] = fP[6]/dl * (1.f - dz*dz/dl2); J[5][3] = dz/dl;
  
  float_v VJT[4][7]; // V*J^T
  for(Int_t i=0; i<4; i++)
  {
    for(Int_t j=0; j<7; j++)
    {
      VJT[i][j] = 0.f;
      for(Int_t k=0; k<4; k++)
        VJT[i][j] += V[IJ(i,k)]*J[j][k];
    }
  }
  //Calculate the covariance matrix of the particle fC
  for( Int_t i=0; i<36; i++ ) fC[i] = 0.f;
  
  for(Int_t i=0; i<7; ++i)
    for(Int_t j=0; j<=i; ++j)
      for(Int_t l=0; l<4; l++)
        fC[IJ(i,j)]+= J[i][l]*VJT[l][j];
  fC[35] = 1.f;
  
  fQ = int_v(Vc::Zero);
  fNDF = 0;
  fChi2 = 0;
  
  SumDaughterMass = float_v(Vc::Zero);
  fMassHypo = float_v(Vc::Zero);
}

KFParticleSIMD::KFParticleSIMD( const KFPVertex &vertex ): KFParticleBaseSIMD()
#ifdef NonhomogeneousField
, fField()
#endif
{
  // Constructor from ALICE vertex

  Double_t r[3];
  Double_t C[21];

  vertex.GetXYZ( r );
  for(Int_t i=0; i<3; i++)
    fP[i] = r[i];
  vertex.GetCovarianceMatrix( C );
  for(Int_t i=0; i<21; i++)
    fC[i] = C[i];
  fChi2 = vertex.GetChi2();
  fNDF = 2*vertex.GetNContributors() - 3;
  fQ = int_v(Vc::Zero);
  fAtProductionVertex = 0;
  fSFromDecay = 0;
}

void KFParticleSIMD::SetOneEntry(int iEntry, KFParticleSIMD& part, int iEntryPart)
{
  for( int i = 0; i < 7; ++i )
    fP[i][iEntry] = part.Parameters()[i][iEntryPart];
  for( int i = 0; i < 36; ++i )
    fC[i][iEntry] = part.CovarianceMatrix()[i][iEntryPart];
  
  fQ[iEntry] = part.Q()[iEntryPart];
  fNDF[iEntry] = part.NDF()[iEntryPart];
  fChi2[iEntry] = part.Chi2()[iEntryPart];
  
//   fSFromDecay[iEntry] = part.fSFromDecay[iEntryPart];
//   SumDaughterMass[iEntry] = part.SumDaughterMass[iEntryPart];
//   fMassHypo[iEntry] = part.fMassHypo[iEntryPart];

  fId[iEntry] = part.Id()[iEntryPart];

  fPDG[iEntry] = part.GetPDG()[iEntryPart];
  
  if(iEntry==0)
    fDaughterIds.resize( part.NDaughters(), int_v(-1) );
  
  for(int iD=0; iD<part.NDaughters(); iD++)
    fDaughterIds[iD][iEntry] = part.fDaughterIds[iD][iEntryPart];

#ifdef NonhomogeneousField
  fField.SetOneEntry( iEntry, part.fField, iEntryPart ); //CHECKME
#endif
}

KFParticleSIMD::KFParticleSIMD(KFParticle* parts[], const int nPart): KFParticleBaseSIMD()
#ifdef NonhomogeneousField
, fField()
#endif
{
  { // check
    bool ok = 1;

    const int nD = (parts[0])->NDaughters();
    for ( int ie = 1; ie < nPart; ie++ ) {
      const KFParticle &part = *(parts[ie]);
      ok &= part.NDaughters() == nD;
    }
//    assert(ok);
    if (!ok) {
      std::cout << " void CbmKFParticle_simd::Create(CbmKFParticle *parts[], int N) " << std::endl;
      exit(1);
    }
  }
  fDaughterIds.resize( (parts[0])->NDaughters(), int_v(-1) );

  for ( int iPart = 0; iPart < float_vLen; iPart++ ) {
    Int_t iEntry = (iPart < nPart) ? iPart : 0; 
    KFParticle &part = *(parts[iEntry]);

    fId[iEntry] = part.Id();
    for(int iD=0; iD<part.NDaughters(); iD++)
      fDaughterIds[iD][iEntry] = part.DaughterIds()[iD];

    fPDG[iEntry] = part.GetPDG();
    
    for( int i = 0; i < 8; ++i )
      fP[i][iEntry] = part.Parameters()[i];
    for( int i = 0; i < 36; ++i )
      fC[i][iEntry] = part.CovarianceMatrix()[i];

    fNDF[iEntry] = part.GetNDF();
    fChi2[iEntry] = part.GetChi2();
    fQ[iEntry] = part.GetQ();
    fAtProductionVertex = part.GetAtProductionVertex(); // CHECKME
#ifdef NonhomogeneousField
    fField.SetOneEntry( part.GetFieldCoeff(), iEntry);
#endif
  }
}

KFParticleSIMD::KFParticleSIMD( KFParticle &part): KFParticleBaseSIMD()
#ifdef NonhomogeneousField
, fField()
#endif
{

 fId = part.Id();
 fNDF = part.GetNDF();
 fChi2 = part.GetChi2();
 fQ = part.GetQ();
 fPDG = part.GetPDG();
 fAtProductionVertex = part.GetAtProductionVertex();

  SetNDaughters(part.NDaughters());
  for( int i = 0; i < part.NDaughters(); ++i ) {
    fDaughterIds.push_back( part.DaughterIds()[i] );
  }
  
  for( int i = 0; i < 8; ++i )
    fP[i] = part.Parameters()[i];
  for( int i = 0; i < 36; ++i )
    fC[i] = part.CovarianceMatrix()[i];

#ifdef NonhomogeneousField
  fField = KFParticleFieldRegion(part.GetFieldCoeff());
#endif
}

float_m KFParticleSIMD::GetDistanceFromVertexXY( const float_v vtx[], const float_v Cv[], float_v &val, float_v &err ) const
{
  //* Calculate DCA distance from vertex (transverse impact parameter) in XY
  //* v = [xy], Cv=[Cxx,Cxy,Cyy ]-covariance matrix
  
  float_v mP[8];
  float_v mC[36];
  
  float_v dsdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  const float_v dS = GetDStoPoint(vtx, dsdr);
  Transport( dS, dsdr, mP, mC );  

  float_v dx = mP[0] - vtx[0];
  float_v dy = mP[1] - vtx[1];
  float_v px = mP[3];
  float_v py = mP[4];
  float_v pt = sqrt(px*px + py*py);
  float_v ex(Vc::Zero), ey(Vc::Zero);
  float_m mask = ( pt < float_v(1.e-4) );

  pt(mask) = float_v(1.f);
  ex(!mask) = (px/pt);
  ey(!mask) = (py/pt);
  val(mask) = float_v(1.e4);
  val(!mask)= dy*ex - dx*ey;

  float_v h0 = -ey;
  float_v h1 = ex;
  float_v h3 = (dy*ey + dx*ex)*ey/pt;
  float_v h4 = -(dy*ey + dx*ex)*ex/pt;
  
  err = 
    h0*(h0*GetCovariance(0,0) + h1*GetCovariance(0,1) + h3*GetCovariance(0,3) + h4*GetCovariance(0,4) ) +
    h1*(h0*GetCovariance(1,0) + h1*GetCovariance(1,1) + h3*GetCovariance(1,3) + h4*GetCovariance(1,4) ) +
    h3*(h0*GetCovariance(3,0) + h1*GetCovariance(3,1) + h3*GetCovariance(3,3) + h4*GetCovariance(3,4) ) +
    h4*(h0*GetCovariance(4,0) + h1*GetCovariance(4,1) + h3*GetCovariance(4,3) + h4*GetCovariance(4,4) );

  if( Cv ){
    err+= h0*(h0*Cv[0] + h1*Cv[1] ) + h1*(h0*Cv[1] + h1*Cv[2] ); 
  }

  err = sqrt(abs(err));

  return mask;
}

float_m KFParticleSIMD::GetDistanceFromVertexXY( const float_v vtx[], float_v &val, float_v &err ) const
{
  return GetDistanceFromVertexXY( vtx, 0, val, err );
}


float_m KFParticleSIMD::GetDistanceFromVertexXY( const KFParticleSIMD &Vtx, float_v &val, float_v &err ) const 
{
  //* Calculate distance from vertex [cm] in XY-plane

  return GetDistanceFromVertexXY( Vtx.fP, Vtx.fC, val, err );
}

#ifdef HomogeneousField
float_m KFParticleSIMD::GetDistanceFromVertexXY( const KFPVertex &Vtx, float_v &val, float_v &err ) const 
{
  //* Calculate distance from vertex [cm] in XY-plane

  return GetDistanceFromVertexXY( KFParticleSIMD(Vtx), val, err );
}
#endif

float_v KFParticleSIMD::GetDistanceFromVertexXY( const float_v vtx[] ) const
{
  //* Calculate distance from vertex [cm] in XY-plane
  float_v val, err;
  GetDistanceFromVertexXY( vtx, 0, val, err );
  return val;
}

float_v KFParticleSIMD::GetDistanceFromVertexXY( const KFParticleSIMD &Vtx ) const 
{
  //* Calculate distance from vertex [cm] in XY-plane

  return GetDistanceFromVertexXY( Vtx.fP );
}

#ifdef HomogeneousField
float_v KFParticleSIMD::GetDistanceFromVertexXY( const KFPVertex &Vtx ) const 
{
  //* Calculate distance from vertex [cm] in XY-plane

  return GetDistanceFromVertexXY( KFParticleSIMD(Vtx).fP );
}
#endif

float_v KFParticleSIMD::GetDistanceFromParticleXY( const KFParticleSIMD &p ) const 
{
  //* Calculate distance to other particle [cm]

  float_v dS[2];
  float_v dsdr[4][6];
  GetDStoParticle( p, dS, dsdr );   
  float_v mP[8], mC[36], mP1[8], mC1[36];
  Transport( dS[0], dsdr[0], mP, mC ); 
  p.Transport( dS[1], dsdr[3], mP1, mC1 ); 
  float_v dx = mP[0]-mP1[0]; 
  float_v dy = mP[1]-mP1[1]; 
  return sqrt(dx*dx+dy*dy);
}

float_v KFParticleSIMD::GetDeviationFromParticleXY( const KFParticleSIMD &p ) const 
{
  //* Calculate sqrt(Chi2/ndf) deviation from other particle

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
      
  for(int iC=0; iC<3; iC++)
    mC1[iC] += V0Tmp[iC] + mC2[iC] + V1Tmp[iC];

  float_v d[3]={ mP2[0]-mP1[0], mP2[1]-mP1[1], mP2[2]-mP1[2]};
  
  return ( ( mC1[0]*d[0] + mC1[1]*d[1])*d[0]
           +(mC1[1]*d[0] + mC1[2]*d[1])*d[1] );
}


float_v KFParticleSIMD::GetDeviationFromVertexXY( const float_v vtx[], const float_v Cv[] ) const 
{
  //* Calculate sqrt(Chi2/ndf) deviation from vertex
  //* v = [xyz], Cv=[Cxx,Cxy,Cyy,Cxz,Cyz,Czz]-covariance matrix

  float_v mP[8];
  float_v mC[36];
  float_v dsdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  const float_v dS = GetDStoPoint(vtx, dsdr);
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
  
  float_v d[3]={ vtx[0]-mP[0], vtx[1]-mP[1], vtx[2]-mP[2]};

  return ( ( mC[0]*d[0] + mC[1]*d[1] )*d[0]
           +(mC[1]*d[0] + mC[2]*d[1] )*d[1] );
}


float_v KFParticleSIMD::GetDeviationFromVertexXY( const KFParticleSIMD &Vtx ) const  
{
  //* Calculate sqrt(Chi2/ndf) deviation from vertex
  //* v = [xyz], Cv=[Cxx,Cxy,Cyy,Cxz,Cyz,Czz]-covariance matrix

  return GetDeviationFromVertexXY( Vtx.fP, Vtx.fC );
}

#ifdef HomogeneousField
float_v KFParticleSIMD::GetDeviationFromVertexXY( const KFPVertex &Vtx ) const 
{
  //* Calculate sqrt(Chi2/ndf) deviation from vertex
  //* v = [xyz], Cv=[Cxx,Cxy,Cyy,Cxz,Cyz,Czz]-covariance matrix

  KFParticleSIMD v(Vtx);
  return GetDeviationFromVertexXY( v.fP, v.fC );
}
#endif

float_v KFParticleSIMD::GetAngle  ( const KFParticleSIMD &p ) const 
{
  //* Calculate the opening angle between two particles

  float_v ds[2] = {0.f,0.f};
  float_v dsdr[4][6];
  GetDStoParticle( p, ds, dsdr );   
  float_v mP[8], mC[36], mP1[8], mC1[36];
  Transport( ds[0], dsdr[0], mP, mC ); 
  p.Transport( ds[1], dsdr[3], mP1, mC1 ); 
  float_v n = sqrt( mP[3]*mP[3] + mP[4]*mP[4] + mP[5]*mP[5] );
  float_v n1= sqrt( mP1[3]*mP1[3] + mP1[4]*mP1[4] + mP1[5]*mP1[5] );
  n*=n1;
  float_v a(Vc::Zero);
  float_m mask = (n>(1.e-8f));
  a(mask) = ( mP[3]*mP1[3] + mP[4]*mP1[4] + mP[5]*mP1[5] )/n;
  mask = ( abs(a) < float_v(1.f));
  float_m aPos = (a>=float_v(Vc::Zero));
  a(mask) = KFPMath::ACos(a);
  a((!mask) && aPos) = float_v(Vc::Zero);
  a((!mask) && (!aPos)) = 3.1415926535f;
  return a;
}

float_v KFParticleSIMD::GetAngleXY( const KFParticleSIMD &p ) const 
{
  //* Calculate the opening angle between two particles in XY plane

  float_v ds[2] = {0.f,0.f};
  float_v dsdr[4][6];
  GetDStoParticle( p, ds, dsdr );   
  float_v mP[8], mC[36], mP1[8], mC1[36];
  Transport( ds[0], dsdr[0], mP, mC ); 
  p.Transport( ds[1], dsdr[3], mP1, mC1 ); 
  float_v n = sqrt( mP[3]*mP[3] + mP[4]*mP[4] );
  float_v n1= sqrt( mP1[3]*mP1[3] + mP1[4]*mP1[4] );
  n*=n1;
  float_v a(Vc::Zero);
  float_m mask = (n>(1.e-8f));
  a = ( mP[3]*mP1[3] + mP[4]*mP1[4] )/n;
  a(!mask) = 0.f;
  mask = ( abs(a) < float_v(1.f));
  float_m aPos = (a>=float_v(Vc::Zero));
  a(mask) = KFPMath::ACos(a);
  a((!mask) && aPos) = float_v(Vc::Zero);
  a((!mask) && (!aPos)) = 3.1415926535f;
  return a;
}

float_v KFParticleSIMD::GetAngleRZ( const KFParticleSIMD &p ) const 
{
  //* Calculate the opening angle between two particles in RZ plane

  float_v ds[2] = {0.f,0.f};
  float_v dsdr[4][6];
  GetDStoParticle( p, ds, dsdr );   
  float_v mP[8], mC[36], mP1[8], mC1[36];
  Transport( ds[0], dsdr[0], mP, mC ); 
  p.Transport( ds[1], dsdr[3], mP1, mC1 );  
  float_v nr = sqrt( mP[3]*mP[3] + mP[4]*mP[4] );
  float_v n1r= sqrt( mP1[3]*mP1[3] + mP1[4]*mP1[4]  );
  float_v n = sqrt( nr*nr + mP[5]*mP[5] );
  float_v n1= sqrt( n1r*n1r + mP1[5]*mP1[5] );
  n*=n1;
  float_v a(Vc::Zero);
  float_m mask = (n>(1.e-8f));
  a(mask) = ( nr*n1r +mP[5]*mP1[5])/n;
  mask = ( abs(a) < float_v(Vc::Zero));
  float_m aPos = (a>=float_v(Vc::Zero));
  a(mask) = KFPMath::ACos(a);
  a((!mask) && aPos) = float_v(Vc::Zero);
  a((!mask) && (!aPos)) = 3.1415926535f;
  return a;
}

  // * Pseudo Proper Time of decay = (r*pt) / |pt| * M/|pt|
float_v KFParticleSIMD::GetPseudoProperDecayTime( const KFParticleSIMD &pV, const float_v& mass, float_v* timeErr2 ) const
{ // TODO optimize with respect to time and stability
  const float_v ipt2 = 1/( Px()*Px() + Py()*Py() );
  const float_v mipt2 = mass*ipt2;
  const float_v dx = X() - pV.X();
  const float_v dy = Y() - pV.Y();

  if ( timeErr2 ) {
      // -- calculate error = sigma(f(r)) = f'Cf'
      // r = {x,y,px,py,x_pV,y_pV}
      // df/dr = { px*m/pt^2,
      //     py*m/pt^2,
      //    ( x - x_pV )*m*(1/pt^2 - 2(px/pt^2)^2),
      //    ( y - y_pV )*m*(1/pt^2 - 2(py/pt^2)^2),
      //     -px*m/pt^2,
      //     -py*m/pt^2 }
    const float_v f0 = Px()*mipt2;
    const float_v f1 = Py()*mipt2;
    const float_v mipt2derivative = mipt2*(1-2*Px()*Px()*ipt2);
    const float_v f2 = dx*mipt2derivative;
    const float_v f3 = -dy*mipt2derivative;
    const float_v f4 = -f0;
    const float_v f5 = -f1;

    const float_v& mC00 =    GetCovariance(0,0);
    const float_v& mC10 =    GetCovariance(0,1);
    const float_v& mC11 =    GetCovariance(1,1);
    const float_v& mC20 =    GetCovariance(3,0);
    const float_v& mC21 =    GetCovariance(3,1);
    const float_v& mC22 =    GetCovariance(3,3);
    const float_v& mC30 =    GetCovariance(4,0);
    const float_v& mC31 =    GetCovariance(4,1);
    const float_v& mC32 =    GetCovariance(4,3);
    const float_v& mC33 =    GetCovariance(4,4);
    const float_v& mC44 = pV.GetCovariance(0,0);
    const float_v& mC54 = pV.GetCovariance(1,0);
    const float_v& mC55 = pV.GetCovariance(1,1);

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

void KFParticleSIMD::GetKFParticle(KFParticle &Part, int iPart)
{
  Part.SetId(static_cast<int>(Id()[iPart]));

  Part.CleanDaughtersId();
  for( unsigned int i = 0; i < DaughterIds().size(); i++ )
    Part.AddDaughterId(static_cast<int>(DaughterIds()[i][iPart]));

  Part.SetPDG( static_cast<int>(GetPDG()[iPart]) );

  for(int iP=0; iP<8; iP++)
    Part.Parameters()[iP] = Parameters()[iP][iPart];
  for(int iC=0; iC<36; iC++)
    Part.CovarianceMatrix()[iC] = CovarianceMatrix()[iC][iPart];

  Part.NDF() = static_cast<int>(GetNDF()[iPart]);
  Part.Chi2() = GetChi2()[iPart];
  Part.Q()    = GetQ()[iPart];
  Part.SetAtProductionVertex(fAtProductionVertex);
#ifdef NonhomogeneousField
  for(int iF=0; iF<10; iF++)
    Part.SetFieldCoeff(fField.fField[iF][iPart], iF);
#endif
}

void KFParticleSIMD::GetKFParticle(KFParticle* Part, int nPart)
{
  for(int i=0; i<nPart; i++)
    GetKFParticle(Part[i],i);
}
