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



#include "KFParticleSIMD.h"
#include "KFParticle.h"
#include <iostream>
#ifdef HomogeneousField
float32_v KFParticleSIMD::fgBz = -5.f;  //* Bz compoment of the magnetic field
#endif

void KFParticleSIMD::Create( const float32_v Param[], const float32_v Cov[], int32_v Charge, float32_v mass /*Int_t PID*/ )
{
  /** Constructor from a "cartesian" track, mass hypothesis should be provided
   **
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

  float32_v energy = sqrt( mass*mass + fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5]);
  fP[6] = energy;
  fP[7] = 0;
  fQ = Charge;
  fNDF = 0;
  fChi2 = 0;

  float32_v energyInv = 1.f/energy;
  float32_v 
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

  SumDaughterMass = mass;
  fMassHypo = mass;
}

KFParticleSIMD::KFParticleSIMD( const KFPTrack *track, Int_t PID ): fQ(0), fNDF(-3), fChi2(0.f), SumDaughterMass(0.f), fMassHypo(-1.f), fId(-1), fPDG(0), fConstructMethod(0), fDaughterIds()
#ifdef NonhomogeneousField
, fField()
#endif
{
  /** Constructor of the particle from an array of tracks.
   ** \param[in] track - pointer to the array of n=SimdLen tracks
   ** \param[in] PID - the PID hypothesis common for all elements of the SIMD vector
   **/
  
  Double_t r[3];
  Double_t C[21];

  alignas(SimdSize) float pArray[6][SimdLen]{0};
  alignas(SimdSize) float cArray[21][SimdLen]{0};
  alignas(SimdSize) int32_t qArray[SimdLen]{0};
  alignas(SimdSize) float chi2Array[SimdLen]{0};
  alignas(SimdSize) int32_t ndfArray[SimdLen]{0};

  for(Int_t iPart = 0; iPart<SimdLen; iPart++)
  {
    track[iPart].XvYvZv(r);
    for(Int_t i=0; i<3; i++)
      pArray[i][iPart] = r[i];
    track[iPart].PxPyPz(r);
    for(Int_t i=0; i<3; i++)
      pArray[i+3][iPart] = r[i];
    qArray[iPart] = track[iPart].Charge();
    track[iPart].GetCovarianceXYZPxPyPz( C );
    for(Int_t i=0; i<21; i++)
      cArray[i][iPart] = C[i];
    chi2Array[iPart] = track[iPart].GetChi2();
    ndfArray[iPart] = track[iPart].GetNDF();
  }

  for(int i=0; i<6; i++)
    fP[i].load(pArray[i]);
  for(int i=0; i<21; i++)
    fC[i].load(cArray[i]);
  fQ.load(qArray);
  fChi2.load(chi2Array);
  fNDF.load(ndfArray);

  float32_v mass = KFParticleDatabase::Instance()->GetMass(PID);
  Create(fP,fC,fQ,mass);
}

KFParticleSIMD::KFParticleSIMD(KFPTrack &Track, const Int_t *pdg): fQ(0), fNDF(-3), fChi2(0.f), SumDaughterMass(0.f), fMassHypo(-1.f), fId(-1), fPDG(0), fConstructMethod(0), fDaughterIds()
#ifdef NonhomogeneousField
, fField()
#endif
{
  /** Constructor of the particle from a single track. The same track is put to each element of the SIMD vector.
   ** \param[in] Track - track in the KFPTrack format
   ** \param[in] PID - the PID hypothesis common for all elements of the SIMD vector
   **/
  
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

  float32_v mass = KFParticleDatabase::Instance()->GetMass(*pdg);
  Create(fP,fC,fQ,mass);

  fChi2 = Track.GetChi2();
  fNDF = Track.GetNDF();
}

KFParticleSIMD::KFParticleSIMD(KFPTrackVector &track, int n, const Int_t *pdg): fQ(0), fNDF(-3), fChi2(0.f), SumDaughterMass(0.f), fMassHypo(-1.f), fId(-1), fPDG(0), fConstructMethod(0), fDaughterIds()
#ifdef NonhomogeneousField
, fField()
#endif
{
  /** Constructor of the particle from a single track with index "n" stored in the KFPTrackVector format. 
   ** The same track is put to each element of the SIMD vector.
   ** \param[in] track - an array with tracks in the KFPTrackVector format
   ** \param[in] n - index of the track to be used
   ** \param[in] pdg - pointer to the pdg hypothesis
   **/
  
  for(int i=0; i<6; i++)
    fP[i] = track.Parameter(i)[n];
  for(int i=0; i<21; i++)
    fC[i] = track.Covariance(i)[n];
#ifdef NonhomogeneousField
  for(int i=0; i<10; i++)
    fField.fField[i] = track.FieldCoefficient(i)[n];
#endif
  fQ = track.Q()[n];

  float32_v mass = KFParticleDatabase::Instance()->GetMass(*pdg);
  Create(fP,fC,fQ,mass);
}

  
KFParticleSIMD::KFParticleSIMD(KFPTrack* Track[], int NTracks, const Int_t *pdg): fQ(0), fNDF(-3), fChi2(0.f), SumDaughterMass(0.f), fMassHypo(-1.f), fId(-1), fPDG(0), fConstructMethod(0), fDaughterIds()
#ifdef NonhomogeneousField
, fField()
#endif
{
  /** Constructor of the particle from an array tracks.
   ** \param[in] Track - an array of pointers to tracks in the KFPTrack format
   ** \param[in] NTracks - number of tracks in the arry
   ** \param[in] pdg - pointer to the pdg hypothesis common for all elements of the SIMD vector
   **/
  Create(Track, NTracks, pdg);
}

void KFParticleSIMD::Create(KFPTrack* Track[], int NTracks, const Int_t *pdg)
{
  /** Create a particle from from an array tracks.
   ** \param[in] Track - an array of pointers to tracks in the KFPTrack format
   ** \param[in] NTracks - number of tracks in the arry
   ** \param[in] pdg - pointer to the pdg hypothesis common for all elements of the SIMD vector
   **/
  
  Double_t r[3];
  Double_t C[21];

  alignas(SimdSize) float pArray[6][SimdLen]{0};
  alignas(SimdSize) float cArray[21][SimdLen]{0};
  alignas(SimdSize) int32_t qArray[SimdLen]{0};
  alignas(SimdSize) float chi2Array[SimdLen]{0};
  alignas(SimdSize) int32_t ndfArray[SimdLen]{0};

  for(Int_t iPart = 0; iPart<SimdLen; iPart++)
  {
    Int_t iEntry = (iPart < NTracks) ? iPart : 0; 
    Track[iEntry]->XvYvZv(r);
    for(Int_t i=0; i<3; i++)
      pArray[i][iEntry] = r[i];
    Track[iEntry]->PxPyPz(r);
    for(Int_t i=0; i<3; i++)
      pArray[i+3][iEntry] = r[i];
    qArray[iEntry] = Track[iEntry]->Charge();
    Track[iEntry]->GetCovarianceXYZPxPyPz( C );
    for(Int_t i=0; i<21; i++)
      cArray[i][iEntry] = C[i];
    chi2Array[iPart] = Track[iEntry]->GetChi2();
    ndfArray[iPart] = Track[iEntry]->GetNDF();
  }

  for(int i=0; i<6; i++)
    fP[i].load(pArray[i]);
  for(int i=0; i<21; i++)
    fC[i].load(cArray[i]);
  fQ.load(qArray);
  fChi2.load(chi2Array);
  fNDF.load(ndfArray);

  float32_v mass = KFParticleDatabase::Instance()->GetMass(*pdg);
  Create(fP,fC,fQ,mass);
}

KFParticleSIMD::KFParticleSIMD(KFPTrackVector &track, int32_v& index, const int32_v& pdg): fQ(0), fNDF(-3), fChi2(0.f), SumDaughterMass(0.f), fMassHypo(-1.f), fId(-1), fPDG(0), fConstructMethod(0), fDaughterIds()
#ifdef NonhomogeneousField
, fField()
#endif
{
  /** Constructor of the particle from a set of tracks with random indices "index" stored in the KFPTrackVector format.
   ** \param[in] track - an array with tracks in the KFPTrackVector format
   ** \param[in] index - indices of the tracks to be converted to the KFParticleSIMD object
   ** \param[in] pdg - a SIMD vector with an individual pdg hypothesis for each element
   **/
  
  Create(track, index, pdg);
}

void KFParticleSIMD::Create(KFPTrackVector &track, int32_v& index, const int32_v& pdg)
{
  /** Create a particle from a set of tracks with indices "index" stored in the KFPTrackVector format.
   ** The function should be used in case if indices are random. If they are aligned please use function Load()
   ** that will benefit of the aligned memory reading and result in a faster code.
   ** \param[in] track - an array with tracks in the KFPTrackVector format
   ** \param[in] index - indices of the tracks to be converted to the KFParticleSIMD object
   ** \param[in] pdg - a SIMD vector with an individual pdg hypothesis for each element
   **/
  
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

  float32_v mass = KFParticleDatabase::Instance()->GetMass(pdg);
  Create(fP,fC,fQ,mass);
}

void KFParticleSIMD::Rotate()
{
  /** Rotates the entries of each SIMD vector of the data members. */
  
  for(int i=0; i<7; i++)
    fP[i] = fP[i].rotate<1>();
  for(int i=0; i<27; i++)
    fC[i] = fC[i].rotate<1>();
#ifdef NonhomogeneousField
  for(int i=0; i<10; i++)
    fField.fField[i] = fField.fField[i].rotate<1>();
#endif
  fQ = fQ.rotate<1>();
  fId = fId.rotate<1>();
}

KFParticleSIMD::KFParticleSIMD(KFPEmcCluster &track, int32_v& index, const KFParticleSIMD& vertexGuess): fQ(0), fNDF(-3), fChi2(0.f), SumDaughterMass(0.f), fMassHypo(-1.f), fId(-1), fPDG(0), fConstructMethod(0), fDaughterIds()
#ifdef NonhomogeneousField
, fField()
#endif
{
  /** Constructor of gamma particles from a set of clusters of the electromagnetic calorimeter (EMC) 
   ** with random indices "index". The vertex hypothesis should be provided for the estimation of the momentum.
   ** \param[in] track - an array of EMC clusters
   ** \param[in] index - indices of the clusters to be converted to the KFParticleSIMD object
   ** \param[in] vertexGuess - vertex guess for estimation of the momentum of created gamma particles
   **/
  
  Create(track, index, vertexGuess);
}

void KFParticleSIMD::Create(KFPEmcCluster &track, int32_v& index, const KFParticleSIMD& vertexGuess)
{
  /** Creates gamma particles from a set of clusters of the electromagnetic calorimeter (EMC) 
   ** with random indices "index". The vertex hypothesis should be provided for the estimation of the momentum.
   ** \param[in] track - an array of EMC clusters
   ** \param[in] index - indices of the clusters to be converted to the KFParticleSIMD object
   ** \param[in] vertexGuess - vertex guess for estimation of the momentum of created gamma particles
   **/
  
  for(int i=0; i<3; i++)
    fP[i].gather(&(track.Parameter(i)[0]), index);
  fP[6].gather(&(track.Parameter(3)[0]), index);
  
  const float32_v& dx = fP[0] - vertexGuess.fP[0];
  const float32_v& dy = fP[1] - vertexGuess.fP[1];
  const float32_v& dz = fP[2] - vertexGuess.fP[2];
  const float32_v& dl2 = dx*dx + dy*dy + dz*dz;
  const float32_v& dl = sqrt(dl2);

  fP[0] = vertexGuess.fP[0];
  fP[1] = vertexGuess.fP[1];
  fP[2] = vertexGuess.fP[2];
  
  fP[3] = dx/dl * fP[6];
  fP[4] = dy/dl * fP[6];
  fP[5] = dz/dl * fP[6];
  
  float32_v V[10];
  for(int i=0; i<10; i++)
    V[i].gather(&(track.Covariance(i)[0]), index);
  
  float32_v J[7][4];
  for(int i=0; i<7; i++)
    for(int j=0; j<4; j++)
      J[i][j] = 0.f;
  J[0][0] = 1.f; J[1][1] = 1.f; J[2][2] = 1.f; J[6][3] = 1.f;
  J[3][0] = fP[6]/dl * (1.f - dx*dx/dl2); J[3][1] = -dx*dy*fP[6]/(dl*dl2); J[3][2] = -dx*dz*fP[6]/(dl*dl2); J[3][3] = dx/dl;
  J[4][0] = -dx*dy*fP[6]/(dl*dl2); J[4][1] = fP[6]/dl * (1.f - dy*dy/dl2); J[4][2] = -dy*dz*fP[6]/(dl*dl2); J[4][3] = dy/dl;
  J[5][0] = -dx*dz*fP[6]/(dl*dl2); J[5][1] = -dy*dz*fP[6]/(dl*dl2); J[5][2] = fP[6]/dl * (1.f - dz*dz/dl2); J[5][3] = dz/dl;
  
  float32_v VJT[4][7]; // V*J^T
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
  
  fQ = int32_v(0);
  fNDF = 0;
  fChi2 = 0;
  
  SumDaughterMass = float32_v(0.f);
  fMassHypo = float32_v(0.f);
}

KFParticleSIMD::KFParticleSIMD(KFPEmcCluster &track, int index, const KFParticleSIMD& vertexGuess): fQ(0), fNDF(-3), fChi2(0.f), SumDaughterMass(0.f), fMassHypo(-1.f), fId(-1), fPDG(0), fConstructMethod(0), fDaughterIds()
#ifdef NonhomogeneousField
, fField()
#endif
{
  /** Constructr gamma particles from a set of consequetive clusters of the electromagnetic calorimeter (EMC) 
   ** starting from the index "index". The vertex hypothesis should be provided for the estimation of the momentum.
   ** \param[in] track - an array with tracks in the KFPTrackVector format
   ** \param[in] index - index of the first EMC cluster
   ** \param[in] vertexGuess - vertex guess for estimation of the momentum of created gamma particles
   **/
  
  Load(track, index, vertexGuess);
}

void KFParticleSIMD::Load(KFPEmcCluster &track, int index, const KFParticleSIMD& vertexGuess)
{
  /** Create gamma particles from a set of consequetive clusters of the electromagnetic calorimeter (EMC) 
   ** starting from the index "index". The vertex hypothesis should be provided for the estimation of the momentum.
   ** \param[in] track - an array with tracks in the KFPTrackVector format
   ** \param[in] index - index of the first EMC cluster
   ** \param[in] vertexGuess - vertex guess for estimation of the momentum of created gamma particles
   **/
  
  for(int i=0; i<3; i++)
    fP[i] = reinterpret_cast<const float32_v&>(track.Parameter(i)[index]);
  fP[6] = reinterpret_cast<const float32_v&>(track.Parameter(3)[index]);
  const float32_v& dx = fP[0] - vertexGuess.fP[0];
  const float32_v& dy = fP[1] - vertexGuess.fP[1];
  const float32_v& dz = fP[2] - vertexGuess.fP[2];
  const float32_v& dl2 = dx*dx + dy*dy + dz*dz;
  const float32_v& dl = sqrt(dl2);

  fP[0] = vertexGuess.fP[0];
  fP[1] = vertexGuess.fP[1];
  fP[2] = vertexGuess.fP[2];
  
  fP[3] = dx/dl * fP[6];
  fP[4] = dy/dl * fP[6];
  fP[5] = dz/dl * fP[6];
  
  float32_v V[10];
  for(int i=0; i<10; i++)
    V[i] = reinterpret_cast<const float32_v&>(track.Covariance(i)[index]);
  
  float32_v J[7][4];
  for(int i=0; i<7; i++)
    for(int j=0; j<4; j++)
      J[i][j] = 0.f;
  J[0][0] = 1.f; J[1][1] = 1.f; J[2][2] = 1.f; J[6][3] = 1.f;
  J[3][0] = fP[6]/dl * (1.f - dx*dx/dl2); J[3][1] = -dx*dy*fP[6]/(dl*dl2); J[3][2] = -dx*dz*fP[6]/(dl*dl2); J[3][3] = dx/dl;
  J[4][0] = -dx*dy*fP[6]/(dl*dl2); J[4][1] = fP[6]/dl * (1.f - dy*dy/dl2); J[4][2] = -dy*dz*fP[6]/(dl*dl2); J[4][3] = dy/dl;
  J[5][0] = -dx*dz*fP[6]/(dl*dl2); J[5][1] = -dy*dz*fP[6]/(dl*dl2); J[5][2] = fP[6]/dl * (1.f - dz*dz/dl2); J[5][3] = dz/dl;
  
  float32_v VJT[4][7]; // V*J^T
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
  
  fQ = int32_v(0);
  fNDF = 0;
  fChi2 = 0;
  
  SumDaughterMass = float32_v(0.f);
  fMassHypo = float32_v(0.f);
}

KFParticleSIMD::KFParticleSIMD( const KFPVertex &vertex ): fQ(0), fNDF(-3), fChi2(0.f), SumDaughterMass(0.f), fMassHypo(-1.f), fId(-1), fPDG(0), fConstructMethod(0), fDaughterIds()
#ifdef NonhomogeneousField
, fField()
#endif
{
  /** Copies a vertex in KFPVertex into each element of the vectorised KFParticle
   ** \param[in] vertex - vertex to b converted
   **/
  
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
  fQ = int32_v(0);
}

void KFParticleSIMD::SetOneEntry(int iEntry, KFParticleSIMD& part, int iEntryPart)
{
  /** Copies one element of the KFParticleSIMD to one element of another KFParticleSIMD.
   ** \param[in] iEntry - index of the element of the current track, where the data will be copied
   ** \param[in] part - particle, element of which should be copied to the current particle
   ** \param[in] iEntryPart - index of the element of particle part, which should be copied to the current particle
   **/
  
  alignas(SimdSize) float floatArray[SimdLen]{0};
  alignas(SimdSize) int32_t intArray[SimdLen]{0};

  for( int i = 0; i < 7; ++i )
  {
    fP[i].store(floatArray);
    floatArray[iEntry] = part.Parameters()[i][iEntryPart];
    fP[i].load(floatArray);
  }
  for( int i = 0; i < 36; ++i )
  {
    fC[i].store(floatArray);
    floatArray[iEntry] = part.CovarianceMatrix()[i][iEntryPart];
    fC[i].load(floatArray);
  }

  fChi2.store(floatArray);
  floatArray[iEntry] = part.Chi2()[iEntryPart];;
  fChi2.load(floatArray);

  fQ.store(intArray);
  intArray[iEntry] = part.Q()[iEntryPart];
  fQ.load(intArray);

  fNDF.store(intArray);
  intArray[iEntry] = part.NDF()[iEntryPart];
  fNDF.load(intArray);

//   SumDaughterMass[iEntry] = part.SumDaughterMass[iEntryPart];
//   fMassHypo[iEntry] = part.fMassHypo[iEntryPart];

  fId.store(intArray);
  intArray[iEntry] = part.Id()[iEntryPart];
  fId.load(intArray);

  fPDG.store(intArray);
  intArray[iEntry] = part.GetPDG()[iEntryPart];
  fPDG.load(intArray);

  if(iEntry==0)
    fDaughterIds.resize( part.NDaughters(), int32_v(-1) );
  
  for(int iD=0; iD<part.NDaughters(); iD++)
  {
    fDaughterIds[iD].store(intArray);
    intArray[iEntry] = part.fDaughterIds[iD][iEntryPart];
    fDaughterIds[iD].load(intArray);
  }

#ifdef NonhomogeneousField
  fField.SetOneEntry( iEntry, part.fField, iEntryPart ); //CHECKME
#endif
}

KFParticleSIMD::KFParticleSIMD(KFParticle* parts[], const int nPart): fQ(0), fNDF(-3), fChi2(0.f), SumDaughterMass(0.f), fMassHypo(-1.f), fId(-1), fPDG(0), fConstructMethod(0), fDaughterIds()
#ifdef NonhomogeneousField
, fField()
#endif
{
  /** Constructs a vectoriesd particle from an array of scalar KFParticle objects.
   ** \param[in] parts - array of scalar KFParticle objects
   ** \param[in] nPart - number of particles in the array
   **/
  
  { // check
    bool ok = 1;

    const int nD = (parts[0])->NDaughters();
    for ( int ie = 1; ie < nPart; ie++ ) {
      const KFParticle &part = *(parts[ie]);
      ok &= part.NDaughters() == nD;
    }
    if (!ok) {
      std::cout << " void CbmKFParticle_simd::Create(CbmKFParticle *parts[], int N) " << std::endl;
      exit(1);
    }
  }

  alignas(SimdSize) float pArray[8][SimdLen]{0};
  alignas(SimdSize) float cArray[36][SimdLen]{0};
  alignas(SimdSize) float chi2Array[SimdLen]{0};
  alignas(SimdSize) int32_t qArray[SimdLen]{0};
  alignas(SimdSize) int32_t ndfArray[SimdLen]{0};
  alignas(SimdSize) int32_t pdgArray[SimdLen]{0};
  alignas(SimdSize) int32_t idArray[SimdLen]{0};

  fDaughterIds.resize( (parts[0])->NDaughters(), int32_v(-1) );

  for ( int iPart = 0; iPart < SimdLen; iPart++ ) {
    Int_t iEntry = (iPart < nPart) ? iPart : 0; 
    KFParticle &part = *(parts[iEntry]);

    for( int i = 0; i < 8; ++i )
      pArray[i][iEntry] = part.Parameters()[i];
    for( int i = 0; i < 36; ++i )
      cArray[i][iEntry] = part.CovarianceMatrix()[i];

    chi2Array[iEntry] = part.GetChi2();

    qArray[iEntry] = part.GetQ();
    ndfArray[iEntry] = part.GetNDF();
    pdgArray[iEntry] = part.GetPDG();
    idArray[iEntry] = part.Id();

#ifdef NonhomogeneousField
    fField.SetOneEntry( part.GetFieldCoeff(), iEntry);
#endif
  }

  for( int i = 0; i < 8; ++i )
    fP[i].load(pArray[i]);
  for( int i = 0; i < 36; ++i )
    fC[i].load(cArray[i]);
  fChi2.load(chi2Array);
  fQ.load(qArray);
  fNDF.load(ndfArray);
  fPDG.load(pdgArray);
  fId.load(idArray);

  for(int iD=0; iD<parts[0]->NDaughters(); iD++)
  {
    alignas(SimdSize) int32_t tmp[SimdLen]{0};
    for ( int iPart = 0; iPart < SimdLen; iPart++ ) {
      Int_t iEntry = (iPart < nPart) ? iPart : 0; 

      tmp[iEntry] = parts[iEntry]->DaughterIds()[iD];
    }
    fDaughterIds[iD].load(tmp);
  }
}

KFParticleSIMD::KFParticleSIMD( const KFParticle &part): fQ(0), fNDF(-3), fChi2(0.f), SumDaughterMass(0.f), fMassHypo(-1.f), fId(-1), fPDG(0), fConstructMethod(0), fDaughterIds()
#ifdef NonhomogeneousField
, fField()
#endif
{
  /** Constructs a vectoriesd particle from a single scalar KFParticle object. The same particle is copied to each element.
   ** \param[in] part - a scalar particle which should be copied to the current vectorised particle
   **/
  
 fId = part.Id();
 fNDF = part.GetNDF();
 fChi2 = part.GetChi2();
 fQ = part.GetQ();
 fPDG = part.GetPDG();

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

void KFParticleSIMD::operator +=( const KFParticleSIMD &Daughter )
{
  /** Operator to add daughter to the current particle. Calls AddDaughter() function.
   ** \param[in] Daughter - the daughter particle
   **/
  
  AddDaughter( Daughter );
}

void KFParticleSIMD::GetMeasurement( const KFParticleSIMD& daughter, float32_v m[], float32_v V[], float32_v D[3][3] )
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
    float32_v ds[2] = {0.f,0.f};
    float32_v dsdr[4][6];
    float32_v F1[36], F2[36], F3[36], F4[36];
    for(int i1=0; i1<36; i1++)
    {
      F1[i1] = 0;
      F2[i1] = 0;
      F3[i1] = 0;
      F4[i1] = 0;
    }
    GetDStoParticle( daughter, ds, dsdr );
    
    float32_v V0Tmp[36] ;
    float32_v V1Tmp[36] ;

    float32_v C[36];
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
    
    float32_v C1F1T[6][6];
    for(int i=0; i<6; i++)
      for(int j=0; j<6; j++)
      {
        C1F1T[i][j] = 0;
        for(int k=0; k<6; k++)
        {
          C1F1T[i][j] +=  C[IJ(i,k)] * F1[j*6+k];
        }
      }
    float32_v F3C1F1T[6][6];
    for(int i=0; i<6; i++)
      for(int j=0; j<6; j++)
      {
        F3C1F1T[i][j] = 0;
        for(int k=0; k<6; k++)
        {
          F3C1F1T[i][j] += F3[i*6+k] * C1F1T[k][j];
        }
      }
    float32_v C2F2T[6][6];
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
    float32_v dsdr[6];
    float32_v dS = daughter.GetDStoPoint(fP, dsdr);
    
    float32_v dsdp[6] = {-dsdr[0], -dsdr[1], -dsdr[2], 0.f, 0.f, 0.f};
    
    float32_v F[36], F1[36];
    for(int i2=0; i2<36; i2++)
    {
      F[i2]  = 0;
      F1[i2] = 0;
    }
    daughter.Transport(dS, dsdr, m, V, dsdp, F, F1);
    
//     float32_v V1Tmp[36] = {0.};
//     MultQSQt(F1, fC, V1Tmp, 6);
    
//     for(int iC=0; iC<21; iC++)
//       V[iC] += V1Tmp[iC];
    
    float32_v VFT[3][6];
    for(int i=0; i<3; i++)
      for(int j=0; j<6; j++)
      {
        VFT[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          VFT[i][j] +=  fC[IJ(i,k)] * F1[j*6+k];
        }
      }
    
    float32_v FVFT[6][6];
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

inline void KFParticleSIMD::AddDaughter( const KFParticleSIMD &Daughter )
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
#ifdef NonhomogeneousField
    fField = Daughter.fField;
#endif
    fNDF   = -1;
    fQ     =  Daughter.GetQ();
    for( Int_t i=0; i<7; i++ ) fP[i] = Daughter.fP[i];
    for( Int_t i=0; i<28; i++ ) fC[i] = Daughter.fC[i];
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

void KFParticleSIMD::AddDaughterWithEnergyFit( const KFParticleSIMD &Daughter )
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

    float32_v m[8], mV[36];

    float32_v D[3][3];
    GetMeasurement(Daughter, m, mV, D);

    float32_v mS[6]= { fC[0]+mV[0], 
                     fC[1]+mV[1], fC[2]+mV[2], 
                     fC[3]+mV[3], fC[4]+mV[4], fC[5]+mV[5] };    
    InvertCholetsky3(mS);
    //* Residual (measured - estimated)
    
    float32_v zeta[3] = { m[0]-fP[0], m[1]-fP[1], m[2]-fP[2] };    

    float32_v K[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        K[i][j] = 0;
        for(int k=0; k<3; k++)
          K[i][j] += fC[IJ(i,k)] * mS[IJ(k,j)];
      }
    
    //* CHt = CH' - D'
    float32_v mCHt0[7], mCHt1[7], mCHt2[7];

    mCHt0[0]=fC[ 0] ;       mCHt1[0]=fC[ 1] ;       mCHt2[0]=fC[ 3] ;
    mCHt0[1]=fC[ 1] ;       mCHt1[1]=fC[ 2] ;       mCHt2[1]=fC[ 4] ;
    mCHt0[2]=fC[ 3] ;       mCHt1[2]=fC[ 4] ;       mCHt2[2]=fC[ 5] ;
    mCHt0[3]=fC[ 6]-mV[ 6]; mCHt1[3]=fC[ 7]-mV[ 7]; mCHt2[3]=fC[ 8]-mV[ 8];
    mCHt0[4]=fC[10]-mV[10]; mCHt1[4]=fC[11]-mV[11]; mCHt2[4]=fC[12]-mV[12];
    mCHt0[5]=fC[15]-mV[15]; mCHt1[5]=fC[16]-mV[16]; mCHt2[5]=fC[17]-mV[17];
    mCHt0[6]=fC[21]-mV[21]; mCHt1[6]=fC[22]-mV[22]; mCHt2[6]=fC[23]-mV[23];
  
    //* Kalman gain K = mCH'*S
    
    float32_v k0[7], k1[7], k2[7];
    
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

    float32_v K2[3][3];
    for(int i=0; i<3; i++)
    {
      for(int j=0; j<3; j++)
        K2[i][j] = -K[j][i];
      K2[i][i] += 1;
    }

    float32_v A[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        A[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          A[i][j] += D[i][k] * K2[k][j];
        }
      }
    
    float32_v M[3][3];
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
    fChi2 += (mS[0]*zeta[0] + mS[1]*zeta[1] + mS[3]*zeta[2])*zeta[0]
      +      (mS[1]*zeta[0] + mS[2]*zeta[1] + mS[4]*zeta[2])*zeta[1]
      +      (mS[3]*zeta[0] + mS[4]*zeta[1] + mS[5]*zeta[2])*zeta[2];
  }
}

void KFParticleSIMD::AddDaughterWithEnergyFitMC( const KFParticleSIMD &Daughter )
{
  /** Adds daughter to the current particle. Uses slower but correct mathematics 
   ** which requires that the masses of daughter particles 
   ** stays fixed in the construction process.
   ** \param[in] Daughter - the daughter particle
   **/

  Int_t maxIter = 1;

  for( Int_t iter=0; iter<maxIter; iter++ ){

    float32_v m[8], mV[36];

    float32_v D[3][3];
    GetMeasurement(Daughter, m, mV, D);
    
    float32_v mS[6]= { fC[0]+mV[0], 
                   fC[1]+mV[1], fC[2]+mV[2], 
                   fC[3]+mV[3], fC[4]+mV[4], fC[5]+mV[5] };
    InvertCholetsky3(mS);
    //* Residual (measured - estimated)

    float32_v zeta[3] = { m[0]-fP[0], m[1]-fP[1], m[2]-fP[2] };    

    float32_v K[3][6];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        K[i][j] = 0;
        for(int k=0; k<3; k++)
          K[i][j] += fC[IJ(i,k)] * mS[IJ(k,j)];
      }

    
    //* CHt = CH'
    
    float32_v mCHt0[7], mCHt1[7], mCHt2[7];
    
    mCHt0[0]=fC[ 0] ; mCHt1[0]=fC[ 1] ; mCHt2[0]=fC[ 3] ;
    mCHt0[1]=fC[ 1] ; mCHt1[1]=fC[ 2] ; mCHt2[1]=fC[ 4] ;
    mCHt0[2]=fC[ 3] ; mCHt1[2]=fC[ 4] ; mCHt2[2]=fC[ 5] ;
    mCHt0[3]=fC[ 6] ; mCHt1[3]=fC[ 7] ; mCHt2[3]=fC[ 8] ;
    mCHt0[4]=fC[10] ; mCHt1[4]=fC[11] ; mCHt2[4]=fC[12] ;
    mCHt0[5]=fC[15] ; mCHt1[5]=fC[16] ; mCHt2[5]=fC[17] ;
    mCHt0[6]=fC[21] ; mCHt1[6]=fC[22] ; mCHt2[6]=fC[23] ;
  
    //* Kalman gain K = mCH'*S
    
    float32_v k0[7], k1[7], k2[7];
    
    for(Int_t i=0;i<7;++i){
      k0[i] = mCHt0[i]*mS[0] + mCHt1[i]*mS[1] + mCHt2[i]*mS[3];
      k1[i] = mCHt0[i]*mS[1] + mCHt1[i]*mS[2] + mCHt2[i]*mS[4];
      k2[i] = mCHt0[i]*mS[3] + mCHt1[i]*mS[4] + mCHt2[i]*mS[5];
    }

    // last itearation -> update the particle

    //* VHt = VH'
    
    float32_v mVHt0[7], mVHt1[7], mVHt2[7];
    
    mVHt0[0]=mV[ 0] ; mVHt1[0]=mV[ 1] ; mVHt2[0]=mV[ 3] ;
    mVHt0[1]=mV[ 1] ; mVHt1[1]=mV[ 2] ; mVHt2[1]=mV[ 4] ;
    mVHt0[2]=mV[ 3] ; mVHt1[2]=mV[ 4] ; mVHt2[2]=mV[ 5] ;
    mVHt0[3]=mV[ 6] ; mVHt1[3]=mV[ 7] ; mVHt2[3]=mV[ 8] ;
    mVHt0[4]=mV[10] ; mVHt1[4]=mV[11] ; mVHt2[4]=mV[12] ;
    mVHt0[5]=mV[15] ; mVHt1[5]=mV[16] ; mVHt2[5]=mV[17] ;
    mVHt0[6]=mV[21] ; mVHt1[6]=mV[22] ; mVHt2[6]=mV[23] ;
  
    //* Kalman gain Km = mCH'*S
    
    float32_v km0[7], km1[7], km2[7];
    
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

    float32_v mDf[7][7];

    for(Int_t i=0;i<7;++i){
      for(Int_t j=0;j<7;++j){
        mDf[i][j] = (km0[i]*mCHt0[j] + km1[i]*mCHt1[j] + km2[i]*mCHt2[j] );
      }
    }

    float32_v mJ1[7][7], mJ2[7][7];
    for(Int_t iPar1=0; iPar1<7; iPar1++)
    {
      for(Int_t iPar2=0; iPar2<7; iPar2++)
      {
        mJ1[iPar1][iPar2] = 0;
        mJ2[iPar1][iPar2] = 0;
      }
    }

    float32_v mMassParticle  = fP[6]*fP[6] - (fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5]);
    float32_v mMassDaughter  = m[6]*m[6] - (m[3]*m[3] + m[4]*m[4] + m[5]*m[5]);
    mMassParticle = select(mMassParticle > 0.f, sqrt(mMassParticle), mMassParticle);
    mMassParticle = select(mMassParticle <= 0.f, 0.f, mMassParticle);
    mMassDaughter = select(mMassDaughter > 0.f, sqrt(mMassDaughter), mMassDaughter);
    mMassDaughter = select(mMassDaughter <= 0.f, 0.f, mMassDaughter);

    mask32_v mask1 = fMassHypo > -0.5f;
    mask32_v mask2 = (!mask1) && ( (mMassParticle < SumDaughterMass) || (fP[6]<0.f)) ;
    SetMassConstraint(fP,fC,mJ1,fMassHypo, mask1);
    SetMassConstraint(fP,fC,mJ1,SumDaughterMass, mask2);

    mask32_v mask3 = Daughter.fMassHypo > -0.5f;
    mask32_v mask4 = ( (!mask3) && ( (mMassDaughter<Daughter.SumDaughterMass) || (m[6]<0.f)) );
    SetMassConstraint(m,mV,mJ2,Daughter.fMassHypo, mask3);
    SetMassConstraint(m,mV,mJ2,Daughter.SumDaughterMass, mask4);

    float32_v mDJ[7][7];

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

    
    float32_v K2[3][3];
    for(int i=0; i<3; i++)
    {
      for(int j=0; j<3; j++)
        K2[i][j] = -K[j][i];
      K2[i][i] += 1;
    }

    float32_v A[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        A[i][j] = 0.f;
        for(int k=0; k<3; k++)
        {
          A[i][j] += D[i][k] * K2[k][j];
        }
      }
    
    float32_v M[3][3];
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
    fChi2 += (mS[0]*zeta[0] + mS[1]*zeta[1] + mS[3]*zeta[2])*zeta[0]
      +      (mS[1]*zeta[0] + mS[2]*zeta[1] + mS[4]*zeta[2])*zeta[1]
      +      (mS[3]*zeta[0] + mS[4]*zeta[1] + mS[5]*zeta[2])*zeta[2];
  }
}

void KFParticleSIMD::SubtractDaughter( const KFParticleSIMD &Daughter )
{
  /** Subtracts a daughter particle from the mother particle. The mathematics is
   ** similar to AddDaughterWithEnergyFit() but momentum is subtracted.
   ** \param[in] Daughter - the daughter particle
   **/
  
  AddDaughterId( Daughter.Id() );

  float32_v m[8], mV[36];

  float32_v D[3][3];
  GetMeasurement(Daughter, m, mV, D);
    
  float32_v mS[6]= { fC[0]+mV[0], 
                     fC[1]+mV[1], fC[2]+mV[2], 
                     fC[3]+mV[3], fC[4]+mV[4], fC[5]+mV[5] };    
    InvertCholetsky3(mS);
    //* Residual (measured - estimated)
    
    float32_v zeta[3] = { m[0]-fP[0], m[1]-fP[1], m[2]-fP[2] };    

    float32_v K[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        K[i][j] = 0;
        for(int k=0; k<3; k++)
          K[i][j] += fC[IJ(i,k)] * mS[IJ(k,j)];
      }
    
    //* CHt = CH' - D'
    float32_v mCHt0[7], mCHt1[7], mCHt2[7];

    mCHt0[0]=fC[ 0] ;       mCHt1[0]=fC[ 1] ;       mCHt2[0]=fC[ 3] ;
    mCHt0[1]=fC[ 1] ;       mCHt1[1]=fC[ 2] ;       mCHt2[1]=fC[ 4] ;
    mCHt0[2]=fC[ 3] ;       mCHt1[2]=fC[ 4] ;       mCHt2[2]=fC[ 5] ;
    mCHt0[3]=fC[ 6]+mV[ 6]; mCHt1[3]=fC[ 7]+mV[ 7]; mCHt2[3]=fC[ 8]+mV[ 8];
    mCHt0[4]=fC[10]+mV[10]; mCHt1[4]=fC[11]+mV[11]; mCHt2[4]=fC[12]+mV[12];
    mCHt0[5]=fC[15]+mV[15]; mCHt1[5]=fC[16]+mV[16]; mCHt2[5]=fC[17]+mV[17];
    mCHt0[6]=fC[21]+mV[21]; mCHt1[6]=fC[22]+mV[22]; mCHt2[6]=fC[23]+mV[23];
  
    //* Kalman gain K = mCH'*S
    
    float32_v k0[7], k1[7], k2[7];
    
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

    float32_v K2[3][3];
    for(int i=0; i<3; i++)
    {
      for(int j=0; j<3; j++)
        K2[i][j] = -K[j][i];
      K2[i][i] += 1;
    }

    float32_v A[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        A[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          A[i][j] += D[i][k] * K2[k][j];
        }
      }
    
    float32_v M[3][3];
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
    fChi2 += (mS[0]*zeta[0] + mS[1]*zeta[1] + mS[3]*zeta[2])*zeta[0]
      +      (mS[1]*zeta[0] + mS[2]*zeta[1] + mS[4]*zeta[2])*zeta[1]
      +      (mS[3]*zeta[0] + mS[4]*zeta[1] + mS[5]*zeta[2])*zeta[2];
}

void KFParticleSIMD::ReconstructMissingMass(const KFParticleSIMD &Daughter, KFParticleSIMD &MotherFiltered, KFParticleSIMD &cDaughterFiltered, float32_v neutralmasshypo )
{
  float32_v mothermasshypo, cdaughtermasshypo, massErr;
  GetMass(mothermasshypo, massErr);
  Daughter.GetMass(cdaughtermasshypo, massErr);
  
  AddDaughterId( Daughter.Id() );
  //* Energy considered as an independent variable, fitted independently from momentum, without any constraints on mass
  //* Add daughter 

  float32_v m[8], mV[36];

  float32_v D[3][3];
  GetMeasurement(Daughter, m, mV, D);
  
//     std::cout << "X: " << fC[0] << " " << mV[0] << " Y: " << fC[2] << " "<< mV[2] << " Z: "<< fC[5] << " "<< mV[5] << std::endl;
  
  float32_v mS[6]= { fC[0]+mV[0], 
                     fC[1]+mV[1], fC[2]+mV[2], 
                     fC[3]+mV[3], fC[4]+mV[4], fC[5]+mV[5] };    
    InvertCholetsky3(mS);
    //* Residual (measured - estimated)
    
    float32_v zeta[3] = { m[0]-fP[0], m[1]-fP[1], m[2]-fP[2] };    

    float32_v K[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        K[i][j] = 0;
        for(int k=0; k<3; k++)
          K[i][j] += fC[IJ(i,k)] * mS[IJ(k,j)];
      }
    //////////////////////
    //////////////////////
    float32_v mCHt0[6], mCHt1[6], mCHt2[6];
    
    mCHt0[0]=fC[ 0] ; mCHt1[0]=fC[ 1] ; mCHt2[0]=fC[ 3] ;
    mCHt0[1]=fC[ 1] ; mCHt1[1]=fC[ 2] ; mCHt2[1]=fC[ 4] ;
    mCHt0[2]=fC[ 3] ; mCHt1[2]=fC[ 4] ; mCHt2[2]=fC[ 5] ;
    mCHt0[3]=fC[ 6] ; mCHt1[3]=fC[ 7] ; mCHt2[3]=fC[ 8] ;
    mCHt0[4]=fC[10] ; mCHt1[4]=fC[11] ; mCHt2[4]=fC[12] ;
    mCHt0[5]=fC[15] ; mCHt1[5]=fC[16] ; mCHt2[5]=fC[17] ;
  
    //* Kalman gain K = mCH'*S
    
    float32_v kmf0[6], kmf1[6], kmf2[6];
    
    for(Int_t i=0;i<6;++i){
      kmf0[i] = mCHt0[i]*mS[0] + mCHt1[i]*mS[1] + mCHt2[i]*mS[3];
      kmf1[i] = mCHt0[i]*mS[1] + mCHt1[i]*mS[2] + mCHt2[i]*mS[4];
      kmf2[i] = mCHt0[i]*mS[3] + mCHt1[i]*mS[4] + mCHt2[i]*mS[5];
    }

    // last itearation -> update the particle

    //* VHt = VH'
    
    float32_v mVHt0[6], mVHt1[6], mVHt2[6];
    
    mVHt0[0]=mV[ 0] ; mVHt1[0]=mV[ 1] ; mVHt2[0]=mV[ 3] ;
    mVHt0[1]=mV[ 1] ; mVHt1[1]=mV[ 2] ; mVHt2[1]=mV[ 4] ;
    mVHt0[2]=mV[ 3] ; mVHt1[2]=mV[ 4] ; mVHt2[2]=mV[ 5] ;
    mVHt0[3]=mV[ 6] ; mVHt1[3]=mV[ 7] ; mVHt2[3]=mV[ 8] ;
    mVHt0[4]=mV[10] ; mVHt1[4]=mV[11] ; mVHt2[4]=mV[12] ;
    mVHt0[5]=mV[15] ; mVHt1[5]=mV[16] ; mVHt2[5]=mV[17] ;
  
    //* Kalman gain Km = mCH'*S
    
    float32_v kcdm0[6], kcdm1[6], kcdm2[6];
    
    for(Int_t i=0;i<6;++i){
      kcdm0[i] = mVHt0[i]*mS[0] + mVHt1[i]*mS[1] + mVHt2[i]*mS[3];
      kcdm1[i] = mVHt0[i]*mS[1] + mVHt1[i]*mS[2] + mVHt2[i]*mS[4];
      kcdm2[i] = mVHt0[i]*mS[3] + mVHt1[i]*mS[4] + mVHt2[i]*mS[5];
    }

    //mother filtered
    for(Int_t i=0;i<6;++i) 
      MotherFiltered.fP[i] = fP[i] + kmf0[i]*zeta[0] + kmf1[i]*zeta[1] + kmf2[i]*zeta[2];

    for(Int_t i=0, k=0;i<6;++i){
      for(Int_t j=0;j<=i;++j,++k){
        MotherFiltered.fC[k] = fC[k] - (kmf0[i]*mCHt0[j] + kmf1[i]*mCHt1[j] + kmf2[i]*mCHt2[j] );
      }
    }
    
    //cd filtered
    for(Int_t i=0;i<6;++i) 
      cDaughterFiltered.fP[i] = m[i] - kcdm0[i]*zeta[0] - kcdm1[i]*zeta[1] - kcdm2[i]*zeta[2];
    
    for(Int_t i=0, k=0;i<6;++i){
      for(Int_t j=0;j<=i;++j,++k){
        cDaughterFiltered.fC[k] = mV[k] - (kcdm0[i]*mVHt0[j] + kcdm1[i]*mVHt1[j] + kcdm2[i]*mVHt2[j] );
      }
    }
    
    ////////////////////
    //* CHt = CH' - D'
  //  float32_v mCHt0[7], mCHt1[7], mCHt2[7];

    mCHt0[0]=fC[ 0] ;       mCHt1[0]=fC[ 1] ;       mCHt2[0]=fC[ 3] ;
    mCHt0[1]=fC[ 1] ;       mCHt1[1]=fC[ 2] ;       mCHt2[1]=fC[ 4] ;
    mCHt0[2]=fC[ 3] ;       mCHt1[2]=fC[ 4] ;       mCHt2[2]=fC[ 5] ;
    mCHt0[3]=fC[ 6]+mV[ 6]; mCHt1[3]=fC[ 7]+mV[ 7]; mCHt2[3]=fC[ 8]+mV[ 8];
    mCHt0[4]=fC[10]+mV[10]; mCHt1[4]=fC[11]+mV[11]; mCHt2[4]=fC[12]+mV[12];
    mCHt0[5]=fC[15]+mV[15]; mCHt1[5]=fC[16]+mV[16]; mCHt2[5]=fC[17]+mV[17];
//     mCHt0[6]=fC[21]+mV[21]; mCHt1[6]=fC[22]+mV[22]; mCHt2[6]=fC[23]+mV[23];
  
    //* Kalman gain K = mCH'*S
    
    float32_v k0[6], k1[6], k2[6];
    
    for(Int_t i=0;i<6;++i){
      k0[i] = mCHt0[i]*mS[0] + mCHt1[i]*mS[1] + mCHt2[i]*mS[3];
      k1[i] = mCHt0[i]*mS[1] + mCHt1[i]*mS[2] + mCHt2[i]*mS[4];
      k2[i] = mCHt0[i]*mS[3] + mCHt1[i]*mS[4] + mCHt2[i]*mS[5];
    }

    //* Add the daughter momentum to the particle momentum
    
    fP[ 3] -= m[ 3];
    fP[ 4] -= m[ 4];
    fP[ 5] -= m[ 5];
//     fP[ 6] -= m[ 6];
  
    fC[ 9] += mV[ 9];
    fC[13] += mV[13];
    fC[14] += mV[14];
    fC[18] += mV[18];
    fC[19] += mV[19];
    fC[20] += mV[20];
//     fC[24] += mV[24];
//     fC[25] += mV[25];
//     fC[26] += mV[26];
//     fC[27] += mV[27];
    
 
   //* New estimation of the vertex position r += K*zeta
    //neutral
    for(Int_t i=0;i<6;++i) 
      fP[i] = fP[i] + k0[i]*zeta[0] + k1[i]*zeta[1] + k2[i]*zeta[2];
    
   //* New covariance matrix C -= K*(mCH')'
    //neutral
    for(Int_t i=0, k=0;i<6;++i){
      for(Int_t j=0;j<=i;++j,++k){
        fC[k] = fC[k] - (k0[i]*mCHt0[j] + k1[i]*mCHt1[j] + k2[i]*mCHt2[j] );
      }
    }

    float32_v K2[3][3];
    for(int i=0; i<3; i++)
    {
      for(int j=0; j<3; j++)
        K2[i][j] = -K[j][i];
      K2[i][i] += 1;
    }

    float32_v A[3][3];
    for(int i=0; i<3; i++)
      for(int j=0; j<3; j++)
      {
        A[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          A[i][j] += D[i][k] * K2[k][j];
        }
      }
    
    float32_v M[3][3];
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
    
    MotherFiltered.fC[0] += 2.f*M[0][0];
    MotherFiltered.fC[1] += M[0][1] + M[1][0];
    MotherFiltered.fC[2] += 2.f*M[1][1];
    MotherFiltered.fC[3] += M[0][2] + M[2][0];
    MotherFiltered.fC[4] += M[1][2] + M[2][1];
    MotherFiltered.fC[5] += 2.f*M[2][2];
    
    cDaughterFiltered.fC[0] += 2.f*M[0][0];
    cDaughterFiltered.fC[1] += M[0][1] + M[1][0];
    cDaughterFiltered.fC[2] += 2.f*M[1][1];
    cDaughterFiltered.fC[3] += M[0][2] + M[2][0];
    cDaughterFiltered.fC[4] += M[1][2] + M[2][1];
    cDaughterFiltered.fC[5] += 2.f*M[2][2];
  
    //* Calculate Chi^2 

    fNDF  += 2;
    fQ    -=  Daughter.GetQ();
    fChi2 += (mS[0]*zeta[0] + mS[1]*zeta[1] + mS[3]*zeta[2])*zeta[0]
      +      (mS[1]*zeta[0] + mS[2]*zeta[1] + mS[4]*zeta[2])*zeta[1]
      +      (mS[3]*zeta[0] + mS[4]*zeta[1] + mS[5]*zeta[2])*zeta[2];
    MotherFiltered.Chi2()=fChi2;
    cDaughterFiltered.Chi2()=fChi2;
    MotherFiltered.NDF()=fNDF;
    cDaughterFiltered.NDF()=fNDF;
    
    ////////energy calculations/////////
    float32_v neutralenergytemp, motherenergytemp, cdaughterenergytemp;
    
    motherenergytemp = sqrt(mothermasshypo*mothermasshypo+MotherFiltered.fP[3]*MotherFiltered.fP[3]+MotherFiltered.fP[4]*MotherFiltered.fP[4]+MotherFiltered.fP[5]*MotherFiltered.fP[5]);
    
    cdaughterenergytemp = sqrt(cdaughtermasshypo*cdaughtermasshypo+cDaughterFiltered.fP[3]*cDaughterFiltered.fP[3]+cDaughterFiltered.fP[4]*cDaughterFiltered.fP[4]+cDaughterFiltered.fP[5]*cDaughterFiltered.fP[5]);
    
    neutralenergytemp = sqrt(neutralmasshypo*neutralmasshypo + fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5]);
    
    fP[6] = motherenergytemp - cdaughterenergytemp; //neutral
    MotherFiltered.fP[6] = cdaughterenergytemp + neutralenergytemp;
    cDaughterFiltered.fP[6] = motherenergytemp - neutralenergytemp;
    
}

void KFParticleSIMD::SetProductionVertex( const KFParticleSIMD &Vtx )
{
  /** Adds a vertex as a point-like measurement to the current particle.
   ** The eights parameter of the state vector is filled with the decay
   ** length to the momentum ratio (s = l/p). The corresponding covariances
   ** are calculated as well. The parameters of the particle are stored
   ** at the position of the production vertex.
   ** \param[in] Vtx - the assumed producation vertex
   **/
  
  const float32_v *m = Vtx.fP, *mV = Vtx.fC;

  float32_v decayPoint[3] = {fP[0], fP[1], fP[2]};
  float32_v decayPointCov[6] = { fC[0], fC[1], fC[2], fC[3], fC[4], fC[5] };

  float32_v D[6][6];
  for(int iD1=0; iD1<6; iD1++)
    for(int iD2=0; iD2<6; iD2++)
      D[iD1][iD2] = 0.f;

  {
    float32_v dsdr[6] = {0.f, 0.f, 0.f, 0.f, 0.f, 0.f};
    float32_v dS = GetDStoPoint(Vtx.fP, dsdr);
      
    float32_v dsdp[6] = {-dsdr[0], -dsdr[1], -dsdr[2], 0.f, 0.f, 0.f };
      
    float32_v F[36], F1[36];
    for(int i2=0; i2<36; i2++)
    {
      F[i2]  = 0.f;
      F1[i2] = 0.f;
    }
    Transport( dS, dsdr, fP, fC, dsdp, F, F1 );
      
    float32_v CTmp[36];
    MultQSQt(F1, mV, CTmp, 6);
      
    for(int iC=0; iC<21; iC++)
      fC[iC] += CTmp[iC];
            
    for(int i=0; i<6; i++)
    {
      for(int j=0; j<3; j++)
      {
        D[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          D[i][j] +=  mV[IJ(j,k)] * F1[i*6+k];
        }
      }
    }
  }

  float32_v mS[6] = { fC[0] + mV[0],
                    fC[1] + mV[1], fC[2] + mV[2],
                    fC[3] + mV[3], fC[4] + mV[4], fC[5] + mV[5] };                 
  InvertCholetsky3(mS);
  
  float32_v res[3] = { m[0] - X(), m[1] - Y(), m[2] - Z() };
  
  float32_v K[3][6];  
  for(int i=0; i<3; i++)
    for(int j=0; j<3; j++)
    {
      K[i][j] = 0;
      for(int k=0; k<3; k++)
        K[i][j] += fC[IJ(i,k)] * mS[IJ(k,j)];
    }
  
  float32_v mCHt0[7], mCHt1[7], mCHt2[7];
  mCHt0[0]=fC[ 0];        mCHt1[0]=fC[ 1];        mCHt2[0]=fC[ 3];
  mCHt0[1]=fC[ 1];        mCHt1[1]=fC[ 2];        mCHt2[1]=fC[ 4];
  mCHt0[2]=fC[ 3];        mCHt1[2]=fC[ 4];        mCHt2[2]=fC[ 5];
  mCHt0[3]=fC[ 6];        mCHt1[3]=fC[ 7];        mCHt2[3]=fC[ 8];
  mCHt0[4]=fC[10];        mCHt1[4]=fC[11];        mCHt2[4]=fC[12];
  mCHt0[5]=fC[15];        mCHt1[5]=fC[16];        mCHt2[5]=fC[17];
  mCHt0[6]=fC[21];        mCHt1[6]=fC[22];        mCHt2[6]=fC[23];
  
  float32_v k0[7], k1[7], k2[7];
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

  float32_v K2[3][3];
  for(int i=0; i<3; i++)
  {
    for(int j=0; j<3; j++)
      K2[i][j] = -K[j][i];
    K2[i][i] += 1;
  }

  float32_v A[3][3];
  for(int i=0; i<3; i++)
    for(int j=0; j<3; j++)
    {
      A[i][j] = 0;
      for(int k=0; k<3; k++)
      {
        A[i][j] += D[k][i] * K2[k][j];
      }
    }
  
  float32_v M[3][3];
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

  {
    float32_v dsdr[6] = {0.f, 0.f, 0.f, 0.f, 0.f, 0.f};
    fP[7] = GetDStoPoint(decayPoint, dsdr);   
      
    float32_v dsdp[6] = {-dsdr[0], -dsdr[1], -dsdr[2], 0.f, 0.f, 0.f};

    float32_v F[36], F1[36];
    for(int i2=0; i2<36; i2++)
    {
      F[i2]  = 0.f;
      F1[i2] = 0.f;
    }
    float32_v tmpP[8], tmpC[36]; 
    Transport( fP[7], dsdr, tmpP, tmpC, dsdp, F, F1 );
            
    fC[35] = 0.f;
    for(int iDsDr=0; iDsDr<6; iDsDr++)
    {
      float32_v dsdrC = 0.f, dsdpV = 0.f;
        
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
  }
}

void KFParticleSIMD::SetMassConstraint( float32_v *mP, float32_v *mC, float32_v mJ[7][7], float32_v mass, mask32_v mask )
{
  /** Sets the exact nonlinear mass constraint on the state vector mP with the covariance matrix mC.
   ** \param[in,out] mP - the state vector to be modified
   ** \param[in,out] mC - the corresponding covariance matrix
   ** \param[in,out] mJ - the Jacobian between initial and modified parameters
   ** \param[in] mass - the mass to be set on the state vector mP
   ** \param[in] mask - mask defines entries of the SIMD vector, for which the constraint should be applied
   **/
  
  const float32_v energy2 = mP[6]*mP[6], p2 = mP[3]*mP[3]+mP[4]*mP[4]+mP[5]*mP[5], mass2 = mass*mass;
  
  const float32_v a = energy2 - p2 + 2.f*mass2;
  const float32_v b = -2.f*(energy2 + p2);
  const float32_v c = energy2 - p2 - mass2;

  float32_v lambda(0.f);
  lambda = select(abs(b) > float32_v(1.e-10f), -c/b, lambda) ;

  float32_v d = 4.f*energy2*p2 - mass2*(energy2-p2-2.f*mass2);
  mask32_v qMask = (d >= 0.f) && (abs(a) > (1.e-10f)) ;
  lambda = select(qMask, (energy2 + p2 - sqrt(d))/a, lambda);

  lambda = select(mP[6]<0.f, -1000000.f, lambda);

  Int_t iIter=0;
  for(iIter=0; iIter<100; iIter++)
  {
    float32_v lambda2 = lambda*lambda;
    float32_v lambda4 = lambda2*lambda2;

//    float32_v lambda0 = lambda;

    float32_v f  = -mass2 * lambda4 + a*lambda2 + b*lambda + c;
    float32_v df = -4.f*mass2 * lambda2*lambda + 2.f*a*lambda + b;
    lambda = select(abs(df) > float32_v(1.e-10f), lambda - f/df, lambda);
//    if(TMath::Abs(lambda0 - lambda) < 1.e-8) break;
  }

  const float32_v lpi = 1.f/(1.f + lambda);
  const float32_v lmi = 1.f/(1.f - lambda);
  const float32_v lp2i = lpi*lpi;
  const float32_v lm2i = lmi*lmi;

  float32_v lambda2 = lambda*lambda;

  float32_v dfl  = -4.f*mass2 * lambda2*lambda + 2.f*a*lambda + b;
  float32_v dfx[4] = {0.f,0.f,0.f,0.f};
  dfx[0] = -2.f*(1.f + lambda)*(1.f + lambda)*mP[3];
  dfx[1] = -2.f*(1.f + lambda)*(1.f + lambda)*mP[4];
  dfx[2] = -2.f*(1.f + lambda)*(1.f + lambda)*mP[5];
  dfx[3] = 2.f*(1.f - lambda)*(1.f - lambda)*mP[6];
  float32_v dlx[4] = {1.f,1.f,1.f,1.f};

  for(int i=0; i<4; i++)
    dlx[i] = select(abs(dfl) > float32_v(1.e-10f), -dfx[i] / dfl, dlx[i]);

  float32_v dxx[4] = {mP[3]*lm2i, mP[4]*lm2i, mP[5]*lm2i, -mP[6]*lp2i};

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

  float32_v mCJ[7][7];

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
      mC[IJ(i,j)] = select(mask, 0.f, mC[IJ(i,j)]);
      for(Int_t l=0; l<7; l++){
        mC[IJ(i,j)] = select(mask, mC[IJ(i,j)] + mJ[i][l]*mCJ[l][j], mC[IJ(i,j)]);
      }
    }
  }

  mP[3] = select(mask, mP[3] * lmi, mP[3]);
  mP[4] = select(mask, mP[4] * lmi, mP[4]);
  mP[5] = select(mask, mP[5] * lmi, mP[5]);
  mP[6] = select(mask, mP[6] * lpi, mP[6]);
}

void KFParticleSIMD::SetNonlinearMassConstraint( float32_v mass )
{
  /** Sets the exact nonlinear mass constraint on the current particle.
   ** \param[in] mass - the mass to be set on the particle
   **/
  
  const float32_v& px = fP[3];
  const float32_v& py = fP[4];
  const float32_v& pz = fP[5];
  const float32_v& energy  = fP[6];
  
  const float32_v residual = (energy*energy - px*px - py*py - pz*pz) - mass*mass;
  const float32_v dm2 = float32_v(4.f) * ( fC[9]*px*px + fC[14]*py*py + fC[20]*pz*pz + fC[27]*energy*energy +
                      float32_v(2.f) * ( (fC[13]*py + fC[18]*pz - fC[24]*energy)*px + (fC[19]*pz - fC[25]*energy)*py - fC[26]*pz*energy) );
  const float32_v dChi2 = residual*residual / dm2;
  fChi2 += dChi2;
  fNDF  += 1;
  
  float32_v mJ[7][7];

  mask32_v trueMask(KFP::SIMD::UninitializeTag{});
  trueMask.setTrue();
  SetMassConstraint( fP, fC, mJ, mass, trueMask );
  fMassHypo = mass;
  SumDaughterMass = mass;
}

void KFParticleSIMD::SetMassConstraint( float32_v Mass, float32_v SigmaMass )
{  
  /** Sets linearised mass constraint on the current particle. The constraint can be set with
   ** an uncertainty.
   ** \param[in] Mass - the mass to be set on the state vector mP
   ** \param[in] SigmaMass - uncertainty of the constraint
   **/
  
  fMassHypo = Mass;
  SumDaughterMass = Mass;

  float32_v m2 = Mass*Mass;            // measurement, weighted by Mass 
  float32_v s2 = m2*SigmaMass*SigmaMass; // sigma^2

  float32_v p2 = fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5]; 
  float32_v e0 = sqrt(m2+p2);

  float32_v mH[8];
  mH[0] = mH[1] = mH[2] = 0.f;
  mH[3] = -2.f*fP[3]; 
  mH[4] = -2.f*fP[4]; 
  mH[5] = -2.f*fP[5]; 
  mH[6] =  2.f*fP[6];//e0;
  mH[7] = 0.f; 

  float32_v zeta = e0*e0 - e0*fP[6];
  zeta = m2 - (fP[6]*fP[6]-p2);
  
  float32_v mCHt[8], s2_est=0.f;
  for( Int_t i=0; i<8; ++i ){
    mCHt[i] = 0.0f;
    for (Int_t j=0;j<8;++j) mCHt[i] += Cij(i,j)*mH[j];
    s2_est += mH[i]*mCHt[i];
  }
  
//TODO add protection
//  if( s2_est<1.e-20 ) return; // calculated mass error is already 0, 
                              // the particle can not be constrained on mass

  float32_v w2 = 1.f/( s2 + s2_est );
  fChi2 += zeta*zeta*w2;
  fNDF  += 1;
  for( Int_t i=0, ii=0; i<8; ++i ){
    float32_v ki = mCHt[i]*w2;
    fP[i]+= ki*zeta;
    for(Int_t j=0;j<=i;++j) fC[ii++] -= ki*mCHt[j];    
  }
}

void KFParticleSIMD::Construct( const KFParticleSIMD* vDaughters[], Int_t nDaughters, 
                                const KFParticleSIMD *Parent,  Float_t Mass )
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
    
    SumDaughterMass = float32_v(0.f);

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

void KFParticleSIMD::SubtractFromVertex(KFParticleSIMD &Vtx) const
{
  /** Subtract the current particle from vertex Vtx using the Kalman filter mathematics.
   ** \param[in] Vtx - vertex from which particle should be subtracted
   **/
  
  float32_v m[8];
  float32_v mCm[36];
  float32_v D[3][3];
  Vtx.GetMeasurement( *this, m, mCm, D );
  //* 
            
  float32_v mS[6] = { mCm[0] - Vtx.fC[0] + (D[0][0] + D[0][0]), 
                  mCm[1] - Vtx.fC[1] + (D[1][0] + D[0][1]), mCm[2] - Vtx.fC[2] + (D[1][1] + D[1][1]), 
                  mCm[3] - Vtx.fC[3] + (D[2][0] + D[0][2]), mCm[4] - Vtx.fC[4] + (D[1][2] + D[2][1]), mCm[5] - Vtx.fC[5] + (D[2][2] + D[2][2]) };
  InvertCholetsky3(mS);   
    
  //* Residual (measured - estimated)
    
  float32_v zeta[3] = { m[0]-Vtx.fP[0], m[1]-Vtx.fP[1], m[2]-Vtx.fP[2] };
        
  //* mCHt = mCH' - D'
    
  float32_v mCHt0[3], mCHt1[3], mCHt2[3];
    
  mCHt0[0]=Vtx.fC[ 0] ;      mCHt1[0]=Vtx.fC[ 1] ;      mCHt2[0]=Vtx.fC[ 3] ;
  mCHt0[1]=Vtx.fC[ 1] ;      mCHt1[1]=Vtx.fC[ 2] ;      mCHt2[1]=Vtx.fC[ 4] ;
  mCHt0[2]=Vtx.fC[ 3] ;      mCHt1[2]=Vtx.fC[ 4] ;      mCHt2[2]=Vtx.fC[ 5] ;
  
  //* Kalman gain K = mCH'*S
    
  float32_v k0[3], k1[3], k2[3];
    
  for(Int_t i=0;i<3;++i){
    k0[i] = mCHt0[i]*mS[0] + mCHt1[i]*mS[1] + mCHt2[i]*mS[3];
    k1[i] = mCHt0[i]*mS[1] + mCHt1[i]*mS[2] + mCHt2[i]*mS[4];
    k2[i] = mCHt0[i]*mS[3] + mCHt1[i]*mS[4] + mCHt2[i]*mS[5];
  }
    
  //* New estimation of the vertex position r += K*zeta
    
  float32_v dChi2 = ((mS[0]*zeta[0] + mS[1]*zeta[1] + mS[3]*zeta[2])*zeta[0]
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

void KFParticleSIMD::SubtractFromParticle(KFParticleSIMD &Vtx) const
{
  /** Subtract the current particle from another particle Vtx using the Kalman filter mathematics. 
   ** The function is depricated and is kept for compatibility reasons. Should be replaced with SubtractDaughter().
   ** \param[in] Vtx - particle from which the current particle should be subtracted
   **/
  
  float32_v m[8];
  float32_v mV[36];

  float32_v D[3][3];
  Vtx.GetMeasurement( *this, m, mV, D );

  float32_v mS[6] = { mV[0] - Vtx.fC[0] + (D[0][0] + D[0][0]), 
                  mV[1] - Vtx.fC[1] + (D[1][0] + D[0][1]), mV[2] - Vtx.fC[2] + (D[1][1] + D[1][1]), 
                  mV[3] - Vtx.fC[3] + (D[2][0] + D[0][2]), mV[4] - Vtx.fC[4] + (D[1][2] + D[2][1]), mV[5] - Vtx.fC[5] + (D[2][2] + D[2][2]) };
  InvertCholetsky3(mS);

  //* Residual (measured - estimated)

  float32_v zeta[3] = { m[0]-Vtx.fP[0], m[1]-Vtx.fP[1], m[2]-Vtx.fP[2] };    

  //* CHt = CH' - D'

  float32_v mCHt0[7], mCHt1[7], mCHt2[7];

  mCHt0[0]=mV[ 0] ;           mCHt1[0]=mV[ 1] ;           mCHt2[0]=mV[ 3] ;
  mCHt0[1]=mV[ 1] ;           mCHt1[1]=mV[ 2] ;           mCHt2[1]=mV[ 4] ;
  mCHt0[2]=mV[ 3] ;           mCHt1[2]=mV[ 4] ;           mCHt2[2]=mV[ 5] ;
  mCHt0[3]=Vtx.fC[ 6]-mV[ 6]; mCHt1[3]=Vtx.fC[ 7]-mV[ 7]; mCHt2[3]=Vtx.fC[ 8]-mV[ 8];
  mCHt0[4]=Vtx.fC[10]-mV[10]; mCHt1[4]=Vtx.fC[11]-mV[11]; mCHt2[4]=Vtx.fC[12]-mV[12];
  mCHt0[5]=Vtx.fC[15]-mV[15]; mCHt1[5]=Vtx.fC[16]-mV[16]; mCHt2[5]=Vtx.fC[17]-mV[17];
  mCHt0[6]=Vtx.fC[21]-mV[21]; mCHt1[6]=Vtx.fC[22]-mV[22]; mCHt2[6]=Vtx.fC[23]-mV[23];

  //* Kalman gain K = mCH'*S
    
  float32_v k0[7], k1[7], k2[7];
    
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

  float32_v ffC[28] = {-mV[ 0],
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
  Vtx.fChi2 -= ((mS[0]*zeta[0] + mS[1]*zeta[1] + mS[3]*zeta[2])*zeta[0]
             +  (mS[1]*zeta[0] + mS[2]*zeta[1] + mS[4]*zeta[2])*zeta[1]
             +  (mS[3]*zeta[0] + mS[4]*zeta[1] + mS[5]*zeta[2])*zeta[2]);
}

void KFParticleSIMD::GetDistanceToVertexLine( const KFParticleSIMD &Vertex, float32_v &l, float32_v &dl, mask32_v *isParticleFromVertex ) const 
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

  float32_v c[6] = {Vertex.fC[0]+fC[0], Vertex.fC[1]+fC[1], Vertex.fC[2]+fC[2],
               Vertex.fC[3]+fC[3], Vertex.fC[4]+fC[4], Vertex.fC[5]+fC[5]};

  float32_v dx = (Vertex.fP[0]-fP[0]);
  float32_v dy = (Vertex.fP[1]-fP[1]);
  float32_v dz = (Vertex.fP[2]-fP[2]);

  l = sqrt( dx*dx + dy*dy + dz*dz );
  dl = c[0]*dx*dx + c[2]*dy*dy + c[5]*dz*dz + 2*(c[1]*dx*dy + c[3]*dx*dz + c[4]*dy*dz);

  l = select(abs(l) < 1.e-8f, 1.e-8f, l);
  mask32_v ok = float32_v(0.f)<=dl;
  dl = select(ok, sqrt( dl )/l, 1.e8f);

  if(isParticleFromVertex)
  {
    *isParticleFromVertex = ok && ( l<float32_v(3.f*dl) );
    float32_v cosV = dx*fP[3] + dy*fP[4] + dz*fP[5];
//     float32_v dCos = dy*dy*fC[14] + dz*dz*fC[20] + dx*dx*fC[9] + 2*dz*fC[15]*fP[3] + c[0]* fP[3]*fP[3] + 
//             2*dz*fC[16]* fP[4] + 2 *c[1] *fP[3] *fP[4] + c[2] *fP[4]*fP[4] + 2 *dz *fC[17]* fP[5] + 
//             2*c[3] *fP[3]* fP[5] + 2 *c[4] *fP[4] *fP[5] + c[5]*fP[5] *fP[5] + 
//             2*dy *(dz *fC[19] + fC[10] *fP[3] + fC[11]* fP[4] + fC[12]* fP[5]) + 
//             2*dx *(dy *fC[13] + dz *fC[18] + fC[6]* fP[3] + fC[7]* fP[4] + fC[8]* fP[5]);
//     ok = float32_v(float32_v(0)<dCos);
//     dCos = float32_v(ok & ( dCos ));
//     dCos = sqrt(dCos);
    *isParticleFromVertex = (*isParticleFromVertex) || (!(*isParticleFromVertex) && (cosV<0.f) ) ;
  }
}

float32_v KFParticleSIMD::GetDStoPointLine( const float32_v xyz[3], float32_v dsdr[6] ) const 
{
  /** Returns dS = l/p parameter, where \n
   ** 1) l - signed distance to the DCA point with the input xyz point;\n
   ** 2) p - momentum of the particle; \n
   ** assuming the straigth line trajectory. Is used for particles with charge 0 or in case of zero magnetic field.
   ** Also calculates partial derivatives dsdr of the parameter dS over the state vector of the current particle.
   ** \param[in] xyz[3] - point where particle should be transported
   ** \param[out] dsdr[6] = ds/dr partial derivatives of the parameter dS over the state vector of the current particle
   **/
  
  float32_v p2 = fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5];  
  p2 = select( p2 < float32_v(1.e-4f), 1.f, p2);
  
  const float32_v& a = fP[3]*(xyz[0]-fP[0]) + fP[4]*(xyz[1]-fP[1]) + fP[5]*(xyz[2]-fP[2]);
  dsdr[0] = -fP[3]/p2;
  dsdr[1] = -fP[4]/p2;
  dsdr[2] = -fP[5]/p2;
  dsdr[3] = ((xyz[0]-fP[0])*p2 - 2.f* fP[3]*a)/(p2*p2);
  dsdr[4] = ((xyz[1]-fP[1])*p2 - 2.f* fP[4]*a)/(p2*p2);
  dsdr[5] = ((xyz[2]-fP[2])*p2 - 2.f* fP[5]*a)/(p2*p2);
  
  return a/p2;
}

float32_v KFParticleSIMD::GetDStoPointBz( float32_v B, const float32_v xyz[3], float32_v dsdr[6], const float32_v* param) const
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
  
  const float32_v& x  = param[0];
  const float32_v& y  = param[1];
  const float32_v& z  = param[2];
  const float32_v& px = param[3];
  const float32_v& py = param[4];
  const float32_v& pz = param[5];
  
  const float32_v kCLight = 0.000299792458f;
  float32_v bq = B * toFloat(fQ) * kCLight;
  float32_v pt2 = param[3]*param[3] + param[4]*param[4];
  float32_v p2 = pt2 + param[5]*param[5];  
  
  float32_v dx = xyz[0] - param[0];
  float32_v dy = xyz[1] - param[1]; 
  float32_v dz = xyz[2] - param[2]; 
  float32_v a = dx*param[3]+dy*param[4];
  float32_v dS(0.f);
  
  float32_v abq = bq*a;

  const float32_v LocalSmall = 1.e-8f;
  mask32_v mask = ( abs(bq)<LocalSmall );
  if( !( (!mask).isFull() ) )
  {
    dS = select(mask && mask32_v(p2>1.e-4f), (a + dz*pz)/p2, 0.f);
    
    dsdr[0] = select(mask && mask32_v(p2>1.e-4f), -px/p2, dsdr[0]);
    dsdr[1] = select(mask && mask32_v(p2>1.e-4f), -py/p2, dsdr[1]);
    dsdr[2] = select(mask && mask32_v(p2>1.e-4f), -pz/p2, dsdr[2]);
    dsdr[3] = select(mask && mask32_v(p2>1.e-4f), (dx*p2 - 2.f* px *(a + dz *pz))/(p2*p2), dsdr[3]);
    dsdr[4] = select(mask && mask32_v(p2>1.e-4f), (dy*p2 - 2.f* py *(a + dz *pz))/(p2*p2), dsdr[4]);
    dsdr[5] = select(mask && mask32_v(p2>1.e-4f), (dz*p2 - 2.f* pz *(a + dz *pz))/(p2*p2), dsdr[5]);
    
    if(mask.isFull())
      return dS;
  }
  
  dS = select(mask, dS, KFPMath::ATan2( abq, pt2 + bq*(dy*px -dx*py) )/bq);

  float32_v bs= bq*dS;

  float32_v s, c;
  KFPMath::sincos(bs, s, c);

  bq = select(abs(bq) < LocalSmall, LocalSmall, bq);
  float32_v bbq = bq*(dx*py - dy*px) - pt2;
  
  dsdr[0] = select(mask, dsdr[0], (px*bbq - py*abq)/(abq*abq + bbq*bbq));
  dsdr[1] = select(mask, dsdr[1], (px*abq + py*bbq)/(abq*abq + bbq*bbq));
  dsdr[2] = select(mask, dsdr[2], 0.f);
  dsdr[3] = select(mask, dsdr[3], -(dx*bbq + dy*abq + 2.f*px*a)/(abq*abq + bbq*bbq));
  dsdr[4] = select(mask, dsdr[4], (dx*abq - dy*bbq - 2.f*py*a)/(abq*abq + bbq*bbq));
  dsdr[5] = select(mask, dsdr[5], 0.f);
  
  const float32_v cCoeff = (bbq*c - abq*s) - pz*pz ;
  const float32_v sz = select(abs(cCoeff) > 1.e-8f, (dS*pz - dz)*pz / cCoeff, 0.f);
  
  float32_v dcdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  dcdr[0] = -bq*py*c - bbq*s*bq*dsdr[0] + px*bq*s - abq*c*bq*dsdr[0];
  dcdr[1] =  bq*px*c - bbq*s*bq*dsdr[1] + py*bq*s - abq*c*bq*dsdr[1];
  dcdr[3] = (-bq*dy-2*px)*c - bbq*s*bq*dsdr[3] - dx*bq*s - abq*c*bq*dsdr[3];
  dcdr[4] = ( bq*dx-2*py)*c - bbq*s*bq*dsdr[4] - dy*bq*s - abq*c*bq*dsdr[4];
  dcdr[5] = -2*pz;
  
  for(int iP=0; iP<6; iP++)
    dsdr[iP] = select(mask, dsdr[iP], dsdr[iP] + pz*pz/cCoeff*dsdr[iP] - sz/cCoeff*dcdr[iP]);

  dsdr[2] = select(mask, dsdr[2], dsdr[2] + pz/cCoeff);
  dsdr[5] = select(mask, dsdr[5], dsdr[5] + (2.f*pz*dS - dz)/cCoeff);
  
  dS = select(mask, dS, dS + sz);
  
  bs = bq*dS;
  KFPMath::sincos(bs, s, c);
  
  float32_v sB, cB;
  const float32_v kOvSqr6 = 1.f/sqrt(float32_v(6.f));

  sB = select(LocalSmall < abs(bs), s/bq , (1.f-bs*kOvSqr6)*(1.f+bs*kOvSqr6)*dS);
  cB = select(LocalSmall < abs(bs), (1.f-c)/bq, .5f*sB*bs);

  float32_v p[5];
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

  dS = select(mask, dS, dS + KFPMath::ATan2( abq, p2 + bq*(dy*p[3] -dx*p[4]) )/bq);
  
  return dS;
}

float32_v KFParticleSIMD::GetDStoPointBy( float32_v By, const float32_v xyz[3], float32_v dsdr[6] ) const
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
  
  const float32_v param[6] = { fP[0], -fP[2], fP[1], fP[3], -fP[5], fP[4] };
  const float32_v point[3] = { xyz[0], -xyz[2], xyz[1] };
  
  float32_v dsdrBz[6] = {0.f,0.f,0.f,0.f,0.f,0.f};

  const float32_v dS = GetDStoPointBz(By, point, dsdrBz, param);
  dsdr[0] =  dsdrBz[0];
  dsdr[1] =  dsdrBz[2];
  dsdr[2] = -dsdrBz[1];
  dsdr[3] =  dsdrBz[3];
  dsdr[4] =  dsdrBz[5];
  dsdr[5] = -dsdrBz[4];
  
  return dS;
}

float32_v KFParticleSIMD::GetDStoPointCBM( const float32_v xyz[3], float32_v dsdr[6] ) const
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
  
  float32_v dS(0.f);

  float32_v fld[3];
  GetFieldValue( fP, fld );
  dS = GetDStoPointBy( fld[1],xyz, dsdr );
  
  dS = select(abs(dS)>1.E3f, 0.f, dS);

  return dS;
}

float32_v KFParticleSIMD::GetDStoPointXYBz( float32_v B, const float32_v xyz[2] ) const
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

  //* Get dS to a certain space point for Bz field
  
  const float32_v& px = fP[3];
  const float32_v& py = fP[4];
  
  const float32_v kCLight = 0.000299792458f;
  float32_v bq = B * toFloat(fQ) * kCLight;
  float32_v pt2 = fP[3]*fP[3] + fP[4]*fP[4];
  
  float32_v dx = xyz[0] - fP[0];
  float32_v dy = xyz[1] - fP[1]; 
  float32_v a = dx*fP[3]+dy*fP[4];
  float32_v dS(0.f);
  
  float32_v abq = bq*a;

  const float32_v LocalSmall = 1.e-8f;
  mask32_v mask = ( abs(bq)<LocalSmall );
  dS = select(!mask, KFPMath::ATan2( abq, pt2 + bq*(dy*px -dx*py) )/bq, dS);
  
  return dS;
}

float32_v KFParticleSIMD::GetDStoPointZBz( const float32_v z0 ) const
{ 
  const float32_v dz = z0 - Z();
  float32_v dS = 0.f;
  dS = select(abs(Pz()) > 1.e-4f, dz/Pz(), dS);
  return dS;
}

void KFParticleSIMD::GetDStoCylinderBz( const float32_v B, const float32_v R, float32_v dS[2]) const
{ 
  /** Calculates 2 dS = l/p parameters from a particle to a cylinder with a radius R and center at (0,0) \n
   ** 1) l - signed distance to the cylinder; \n
   ** 2) p - momentum of the particle; \n
   ** \param[in] B - magnetic field Bz
   ** \param[in] R - radius of the cylinder
   ** \param[out] dS - l/p for two points of the closest approach with the cylinder
   **/

  //* Get dS to another particle for Bz field
  const float32_v kCLight = 0.000299792458f;

  //in XY plane
  //first root    
  const float32_v& bq1 = B*toFloat(fQ)*kCLight;
  const float32_v& bq2 = B*kCLight;

  const mask32_v& isStraight = abs(bq1) < float32_v(1.e-8f);
  
  const float32_v& px1 = fP[3];
  const float32_v& py1 = fP[4];

  const float32_v& px2 = R*bq2;
  const float32_v& py2 = 0.f;

  const float32_v& pt12 = px1*px1 + py1*py1;
  const float32_v& pt22 = px2*px2 + py2*py2;

  const float32_v& x01 = fP[0];
  const float32_v& y01 = fP[1];

  const float32_v& x02 = 0.f;
  const float32_v& y02 = R;

  const float32_v& dx0 = (x01 - x02);
  const float32_v& dy0 = (y01 - y02);
  const float32_v& dr02 = dx0*dx0 + dy0*dy0;
  const float32_v& drp1  = dx0*px1 + dy0*py1;
  const float32_v& dxyp1 = dx0*py1 - dy0*px1;
  const float32_v& dxyp2 = dx0*py2 - dy0*px2;
  const float32_v& p1p2 = px1*px2 + py1*py2;
  const float32_v& dp1p2 = px1*py2 - px2*py1;
  
  const float32_v& k11 = (bq2*drp1 - dp1p2);
  const float32_v& k21 = (bq1*(bq2*dxyp1 - p1p2) + bq2*pt12);
  
  const float32_v& kp = (dxyp1*bq2 - dxyp2*bq1 - p1p2);
  const float32_v& kd = dr02/2.f*bq1*bq2 + kp;
  const float32_v& c1 = -(bq1*kd + pt12*bq2);
  
  float32_v d1 = pt12*pt22 - kd*kd;
  d1 = select(d1 < 0.f, 0.f, d1);
  d1 = sqrt( d1 );
    
  // find two points of closest approach in XY plane
  if( ! ( (!isStraight).isEmpty() ) )
  {
    dS[0] = select(!isStraight, KFPMath::ATan2( (bq1*k11*c1 + k21*d1*bq1), (bq1*k11*d1*bq1 - k21*c1) )/bq1, dS[0]);
    dS[1] = select(!isStraight, KFPMath::ATan2( (bq1*k11*c1 - k21*d1*bq1), (-bq1*k11*d1*bq1 - k21*c1) )/bq1, dS[1]);
  }
  if( ! ( isStraight.isEmpty() ) )
  {
    dS[0] = select(isStraight && (pt12>0.f), (k11*c1 + k21*d1)/(- k21*c1), dS[0]);
    dS[1] = select(isStraight && (pt12>0.f), (k11*c1 - k21*d1)/(- k21*c1), dS[1]);
  }
}

void KFParticleSIMD::GetDStoParticleBz( float32_v B, const KFParticleSIMD &p, 
                                            float32_v dS[2], float32_v dsdr[4][6], const float32_v* param1, const float32_v* param2 ) const
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
  const float32_v kOvSqr6 = 1.f/sqrt(float32_v(6.f));
  const float32_v kCLight = 0.000299792458f;

  //in XY plane
  //first root    
  const float32_v& bq1 = B*toFloat(fQ)*kCLight;
  const float32_v& bq2 = B*toFloat(p.fQ)*kCLight;

  const mask32_v& isStraight1 = abs(bq1) < float32_v(1.e-8f);
  const mask32_v& isStraight2 = abs(bq2) < float32_v(1.e-8f);
  
  if( isStraight1.isFull() && isStraight2.isFull() )
  {
    GetDStoParticleLine(p, dS, dsdr);
    return;
  }
    
  const float32_v& px1 = param1[3];
  const float32_v& py1 = param1[4];
  const float32_v& pz1 = param1[5];

  const float32_v& px2 = param2[3];
  const float32_v& py2 = param2[4];
  const float32_v& pz2 = param2[5];

  const float32_v& pt12 = px1*px1 + py1*py1;
  const float32_v& pt22 = px2*px2 + py2*py2;

  const float32_v& x01 = param1[0];
  const float32_v& y01 = param1[1];
  const float32_v& z01 = param1[2];

  const float32_v& x02 = param2[0];
  const float32_v& y02 = param2[1];
  const float32_v& z02 = param2[2];

  float32_v dS1[2] = {0.f, 0.f}, dS2[2]={0.f, 0.f};
  
  const float32_v& dx0 = (x01 - x02);
  const float32_v& dy0 = (y01 - y02);
  const float32_v& dr02 = dx0*dx0 + dy0*dy0;
  const float32_v& drp1  = dx0*px1 + dy0*py1;
  const float32_v& dxyp1 = dx0*py1 - dy0*px1;
  const float32_v& drp2  = dx0*px2 + dy0*py2;
  const float32_v& dxyp2 = dx0*py2 - dy0*px2;
  const float32_v& p1p2 = px1*px2 + py1*py2;
  const float32_v& dp1p2 = px1*py2 - px2*py1;
  
  const float32_v& k11 = (bq2*drp1 - dp1p2);
  const float32_v& k21 = (bq1*(bq2*dxyp1 - p1p2) + bq2*pt12);
  const float32_v& k12 = ((bq1*drp2 - dp1p2));
  const float32_v& k22 = (bq2*(bq1*dxyp2 + p1p2) - bq1*pt22);
  
  const float32_v& kp = (dxyp1*bq2 - dxyp2*bq1 - p1p2);
  const float32_v& kd = dr02/2.f*bq1*bq2 + kp;
  const float32_v& c1 = -(bq1*kd + pt12*bq2);
  const float32_v& c2 = bq2*kd + pt22*bq1; 
  
  float32_v d1 = pt12*pt22 - kd*kd;
  d1 = select(d1 < 0.f, 0.f, d1);
  d1 = sqrt( d1 );
  float32_v d2 = pt12*pt22 - kd*kd;
  d2 = select(d2 < 0.f, 0.f, d2);
  d2 = sqrt( d2 );
    
  float32_v dS1dR1[2][6];
  float32_v dS2dR2[2][6];

  float32_v dS1dR2[2][6];
  float32_v dS2dR1[2][6];

  float32_v dk11dr1[6] = {bq2*px1, bq2*py1, 0.f, bq2*dx0 - py2, bq2*dy0 + px2, 0.f};
  float32_v dk11dr2[6] = {-bq2*px1, -bq2*py1, 0.f, py1, -px1, 0.f};
  float32_v dk12dr1[6] = {bq1*px2, bq1*py2, 0.f, -py2, px2, 0.f};
  float32_v dk12dr2[6] = {-bq1*px2, -bq1*py2, 0.f, bq1*dx0 + py1, bq1*dy0 - px1, 0.f};
  float32_v dk21dr1[6] = {bq1*bq2*py1, -bq1*bq2*px1, 0.f, 2.f*bq2*px1 + bq1*(-(bq2*dy0) - px2), 2.f*bq2*py1 + bq1*(bq2*dx0 - py2), 0.f};
  float32_v dk21dr2[6] = {-(bq1*bq2*py1), bq1*bq2*px1, 0.f, -(bq1*px1), -(bq1*py1), 0.f};
  float32_v dk22dr1[6] = {bq1*bq2*py2, -(bq1*bq2*px2), 0.f, bq2*px2, bq2*py2, 0.f};
  float32_v dk22dr2[6] = {-(bq1*bq2*py2), bq1*bq2*px2, 0.f, bq2*(-(bq1*dy0) + px1) - 2.f*bq1*px2, bq2*(bq1*dx0 + py1) - 2.f*bq1*py2, 0.f};
  
  float32_v dkddr1[6] = {bq1*bq2*dx0 + bq2*py1 - bq1*py2, bq1*bq2*dy0 - bq2*px1 + bq1*px2, 0.f, -bq2*dy0 - px2, bq2*dx0 - py2, 0.f};
  float32_v dkddr2[6] = {-bq1*bq2*dx0 - bq2*py1 + bq1*py2, -bq1*bq2*dy0 + bq2*px1 - bq1*px2, 0.f, bq1*dy0 - px1, -bq1*dx0 - py1, 0.f};
  
  float32_v dc1dr1[6] = {-(bq1*(bq1*bq2*dx0 + bq2*py1 - bq1*py2)), -(bq1*(bq1*bq2*dy0 - bq2*px1 + bq1*px2)), 0.f, -2.f*bq2*px1 - bq1*(-(bq2*dy0) - px2), -2.f*bq2*py1 - bq1*(bq2*dx0 - py2), 0.f};
  float32_v dc1dr2[6] = {-(bq1*(-(bq1*bq2*dx0) - bq2*py1 + bq1*py2)), -(bq1*(-(bq1*bq2*dy0) + bq2*px1 - bq1*px2)), 0.f, -(bq1*(bq1*dy0 - px1)), -(bq1*(-(bq1*dx0) - py1)), 0.f};
  
  float32_v dc2dr1[6] = {bq2*(bq1*bq2*dx0 + bq2*py1 - bq1*py2), bq2*(bq1*bq2*dy0 - bq2*px1 + bq1*px2), 0.f, bq2*(-(bq2*dy0) - px2), bq2*(bq2*dx0 - py2), 0.f};
  float32_v dc2dr2[6] = {bq2*(-(bq1*bq2*dx0) - bq2*py1 + bq1*py2), bq2*(-(bq1*bq2*dy0) + bq2*px1 - bq1*px2), 0.f, bq2*(bq1*dy0 - px1) + 2.f*bq1*px2, bq2*(-(bq1*dx0) - py1) + 2.f*bq1*py2, 0.f};
  
  float32_v dd1dr1[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  float32_v dd1dr2[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  {
    for(int i=0; i<6; i++)
    {
      dd1dr1[i] = select(d1>0.f, dd1dr1[i] - kd/d1*dkddr1[i], dd1dr1[i]);
      dd1dr2[i] = select(d1>0.f, dd1dr2[i] - kd/d1*dkddr2[i], dd1dr2[i]);
    }
    dd1dr1[3] = select(d1>0.f, dd1dr1[3] + px1/d1*pt22, dd1dr1[3]); dd1dr1[4] = select(d1>0.f, dd1dr1[4] + py1/d1*pt22, dd1dr1[4]);
    dd1dr2[3] = select(d1>0.f, dd1dr2[3] + px2/d1*pt12, dd1dr2[3]); dd1dr2[4] = select(d1>0.f, dd1dr2[4] + py2/d1*pt12, dd1dr2[4]);
  }

  // find two points of closest approach in XY plane
  if( ! ( (!isStraight1).isEmpty() ) )
  {
    dS1[0] = select(!isStraight1, KFPMath::ATan2( (bq1*k11*c1 + k21*d1*bq1), (bq1*k11*d1*bq1 - k21*c1) )/bq1, dS1[0]);
    dS1[1] = select(!isStraight1, KFPMath::ATan2( (bq1*k11*c1 - k21*d1*bq1), (-bq1*k11*d1*bq1 - k21*c1) )/bq1, dS1[1]);
    
    float32_v a = bq1*(k11*c1 + k21*d1);
    float32_v b = bq1*k11*d1*bq1 - k21*c1;
    for(int iP=0; iP<6; iP++)
    {
      const float32_v dadr1 = bq1*( dk11dr1[iP]*c1 + k11*dc1dr1[iP] + dk21dr1[iP]*d1 + k21*dd1dr1[iP] );
      const float32_v dadr2 = bq1*( dk11dr2[iP]*c1 + k11*dc1dr2[iP] + dk21dr2[iP]*d1 + k21*dd1dr2[iP] );
      const float32_v dbdr1 = bq1*bq1*( dk11dr1[iP]*d1 + k11*dd1dr1[iP] ) - ( dk21dr1[iP]*c1 + k21*dc1dr1[iP] );
      const float32_v dbdr2 = bq1*bq1*( dk11dr2[iP]*d1 + k11*dd1dr2[iP] ) - ( dk21dr2[iP]*c1 + k21*dc1dr2[iP] );
      
      dS1dR1[0][iP] = select(!isStraight1, 1/bq1 * 1/( b*b + a*a ) * ( dadr1*b - dbdr1*a ), dS1dR1[0][iP]);
      dS1dR2[0][iP] = select(!isStraight1, 1/bq1 * 1/( b*b + a*a ) * ( dadr2*b - dbdr2*a ), dS1dR2[0][iP]);
    }
    
    a = bq1*(k11*c1 - k21*d1);
    b = -bq1*k11*d1*bq1 - k21*c1;
    for(int iP=0; iP<6; iP++)
    {
      const float32_v dadr1 = bq1*( dk11dr1[iP]*c1 + k11*dc1dr1[iP] - (dk21dr1[iP]*d1 + k21*dd1dr1[iP]) );
      const float32_v dadr2 = bq1*( dk11dr2[iP]*c1 + k11*dc1dr2[iP] - (dk21dr2[iP]*d1 + k21*dd1dr2[iP]) );
      const float32_v dbdr1 = -bq1*bq1*( dk11dr1[iP]*d1 + k11*dd1dr1[iP] ) - ( dk21dr1[iP]*c1 + k21*dc1dr1[iP] );
      const float32_v dbdr2 = -bq1*bq1*( dk11dr2[iP]*d1 + k11*dd1dr2[iP] ) - ( dk21dr2[iP]*c1 + k21*dc1dr2[iP] );
      
      dS1dR1[1][iP] = select(!isStraight1, 1/bq1 * 1/( b*b + a*a ) * ( dadr1*b - dbdr1*a ), dS1dR1[1][iP]);
      dS1dR2[1][iP] = select(!isStraight1, 1/bq1 * 1/( b*b + a*a ) * ( dadr2*b - dbdr2*a ), dS1dR2[1][iP]);
    }
  }
  if( ! ( (!isStraight2).isEmpty() ) )
  {
    dS2[0] = select(!isStraight2, KFPMath::ATan2( (bq2*k12*c2 + k22*d2*bq2), (bq2*k12*d2*bq2 - k22*c2) )/bq2, dS2[0]);
    dS2[1] = select(!isStraight2, KFPMath::ATan2( (bq2*k12*c2 - k22*d2*bq2), (-bq2*k12*d2*bq2 - k22*c2) )/bq2, dS2[1]);
    
    float32_v a = bq2*(k12*c2 + k22*d2);
    float32_v b = bq2*k12*d2*bq2 - k22*c2;
    for(int iP=0; iP<6; iP++)
    {
      const float32_v dadr1 = bq2*( dk12dr1[iP]*c2 + k12*dc2dr1[iP] + dk22dr1[iP]*d1 + k22*dd1dr1[iP] );
      const float32_v dadr2 = bq2*( dk12dr2[iP]*c2 + k12*dc2dr2[iP] + dk22dr2[iP]*d1 + k22*dd1dr2[iP] );
      const float32_v dbdr1 = bq2*bq2*( dk12dr1[iP]*d1 + k12*dd1dr1[iP] ) - (dk22dr1[iP]*c2 + k22*dc2dr1[iP]);
      const float32_v dbdr2 = bq2*bq2*( dk12dr2[iP]*d1 + k12*dd1dr2[iP] ) - (dk22dr2[iP]*c2 + k22*dc2dr2[iP]);
      
      dS2dR1[0][iP] = select(!isStraight2, 1/bq2 * 1/( b*b + a*a ) * ( dadr1*b - dbdr1*a ), dS2dR1[0][iP]);
      dS2dR2[0][iP] = select(!isStraight2, 1/bq2 * 1/( b*b + a*a ) * ( dadr2*b - dbdr2*a ), dS2dR2[0][iP]);
    }
    
    a = bq2*(k12*c2 - k22*d2);
    b = -bq2*k12*d2*bq2 - k22*c2;
    for(int iP=0; iP<6; iP++)
    {
      const float32_v dadr1 = bq2*( dk12dr1[iP]*c2 + k12*dc2dr1[iP] - (dk22dr1[iP]*d1 + k22*dd1dr1[iP]) );
      const float32_v dadr2 = bq2*( dk12dr2[iP]*c2 + k12*dc2dr2[iP] - (dk22dr2[iP]*d1 + k22*dd1dr2[iP]) );
      const float32_v dbdr1 = -bq2*bq2*( dk12dr1[iP]*d1 + k12*dd1dr1[iP] ) - (dk22dr1[iP]*c2 + k22*dc2dr1[iP]);
      const float32_v dbdr2 = -bq2*bq2*( dk12dr2[iP]*d1 + k12*dd1dr2[iP] ) - (dk22dr2[iP]*c2 + k22*dc2dr2[iP]);
      
      dS2dR1[1][iP] = select(!isStraight2, 1/bq2 * 1/( b*b + a*a ) * ( dadr1*b - dbdr1*a ), dS2dR1[1][iP]);
      dS2dR2[1][iP] = select(!isStraight2, 1/bq2 * 1/( b*b + a*a ) * ( dadr2*b - dbdr2*a ), dS2dR2[1][iP]);
    }
  }
  if( ! ( isStraight1.isEmpty() ) )
  {
    dS1[0] = select(isStraight1 && (pt12>0.f), (k11*c1 + k21*d1)/(- k21*c1), dS1[0]);
    dS1[1] = select(isStraight1 && (pt12>0.f), (k11*c1 - k21*d1)/(- k21*c1), dS1[1]);
    
    float32_v a = k11*c1 + k21*d1;
    float32_v b = -k21*c1;
    
    for(int iP=0; iP<6; iP++)
    {
      const float32_v dadr1 = ( dk11dr1[iP]*c1 + k11*dc1dr1[iP] + dk21dr1[iP]*d1 + k21*dd1dr1[iP] );
      const float32_v dadr2 = ( dk11dr2[iP]*c1 + k11*dc1dr2[iP] + dk21dr2[iP]*d1 + k21*dd1dr2[iP] );
      const float32_v dbdr1 = -( dk21dr1[iP]*c1 + k21*dc1dr1[iP] );
      const float32_v dbdr2 = -( dk21dr2[iP]*c1 + k21*dc1dr2[iP] );
    
      dS1dR1[0][iP] = select(isStraight1 && (pt12>0.f), dadr1/b - dbdr1*a/(b*b), dS1dR1[0][iP]);
      dS1dR2[0][iP] = select(isStraight1 && (pt12>0.f), dadr2/b - dbdr2*a/(b*b), dS1dR2[0][iP]);
    }
    
    a = k11*c1 - k21*d1;
    for(int iP=0; iP<6; iP++)
    {
      const float32_v dadr1 = ( dk11dr1[iP]*c1 + k11*dc1dr1[iP] - dk21dr1[iP]*d1 - k21*dd1dr1[iP] );
      const float32_v dadr2 = ( dk11dr2[iP]*c1 + k11*dc1dr2[iP] - dk21dr2[iP]*d1 - k21*dd1dr2[iP] );
      const float32_v dbdr1 = -( dk21dr1[iP]*c1 + k21*dc1dr1[iP] );
      const float32_v dbdr2 = -( dk21dr2[iP]*c1 + k21*dc1dr2[iP] );
      
      dS1dR1[1][iP] = select(isStraight1 && (pt12>0.f), dadr1/b - dbdr1*a/(b*b), dS1dR1[1][iP]);
      dS1dR2[1][iP] = select(isStraight1 && (pt12>0.f), dadr2/b - dbdr2*a/(b*b), dS1dR2[1][iP]);
    }
  }
  if( ! ( isStraight2.isEmpty() ) )
  {
    dS2[0] = select(isStraight2 && (pt22>0.f), (k12*c2 + k22*d2)/(- k22*c2), dS2[0]);
    dS2[1] = select(isStraight2 && (pt22>0.f), (k12*c2 - k22*d2)/(- k22*c2), dS2[1]);
    
    float32_v a = k12*c2 + k22*d1;
    float32_v b = -k22*c2;
    
    for(int iP=0; iP<6; iP++)
    {
      const float32_v dadr1 = ( dk12dr1[iP]*c2 + k12*dc2dr1[iP] + dk22dr1[iP]*d1 + k22*dd1dr1[iP] );
      const float32_v dadr2 = ( dk12dr2[iP]*c2 + k12*dc2dr2[iP] + dk22dr2[iP]*d1 + k22*dd1dr2[iP] );
      const float32_v dbdr1 = -( dk22dr1[iP]*c2 + k22*dc2dr1[iP] );
      const float32_v dbdr2 = -( dk22dr2[iP]*c2 + k22*dc2dr2[iP] );
    
      dS2dR1[0][iP] = select(isStraight2 && (pt22>0.f), dadr1/b - dbdr1*a/(b*b), dS2dR1[0][iP]);
      dS2dR2[0][iP] = select(isStraight2 && (pt22>0.f), dadr2/b - dbdr2*a/(b*b), dS2dR2[0][iP]);
    }
    
    a = k12*c2 - k22*d1;
    for(int iP=0; iP<6; iP++)
    {
      const float32_v dadr1 = ( dk12dr1[iP]*c2 + k12*dc2dr1[iP] - dk22dr1[iP]*d1 - k22*dd1dr1[iP] );
      const float32_v dadr2 = ( dk12dr2[iP]*c2 + k12*dc2dr2[iP] - dk22dr2[iP]*d1 - k22*dd1dr2[iP] );
      const float32_v dbdr1 = -( dk22dr1[iP]*c2 + k22*dc2dr1[iP] );
      const float32_v dbdr2 = -( dk22dr2[iP]*c2 + k22*dc2dr2[iP] );
    
      dS2dR1[1][iP] = select(isStraight2 && (pt22>0.f), dadr1/b - dbdr1*a/(b*b), dS2dR1[1][iP]);
      dS2dR2[1][iP] = select(isStraight2 && (pt22>0.f), dadr2/b - dbdr2*a/(b*b), dS2dR2[1][iP]);
    }
  }
  
  //select a point which is close to the primary vertex (with the smallest r)
  
  float32_v dr2[2];
  for(int iP = 0; iP<2; iP++)
  {
    const float32_v& bs1 = bq1*dS1[iP];
    const float32_v& bs2 = bq2*dS2[iP];
    float32_v sss, ccc;
    KFPMath::sincos(bs1, sss, ccc);
    
    const mask32_v& bs1Big = abs(bs1) > 1.e-8f;
    const mask32_v& bs2Big = abs(bs2) > 1.e-8f;
    
    float32_v sB(0.f), cB(0.f);
    sB = select(bs1Big, sss/bq1, sB);
    sB = select(!bs1Big, (1.f-bs1*kOvSqr6)*(1.f+bs1*kOvSqr6)*dS1[iP], sB);
    cB = select(bs1Big, (1.f-ccc)/bq1, cB);
    cB = select(!bs1Big, 0.5f*sB*bs1, cB);
  
    const float32_v& x1 = param1[0] + sB*px1 + cB*py1;
    const float32_v& y1 = param1[1] - cB*px1 + sB*py1;
    const float32_v& z1 = param1[2] + dS1[iP]*param1[5];

    KFPMath::sincos(bs2, sss, ccc);

    sB = select(bs2Big, sss/bq2, sB);
    sB = select(!bs2Big, ((1.f-bs2*kOvSqr6)*(1.f+bs2*kOvSqr6)*dS2[iP]), sB);
    cB = select(bs2Big, (1.f-ccc)/bq2, cB);
    cB = select(!bs2Big, 0.5f*sB*bs2, cB);

    const float32_v& x2 = param2[0] + sB*px2 + cB*py2;
    const float32_v& y2 = param2[1] - cB*px2 + sB*py2;
    const float32_v& z2 = param2[2] + dS2[iP]*param2[5];

    float32_v dx = (x1-x2);
    float32_v dy = (y1-y2);
    float32_v dz = (z1-z2);
    
    dr2[iP] = dx*dx + dy*dy + dz*dz;
  }
  
  float32_v pointParam[2][8];
  float32_v pointCov[2][36];
  
  float32_v dsdrM1[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  for(int iP=0; iP<6; iP++)
    dsdrM1[iP] = (dS1dR1[0][iP] + dS1dR1[1][iP])/2.f;
  Transport((dS1[0] + dS1[1]) / 2.f, dsdrM1, pointParam[0], pointCov[0]);
  float32_v dsdrM2[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  for(int iP=0; iP<6; iP++)
    dsdrM2[iP] = (dS2dR2[0][iP] + dS2dR2[1][iP])/2.f;
  p.Transport((dS2[0] + dS2[1]) / 2.f, dsdrM2, pointParam[1], pointCov[1]);
  
  const float32_v drPoint[3] = { pointParam[0][0] - pointParam[1][0], pointParam[0][1] - pointParam[1][1], pointParam[0][2] - pointParam[1][2] } ;
  const float32_v covPoint[6] = { pointCov[0][0] + pointCov[1][0],
                                pointCov[0][1] + pointCov[1][1],
                                pointCov[0][2] + pointCov[1][2],
                                pointCov[0][3] + pointCov[1][3],
                                pointCov[0][4] + pointCov[1][4],
                                pointCov[0][5] + pointCov[1][5]  };
  float32_v dr2Points = drPoint[0]*drPoint[0] + drPoint[1]*drPoint[1] + drPoint[2]*drPoint[2];
  float32_v dr2PointCov = drPoint[0]*drPoint[0]*covPoint[0] + drPoint[1]*drPoint[1]*covPoint[2] + drPoint[2]*drPoint[2]*covPoint[5] +
                        2.f*( drPoint[0]*drPoint[1]*covPoint[1] + drPoint[0]*drPoint[2]*covPoint[3] + drPoint[1]*drPoint[2]*covPoint[4] );
  const mask32_v isMiddlePoint = (dr2Points*dr2Points < 25.f*dr2PointCov);// && (abs(fPDG)==int32_v(11)) && (abs(p.fPDG)==int32_v(11));
  const mask32_v isFirstRoot = (dr2[0] < dr2[1]) && (!isMiddlePoint);
  
 // if(!(isFirstRoot.isEmpty()))
  {
    dS[0] = select(isFirstRoot, dS1[0], dS[0]);
    dS[1] = select(isFirstRoot, dS2[0], dS[1]);
    
    for(int iP=0; iP<6; iP++)
    {
      dsdr[0][iP] = select(isFirstRoot, dS1dR1[0][iP], dsdr[0][iP]);
      dsdr[1][iP] = select(isFirstRoot, dS1dR2[0][iP], dsdr[1][iP]);
      dsdr[2][iP] = select(isFirstRoot, dS2dR1[0][iP], dsdr[2][iP]);
      dsdr[3][iP] = select(isFirstRoot, dS2dR2[0][iP], dsdr[3][iP]);
    }
  }
 // if( !( (!isFirstRoot).isEmpty() ) )
  {
    dS[0] = select(!isFirstRoot, dS1[1], dS[0]);
    dS[1] = select(!isFirstRoot, dS2[1], dS[1]);
    
    for(int iP=0; iP<6; iP++)
    {
      dsdr[0][iP] = select(!isFirstRoot, dS1dR1[1][iP], dsdr[0][iP]);
      dsdr[1][iP] = select(!isFirstRoot, dS1dR2[1][iP], dsdr[1][iP]);
      dsdr[2][iP] = select(!isFirstRoot, dS2dR1[1][iP], dsdr[2][iP]);
      dsdr[3][iP] = select(!isFirstRoot, dS2dR2[1][iP], dsdr[3][iP]);
    }    
  }
 // if(!(isMiddlePoint.isEmpty()))
  {
    dS[0] = select(isMiddlePoint, (dS1[0] + dS1[1]) / 2.f, dS[0]);
    dS[1] = select(isMiddlePoint, (dS2[0] + dS2[1]) / 2.f, dS[1]);
    
    for(int iP=0; iP<6; iP++)
    {
      dsdr[0][iP] = select(isMiddlePoint, (dS1dR1[1][iP] + dS1dR1[0][iP])/2.f, dsdr[0][iP]);
      dsdr[1][iP] = select(isMiddlePoint, (dS1dR2[1][iP] + dS1dR2[0][iP])/2.f, dsdr[1][iP]);
      dsdr[2][iP] = select(isMiddlePoint, (dS2dR1[1][iP] + dS2dR1[0][iP])/2.f, dsdr[2][iP]);
      dsdr[3][iP] = select(isMiddlePoint, (dS2dR2[1][iP] + dS2dR2[0][iP])/2.f, dsdr[3][iP]);
    }  
  }
        
  //find correct parts of helices
  int32_v n1(0);
  int32_v n2(0);
//  float32_v dzMin = abs( (z01-z02) + dS[0]*pz1 - dS[1]*pz2 );
  const float32_v pi2(6.283185307f);
  
//   //TODO optimise for loops for neutral particles
//   const float32_v& i1Float = -bq1/pi2*(z01/pz1+dS[0]);
//   for(int di1=-1; di1<=1; di1++)
//   {
//     int32_v i1(0);
//     i1(int_m(!isStraight1)) = int32_v(i1Float) + di1;
//     
//     const float32_v& i2Float = ( ((z01-z02) + (dS[0]+pi2*i1/bq1)*pz1)/pz2 - dS[1]) * bq2/pi2;
//     for(int di2 = -1; di2<=1; di2++)
//     {
//       int32_v i2(0);
//       i2(int_m(!isStraight2)) = int32_v(i2Float) + di2;
//       
//       const float32_v& z1 = z01 + (dS[0]+pi2*i1/bq1)*pz1;
//       const float32_v& z2 = z02 + (dS[1]+pi2*i2/bq2)*pz2;
//       const float32_v& dz = abs( z1-z2 );
//     
//       n1(int_m(dz < dzMin)) = i1;
//       n2(int_m(dz < dzMin)) = i2;
//       dzMin(dz < dzMin) = dz;
//     }
//   }
// 
//   dS[0](!isStraight1) += float32_v(n1)*pi2/bq1;
//   dS[1](!isStraight2) += float32_v(n2)*pi2/bq2;

  //add a correction on z-coordinate
#if 0
  {
    const float32_v& bs1 = bq1*dS[0];
    const float32_v& bs2 = bq2*dS[1];
    
    float32_v sss = KFPMath::Sin(bs1), ccc = KFPMath::Cos(bs1);
    const float32_v& xr1 = sss*px1 - ccc*py1;
    const float32_v& yr1 = ccc*px1 + sss*py1;

    float32_v sss1 = KFPMath::Sin(bs2), ccc1 = KFPMath::Cos(bs2);
    const float32_v& xr2 = sss1*px2 - ccc1*py2;
    const float32_v& yr2 = ccc1*px2 + sss1*py2;
    
    const float32_v& br = xr1*xr2 + yr1*yr2;
    const float32_v& dx0mod = dx0*bq1*bq2 + py1*bq2 - py2*bq1;
    const float32_v& dy0mod = dy0*bq1*bq2 - px1*bq2 + px2*bq1;
    const float32_v& ar1 = dx0mod*xr1 + dy0mod*yr1;
    const float32_v& ar2 = dx0mod*xr2 + dy0mod*yr2;
    const float32_v& cz = (z01 - z02) + dS[0]*pz1 - dS[1]*pz2;
    
    float32_v kz11 =  - ar1 + bq1*br + bq2*pz1*pz1;
    float32_v kz12 =  -bq2*(br+pz1*pz2);
    float32_v kz21 =   bq1*(br+pz1*pz2);
    float32_v kz22 =  - ar2 - bq2*br - bq1*pz2*pz2;
    
    kz11(isStraight2) = pz1*pz1 + (px1*px1+py1*py1)*ccc + bq1*( (px2*dS[1] - dx0)*xr1 + (py2*dS[1] - dy0)*yr1 );
    kz12(isStraight2) = -(br + pz1*pz2);
    kz21(isStraight1) = (br + pz1*pz2);
    kz22(isStraight1) = -pz2*pz2 - (px2*px2+py2*py2)*ccc1 - bq2*( (px1*dS[0] + dx0)*xr2 + (py1*dS[0] + dy0)*yr2 );

    const float32_v& delta = kz11*kz22 - kz12*kz21;
    float32_v sz1(0.f);
    float32_v sz2(0.f);
    
    {
      float32_v aaa1 = -cz*(pz1*bq2*kz22 - pz2*bq1*kz12);
      float32_v aaa2 = -cz*(pz2*bq1*kz11 - pz1*bq2*kz21);
      
      aaa1(isStraight2) = -cz*(pz1*kz22 - pz2*bq1*kz12);
      aaa2(isStraight2) = -cz*(pz2*bq1*kz11 - pz1*kz21);

      aaa1(isStraight1) = -cz*(pz1*bq2*kz22 - pz2*kz12);
      aaa2(isStraight1) = -cz*(pz2*kz11 - pz1*bq2*kz21);
      
      sz1( abs(delta) > 1.e-16f ) = aaa1 / delta;
      sz2( abs(delta) > 1.e-16f ) = aaa2 / delta;
      
      float32_v dkz11dr1[6] = {-(bq1*bq2*xr1), -(bq1*bq2*yr1), 0.f, 
                                   -ccc*dy0mod - dx0mod*sss + bq2*yr1 + bq1*(sss*xr2 + ccc*yr2), 
                                    ccc*dx0mod - dy0mod*sss - bq2*xr1 + bq1*(-ccc*xr2 + sss*yr2), 2.f*bq2*pz1};
              dkz11dr1[0](isStraight2) = -bq1*xr1;
              dkz11dr1[1](isStraight2) = -bq1*yr1;
              dkz11dr1[3](isStraight2) = 2.f*ccc*px1 + bq1*(sss*(dS[1]*px2 - x01 + x02) + ccc*(dS[1]*py2 - y01 + y02));
              dkz11dr1[4](isStraight2) = 2.f*ccc*py1 + bq1*(-(ccc*(dS[1]*px2 - x01 + x02)) + sss*(dS[1]*py2 - y01 + y02));
              dkz11dr1[5](isStraight2) = 2.f*pz1;
      float32_v dkz11dr2[6] = {bq1*bq2*xr1, bq1*bq2*yr1, 0.f, -bq1*yr1 + bq1*(sss1*xr1 + ccc1*yr1), bq1*xr1 + bq1*(-ccc1*xr1 + sss1*yr1), 0.f};
              dkz11dr2[0](isStraight2) = bq1*xr1;
              dkz11dr2[1](isStraight2) = bq1*yr1;
              dkz11dr2[3](isStraight2) = bq1*dS[1]*xr1;
              dkz11dr2[4](isStraight2) = bq1*dS[1]*yr1;

      float32_v dkz12dr1[6] = {0.f, 0.f, 0.f, -bq2*(sss*xr2 + ccc*yr2), -bq2*(-ccc*xr2 + sss*yr2), -bq2*pz2};
              dkz12dr1[3](isStraight2) = -(sss*xr2 + ccc*yr2);
              dkz12dr1[4](isStraight2) = -(-ccc*xr2 + sss*yr2);
              dkz12dr1[5](isStraight2) = -pz2;
      float32_v dkz12dr2[6] = {0.f, 0.f, 0.f, -bq2*(sss1*xr1 + ccc1*yr1), -bq2*(-ccc1*xr1 + sss1*yr1), -bq2*pz1};
              dkz12dr2[3](isStraight2) = -(sss1*xr1 + ccc1*yr1);
              dkz12dr2[4](isStraight2) = -(-ccc1*xr1 + sss1*yr1);
              dkz12dr2[5](isStraight2) = -pz1;
      float32_v dkz21dr1[6] = {0.f, 0.f, 0.f, bq1*(sss*xr2 + ccc*yr2), bq1*(-ccc*xr2 + sss*yr2), bq1*pz2};
              dkz21dr1[3](isStraight1) = yr2;
              dkz21dr1[4](isStraight1) = -xr2;
              dkz21dr1[5](isStraight1) =  pz2;
      float32_v dkz21dr2[6] = {0.f, 0.f, 0.f, bq1*(sss1*xr1 + ccc1*yr1), bq1*(-ccc1*xr1 + sss1*yr1), bq1*pz1};
              dkz21dr2[3](isStraight1) = (sss1*xr1 + ccc1*yr1);
              dkz21dr2[4](isStraight1) = (-ccc1*xr1 + sss1*yr1);
              dkz21dr2[5](isStraight1) = pz1;
      float32_v dkz22dr1[6] = {-bq1*bq2*xr2, -bq1*bq2*yr2, 0.f, bq2*yr2 - bq2*(sss*xr2 + ccc*yr2), -bq2*xr2 - bq2*(-ccc*xr2 + sss*yr2), 0.f};
              dkz22dr1[0](isStraight1) = -(bq2*xr2);
              dkz22dr1[1](isStraight1) = -(bq2*yr2);
              dkz22dr1[3](isStraight1) = -(bq2*dS[0]*xr2);
              dkz22dr1[4](isStraight1) = -(bq2*dS[0]*yr2);
      float32_v dkz22dr2[6] = {bq1*bq2*xr2, bq1*bq2*yr2, 0.f, 
                             -ccc1*dy0mod - dx0mod*sss1 - bq2*(sss1*xr1 + ccc1*yr1) - bq1*yr2, 
                              ccc1*dx0mod - dy0mod*sss1 + bq1*xr2 - bq2*(-ccc1*xr1 + sss1*yr1), -2.f*bq1*pz2};
              dkz22dr2[0](isStraight1) = bq2*xr2;
              dkz22dr2[1](isStraight1) = bq2*yr2;
              dkz22dr2[3](isStraight1) = -2.f*ccc1*px2 - bq2*(ccc1*(dy0 + dS[0]*py1) + (dx0 + dS[0]*px1)*sss1);
              dkz22dr2[4](isStraight1) = -2.f*ccc1*py2 - bq2*(-(ccc1*(dx0 + dS[0]*px1)) + (dy0 + dS[0]*py1)*sss1);
              dkz22dr2[5](isStraight1) = -2.f*pz2;
      
      float32_v dczdr1[6] = {0.f, 0.f, 1.f, 0.f, 0.f, dS[0]};
      float32_v dczdr2[6] = {0.f, 0.f, -1.f, 0.f, 0.f, -dS[1]};
      
      float32_v daaa1dr1[6];
      float32_v daaa1dr2[6];
      float32_v daaa2dr2[6];
      float32_v daaa2dr1[6];
      float32_v dDeltadr1[6];
      float32_v dDeltadr2[6];
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
      float32_v dkz11ds0 = bq1*(dy0mod*xr1 - dx0mod*yr1 +  bq1*(xr2*yr1 - xr1*yr2));
      dkz11ds0(isStraight2) = -(bq1*(px1*px1 + py1*py1)*sss) + bq1*(bq1*yr1*(dS[1]*px2 - x01 + x02) -bq1*xr1*(dS[1]*py2 - y01 + y02));
      float32_v dkz11ds1 = bq1*bq2*( xr1*yr2 - xr2*yr1 );
      dkz11ds1(isStraight2) = bq1*(px2*xr1 + py2*yr1);
      float32_v dkz12ds0 = bq2*bq1*( xr1*yr2 - xr2*yr1 );
      dkz12ds0(isStraight2) = bq1*( xr1*yr2 - xr2*yr1 );
      float32_v dkz12ds1 = bq2*bq2*( xr2*yr1 - xr1*yr2 );
      dkz12ds1(isStraight2) = 0;
      float32_v dkz21ds0 = bq1*bq1*( xr2*yr1 - xr1*yr2 );
      dkz21ds0(isStraight1) = 0.f;
      float32_v dkz21ds1 = bq1*bq2*( xr1*yr2 - xr2*yr1 );
      dkz21ds1(isStraight1) = px1*(bq2*ccc1*py2 - bq2*px2*sss1) - py1*(bq2*ccc1*px2 + bq2*py2*sss1);
      float32_v dkz22ds0 = bq1*bq2*( xr1*yr2 - xr2*yr1 );      
      dkz22ds0(isStraight1) = -bq2*(px1*xr2 + py1*yr2);
      float32_v dkz22ds1 = -bq2*( dy0mod*xr2 - dx0mod*yr2 - bq2*(xr2*yr1 - xr1*yr2) );
      dkz22ds1(isStraight1) = bq2*(px2*px2 + py2*py2)*sss1 - bq2*((dy0 + dS[0]*py1)*(bq2*ccc1*py2 - bq2*px2*sss1) + (dx0 + dS[0]*px1)*(bq2*ccc1*px2 + bq2*py2*sss1));
      const float32_v dczds0   = pz1;
      const float32_v dczds1   = -pz2;
      const float32_v da1ds0   = -( dczds0*(pz1*bq2*kz22 - pz2*bq1*kz12) + cz*(pz1*bq2*dkz22ds0 - pz2*bq1*dkz12ds0));
      const float32_v da1ds1   = -( dczds1*(pz1*bq2*kz22 - pz2*bq1*kz12) + cz*(pz1*bq2*dkz22ds1 - pz2*bq1*dkz12ds1));
      const float32_v da2ds0   = -( dczds0*(pz2*bq1*kz11 - pz1*bq2*kz21) + cz*(pz2*bq1*dkz11ds0 - pz1*bq2*dkz21ds0));
      const float32_v da2ds1   = -( dczds1*(pz2*bq1*kz11 - pz1*bq2*kz21) + cz*(pz2*bq1*dkz11ds1 - pz1*bq2*dkz21ds1));
      const float32_v dDeltads0 = kz11*dkz22ds0 + dkz11ds0*kz11 - kz12*dkz21ds0 - dkz12ds0*kz21;
      const float32_v dDeltads1 = kz11*dkz22ds1 + dkz11ds1*kz11 - kz12*dkz21ds1 - dkz12ds1*kz21;
      
      const float32_v dsz1ds0 = da1ds0/delta - aaa1*dDeltads0/(delta*delta);
      const float32_v dsz1ds1 = da1ds1/delta - aaa1*dDeltads1/(delta*delta);
      const float32_v dsz2ds0 = da2ds0/delta - aaa2*dDeltads0/(delta*delta);
      const float32_v dsz2ds1 = da2ds1/delta - aaa2*dDeltads1/(delta*delta);
      
      float32_v dszdr[4][6];
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
    const float32_v& bs1 = bq1*dS[0];
    const float32_v& bs2 = bq2*dS[1];
    float32_v sss, ccc;
    KFPMath::sincos(bs1, sss, ccc);
    
    const mask32_v& bs1Big = abs(bs1) > 1.e-8f;
    const mask32_v& bs2Big = abs(bs2) > 1.e-8f;
    
    float32_v sB(0.f), cB(0.f);
    sB = select(bs1Big, sss/bq1, sB);
    cB = select(bs1Big, (1.f-ccc)/bq1, cB);
    sB = select(!bs1Big, ((1.f-bs1*kOvSqr6)*(1.f+bs1*kOvSqr6)*dS[0]), sB);
    cB = select(!bs1Big, .5f*sB*bs1, cB);
  
    const float32_v& x1 = x01 + sB*px1 + cB*py1;
    const float32_v& y1 = y01 - cB*px1 + sB*py1;
    const float32_v& z1 = z01 + dS[0]*pz1;
    const float32_v& ppx1 =  ccc*px1 + sss*py1;
    const float32_v& ppy1 = -sss*px1 + ccc*py1;
    const float32_v& ppz1 = pz1;
    
    float32_v sss1, ccc1;
    KFPMath::sincos(bs2, sss1, ccc1);

    float32_v sB1(0.f), cB1(0.f);
    sB1 = select(bs2Big, sss1/bq2, sB1);
    cB1 = select(bs2Big, (1.f-ccc1)/bq2, cB1);
    sB1 = select(!bs2Big, ((1.f-bs2*kOvSqr6)*(1.f+bs2*kOvSqr6)*dS[1]), sB1);
    cB1 = select(!bs2Big, .5f*sB1*bs2, cB1);

    const float32_v& x2 = x02 + sB1*px2 + cB1*py2;
    const float32_v& y2 = y02 - cB1*px2 + sB1*py2;
    const float32_v& z2 = z02 + dS[1]*pz2;
    const float32_v& ppx2 =  ccc1*px2 + sss1*py2;
    const float32_v& ppy2 = -sss1*px2 + ccc1*py2;    
    const float32_v& ppz2 = pz2;

    const float32_v& p12  = ppx1*ppx1 + ppy1*ppy1 + ppz1*ppz1;
    const float32_v& p22  = ppx2*ppx2 + ppy2*ppy2 + ppz2*ppz2;
    const float32_v& lp1p2 = ppx1*ppx2 + ppy1*ppy2 + ppz1*ppz2;

    const float32_v& dx = (x2 - x1);
    const float32_v& dy = (y2 - y1);
    const float32_v& dz = (z2 - z1);
    
    const float32_v& ldrp1 = ppx1*dx + ppy1*dy + ppz1*dz;
    const float32_v& ldrp2 = ppx2*dx + ppy2*dy + ppz2*dz;

    float32_v detp =  lp1p2*lp1p2 - p12*p22;
    detp = select(abs(detp)<1.e-4f, 1.f, detp); //TODO correct!!!
    
    //dsdr calculation
    const float32_v a1 = ldrp2*lp1p2 - ldrp1*p22;
    const float32_v a2 = ldrp2*p12 - ldrp1*lp1p2;
    const float32_v lp1p2_ds0 = bq1*( ppx2*ppy1 - ppy2*ppx1);
    const float32_v lp1p2_ds1 = bq2*( ppx1*ppy2 - ppy1*ppx2);
    const float32_v ldrp1_ds0 = -p12 + bq1*(ppy1*dx - ppx1*dy);
    const float32_v ldrp1_ds1 =  lp1p2;
    const float32_v ldrp2_ds0 = -lp1p2;
    const float32_v ldrp2_ds1 =  p22 + bq2*(ppy2*dx - ppx2*dy);
    const float32_v detp_ds0 = 2.f*lp1p2*lp1p2_ds0;
    const float32_v detp_ds1 = 2.f*lp1p2*lp1p2_ds1;
    const float32_v a1_ds0 = ldrp2_ds0*lp1p2 + ldrp2*lp1p2_ds0 - ldrp1_ds0*p22;
    const float32_v a1_ds1 = ldrp2_ds1*lp1p2 + ldrp2*lp1p2_ds1 - ldrp1_ds1*p22;
    const float32_v a2_ds0 = ldrp2_ds0*p12 - ldrp1_ds0*lp1p2 - ldrp1*lp1p2_ds0;
    const float32_v a2_ds1 = ldrp2_ds1*p12 - ldrp1_ds1*lp1p2 - ldrp1*lp1p2_ds1;
    
    const float32_v dsl1ds0 = a1_ds0/detp - a1*detp_ds0/(detp*detp);
    const float32_v dsl1ds1 = a1_ds1/detp - a1*detp_ds1/(detp*detp);
    const float32_v dsl2ds0 = a2_ds0/detp - a2*detp_ds0/(detp*detp);
    const float32_v dsl2ds1 = a2_ds1/detp - a2*detp_ds1/(detp*detp);
    
    float32_v dsldr[4][6];
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
      
    const float32_v lp1p2_dr0[6] = {0.f, 0.f, 0.f, ccc*ppx2 - ppy2*sss, ccc*ppy2 + ppx2*sss, pz2};
    const float32_v lp1p2_dr1[6] = {0.f, 0.f, 0.f, ccc1*ppx1 - ppy1*sss1, ccc1*ppy1 + ppx1*sss1, pz1};
    const float32_v ldrp1_dr0[6] = {-ppx1, -ppy1, -pz1,  cB*ppy1 - ppx1*sB + ccc*dx - sss*dy, -cB*ppx1-ppy1*sB + sss*dx + ccc*dy, -dS[0]*pz1 + dz};
    const float32_v ldrp1_dr1[6] = { ppx1,  ppy1,  pz1, -cB1*ppy1 + ppx1*sB1, cB1*ppx1 + ppy1*sB1, dS[1]*pz1};
    const float32_v ldrp2_dr0[6] = {-ppx2, -ppy2, -pz2, cB*ppy2 - ppx2*sB, -cB*ppx2-ppy2*sB, -dS[0]*pz2};
    const float32_v ldrp2_dr1[6] = { ppx2, ppy2, pz2, -cB1*ppy2 + ppx2*sB1 + ccc1*dx- sss1*dy, cB1*ppx2 + ppy2*sB1 + sss1*dx + ccc1*dy, dz + dS[1]*pz2};
    const float32_v p12_dr0[6] = {0.f, 0.f, 0.f, 2.f*px1, 2.f*py1, 2.f*pz1};
    const float32_v p22_dr1[6] = {0.f, 0.f, 0.f, 2.f*px2, 2.f*py2, 2.f*pz2};
    float32_v a1_dr0[6], a1_dr1[6], a2_dr0[6], a2_dr1[6], detp_dr0[6], detp_dr1[6];
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

void KFParticleSIMD::GetDStoParticleBz( float32_v B, const KFParticleSIMD &p, 
                                            float32_v dS[2], const float32_v* param1, const float32_v* param2 ) const
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
  const float32_v kOvSqr6 = 1.f/sqrt(float32_v(6.f));
  const float32_v kCLight = 0.000299792458f;

  //in XY plane
  //first root    
  const float32_v& bq1 = B*toFloat(fQ)*kCLight;
  const float32_v& bq2 = B*toFloat(p.fQ)*kCLight;

  const mask32_v& isStraight1 = abs(bq1) < float32_v(1.e-8f);
  const mask32_v& isStraight2 = abs(bq2) < float32_v(1.e-8f);
  
  if( isStraight1.isFull() && isStraight2.isFull() )
  {
    GetDStoParticleLine(p, dS);
    return;
  }
    
  const float32_v& px1 = param1[3];
  const float32_v& py1 = param1[4];
  const float32_v& pz1 = param1[5];

  const float32_v& px2 = param2[3];
  const float32_v& py2 = param2[4];
  const float32_v& pz2 = param2[5];

  const float32_v& pt12 = px1*px1 + py1*py1;
  const float32_v& pt22 = px2*px2 + py2*py2;

  const float32_v& x01 = param1[0];
  const float32_v& y01 = param1[1];
  const float32_v& z01 = param1[2];

  const float32_v& x02 = param2[0];
  const float32_v& y02 = param2[1];
  const float32_v& z02 = param2[2];

  float32_v dS1[2] = {0.f, 0.f}, dS2[2]={0.f, 0.f};
  
  const float32_v& dx0 = (x01 - x02);
  const float32_v& dy0 = (y01 - y02);
  const float32_v& dr02 = dx0*dx0 + dy0*dy0;
  const float32_v& drp1  = dx0*px1 + dy0*py1;
  const float32_v& dxyp1 = dx0*py1 - dy0*px1;
  const float32_v& drp2  = dx0*px2 + dy0*py2;
  const float32_v& dxyp2 = dx0*py2 - dy0*px2;
  const float32_v& p1p2 = px1*px2 + py1*py2;
  const float32_v& dp1p2 = px1*py2 - px2*py1;
  
  const float32_v& k11 = (bq2*drp1 - dp1p2);
  const float32_v& k21 = (bq1*(bq2*dxyp1 - p1p2) + bq2*pt12);
  const float32_v& k12 = ((bq1*drp2 - dp1p2));
  const float32_v& k22 = (bq2*(bq1*dxyp2 + p1p2) - bq1*pt22);
  
  const float32_v& kp = (dxyp1*bq2 - dxyp2*bq1 - p1p2);
  const float32_v& kd = dr02/2.f*bq1*bq2 + kp;
  const float32_v& c1 = -(bq1*kd + pt12*bq2);
  const float32_v& c2 = bq2*kd + pt22*bq1; 
  
  float32_v d1 = pt12*pt22 - kd*kd;
  d1 = select(d1 < 0.f, 0.f, d1);
  d1 = sqrt( d1 );
  float32_v d2 = pt12*pt22 - kd*kd;
  d2 = select(d2 < 0.f, 0.f, d2);
  d2 = sqrt( d2 );

  // find two points of closest approach in XY plane
  if( ! ( (!isStraight1).isEmpty() ) )
  {
    dS1[0] = select(!isStraight1, KFPMath::ATan2( (bq1*k11*c1 + k21*d1*bq1), (bq1*k11*d1*bq1 - k21*c1) )/bq1, dS1[0]);
    dS1[1] = select(!isStraight1, KFPMath::ATan2( (bq1*k11*c1 - k21*d1*bq1), (-bq1*k11*d1*bq1 - k21*c1) )/bq1, dS1[1]);
  }
  if( ! ( (!isStraight2).isEmpty() ) )
  {
    dS2[0] = select(!isStraight2, KFPMath::ATan2( (bq2*k12*c2 + k22*d2*bq2), (bq2*k12*d2*bq2 - k22*c2) )/bq2, dS2[0]);
    dS2[1] = select(!isStraight2, KFPMath::ATan2( (bq2*k12*c2 - k22*d2*bq2), (-bq2*k12*d2*bq2 - k22*c2) )/bq2, dS2[1]);
  }
  if( ! ( isStraight1.isEmpty() ) )
  {
    dS1[0] = select(isStraight1 && (pt12>0.f), (k11*c1 + k21*d1)/(- k21*c1), dS1[0]);
    dS1[1] = select(isStraight1 && (pt12>0.f), (k11*c1 - k21*d1)/(- k21*c1), dS1[1]);
  }
  if( ! ( isStraight2.isEmpty() ) )
  {
    dS2[0] = select(isStraight2 && (pt22>0.f), (k12*c2 + k22*d2)/(- k22*c2), dS2[0]);
    dS2[1] = select(isStraight2 && (pt22>0.f), (k12*c2 - k22*d2)/(- k22*c2), dS2[1]);
  }
  
  //select a point which is close to the primary vertex (with the smallest r)
  
  float32_v dr2[2];
  for(int iP = 0; iP<2; iP++)
  {
    const float32_v& bs1 = bq1*dS1[iP];
    const float32_v& bs2 = bq2*dS2[iP];
    float32_v sss, ccc;
    KFPMath::sincos(bs1, sss, ccc);
    
    const mask32_v& bs1Big = abs(bs1) > 1.e-8f;
    const mask32_v& bs2Big = abs(bs2) > 1.e-8f;
    
    float32_v sB(0.f), cB(0.f);
    sB = select(bs1Big, sss/bq1, sB);
    sB = select(!bs1Big, ((1.f-bs1*kOvSqr6)*(1.f+bs1*kOvSqr6)*dS1[iP]), sB);
    cB = select(bs1Big, (1.f-ccc)/bq1, cB);
    cB = select(!bs1Big, .5f*sB*bs1, cB);
  
    const float32_v& x1 = param1[0] + sB*px1 + cB*py1;
    const float32_v& y1 = param1[1] - cB*px1 + sB*py1;
    const float32_v& z1 = param1[2] + dS1[iP]*param1[5];

    KFPMath::sincos(bs2, sss, ccc);

    sB = select(bs2Big, sss/bq2, sB);
    sB = select(!bs2Big, ((1.f-bs2*kOvSqr6)*(1.f+bs2*kOvSqr6)*dS2[iP]), sB);
    cB = select(bs2Big, (1.f-ccc)/bq2, cB);
    cB = select(!bs2Big, .5f*sB*bs2, cB);

    const float32_v& x2 = param2[0] + sB*px2 + cB*py2;
    const float32_v& y2 = param2[1] - cB*px2 + sB*py2;
    const float32_v& z2 = param2[2] + dS2[iP]*param2[5];

    float32_v dx = (x1-x2);
    float32_v dy = (y1-y2);
    float32_v dz = (z1-z2);
    
    dr2[iP] = dx*dx + dy*dy + dz*dz;
  }
  
  
  const mask32_v isFirstRoot = (dr2[0] < dr2[1]);
  
 // if(!(isFirstRoot.isEmpty()))
  {
    dS[0] = select(isFirstRoot, dS1[0], dS[0]);
    dS[1] = select(isFirstRoot, dS2[0], dS[1]);
  }
 // if( !( (!isFirstRoot).isEmpty() ) )
  {
    dS[0] = select(!isFirstRoot, dS1[1], dS[0]);
    dS[1] = select(!isFirstRoot, dS2[1], dS[1]);
  }
        
  //find correct parts of helices
  int32_v n1(0);
  int32_v n2(0);
//  float32_v dzMin = abs( (z01-z02) + dS[0]*pz1 - dS[1]*pz2 );
  const float32_v pi2(6.283185307f);
  
//   //TODO optimise for loops for neutral particles
//   const float32_v& i1Float = -bq1/pi2*(z01/pz1+dS[0]);
//   for(int di1=-1; di1<=1; di1++)
//   {
//     int32_v i1(0);
//     i1(int_m(!isStraight1)) = int32_v(i1Float) + di1;
//     
//     const float32_v& i2Float = ( ((z01-z02) + (dS[0]+pi2*i1/bq1)*pz1)/pz2 - dS[1]) * bq2/pi2;
//     for(int di2 = -1; di2<=1; di2++)
//     {
//       int32_v i2(0);
//       i2(int_m(!isStraight2)) = int32_v(i2Float) + di2;
//       
//       const float32_v& z1 = z01 + (dS[0]+pi2*i1/bq1)*pz1;
//       const float32_v& z2 = z02 + (dS[1]+pi2*i2/bq2)*pz2;
//       const float32_v& dz = abs( z1-z2 );
//     
//       n1(int_m(dz < dzMin)) = i1;
//       n2(int_m(dz < dzMin)) = i2;
//       dzMin(dz < dzMin) = dz;
//     }
//   }
// 
//   dS[0](!isStraight1) += float32_v(n1)*pi2/bq1;
//   dS[1](!isStraight2) += float32_v(n2)*pi2/bq2;

  //Line correction
  {
    const float32_v& bs1 = bq1*dS[0];
    const float32_v& bs2 = bq2*dS[1];
    float32_v sss, ccc;
    KFPMath::sincos(bs1, sss, ccc);
    
    const mask32_v& bs1Big = abs(bs1) > 1.e-8f;
    const mask32_v& bs2Big = abs(bs2) > 1.e-8f;
    
    float32_v sB(0.f), cB(0.f);
    sB = select(bs1Big, sss/bq1, sB);
    cB = select(bs1Big, (1.f-ccc)/bq1, cB);
    sB = select(!bs1Big, ((1.f-bs1*kOvSqr6)*(1.f+bs1*kOvSqr6)*dS[0]), sB);
    cB = select(!bs1Big, .5f*sB*bs1, cB);
  
    const float32_v& x1 = x01 + sB*px1 + cB*py1;
    const float32_v& y1 = y01 - cB*px1 + sB*py1;
    const float32_v& z1 = z01 + dS[0]*pz1;
    const float32_v& ppx1 =  ccc*px1 + sss*py1;
    const float32_v& ppy1 = -sss*px1 + ccc*py1;
    const float32_v& ppz1 = pz1;
    
    float32_v sss1, ccc1;
    KFPMath::sincos(bs2, sss1, ccc1);

    float32_v sB1(0.f), cB1(0.f);
    sB1 = select(bs2Big, sss1/bq2, sB1);
    cB1 = select(bs2Big, (1.f-ccc1)/bq2, cB1);
    sB1 = select(!bs2Big, ((1.f-bs2*kOvSqr6)*(1.f+bs2*kOvSqr6)*dS[1]), sB1);
    cB1 = select(!bs2Big, .5f*sB1*bs2, cB1);

    const float32_v& x2 = x02 + sB1*px2 + cB1*py2;
    const float32_v& y2 = y02 - cB1*px2 + sB1*py2;
    const float32_v& z2 = z02 + dS[1]*pz2;
    const float32_v& ppx2 =  ccc1*px2 + sss1*py2;
    const float32_v& ppy2 = -sss1*px2 + ccc1*py2;    
    const float32_v& ppz2 = pz2;

    const float32_v& p12  = ppx1*ppx1 + ppy1*ppy1 + ppz1*ppz1;
    const float32_v& p22  = ppx2*ppx2 + ppy2*ppy2 + ppz2*ppz2;
    const float32_v& lp1p2 = ppx1*ppx2 + ppy1*ppy2 + ppz1*ppz2;

    const float32_v& dx = (x2 - x1);
    const float32_v& dy = (y2 - y1);
    const float32_v& dz = (z2 - z1);
    
    const float32_v& ldrp1 = ppx1*dx + ppy1*dy + ppz1*dz;
    const float32_v& ldrp2 = ppx2*dx + ppy2*dy + ppz2*dz;

    float32_v detp =  lp1p2*lp1p2 - p12*p22;
    detp = select(abs(detp)<1.e-4f, 1.f, detp); //TODO correct!!!
    
    dS[0] += (ldrp2*lp1p2 - ldrp1*p22) /detp;
    dS[1] += (ldrp2*p12 - ldrp1*lp1p2)/detp;    
  }
}

void KFParticleSIMD::GetDStoParticleBy( float32_v B, const KFParticleSIMD &p, float32_v dS[2], float32_v dsdr[4][6] ) const
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
  
  const float32_v param1[6] = { fP[0], -fP[2], fP[1], fP[3], -fP[5], fP[4] };
  const float32_v param2[6] = { p.fP[0], -p.fP[2], p.fP[1], p.fP[3], -p.fP[5], p.fP[4] };
  
  float32_v dsdrBz[4][6];
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

void KFParticleSIMD::GetDStoParticleBy( float32_v B, const KFParticleSIMD &p, float32_v dS[2] ) const
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
  
  const float32_v param1[6] = { fP[0], -fP[2], fP[1], fP[3], -fP[5], fP[4] };
  const float32_v param2[6] = { p.fP[0], -p.fP[2], p.fP[1], p.fP[3], -p.fP[5], p.fP[4] };
  
  GetDStoParticleBz(B, p, dS, param1, param2);
}

void KFParticleSIMD::GetDStoParticleB( float32_v B[3], const KFParticleSIMD &p, float32_v dS[2], float32_v dsdr[4][6] ) const
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
  
  const float32_v& Bx = B[0];
  const float32_v& By = B[1];
  const float32_v& Bz = B[2];
  
  const float32_v& Bxz = sqrt(Bx*Bx + Bz*Bz);
  const float32_v& Br = sqrt(Bx*Bx + By*By + Bz*Bz);
    
  float32_v cosA = 1.f;
  float32_v sinA = 0.f;

  cosA = select( abs(Bxz) > 1.e-8f, Bz/Bxz, cosA);
  sinA = select( abs(Bxz) > 1.e-8f, Bx/Bxz, sinA);
  
  const float32_v& sinP = By/Br;
  const float32_v& cosP = Bxz/Br;

  
  const float32_v param1[6] = { cosA*fP[0] - sinA*fP[2], 
                             -sinA*sinP*fP[0] + cosP*fP[1] - cosA*sinP*fP[2], 
                              cosP*sinA*fP[0] + sinP*fP[1] + cosA*cosP*fP[2],
                              cosA*fP[3] - sinA*fP[5], 
                             -sinA*sinP*fP[3] + cosP*fP[4] - cosA*sinP*fP[5], 
                              cosP*sinA*fP[3] + sinP*fP[4] + cosA*cosP*fP[5]};
  const float32_v param2[6] = { cosA*p.fP[0] - sinA*p.fP[2], 
                             -sinA*sinP*p.fP[0] + cosP*p.fP[1] - cosA*sinP*p.fP[2], 
                              cosP*sinA*p.fP[0] + sinP*p.fP[1] + cosA*cosP*p.fP[2],
                              cosA*p.fP[3] - sinA*p.fP[5], 
                             -sinA*sinP*p.fP[3] + cosP*p.fP[4] - cosA*sinP*p.fP[5], 
                              cosP*sinA*p.fP[3] + sinP*p.fP[4] + cosA*cosP*p.fP[5]};

  float32_v dsdrBz[4][6];
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

void KFParticleSIMD::GetDStoParticleB( float32_v B[3], const KFParticleSIMD &p, float32_v dS[2] ) const
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
  
  const float32_v& Bx = B[0];
  const float32_v& By = B[1];
  const float32_v& Bz = B[2];
  
  const float32_v& Bxz = sqrt(Bx*Bx + Bz*Bz);
  const float32_v& Br = sqrt(Bx*Bx + By*By + Bz*Bz);
    
  float32_v cosA = 1.f;
  float32_v sinA = 0.f;

  cosA = select( abs(Bxz) > 1.e-8f, Bz/Bxz, cosA);
  sinA = select( abs(Bxz) > 1.e-8f, Bx/Bxz, sinA);
  
  const float32_v& sinP = By/Br;
  const float32_v& cosP = Bxz/Br;

  
  const float32_v param1[6] = { cosA*fP[0] - sinA*fP[2], 
                             -sinA*sinP*fP[0] + cosP*fP[1] - cosA*sinP*fP[2], 
                              cosP*sinA*fP[0] + sinP*fP[1] + cosA*cosP*fP[2],
                              cosA*fP[3] - sinA*fP[5], 
                             -sinA*sinP*fP[3] + cosP*fP[4] - cosA*sinP*fP[5], 
                              cosP*sinA*fP[3] + sinP*fP[4] + cosA*cosP*fP[5]};
  const float32_v param2[6] = { cosA*p.fP[0] - sinA*p.fP[2], 
                             -sinA*sinP*p.fP[0] + cosP*p.fP[1] - cosA*sinP*p.fP[2], 
                              cosP*sinA*p.fP[0] + sinP*p.fP[1] + cosA*cosP*p.fP[2],
                              cosA*p.fP[3] - sinA*p.fP[5], 
                             -sinA*sinP*p.fP[3] + cosP*p.fP[4] - cosA*sinP*p.fP[5], 
                              cosP*sinA*p.fP[3] + sinP*p.fP[4] + cosA*cosP*p.fP[5]};

  GetDStoParticleBz(Br, p, dS, param1, param2);
}

void KFParticleSIMD::GetDStoParticleLine( const KFParticleSIMD &p, float32_v dS[2], float32_v dsdr[4][6] ) const
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
  
  float32_v p12 = fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5];
  float32_v p22 = p.fP[3]*p.fP[3] + p.fP[4]*p.fP[4] + p.fP[5]*p.fP[5];
  float32_v p1p2 = fP[3]*p.fP[3] + fP[4]*p.fP[4] + fP[5]*p.fP[5];

  float32_v drp1 = fP[3]*(p.fP[0]-fP[0]) + fP[4]*(p.fP[1]-fP[1]) + fP[5]*(p.fP[2]-fP[2]);
  float32_v drp2 = p.fP[3]*(p.fP[0]-fP[0]) + p.fP[4]*(p.fP[1]-fP[1]) + p.fP[5]*(p.fP[2]-fP[2]);

  float32_v detp =  p1p2*p1p2 - p12*p22;
  detp = select( abs(detp)<float32_v(1.e-4f), 1.f, detp); //TODO correct!!!

  dS[0] = (drp2*p1p2 - drp1*p22) /detp;
  dS[1] = (drp2*p12  - drp1*p1p2)/detp;
  
  const float32_v x01 = fP[0];
  const float32_v y01 = fP[1];
  const float32_v z01 = fP[2];
  const float32_v px1 = fP[3];
  const float32_v py1 = fP[4];
  const float32_v pz1 = fP[5];

  const float32_v x02 = p.fP[0];
  const float32_v y02 = p.fP[1];
  const float32_v z02 = p.fP[2];
  const float32_v px2 = p.fP[3];
  const float32_v py2 = p.fP[4];
  const float32_v pz2 = p.fP[5];
  
  const float32_v drp1_dr1[6]  = {-px1, -py1, -pz1, -x01 + x02, -y01 + y02, -z01 + z02};
  const float32_v drp1_dr2[6]  = {px1, py1, pz1, 0.f, 0.f, 0.f};
  const float32_v drp2_dr1[6]  = {-px2, -py2, -pz2, 0.f, 0.f, 0.f};
  const float32_v drp2_dr2[6]  = {px2, py2, pz2, -x01 + x02, -y01 + y02, -z01 + z02};
  const float32_v dp1p2_dr1[6] = {0.f, 0.f, 0.f, px2, py2, pz2};
  const float32_v dp1p2_dr2[6] = {0.f, 0.f, 0.f, px1, py1, pz1};
  const float32_v dp12_dr1[6]  = {0.f, 0.f, 0.f, 2.f*px1, 2.f*py1, 2.f*pz1};
  const float32_v dp12_dr2[6]  = {0.f, 0.f, 0.f, 0.f, 0.f, 0.f};
  const float32_v dp22_dr1[6]  = {0.f, 0.f, 0.f, 0.f, 0.f, 0.f};
  const float32_v dp22_dr2[6]  = {0.f, 0.f, 0.f, 2.f*px2, 2.f*py2, 2.f*pz2};
  const float32_v ddetp_dr1[6] = {0.f, 0.f, 0.f, -2.f*p22*px1 + 2.f*p1p2*px2, -2.f*p22*py1 + 2.f*p1p2*py2, -2.f*p22*pz1 + 2.f*p1p2*pz2};
  const float32_v ddetp_dr2[6] = {0.f, 0.f, 0.f, 2.f*p1p2*px1 - 2.f*p12*px2,   2.f*p1p2*py1 - 2.f*p12*py2, 2.f*p1p2*pz1 - 2.f*p12*pz2};
  
  
  float32_v da1_dr1[6], da1_dr2[6], da2_dr1[6], da2_dr2[6];
  
  const float32_v a1 = drp2*p1p2 - drp1*p22;
  const float32_v a2 = drp2*p12  - drp1*p1p2;
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

void KFParticleSIMD::GetDStoParticleLine( const KFParticleSIMD &p, float32_v dS[2] ) const
{
  /** Calculates dS = l/p parameters for two particles, where \n
   ** 1) l - signed distance to the DCA point with the other particle;\n
   ** 2) p - momentum of the particle; \n
   ** under the assumption of the straight line trajectory. Is used for particles with charge 0 or in case of zero magnetic field.
   ** dS[0] is the transport parameter for the current particle, dS[1] - for the particle "p".
   ** \param[in] p - second particle
   ** \param[out] dS[2] - transport parameters dS for the current particle (dS[0]) and the second particle "p" (dS[1])
   **/
  
  float32_v p12 = fP[3]*fP[3] + fP[4]*fP[4] + fP[5]*fP[5];
  float32_v p22 = p.fP[3]*p.fP[3] + p.fP[4]*p.fP[4] + p.fP[5]*p.fP[5];
  float32_v p1p2 = fP[3]*p.fP[3] + fP[4]*p.fP[4] + fP[5]*p.fP[5];

  float32_v drp1 = fP[3]*(p.fP[0]-fP[0]) + fP[4]*(p.fP[1]-fP[1]) + fP[5]*(p.fP[2]-fP[2]);
  float32_v drp2 = p.fP[3]*(p.fP[0]-fP[0]) + p.fP[4]*(p.fP[1]-fP[1]) + p.fP[5]*(p.fP[2]-fP[2]);

  float32_v detp =  p1p2*p1p2 - p12*p22;
  detp = select( abs(detp)<float32_v(1.e-4f), 1.f, detp); //TODO correct!!!

  dS[0] = (drp2*p1p2 - drp1*p22) /detp;
  dS[1] = (drp2*p12  - drp1*p1p2)/detp;
}

void KFParticleSIMD::GetDStoParticleCBM( const KFParticleSIMD &p, float32_v dS[2], float32_v dsdr[4][6] ) const
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
  
  float32_v fld[3];
  GetFieldValue( fP, fld );

  const float32_v& bq1 = fld[1]*toFloat(fQ);
  const float32_v& bq2 = fld[1]*toFloat(p.fQ);
  const mask32_v& isStraight1 = abs(bq1) < float32_v(1.e-8f);
  const mask32_v& isStraight2 = abs(bq2) < float32_v(1.e-8f);
  
  if( isStraight1.isFull() && isStraight2.isFull() )
    GetDStoParticleLine(p, dS, dsdr);
  else
    GetDStoParticleBy(fld[1], p, dS, dsdr);
}

void KFParticleSIMD::GetDStoParticleCBM( const KFParticleSIMD &p, float32_v dS[2] ) const
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
  
  float32_v fld[3];
  GetFieldValue( fP, fld );

  const float32_v& bq1 = fld[1]*toFloat(fQ);
  const float32_v& bq2 = fld[1]*toFloat(p.fQ);
  const mask32_v& isStraight1 = abs(bq1) < float32_v(1.e-8f);
  const mask32_v& isStraight2 = abs(bq2) < float32_v(1.e-8f);
  
  if( isStraight1.isFull() && isStraight2.isFull() )
    GetDStoParticleLine(p, dS);
  else
    GetDStoParticleBy(fld[1], p, dS);
}

float32_v KFParticleSIMD::GetDistanceFromVertex( const KFParticleSIMD &Vtx ) const
{
  /** Returns the DCA distance from vertex in the KFParticle format in 3D.
   ** \param[in] Vtx - the vertex in the KFParticle format
   **/
  
  return GetDistanceFromVertex( Vtx.fP );
}

float32_v KFParticleSIMD::GetDistanceFromVertex( const float32_v vtx[] ) const
{
  /** Returns the DCA distance from vertex in 3D.
   ** \param[in] vtx[3] - the vertex coordinates {X, Y, Z}
   **/
  
  float32_v mP[8], mC[36];  
  float32_v dsdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  const float32_v dS = GetDStoPoint(vtx, dsdr);
  Transport( dS, dsdr, mP, mC );
  float32_v d[3]={ vtx[0]-mP[0], vtx[1]-mP[1], vtx[2]-mP[2]};
  return sqrt( d[0]*d[0]+d[1]*d[1]+d[2]*d[2] );
}

float32_v KFParticleSIMD::GetDistanceFromParticle( const KFParticleSIMD &p ) const
{ 
  /** Returns the DCA distance from another particle p.
   ** \param[in] p - the second particle
   **/
  
  float32_v dS[2];
  GetDStoParticleFast( p, dS );   
  float32_v mP[8], mP1[8];
  TransportFast( dS[0], mP ); 
  p.TransportFast( dS[1], mP1 ); 
  float32_v dx = mP[0]-mP1[0]; 
  float32_v dy = mP[1]-mP1[1]; 
  float32_v dz = mP[2]-mP1[2]; 
  return sqrt(dx*dx+dy*dy+dz*dz);  
}

float32_v KFParticleSIMD::GetDeviationFromVertex( const KFParticleSIMD &Vtx ) const
{
  /** Returns Chi2 deviation of the current particle from the vertex in the KFParticle format in 3D.
   ** \param[in] Vtx - the vertex in KFPartcile format
   **/
  
  return GetDeviationFromVertex( Vtx.fP, Vtx.fC );
}

float32_v KFParticleSIMD::GetDeviationFromVertex( const float32_v v[], const float32_v Cv[] ) const
{
  /** Returns Chi2 deviation of the current particle from the vertex v with the covariance matrix Cv in 3D.
   ** \param[in] v[3] - coordinates of the vertex {X, Y, Z}
   ** \param[in] Cv[6] - covariance matrix of the vertex {Cxx, Cxy, Cyy, Cxz, Czy, Czz}
   **/

  float32_v mP[8];
  float32_v mC[36];
  float32_v dsdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  const float32_v dS = GetDStoPoint(v, dsdr);
  float32_v dsdp[6] = {-dsdr[0], -dsdr[1], -dsdr[2], 0.f, 0.f, 0.f};
  float32_v F1[36];
  Transport( dS, dsdr, mP, mC, dsdp, nullptr, F1, false );  

  if(Cv)
  {
    float32_v VFT[3][3];
    VFT[0][0] = Cv[0]*F1[ 0] + Cv[1]*F1[ 1] + Cv[3]*F1[ 2];
    VFT[0][1] = Cv[0]*F1[ 6] + Cv[1]*F1[ 7] + Cv[3]*F1[ 8];
    VFT[0][2] = Cv[0]*F1[12] + Cv[1]*F1[13] + Cv[3]*F1[14];

    VFT[1][0] = Cv[1]*F1[ 0] + Cv[2]*F1[ 1] + Cv[4]*F1[ 2];
    VFT[1][1] = Cv[1]*F1[ 6] + Cv[2]*F1[ 7] + Cv[4]*F1[ 8];
    VFT[1][2] = Cv[1]*F1[12] + Cv[2]*F1[13] + Cv[4]*F1[14];

    VFT[2][0] = Cv[3]*F1[ 0] + Cv[4]*F1[ 1] + Cv[5]*F1[ 2];
    VFT[2][1] = Cv[3]*F1[ 6] + Cv[4]*F1[ 7] + Cv[5]*F1[ 8];
    VFT[2][2] = Cv[3]*F1[12] + Cv[4]*F1[13] + Cv[5]*F1[14];

    float32_v FVFT[6];
    FVFT[0] = F1[ 0]*VFT[0][0] + F1[ 1]*VFT[1][0] + F1[ 2]*VFT[2][0];
    FVFT[1] = F1[ 6]*VFT[0][0] + F1[ 7]*VFT[1][0] + F1[ 8]*VFT[2][0];
    FVFT[2] = F1[ 6]*VFT[0][1] + F1[ 7]*VFT[1][1] + F1[ 8]*VFT[2][1];
    FVFT[3] = F1[12]*VFT[0][0] + F1[13]*VFT[1][0] + F1[14]*VFT[2][0];
    FVFT[4] = F1[12]*VFT[0][1] + F1[13]*VFT[1][1] + F1[14]*VFT[2][1];
    FVFT[5] = F1[12]*VFT[0][2] + F1[13]*VFT[1][2] + F1[14]*VFT[2][2];
    
    mC[0] += FVFT[0] + Cv[0];
    mC[1] += FVFT[1] + Cv[1];
    mC[2] += FVFT[2] + Cv[2];
    mC[3] += FVFT[3] + Cv[3];
    mC[4] += FVFT[4] + Cv[4];
    mC[5] += FVFT[5] + Cv[5];
  }
  
  InvertCholetsky3(mC);
  
  float32_v d[3]={ v[0]-mP[0], v[1]-mP[1], v[2]-mP[2]};

  return ( ( mC[0]*d[0] + mC[1]*d[1] + mC[3]*d[2])*d[0]
           +(mC[1]*d[0] + mC[2]*d[1] + mC[4]*d[2])*d[1]
           +(mC[3]*d[0] + mC[4]*d[1] + mC[5]*d[2])*d[2] );
}

float32_v KFParticleSIMD::GetDeviationFromParticle( const KFParticleSIMD &p ) const
{ 
  /** Returns Chi2 deviation of the current particle from another particle in 3D.
   ** \param[in] p - the second particle
   **/
  
  float32_v ds[2] = {0.f,0.f};
  float32_v dsdr[4][6];
  float32_v F1[36], F2[36], F3[36], F4[36];
  for(int i1=0; i1<36; i1++)
  {
    F1[i1] = 0;
    F2[i1] = 0;
    F3[i1] = 0;
    F4[i1] = 0;
  }
  GetDStoParticle( p, ds, dsdr );
  
  float32_v V0Tmp[36] ;
  float32_v V1Tmp[36] ;

  
  float32_v mP1[8], mC1[36];
  float32_v mP2[8], mC2[36]; 
  
    Transport(ds[0], dsdr[0], mP1, mC1, dsdr[1], F1, F2);
  p.Transport(ds[1], dsdr[3], mP2, mC2, dsdr[2], F4, F3);
  
  MultQSQt(F2, p.fC, V0Tmp, 6);
  MultQSQt(F3,   fC, V1Tmp, 6);
      
  for(int iC=0; iC<6; iC++)
    mC1[iC] += V0Tmp[iC] + mC2[iC] + V1Tmp[iC];

  float32_v d[3]={ mP2[0]-mP1[0], mP2[1]-mP1[1], mP2[2]-mP1[2]};
  
  return ( ( mC1[0]*d[0] + mC1[1]*d[1] + mC1[3]*d[2])*d[0]
           +(mC1[1]*d[0] + mC1[2]*d[1] + mC1[4]*d[2])*d[1]
           +(mC1[3]*d[0] + mC1[4]*d[1] + mC1[5]*d[2])*d[2] );
}

mask32_v KFParticleSIMD::GetDistanceFromVertexXY( const float32_v vtx[], const float32_v Cv[], float32_v &val, float32_v &err ) const
{
  /** Calculates the DCA distance from a vertex together with the error in the XY plane.
   ** Returns "true" if calculation is failed, "false" if both value and the error are well defined.
   **
   ** \param[in] vtx[2] - { X, Y } coordinates of the vertex
   ** \param[in] Cv[3] - lower-triangular part of the covariance matrix of the vertex
   ** \param[out] val - the distance in the XY plane to the vertex
   ** \param[out] err - the error of the calculated distance, takes into account errors of the particle and vertex
   **/
  
  float32_v mP[8];
  float32_v mC[36];
  
  float32_v dsdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  const float32_v dS = GetDStoPoint(vtx, dsdr);
  Transport( dS, dsdr, mP, mC );  

  float32_v dx = mP[0] - vtx[0];
  float32_v dy = mP[1] - vtx[1];
  float32_v px = mP[3];
  float32_v py = mP[4];
  float32_v pt = sqrt(px*px + py*py);
  float32_v ex(0.f), ey(0.f);
  mask32_v mask = ( pt < float32_v(1.e-4) );

  pt = select(mask, 1.f, pt);
  ex = select(!mask, px/pt, ex);
  ey = select(!mask, py/pt, ey);
  val = select(mask,  float32_v(1.e4), val);
  val = select(!mask, dy*ex - dx*ey, val);

  float32_v h0 = -ey;
  float32_v h1 = ex;
  float32_v h3 = (dy*ey + dx*ex)*ey/pt;
  float32_v h4 = -(dy*ey + dx*ex)*ex/pt;
  
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

mask32_v KFParticleSIMD::GetDistanceFromVertexXY( const float32_v vtx[], float32_v &val, float32_v &err ) const
{
  /** Calculates the DCA distance from a vertex together with the error in the XY plane.
   ** Returns "true" if calculation is failed, "false" if both value and the error are well defined.
   ** \param[in] vtx[2] - { X, Y } coordinates of the vertex
   ** \param[out] val - the distance in the XY plane to the vertex
   ** \param[out] err - the error of the calculated distance, takes into account errors of the particle only
   **/
  return GetDistanceFromVertexXY( vtx, 0, val, err );
}

mask32_v KFParticleSIMD::GetDistanceFromVertexXY( const KFParticleSIMD &Vtx, float32_v &val, float32_v &err ) const 
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
mask32_v KFParticleSIMD::GetDistanceFromVertexXY( const KFPVertex &Vtx, float32_v &val, float32_v &err ) const 
{
  /** Calculates the DCA distance from a vertex in the KFPVertex format together with the error in the XY plane.
   ** Returns "true" if calculation is failed, "false" if both value and the error are well defined.
   ** \param[in] Vtx - the vertex in the KFPVertex format
   ** \param[out] val - the distance in the XY plane to the vertex
   ** \param[out] err - the error of the calculated distance, takes into account errors of the particle and vertex
   **/
  
  return GetDistanceFromVertexXY( KFParticleSIMD(Vtx), val, err );
}
#endif

float32_v KFParticleSIMD::GetDistanceFromVertexXY( const float32_v vtx[] ) const
{
  /** Returns the DCA distance from a vertex in the XY plane.
   ** \param[in] vtx[2] - { X, Y } coordinates of the vertex
   **/

  float32_v val, err;
  GetDistanceFromVertexXY( vtx, 0, val, err );
  return val;
}

float32_v KFParticleSIMD::GetDistanceFromVertexXY( const KFParticleSIMD &Vtx ) const 
{
  /** Returns the DCA distance from a vertex in the KFParticle format in the XY plane.
   ** \param[in] Vtx - the vertex in the KFParticle format
   **/
  
  return GetDistanceFromVertexXY( Vtx.fP );
}

#ifdef HomogeneousField
float32_v KFParticleSIMD::GetDistanceFromVertexXY( const KFPVertex &Vtx ) const 
{
  /** Returns the DCA distance from a vertex in the KFParticle format in the XY plane.
   ** \param[in] Vtx - the vertex in the KFPVertex format
   **/
  
  return GetDistanceFromVertexXY( KFParticleSIMD(Vtx).fP );
}
#endif

float32_v KFParticleSIMD::GetDistanceFromParticleXY( const KFParticleSIMD &p ) const 
{
  /** Returns the DCA distance between the current and the second particles in the XY plane.
   ** \param[in] p - the second particle
   **/
  
  float32_v dS[2];
  float32_v dsdr[4][6];
  GetDStoParticle( p, dS, dsdr );   
  float32_v mP[8], mC[36], mP1[8], mC1[36];
  Transport( dS[0], dsdr[0], mP, mC ); 
  p.Transport( dS[1], dsdr[3], mP1, mC1 ); 
  float32_v dx = mP[0]-mP1[0]; 
  float32_v dy = mP[1]-mP1[1]; 
  return sqrt(dx*dx+dy*dy);
}

float32_v KFParticleSIMD::GetDeviationFromParticleXY( const KFParticleSIMD &p ) const 
{
  /** Returns sqrt(Chi2/ndf) deviation from other particle in the XY plane.
   ** \param[in] p - the second particle
   **/
  
  float32_v ds[2] = {0.f,0.f};
  float32_v dsdr[4][6];
  float32_v F1[36], F2[36], F3[36], F4[36];
  for(int i1=0; i1<36; i1++)
  {
    F1[i1] = 0;
    F2[i1] = 0;
    F3[i1] = 0;
    F4[i1] = 0;
  }
  GetDStoParticle( p, ds, dsdr );
  
  float32_v V0Tmp[36] ;
  float32_v V1Tmp[36] ;

  
  float32_v mP1[8], mC1[36];
  float32_v mP2[8], mC2[36]; 
  
    Transport(ds[0], dsdr[0], mP1, mC1, dsdr[1], F1, F2);
  p.Transport(ds[1], dsdr[3], mP2, mC2, dsdr[2], F4, F3);
  
  MultQSQt(F2, p.fC, V0Tmp, 6);
  MultQSQt(F3,   fC, V1Tmp, 6);
      
  for(int iC=0; iC<3; iC++)
    mC1[iC] += V0Tmp[iC] + mC2[iC] + V1Tmp[iC];

  float32_v d[3]={ mP2[0]-mP1[0], mP2[1]-mP1[1], mP2[2]-mP1[2]};
  
  return ( ( mC1[0]*d[0] + mC1[1]*d[1])*d[0]
           +(mC1[1]*d[0] + mC1[2]*d[1])*d[1] );
}

float32_v KFParticleSIMD::GetDeviationFromVertexXY( const float32_v vtx[], const float32_v Cv[] ) const 
{
  /** Returns sqrt(Chi2/ndf) deviation from the vertex in the XY plane.
   ** \param[in] vtx[2] - { X, Y } coordinates of the vertex
   ** \param[in] Cv[3] - lower-triangular part of the covariance matrix of the vertex
   **/
  
  float32_v mP[8];
  float32_v mC[36];
  float32_v dsdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
  const float32_v dS = GetDStoPoint(vtx, dsdr);
  float32_v dsdp[6] = {-dsdr[0], -dsdr[1], -dsdr[2], 0.f, 0.f, 0.f};
  float32_v F[36], F1[36];
  for(int i2=0; i2<36; i2++)
  {
    F[i2]  = 0.f;
    F1[i2] = 0.f;
  }
  Transport( dS, dsdr, mP, mC, dsdp, F, F1 );  

  if(Cv)
  {
    float32_v VFT[3][6];
    for(int i=0; i<3; i++)
      for(int j=0; j<6; j++)
      {
        VFT[i][j] = 0;
        for(int k=0; k<3; k++)
        {
          VFT[i][j] +=  Cv[IJ(i,k)] * F1[j*6+k];
        }
      }
  
    float32_v FVFT[6][6];
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
  
  float32_v d[3]={ vtx[0]-mP[0], vtx[1]-mP[1], vtx[2]-mP[2]};

  return ( ( mC[0]*d[0] + mC[1]*d[1] )*d[0]
           +(mC[1]*d[0] + mC[2]*d[1] )*d[1] );
}

float32_v KFParticleSIMD::GetDeviationFromVertexXY( const KFParticleSIMD &Vtx ) const  
{
  /** Returns sqrt(Chi2/ndf) deviation from the vertex in the KFParticle format in the XY plane.
   ** \param[in] Vtx - the vertex in the KFParticle format
   **/
  
  return GetDeviationFromVertexXY( Vtx.fP, Vtx.fC );
}

#ifdef HomogeneousField
float32_v KFParticleSIMD::GetDeviationFromVertexXY( const KFPVertex &Vtx ) const 
{
  /** Returns sqrt(Chi2/ndf) deviation from the vertex in the KFPVertex format in the XY plane.
   ** \param[in] Vtx - the vertex in the KFPVertex format
   **/
  
  KFParticleSIMD v(Vtx);
  return GetDeviationFromVertexXY( v.fP, v.fC );
}
#endif

void KFParticleSIMD::TransportToDS( float32_v dS, const float32_v* dsdr )
{ 
  /** Transport the particle on a certain distane. The distance is defined by the dS=l/p parameter, where \n
   ** 1) l - signed distance;\n
   ** 2) p - momentum of the particle. \n
   ** \param[in] dS = l/p - distance normalised to the momentum of the particle to be transported on
   ** \param[in] dsdr[6] = ds/dr partial derivatives of the parameter dS over the state vector of the current particle
   **/
  
  Transport( dS, dsdr, fP, fC );
}

void KFParticleSIMD::TransportToDSLine( float32_v dS, const float32_v* dsdr )
{ 
  /** Transport the particle on a certain distane assuming the linear trajectory. 
   ** The distance is defined by the dS=l/p parameter, where \n
   ** 1) l - signed distance;\n
   ** 2) p - momentum of the particle. \n
   ** \param[in] dS = l/p - distance normalised to the momentum of the particle to be transported on
   ** \param[in] dsdr[6] = ds/dr partial derivatives of the parameter dS over the state vector of the current particle
   **/
  
  TransportLine( dS, dsdr, fP, fC );
}

void KFParticleSIMD::TransportCBM( float32_v dS, const float32_v* dsdr, float32_v P[], float32_v C[], float32_v* dsdr1, float32_v* F, float32_v* F1 ) const
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
  
  if( (fQ == int32_v(0)).isFull() ){
    TransportLine( dS, dsdr, P, C, dsdr1, F, F1 );
    return;
  }

  const float32_v kCLight = 0.000299792458f;

  float32_v c = toFloat(fQ)*kCLight;

  // construct coefficients 

  float32_v 
    px   = fP[3],
    py   = fP[4],
    pz   = fP[5];
      
  float32_v sx=0.f, sy=0.f, sz=0.f, syy=0.f, syz=0.f, syyy=0.f, ssx=0.f, ssy=0.f, ssz=0.f, ssyy=0.f, ssyz=0.f, ssyyy=0.f;

  { // get field integrals

    float32_v fld[3][3];   
    float32_v p0[3], p1[3], p2[3];

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

      float32_v ssy1 = ( 7.f*fld[0][1] + 6.f*fld[1][1]-fld[2][1] )*c*dS*dS/96.f;
      float32_v ssy2 = (   fld[0][1] + 2.f*fld[1][1]         )*c*dS*dS/6.f;

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
        fld[iF1][iF2] = select(abs(fld[iF1][iF2]) > 100.f, 0.f, fld[iF1][iF2]);

    sx = c*( fld[0][0] + 4*fld[1][0] + fld[2][0] )*dS/6.f;
    sy = c*( fld[0][1] + 4*fld[1][1] + fld[2][1] )*dS/6.f;
    sz = c*( fld[0][2] + 4*fld[1][2] + fld[2][2] )*dS/6.f;

    ssx = c*( fld[0][0] + 2*fld[1][0])*dS*dS/6.f;
    ssy = c*( fld[0][1] + 2*fld[1][1])*dS*dS/6.f;
    ssz = c*( fld[0][2] + 2*fld[1][2])*dS*dS/6.f;

    float32_v c2[3][3]    =   { {  5.f, -4.f, -1.f},{  44.f,  80.f,  -4.f},{ 11.f, 44.f, 5.f} }; // /=360.    
    float32_v cc2[3][3]    =   { { 38.f,  8.f, -4.f},{ 148.f, 208.f, -20.f},{  3.f, 36.f, 3.f} }; // /=2520.
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

//   float32_v mJ[11];
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

  float32_v mJ[8][8];
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

  float32_v mJds[6][6];
  for( Int_t i=0; i<6; i++ ) for( Int_t j=0; j<6; j++) mJds[i][j]=0;

  mJds[0][3]= 1.f;
  mJds[1][4]= 1.f;
  mJds[2][5]= 1.f;
  
  mJds[0][3] = select(abs(dS)>0.f, 1.f - 3.f*ssyy/dS, mJds[0][3]);
  mJds[1][3] = select(abs(dS)>0.f, -2.f*ssz/dS, mJds[1][3]);
  mJds[2][3] = select(abs(dS)>0.f, (2.f*ssy-4.f*ssyyy)/dS, mJds[2][3]);

  mJds[0][4] = select(abs(dS)>0.f, 2.f*ssx/dS, mJds[0][4]);
  mJds[1][4] = select(abs(dS)>0.f, 1.f, mJds[1][4]);
  mJds[2][4] = select(abs(dS)>0.f,-2.f*ssx/dS, mJds[2][4]);

  mJds[0][5] = select(abs(dS)>0.f, (4.f*ssyyy-2.f*ssy)/dS, mJds[0][5]);
  mJds[1][5] = select(abs(dS)>0.f, (2.f*ssx + 3.f*ssyz)/dS, mJds[1][5]);
  mJds[2][5] = select(abs(dS)>0.f, 1.f - 3.f*ssyy/dS, mJds[2][5]);

  mJds[3][3] = select(abs(dS)>0.f, -2.f*syy/dS, mJds[3][3]);
  mJds[4][3] = select(abs(dS)>0.f, -sz/dS, mJds[4][3]);
  mJds[5][3] = select(abs(dS)>0.f, sy/dS - 3.f*syyy/dS, mJds[5][3]);

  mJds[3][4] = select(abs(dS)>0.f, sx/dS, mJds[3][4]);
  mJds[4][4] = select(abs(dS)>0.f,0.f, mJds[4][4]);
  mJds[5][4] = select(abs(dS)>0.f,-sx/dS, mJds[5][4]);

  mJds[3][5] = select(abs(dS)>0.f,3.f*syyy/dS - sy/dS, mJds[3][5]);
  mJds[4][5] = select(abs(dS)>0.f, sx/dS + 2.f*syz/dS, mJds[4][5]);
  mJds[5][5] = select(abs(dS)>0.f,-2.f*syy/dS, mJds[5][5]);

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

void KFParticleSIMD::TransportCBM( float32_v dS, float32_v P[] ) const
{  
  /** Transports the parameters of the current particle assuming CBM-like nonhomogeneous 
   ** magnetic field on the length defined by the transport parameter dS = l/p, where l is the signed distance and p is 
   ** the momentum of the current particle. The obtained parameters and covariance matrix are stored to the array P. 
   ** P can be set to the parameters fP. In this
   ** case the particle parameters will be modified.
   ** \param[in] dS - transport parameter which defines the distance to which particle should be transported
   ** \param[out] P[8] - array, where transported parameters should be stored
   **/ 
  if( (fQ == int32_v(0)).isFull() ){
    TransportLine( dS, P );
    return;
  }

  const float32_v kCLight = 0.000299792458f;

  float32_v c = toFloat(fQ)*kCLight;

  // construct coefficients 

  float32_v 
    px   = fP[3],
    py   = fP[4],
    pz   = fP[5];
      
  float32_v sx=0.f, sy=0.f, sz=0.f, syy=0.f, syz=0.f, syyy=0.f, ssx=0.f, ssy=0.f, ssz=0.f, ssyy=0.f, ssyz=0.f, ssyyy=0.f;

  { // get field integrals

    float32_v fld[3][3];   
    float32_v p0[3], p1[3], p2[3];

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

      float32_v ssy1 = ( 7.f*fld[0][1] + 6.f*fld[1][1]-fld[2][1] )*c*dS*dS/96.f;
      float32_v ssy2 = (   fld[0][1] + 2.f*fld[1][1]         )*c*dS*dS/6.f;

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
        fld[iF1][iF2] = select(abs(fld[iF1][iF2]) > 100.f, 0.f, fld[iF1][iF2]);

    sx = c*( fld[0][0] + 4*fld[1][0] + fld[2][0] )*dS/6.f;
    sy = c*( fld[0][1] + 4*fld[1][1] + fld[2][1] )*dS/6.f;
    sz = c*( fld[0][2] + 4*fld[1][2] + fld[2][2] )*dS/6.f;

    ssx = c*( fld[0][0] + 2*fld[1][0])*dS*dS/6.f;
    ssy = c*( fld[0][1] + 2*fld[1][1])*dS*dS/6.f;
    ssz = c*( fld[0][2] + 2*fld[1][2])*dS*dS/6.f;

    float32_v c2[3][3]    =   { {  5.f, -4.f, -1.f},{  44.f,  80.f,  -4.f},{ 11.f, 44.f, 5.f} }; // /=360.    
    float32_v cc2[3][3]    =   { { 38.f,  8.f, -4.f},{ 148.f, 208.f, -20.f},{  3.f, 36.f, 3.f} }; // /=2520.
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

  float32_v mJ[8][8];
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

void KFParticleSIMD::TransportBz( float32_v Bz, float32_v dS, const float32_v* dsdr, float32_v P[], float32_v C[], float32_v* dsdr1, float32_v* F, float32_v* F1, const bool fullC  ) const 
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
  
  const float32_v kCLight = 0.000299792458f;
  Bz = Bz*toFloat(fQ)*kCLight;
  float32_v bs= Bz*dS;
  float32_v s, c;
  KFPMath::sincos(bs, s, c);

  float32_v sB(0.f), cB(0.f);

  const float32_v kOvSqr6 = 1.f/sqrt(float32_v(6.f));
  const float32_v LocalSmall = 1.e-10f;

  Bz = select(abs(bs) <= LocalSmall, LocalSmall, Bz);
  sB = select(LocalSmall < abs(bs), s/Bz, sB);
  sB = select(LocalSmall >= abs(bs), (1.f-bs*kOvSqr6)*(1.f+bs*kOvSqr6)*dS, sB);
  cB = select(LocalSmall < abs(bs), (1.f-c)/Bz, cB);
  cB = select(LocalSmall >= abs(bs), .5f*sB*bs, cB);
  
  float32_v px = fP[3];
  float32_v py = fP[4];
  float32_v pz = fP[5];

  P[0] = fP[0] + sB*px + cB*py;
  P[1] = fP[1] - cB*px + sB*py;
  P[2] = fP[2] +  dS*pz;
  P[3] =          c*px + s*py;
  P[4] =         -s*px + c*py;
  P[5] = fP[5];
  P[6] = fP[6];
  P[7] = fP[7];

  float32_v mJ[6][6];
  
  const float32_v cPx = c * px;
  const float32_v sPy = s * py;
  mJ[0][0] = (cPx * dsdr[0] + sPy * dsdr[0]) + 1.f;
  mJ[0][1] = cPx * dsdr[1] + sPy * dsdr[1];
  mJ[0][2] = cPx * dsdr[2] + sPy * dsdr[2];
  mJ[0][3] = (cPx * dsdr[3] + sPy * dsdr[3]) + sB;
  mJ[0][4] = (cPx * dsdr[4] + sPy * dsdr[4]) + cB;
  mJ[0][5] = cPx * dsdr[5] + sPy * dsdr[5];
  
  const float32_v sPx = s * px;
  const float32_v cPy = c * py;
  mJ[1][0] = (cPy * dsdr[0] - sPx * dsdr[0]);
  mJ[1][1] = (cPy * dsdr[1] - sPx * dsdr[1]) + 1.f;
  mJ[1][2] = (cPy * dsdr[2] - sPx * dsdr[2]);
  mJ[1][3] = (cPy * dsdr[3] - sPx * dsdr[3]) - cB;
  mJ[1][4] = (cPy * dsdr[4] - sPx * dsdr[4]) + sB;
  mJ[1][5] = (cPy * dsdr[5] - sPx * dsdr[5]);

  mJ[2][0] = pz*dsdr[0];
  mJ[2][1] = pz*dsdr[1];
  mJ[2][2] = pz*dsdr[2] + 1.f;
  mJ[2][3] = pz*dsdr[3];
  mJ[2][4] = pz*dsdr[4];
  mJ[2][5] = pz*dsdr[5] + dS;
  
  float32_v CJt[6][5];
  
  CJt[0][0] = fC[ 0]*mJ[0][0] + fC[ 1]*mJ[0][1] + fC[ 3]*mJ[0][2] + fC[ 6]*mJ[0][3] + fC[10]*mJ[0][4] + fC[15]*mJ[0][5];
  CJt[0][1] = fC[ 0]*mJ[1][0] + fC[ 1]*mJ[1][1] + fC[ 3]*mJ[1][2] + fC[ 6]*mJ[1][3] + fC[10]*mJ[1][4] + fC[15]*mJ[1][5];
  CJt[0][2] = fC[ 0]*mJ[2][0] + fC[ 1]*mJ[2][1] + fC[ 3]*mJ[2][2] + fC[ 6]*mJ[2][3] + fC[10]*mJ[2][4] + fC[15]*mJ[2][5];

  CJt[1][0] = fC[ 1]*mJ[0][0] + fC[ 2]*mJ[0][1] + fC[ 4]*mJ[0][2] + fC[ 7]*mJ[0][3] + fC[11]*mJ[0][4] + fC[16]*mJ[0][5];
  CJt[1][1] = fC[ 1]*mJ[1][0] + fC[ 2]*mJ[1][1] + fC[ 4]*mJ[1][2] + fC[ 7]*mJ[1][3] + fC[11]*mJ[1][4] + fC[16]*mJ[1][5];
  CJt[1][2] = fC[ 1]*mJ[2][0] + fC[ 2]*mJ[2][1] + fC[ 4]*mJ[2][2] + fC[ 7]*mJ[2][3] + fC[11]*mJ[2][4] + fC[16]*mJ[2][5];

  CJt[2][0] = fC[ 3]*mJ[0][0] + fC[ 4]*mJ[0][1] + fC[ 5]*mJ[0][2] + fC[ 8]*mJ[0][3] + fC[12]*mJ[0][4] + fC[17]*mJ[0][5];
  CJt[2][1] = fC[ 3]*mJ[1][0] + fC[ 4]*mJ[1][1] + fC[ 5]*mJ[1][2] + fC[ 8]*mJ[1][3] + fC[12]*mJ[1][4] + fC[17]*mJ[1][5];
  CJt[2][2] = fC[ 3]*mJ[2][0] + fC[ 4]*mJ[2][1] + fC[ 5]*mJ[2][2] + fC[ 8]*mJ[2][3] + fC[12]*mJ[2][4] + fC[17]*mJ[2][5];

  CJt[3][0] = fC[ 6]*mJ[0][0] + fC[ 7]*mJ[0][1] + fC[ 8]*mJ[0][2] + fC[ 9]*mJ[0][3] + fC[13]*mJ[0][4] + fC[18]*mJ[0][5];
  CJt[3][1] = fC[ 6]*mJ[1][0] + fC[ 7]*mJ[1][1] + fC[ 8]*mJ[1][2] + fC[ 9]*mJ[1][3] + fC[13]*mJ[1][4] + fC[18]*mJ[1][5];
  CJt[3][2] = fC[ 6]*mJ[2][0] + fC[ 7]*mJ[2][1] + fC[ 8]*mJ[2][2] + fC[ 9]*mJ[2][3] + fC[13]*mJ[2][4] + fC[18]*mJ[2][5];

  CJt[4][0] = fC[10]*mJ[0][0] + fC[11]*mJ[0][1] + fC[12]*mJ[0][2] + fC[13]*mJ[0][3] + fC[14]*mJ[0][4] + fC[19]*mJ[0][5];
  CJt[4][1] = fC[10]*mJ[1][0] + fC[11]*mJ[1][1] + fC[12]*mJ[1][2] + fC[13]*mJ[1][3] + fC[14]*mJ[1][4] + fC[19]*mJ[1][5];
  CJt[4][2] = fC[10]*mJ[2][0] + fC[11]*mJ[2][1] + fC[12]*mJ[2][2] + fC[13]*mJ[2][3] + fC[14]*mJ[2][4] + fC[19]*mJ[2][5];

  CJt[5][0] = fC[15]*mJ[0][0] + fC[16]*mJ[0][1] + fC[17]*mJ[0][2] + fC[18]*mJ[0][3] + fC[19]*mJ[0][4] + fC[20]*mJ[0][5];
  CJt[5][1] = fC[15]*mJ[1][0] + fC[16]*mJ[1][1] + fC[17]*mJ[1][2] + fC[18]*mJ[1][3] + fC[19]*mJ[1][4] + fC[20]*mJ[1][5];
  CJt[5][2] = fC[15]*mJ[2][0] + fC[16]*mJ[2][1] + fC[17]*mJ[2][2] + fC[18]*mJ[2][3] + fC[19]*mJ[2][4] + fC[20]*mJ[2][5];

  
  C[ 0] = mJ[0][0]*CJt[0][0] + mJ[0][1]*CJt[1][0] + mJ[0][2]*CJt[2][0] + mJ[0][3]*CJt[3][0] + mJ[0][4]*CJt[4][0] + mJ[0][5]*CJt[5][0];
  
  C[ 1] = mJ[1][0]*CJt[0][0] + mJ[1][1]*CJt[1][0] + mJ[1][2]*CJt[2][0] + mJ[1][3]*CJt[3][0] + mJ[1][4]*CJt[4][0] + mJ[1][5]*CJt[5][0];
  C[ 2] = mJ[1][0]*CJt[0][1] + mJ[1][1]*CJt[1][1] + mJ[1][2]*CJt[2][1] + mJ[1][3]*CJt[3][1] + mJ[1][4]*CJt[4][1] + mJ[1][5]*CJt[5][1];

  C[ 3] = mJ[2][0]*CJt[0][0] + mJ[2][1]*CJt[1][0] + mJ[2][2]*CJt[2][0] + mJ[2][3]*CJt[3][0] + mJ[2][4]*CJt[4][0] + mJ[2][5]*CJt[5][0];
  C[ 4] = mJ[2][0]*CJt[0][1] + mJ[2][1]*CJt[1][1] + mJ[2][2]*CJt[2][1] + mJ[2][3]*CJt[3][1] + mJ[2][4]*CJt[4][1] + mJ[2][5]*CJt[5][1];
  C[ 5] = mJ[2][0]*CJt[0][2] + mJ[2][1]*CJt[1][2] + mJ[2][2]*CJt[2][2] + mJ[2][3]*CJt[3][2] + mJ[2][4]*CJt[4][2] + mJ[2][5]*CJt[5][2];

  const float32_v sBzPx = Bz * sPx;
  const float32_v cBzPy = Bz * cPy;
  const float32_v cBzPx = Bz * cPx;
  const float32_v sBzPy = Bz * sPy;
  
  if(fullC){
    mJ[3][0] = (cBzPy * dsdr[0] - sBzPx * dsdr[0]);
    mJ[3][1] = (cBzPy * dsdr[1] - sBzPx * dsdr[1]);
    mJ[3][2] = (cBzPy * dsdr[2] - sBzPx * dsdr[2]);
    mJ[3][3] = (cBzPy * dsdr[3] - sBzPx * dsdr[3]) + c;
    mJ[3][4] = (cBzPy * dsdr[4] - sBzPx * dsdr[4]) + s;
    mJ[3][5] = (cBzPy * dsdr[5] - sBzPx * dsdr[5]);
    
    mJ[4][0] = -(cBzPx * dsdr[0] + sBzPy * dsdr[0]);
    mJ[4][1] = -(cBzPx * dsdr[1] + sBzPy * dsdr[1]);
    mJ[4][2] = -(cBzPx * dsdr[2] + sBzPy * dsdr[2]);
    mJ[4][3] = -s - (cBzPx * dsdr[3] + sBzPy * dsdr[3]);
    mJ[4][4] = c - (cBzPx * dsdr[4] + sBzPy * dsdr[4]);
    mJ[4][5] = -(cBzPx * dsdr[5] + sBzPy * dsdr[5]);
    
    mJ[5][0] = 0.f;
    mJ[5][1] = 0.f;
    mJ[5][2] = 0.f;
    mJ[5][3] = 0.f;
    mJ[5][4] = 0.f;
    mJ[5][5] = 1.f;
  
    CJt[0][3] = fC[ 0]*mJ[3][0] + fC[ 1]*mJ[3][1] + fC[ 3]*mJ[3][2] + fC[ 6]*mJ[3][3] + fC[10]*mJ[3][4] + fC[15]*mJ[3][5];
    CJt[0][4] = fC[ 0]*mJ[4][0] + fC[ 1]*mJ[4][1] + fC[ 3]*mJ[4][2] + fC[ 6]*mJ[4][3] + fC[10]*mJ[4][4] + fC[15]*mJ[4][5];

    CJt[1][3] = fC[ 1]*mJ[3][0] + fC[ 2]*mJ[3][1] + fC[ 4]*mJ[3][2] + fC[ 7]*mJ[3][3] + fC[11]*mJ[3][4] + fC[16]*mJ[3][5];
    CJt[1][4] = fC[ 1]*mJ[4][0] + fC[ 2]*mJ[4][1] + fC[ 4]*mJ[4][2] + fC[ 7]*mJ[4][3] + fC[11]*mJ[4][4] + fC[16]*mJ[4][5];

    CJt[2][3] = fC[ 3]*mJ[3][0] + fC[ 4]*mJ[3][1] + fC[ 5]*mJ[3][2] + fC[ 8]*mJ[3][3] + fC[12]*mJ[3][4] + fC[17]*mJ[3][5];
    CJt[2][4] = fC[ 3]*mJ[4][0] + fC[ 4]*mJ[4][1] + fC[ 5]*mJ[4][2] + fC[ 8]*mJ[4][3] + fC[12]*mJ[4][4] + fC[17]*mJ[4][5];

    CJt[3][3] = fC[ 6]*mJ[3][0] + fC[ 7]*mJ[3][1] + fC[ 8]*mJ[3][2] + fC[ 9]*mJ[3][3] + fC[13]*mJ[3][4] + fC[18]*mJ[3][5];
    CJt[3][4] = fC[ 6]*mJ[4][0] + fC[ 7]*mJ[4][1] + fC[ 8]*mJ[4][2] + fC[ 9]*mJ[4][3] + fC[13]*mJ[4][4] + fC[18]*mJ[4][5];

    CJt[4][3] = fC[10]*mJ[3][0] + fC[11]*mJ[3][1] + fC[12]*mJ[3][2] + fC[13]*mJ[3][3] + fC[14]*mJ[3][4] + fC[19]*mJ[3][5];
    CJt[4][4] = fC[10]*mJ[4][0] + fC[11]*mJ[4][1] + fC[12]*mJ[4][2] + fC[13]*mJ[4][3] + fC[14]*mJ[4][4] + fC[19]*mJ[4][5];

    CJt[5][3] = fC[15]*mJ[3][0] + fC[16]*mJ[3][1] + fC[17]*mJ[3][2] + fC[18]*mJ[3][3] + fC[19]*mJ[3][4] + fC[20]*mJ[3][5];
    CJt[5][4] = fC[15]*mJ[4][0] + fC[16]*mJ[4][1] + fC[17]*mJ[4][2] + fC[18]*mJ[4][3] + fC[19]*mJ[4][4] + fC[20]*mJ[4][5];
  
    C[ 6] = mJ[3][0]*CJt[0][0] + mJ[3][1]*CJt[1][0] + mJ[3][2]*CJt[2][0] + mJ[3][3]*CJt[3][0] + mJ[3][4]*CJt[4][0] + mJ[3][5]*CJt[5][0];
    C[ 7] = mJ[3][0]*CJt[0][1] + mJ[3][1]*CJt[1][1] + mJ[3][2]*CJt[2][1] + mJ[3][3]*CJt[3][1] + mJ[3][4]*CJt[4][1] + mJ[3][5]*CJt[5][1];
    C[ 8] = mJ[3][0]*CJt[0][2] + mJ[3][1]*CJt[1][2] + mJ[3][2]*CJt[2][2] + mJ[3][3]*CJt[3][2] + mJ[3][4]*CJt[4][2] + mJ[3][5]*CJt[5][2];
    C[ 9] = mJ[3][0]*CJt[0][3] + mJ[3][1]*CJt[1][3] + mJ[3][2]*CJt[2][3] + mJ[3][3]*CJt[3][3] + mJ[3][4]*CJt[4][3] + mJ[3][5]*CJt[5][3];
    
    C[10] = mJ[4][0]*CJt[0][0] + mJ[4][1]*CJt[1][0] + mJ[4][2]*CJt[2][0] + mJ[4][3]*CJt[3][0] + mJ[4][4]*CJt[4][0] + mJ[4][5]*CJt[5][0];
    C[11] = mJ[4][0]*CJt[0][1] + mJ[4][1]*CJt[1][1] + mJ[4][2]*CJt[2][1] + mJ[4][3]*CJt[3][1] + mJ[4][4]*CJt[4][1] + mJ[4][5]*CJt[5][1];
    C[12] = mJ[4][0]*CJt[0][2] + mJ[4][1]*CJt[1][2] + mJ[4][2]*CJt[2][2] + mJ[4][3]*CJt[3][2] + mJ[4][4]*CJt[4][2] + mJ[4][5]*CJt[5][2];
    C[13] = mJ[4][0]*CJt[0][3] + mJ[4][1]*CJt[1][3] + mJ[4][2]*CJt[2][3] + mJ[4][3]*CJt[3][3] + mJ[4][4]*CJt[4][3] + mJ[4][5]*CJt[5][3];
    C[14] = mJ[4][0]*CJt[0][4] + mJ[4][1]*CJt[1][4] + mJ[4][2]*CJt[2][4] + mJ[4][3]*CJt[3][4] + mJ[4][4]*CJt[4][4] + mJ[4][5]*CJt[5][4];
    
    C[15] = CJt[5][0];
    C[16] = CJt[5][1];
    C[17] = CJt[5][2];
    C[18] = CJt[5][3];
    C[19] = CJt[5][4];
    C[20] = fC[20];

    const float32_v C21 = fC[21]*mJ[0][0] + fC[22]*mJ[0][1] + fC[23]*mJ[0][2] + fC[24]*mJ[0][3] + fC[25]*mJ[0][4] + fC[26]*mJ[0][5];
    const float32_v C22 = fC[21]*mJ[1][0] + fC[22]*mJ[1][1] + fC[23]*mJ[1][2] + fC[24]*mJ[1][3] + fC[25]*mJ[1][4] + fC[26]*mJ[1][5];
    const float32_v C23 = fC[21]*mJ[2][0] + fC[22]*mJ[2][1] + fC[23]*mJ[2][2] + fC[24]*mJ[2][3] + fC[25]*mJ[2][4] + fC[26]*mJ[2][5];
    const float32_v C24 = fC[21]*mJ[3][0] + fC[22]*mJ[3][1] + fC[23]*mJ[3][2] + fC[24]*mJ[3][3] + fC[25]*mJ[3][4] + fC[26]*mJ[3][5];
    const float32_v C25 = fC[21]*mJ[4][0] + fC[22]*mJ[4][1] + fC[23]*mJ[4][2] + fC[24]*mJ[4][3] + fC[25]*mJ[4][4] + fC[26]*mJ[4][5];
    C[21] = C21;
    C[22] = C22;
    C[23] = C23;
    C[24] = C24;
    C[25] = C25;
    C[26] = fC[26];
    C[27] = fC[27];
    
    const float32_v C28 = fC[28]*mJ[0][0] + fC[29]*mJ[0][1] + fC[30]*mJ[0][2] + fC[31]*mJ[0][3] + fC[32]*mJ[0][4] + fC[33]*mJ[0][5];
    const float32_v C29 = fC[28]*mJ[1][0] + fC[29]*mJ[1][1] + fC[30]*mJ[1][2] + fC[31]*mJ[1][3] + fC[32]*mJ[1][4] + fC[33]*mJ[1][5];
    const float32_v C30 = fC[28]*mJ[2][0] + fC[29]*mJ[2][1] + fC[30]*mJ[2][2] + fC[31]*mJ[2][3] + fC[32]*mJ[2][4] + fC[33]*mJ[2][5];
    const float32_v C31 = fC[28]*mJ[3][0] + fC[29]*mJ[3][1] + fC[30]*mJ[3][2] + fC[31]*mJ[3][3] + fC[32]*mJ[3][4] + fC[33]*mJ[3][5];
    const float32_v C32 = fC[28]*mJ[4][0] + fC[29]*mJ[4][1] + fC[30]*mJ[4][2] + fC[31]*mJ[4][3] + fC[32]*mJ[4][4] + fC[33]*mJ[4][5];
    C[28] = C28;
    C[29] = C29;
    C[30] = C30;
    C[31] = C31;
    C[32] = C32;
    C[33] = fC[33];
    C[34] = fC[34];
    C[35] = fC[35];
  }

  if(F) {
    for(int i=0; i<6; i++)
      for(int j=0; j<6; j++)
        F[i*6+j] = mJ[i][j];
  }  

  if(F1) {
    F1[ 0] = cPx * dsdr1[0] + sPy * dsdr1[0];
    F1[ 1] = cPx * dsdr1[1] + sPy * dsdr1[1];
    F1[ 2] = cPx * dsdr1[2] + sPy * dsdr1[2];
    
    F1[ 6] = cPy * dsdr1[0] - sPx * dsdr1[0];
    F1[ 7] = cPy * dsdr1[1] - sPx * dsdr1[1];
    F1[ 8] = cPy * dsdr1[2] - sPx * dsdr1[2];

    F1[12] = pz*dsdr1[0];
    F1[13] = pz*dsdr1[1];
    F1[14] = pz*dsdr1[2];
        
    if(fullC){
      F1[ 3] = cPx * dsdr1[3] + sPy * dsdr1[3];
      F1[ 4] = cPx * dsdr1[4] + sPy * dsdr1[4];
      F1[ 5] = cPx * dsdr1[5] + sPy * dsdr1[5];

      F1[ 9] = cPy * dsdr1[3] - sPx * dsdr1[3];
      F1[10] = cPy * dsdr1[4] - sPx * dsdr1[4];
      F1[11] = cPy * dsdr1[5] - sPx * dsdr1[5];

      F1[15] = pz*dsdr1[3];
      F1[16] = pz*dsdr1[4];
      F1[17] = pz*dsdr1[5];
      
      F1[18] = cBzPy * dsdr1[0] - sBzPx * dsdr1[0];
      F1[19] = cBzPy * dsdr1[1] - sBzPx * dsdr1[1];
      F1[20] = cBzPy * dsdr1[2] - sBzPx * dsdr1[2];
      F1[21] = cBzPy * dsdr1[3] - sBzPx * dsdr1[3];
      F1[22] = cBzPy * dsdr1[4] - sBzPx * dsdr1[4];
      F1[23] = cBzPy * dsdr1[5] - sBzPx * dsdr1[5];

      F1[24] = -(cBzPx * dsdr1[0] + sBzPy * dsdr1[0]);
      F1[25] = -(cBzPx * dsdr1[1] + sBzPy * dsdr1[1]);
      F1[26] = -(cBzPx * dsdr1[2] + sBzPy * dsdr1[2]);
      F1[27] = -(cBzPx * dsdr1[3] + sBzPy * dsdr1[3]);
      F1[28] = -(cBzPx * dsdr1[4] + sBzPy * dsdr1[4]);
      F1[29] = -(cBzPx * dsdr1[5] + sBzPy * dsdr1[5]);

      F1[30] = 0.f;
      F1[31] = 0.f;
      F1[32] = 0.f;
      F1[33] = 0.f;
      F1[34] = 0.f;
      F1[35] = 0.f;    
    }
  }
}

void KFParticleSIMD::TransportBz( float32_v Bz, float32_v dS, float32_v P[] ) const 
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
  
  const float32_v kCLight = 0.000299792458f;
  Bz = Bz*toFloat(fQ)*kCLight;
  float32_v bs= Bz*dS;
  float32_v s, c;
  KFPMath::sincos(bs, s, c);

  float32_v sB(0.f), cB(0.f);

  const float32_v kOvSqr6 = 1.f/sqrt(float32_v(6.f));
  const float32_v LocalSmall = 1.e-10f;

  Bz = select(abs(bs) <= LocalSmall, LocalSmall, Bz);
  sB = select(LocalSmall < abs(bs), s/Bz, sB);
  sB = select(LocalSmall >= abs(bs), (1.f-bs*kOvSqr6)*(1.f+bs*kOvSqr6)*dS, sB);
  cB = select(LocalSmall < abs(bs), (1.f-c)/Bz, cB);
  cB = select(LocalSmall >= abs(bs), .5f*sB*bs, cB);
  
  float32_v px = fP[3];
  float32_v py = fP[4];
  float32_v pz = fP[5];

  P[0] = fP[0] + sB*px + cB*py;
  P[1] = fP[1] - cB*px + sB*py;
  P[2] = fP[2] +  dS*pz;
  P[3] =          c*px + s*py;
  P[4] =         -s*px + c*py;
  P[5] = fP[5];
  P[6] = fP[6];
  P[7] = fP[7];
}

void KFParticleSIMD::TransportLine( float32_v dS, const float32_v* dsdr, float32_v P[], float32_v C[], float32_v* dsdr1, float32_v* F, float32_v* F1 ) const 
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
  
  float32_v mJ[8][8];
  for( Int_t i=0; i<8; i++ ) for( Int_t j=0; j<8; j++) mJ[i][j]=0;

  mJ[0][0]=1; mJ[0][1]=0; mJ[0][2]=0; mJ[0][3]=dS;  mJ[0][4]=0;  mJ[0][5]=0;
  mJ[1][0]=0; mJ[1][1]=1; mJ[1][2]=0; mJ[1][3]=0;     mJ[1][4]=dS;  mJ[1][5]=0;
  mJ[2][0]=0; mJ[2][1]=0; mJ[2][2]=1; mJ[2][3]=0; mJ[2][4]=0; mJ[2][5]=dS;
  
  mJ[3][0]=0; mJ[3][1]=0; mJ[3][2]=0; mJ[3][3]=1;   mJ[3][4]=0;  mJ[3][5]=0;
  mJ[4][0]=0; mJ[4][1]=0; mJ[4][2]=0; mJ[4][3]=0;     mJ[4][4]=1;   mJ[4][5]=0;
  mJ[5][0]=0; mJ[5][1]=0; mJ[5][2]=0; mJ[5][3]=0; mJ[5][4]=0; mJ[5][5]=1;
  mJ[6][6] = mJ[7][7] = 1;
  
  float32_v px = fP[3], py = fP[4], pz = fP[5];
  
  P[0] = fP[0] + dS*fP[3];
  P[1] = fP[1] + dS*fP[4];
  P[2] = fP[2] + dS*fP[5];
  P[3] = fP[3];
  P[4] = fP[4];
  P[5] = fP[5];
  P[6] = fP[6];
  P[7] = fP[7];
  
  float32_v mJds[6][6];
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

void KFParticleSIMD::TransportLine( float32_v dS, float32_v P[] ) const 
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

mask32_v KFParticleSIMD::GetMomentum( float32_v &p, float32_v &error )  const 
{
  /** Calculates particle momentum and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** \param[out] p - momentum of the particle
   ** \param[out] error - its error
   **/
  
  float32_v x = fP[3];
  float32_v y = fP[4];
  float32_v z = fP[5];
  float32_v x2 = x*x;
  float32_v y2 = y*y;
  float32_v z2 = z*z;
  float32_v p2 = x2+y2+z2;
  p = sqrt(p2);
  error = (x2*fC[9]+y2*fC[14]+z2*fC[20] + 2*(x*y*fC[13]+x*z*fC[18]+y*z*fC[19]) );
  const float32_v LocalSmall = 1.e-4f;
  mask32_v mask = (0.f < error) && (LocalSmall < abs(p));
  error = select(!mask, 1.e20f, error);
  error = sqrt(error);
  return (!mask);
}

mask32_v KFParticleSIMD::GetPt( float32_v &pt, float32_v &error )  const 
{
  /** Calculates particle transverse  momentum and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** \param[out] pt - transverse momentum of the particle
   ** \param[out] error - its error
   **/

  float32_v px = fP[3];
  float32_v py = fP[4];
  float32_v px2 = px*px;
  float32_v py2 = py*py;
  float32_v pt2 = px2+py2;
  pt = sqrt(pt2);
  error = (px2*fC[9] + py2*fC[14] + 2*px*py*fC[13] );
  const float32_v LocalSmall = 1.e-4f;
  mask32_v mask = ( (0.f < error) && (LocalSmall < abs(pt)));
  error = select(!mask, 1.e20f, error);
  error = sqrt(error);
  return (!mask);
}

mask32_v KFParticleSIMD::GetEta( float32_v &eta, float32_v &error )  const 
{
  /** Calculates particle pseudorapidity and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** \param[out] eta - pseudorapidity of the particle
   ** \param[out] error - its error
   **/
  
  const float32_v BIG = 1.e8f;
  const float32_v LocalSmall = 1.e-8f;

  float32_v px = fP[3];
  float32_v py = fP[4];
  float32_v pz = fP[5];
  float32_v pt2 = px*px + py*py;
  float32_v p2 = pt2 + pz*pz;
  float32_v p = sqrt(p2);
  float32_v a = p + pz;
  float32_v b = p - pz;
  eta = BIG;
  float32_v c = 0.f;
  c = select(b > LocalSmall, a/b, c);
  float32_v logc = 0.5f*KFPMath::log(c);
  eta = select(LocalSmall<abs(c), logc, eta);

  float32_v h3 = -px*pz;
  float32_v h4 = -py*pz;  
  float32_v pt4 = pt2*pt2;
  float32_v p2pt4 = p2*pt4;
  error = (h3*h3*fC[9] + h4*h4*fC[14] + pt4*fC[20] + 2*( h3*(h4*fC[13] + fC[18]*pt2) + pt2*h4*fC[19] ) );

  mask32_v mask = ((LocalSmall < abs(p2pt4)) && (0.f < error));
  error = select(mask, sqrt(error/p2pt4), error);
  error = select(!mask, BIG, error);

  return (!mask);
}

mask32_v KFParticleSIMD::GetPhi( float32_v &phi, float32_v &error )  const 
{
  /** Calculates particle polar angle at the current point and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** \param[out] phi - polar angle of the particle
   ** \param[out] error - its error
   **/
  
  float32_v px = fP[3];
  float32_v py = fP[4];
  float32_v px2 = px*px;
  float32_v py2 = py*py;
  float32_v pt2 = px2 + py2;
  phi = KFPMath::ATan2(py,px);
  error = (py2*fC[9] + px2*fC[14] - float32_v(2.f)*px*py*fC[13] );

  mask32_v mask = (0.f < error) && (1.e-4f < pt2);
  error = select(mask, sqrt(error)/pt2, error);
  error = select(!mask, 1.e10f, error);
  return !mask;
}

mask32_v KFParticleSIMD::GetR( float32_v &r, float32_v &error )  const 
{
  /** Calculates the distance to the point {0,0,0} and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** \param[out] r - polar angle of the particle
   ** \param[out] error - its error
   **/
  
  float32_v x = fP[0];
  float32_v y = fP[1];
  float32_v x2 = x*x;
  float32_v y2 = y*y;
  r = sqrt(x2 + y2);
  error = (x2*fC[0] + y2*fC[2] - float32_v(2.f)*x*y*fC[1] );

  mask32_v mask = (0.f < error) && (1.e-4f < r);
  error = select(mask, sqrt(error)/r, error);
  error = select(!mask, 1.e10f, error);
  return !mask;
}

mask32_v KFParticleSIMD::GetMass( float32_v &m, float32_v &error ) const 
{
  /** Calculates the mass of the particle and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** \param[out] m - mass of the particle
   ** \param[out] error - its error
   **/  

  const float32_v BIG = 1.e8f;
  const float32_v LocalSmall = 1.e-8f;

  float32_v s = (  fP[3]*fP[3]*fC[9] + fP[4]*fP[4]*fC[14] + fP[5]*fP[5]*fC[20] 
               + fP[6]*fP[6]*fC[27] 
               + 2.f * ( fP[3]*fP[4]*fC[13] + fP[5]*(fP[3]*fC[18] + fP[4]*fC[19]) 
               - fP[6]*( fP[3]*fC[24] + fP[4]*fC[25] + fP[5]*fC[26] )   )
              ); 

  float32_v m2 = (fP[6]*fP[6] - fP[3]*fP[3] - fP[4]*fP[4] - fP[5]*fP[5]);

  mask32_v mask = 0.f <= m2;
  m = select(mask, sqrt(m2), m);
  m = select(!mask, -sqrt(-m2), m);

  mask = (mask && (0.f <= s) && (LocalSmall < m));
  error = select(mask, sqrt(s)/m, error);
  error = select(!mask, BIG, error);

  return !mask;
}

mask32_v KFParticleSIMD::GetDecayLength( float32_v &l, float32_v &error ) const 
{
  /** Calculates the decay length of the particle in the laboratory system and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** The production point should be set before calling this function.
   ** \param[out] l - the decay length
   ** \param[out] error - its error
   **/
  
  const float32_v BIG = 1.e20f;

  float32_v x = fP[3];
  float32_v y = fP[4];
  float32_v z = fP[5];
  float32_v t = fP[7];
  float32_v x2 = x*x;
  float32_v y2 = y*y;
  float32_v z2 = z*z;
  float32_v p2 = x2+y2+z2;
  l = t*sqrt(p2);

  error = p2*fC[35] + t*t/p2*(x2*fC[9]+y2*fC[14]+z2*fC[20]
        + float32_v(2.f)*(x*y*fC[13]+x*z*fC[18]+y*z*fC[19]) )
        + float32_v(2.f)*t*(x*fC[31]+y*fC[32]+z*fC[33]);

  mask32_v mask = ((1.e-4f) < p2);
  error = select(mask, sqrt(abs(error)), error);
  error = select(!mask, BIG, error);
  return !mask;
}

mask32_v KFParticleSIMD::GetDecayLengthXY( float32_v &l, float32_v &error ) const 
{
  /** Calculates the projection in the XY plane of the decay length of the particle in the laboratory 
   ** system and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** The production point should be set before calling this function.
   ** \param[out] l - the decay length
   ** \param[out] error - its error
   **/
  
  const float32_v BIG = 1.e8f;
  float32_v x = fP[3];
  float32_v y = fP[4];
  float32_v t = fP[7];
  float32_v x2 = x*x;
  float32_v y2 = y*y;
  float32_v pt2 = x2+y2;
  l = t*sqrt(pt2);

  error = pt2*fC[35] + t*t/pt2*(x2*fC[9]+y2*fC[14] + 2*x*y*fC[13] )
        + float32_v(2.f)*t*(x*fC[31]+y*fC[32]);
  mask32_v mask = ((1.e-4f) < pt2);
  error = select(mask, sqrt(abs(error)), error);
  error = select(!mask, BIG, error);
  return !mask;
}

mask32_v KFParticleSIMD::GetLifeTime( float32_v &tauC, float32_v &error ) const 
{
  /** Calculates the lifetime times speed of life (ctau) [cm] of the particle in the  
   ** center of mass frame and its error. If they are well defined the corresponding element of the 
   ** return mask is set to 0, otherwise 1.
   ** The production point should be set before calling this function.
   ** \param[out] ctau - lifetime of the particle [cm]
   ** \param[out] error - its error
   **/
  
  const float32_v BIG = 1.e20f;

  float32_v m, dm;
  GetMass( m, dm );
  float32_v cTM = (-fP[3]*fC[31] - fP[4]*fC[32] - fP[5]*fC[33] + fP[6]*fC[34]);
  tauC = fP[7]*m;
  error = m*m*fC[35] + 2*fP[7]*cTM + fP[7]*fP[7]*dm*dm;
  mask32_v mask = (0.f < error);
  error = select(mask, sqrt(error), error);
  error = select(!mask, BIG, error);
  return !mask;
}

float32_v KFParticleSIMD::GetAngle  ( const KFParticleSIMD &p ) const 
{
  /** Returns the opening angle between the current and the second particle in 3D.
   ** \param[in] p - the second particle
   **/
  
  float32_v ds[2] = {0.f,0.f};
  float32_v dsdr[4][6];
  GetDStoParticle( p, ds, dsdr );   
  float32_v mP[8], mC[36], mP1[8], mC1[36];
  Transport( ds[0], dsdr[0], mP, mC ); 
  p.Transport( ds[1], dsdr[3], mP1, mC1 ); 
  float32_v n = sqrt( mP[3]*mP[3] + mP[4]*mP[4] + mP[5]*mP[5] );
  float32_v n1= sqrt( mP1[3]*mP1[3] + mP1[4]*mP1[4] + mP1[5]*mP1[5] );
  n*=n1;
  float32_v a(0.f);
  mask32_v mask = (n>(1.e-8f));
  a = select(mask, ( mP[3]*mP1[3] + mP[4]*mP1[4] + mP[5]*mP1[5] )/n, a);
  mask = ( abs(a) < float32_v(1.f));
  mask32_v aPos = (a>=float32_v(0.f));
  a = select(mask, KFPMath::acos(a), a);
  a = select((!mask) && aPos, 0.f, a);
  a = select((!mask) && (!aPos), 3.1415926535f, a);
  return a;
}

float32_v KFParticleSIMD::GetAngleXY( const KFParticleSIMD &p ) const 
{
  /** Returns the opening angle between the current and the second particle in the XY plane.
   ** \param[in] p - the second particle
   **/
  
  float32_v ds[2] = {0.f,0.f};
  float32_v dsdr[4][6];
  GetDStoParticle( p, ds, dsdr );   
  float32_v mP[8], mC[36], mP1[8], mC1[36];
  Transport( ds[0], dsdr[0], mP, mC ); 
  p.Transport( ds[1], dsdr[3], mP1, mC1 ); 
  float32_v n = sqrt( mP[3]*mP[3] + mP[4]*mP[4] );
  float32_v n1= sqrt( mP1[3]*mP1[3] + mP1[4]*mP1[4] );
  n*=n1;
  float32_v a(0.f);
  mask32_v mask = (n>(1.e-8f));
  a = ( mP[3]*mP1[3] + mP[4]*mP1[4] )/n;
  a = select(!mask, 0.f, a);
  mask = ( abs(a) < float32_v(1.f));
  mask32_v aPos = (a>=float32_v(0.f));
  a = select(mask, KFPMath::acos(a), a);
  a = select((!mask) && aPos, 0.f, a);
  a = select((!mask) && (!aPos), 3.1415926535f, a);
  return a;
}

float32_v KFParticleSIMD::GetAngleRZ( const KFParticleSIMD &p ) const 
{
  /** Returns the opening angle between the current and the second particle in the RZ plane, R = sqrt(X*X+Y*Y).
   ** \param[in] p - the second particle
   **/
  
  float32_v ds[2] = {0.f,0.f};
  float32_v dsdr[4][6];
  GetDStoParticle( p, ds, dsdr );   
  float32_v mP[8], mC[36], mP1[8], mC1[36];
  Transport( ds[0], dsdr[0], mP, mC ); 
  p.Transport( ds[1], dsdr[3], mP1, mC1 );  
  float32_v nr = sqrt( mP[3]*mP[3] + mP[4]*mP[4] );
  float32_v n1r= sqrt( mP1[3]*mP1[3] + mP1[4]*mP1[4]  );
  float32_v n = sqrt( nr*nr + mP[5]*mP[5] );
  float32_v n1= sqrt( n1r*n1r + mP1[5]*mP1[5] );
  n*=n1;
  float32_v a(0.f);
  mask32_v mask = (n>(1.e-8f));
  a = select(mask, ( nr*n1r +mP[5]*mP1[5])/n, a);
  mask = ( abs(a) < float32_v(0.f));
  mask32_v aPos = (a>=float32_v(0.f));
  a = select(mask, KFPMath::acos(a), a);
  a = select((!mask) && aPos, 0.f, a);
  a = select((!mask) && (!aPos), 3.1415926535f, a);
  return a;
}

float32_v KFParticleSIMD::GetPseudoProperDecayTime( const KFParticleSIMD &pV, const float32_v& mass, float32_v* timeErr2 ) const
{ 
  /** Returns the Pseudo Proper Time of the decay = (r*pt) / |pt| * M/|pt|
   **
   ** \param[in] pV - the creation point of the particle
   ** \param[in] mass - the mass of the particle
   ** \param[out] timeErr2 - error of the returned value, if null pointer is provided - is not calculated
   **/
  
  const float32_v ipt2 = 1/( Px()*Px() + Py()*Py() );
  const float32_v mipt2 = mass*ipt2;
  const float32_v dx = X() - pV.X();
  const float32_v dy = Y() - pV.Y();

  if ( timeErr2 ) {
      // -- calculate error = sigma(f(r)) = f'Cf'
      // r = {x,y,px,py,x_pV,y_pV}
      // df/dr = { px*m/pt^2,
      //     py*m/pt^2,
      //    ( x - x_pV )*m*(1/pt^2 - 2(px/pt^2)^2),
      //    ( y - y_pV )*m*(1/pt^2 - 2(py/pt^2)^2),
      //     -px*m/pt^2,
      //     -py*m/pt^2 }
    const float32_v f0 = Px()*mipt2;
    const float32_v f1 = Py()*mipt2;
    const float32_v mipt2derivative = mipt2*(1-2*Px()*Px()*ipt2);
    const float32_v f2 = dx*mipt2derivative;
    const float32_v f3 = -dy*mipt2derivative;
    const float32_v f4 = -f0;
    const float32_v f5 = -f1;

    const float32_v& mC00 =    GetCovariance(0,0);
    const float32_v& mC10 =    GetCovariance(0,1);
    const float32_v& mC11 =    GetCovariance(1,1);
    const float32_v& mC20 =    GetCovariance(3,0);
    const float32_v& mC21 =    GetCovariance(3,1);
    const float32_v& mC22 =    GetCovariance(3,3);
    const float32_v& mC30 =    GetCovariance(4,0);
    const float32_v& mC31 =    GetCovariance(4,1);
    const float32_v& mC32 =    GetCovariance(4,3);
    const float32_v& mC33 =    GetCovariance(4,4);
    const float32_v& mC44 = pV.GetCovariance(0,0);
    const float32_v& mC54 = pV.GetCovariance(1,0);
    const float32_v& mC55 = pV.GetCovariance(1,1);

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
  /** Copies an entry "iPart" of the current vectorised particle to the scalar KFParticle object.
   ** \param[out] Part - an output scalar particle, where element "iPart" will be copied
   ** \param[in] iPart - index of the element to be copied to the scalar particle
   **/
  
  Part.SetId(static_cast<int>(Id()[iPart]));

  Part.CleanDaughtersId();
  for( unsigned int i = 0; i < DaughterIds().size(); i++ )
    Part.AddDaughterId(static_cast<int>(DaughterIds()[i][iPart]));

  Part.SetPDG( static_cast<int>(GetPDG()[iPart]) );

  for(int iP=0; iP<8; iP++)
    Part.Parameter(iP) = fP[iP][iPart];
  for(int iC=0; iC<36; iC++)
    Part.Covariance(iC) = fC[iC][iPart];

  Part.NDF() = static_cast<int>(GetNDF()[iPart]);
  Part.Chi2() = fChi2[iPart];
  Part.Q()    = fQ[iPart];
#ifdef NonhomogeneousField
  for(int iF=0; iF<10; iF++)
    Part.SetFieldCoeff(fField.fField[iF][iPart], iF);
#endif
}

void KFParticleSIMD::GetKFParticle(KFParticle* Part, int nPart)
{
  /** Copies "nPart" elements of the current vectorised particle to the array of scalar KFParticle objects.
   ** \param[out] Part - an output array of scalar particles
   ** \param[in] nPart - number of elements to be copied to the array of scalar objects
   **/
  
  for(int i=0; i<nPart; i++)
    GetKFParticle(Part[i],i);
}

void KFParticleSIMD::GetArmenterosPodolanski(KFParticleSIMD& positive, KFParticleSIMD& negative, float32_v QtAlfa[2] )
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

  float32_v alpha = 0.f, qt = 0.f;
  float32_v spx = positive.GetPx() + negative.GetPx();
  float32_v spy = positive.GetPy() + negative.GetPy();
  float32_v spz = positive.GetPz() + negative.GetPz();
  float32_v sp  = sqrt(spx*spx + spy*spy + spz*spz);
  mask32_v mask = mask32_v(  abs(sp) < float32_v(1.e-10f));
  float32_v pn, pp, pln, plp;

  pn = sqrt(negative.GetPx()*negative.GetPx() + negative.GetPy()*negative.GetPy() + negative.GetPz()*negative.GetPz());
  pp = sqrt(positive.GetPx()*positive.GetPx() + positive.GetPy()*positive.GetPy() + positive.GetPz()*positive.GetPz());
  pln  = (negative.GetPx()*spx+negative.GetPy()*spy+negative.GetPz()*spz)/sp;
  plp  = (positive.GetPx()*spx+positive.GetPy()*spy+positive.GetPz()*spz)/sp;

  mask = mask32_v(mask && mask32_v(  abs(pn) < float32_v(1.E-10f)));
  float32_v ptm  = (1.f-((pln/pn)*(pln/pn)));
  qt = select(ptm >= 0.f, pn*sqrt(ptm), qt);
  alpha = (plp-pln)/(plp+pln);

  QtAlfa[0] = select(mask, qt, QtAlfa[0]);
  QtAlfa[1] = select(mask, alpha, QtAlfa[1]);
}

void KFParticleSIMD::RotateXY(float32_v angle, float32_v Vtx[3])
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
  float32_v s, c;
  KFPMath::sincos(angle, s, c);
  
  float32_v mA[8][ 8];
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

  float32_v mAC[8][8];
  float32_v mAp[8];

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
      float32_v xx = 0.f;
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

void KFParticleSIMD::MultQSQt( const float32_v Q[], const float32_v S[], float32_v SOut[], const int kN )
{
  /** Matrix multiplication SOut = Q*S*Q^T, where Q - square matrix, S - symmetric matrix.
   ** \param[in] Q - square matrix
   ** \param[in] S - input symmetric matrix
   ** \param[out] SOut - output symmetric matrix
   ** \param[in] kN - dimensionality of the matrices
   **/
  
  float32_v* mA = new float32_v[kN*kN];
  
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
