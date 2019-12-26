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

#include "KFParticlePVReconstructor.h"
#include "KFPTrackVector.h"
#include "KFParticle.h"

using std::vector;

void KFParticlePVReconstructor::Init(KFPTrackVector *tracks, int nParticles)
{
  /** The function initialises an input for the search of primary vertices:\n
   ** 1) it receives as an input an array with tracks;\n
   ** 2) tracks are converted to KFParticle objects assuming pion mass;\n
   ** 3) the position of the primary vertex is estimated with simplified
   ** Kalman filter equations using all tracks; \n
   ** 4) tracks are checked to deviate from the obtained estimation within
   ** KFParticlePVReconstructor::fChi2CutPreparation; \n
   ** 5) from the selected tracks a more precise estimation is obtained
   ** using KFVertex::ConstructPrimaryVertex() with soft cut 
   ** KFParticlePVReconstructor::fChi2CutPreparation; \n
   ** 6) input particles are transported to the DCA point with the obtained
   ** estimation;\n
   ** 7) the weight for each particle is calculated according to its errors,
   ** if errors are not defined after extrapolation or if particle 10 cm
   ** away from the {0,0,0} point the weight of -100 is assigned.
   ** \param[in] tracks - a pointer to the KFPTrackVector with input tracks
   ** \param[in] nParticles - number of the input tracks
   **/
  
  fNParticles = nParticles;
  fParticles.resize(fNParticles);
  fWeight.resize(fNParticles);
  
  float C[3] = {0,0,0};
  int nC[3] = {0,0,0};
  
  KFPTrack track;
  for ( int iTr = 0; iTr < fNParticles; iTr++ ) {
    tracks->GetTrack(track,iTr);
    fParticles[iTr] = KFParticle( track, 211 );
    fParticles[iTr].AddDaughterId(track.Id());
    float zeroPoint[3]{0,0,0};
    fParticles[iTr].TransportToPoint(zeroPoint);
    
    for(int iC=0; iC<3; iC++)
    {
      if(!(fParticles[0].Covariance(iC,iC)==fParticles[0].Covariance(iC,iC))) continue; 
      if(fParticles[0].Covariance(iC,iC) < 10.f && fParticles[0].Covariance(iC,iC) > 0.f )
      {
        C[iC] += fParticles[0].Covariance(iC,iC);
        nC[iC]++;
      }
    }
  }
  
  fPrimVertices.clear();
  fClusters.clear();
  
  float pvEstimation[3] = {0.};
  float pvEstimationTr[3] = {0.};

  float parTmp[8] = {0.};
  float covTmp[36] = {0.};
  
  for(int iC=0; iC<3; iC++)
  {
    if(nC[iC] >0)
      C[iC] /= nC[iC];
    else
      C[iC] = 1.e-2;
  }
  
  for(int iIter=0; iIter<3; iIter++)
  {
    C[0]*=100.f; C[1]*=100.f; C[2]*=100.f;
    for ( int iTr = 0; iTr < fNParticles; iTr++ ) {
      float ds = 0.f;
      float dsdr[6] = {0.f};
      if(iIter>0)
        ds = fParticles[iTr].GetDStoPoint(pvEstimationTr, dsdr);
      fParticles[iTr].Transport( ds, dsdr, parTmp, covTmp);

      float r2 = parTmp[0]*parTmp[0] + parTmp[1]*parTmp[1];
      if(!(r2==r2)) continue;
      if(r2 > 25 ) continue;  
      
      const float V[3] = {covTmp[0], covTmp[2], covTmp[5]}; 
      
      for(int iComp=0; iComp<3; iComp++)
      {
        float K = C[iComp]/(C[iComp]+V[iComp]);
        if (fabs(V[iComp]) < 1.e-8) continue;
        if(C[iComp] > 16*V[iComp])
          K = 1.f - V[iComp]/C[iComp];
        const float dzeta = parTmp[iComp]-pvEstimation[iComp];
        if(K!=K) continue;
        if(K<0. || K>0.999) continue;
        pvEstimation[iComp] += K*dzeta;
        C[iComp] -= K*C[iComp];
      }
    }
    pvEstimationTr[0] = pvEstimation[0];
    pvEstimationTr[1] = pvEstimation[1];
    pvEstimationTr[2] = pvEstimation[2];
  }

  for(int iP=0; iP<fNParticles; iP++)
  {
    fParticles[iP].TransportToPoint(pvEstimation);

    fWeight[iP] = fParticles[iP].CovarianceMatrix()[0]
      + fParticles[iP].CovarianceMatrix()[2] + fParticles[iP].CovarianceMatrix()[5];
    
    if(fWeight[iP] > 0.f)
      fWeight[iP] = 1.f/sqrt(fWeight[iP]);
    else
      fWeight[iP] = -100;
    
    if( (fParticles[iP].X()*fParticles[iP].X() + fParticles[iP].Y()*fParticles[iP].Y()) > 100.f ) fWeight[iP] = -100.f;
  }
} // void KFParticlePVReconstructor::Init

void KFParticlePVReconstructor::FindPrimaryClusters( int cutNDF )
{
  /** The functions searches for a set of clusters of particles - candidates for the primary
   ** vertex:\n
   ** 1) input particles are assumed to be transported to the beam line or target position;\n
   ** 2) at first, the best particle with the highest weight is selected;\n
   ** 3) then a cluster is formed around this particle;\n
   ** 4) if a beam line is set it is used for the reconstruction as an additional track,
   ** but will not be added to the resulting cluster of daughter particles;\n
   ** 5) the primary vertex candidate is fitted with KFVertex::ConstructPrimaryVertex()
   ** using KFParticlePVReconstructor::fChi2Cut;\n
   ** 6) cluster is cleaned from particles deviating more then the fChi2Cut from the fitted
   ** candidate;\n
   ** 7) the cluster and the vertex candidate are stored if they satisfy the provided cutNDF;\n
   ** 8) the procedure is repeated until not used tracks with well-defined weight are left.
   ** 
   ** \param[in] cutNDF - cut on the number of degrees of freedom (effectively - number of
   ** particles used for the reconstruction), if resulting NDF is smaller then this cut - 
   ** the PV-candidate is rejected
   **/

  if( IsBeamLine() )
    cutNDF += 2;
  
  vector<unsigned short int> notUsedTracks(fNParticles);
  vector<unsigned short int> *notUsedTracksPtr = &notUsedTracks;
  int nNotUsedTracks = fNParticles;

  vector<unsigned short int> notUsedTracksNew(fNParticles);
  vector<unsigned short int> *notUsedTracksNewPtr = &notUsedTracksNew;
  int nNotUsedTracksNew = 0;

  for(int iTr=0; iTr<fNParticles; iTr++)
    (*notUsedTracksPtr)[iTr] = iTr;
    
  while(nNotUsedTracks>0)
  {
    short int bestTrack = 0;
    float bestWeight = -1.f;

    for(unsigned short int iTr = 0; iTr < nNotUsedTracks; iTr++)
    {
      unsigned short int &curTrack = (*notUsedTracksPtr)[iTr];
      
      if (fWeight[curTrack] > bestWeight)
      {
        bestWeight = fWeight[curTrack];
        bestTrack = curTrack;
      }
    }

    if(bestWeight < 0.f) break;
    
    KFParticleCluster cluster;
    cluster.fTracks.reserve(nNotUsedTracks);

    const float *rBest = fParticles[bestTrack].Parameters();
    const float *covBest = fParticles[bestTrack].CovarianceMatrix();

    float rVertex[3] = {0.f};
    float covVertex[6] = {0.f};
    float weightVertex = 0.f;

    for(unsigned short int iTr = 0; iTr < nNotUsedTracks; iTr++)
    {
      unsigned short int &curTrack = (*notUsedTracksPtr)[iTr];
      float chi2deviation = fParticles[curTrack].GetDeviationFromVertex(rBest, covBest);
      if( ( chi2deviation < fChi2CutPreparation && chi2deviation >= 0 && fWeight[curTrack] > -1.f) || curTrack == bestTrack)
      {
        for(int iP=0; iP<3; iP++)
          rVertex[iP] += fWeight[curTrack] * fParticles[curTrack].Parameters()[iP];
        
        float weight2 = fWeight[curTrack] * fWeight[curTrack];
        for(int iC=0; iC<6; iC++)
          covVertex[iC] += weight2 *fParticles[curTrack].CovarianceMatrix()[iC];

        weightVertex += fWeight[curTrack];
        cluster.fTracks.push_back(curTrack);
      }
      else
      {
        (*notUsedTracksNewPtr)[nNotUsedTracksNew] = curTrack;
        nNotUsedTracksNew++;
      }
    }
    
    vector<unsigned short int> *notUsedTracksPtrSave = notUsedTracksPtr;
    notUsedTracksPtr = notUsedTracksNewPtr;
    notUsedTracksNewPtr = notUsedTracksPtrSave;
    
    nNotUsedTracks = nNotUsedTracksNew;
    nNotUsedTracksNew = 0;

    if(cluster.fTracks.size() < 2) continue;
    if((cluster.fTracks.size() < 3) && (fNParticles>3)) continue;

    for(int iP=0; iP<3; iP++)
      cluster.fP[iP] = rVertex[iP]/weightVertex;

    for(int iC=0; iC<6; iC++)
      cluster.fC[iC] = covVertex[iC]/(weightVertex*weightVertex);


    int nPrimCand = cluster.fTracks.size(); // 1 is reserved for a beam line
    int nTracks = cluster.fTracks.size();
    
    const KFParticle **pParticles = new const KFParticle*[nPrimCand+1]; // tmp array

    for ( int iP = 0; iP < nPrimCand; iP++ )
      pParticles[iP] = &fParticles[ cluster.fTracks[iP] ];

    if( IsBeamLine() )
    {
      pParticles[nPrimCand] = &fBeamLine;
      nPrimCand++;
    }
      
      
      // find prim vertex
    KFVertex primVtx;

    if(fNParticles>1)
    {
      // construct PV candidate from a cluster
      bool *vFlags = new bool[nPrimCand];  // flags returned by the vertex finder
      for(int iFl=0; iFl<nPrimCand; iFl++)
        vFlags[iFl] = true;
//       primVtx.SetVtxGuess(cluster.fP[0], cluster.fP[1], cluster.fP[2]);
      primVtx.SetConstructMethod(0);
      primVtx.ConstructPrimaryVertex( pParticles, nPrimCand, vFlags, fChi2Cut );

      // clean cluster
      vector<int> clearClusterInd;
      clearClusterInd.reserve(cluster.fTracks.size());
      for ( int iP = 0; iP < nTracks; iP++ ){
        if(cluster.fTracks[iP] == bestTrack) {
          clearClusterInd.push_back(cluster.fTracks[iP]);
          continue;
        }
        if(vFlags[iP])
          clearClusterInd.push_back(cluster.fTracks[iP]);
        else
        {
          (*notUsedTracksPtr)[nNotUsedTracks] = cluster.fTracks[iP];
          nNotUsedTracks++;
        }
      }
      
      for(unsigned short int iTr = 0; iTr < nNotUsedTracks; iTr++)
      {
        unsigned short int &curTrack = (*notUsedTracksPtr)[iTr];
        if( fParticles[curTrack].GetDeviationFromVertex(primVtx)<fChi2Cut )
        {
          primVtx += fParticles[curTrack];
          clearClusterInd.push_back(curTrack);
        }
        else
        {
          (*notUsedTracksNewPtr)[nNotUsedTracksNew] = curTrack;
          nNotUsedTracksNew++;
        }
      }      
      cluster.fTracks = clearClusterInd;

      notUsedTracksPtrSave = notUsedTracksPtr;
      notUsedTracksPtr = notUsedTracksNewPtr;
      notUsedTracksNewPtr = notUsedTracksPtrSave;
    
      nNotUsedTracks = nNotUsedTracksNew;
      nNotUsedTracksNew = 0;
    
      // save PV
#ifdef CBM
      if( primVtx.GetNDF() >= cutNDF && ((cluster.fTracks.size()>0.1f*fNParticles && fNParticles > 30) || fNParticles<=30 ) ) //at least 2 particles
#else
      if( primVtx.GetNDF() >= cutNDF)
#endif
      {
        fPrimVertices.push_back(primVtx);
        fClusters.push_back(cluster);
      }
      
      if(vFlags) delete [] vFlags;
    }
    if(pParticles) delete [] pParticles;
  }
}

void KFParticlePVReconstructor::ReconstructPrimVertex()
{
  /** Reconstructs primary vertices and corresponding clusters of tracks.
   ** For this it calls KFParticlePVReconstructor::FindPrimaryClusters(),
   ** if no vertex is found empty primary vertex is used.
   **/
  
  FindPrimaryClusters();

  if ( fPrimVertices.size() == 0 )
  {
    float X=0,Y=0,Z=0;

    KFPVertex primVtx_tmp;
    primVtx_tmp.SetXYZ(X, Y, Z);
    primVtx_tmp.SetCovarianceMatrix( 0, 0, 0, 0, 0, 0 );
    primVtx_tmp.SetNContributors(0);
    primVtx_tmp.SetChi2(-100);

    fPrimVertices.push_back(primVtx_tmp);
    
    KFParticleCluster cluster;
    fClusters.push_back(cluster);
  }
}

void KFParticlePVReconstructor::AddPV(const KFVertex &pv, const vector<int> &tracks)
{ 
  fPrimVertices.push_back(pv);
  KFParticleCluster cluster;
  cluster.fTracks = tracks;
  fClusters.push_back(cluster);
}

void KFParticlePVReconstructor::AddPV(const KFVertex &pv)
{
  fPrimVertices.push_back(pv);
  KFParticleCluster cluster;
  fClusters.push_back(cluster);
}
