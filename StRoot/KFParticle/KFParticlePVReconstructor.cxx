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

#include "KFParticlePVReconstructor.h"
#include "KFPTrackVector.h"
#include "KFParticle.h"

#include <iostream>


void KFParticlePVReconstructor::Init(KFPTrackVector *tracks, int nParticles)
{
    // copy tracks in particles.
  fNParticles = nParticles;
  fParticles.resize(fNParticles);
  fWeight.resize(fNParticles);
  
  KFPTrack track;
  for ( int iTr = 0; iTr < fNParticles; iTr++ ) {
    // {   // check cov matrix TODO
    //   bool ok = true;  
    //   int k = 0;
    //   for (int i = 0; i < 6; i++) {
    //     for (int j = 0; j <= i; j++, k++) {
    //       ok &= finite( KFPCov[k] );
    //     }
    //     ok &= ( KFPCov[k-1] > 0 );
    //   }
    //   if (!ok) continue;
    // }
    tracks->GetTrack(track,iTr);
    fParticles[iTr] = KFParticle( track, 211 ); // pi+ // TODO include PDG in KFPTrack?
    fParticles[iTr].AddDaughterId(track.Id());
  }
  
  fPrimVertices.clear();
  fClusters.clear();
  
  float pvEstimation[3] = {0.};
  float pvEstimationTr[3] = {0.};

  float parTmp[8] = {0.};
  float covTmp[36] = {0.};
  
  for(int iIter=0; iIter<3; iIter++)
  {
    float C[3] = {100.,100.,100.};
    for ( int iTr = 0; iTr < fNParticles; iTr++ ) {
      
      float ds = 0.f;
      float dsdr[6] = {0.f};
      if(iIter>0)
        ds = fParticles[iTr].GetDStoPoint(pvEstimationTr, dsdr);
      fParticles[iTr].Transport( ds, dsdr, parTmp, covTmp);

      float r2 = parTmp[0]*parTmp[0] + parTmp[1]*parTmp[1];
      //      if(r2!=r2) continue;
      if(r2 > 100 ) continue;  
      
      const float V[3] = {covTmp[0], covTmp[2], covTmp[5]}; 
      for(int iComp=0; iComp<3; iComp++)
      {
        float K = C[iComp]/(C[iComp]+V[iComp]);
        if (fabs(V[iComp]) < 1.e-8) continue;
        if(C[iComp] > 16*V[iComp])
          K = 1.f - V[iComp]/C[iComp];
        const float dzeta = parTmp[iComp]-pvEstimation[iComp];
        pvEstimation[iComp] += K*dzeta;
        C[iComp] -= K*C[iComp];
      }
    }
    pvEstimationTr[0] = pvEstimation[0];
    pvEstimationTr[1] = pvEstimation[1];
    pvEstimationTr[2] = pvEstimation[2];
  }

  {
    int nPrimCand = fParticles.size();

    const KFParticle **pParticles = new const KFParticle*[nPrimCand+1]; // tmp array
    bool *vFlags = new bool[nPrimCand];  // flags returned by the vertex finder

    KFVertex primVtx;
//     primVtx.SetVtxGuess(pvEstimation[0], pvEstimation[1], pvEstimation[2]);
    primVtx.X() = pvEstimation[0];
    primVtx.Y() = pvEstimation[1];
    primVtx.Z() = pvEstimation[2];
    
    nPrimCand = 0;
    for ( unsigned int iP = 0; iP < fParticles.size(); iP++ )
    {
      const KFParticle &p = fParticles[iP];
      float chi = p.GetDeviationFromVertex( primVtx );      
       
      bool isBadDaughter=0;
      for(int iParam=0; iParam<8; iParam++)
        isBadDaughter|= p.GetParameter(iParam) != p.GetParameter(iParam);
      for(int iC=0; iC<36; iC++)
        isBadDaughter|= p.GetCovariance(iC) != p.GetCovariance(iC);

      if( chi >= fChi2CutPreparation || isBadDaughter)
         continue;
      
      pParticles[nPrimCand] = &fParticles[iP];
      vFlags[nPrimCand] = 1;
      nPrimCand++;
    }
  
    primVtx.SetConstructMethod(0);
    primVtx.ConstructPrimaryVertex( pParticles, nPrimCand, vFlags, fChi2CutPreparation );
      
    delete [] pParticles;
    delete [] vFlags;
    pvEstimation[0] = primVtx.GetX();
    pvEstimation[1] = primVtx.GetY();
    pvEstimation[2] = primVtx.GetZ();
  }

  for(int iP=0; iP<fNParticles; iP++)
  {
//     fParticles[iP].TransportToPoint(pvEstimation);

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
  // The function finds a set of clusters of tracks.
  // Tracks are assumed to be transported to the beam line.
  // If a beam line is set - it will be used for a reconstruction but will not be added to a daughter cluster

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

    KFVertex primVtx;
    primVtx.SetConstructMethod(0);
    
    primVtx += fParticles[bestTrack];
    cluster.fTracks.push_back(bestTrack);
    
    for(unsigned short int iTr = 0; iTr < nNotUsedTracks; iTr++)
    {
      unsigned short int &curTrack = (*notUsedTracksPtr)[iTr];
      if(curTrack == bestTrack) continue;
      
      KFVertex tmpVtx = primVtx;
      tmpVtx += fParticles[curTrack];   
      
      if( ( fParticles[curTrack].GetDeviationFromVertex( fParticles[bestTrack] ) < fChi2Cut && (tmpVtx.Chi2()/tmpVtx.NDF() < 5) && fWeight[curTrack] > -1.f && fParticles[curTrack].GetP() > 0.5) )
      {
//         std::cout.precision(5);
//         std::cout << " curTrack " << curTrack <<"  track " << fParticles[curTrack].DaughterIds()[0] << "  " << fParticles[curTrack].X() << "  " << fParticles[curTrack].Y() << " " << fParticles[curTrack].Z() << std::endl;
//         std::cout << "        verex: r  " << primVtx.GetX() << " " << primVtx.GetY() << " " << primVtx.GetZ() << "      err " <<   primVtx.GetCovariance(0) << " " << primVtx.GetCovariance(2) << " " << primVtx.GetCovariance(5) << "   Chi2 " << primVtx.GetChi2()/primVtx.NDF() << std::endl;
        primVtx = tmpVtx;
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

    if(fNParticles>1)
    {
// #ifdef CBM
//       if( primVtx.GetNDF() >= cutNDF && ((cluster.fTracks.size()>0.1f*fNParticles && fNParticles > 30) || fNParticles<=30 ) ) //at least 2 particles
// #else
      if( primVtx.GetNDF() >= cutNDF)
// #endif
      {
        KFParticleCluster clusterEmpty;
        fPrimVertices.push_back(primVtx);
        fClusters.push_back(clusterEmpty);
      }
    }    
  }
}

void FilterPV(KFVertex& v1, KFVertex& v2, KFVertex& v)
{
  float* r1 = v1.Parameters();
  float* C1 = v1.CovarianceMatrix();

  float* r2 = v2.Parameters();
  float* C2 = v2.CovarianceMatrix();

  float* r  = v.Parameters();
  float* C  = v.CovarianceMatrix();
  
  float S[6] = { C1[0]+C2[0],
                 C1[1]+C2[1], C1[2]+C2[2],
                 C1[3]+C2[3], C1[4]+C2[4], C1[5]+C2[5] };
                  
  KFParticle::InvertCholetsky3(S);
  
  float K[3][3] = { {C2[0]*S[0]+C2[1]*S[1]+C2[3]*S[3], C2[0]*S[1]+C2[1]*S[2]+C2[3]*S[3], C2[0]*S[3]+C2[1]*S[4]+C2[3]*S[5]},
                    {C2[1]*S[0]+C2[2]*S[1]+C2[4]*S[3], C2[1]*S[1]+C2[2]*S[2]+C2[4]*S[3], C2[1]*S[3]+C2[2]*S[4]+C2[4]*S[5]},
                    {C2[3]*S[0]+C2[4]*S[1]+C2[5]*S[3], C2[3]*S[1]+C2[4]*S[2]+C2[5]*S[3], C2[3]*S[3]+C2[4]*S[4]+C2[5]*S[5]} };
                    
  float dr[3] = { r1[0] - r2[0], r1[1] - r2[1], r1[2] - r2[2] };
  
  for(int i=0; i<3; i++)
    r[i] = r2[i] + (K[i][0]*dr[0] + K[i][1]*dr[1] + K[i][2]*dr[2]);

  C[0] = C2[0] - (K[0][0]*C2[0]+K[0][1]*C2[1]+K[0][2]*C2[3]);
  C[1] = C2[1] - (K[1][0]*C2[0]+K[1][1]*C2[1]+K[1][2]*C2[3]);
  C[2] = C2[2] - (K[1][0]*C2[1]+K[1][1]*C2[2]+K[1][2]*C2[4]);
  C[3] = C2[3] - (K[2][0]*C2[0]+K[2][1]*C2[1]+K[2][2]*C2[3]);
  C[4] = C2[4] - (K[2][0]*C2[1]+K[2][1]*C2[2]+K[2][2]*C2[4]);
  C[5] = C2[5] - (K[2][0]*C2[3]+K[2][1]*C2[4]+K[2][2]*C2[5]);
  
  v.Chi2() = (S[0]*dr[0] + S[1]*dr[1] + S[3]*dr[2])*dr[0]
           + (S[1]*dr[0] + S[2]*dr[1] + S[4]*dr[2])*dr[1]
           + (S[3]*dr[0] + S[4]*dr[1] + S[5]*dr[2])*dr[2];              
}

void KFParticlePVReconstructor::FindBestPV()
{
  int nIters = 3;
  for(int iIter=0; iIter<nIters; iIter++)
  {
//     for(unsigned int iPV=0; iPV<fPrimVertices.size(); iPV++)
//       for(unsigned int jPV=iPV+1; jPV<fPrimVertices.size(); jPV++)
//       {
//         KFVertex newVertex;
//         FilterPV(fPrimVertices[iPV], fPrimVertices[jPV], newVertex);
//         if(newVertex.Chi2() < 60)
//         {
//           fPrimVertices.erase(fPrimVertices.begin() + jPV);
//           fClusters.erase(fClusters.begin() + jPV);
//           
//           fPrimVertices[iPV] = newVertex;
//         }
//       }
      
      
    for(unsigned int iCluster=0; iCluster<fClusters.size(); iCluster++)
      fClusters[iCluster].fTracks.clear();
      
    vector<KFVertex> tmpPrimVertex(fPrimVertices.size());
    
    for(int iParticle=0; iParticle<fNParticles; iParticle++)
    {
      if(fWeight[iParticle] < 0.f) continue;
      if(fParticles[iParticle].GetP() < 0.5) continue;
        
      float minDeviation = fChi2Cut;
      int minPV = -1;
      for(unsigned int iPV=0; iPV<fPrimVertices.size(); iPV++)
      {
        float deviation = fParticles[iParticle].GetDeviationFromVertex(fPrimVertices[iPV]);
        if(deviation < 0.f) continue;
        
        if(deviation < minDeviation)
        {
          minDeviation = deviation;
          minPV = iPV;
        }
      }
      
      if(minPV > -1)
      {
        tmpPrimVertex[minPV] += fParticles[iParticle];
        fClusters[minPV].fTracks.push_back(iParticle);
      }
    }
    
    for(unsigned int iPV=0; iPV<fPrimVertices.size(); iPV++)
    {
      if( tmpPrimVertex[iPV].GetNDF() >= 1 && ((fClusters[iPV].fTracks.size()>0.1f*fNParticles && fNParticles > 30) || fNParticles<=30 ) )
        fPrimVertices[iPV] = tmpPrimVertex[iPV];
      else
      {
        fPrimVertices.erase(fPrimVertices.begin() + iPV);
        fClusters.erase(fClusters.begin() + iPV);
      }
    }
  }
}

void KFParticlePVReconstructor::ReconstructPrimVertex()
{

  FindPrimaryClusters();
  FindBestPV();

  if ( fPrimVertices.size() == 0 ) { // fill prim vertex by dummy values
    
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
} // void KFParticlePVReconstructor::Run()

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
