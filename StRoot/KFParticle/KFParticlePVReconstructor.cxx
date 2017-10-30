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
      if(r2!=r2) continue;
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

    const float *rBest = fParticles[bestTrack].Parameters();
    const float *covBest = fParticles[bestTrack].CovarianceMatrix();

    float rVertex[3] = {0.f};
    float covVertex[6] = {0.f};
    float weightVertex = 0.f;

    for(unsigned short int iTr = 0; iTr < nNotUsedTracks; iTr++)
    {
      unsigned short int &curTrack = (*notUsedTracksPtr)[iTr];

      if( ( fParticles[curTrack].GetDeviationFromVertex(rBest, covBest) < fChi2CutPreparation && fWeight[curTrack] > -1.f) || curTrack == bestTrack)
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
// #ifdef CBM
      if( primVtx.GetNDF() >= cutNDF && ((cluster.fTracks.size()>0.1f*fNParticles && fNParticles > 30) || fNParticles<=30 ) ) //at least 2 particles
// #else
//       if( primVtx.GetNDF() >= cutNDF)
// #endif
      {
//         std::cout << primVtx.X() << " " << primVtx.Y() << " " << primVtx.Z() << " " << cluster.fTracks.size() << std::endl;
        fPrimVertices.push_back(primVtx);
        fClusters.push_back(cluster);
      }
      
      if(vFlags) delete [] vFlags;
    }
    if(pParticles) delete [] pParticles;
  }
//   if(fClusters.size()>1)
//   {
//     for(int i=1; i<fClusters.size(); i++)
//     {
//       float dx = fClusters[0].fP[0] - fClusters[i].fP[0];
//       float dy = fClusters[0].fP[1] - fClusters[i].fP[1];
//       float dz = fClusters[0].fP[2] - fClusters[i].fP[2];
// 
//       float dr[3] = {dx, dy, dz};
//       float cov[6] = {fClusters[0].fC[0] + fClusters[i].fC[0],
//                       fClusters[0].fC[1] + fClusters[i].fC[1],
//                       fClusters[0].fC[2] + fClusters[i].fC[2],
//                       fClusters[0].fC[3] + fClusters[i].fC[3],
//                       fClusters[0].fC[4] + fClusters[i].fC[4],
//                       fClusters[0].fC[5] + fClusters[i].fC[5] };
//       float dr2 = dr[0]*dr[0] + dr[1]*dr[1] + dr[2]*dr[2];
//       float drError2 = dr[0]* (cov[0]* dr[0] + cov[1]* dr[1] + cov[3]* dr[2]) +
//                        dr[1]* (cov[1]* dr[0] + cov[2]* dr[1] + cov[4]* dr[2]) + 
//                        dr[2]* (cov[3]* dr[0] + cov[4]* dr[1] + cov[5]* dr[2]);
//       drError2 /= dr2;
//     
//       std::cout << "Ntr 1 " << fClusters[0].fTracks.size() << " ntr2  " << fClusters[i].fTracks.size() << std::endl;
//       std::cout << "dr2 " << dr2 << " err " << drError2 << " chi " << sqrt(dr2/drError2) << std::endl;
//     }
//     int ui;
//     std::cin >> ui;
//   }
//   static int nVert[10]={0.};
//   if(fClusters.size()<10)
//     nVert[fClusters.size()]++;
//   std::cout << "N Vert     ";
//   for(int i=0; i<10; i++)
//     std::cout << i << ": " << nVert[i] << "     ";
//   std::cout << std::endl;
// 
//   static int nPart[10]={0.};
//   if(fNParticles<10)
//     nPart[fNParticles]++;
//   std::cout << "N Part     ";
//   for(int i=0; i<10; i++)
//     std::cout << i << ": " << nPart[i] << "     ";
//   std::cout << std::endl;
}

void KFParticlePVReconstructor::ReconstructPrimVertex()
{

  FindPrimaryClusters();

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