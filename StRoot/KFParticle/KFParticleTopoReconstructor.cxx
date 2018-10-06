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


#include "KFParticleTopoReconstructor.h"

#ifdef KFPWITHTRACKER
#include "AliHLTTPCCAGBTracker.h"
#endif

#include "KFParticleSIMD.h"
#include "KFParticleDatabase.h"

#include <fstream>
#include <iostream>
#include <stdio.h>
#include "string"
using std::string;
using std::ofstream;

#include "KFPInputData.h"

KFParticleTopoReconstructor::~KFParticleTopoReconstructor()
{
  if (fKFParticlePVReconstructor) delete fKFParticlePVReconstructor;
  if (fKFParticleFinder) delete fKFParticleFinder;
  if(fTracks) delete [] fTracks;
}

#ifdef HomogeneousField
void KFParticleTopoReconstructor::SetField(double b)
{
  KFParticle::SetField(b);
  KFParticleSIMD::SetField(float(b));
}
#endif

#ifdef KFPWITHTRACKER
void KFParticleTopoReconstructor::Init(AliHLTTPCCAGBTracker* tracker, vector<int>* pdg)
{
  if(!fTracks) 
    fTracks = new KFPTrackVector[NInputSets];

  fTracks[0].Resize(0);
  fTracks[1].Resize(0);
  fTracks[2].Resize(0);
  fTracks[3].Resize(0);
  fTracks[4].Resize(0);
  fTracks[5].Resize(0);
  fTracks[6].Resize(0);
  fTracks[7].Resize(0);
  
#ifdef USE_TIMERS
  timer.Start();
#endif // USE_TIMERS

  KFParticle::SetField( tracker->Slice(0).Param().Bz() ); // to understand -1 see SetField
  KFParticleSIMD::SetField( tracker->Slice(0).Param().Bz() ); // to understand -1 see SetField

    // create and fill array of tracks to init KFParticleTopoReconstructor
  const int nTracks = tracker->NTracks();
  fTracks[1].Resize( int(nTracks/float_vLen+1)*float_vLen );
  fTracks[5].Resize( int(nTracks/float_vLen+1)*float_vLen );
  fParticles.clear();
  int iOTr = 0; // index in out array
  
  float_v alpha(Vc::Zero);
  int nElements=0;
  
  for ( int iTr = 0; iTr < nTracks; iTr++ ) {
      // get track params in local CS
    
    bool ok = true;
    const int q = -(tracker->Tracks()[ iTr ].InnerParam().QPt()>=0 ? 1 : -1);
    
    for(int iParamSet=0; iParamSet<2; iParamSet++)
    {
      AliHLTTPCCATrackParam trParam;
      int arrayIndex = -1;
      if(iParamSet==0)
      {
        arrayIndex = 1;
        trParam = tracker->Tracks()[ iTr ].InnerParam();
      }
      if(iParamSet==1)
      {
        arrayIndex = 5;
        trParam = tracker->Tracks()[ iTr ].OuterParam();
      }
          
      const float x0 = 0;
      trParam.TransportToXWithMaterial( x0, tracker->Slice(0).Param().cBz( ) );

        // -- convert parameters
      fTracks[arrayIndex].SetParameter(trParam.X(), 0, iOTr); // X
      fTracks[arrayIndex].SetParameter(trParam.Y(), 1, iOTr); // Y
      fTracks[arrayIndex].SetParameter(trParam.Z(), 2, iOTr); // Z

      const float pt = CAMath::Abs( 1.f / trParam.QPt() );
//       const int q = -(trParam.QPt()>=0 ? 1 : -1);
  //    if ( pt < 1 ) continue; // dbg
      ok = ok && !( trParam.NDF() < 10+5); //if ( trParam.NDF() < 10+5 ) continue; // at least 15 hits in track
      ok = ok && !( trParam.Chi2() > 10*trParam.NDF() ); //if ( trParam.Chi2() > 10*trParam.NDF() ) continue; // dbg
  //    if ( iOTr >= 4 ) continue; // dbg
      
      const float cosL = trParam.DzDs();
      fTracks[arrayIndex].SetParameter(pt * trParam.GetCosPhi(), 3, iOTr); // Px
      fTracks[arrayIndex].SetParameter(pt * trParam.SinPhi()   , 4, iOTr); // Py
      fTracks[arrayIndex].SetParameter(pt * cosL               , 5, iOTr); // Pz
      
        // -- convert cov matrix
        // get jacobian
      float J[6][6];
      for (int i = 0; i < 6; i++)
        for (int j = 0; j < 6; j++)
          J[i][j] = 0;
      J[0][0] = 1; // x -> x
      J[1][1] = 1; // y -> y
      J[2][2] = 1; // z -> z
      J[3][3] = -pt * trParam.SinPhi() / trParam.GetCosPhi();
      J[3][5] = -q * pt * pt * trParam.GetCosPhi(); // q/pt -> px
      J[4][3] = pt; // sinPhi -> py
      J[4][5] = -q* pt * pt * trParam.SinPhi(); // q/pt -> py
      J[5][4] = pt; // dz/ds -> pz
      J[5][5] = -q* pt * pt * cosL; // q/pt -> pz

      float CovIn[6][6]; // triangular -> symmetric matrix
      {
        CovIn[0][0] = .001f*.001f; // dx. From nowhere. TODO
        for (int i = 1; i < 6; i++) {
          CovIn[i][0] = 0;
          CovIn[0][i] = 0;
        }
        int k = 0;
        for (int i = 1; i < 6; i++) {
          for (int j = 1; j <= i; j++, k++) {
            CovIn[i][j] = trParam.Cov()[k];
            CovIn[j][i] = trParam.Cov()[k];
          }
        }
      }
      
      float CovInJ[6][6];      // CovInJ = CovIn * J^t
      for (int i = 0; i < 6; i++)
        for (int j = 0; j < 6; j++) {
          CovInJ[i][j] = 0;
          for (int k = 0; k < 6; k++) {
            CovInJ[i][j] += CovIn[i][k] * J[j][k];
          }
        }
      
      float CovOut[6][6];      // CovOut = J * CovInJ
      for (int i = 0; i < 6; i++)
        for (int j = 0; j < 6; j++) {
          CovOut[i][j] = 0;
          for (int k = 0; k < 6; k++) {
            CovOut[i][j] += J[i][k] * CovInJ[k][j];
          }
        }

      float KFPCov[21]; // symmetric matrix -> triangular
      {
        int k = 0;
        for (int i = 0; i < 6; i++) {
          for (int j = 0; j <= i; j++, k++) {
            KFPCov[k] = CovOut[i][j];
            ASSERT( !CAMath::Finite(CovOut[i][j]) ||  CovOut[i][j] == 0 || fabs( 1. - CovOut[j][i]/CovOut[i][j] ) <= 0.05,
              "CovOut[" << i << "][" << j << "] == CovOut[" << j << "][" << i << "] : " << CovOut[i][j] << " == " << CovOut[j][i]);
          }
        }
      }
      
      if(iParamSet == 0)
      {   // check cov matrix
        int k = 0;
        for (int i = 0; i < 6; i++) {
          for (int j = 0; j <= i; j++, k++) {
            ok &= CAMath::Finite( KFPCov[k] );
          }
          ok &= ( KFPCov[k-1] > 0 );
        }
      }
      
      if(ok)
      {
        int trackPDG = -1;  
        if(pdg)
          trackPDG = (*pdg)[iTr];
      
        for(int iC=0; iC<21; iC++)
          fTracks[arrayIndex].SetCovariance( KFPCov[iC], iC, iOTr);
        fTracks[arrayIndex].SetId(iTr, iOTr);
        fTracks[arrayIndex].SetPDG(trackPDG, iOTr);
        fTracks[arrayIndex].SetQ(q, iOTr);
        fTracks[arrayIndex].SetPVIndex(-1, iOTr);
      }
    }
    if (!ok) continue;
    
    iOTr++;
    
    // convert into Global CS. Can't be done erlier because in tracker X hasn't correspondent covMatrix elements.
    alpha[nElements] = tracker->Tracks()[ iTr ].Alpha();
    nElements++;
    if(nElements == float_vLen)
    {
      fTracks[1].RotateXY( alpha, iOTr-nElements);
      fTracks[5].RotateXY( alpha, iOTr-nElements);
      nElements=0;
    }
  }
  if(nElements>0)
  {
    fTracks[1].RotateXY( alpha, iOTr-nElements);
    fTracks[5].RotateXY( alpha, iOTr-nElements);
  }
    
  fTracks[0].Resize(iOTr);
  fTracks[0].Set(fTracks[1],iOTr,0);

  fTracks[4].Resize(iOTr);
  fTracks[4].Set(fTracks[5],iOTr,0);
  
  fKFParticlePVReconstructor->Init( &fTracks[0], iOTr );
#ifdef USE_TIMERS
  timer.Stop();
  fStatTime[0] = timer.RealTime();
#endif /// USE_TIMERS
} // void KFParticleTopoReconstructor::Init(AliHLTTPCCAGBTracker* tracker)
#endif

void KFParticleTopoReconstructor::Init(vector<KFParticle> &particles, vector<int>* pdg, vector<int>* nPixelHits)
{
#ifdef USE_TIMERS
  timer.Start();
#endif // USE_TIMERS
  
  if(!fTracks) 
    fTracks = new KFPTrackVector[NInputSets];
  
  fParticles.clear();
  fPV.clear(); 

  int nTracks = particles.size();
  fTracks[0].Resize(nTracks);
  fTracks[1].Resize(0);
  fTracks[2].Resize(0);
  fTracks[3].Resize(0);
  fTracks[4].Resize(0);
  fTracks[5].Resize(0);
  fTracks[6].Resize(0);
  fTracks[7].Resize(0);
  
  for(int iTr=0; iTr<nTracks; iTr++)
  {  
    int trackPDG = -1;
    if(pdg)
      trackPDG = (*pdg)[iTr];
    
    int npixelhits = 0;
    if(nPixelHits)
      npixelhits = nPixelHits->at(iTr);
    
    for(int iP=0; iP<6; iP++)
      fTracks[0].SetParameter(particles[iTr].Parameters()[iP], iP, iTr);
    for(int iC=0; iC<21; iC++)
      fTracks[0].SetCovariance(particles[iTr].CovarianceMatrix()[iC], iC, iTr);
//     fTracks[0].SetId(iTr, iTr);
    fTracks[0].SetId(particles[iTr].Id(), iTr);
    fTracks[0].SetPDG(trackPDG, iTr);
    fTracks[0].SetQ(particles[iTr].Q(), iTr);
    fTracks[0].SetPVIndex(-1, iTr);
    fTracks[0].SetNPixelHits(npixelhits,iTr);
//     if (particles[iTr].GetParentID() > 0) fTracks[0].SetPVIndex(particles[iTr].GetParentID(), iTr);
  }
// #if 0  
  fKFParticlePVReconstructor->Init( &fTracks[0], nTracks );
// #endif
  
#ifdef USE_TIMERS
  timer.Stop();
  fStatTime[0] = timer.RealTime();
#endif /// USE_TIMERS
}

void KFParticleTopoReconstructor::Init(KFPTrackVector &tracks, KFPTrackVector &tracksAtLastPoint)
{
#ifdef USE_TIMERS
  timer.Start();
#endif // USE_TIMERS
  
  if(!fTracks) 
    fTracks = new KFPTrackVector[NInputSets];
  
  fParticles.clear();
  fPV.clear(); 
  
  int nTracks = tracks.Size();
  fTracks[0].Resize(nTracks);
  fTracks[0].Set(tracks, nTracks, 0);
  fTracks[1].Resize(0);
  fTracks[2].Resize(0);
  fTracks[3].Resize(0);
  fTracks[4].Resize(tracksAtLastPoint.Size());
  fTracks[4].Set(tracksAtLastPoint, tracksAtLastPoint.Size(), 0);
  fTracks[5].Resize(0);
  fTracks[6].Resize(0);
  fTracks[7].Resize(0);
  fKFParticlePVReconstructor->Init( &fTracks[0], nTracks );
  
#ifdef USE_TIMERS
  timer.Stop();
  fStatTime[0] = timer.RealTime();
#endif /// USE_TIMERS
}


void KFParticleTopoReconstructor::Init(const KFPTrackVector *particles, const vector<KFParticle>& pv)
{
#ifdef USE_TIMERS
  timer.Start();
#endif // USE_TIMERS
  fParticles.clear();
  fPV.clear(); 

  fTracks = const_cast< KFPTrackVector* >(particles);
  fChiToPrimVtx[0].resize(fTracks[0].Size());
  fChiToPrimVtx[1].resize(fTracks[1].Size());
  fPV.resize(pv.size());

  for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
    fPV[iPV] = KFParticleSIMD(const_cast<KFParticle&>(pv[iPV]));

#ifdef USE_TIMERS
  timer.Stop();
  fStatTime[0] = timer.RealTime();
#endif /// USE_TIMERS
}

void KFParticleTopoReconstructor::ReconstructPrimVertex(bool isHeavySystem)
{
#ifdef USE_TIMERS
  timer.Start();
#endif // USE_TIMERS
  fKFParticlePVReconstructor->ReconstructPrimVertex();
  
  fPV.clear(); 

  int nPrimVtx = NPrimaryVertices();
  int nPV = 0;
  if(isHeavySystem)
  {
    if(NPrimaryVertices() > 1)
    {
      unsigned int nMax = GetPVTrackIndexArray(0).size();
      for(int i=1; i<NPrimaryVertices(); i++)
        if(GetPVTrackIndexArray(i).size() > nMax)
        {
          nMax = GetPVTrackIndexArray(i).size();
          nPV = i;
        }
    }
  
    nPrimVtx = 1;
    fPV.resize(nPrimVtx);
    fPV[0] = GetPrimVertex(nPV);
  }
  else
  {
    fPV.resize(nPrimVtx);
    for(int iPV=0; iPV<nPrimVtx; iPV++)
      fPV[iPV] = GetPrimVertex(iPV);
  }
  
  for(int iPV=0; iPV<NPrimaryVertices(); iPV++)
  {
    int pvI = iPV;

    if( isHeavySystem )
    {
      if(iPV != nPV) continue;
      pvI = 0; //save only one PV
    }

    vector<int>& tracks = GetPVTrackIndexArray(iPV);
    for(unsigned int iTr=0; iTr<tracks.size(); iTr++)
      fTracks[0].SetPVIndex(pvI, tracks[iTr]);
  }
  
  if(isHeavySystem)
  {
    vector<int> pvTracks = fKFParticlePVReconstructor->GetPVTrackIndexArray(nPV);
    KFVertex pv = fKFParticlePVReconstructor->GetPrimKFVertex(nPV);
    fKFParticlePVReconstructor->CleanPV();
    fKFParticlePVReconstructor->AddPV(pv, pvTracks);
  }
  
#ifdef USE_TIMERS
  timer.Stop();
  fStatTime[1] = timer.RealTime();
#endif /// USE_TIMERS
} // void KFParticleTopoReconstructor::ReconstructPrimVertex

void KFParticleTopoReconstructor::SortTracks()
{
#ifdef USE_TIMERS
  timer.Start();
#endif // USE_TIMERS
  
  int offset[2] = {0, 4};
  int nSets = 2;
  
  if(fTracks[4].Size() == 0)
    nSets = 1;
  
  for(int iSet=nSets-1; iSet>=0; iSet--)
  {
    int Size = fTracks[0].Size();
    
    vector<KFPTrackIndex> sortedTracks(Size);
    kfvector_uint trackIndex[4];
    for(int iTV=0; iTV<4; iTV++)
      trackIndex[iTV].resize(Size);
    int nTracks[4] = {0,0,0,0};
    
    for(int iTr=0; iTr<Size; iTr++)
    {
      sortedTracks[iTr].fIndex = iTr;
      sortedTracks[iTr].fPdg = fTracks[0].PDG()[iTr];
    }
    
    std::sort(sortedTracks.begin(), sortedTracks.end(), KFPTrackIndex::Compare);
    
    for(int iTr=0; iTr<Size; iTr++)
    {
      int iTrSorted = sortedTracks[iTr].fIndex;
      
      //int q = fTracks[offset[iSet]].Q()[iTrSorted];
      int q = fTracks[0].Q()[iTrSorted]; //take the charge at the first point to avoid ambiguities in array size
      if(fTracks[0].PVIndex()[iTrSorted] < 0) //secondary track
      {

        if(q<0) //secondary negative track
        {
          trackIndex[1][nTracks[1]] = iTrSorted;
          nTracks[1]++;
        }
        else //secondary positive track
        {
          trackIndex[0][nTracks[0]] = iTrSorted;
          nTracks[0]++;
        }
      }
      else //primary track
      {
        if(q<0) //primary negative track
        {
          trackIndex[3][nTracks[3]] = iTrSorted;
          nTracks[3]++;
        }
        else //primary positive track
        {
          trackIndex[2][nTracks[2]] = iTrSorted;
          nTracks[2]++;
        }
      }
    }
    
    for(int iTV=1; iTV<4; iTV++)  
      fTracks[iTV+offset[iSet]].SetTracks(fTracks[offset[iSet]], trackIndex[iTV], nTracks[iTV]);
      
    KFPTrackVector positive;
    positive.SetTracks(fTracks[offset[iSet]], trackIndex[0], nTracks[0]);
    fTracks[offset[iSet]].Resize(nTracks[0]);
    fTracks[offset[iSet]].Set(positive,nTracks[0],0);
      
    for(int iTV=0; iTV<4; iTV++)
      fTracks[iTV+offset[iSet]].RecalculateLastIndex();
    
    //correct index of tracks in primary clusters with respect to the sorted array 
    if(iSet == 0)
    {
      vector<int> newIndex(Size);
      int iCurrentTrack=0;
      for(int iTC=0; iTC<4; iTC++)
      {
        for(int iTrackIndex=0; iTrackIndex<fTracks[iTC].Size(); iTrackIndex++)
        {
          newIndex[trackIndex[iTC][iTrackIndex]] = iCurrentTrack;
          iCurrentTrack++;
        }
      }
      
      for(int iPV=0; iPV<NPrimaryVertices(); iPV++)
        for(unsigned int iTrack=0; iTrack<GetPVTrackIndexArray(iPV).size(); iTrack++)
          fKFParticlePVReconstructor->GetPVTrackIndexArray(iPV)[iTrack] = newIndex[GetPVTrackIndexArray(iPV)[iTrack]];
    }
  }
  
  fChiToPrimVtx[0].resize(fTracks[0].Size(), -1);
  fChiToPrimVtx[1].resize(fTracks[1].Size(), -1);
  
#ifdef USE_TIMERS
  timer.Stop();
  fStatTime[2] = timer.RealTime();
#endif // USE_TIMERS
}

void KFParticleTopoReconstructor::TransportPVTracksToPrimVertex()
{
  float_v point[3];
  KFParticleSIMD tmpPart;
  
  for(int iTV=2; iTV<4; iTV++)
  {
    unsigned int NTr = fTracks[iTV].Size(); 
    for(unsigned int iTr=0; iTr < NTr; iTr += float_vLen) 
    { 
      const int_v& pdg = reinterpret_cast<const int_v&>(fTracks[iTV].PDG()[iTr]);
      const int_v& pvIndex = reinterpret_cast<const int_v&>(fTracks[iTV].PVIndex()[iTr]);
      
      tmpPart.Load(fTracks[iTV], iTr, pdg);
      
      for(unsigned int iV=0; iV < (unsigned int)float_vLen; iV++)
      {
        if(iV+iTr >= NTr) continue;
        
        int iPV = pvIndex[iV];
        point[0][iV] = fPV[iPV].X()[0];
        point[1][iV] = fPV[iPV].Y()[0];
        point[2][iV] = fPV[iPV].Z()[0];     
      }
      
      tmpPart.TransportToPoint(point);
      
      for(int iP=0; iP<6; iP++)
        fTracks[iTV].SetParameter( tmpPart.GetParameter(iP), iP, iTr );
      for(int iC=0; iC<21; iC++)
        fTracks[iTV].SetCovariance( tmpPart.GetCovariance(iC), iC, iTr ); 
    }
  }
}

void KFParticleTopoReconstructor::GetChiToPrimVertex(KFParticleSIMD* pv, const int nPV)
{ 
  KFParticleSIMD tmpPart; // for chi_prim calculation 
  
  for(int iTV=0; iTV<2; iTV++)
  {
    unsigned int NTr = fTracks[iTV].Size();
    for(unsigned int iTr=0; iTr < NTr; iTr += float_vLen) 
    { 
      uint_v trackIndex = iTr + uint_v::IndexesFromZero();
      const int_v& pdg = reinterpret_cast<const int_v&>(fTracks[iTV].PDG()[iTr]);
      tmpPart.Create(fTracks[iTV],trackIndex, pdg);
      
      float_v& chi2 = reinterpret_cast<float_v&>(fChiToPrimVtx[iTV][iTr]);
      chi2(float_m(trackIndex<NTr)) = 10000.f;

      for(int iPV=0; iPV<nPV; iPV++)
      {
        const float_v point[3] = {pv[iPV].X(), pv[iPV].Y(), pv[iPV].Z()};
        tmpPart.TransportToPoint(point);
        const float_v& chiVec = tmpPart.GetDeviationFromVertex(pv[iPV]);
        chi2( (chi2>chiVec) && float_m(trackIndex<NTr) ) = chiVec;
      }
    } 
  }
}

struct ParticleInfo
{
  ParticleInfo():fParticleIndex(-1),fMassDistance(1.e9f) {};
  ParticleInfo(int index, float massDistance):fParticleIndex(index),fMassDistance(massDistance) {};
  
  static bool compare(const ParticleInfo& a, const ParticleInfo& b) { return (a.fMassDistance < b.fMassDistance); }
  
  int   fParticleIndex;
  float fMassDistance;
};

bool UseParticleInCompetition(int PDG)
{
  bool use = (PDG == 310) ||         //K0
             (PDG == 22) ||          //gamma
             (PDG == 111) ||         //pi0
             (abs(PDG) == 3122) ||   //Lambda
             (abs(PDG) == 3312) ||   //Xi
             (abs(PDG) == 3334) ||   //Omega
             (abs(PDG) == 3003) ||   //LambdaN
             (abs(PDG) == 3103) ||   //LambdaNN
             (abs(PDG) == 3004) ||   //H3L
             (abs(PDG) == 3005) ||   //H4L
             (abs(PDG) == 3006) ||   //He4L
             (abs(PDG) == 3007) ||   //He5L
             (abs(PDG) == 3203) ||   //LLn
             (abs(PDG) == 3008) ||   //H4LL
             (abs(PDG) == 3009) ||   //H4LL
             (abs(PDG) == 3010) ||   //H5LL
             (abs(PDG) == 3011);     //He6LL
  return use;
}

void KFParticleTopoReconstructor::SelectParticleCandidates()
{
  std::vector<ParticleInfo> particleInfo;
  std::vector<bool> isUsed(fParticles.size());
  std::vector<bool> deleteCandidate(fParticles.size());
  std::vector<int>  bestMother(fParticles.size());
  
  for(unsigned int iParticle=0; iParticle<fParticles.size(); iParticle++)
  {
    isUsed[iParticle] = false;
    deleteCandidate[iParticle] = false; 
    bestMother[iParticle] = -1;
  }

  for(unsigned int iParticle=0; iParticle<fParticles.size(); iParticle++)
  {
    if(!UseParticleInCompetition(fParticles[iParticle].GetPDG())) continue;
    
    bool isSecondary = 1;
    for(int iPV=0; iPV<NPrimaryVertices(); iPV++)
    {
      KFParticle tmp = fParticles[iParticle];
      tmp.SetProductionVertex(GetPrimVertex(iPV));
      if(tmp.Chi2()/tmp.NDF()<5)
        isSecondary=0;
    }
    if(isSecondary)
      deleteCandidate[iParticle] = true;
  }

//   for(unsigned int iParticle=0; iParticle<fParticles.size(); iParticle++)
//   {
//     if(abs(fParticles[iParticle].GetPDG()) == 431 || abs(fParticles[iParticle].GetPDG()) == 4122)
//     {
//       vector<int> daughterIds(fParticles[iParticle].NDaughters());
//       for(int iDaughter=0; iDaughter<fParticles[iParticle].NDaughters(); iDaughter++)
//         daughterIds[iDaughter] = fParticles[fParticles[iParticle].DaughterIds()[iDaughter]].DaughterIds()[0];
//       std::sort(daughterIds.begin(), daughterIds.end());
//       
//       for(unsigned int jParticle=0; jParticle<fParticles.size(); jParticle++)
//       {
//         if(abs(fParticles[jParticle].GetPDG()) == 411)
//         {
//           vector<int> daughterIdsDPlus(fParticles[jParticle].NDaughters());
//           for(int iDaughter=0; iDaughter<fParticles[jParticle].NDaughters(); iDaughter++)
//             daughterIdsDPlus[iDaughter] = fParticles[fParticles[jParticle].DaughterIds()[iDaughter]].DaughterIds()[0];
//           std::sort(daughterIdsDPlus.begin(), daughterIdsDPlus.end());
//           
//           if(daughterIdsDPlus.size() != daughterIds.size()) continue;
//           
//           bool isSameParticle=1;
//           for(unsigned int iDaughter=0; iDaughter<daughterIds.size(); iDaughter++)
//             isSameParticle &= daughterIds[iDaughter] == daughterIdsDPlus[iDaughter];
//           if(!isSameParticle) continue; 
//       
//           float mass, massSigma;
//           fParticles[jParticle].GetMass(mass, massSigma);
//           
//           if(fabs(mass - KFParticleDatabase::Instance()->GetDPlusMass()) < 3*KFParticleDatabase::Instance()->GetDPlusMassSigma())
//           {
//             deleteCandidate[iParticle] = true;
//             break;
//           }
//         }
//       }
//     }
//   }
  
#if 0
  //clean K0 and Lambda
  for(unsigned int iParticle=0; iParticle<fParticles.size(); iParticle++)
  {
    if(deleteCandidate[iParticle]) continue;
    if(!UseParticleInCompetition(fParticles[iParticle].GetPDG())) continue;
    
    float mass, massSigma;
    fParticles[iParticle].GetMass(mass, massSigma);
    
    float massPDG, massPDGSigma;
    KFParticleDatabase::Instance()->GetMotherMass(fParticles[iParticle].GetPDG(), massPDG, massPDGSigma);
    
    float dm1 = fabs(mass - massPDG)/massPDGSigma;
//     if(dm1 > 3.f)  continue;
      
    for(unsigned int jParticle=iParticle+1; jParticle<fParticles.size(); jParticle++)
    {
      if(deleteCandidate[jParticle]) continue;
      if(!UseParticleInCompetition(fParticles[jParticle].GetPDG())) continue;
      
      fParticles[jParticle].GetMass(mass, massSigma);
      KFParticleDatabase::Instance()->GetMotherMass(fParticles[jParticle].GetPDG(), massPDG, massPDGSigma);
      
      float dm2 = fabs(mass - massPDG)/massPDGSigma;
//       if(dm2 > 3.f)  continue;
      
      if(! (fParticles[iParticle].DaughterIds()[0] == fParticles[jParticle].DaughterIds()[0] &&
            fParticles[iParticle].DaughterIds()[1] == fParticles[jParticle].DaughterIds()[1]) ) continue;
      
      if(dm1 < 3.f || dm2 < 3.f)
      {
//         if(dm1 < 3.f && dm2<3.f)
//         {
//           KFParticle part1 = fParticles[iParticle];
//           KFParticle part2 = fParticles[jParticle];
//           
//           if(fParticles[iParticle].GetPDG() == 310)
//           {
//             deleteCandidate[iParticle] = true;
//             break;
//           }
//           else
//           {
//             deleteCandidate[jParticle] = true;
//           }
//         }
          
        if(dm1 < dm2)
        {
          deleteCandidate[jParticle] = true;
          bestMother[fParticles[iParticle].DaughterIds()[0]] = iParticle;
          bestMother[fParticles[iParticle].DaughterIds()[1]] = iParticle;
        }
        else
        {
          deleteCandidate[iParticle] = true;
          bestMother[fParticles[iParticle].DaughterIds()[0]] = jParticle;
          bestMother[fParticles[iParticle].DaughterIds()[1]] = jParticle;
          break;
        }
      }
    }
  }
  
//   for(unsigned int iParticle=0; iParticle<fParticles.size(); iParticle++)
//   {
//     if(!(abs(fParticles[iParticle].GetPDG()) == 22)) continue;
//     
//     bool bothDaughtersElectrons = 1;
//     for(int iDaughter=0; iDaughter<fParticles[iParticle].NDaughters(); iDaughter++)
//     {
//       const int daughterIndex = fParticles[iParticle].DaughterIds()[iDaughter];
//       bothDaughtersElectrons &= abs(fParticles[daughterIndex].GetPDG()) == 11;
//     }
//     if(!bothDaughtersElectrons)
//     {
//       deleteCandidate[iParticle] = true;
//       continue;
//     }
//   }
  //clean dielectron spectrum
  //at first - both electrons are identified
  for(unsigned int iParticle=0; iParticle<fParticles.size(); iParticle++)
  {
    if(deleteCandidate[iParticle]) continue;
    if(!(abs(fParticles[iParticle].GetPDG()) == 22)) continue;
    
//     bool bothDaughtersElectrons = 1;
//     for(int iDaughter=0; iDaughter<fParticles[iParticle].NDaughters(); iDaughter++)
//     {
//       const int daughterIndex = fParticles[iParticle].DaughterIds()[iDaughter];
//       bothDaughtersElectrons &= abs(fParticles[daughterIndex].GetPDG()) == 11;
//     }
//     if(!bothDaughtersElectrons) continue;
    
    float mass, massSigma;
    fParticles[iParticle].GetMass(mass, massSigma);  
    float massPDG, massPDGSigma;
    KFParticleDatabase::Instance()->GetMotherMass(fParticles[iParticle].GetPDG(), massPDG, massPDGSigma);
    float dm = fabs(mass - massPDG)/massPDGSigma;
    
    if(dm < 3.f)
      particleInfo.push_back(ParticleInfo(iParticle, dm));
  }

  std::sort(particleInfo.begin(), particleInfo.end(), ParticleInfo::compare);
  
  for(unsigned int iPI=0; iPI<particleInfo.size(); iPI++)
  {
    const int index = particleInfo[iPI].fParticleIndex;
    if(deleteCandidate[index]) continue;
    
    bool isStore = true;
    for(int iDaughter=0; iDaughter<fParticles[index].NDaughters(); iDaughter++)
      isStore &= !(isUsed[ fParticles[index].DaughterIds()[iDaughter] ]);
    
    if(isStore)
    {
      for(int iDaughter=0; iDaughter<fParticles[index].NDaughters(); iDaughter++)
        isUsed[ fParticles[index].DaughterIds()[iDaughter] ] = true;
    }
    else
      deleteCandidate[index] = true;
  }
  
  for(unsigned int iParticle=0; iParticle<fParticles.size(); iParticle++)
  {
    if(deleteCandidate[iParticle]) continue;
    if(!(abs(fParticles[iParticle].GetPDG()) == 22)) continue;
    
    bool bothDaughtersElectrons = 1;
    for(int iDaughter=0; iDaughter<fParticles[iParticle].NDaughters(); iDaughter++)
    {
      const int daughterIndex = fParticles[iParticle].DaughterIds()[iDaughter];
      bothDaughtersElectrons &= abs(fParticles[daughterIndex].GetPDG()) == 11;
    }
    
    float mass, massSigma;
    fParticles[iParticle].GetMass(mass, massSigma);  
    float massPDG, massPDGSigma;
    KFParticleDatabase::Instance()->GetMotherMass(fParticles[iParticle].GetPDG(), massPDG, massPDGSigma);
    float dm = fabs(mass - massPDG)/massPDGSigma;
    
//     if( (bothDaughtersElectrons && dm > 3.f) || !bothDaughtersElectrons)
    if( dm > 3.f )
    {
      bool isStore = true;
      for(int iDaughter=0; iDaughter<fParticles[iParticle].NDaughters(); iDaughter++)
        isStore &= !(isUsed[ fParticles[iParticle].DaughterIds()[iDaughter] ]);
      if(!isStore)
      {
        deleteCandidate[iParticle] = true;
      }
    }
  }
  
  // clean LMVM spectrum
  for(unsigned int iParticle=0; iParticle<fParticles.size(); iParticle++)
  {
    if(!(abs(fParticles[iParticle].GetPDG()) == 100113 || abs(fParticles[iParticle].GetPDG()) == 443)) continue;
    
    bool bothDaughtersElectrons = 1;
    for(int iDaughter=0; iDaughter<fParticles[iParticle].NDaughters(); iDaughter++)
    {
      const int daughterIndex = fParticles[iParticle].DaughterIds()[iDaughter];
      bothDaughtersElectrons &= abs(fParticles[daughterIndex].GetPDG()) == 11;
    }
    if(!bothDaughtersElectrons)
    {
      deleteCandidate[iParticle] = true;
      continue;
    }
    
    bool isStore = true;
    for(int iDaughter=0; iDaughter<fParticles[iParticle].NDaughters(); iDaughter++)
      isStore &= !(isUsed[ fParticles[iParticle].DaughterIds()[iDaughter] ]);
    if(!isStore)
    {
      deleteCandidate[iParticle] = true;
    }
  }
//   for(unsigned int iParticle=0; iParticle<fParticles.size(); iParticle++)
//   {
//     if(deleteCandidate[iParticle]) continue;
//     if(abs(fParticles[iParticle].GetPDG()) == 310 || abs(fParticles[iParticle].GetPDG()) == 3122)
//     {
// //       float mass, massSigma;
// //       fParticles[iParticle].GetMass(mass, massSigma);
// //       
//       float massPDG, massPDGSigma;
//       KFParticleDatabase::Instance()->GetMotherMass(fParticles[iParticle].GetPDG(), massPDG, massPDGSigma);
// //       
// //       float dm = fabs(mass - massPDG)/massPDGSigma;
//       
//       KFParticle tmp = fParticles[iParticle];
// //       tmp.SetNonlinearMassConstraint(massPDG);
//       
//       particleInfo.push_back(ParticleInfo(iParticle, tmp.Chi2()));
//     }
//   }
//   
//   std::sort(particleInfo.begin(), particleInfo.end(), ParticleInfo::compare);
//   
//   for(unsigned int iPI=0; iPI<particleInfo.size(); iPI++)
//   {
//     const int index = particleInfo[iPI].fParticleIndex;
//     if(deleteCandidate[index]) continue;
//     
//     bool isStore = true;
//     for(int iDaughter=0; iDaughter<fParticles[index].NDaughters(); iDaughter++)
//       isStore &= !(isUsed[ fParticles[index].DaughterIds()[iDaughter] ]);
//     
//     if(isStore)
//     {
//       for(int iDaughter=0; iDaughter<fParticles[index].NDaughters(); iDaughter++)
//         isUsed[ fParticles[index].DaughterIds()[iDaughter] ] = true;
//     }
//     else
//       deleteCandidate[index] = true;
//   }
  
  for(int iParticle=0; iParticle<int(fParticles.size()); iParticle++)
  {
    if(deleteCandidate[iParticle]) continue;
    
    for(int iDaughter=0; iDaughter<fParticles[iParticle].NDaughters(); iDaughter++)
      if(bestMother[fParticles[iParticle].DaughterIds()[iDaughter]] != iParticle && bestMother[fParticles[iParticle].DaughterIds()[iDaughter]] > -1)
      {
        deleteCandidate[iParticle] = true;
        break;
      }
  }
#endif
  for(unsigned int iParticle=0; iParticle<fParticles.size(); iParticle++)
    if(deleteCandidate[iParticle])
      fParticles[iParticle].SetPDG(-1);
}

bool KFParticleTopoReconstructor::ParticleHasRepeatingDaughters(const KFParticle& particle)
{
  if(particle.NDaughters() < 2) return 0;
  
  vector<int> daughters;
  GetListOfDaughterTracks(particle, daughters);
  std::sort(daughters.begin(), daughters.end());
  bool sameDaughter=0;
  for(unsigned int iDaughter=1; iDaughter<daughters.size(); iDaughter++)
  {
    if(daughters[iDaughter] == daughters[iDaughter-1])
    {
      sameDaughter = 1;
      break;
    }
  }
  return sameDaughter;
}

void KFParticleTopoReconstructor::GetListOfDaughterTracks(const KFParticle& particle, vector<int>& daughters)
{
  if(particle.NDaughters() == 1)
    daughters.push_back( particle.DaughterIds()[0] );
  else
    for(int iDaughter=0; iDaughter<particle.NDaughters(); iDaughter++)
      GetListOfDaughterTracks( fParticles[ particle.DaughterIds()[iDaughter] ], daughters);
}

void KFParticleTopoReconstructor::ReconstructParticles()
{
  // find short-lived particles
#ifdef USE_TIMERS
  timer.Start();
#endif // USE_TIMERS

  fParticles.clear();

  if(fPV.size() < 1) return;

  TransportPVTracksToPrimVertex();
  //calculate chi to primary vertex, chi = sqrt(dr C-1 dr)
  GetChiToPrimVertex(&(fPV[0]), fPV.size());

  fKFParticleFinder->FindParticles(fTracks, fChiToPrimVtx, fParticles, fPV, fPV.size());
// #pragma omp critical 
//   std::cout << "NPart " << fParticles.size() << " " << fTracks[0].Size() << " "<< fTracks[1].Size() << " " << fTracks[2].Size() << " " << fTracks[3].Size()<< std::endl;
    
  for(unsigned int iParticle=0; iParticle<fParticles.size(); iParticle++)
    if(ParticleHasRepeatingDaughters(fParticles[iParticle]))
      fParticles[iParticle].SetPDG(-1);
    
  SelectParticleCandidates();
      
#ifdef USE_TIMERS
  timer.Stop();
  fStatTime[3] = timer.RealTime();
#endif // USE_TIMERS
} // void KFParticleTopoReconstructor::ReconstructPrimVertex

#ifdef WITHSCIF
void KFParticleTopoReconstructor::SendDataToXeonPhi( int iHLT, scif_epd_t& endpoint, void* buffer, off_t& offsetServer, off_t& offsetSender, float Bz)
{
  //pack the input data
  int* data = reinterpret_cast<int*>(buffer);
  int dataSize = NInputSets + 1 + 1; //sizes of the track vectors and pv vector, and field
  for(int iSet=0; iSet<NInputSets; iSet++)
    dataSize += fTracks[iSet].DataSize();
  dataSize += fPV.size() * 9;
      
  for(int iSet=0; iSet<NInputSets; iSet++)
    data[iSet] = fTracks[iSet].Size();
  data[NInputSets] = fPV.size();
  
  float& field = reinterpret_cast<float&>(data[NInputSets+1]);
  field = Bz;
  
  int offset = NInputSets+2;
      
  for(int iSet=0; iSet<NInputSets; iSet++)
    fTracks[iSet].SetDataToVector(data, offset);
  
  for(int iP=0; iP<3; iP++)
  {
    for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
    {
      float& tmpFloat = reinterpret_cast<float&>(data[offset + iPV]);
      tmpFloat = fPV[iPV].Parameter(iP)[0];
    }
    offset += fPV.size();
  }
  
  for(int iC=0; iC<6; iC++)
  {
    for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
    {
      float& tmpFloat = reinterpret_cast<float&>(data[offset + iPV]);
      tmpFloat = fPV[iPV].Covariance(iC)[0];
    }
    offset += fPV.size();
  }
  
  //send the input data to Xeon Phi
  int msgSize = sizeof(int) * dataSize;//1000 * sizeof(int);
  
  const uint16_t portSenderId = 2000 + iHLT;

  int controlSignal = portSenderId;
  scif_send(endpoint, &msgSize, sizeof(int), SCIF_SEND_BLOCK);
  scif_recv(endpoint, &controlSignal, sizeof(controlSignal), 1);
  if(controlSignal != portSenderId) { std::cout << controlSignal << " " << portSenderId << std::endl; return; }
  
  int ret = scif_writeto(endpoint, offsetServer, msgSize, offsetSender, 0);
  if ( ret == -1 ) std::cout << "Fail sending array to the server. Error: " << errno << std::endl;
      
  scif_send(endpoint, &controlSignal, sizeof(int), SCIF_SEND_BLOCK); // synchronization
}
#endif

struct PrimVertexVector
{
  vector<float> fP[3];
  vector<float> fC[6];
};

void KFParticleTopoReconstructor::SaveInputParticles(const string prefix, bool onlySecondary)
{
  static int nEvents = 0;
  string outFileName = "/event";
  char Result[16]; // string which will contain the number
  sprintf ( Result, "%d", nEvents ); 
  outFileName += string(Result);
  outFileName += "_KFPTracks.data";

  ofstream out((prefix+outFileName).data());
  
  //save tracks. tracks are already propagated to the beam
  
  int nSets = NInputSets;
  if(onlySecondary)
    nSets = 2;
    
  float B[3] = {0.f}, r[3] = {0.f};
  KFParticle kfpTmp;
  kfpTmp.GetFieldValue(r, B);
  out << B[2] << std::endl;
  out << nSets << std::endl;
  for(int iSet=0; iSet<nSets; iSet++)
  {
    out << fTracks[iSet].Size() << std::endl;
    for(int iP=0; iP<6; iP++)
    {
      for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
        out << fTracks[iSet].Parameter(iP)[iTr]<< " ";
      out << std::endl;
    }

    for(int iC=0; iC<21; iC++)
    {
      for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
        out << fTracks[iSet].Covariance(iC)[iTr]<< " ";
      out << std::endl;
    }

    for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
      out <<  fTracks[iSet].Id()[iTr] << " ";
    out << std::endl;
    
    for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
      out <<  fTracks[iSet].PDG()[iTr] << " ";
    out << std::endl;

    for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
      out <<  fTracks[iSet].Q()[iTr] << " ";
    out << std::endl;
    
    for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
      out <<  fTracks[iSet].PVIndex()[iTr] << " ";
    out << std::endl;
    
    out << fTracks[iSet].LastElectron() << " "
        << fTracks[iSet].LastMuon() << " "
        << fTracks[iSet].LastPion() << " "
        << fTracks[iSet].LastKaon() << " "
        << fTracks[iSet].LastProton() << std::endl;
  }

  //Save PVs
  out << fPV.size() << std::endl;
  for(unsigned int iPV=0; iPV < fPV.size(); iPV++)
  {
    out << fPV[iPV].X()[0] << " " << fPV[iPV].Y()[0] << " " << fPV[iPV].Z()[0] << " " << std::endl;
  
    for(int iC=0; iC<6; iC++)
      out << fPV[iPV].GetCovariance(iC)[0] << " ";
    out << std::endl;
  }
  out.close();
  
  nEvents++;
}

