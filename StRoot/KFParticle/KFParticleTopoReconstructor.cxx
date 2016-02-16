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
  fParticles.clear();
  int iOTr = 0; // index in out array
  
  float_v alpha(Vc::Zero);
  int nElements=0;
  
  for ( int iTr = 0; iTr < nTracks; iTr++ ) {
      // get track params in local CS
    AliHLTTPCCATrackParam trParam = tracker->Tracks()[ iTr ].InnerParam();
        
    const float x0 = 0;
    trParam.TransportToXWithMaterial( x0, tracker->Slice(0).Param().cBz( ) );

      // -- convert parameters
    fTracks[1].SetParameter(trParam.X(), 0, iOTr); // X
    fTracks[1].SetParameter(trParam.Y(), 1, iOTr); // Y
    fTracks[1].SetParameter(trParam.Z(), 2, iOTr); // Z

    const float pt = CAMath::Abs( 1.f / trParam.QPt() );
    const int q = -(trParam.QPt()>=0 ? 1 : -1);
//    if ( pt < 1 ) continue; // dbg
    if ( trParam.NDF() < 10+5 ) continue; // at least 15 hits in track
    if ( trParam.Chi2() > 10*trParam.NDF() ) continue; // dbg
//    if ( iOTr >= 4 ) continue; // dbg
    
    const float cosL = trParam.DzDs();
    fTracks[1].SetParameter(pt * trParam.GetCosPhi(), 3, iOTr); // Px
    fTracks[1].SetParameter(pt * trParam.SinPhi()   , 4, iOTr); // Py
    fTracks[1].SetParameter(pt * cosL               , 5, iOTr); // Pz
    
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
     
    {   // check cov matrix
      bool ok = true;
      int k = 0;
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j <= i; j++, k++) {
          ok &= CAMath::Finite( KFPCov[k] );
        }
        ok &= ( KFPCov[k-1] > 0 );
      }
      if (!ok) continue;
    }
    
    short trackPDG = -1;  
    if(pdg)
      trackPDG = (*pdg)[iTr];
    
    for(int iC=0; iC<21; iC++)
      fTracks[1].SetCovariance( KFPCov[iC], iC, iOTr);
    fTracks[1].SetId(iTr, iOTr);
    fTracks[1].SetPDG(trackPDG, iOTr);
    fTracks[1].SetQ(q, iOTr);
    fTracks[1].SetPVIndex(-1, iOTr);
    
    iOTr++;
    
    // convert into Global CS. Can't be done erlier because in tracker X hasn't correspondent covMatrix elements.
    alpha[nElements] = tracker->Tracks()[ iTr ].Alpha();
    nElements++;
    if(nElements == float_vLen)
    {
      fTracks[1].RotateXY( alpha, iOTr-nElements);
      nElements=0;
    }
  }
  if(nElements>0)
    fTracks[1].RotateXY( alpha, iOTr-nElements);
    
  fTracks[0].Resize(iOTr);
  fTracks[0].Set(fTracks[1],iOTr,0);
  
  fKFParticlePVReconstructor->Init( &fTracks[0], iOTr );
#ifdef USE_TIMERS
  timer.Stop();
  fStatTime[0] = timer.RealTime();
#endif /// USE_TIMERS
} // void KFParticleTopoReconstructor::Init(AliHLTTPCCAGBTracker* tracker)
#endif

void KFParticleTopoReconstructor::Init(vector<KFParticle> &particles, vector<int>* pdg)
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
    short trackPDG = -1;
    if(pdg)
      trackPDG = (*pdg)[iTr];
    
    for(int iP=0; iP<6; iP++)
      fTracks[0].SetParameter(particles[iTr].Parameters()[iP], iP, iTr);
    for(int iC=0; iC<21; iC++)
      fTracks[0].SetCovariance(particles[iTr].CovarianceMatrix()[iC], iC, iTr);
    fTracks[0].SetId(iTr, iTr);
    fTracks[0].SetPDG(trackPDG, iTr);
    fTracks[0].SetQ(particles[iTr].Q(), iTr);
    fTracks[0].SetPVIndex(-1, iTr);
  }
  
  fKFParticlePVReconstructor->Init( &fTracks[0], nTracks );
  
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
    fPV[iPV] = const_cast<KFParticle&>(pv[iPV]);

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
      
      int q = fTracks[offset[iSet]].Q()[iTrSorted];
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

  #ifdef USE_TIMERS
  timer.Stop();
  fStatTime[3] = timer.RealTime();
#endif // USE_TIMERS
} // void KFParticleTopoReconstructor::ReconstructPrimVertex

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

