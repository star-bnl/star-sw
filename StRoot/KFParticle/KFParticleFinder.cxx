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

#include "KFParticleFinder.h"

//for particle finding
#include <map>
using std::map;
using std::vector;

#include "KFParticleDatabase.h"
#include <iomanip>

KFParticleFinder::KFParticleFinder():
//  fNPV(-1),fNThreads(1),fCutCharmPt(0.2f),fCutCharmChiPrim(6.f),fCutLVMPt(0.2f),fCutLVMP(1.0f),fCutJPsiPt(1.0f),
  fNPV(-1),fNThreads(1),fDistanceCut(1.f),fLCut(5.f),fCutCharmPt(0.6f),fCutCharmChiPrim(85.f),fCutLVMPt(0.0f),fCutLVMP(0.0f),fCutJPsiPt(1.0f),
  fD0(0), fD0bar(0), fD04(0), fD04bar(0), fD0KK(0), fD0pipi(0), fDPlus(0), fDMinus(0), 
  fDPlus3Pi(0), fDMinus3Pi(0), fDsPlusK2Pi(0), fDsMinusK2Pi(0), fLcPlusP2Pi(0), fLcMinusP2Pi(0),
  fLPi(0), fLPiPIndex(0), fHe3Pi(0), fHe3PiBar(0), fHe4Pi(0), fHe4PiBar(0), 
  fHe4L(0), fHe5L(0),  fLLn(0), fH5LL(0),
  fEmcClusters(0), fMixedEventAnalysis(0), fDecayReconstructionList()
{
  //Cuts
  //track + track
  //chi2_prim         chi2_geo          ldl
  fCuts2D[0] = 3.f; fCuts2D[1] = 10.f; fCuts2D[2] = 5.f; 
  
  //tracks to select primary and secondary particles
  //mass              chi2_topo          ldl
#ifdef PANDA_STT
  fSecCuts[0] = 3.f; fSecCuts[1] = -3.f; fSecCuts[2] = 10.f;
#else  
  fSecCuts[0] = 3.f; fSecCuts[1] = 5.f; fSecCuts[2] = -10.f;
#endif
  
#ifdef __ROOT__
  fCutCharmChiPrim = 8;
#endif
  
  //track + particle
  //                ldl          chi2_topo                        chi2_geo
  fCutsTrackV0[0][0] = 10;     fCutsTrackV0[0][1] = 5;        fCutsTrackV0[0][2] = 6;  //Xi, Omega
  fCutsTrackV0[1][0] = 5;     fCutsTrackV0[1][1] = 3;        fCutsTrackV0[1][2] = 3;  //Omega, charm, H0, Sigma+
  fCutsTrackV0[2][0] = -100.;  fCutsTrackV0[2][1] = 10000.;   fCutsTrackV0[2][2] = 3e10;  //resonances
  
  //charm
  //chi2               l/dl                  chi2_topo
  fCutsCharm[0] = 3.f; fCutsCharm[1] = 5.f;  fCutsCharm[2] = 3.f; //D0 -> pi+ K-
  
  //cuts on particles reconstructed from short-lived particles
  //ldl,                      chi2_topo                 chi2_geo
  //H0 -> Lambda Lambda, Xi0 -> Lambda pi0
  fCutsPartPart[0][0] =  10;  fCutsPartPart[0][1] = 3;  fCutsPartPart[0][2] = 3;
  //Sigma0 -> Lambda Gamma, pi0 -> Gamma Gamma, K* -> K pi0, Sigma*0 -> Lambda pi0, Xi* -> Xi pi0
  fCutsPartPart[1][0] = -10;  fCutsPartPart[1][1] = 3;  fCutsPartPart[1][2] = 3;  
}

//________________________________________________________________________________
void KFParticleFinder::Init(int nPV) {
  fNPV = nPV;
  
  
//std::cout << "NPart estim " << nPart << std::endl;
//   Particles.reserve(vRTracks.size() + nPart);

  fD0.clear();
  fD0bar.clear();
  fD04.clear();
  fD04bar.clear();
  fD0KK.clear();
  fD0pipi.clear();
  fDPlus.clear();
  fDMinus.clear();
  fDPlus3Pi.clear();
  fDMinus3Pi.clear();
  fDsPlusK2Pi.clear();
  fDsMinusK2Pi.clear();
  fLcPlusP2Pi.clear();
  fLcMinusP2Pi.clear();
  fLPi.clear();
  fLPiPIndex.clear();
  fHe3Pi.clear();
  fHe3PiBar.clear();
  fHe4Pi.clear();
  fHe4PiBar.clear();
  fHe4L.clear();
  fHe5L.clear();
  fLLn.clear();
  fH5LL.clear();
  
  for(int iCandidates=0; iCandidates<fNSecCandidatesSets; iCandidates++)
    fSecCandidates[iCandidates].clear();
  
  for(int iCandidates=0; iCandidates<fNPrimCandidatesSets; iCandidates++)
  {
    fPrimCandidates[iCandidates].clear();
    fPrimCandidates[iCandidates].resize(fNPV);
  }
  
  for(int iCandidates=0; iCandidates<fNPrimCandidatesTopoSets; iCandidates++)
  {
    fPrimCandidatesTopo[iCandidates].clear();
    fPrimCandidatesTopo[iCandidates].resize(fNPV);
    
    fPrimCandidatesTopoMass[iCandidates].clear();
    fPrimCandidatesTopoMass[iCandidates].resize(fNPV);
  }
  
}
//________________________________________________________________________________

void KFParticleFinder::FindParticles(KFPTrackVector* vRTracks, kfvector_float* ChiToPrimVtx,
                     std::vector<KFParticle>& Particles, std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx, int nPV)
{
  Init(nPV);
  const int nPartPrim = vRTracks[2].NPions() * vRTracks[3].NKaons() + 
                        vRTracks[3].NPions() * vRTracks[2].NKaons() + 
                        vRTracks[2].NKaons() * vRTracks[3].NKaons() + 
                        vRTracks[2].NKaons() * vRTracks[3].NProtons() + 
                        vRTracks[3].NKaons() * vRTracks[2].NProtons() + 
                        vRTracks[2].NElectrons() * vRTracks[3].NElectrons() + 
                        vRTracks[2].NMuons() * vRTracks[3].NMuons();

  const int nPart = vRTracks[0].NPions() * vRTracks[1].NPions() +
                    vRTracks[0].NPions() * vRTracks[1].NProtons() +
                    vRTracks[1].NPions() * vRTracks[0].NProtons() + nPartPrim;
  int nEmcClusters = 0;
  if(fEmcClusters)
    nEmcClusters = fEmcClusters->Size();
  vector<KFParticle> vGammaPrimEmc;

  int nPartEstimation = nPart+vRTracks[0].Size()+vRTracks[1].Size()+vRTracks[2].Size()+vRTracks[3].Size() + nEmcClusters;

//   if(nPartEstimation < 100000)
//     Particles.reserve(nPartEstimation);
  //* Finds particles (K0s and Lambda) from a given set of tracks
// std::cout << "kfp size  " <<  sizeof(KFParticle) << std::endl;
  {
    KFPTrack kfTrack;
    for(int iV=0; iV<4; iV++)
    {
      for(int iTr=0; iTr < vRTracks[iV].Size(); iTr++)
      {
        vRTracks[iV].GetTrack(kfTrack, iTr);
        int pdg = vRTracks[iV].PDG()[iTr];
        if( pdg == 19 ) pdg =  13;
        if( pdg ==-19 ) pdg = -13;
        KFParticle tmp(kfTrack, pdg);
        tmp.SetPDG(pdg);
        tmp.SetId(Particles.size());
// #if 0 /* yf don't touch Id */
        vRTracks[iV].SetId(Particles.size(),iTr);
        if(vRTracks[iV+4].Size() > 0)
          vRTracks[iV+4].SetId(Particles.size(),iTr);
// #endif
        tmp.AddDaughterId( kfTrack.Id() );
#ifdef NonhomogeneousField
        for(int iF=0; iF<10; iF++)
          tmp.SetFieldCoeff( vRTracks[iV].FieldCoefficient(iF)[iTr], iF);
#endif
        Particles.push_back(tmp);
      }
    }

    if(fEmcClusters)
    {
      KFParticleSIMD tmpGammaSIMD;
      KFParticle tmpGamma;
      
      for(int iEmc=0; iEmc < fEmcClusters->Size(); iEmc += float_vLen)
      {
        const int NClustersVec = (iEmc + float_vLen < fEmcClusters->Size()) ? float_vLen : (fEmcClusters->Size() - iEmc);
        tmpGammaSIMD.Load(*fEmcClusters, iEmc, PrimVtx[0]);
        for(int iV=0; iV<NClustersVec; iV++)
        {
          tmpGammaSIMD.GetKFParticle(tmpGamma, iV);
          tmpGamma.SetPDG(22); //gamma pdg
          tmpGamma.SetId(Particles.size());
          tmpGamma.CleanDaughtersId();
          tmpGamma.AddDaughterId(fEmcClusters->Id()[iEmc+iV]);
          Particles.push_back(tmpGamma);
          vGammaPrimEmc.push_back(tmpGamma);
        }
      }
    }
  }



  Find2DaughterDecay(vRTracks, ChiToPrimVtx,
                     Particles, PrimVtx, fCuts2D,
                     fSecCuts, fPrimCandidates, fSecCandidates);

  if(!fMixedEventAnalysis)
  {
    //Construct two-particle background from positive primary tracks for subtraction from the resonance spectra
    ConstructPrimaryBG(vRTracks, Particles, PrimVtx, fCuts2D, fSecCuts, fPrimCandidates, fSecCandidates);
    
    for(int iPV=0; iPV<fNPV; iPV++ )
    {
      ExtrapolateToPV(fPrimCandidates[0][iPV],PrimVtx[iPV]);
      ExtrapolateToPV(fPrimCandidates[1][iPV],PrimVtx[iPV]);
      ExtrapolateToPV(fPrimCandidates[2][iPV],PrimVtx[iPV]);
      ExtrapolateToPV(fPrimCandidates[3][iPV],PrimVtx[iPV]);
    }
    
    NeutralDaughterDecay(vRTracks, Particles);
    
    //Xi- -> Lambda pi-, Omega- -> Lambda K-
    FindTrackV0Decay(fSecCandidates[1], 3122, vRTracks[1], -1, vRTracks[1].FirstPion(), vRTracks[1].LastKaon(),
                    Particles, PrimVtx, -1, &(ChiToPrimVtx[1]), &fPrimCandidates[5]);
    //Xi+ -> Lambda pi+, Omega+ -> Lambda K+
    FindTrackV0Decay(fSecCandidates[2], -3122, vRTracks[0], 1, vRTracks[0].FirstPion(), vRTracks[0].LastKaon(),
                    Particles, PrimVtx, -1, &(ChiToPrimVtx[0]), &fPrimCandidates[6]);

    for(int iPV=0; iPV<fNPV; iPV++ )
    {
      ExtrapolateToPV(fPrimCandidates[5][iPV],PrimVtx[iPV]);
      ExtrapolateToPV(fPrimCandidates[6][iPV],PrimVtx[iPV]);
      
      ExtrapolateToPV(fPrimCandidates[7][iPV],PrimVtx[iPV]);
      ExtrapolateToPV(fPrimCandidates[8][iPV],PrimVtx[iPV]);
    }

    //K*+ -> K0 pi+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[0][iPV], 310, vRTracks[2], 1, vRTracks[2].FirstPion(), vRTracks[2].LastPion(),
                      Particles, PrimVtx, iPV, 0);
    //K*- -> K0 pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[0][iPV], 310, vRTracks[3], -1, vRTracks[3].FirstPion(), vRTracks[3].LastPion(),
                      Particles, PrimVtx, iPV, 0);
    //Sigma*+ -> Lambda pi+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[1][iPV], 3122, vRTracks[2], 1, vRTracks[2].FirstPion(), vRTracks[2].LastPion(),
                      Particles, PrimVtx, iPV, 0);
    //Sigma*- -> Lambda pi-, Xi*- -> Lambda K-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[1][iPV], 3122, vRTracks[3], -1, vRTracks[3].FirstPion(), vRTracks[3].LastKaon(),
                      Particles, PrimVtx, iPV, 0);
    //Sigma*+_bar -> Lambda_bar pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[2][iPV], -3122, vRTracks[3], -1, vRTracks[3].FirstPion(), vRTracks[3].LastPion(),
                        Particles, PrimVtx, iPV, 0);
    //Sigma*-_bar -> Lambda_bar pi+, Xi*+ -> Lambda_bar + K+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[2][iPV], -3122, vRTracks[2], 1, vRTracks[2].FirstPion(), vRTracks[2].LastKaon(),
                        Particles, PrimVtx, iPV, 0);
    //Xi*0 -> Xi- pi+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[5][iPV], 3312, vRTracks[2], 1, vRTracks[2].FirstPion(), vRTracks[2].LastPion(),
                        Particles, PrimVtx, iPV, 0, &fPrimCandidates[9]);
    //Xi*0_bar -> Xi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[6][iPV], -3312, vRTracks[3], -1, vRTracks[3].FirstPion(), vRTracks[3].LastPion(),
                        Particles, PrimVtx, iPV, 0, &fPrimCandidates[10]);
    //Omega*- -> Xi- pi+ K-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[9][iPV], 3324, vRTracks[3], -1, vRTracks[3].FirstKaon(), vRTracks[3].LastKaon(),
                        Particles, PrimVtx, iPV, 0);
    //Omega*+ -> Xi+ pi- K+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[10][iPV], -3324, vRTracks[2], 1, vRTracks[2].FirstKaon(), vRTracks[2].LastKaon(),
                        Particles, PrimVtx, iPV, 0);
    //Hypernuclei
    //He4L -> He3 p pi-
    FindTrackV0Decay(fHe3Pi   , 3004, vRTracks[0],  1, vRTracks[0].FirstProton(), vRTracks[0].LastProton(), Particles, PrimVtx, -1, 0, 0, &fHe4L);
    //LLn -> H3L pi-
    FindTrackV0Decay(fHe3Pi   , 3004, vRTracks[1], -1, vRTracks[1].FirstPion(),   vRTracks[1].LastPion(),   Particles, PrimVtx, -1, 0 );
    //He4L_bar -> He3_bar p- pi+
    FindTrackV0Decay(fHe3PiBar,-3004, vRTracks[1], -1, vRTracks[1].FirstProton(), vRTracks[1].LastProton(), Particles, PrimVtx, -1, 0);
    //He5L -> He4 p pi-
    FindTrackV0Decay(fHe4Pi   , 3005, vRTracks[0],  1, vRTracks[0].FirstProton(), vRTracks[0].LastProton(), Particles, PrimVtx, -1, 0, 0, &fHe5L);
    //He5L_bar -> He4_bar p- pi+
    FindTrackV0Decay(fHe4PiBar,-3005, vRTracks[1], -1, vRTracks[1].FirstProton(), vRTracks[1].LastProton(), Particles, PrimVtx, -1, 0);
    //H4LL -> He4L pi-
    FindTrackV0Decay(fHe4L    , 3006, vRTracks[1], -1, vRTracks[1].FirstPion(),   vRTracks[1].LastPion(),   Particles, PrimVtx, -1, 0 );
    //H5LL -> He5L pi-
    FindTrackV0Decay(fHe5L    , 3007, vRTracks[1], -1, vRTracks[1].FirstPion(),   vRTracks[1].LastPion(),   Particles, PrimVtx, -1, 0 );
    //H4LL -> H3L p pi-
    FindTrackV0Decay(fLLn     , 3203, vRTracks[0],  1, vRTracks[0].FirstProton(), vRTracks[0].LastProton(), Particles, PrimVtx, -1, 0 );
    //He6LL -> He5L p pi-
    FindTrackV0Decay(fH5LL    , 3010, vRTracks[0],  1, vRTracks[0].FirstProton(), vRTracks[0].LastProton(), Particles, PrimVtx, -1, 0 );
    // Charm
    //LambdaC -> pi+ K- p, Ds+ -> pi+ K- K+, D+ -> pi+ K- pi+
    FindTrackV0Decay(fD0, 421, vRTracks[0], 1, vRTracks[0].FirstPion(), vRTracks[0].LastProton(),
                    Particles, PrimVtx, -1, &(ChiToPrimVtx[0]));
    //LambdaC_bar -> pi- K+ p-, Ds- -> pi- K+ K-, D- -> pi- K+ pi-
    FindTrackV0Decay(fD0bar, -421, vRTracks[1], -1, vRTracks[1].FirstPion(), vRTracks[1].LastProton(),
                    Particles, PrimVtx, -1, &(ChiToPrimVtx[1]));    
    //D0->pi+ K- pi+ pi-
    FindTrackV0Decay(fDPlus, 411, vRTracks[1], -1, vRTracks[1].FirstPion(), vRTracks[1].LastPion(),
                    Particles, PrimVtx, -1, &(ChiToPrimVtx[1]));
    //D0_bar->pi- K+ pi- pi+
    FindTrackV0Decay(fDMinus, -411, vRTracks[0], 1, vRTracks[0].FirstPion(), vRTracks[0].LastPion(),
                    Particles, PrimVtx, -1, &(ChiToPrimVtx[0]));
    //B+ -> D0_bar pi+, B+ -> D0_bar K+
    FindTrackV0Decay(fD0bar, -421, vRTracks[0], 1, vRTracks[0].FirstPion(), vRTracks[0].LastKaon(),
                     Particles, PrimVtx, -1, &(ChiToPrimVtx[0]));    
    //B- -> D0 pi-, B- -> D0 K-
    FindTrackV0Decay(fD0, 421, vRTracks[1], -1, vRTracks[1].FirstPion(), vRTracks[1].LastKaon(),
                     Particles, PrimVtx, -1, &(ChiToPrimVtx[1]));
    //B0 -> D- pi+, B0 -> D- K+
    FindTrackV0Decay(fDMinus, -419, vRTracks[0], 1, vRTracks[0].FirstPion(), vRTracks[0].LastKaon(),
                     Particles, PrimVtx, -1, &(ChiToPrimVtx[0]));    
    //B0_bar -> D+ pi-, B0_bar -> D+ K-
    FindTrackV0Decay(fDPlus, 419, vRTracks[1], -1, vRTracks[1].FirstPion(), vRTracks[1].LastKaon(),
                     Particles, PrimVtx, -1, &(ChiToPrimVtx[1]));
    //D0 -> pi+ K-
    SelectParticles(Particles,fD0,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //D0_bar -> pi+ K-
    SelectParticles(Particles,fD0bar,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);    
    //D*+->D0 pi+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fD0, 421, vRTracks[2], 1, vRTracks[2].FirstPion(), vRTracks[2].LastPion(),
                        Particles, PrimVtx, iPV, 0);
    //D*- -> D0_bar pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fD0, -421, vRTracks[3], -1, vRTracks[3].FirstPion(), vRTracks[3].LastPion(),
                        Particles, PrimVtx, iPV, 0);
    //D0 -> pi+ K- pi+ pi-
    SelectParticles(Particles,fD04,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //D0_bar -> pi- K+ pi- pi+
    SelectParticles(Particles,fD04bar,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //D*+->D0 pi+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fD04, 429, vRTracks[2], 1, vRTracks[2].FirstPion(), vRTracks[2].LastPion(),
                        Particles, PrimVtx, iPV, 0);
    //D0*- -> D0_bar pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fD04bar, -429, vRTracks[3], -1, vRTracks[3].FirstPion(), vRTracks[3].LastPion(),
                        Particles, PrimVtx, iPV, 0);
    //D+
    SelectParticles(Particles,fDPlus,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetDPlusMass(), KFParticleDatabase::Instance()->GetDPlusMassSigma(), fSecCuts[0]);
    //D-
    SelectParticles(Particles,fDMinus,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetDPlusMass(), KFParticleDatabase::Instance()->GetDPlusMassSigma(), fSecCuts[0]);
    //D*0->D+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fDPlus, 411, vRTracks[3], -1, vRTracks[3].FirstPion(), vRTracks[3].LastPion(),
                        Particles, PrimVtx, iPV, 0);
    //D*0_bar->D- pi+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fDMinus, -411, vRTracks[2], 1, vRTracks[2].FirstPion(), vRTracks[2].LastPion(),
                        Particles, PrimVtx, iPV, 0);
    
    float cutsD0[3] = {fCutsCharm[1], fCutsCharm[2], fCutsCharm[0]};
//     float cutsD0[3] = {-5, 1e10, 1e10}; 
    //D0 -> K0 pi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fD0pipi, fPrimCandidates[0][iPV], Particles, PrimVtx, cutsD0, -1, 425, 0, 1);
    //D0 -> K0 K+ K-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fD0KK, fPrimCandidates[0][iPV], Particles, PrimVtx, cutsD0, -1, 427, 0, 1);
    
    
    //LambdaC -> p pi+ pi-, Ds+ -> K+ pi+ pi-, D+ -> pi+ pi+ pi-
    FindTrackV0Decay(fD0pipi, 420, vRTracks[0], 1, vRTracks[0].FirstPion(), vRTracks[0].LastProton(),
                     Particles, PrimVtx, -1, &(ChiToPrimVtx[0]));
    //LambdaC_bar -> p_bar pi+ pi-, Ds- -> K- pi+ pi-, D- -> pi+ pi- pi-
    FindTrackV0Decay(fD0pipi, 420, vRTracks[1],-1, vRTracks[1].FirstPion(), vRTracks[1].LastProton(),
                     Particles, PrimVtx, -1, &(ChiToPrimVtx[1]));
    
    //D+ -> K0 pi+ pi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fDPlus3Pi, fPrimCandidates[0][iPV], Particles, PrimVtx, cutsD0, -1, 200411, 0, 1);
    //D- -> K0 pi+ pi- pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fDMinus3Pi, fPrimCandidates[0][iPV], Particles, PrimVtx, cutsD0, -1, -200411, 0, 1);
    //Lc+ -> Lambda pi+ pi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fDPlus3Pi, fPrimCandidates[1][iPV], Particles, PrimVtx, cutsD0, -1, 404122, 0, 1);
    //Lc- -> Lambda_bar pi+ pi- pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fDMinus3Pi, fPrimCandidates[2][iPV], Particles, PrimVtx, cutsD0, -1, -404122, 0, 1);
    //Xic0 -> Xi- pi+ pi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fDPlus3Pi, fPrimCandidates[5][iPV], Particles, PrimVtx, cutsD0, -1, 4132, 0, 1);
    //Xic0_bar -> Xi+ pi+ pi- pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fDMinus3Pi, fPrimCandidates[6][iPV], Particles, PrimVtx, cutsD0, -1, -4132, 0, 1);
    
    //Ds+ -> K0 K+ pi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fDsPlusK2Pi, fPrimCandidates[0][iPV], Particles, PrimVtx, cutsD0, -1, 300431, 0, 1);
    //Ds- -> K0 K- pi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fDsMinusK2Pi, fPrimCandidates[0][iPV], Particles, PrimVtx, cutsD0, -1, -300431, 0, 1);
    
    //Lc+ -> p K0 pi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fLcPlusP2Pi, fPrimCandidates[0][iPV], Particles, PrimVtx, cutsD0, -1, 204122, 0, 1);
    //Lc- -> p- K0 pi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fLcMinusP2Pi, fPrimCandidates[0][iPV], Particles, PrimVtx, cutsD0, -1, -204122, 0, 1);
    
    //D0 -> pi+ pi-
    SelectParticles(Particles,fD0pipi,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //D0 -> K+ K-
    SelectParticles(Particles,fD0KK,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //D+ -> pi+ pi+ pi-
    SelectParticles(Particles,fDPlus3Pi,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetDPlusMass(), KFParticleDatabase::Instance()->GetDPlusMassSigma(), fSecCuts[0]);
    //D- -> pi+ pi- pi-
    SelectParticles(Particles,fDMinus3Pi,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetDPlusMass(), KFParticleDatabase::Instance()->GetDPlusMassSigma(), fSecCuts[0]);
    //Ds+ -> K+ pi+ pi-, 
    SelectParticles(Particles,fDsPlusK2Pi,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //Ds- -> K- pi+ pi-
    SelectParticles(Particles,fDsMinusK2Pi,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //LambdaC -> p pi+ pi-
    SelectParticles(Particles,fLcPlusP2Pi,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //LambdaC_bar -> p_bar pi+ pi-
    SelectParticles(Particles,fLcMinusP2Pi,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    
    
    //D+ -> K0 pi+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[0][iPV], 310, vRTracks[0],  1, vRTracks[0].FirstPion(), vRTracks[0].LastPion(),
                       Particles, PrimVtx, -1, &(ChiToPrimVtx[0]) );
    //D- -> K0 pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[0][iPV], 310, vRTracks[1], -1, vRTracks[1].FirstPion(), vRTracks[1].LastPion(),
                       Particles, PrimVtx, -1, &(ChiToPrimVtx[1]) );
    //Ds+ -> K0 K+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[0][iPV], 310, vRTracks[0],  1, vRTracks[0].FirstKaon(), vRTracks[0].LastKaon(),
                       Particles, PrimVtx, -1, &(ChiToPrimVtx[0]) );
    //Ds- -> K0 K-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[0][iPV], 310, vRTracks[1], -1, vRTracks[1].FirstKaon(), vRTracks[1].LastKaon(),
                       Particles, PrimVtx, -1, &(ChiToPrimVtx[1]) );      
    //Lambdac+ -> Lambda pi+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[1][iPV], 3122, vRTracks[0], 1, vRTracks[0].FirstPion(), vRTracks[0].LastPion(),
                       Particles, PrimVtx, -1, &(ChiToPrimVtx[0]) );
    //Lambdac_bar- -> Lambda_bar pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[2][iPV], -3122, vRTracks[1], -1, vRTracks[1].FirstPion(), vRTracks[1].LastPion(),
                       Particles, PrimVtx, -1, &(ChiToPrimVtx[1]) );
    //Lambdac+ -> p K0s
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[0][iPV], 310, vRTracks[0], 1, vRTracks[0].FirstProton(), vRTracks[0].LastProton(),
                       Particles, PrimVtx, -1, &(ChiToPrimVtx[0]) );
    //Lambdac_bar- -> p_bar K0s
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[0][iPV], 310, vRTracks[1], -1, vRTracks[1].FirstProton(), vRTracks[1].LastProton(),
                       Particles, PrimVtx, -1, &(ChiToPrimVtx[1]) );   
      
    //H0 -> Lambda Lambda
    CombinePartPart(fSecCandidates[1], fSecCandidates[1], Particles, PrimVtx, fCutsPartPart[0], -1, 3000, 1, 1);
    //H0 -> Lambda p pi-
    FindTrackV0Decay(fLPi, 3002, vRTracks[0], 1, vRTracks[0].FirstProton(), vRTracks[0].LastProton(),
                      Particles, PrimVtx, -1);
    //Sigma0 -> Lambda Gamma
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[3][iPV], fPrimCandidates[1][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, 3212);
    //Sigma0_bar -> Lambda_bar Gamma
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[3][iPV], fPrimCandidates[2][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, -3212);
    //pi0 -> gamma gamma
    const float& mPi0 = KFParticleDatabase::Instance()->GetPi0Mass();
    const float& mPi0Sigma = KFParticleDatabase::Instance()->GetPi0MassSigma();
    CombinePartPart(fSecCandidates[3], fSecCandidates[3], Particles, PrimVtx, fCutsPartPart[1], -1, 111, 1, 0, &fPrimCandidates[4], &fSecCandidates[4], mPi0, mPi0Sigma);
    for(int iPV=0; iPV<fNPV; iPV++)
    {
      CombinePartPart(fPrimCandidates[3][iPV], fPrimCandidates[3][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, 111, 1, 0, &fPrimCandidates[4], &fSecCandidates[4], mPi0, mPi0Sigma);
      CombinePartPart(fSecCandidates[3],       fPrimCandidates[3][iPV], Particles, PrimVtx, fCutsPartPart[1],  -1, 111, 0, 0, &fPrimCandidates[4], &fSecCandidates[4], mPi0, mPi0Sigma);
    }
    for(int iPV=0; iPV<fNPV; iPV++ )
      ExtrapolateToPV(fPrimCandidates[4][iPV],PrimVtx[iPV]);
    //eta -> pi0 pi0 pi0
    //TODO implement this
    //Sigma+ -> p pi0
    FindTrackV0Decay(fSecCandidates[4], 111, vRTracks[0],  1, vRTracks[0].FirstProton(), vRTracks[0].LastProton(),
                      Particles, PrimVtx, -1);
    //Sigma+_bar -> p- pi0
    FindTrackV0Decay(fSecCandidates[4], 111, vRTracks[1], -1, vRTracks[1].FirstProton(), vRTracks[1].LastProton(),
                      Particles, PrimVtx, -1);
    //Xi0 -> Lambda pi0
    CombinePartPart(fSecCandidates[4], fSecCandidates[1], Particles, PrimVtx, fCutsPartPart[0], -1, 3322);
    //Xi0_bar -> Lambda_bar pi0
    CombinePartPart(fSecCandidates[4], fSecCandidates[2], Particles, PrimVtx, fCutsPartPart[0], -1, -3322);
    //K*+ -> K+ pi0
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[4][iPV], 111, vRTracks[2],  1, vRTracks[2].FirstKaon(), vRTracks[2].LastKaon(),
                        Particles, PrimVtx, -1);
    //K*- -> K- pi0
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[4][iPV], 111, vRTracks[3], -1, vRTracks[3].FirstKaon(), vRTracks[3].LastKaon(),
                        Particles, PrimVtx, -1);
    //K*0 -> K0 pi0
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[4][iPV], fPrimCandidates[0][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, 100313, 0, 1);    
    //Sigma*0 -> Lambda pi0
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[4][iPV], fPrimCandidates[1][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, 3214, 0, 1);       
    //Sigma*0_bar -> Lambda_bar pi0
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[4][iPV], fPrimCandidates[2][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, -3214, 0, 1);       
    //Xi*- -> Xi- pi0
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[4][iPV], fPrimCandidates[5][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, 3314, 0, 1);   
    //Xi*+ -> Xi+ pi0
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[4][iPV], fPrimCandidates[6][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, -3314, 0, 1);  
    //JPsi -> Lambda Lambda_bar
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[1][iPV], fPrimCandidates[2][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, 300443, 0, 1);  
    //JPsi -> Xi- Xi+
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[5][iPV], fPrimCandidates[6][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, 400443, 0, 1);  
    //Psi -> Omega- Omega+
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[7][iPV], fPrimCandidates[8][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, 500443, 0, 1);  
    
    //reconstruct particles with daughters in ElectroMagnetic Calorimeter
//     if(fEmcClusters)
//     {
//       //pi0 -> gamma gamma, EMC
//       vector< vector<KFParticle> > vPi0PrimEmc(1);
//       vector<KFParticle> vPi0SecEmc;
//       vector< vector<KFParticle> > vD0PrimEmc(1);
//       CombinePartPart(vGammaPrimEmc, vGammaPrimEmc, Particles, PrimVtx, fCutsPartPart[1], 0, 111, 1, 0, &vPi0PrimEmc, &vPi0SecEmc, mPi0, mPi0Sigma);
//           
//       //D+ -> K0 pi+
//       FindTrackV0Decay(fSecCandidates[0],    310, vRTracks[0],  1, vRTracks[0].FirstPion(), vRTracks[0].LastPion(), Particles, PrimVtx, -1/*, &(ChiToPrimVtx[0])*/);
//       //D0 -> K0 pi+ pi-
//       FindTrackV0Decay(fK0PiPlus, 100411, vRTracks[1], -1, vRTracks[1].FirstPion(), vRTracks[1].LastPion(), Particles, PrimVtx, -1/*, &(ChiToPrimVtx[0])*/);
//       //D0 -> K0 pi+ pi- pi0
//       CombinePartPart(fK0PiPi, vPi0PrimEmc[0], Particles, PrimVtx, fCutsPartPart[1], -1, 428, 0, 0, &vD0PrimEmc, 0, 
//                       KFParticleDatabase::Instance()->GetD0Mass(), 0.025);
//       
//       for(int iPV=0; iPV<1; iPV++ )
//       {
//         ExtrapolateToPV(vPi0PrimEmc[iPV],PrimVtx[iPV]);
//         ExtrapolateToPV(vD0PrimEmc[iPV],PrimVtx[iPV]);
//       }
//       //D0* -> D0 pi0
//       CombinePartPart(vD0PrimEmc[0], vPi0PrimEmc[0], Particles, PrimVtx, fCutsPartPart[1], 0, 10428);
//     }
  }
  else
  {
    //D0 -> pi+ K-
    SelectParticles(Particles,fD0,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //D0_bar -> pi+ K-
    SelectParticles(Particles,fD0bar,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
  }
}

void KFParticleFinder::ExtrapolateToPV(vector<KFParticle>& vParticles, KFParticleSIMD& PrimVtx)
{
  KFParticle* parts[float_vLen];
  KFParticle tmpPart[float_vLen];
  
  for(int iv=0; iv<float_vLen; iv++)
    parts[iv] = &tmpPart[iv];
    
  for(unsigned int iL=0; iL<vParticles.size(); iL += float_vLen)
  {

    unsigned int nPart = vParticles.size();
    unsigned int nEntries = (iL + float_vLen < nPart) ? float_vLen : (nPart - iL);

    
    for(unsigned int iv=0; iv<nEntries; iv++)
      tmpPart[iv] = vParticles[iL+iv];

    KFParticleSIMD tmp(parts,nEntries);

    tmp.TransportToPoint(PrimVtx.Parameters());

    for(unsigned int iv=0; iv<nEntries; iv++)
    {
      tmp.GetKFParticle(vParticles[iL+iv], iv);
    }
  }
}

float_v KFParticleFinder::GetChi2BetweenParticles(KFParticleSIMD &p1, KFParticleSIMD &p2)
{
  const float_v& x1 = p1.GetX();
  const float_v& y1 = p1.GetY();
  const float_v& z1 = p1.GetZ();

  const float_v& x2 = p2.GetX();
  const float_v& y2 = p2.GetY();
  const float_v& z2 = p2.GetZ();

  const float_v dx = x1 - x2;
  const float_v dy = y1 - y2;
  const float_v dz = z1 - z2;

  const float_v& c0 = p1.GetCovariance(0) + p2.GetCovariance(0);
  const float_v& c1 = p1.GetCovariance(1) + p2.GetCovariance(1);
  const float_v& c2 = p1.GetCovariance(2) + p2.GetCovariance(2);
  const float_v& c3 = p1.GetCovariance(3) + p2.GetCovariance(3);
  const float_v& c4 = p1.GetCovariance(4) + p2.GetCovariance(4);
  const float_v& c5 = p1.GetCovariance(5) + p2.GetCovariance(5);

  const float_v r2 = dx*dx + dy*dy + dz*dz;
  const float_v err2 = c0*dx*dx + c2*dy*dy + c5*dz*dz + 2.f*( c1*dx*dy + c3*dx*dz + c4*dy*dz );

  return (r2*r2/err2);
}

inline void KFParticleFinder::ConstructV0(KFPTrackVector* vTracks,
                                          int iTrTypePos,
                                          int iTrTypeNeg,
                                          uint_v& idPosDaughters,
                                          uint_v& idNegDaughters,
                                          int_v& daughterPosPDG,
                                          int_v& daughterNegPDG,
                                          KFParticleSIMD& mother,
                                          KFParticle& mother_temp,
                                          const unsigned short NTracks,
                                          kfvector_floatv& l,
                                          kfvector_floatv& dl,
                                          vector<KFParticle>& Particles,
                                          std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                                          const float* cuts,
                                          const int_v& pvIndex,
                                          const float* secCuts,
                                          const float_v& massMotherPDG,
                                          const float_v& massMotherPDGSigma,
                                          KFParticleSIMD& motherPrimSecCand,
                                          int& nPrimSecCand,
                                          vector< vector<KFParticle> >* vMotherPrim,
                                          vector<KFParticle>* vMotherSec
                                         )
{
  float_m isPrimary(pvIndex>-1);
  int_v trackId;
  KFParticleSIMD posDaughter(vTracks[iTrTypePos],idPosDaughters, daughterPosPDG);
  trackId.gather( &(vTracks[iTrTypePos].Id()[0]), idPosDaughters );
  posDaughter.SetId(trackId);

  KFParticleSIMD negDaughter(vTracks[iTrTypeNeg],idNegDaughters, daughterNegPDG);
  trackId.gather( &(vTracks[iTrTypeNeg].Id()[0]), idNegDaughters );
  negDaughter.SetId(trackId);   

//   float_v ds[2] = {0.f,0.f};
//   float_v dsdr[4][6];
//   negDaughter.GetDStoParticle( posDaughter, ds, dsdr );
//   negDaughter.TransportToDS(ds[0], dsdr[0]);
//   posDaughter.TransportToDS(ds[1], dsdr[3]);
    
  const KFParticleSIMD* vDaughtersPointer[2] = {&negDaughter, &posDaughter};
  mother.Construct(vDaughtersPointer, 2, 0);
  
  float_m saveParticle(int_v::IndexesFromZero() < int(NTracks));
  float_v chi2Cut = cuts[1];
  float_v ldlCut  = cuts[2];
  if( !(float_m(abs(mother.PDG()) == 421 || abs(mother.PDG()) == 426 || abs(mother.PDG()) == 420)).isEmpty() )
  {
    chi2Cut( float_m(abs(mother.PDG()) == 421 || abs(mother.PDG()) == 426 || abs(mother.PDG()) == 420) ) = fCutsCharm[0];
    ldlCut( float_m(abs(mother.PDG()) == 421 || abs(mother.PDG()) == 426 || abs(mother.PDG()) == 420) ) = -1;//fCutsCharm[1];
  }
  
//   std::cout << "pdg " << mother.PDG() << std::endl;
//   saveParticle &= float_m( mother.NDF() == int_v(1) );
  saveParticle &= (mother.Chi2()/static_cast<float_v>(mother.NDF()) < chi2Cut );
//   std::cout << "chi " <<  (mother.Chi2()/static_cast<float_v>(mother.NDF()))<< " " << saveParticle << std::endl;
//   std::cin.get();
  saveParticle &= KFPMath::Finite(mother.GetChi2());
  saveParticle &= (mother.GetChi2() > 0.0f);
  saveParticle &= (mother.GetChi2() == mother.GetChi2());

  if( saveParticle.isEmpty() ) return;
  
  float_v lMin(1.e8f);
  float_v ldlMin(1.e8f);
  float_m isParticleFromVertex(false);

  for(int iP=0; iP<fNPV; iP++)
  {
    float_m isParticleFromVertexLocal;
    mother.GetDistanceToVertexLine(PrimVtx[iP], l[iP], dl[iP], &isParticleFromVertexLocal);
    isParticleFromVertex |= isParticleFromVertexLocal;
    float_v ldl = (l[iP]/dl[iP]);
    lMin( (l[iP] < lMin) && saveParticle) = l[iP];
    ldlMin( (ldl < ldlMin) && saveParticle) = ldl;
  }
//   saveParticle &= (float_m(!isPrimary) && ldlMin > ldlCut) || isPrimary;
//   std::cout << "ldl " <<  (ldlMin > ldlCut)<< " " << saveParticle << std::endl;
//   std::cin.get();

  saveParticle &= (lMin < 200.f);

//   saveParticle &= ( float_m(abs(mother.PDG()) == 421) && lMin>0.025f ) || !float_m(abs(mother.PDG()) == 421); //TODO
//   saveParticle &= ( float_m(abs(mother.PDG()) == int_v(310) || abs(mother.PDG()) == int_v(3122)) && lMin>float_v(3.f) ) || 
//                    !float_m(abs(mother.PDG()) == int_v(310) || abs(mother.PDG()) == int_v(3122)); //TODO
  
//   KFParticleSIMD motherTopo;
//     ldlMin = 1.e8f;
//   for(int iP=0; iP<fNPV; iP++)
//   {
//     motherTopo = mother;
//     motherTopo.SetProductionVertex(PrimVtx[iP]);
//     motherTopo.GetDecayLength(l[iP], dl[iP]);
//     float_v ldl = (l[iP]/dl[iP]);
//     ldlMin( (ldl < ldlMin) && saveParticle) = ldl;
//   }
  saveParticle &= ( (float_m(!isPrimary) && ldlMin > ldlCut) || float_m(isPrimary) );
  
  
//         if(isPrimary && (float(ldlMin > 3) )) continue;
  saveParticle &= (float_m(!isPrimary) && isParticleFromVertex) || isPrimary;
//   std::cout << "ldl " <<  (ldlMin > ldlCut)<< " " << saveParticle << std::endl;
//   std::cin.get();
  if( saveParticle.isEmpty() ) return;
  
  float_m isK0     = saveParticle && float_m(mother.PDG() == int_v(310));
  float_m isLambda = saveParticle && float_m(abs(mother.PDG()) == int_v(3122));
  float_m isGamma  = saveParticle && float_m(mother.PDG() == int_v(22));
  float_m isHyperNuclei = saveParticle && float_m(abs(mother.PDG()) > 3000 && abs(mother.PDG()) < 3104);
  
  saveParticle &= ( ((isK0 || isLambda || isHyperNuclei) && lMin > float_v(fLCut)) || !(isK0 || isLambda || isHyperNuclei) );

  float_m saveMother(false);
  
  if( !(isK0.isEmpty()) || !(isLambda.isEmpty()) || !(isGamma.isEmpty()))
  { 
    float_v mass, errMass;

    mother.GetMass(mass, errMass);
    saveMother = saveParticle;
// #ifdef __CUT_MASS__
    saveMother &= (abs(mass - massMotherPDG)/massMotherPDGSigma) < secCuts[0];
// #endif
    saveMother &= ((ldlMin > secCuts[2]) && !isGamma) || isGamma;
    saveMother &= (isK0 || isLambda || isGamma);
  }
  
  for(int iv=0; iv<NTracks; iv++)
  {
    if(!saveParticle[iv]) continue;
  
    mother.GetKFParticle(mother_temp, iv);
    int motherId = Particles.size();
    mother_temp.SetId(Particles.size());
    if( mother.PDG()[iv] == 421 ) 
    {
      fD0.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == -421 ) 
    {
      fD0bar.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == 426 ) 
    {
      fD0KK.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == 420 ) 
    {
      fD0pipi.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == 3004)
      fHe3Pi.push_back(mother_temp);
    if( mother.PDG()[iv] == -3004)
      fHe3PiBar.push_back(mother_temp);
    if( mother.PDG()[iv] == 3005)
      fHe4Pi.push_back(mother_temp);
    if( mother.PDG()[iv] == -3005)
      fHe4PiBar.push_back(mother_temp);
    
    Particles.push_back(mother_temp);
    
    if( mother.PDG()[iv] == 22 && isPrimary[iv] )
    {
      float negPt2 = negDaughter.Px()[iv]*negDaughter.Px()[iv] + negDaughter.Py()[iv]*negDaughter.Py()[iv];
      float posPt2 = posDaughter.Px()[iv]*posDaughter.Px()[iv] + posDaughter.Py()[iv]*posDaughter.Py()[iv];

      if( (negPt2 >fCutLVMPt*fCutLVMPt) && (posPt2 >fCutLVMPt*fCutLVMPt) )
      {
        mother_temp.SetPDG(100113);
        mother_temp.SetId(Particles.size());
        Particles.push_back(mother_temp);
        
        if( (negPt2 >fCutJPsiPt*fCutJPsiPt) && (posPt2 >fCutJPsiPt*fCutJPsiPt) )
        {
          mother_temp.SetPDG(443);
          mother_temp.SetId(Particles.size());
          Particles.push_back(mother_temp);
        }
      }  
    }

    if( mother.PDG()[iv] == 200113 )
    {
      float negPt2 = negDaughter.Px()[iv]*negDaughter.Px()[iv] + negDaughter.Py()[iv]*negDaughter.Py()[iv];
      float posPt2 = posDaughter.Px()[iv]*posDaughter.Px()[iv] + posDaughter.Py()[iv]*posDaughter.Py()[iv];
      
      if( (negPt2 >fCutJPsiPt*fCutJPsiPt) && (posPt2 >fCutJPsiPt*fCutJPsiPt) && (abs(daughterPosPDG[iv]) == 13) && (abs(daughterNegPDG[iv]) == 13))
      {
        mother_temp.SetPDG(100443);
        mother_temp.SetId(Particles.size());
        Particles.push_back(mother_temp);
      }  
    }
    
    if(saveMother[iv])
    {
      mother.SetId(motherId);
      motherPrimSecCand.SetOneEntry(nPrimSecCand,mother,iv);

      nPrimSecCand++;
      if(nPrimSecCand==float_vLen)
      {
        SaveV0PrimSecCand(motherPrimSecCand,nPrimSecCand,mother_temp,PrimVtx,secCuts,vMotherPrim,vMotherSec);
        nPrimSecCand = 0;
      }
    }
  }
}

inline void KFParticleFinder::SaveV0PrimSecCand(KFParticleSIMD& mother,
                                                int& NParticles,
                                                KFParticle& mother_temp,
                                                std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                                                const float* secCuts,
                                                vector< vector<KFParticle> >* vMotherPrim,
                                                vector<KFParticle>* vMotherSec)
{
  KFParticleSIMD motherTopo;
  float_v massMotherPDG, massMotherPDGSigma;
  
  float_m isSec(false);
  float_m isPrim(false);
  vector<int> iPrimVert[float_vLen];

  KFParticleDatabase::Instance()->GetMotherMass(mother.PDG(),massMotherPDG,massMotherPDGSigma);
  
  float_m isK0        = float_m(mother.PDG() == int_v(310));
  float_m isLambda    = float_m(abs(mother.PDG()) == int_v(3122));
  float_m isGamma     = float_m(mother.PDG() == int_v(22));

  int_v arrayIndex(-1); //for saving primary candidates; 

  arrayIndex(mother.PDG() ==   int_v(310)) = 0;
  arrayIndex(mother.PDG() ==  int_v(3122)) = 1;
  arrayIndex(mother.PDG() == int_v(-3122)) = 2;
  arrayIndex(mother.PDG() ==    int_v(22)) = 3;

  float_m isPrimaryPart(false);

  float_v chi2TopoMin = 1.e4f;
  
  for(int iP=0; iP< fNPV; iP++)
  {
    motherTopo = mother;
    motherTopo.SetProductionVertex(PrimVtx[iP]);
    
    const float_v& motherTopoChi2Ndf = motherTopo.GetChi2()/float_v(motherTopo.GetNDF());
    chi2TopoMin(motherTopoChi2Ndf < chi2TopoMin) = motherTopoChi2Ndf;
    const float_m isPrimaryPartLocal = ( motherTopoChi2Ndf < secCuts[1] );
    if(isPrimaryPartLocal.isEmpty()) continue;
    isPrimaryPart |= isPrimaryPartLocal;
    for(int iV=0; iV<NParticles; iV++)
    {
      if(isPrimaryPartLocal[iV])
      {
        motherTopo.GetKFParticle(mother_temp, iV);
        fPrimCandidatesTopo[arrayIndex[iV]][iP].push_back(mother_temp);
        iPrimVert[iV].push_back(iP);
      }
    }
    
    motherTopo.SetNonlinearMassConstraint(massMotherPDG);
    for(int iV=0; iV<NParticles; iV++)
    {
      if(isPrimaryPartLocal[iV])
      {
        motherTopo.GetKFParticle(mother_temp, iV);
        fPrimCandidatesTopoMass[arrayIndex[iV]][iP].push_back(mother_temp);
      }
    }
  }
  
  isPrim |= ( ( isPrimaryPart ) && (isK0 || isLambda || isGamma) );
  isSec  |= ( (!isPrimaryPart ) && (isK0 || isLambda || isGamma) && (chi2TopoMin < float_v(500.f)) );
//   isSec  |= ( (!isPrimaryPart ) && (isK0 || isLambda || isGamma) );
  
  mother.SetNonlinearMassConstraint(massMotherPDG);

  for(int iv=0; iv<NParticles; iv++)
  { 
    if(isPrim[iv] || isSec[iv])
    {  
      mother.GetKFParticle(mother_temp, iv);
      
      if(isPrim[iv] )
      {
        for(unsigned int iP = 0; iP<iPrimVert[iv].size(); iP++)
          vMotherPrim[arrayIndex[iv]][iPrimVert[iv][iP]].push_back(mother_temp);
      }
      
      if(isSec[iv] )
        vMotherSec[arrayIndex[iv]].push_back(mother_temp);
    }
  }
}

void KFParticleFinder::Find2DaughterDecay(KFPTrackVector* vTracks, kfvector_float* ChiToPrimVtx,
                                          vector<KFParticle>& Particles,
                                          std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                                          const float* cuts,
                                          const float* secCuts,
                                          vector< vector<KFParticle> >* vMotherPrim,
                                          vector<KFParticle>* vMotherSec )
{ 
  KFParticle mother_temp;
  KFParticleSIMD mother;
  kfvector_floatv l(fNPV), dl(fNPV);

  KFParticleSIMD daughterNeg, daughterPos;

    
  // for secondary V0
  unsigned int nBufEntry = 0;
  float_v dS;
  uint_v idNegDaughters;
  uint_v idPosDaughters;
  int_v daughterPosPDG(-1);
  int_v daughterNegPDG(-1);
    
  int_v pvIndexMother(-1);
  
  float_v massMotherPDG(Vc::Zero), massMotherPDGSigma(Vc::Zero);
  int_v V0PDG(Vc::Zero);

  KFParticleSIMD motherPrimSecCand;
  int nPrimSecCand =0;
  
  int trTypeIndexPos[2] = {0,2};
  int trTypeIndexNeg[2] = {1,3};

  for( int iTrTypeNeg = 0; iTrTypeNeg<2; iTrTypeNeg++)
  {
    KFPTrackVector& negTracks = vTracks[ trTypeIndexNeg[iTrTypeNeg] ];
    
    for(int iTrTypePos=0; iTrTypePos<2; iTrTypePos++)
    {
      KFPTrackVector& posTracks = vTracks[ trTypeIndexPos[iTrTypePos] ];
      int_v negTracksSize = negTracks.Size();
      int nPositiveTracks = posTracks.Size();
      
      //track categories
      int nTC = 5;
      int startTCPos[5] = {0};
      int endTCPos[5] = {0};
      int startTCNeg[5] = {0};
      int endTCNeg[5] = {0};
      
      if((iTrTypeNeg == 0) && (iTrTypePos == 0))
      {
        // Secondary particles
        nTC = 5;
        // e-
        startTCPos[0] = 0; endTCPos[0] = nPositiveTracks; //posTracks.LastElectron();
        startTCNeg[0] = 0; endTCNeg[0] = negTracksSize[0];  //negTracks.LastElectron(); 
        //mu-
        startTCPos[1] = 0; endTCPos[1] = 0;
        startTCNeg[1] = 0; endTCNeg[1] = 0; 
        //pi- + ghosts
        startTCPos[2] = posTracks.FirstPion(); endTCPos[2] = nPositiveTracks;
        startTCNeg[2] = negTracks.FirstPion(); endTCNeg[2] = negTracks.LastPion();        
        //K-
        startTCPos[3] = posTracks.FirstPion(); endTCPos[3] = posTracks.LastKaon();
        startTCNeg[3] = negTracks.FirstKaon(); endTCNeg[3] = negTracks.LastKaon();  
        //p-, d-, t-, he3-, he4-
        startTCPos[4] = posTracks.FirstPion(); endTCPos[4] = posTracks.LastPion();
        startTCNeg[4] = negTracks.FirstProton(); endTCNeg[4] = negTracksSize[0];  
      }
      
      if( iTrTypeNeg != iTrTypePos )
      {
        //Mixed particles - only gamma -> e+ e-
        nTC = 1;
        startTCPos[0] = 0; endTCPos[0] = nPositiveTracks; //posTracks.LastElectron();
        startTCNeg[0] = 0; endTCNeg[0] = negTracksSize[0];  //negTracks.LastElectron(); 
      }
      
      if((iTrTypeNeg == 1) && (iTrTypePos == 1))
      {
        //primary particles
        nTC = 5;
        // e-
        startTCPos[0] = 0; endTCPos[0] = nPositiveTracks; //posTracks.LastElectron();
        startTCNeg[0] = 0; endTCNeg[0] = negTracksSize[0];  //negTracks.LastElectron(); 
        //mu-
        startTCPos[1] = posTracks.FirstMuon(); endTCPos[1] = posTracks.LastMuon();
        startTCNeg[1] = negTracks.FirstMuon(); endTCNeg[1] = negTracks.LastMuon(); 
        //pi- + ghosts
        startTCPos[2] = posTracks.FirstPion(); endTCPos[2] = posTracks.LastProton();
        startTCNeg[2] = negTracks.FirstPion(); endTCNeg[2] = negTracks.LastPion();        
        //K-
        startTCPos[3] = posTracks.FirstPion(); endTCPos[3] = nPositiveTracks;
        startTCNeg[3] = negTracks.FirstKaon(); endTCNeg[3] = negTracks.LastKaon();  
        //p-
        startTCPos[4] = posTracks.FirstPion(); endTCPos[4] = posTracks.LastProton();
        startTCNeg[4] = negTracks.FirstProton(); endTCNeg[4] = negTracks.LastProton();      
      }
      
      for(int iTC=0; iTC<nTC; iTC++)
      {
        for(int iTrN=startTCNeg[iTC]; iTrN < endTCNeg[iTC]; iTrN += float_vLen)
        {
          const int NTracksNeg = (iTrN + float_vLen < negTracks.Size()) ? float_vLen : (negTracks.Size() - iTrN);

          int_v negInd = int_v::IndexesFromZero() + int(iTrN);

          int_v negPDG = reinterpret_cast<const int_v&>(negTracks.PDG()[iTrN]);
          int_v negPVIndex = reinterpret_cast<const int_v&>(negTracks.PVIndex()[iTrN]);
          int_v negNPixelHits = reinterpret_cast<const int_v&>(negTracks.NPixelHits()[iTrN]);
          
          int_v id = reinterpret_cast<const int_v&>(negTracks.Id()[iTrN]);
          
//           for(int iV=0; iV<NTracksNeg; iV++)
//             if(id[iV] == 316 || id[iV] == 77)
//               std::cout << "id " << id[iV] << " pdg " << negPDG[iV] << " nhits " << negNPixelHits[iV] << std::endl;
          
          int_v trackPdgNeg = negPDG;
          int_m activeNeg = (negPDG != -1);
          
//           if( !((negPDG == -1).isEmpty()) )
//           {
//             trackPdgNeg(negPVIndex<0 && (negPDG == -1) ) = -211;
//                 
//             activeNeg |= int_m(negPVIndex < 0) && int_m(negPDG == -1) ;
//           }
          
          activeNeg &= (int_v::IndexesFromZero() < int(NTracksNeg));
              
          daughterNeg.Load(negTracks, iTrN, negPDG);
                
          float_v chiPrimNeg(Vc::Zero);
          float_v chiPrimPos(Vc::Zero);
          
          if( (iTrTypeNeg == 0) && (iTrTypePos == 0) )
            chiPrimNeg = reinterpret_cast<const float_v&>( ChiToPrimVtx[trTypeIndexNeg[iTrTypeNeg]][iTrN]);
          
          for(int iTrP=startTCPos[iTC]; iTrP < endTCPos[iTC]; iTrP += float_vLen)
          {
            const int NTracks = (iTrP + float_vLen < nPositiveTracks) ? float_vLen : (nPositiveTracks - iTrP);

            const int_v& posPDG = reinterpret_cast<const int_v&>(posTracks.PDG()[iTrP]);
            const int_v& posPVIndex = reinterpret_cast<const  int_v&>(posTracks.PVIndex()[iTrP]);     
            const int_v& posNPixelHits = reinterpret_cast<const int_v&>(posTracks.NPixelHits()[iTrP]);
            const int_m& isPosSecondary = (posPVIndex < 0);

            daughterPos.Load(posTracks, iTrP, posPDG);
            
            if( (iTrTypeNeg == 0) && (iTrTypePos == 0) )
              chiPrimPos = reinterpret_cast<const float_v&>( ChiToPrimVtx[trTypeIndexPos[iTrTypePos]][iTrP]);
            
            for(int iRot = 0; iRot<float_vLen; iRot++)
            {
//               if(iRot>0)
              {
                negPDG = negPDG.rotated(1);
                negPVIndex = negPVIndex.rotated(1);
                negNPixelHits = negNPixelHits.rotated(1);
                negInd = negInd.rotated(1);
                trackPdgNeg = trackPdgNeg.rotated(1);
              
                daughterNeg.Rotate();
                chiPrimNeg = chiPrimNeg.rotated(1);

                activeNeg = ( (negPDG != -1) || ( (negPVIndex < 0) && (negPDG == -1) ) ) && (negInd < negTracksSize);
              }
              const int_m& isSecondary = int_m( negPVIndex < 0 ) && isPosSecondary;
              const int_m& isPrimary   = int_m( negPVIndex >= 0 ) && (!isPosSecondary);
            
              float_m closeDaughters = float_m(activeNeg && (int_v::IndexesFromZero() < int_v(NTracks)));
              
//               float_v ds(Vc::Zero), dsPos(Vc::Zero);
//               if(!( (iTrTypePos == 1) && (iTrTypeNeg == 1) ) )
//               { 
//                 float_v par1[8], cov1[36], par2[8], cov2[36];
//                 daughterNeg.GetDStoParticle(daughterPos, ds, dsPos);
//                 float_v dsdr[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
//                 daughterNeg.Transport(ds,dsdr,par1,cov1);
//                 daughterPos.Transport(dsPos,dsdr,par2,cov2);
//           
//                 const float_v& dx = par1[0] - par2[0];
//                 const float_v& dy = par1[1] - par2[1];
//                 const float_v& dz = par1[2] - par2[2];
//                 const float_v& r2 = dx*dx + dy*dy + dz*dz;
//                 
//                 const float_v vtx[3] = {(par1[0] + par2[0])/2.f,
//                                         (par1[1] + par2[1])/2.f,
//                                         (par1[2] + par2[2])/2.f, };
//         
//                 
//                 const float_v cov[6] = {cov1[0]+cov2[0],
//                                         cov1[1]+cov2[1],
//                                         cov1[2]+cov2[2],
//                                         cov1[3]+cov2[3],
//                                         cov1[4]+cov2[4],
//                                         cov1[5]+cov2[5] };
//                 const float_v& err2 = cov[0]*dx*dx + cov[2]*dy*dy + cov[5]*dz*dz + 2.f*( cov[1]*dx*dy + cov[3]*dx*dz + cov[4]*dy*dz );
  
//                 closeDaughters &= daughterNeg.GetDeviationFromParticle(daughterPos) < float_v(1.f);
//                 closeDaughters &= (r2 < float_v(1.f));
//                 
//               std::cout << "distance " << daughterNeg.GetDistanceFromParticle(daughterPos) << " mask " << closeDaughters << std::endl;
//                 closeDaughters &= (daughterNeg.GetDistanceFromParticle(daughterPos) < float_v(1.f));
//               }
              if(closeDaughters.isEmpty() && (iTC != 0)) continue;
              
              
              int_v trackPdgPos[2];
              int_m active[2];

//               int nPDGPos = 2;//TODO
              int nPDGPos = 1;
              
              active[0] = (posPDG != -1);
              active[0] &= ((isPrimary && (posPVIndex == negPVIndex)) || !(isPrimary));

              active[1] = int_m(false);
              
              trackPdgPos[0] = posPDG;
              
//               if( (posPDG == -1).isEmpty() && (posPDG > 1000000000).isEmpty() && (posPDG == 211).isEmpty() )
//               {
//                 nPDGPos = 1;
//               }
//               else
//               {
//                 trackPdgPos[0](isSecondary && posPDG == -1) = 211;
//                 trackPdgPos[1] = 2212;
//                 
//                 active[0] |= isSecondary && int_m(posPDG == -1);
//                 active[1]  = isSecondary && (int_m(posPDG == -1) || (posPDG > 1000000000) || (posPDG == 211));
//               }

              active[0] &= int_m(closeDaughters);
              active[1] &= int_m(closeDaughters);
              
              if(iTC==0) 
              {
                nPDGPos = 1;
                active[0] = (negInd < negTracksSize) && (int_v::IndexesFromZero() < int_v(NTracks));
              }

              for(int iPDGPos=0; iPDGPos<nPDGPos; iPDGPos++)
              {
                if(active[iPDGPos].isEmpty()) continue;
                
                //detetrmine a pdg code of the mother particle
                
                int_v motherPDG(-1);
                
                if(!fMixedEventAnalysis)
                {
                  if(iTC==0)
                  {
                    motherPDG( (abs(trackPdgPos[iPDGPos]) == 11) || int_m(abs(trackPdgNeg) == 11) || isSecondary ) = 22; //gamma -> e+ e-
                  }
                  else if(iTC==1)
                  {
                    motherPDG( isPrimary   && (abs(trackPdgPos[iPDGPos])== 13 || abs(trackPdgPos[iPDGPos])==19)
                                           && (int_m(abs(trackPdgNeg) == 13) || int_m(abs(trackPdgNeg) == 19)) ) =   200113; //rho -> mu+ mu-
                  }
                  else if(iTC==2)
                  {
                    motherPDG( isSecondary && (abs(trackPdgPos[iPDGPos])==        211) && int_m(abs(trackPdgNeg) ==  211) ) =   310; //K0 -> pi+ pi-
                    motherPDG( isSecondary && (abs(trackPdgPos[iPDGPos])==       2212) && int_m(abs(trackPdgNeg) ==  211) ) =  3122; //Lambda -> p+ pi-
                    motherPDG( isSecondary && (abs(trackPdgPos[iPDGPos])== 1000010020) && int_m(abs(trackPdgNeg) ==  211) ) =  3003; //LambdaN -> d+ pi-
                    motherPDG( isSecondary && (abs(trackPdgPos[iPDGPos])== 1000010030) && int_m(abs(trackPdgNeg) ==  211) ) =  3103; //LambdaNN -> t+ pi-
                    motherPDG( isSecondary && (abs(trackPdgPos[iPDGPos])== 1000020030) && int_m(abs(trackPdgNeg) ==  211) ) =  3004; //H3Lambda -> He3+ pi-
                    motherPDG( isSecondary && (abs(trackPdgPos[iPDGPos])== 1000020040) && int_m(abs(trackPdgNeg) ==  211) ) =  3005; //H4Lambda -> He4+ pi-
                    motherPDG( isSecondary && (abs(trackPdgPos[iPDGPos])==        321) && int_m(abs(trackPdgNeg) ==  211) ) =  -421; //D0_bar -> pi- K+
                    motherPDG( isPrimary   && (abs(trackPdgPos[iPDGPos])==        321) && int_m(abs(trackPdgNeg) ==  211) ) =   313; //K*0 -> K+ pi-
                    motherPDG( isPrimary   && (abs(trackPdgPos[iPDGPos])==        211) && int_m(abs(trackPdgNeg) ==  211) ) =   113; //rho -> pi+ pi-
                    motherPDG( isPrimary   && (abs(trackPdgPos[iPDGPos])==       2212) && int_m(abs(trackPdgNeg) ==  211) ) =  2114; //Delta0 -> p pi-
                  }
                  else if(iTC==3)
                  {
                    motherPDG( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && int_m(abs(trackPdgNeg) ==  321) ) =   421; //D0 -> pi+ K-
                    motherPDG( isSecondary && (abs(trackPdgPos[iPDGPos])==  321) && int_m(abs(trackPdgNeg) ==  321) ) =   426; //D0 -> K+ K-
                    motherPDG( isPrimary   && (abs(trackPdgPos[iPDGPos])==  211) && int_m(abs(trackPdgNeg) ==  321) ) =  -313; //K*0_bar -> K- pi+
                    motherPDG( isPrimary   && (abs(trackPdgPos[iPDGPos])== 2212) && int_m(abs(trackPdgNeg) ==  321) ) =  3124; //Lambda* -> p K-
                    motherPDG( isPrimary   && (abs(trackPdgPos[iPDGPos])==  321) && int_m(abs(trackPdgNeg) ==  321) ) =   333; //phi -> K+ K-
                  }
                  else if(iTC==4)
                  {
                    motherPDG( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && int_m(abs(trackPdgNeg) ==       2212) ) =  -3122; //Lambda_bar -> p- pi+
                    motherPDG( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && int_m(abs(trackPdgNeg) == 1000010020) ) =  -3003; //LambdaN_bar -> d- pi+
                    motherPDG( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && int_m(abs(trackPdgNeg) == 1000010030) ) =  -3103; //LambdaNN_bar -> t- pi+
                    motherPDG( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && int_m(abs(trackPdgNeg) == 1000020030) ) =  -3004; //H3Lambda_bar -> He3- pi+
                    motherPDG( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && int_m(abs(trackPdgNeg) == 1000020040) ) =  -3005; //H4Lambda_bar -> He4- pi+
                    motherPDG( isPrimary   && (abs(trackPdgPos[iPDGPos])==  321) && int_m(abs(trackPdgNeg) ==       2212) ) =  -3124; //Lambda*_bar -> p- K+
                    motherPDG( isPrimary   && (abs(trackPdgPos[iPDGPos])== 2212) && int_m(abs(trackPdgNeg) ==       2212) ) = 200443; //JPsi -> p- p
                    motherPDG( isPrimary   && (abs(trackPdgPos[iPDGPos])==  211) && int_m(abs(trackPdgNeg) ==       2212) ) =  -2114; //Delta0_bar -> p- pi+
                  }
                }
                else
                {
                  if(iTC==0)
                    motherPDG(                (abs(trackPdgPos[iPDGPos])==   11) && int_m(abs(trackPdgNeg) ==   11) ) =    22; //gamma -> e+ e-
                  else if(iTC==1)
                    motherPDG( isPrimary   && (abs(trackPdgPos[iPDGPos])== 13 || abs(trackPdgPos[iPDGPos])==19)
                                           && (int_m(abs(trackPdgNeg) == 13) || int_m(abs(trackPdgNeg) == 19)) ) =   200113; //rho -> mu+ mu-
                  else if(iTC==2)
                    motherPDG( isSecondary && (abs(trackPdgPos[iPDGPos])==        321) && int_m(abs(trackPdgNeg) ==  211) ) =  -421; //D0_bar -> pi- K+
                  else if(iTC==3)
                    motherPDG( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && int_m(abs(trackPdgNeg) ==  321) ) =   421; //D0 -> pi+ K-
                }
                
//                 if( (iTrTypeNeg == 0) && (iTrTypePos == 0) )
//                 {
//                   float_v chiprimCut = fCuts2D[0];
//                   chiprimCut( float_m(abs(motherPDG) == 421 || abs(motherPDG) == 426) ) = fCutCharmChiPrim;
//                   active[iPDGPos] &= int_m(chiPrimNeg > chiprimCut) && int_m(chiPrimPos > chiprimCut);
//                 }
                
                active[iPDGPos] &= (motherPDG != -1);
                if(!(fDecayReconstructionList.empty()))
                {
                  for(int iV=0; iV<float_vLen; iV++)
                  {
                    if(!(active[iPDGPos][iV])) continue;
                    if(fDecayReconstructionList.find(motherPDG[iV]) == fDecayReconstructionList.end())
                    {
                      active[iPDGPos][iV] = false;
                      motherPDG[iV] = -1;
                    }
                  }
                }
                if(active[iPDGPos].isEmpty()) continue;

                if(!( (iTrTypePos == 1) && (iTrTypeNeg == 1) ) )
                {
                  active[iPDGPos] &= int_m(daughterNeg.GetDistanceFromParticle(daughterPos) < float_v(fDistanceCut));
                  if(active[iPDGPos].isEmpty()) continue;
                }
                
                const float_v& ptNeg2 = daughterNeg.Px()*daughterNeg.Px() + daughterNeg.Py()*daughterNeg.Py();
                const float_v& ptPos2 = daughterPos.Px()*daughterPos.Px() + daughterPos.Py()*daughterPos.Py();
                if( !((abs(motherPDG) == 421 || abs(motherPDG) == 426).isEmpty()) )
                {
//                   std::cout << active[iPDGPos] << std::endl;

                  active[iPDGPos] &= ( (abs(motherPDG) == 421 || abs(motherPDG) == 426) && 
                                      int_m(ptNeg2 >= fCutCharmPt*fCutCharmPt) && 
                                      int_m(ptPos2 >= fCutCharmPt*fCutCharmPt) &&
                                      int_m(chiPrimNeg > fCutCharmChiPrim) && int_m(chiPrimPos > fCutCharmChiPrim) &&
                                      int_m(negNPixelHits >= int_v(3)) && int_m(posNPixelHits >= int_v(3)) )
                                    || (!(abs(motherPDG) == 421 || abs(motherPDG) == 426));
//                   std::cout << "chiPrimNeg "  << chiPrimNeg << " chiPrimPos " << chiPrimPos << " ptNeg2 " << ptNeg2 << " ptPos2 " << ptPos2 <<  std::endl;
//                   std::cout << "fCutCharmPt " << fCutCharmPt << " fCutCharmChiPrim " << fCutCharmChiPrim << std::endl;
//                   std::cout << active[iPDGPos] << std::endl;
//                   std::cin.get();
                }
                if( !((abs(motherPDG) == 200113).isEmpty()) )
                {
                  const float_v& pNeg2 = ptNeg2 + daughterNeg.Pz()*daughterNeg.Pz();
                  const float_v& pPos2 = ptPos2 + daughterPos.Pz()*daughterPos.Pz();
                  
//                   active[iPDGPos] &= ( (abs(motherPDG) == int_v(200113)) && 
//                                       int_m(ptNeg2 >= fCutLVMPt*fCutLVMPt) && 
//                                       int_m(ptPos2 >= fCutLVMPt*fCutLVMPt) && 
//                                       int_m(pNeg2 >= fCutLVMP*fCutLVMP) && 
//                                       int_m(pPos2 >= fCutLVMP*fCutLVMP)) 
//                                     || (!(abs(motherPDG) == int_v(200113)));
                }
                
                if(active[iPDGPos].isEmpty()) continue;

                for(int iV=0; iV<float_vLen; iV++)
                {
                  if(!(active[iPDGPos][iV])) continue;
                  

                  idPosDaughters[nBufEntry] = iTrP+iV;
                  idNegDaughters[nBufEntry] = negInd[iV];
                  
                  daughterPosPDG[nBufEntry] = trackPdgPos[iPDGPos][iV];
                  daughterNegPDG[nBufEntry] = trackPdgNeg[iV];
                  
                  if(motherPDG[iV] == 22)
                  {
                    daughterPosPDG[nBufEntry] = -11;
                    daughterNegPDG[nBufEntry] =  11;
                  }
                  
                  pvIndexMother[nBufEntry] = isPrimary[iV] ? negPVIndex[iV] : -1;
                  
                  if( iTrTypeNeg != iTrTypePos ) pvIndexMother[nBufEntry] = 0;
                  
                  V0PDG[nBufEntry] = motherPDG[iV];
                  
                  nBufEntry++;

                  if(int(nBufEntry) == float_vLen)
                  {
                    KFParticleDatabase::Instance()->GetMotherMass(V0PDG,massMotherPDG,massMotherPDGSigma);
                    mother.SetPDG( V0PDG );
                    ConstructV0(vTracks, trTypeIndexPos[iTrTypePos], trTypeIndexNeg[iTrTypeNeg],                
                                idPosDaughters, idNegDaughters, daughterPosPDG, daughterNegPDG,
                                mother, mother_temp,
                                nBufEntry, l, dl, Particles, PrimVtx,
                                cuts, pvIndexMother, secCuts, massMotherPDG,
                                massMotherPDGSigma, motherPrimSecCand, nPrimSecCand, vMotherPrim, vMotherSec);
                    nBufEntry = 0; 
                  }
                  
                  //TODO optimize this part of code for D-mesons
                  if(motherPDG[iV] == 310 && 
                     negNPixelHits[iV] >= 3 && posNPixelHits[iV] >= 3 &&
                     chiPrimNeg[iV] > fCutCharmChiPrim && chiPrimPos[iV] > fCutCharmChiPrim &&
                     ptNeg2[iV] >= fCutCharmPt*fCutCharmPt && ptPos2[iV] >= fCutCharmPt*fCutCharmPt )
                  {
                    idPosDaughters[nBufEntry] = iTrP+iV;
                    idNegDaughters[nBufEntry] = negInd[iV];
                    
                    daughterPosPDG[nBufEntry] = trackPdgPos[iPDGPos][iV];
                    daughterNegPDG[nBufEntry] = trackPdgNeg[iV];
                    
                    pvIndexMother[nBufEntry] = isPrimary[iV] ? negPVIndex[iV] : -1;
                    
                    V0PDG[nBufEntry] = 420;
                    
                    nBufEntry++;

                    if(int(nBufEntry) == float_vLen)
                    {
                      KFParticleDatabase::Instance()->GetMotherMass(V0PDG,massMotherPDG,massMotherPDGSigma);
                      mother.SetPDG( V0PDG );
                      ConstructV0(vTracks, trTypeIndexPos[iTrTypePos], trTypeIndexNeg[iTrTypeNeg],                
                                  idPosDaughters, idNegDaughters, daughterPosPDG, daughterNegPDG,
                                  mother, mother_temp,
                                  nBufEntry, l, dl, Particles, PrimVtx,
                                  cuts, pvIndexMother, secCuts, massMotherPDG,
                                  massMotherPDGSigma, motherPrimSecCand, nPrimSecCand, vMotherPrim, vMotherSec);
                      nBufEntry = 0; 
                    }
                  }
                  
                }//iV
              }//iPDGPos
            }//iRot
          }//iTrP
        }//iTrN
        
        if( nBufEntry>0 )
        {
          for(int iV=nBufEntry; iV<float_vLen; iV++)
          {
            idPosDaughters[iV] = idPosDaughters[0];
            idNegDaughters[iV] = idNegDaughters[0];
          }

          KFParticleDatabase::Instance()->GetMotherMass(V0PDG,massMotherPDG,massMotherPDGSigma);
          mother.SetPDG( V0PDG );

          ConstructV0(vTracks, trTypeIndexPos[iTrTypePos], trTypeIndexNeg[iTrTypeNeg],              
                      idPosDaughters, idNegDaughters, daughterPosPDG, daughterNegPDG,
                      mother, mother_temp,
                      nBufEntry, l, dl, Particles, PrimVtx,
                      cuts, pvIndexMother, secCuts, massMotherPDG,
                      massMotherPDGSigma, motherPrimSecCand, nPrimSecCand, vMotherPrim, vMotherSec);
          nBufEntry = 0; 
        }
        
        if(nPrimSecCand>0)
        {
          SaveV0PrimSecCand(motherPrimSecCand,nPrimSecCand,mother_temp,PrimVtx,secCuts,vMotherPrim,vMotherSec);
          nPrimSecCand = 0;
        }
      }//iTC
    }//iTrTypeNeg
  }//iTrTypePos
}

void KFParticleFinder::ConstructPrimaryBG(KFPTrackVector* vTracks,
                                          vector<KFParticle>& Particles,
                                          std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                                          const float* cuts,
                                          const float* secCuts,
                                          vector< vector<KFParticle> >* vMotherPrim,
                                          vector<KFParticle>* vMotherSec )
{ 
  KFParticle mother_temp;
  KFParticleSIMD mother;
  kfvector_floatv l(fNPV), dl(fNPV);

  KFParticleSIMD daughterNeg, daughterPos;

  // for secondary V0
  unsigned int nBufEntry = 0;
  float_v dS;
  uint_v idNegDaughters;
  uint_v idPosDaughters;
  int_v daughterPosPDG(-1);
  int_v daughterNegPDG(-1);
    
  int_v pvIndexMother(-1);
  
  float_v massMotherPDG(Vc::Zero), massMotherPDGSigma(Vc::Zero);
  int_v V0PDG(Vc::Zero);

  KFParticleSIMD motherPrimSecCand;
  int nPrimSecCand =0;
  
  for(int iSet=2; iSet<4; iSet++)
  {
    int signPDG = 1;
    if(iSet == 3)
      signPDG = -1;
    
    KFPTrackVector& positivePrimaryTracks = vTracks[iSet];
    int nPositiveTracks = positivePrimaryTracks.Size();

    for(int iTrN = positivePrimaryTracks.FirstPion(); iTrN < positivePrimaryTracks.LastProton(); iTrN += float_vLen)
    {
      int_v negPDG = positivePrimaryTracks.PDG()[iTrN];
      int_v negPVIndex = positivePrimaryTracks.PVIndex()[iTrN];
      
      int_m activeNeg = (negPDG != -1);

      for(int iTrP = iTrN+1; iTrP < positivePrimaryTracks.LastProton(); iTrP += float_vLen)
      {
        const int NTracks = (iTrP + float_vLen < nPositiveTracks) ? float_vLen : (nPositiveTracks - iTrP);

        int_v posPDG(0); // = reinterpret_cast<const int_v&>(positivePrimaryTracks.PDG()[iTrP]);
        int_v posPVIndex(0); // = reinterpret_cast<const  int_v&>(positivePrimaryTracks.PVIndex()[iTrP]);              
        for(int iV=0; iV<NTracks; iV++)
        {
          posPDG[iV] = positivePrimaryTracks.PDG()[iTrP+iV];
          posPVIndex[iV] = positivePrimaryTracks.PVIndex()[iTrP+iV];
        }

        int_m active = (activeNeg && (int_v::IndexesFromZero() < int_v(NTracks)));
        if(active.isEmpty()) continue;
        
        active &= (posPDG != int_v(-1));
        active &= (posPVIndex == negPVIndex);
        
        if(active.isEmpty()) continue;
        
        //detetrmine a pdg code of the mother particle
        
        int_v motherPDG(-1);
        
        motherPDG( (abs(posPDG)==  211) && int_m(abs(negPDG) ==  211) ) = signPDG*9001; //pi+pi+
        motherPDG( (abs(posPDG)==  321) && int_m(abs(negPDG) ==  211) ) = signPDG*9002; //pi+K+
        motherPDG( (abs(posPDG)==  211) && int_m(abs(negPDG) ==  321) ) = signPDG*9002; //pi+K+
        motherPDG( (abs(posPDG)==  211) && int_m(abs(negPDG) == 2212) ) = signPDG*2224; //pi+p
        motherPDG( (abs(posPDG)== 2212) && int_m(abs(negPDG) ==  211) ) = signPDG*2224; //pi+p
        motherPDG( (abs(posPDG)==  321) && int_m(abs(negPDG) ==  321) ) = signPDG*9003; //K+K+
        motherPDG( (abs(posPDG)==  321) && int_m(abs(negPDG) == 2212) ) = signPDG*9004; //K+p
        motherPDG( (abs(posPDG)== 2212) && int_m(abs(negPDG) ==  321) ) = signPDG*9004; //K+p
        
        active &= (motherPDG != -1);
        if(!(fDecayReconstructionList.empty()))
        {
          for(int iV=0; iV<float_vLen; iV++)
          {
            if(!(active[iV])) continue;
            if(fDecayReconstructionList.find(motherPDG[iV]) == fDecayReconstructionList.end())
            {
              active[iV] = false;
              motherPDG[iV] = -1;
            }
          }
        }
        if(active.isEmpty()) continue;

        
        if(active.isEmpty()) continue;

        for(int iV=0; iV<NTracks; iV++)
        {
          if(!(active[iV])) continue;
          
          idPosDaughters[nBufEntry] = iTrP+iV;
          idNegDaughters[nBufEntry] = iTrN;
          
          daughterPosPDG[nBufEntry] = posPDG[iV];
          daughterNegPDG[nBufEntry] = negPDG[iV];
          
          pvIndexMother[nBufEntry] = negPVIndex[iV];
          
          V0PDG[nBufEntry] = motherPDG[iV];
          
          nBufEntry++;

          if(int(nBufEntry) == float_vLen)
          {
            KFParticleDatabase::Instance()->GetMotherMass(V0PDG,massMotherPDG,massMotherPDGSigma);
            mother.SetPDG( V0PDG );
            ConstructV0(vTracks, iSet, iSet,                
                        idPosDaughters, idNegDaughters, daughterPosPDG, daughterNegPDG,
                        mother, mother_temp,
                        nBufEntry, l, dl, Particles, PrimVtx,
                        cuts, pvIndexMother, secCuts, massMotherPDG,
                        massMotherPDGSigma, motherPrimSecCand, nPrimSecCand, vMotherPrim, vMotherSec);
            nBufEntry = 0; 
          }
        }//iV
      }//iTrP
          
      if( nBufEntry>0 )
      {
        for(int iV=nBufEntry; iV<float_vLen; iV++)
        {
          idPosDaughters[iV] = idPosDaughters[0];
          idNegDaughters[iV] = idNegDaughters[0];
        }

        KFParticleDatabase::Instance()->GetMotherMass(V0PDG,massMotherPDG,massMotherPDGSigma);
        mother.SetPDG( V0PDG );

        ConstructV0(vTracks, iSet, iSet,              
                    idPosDaughters, idNegDaughters, daughterPosPDG, daughterNegPDG,
                    mother, mother_temp,
                    nBufEntry, l, dl, Particles, PrimVtx,
                    cuts, pvIndexMother, secCuts, massMotherPDG,
                    massMotherPDGSigma, motherPrimSecCand, nPrimSecCand, vMotherPrim, vMotherSec);
        nBufEntry = 0; 
      }
      
      if(nPrimSecCand>0)
      {
        SaveV0PrimSecCand(motherPrimSecCand,nPrimSecCand,mother_temp,PrimVtx,secCuts,vMotherPrim,vMotherSec);
        nPrimSecCand = 0;
      }
    }
  }
}

void KFParticleFinder::ConstructTrackV0Cand(KFPTrackVector& vTracks,
                                            uint_v& idTracks,
                                            int_v& trackPDG,
                                            KFParticle* vV0[],
                                            KFParticleSIMD& mother,
                                            std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& motherTopo,
                                            KFParticle& mother_temp,
                                            const unsigned short nElements,
                                            kfvector_floatv& l,
                                            kfvector_floatv& dl,
                                            std::vector<KFParticle>& Particles,
                                            std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                                            const float_v* cuts,
                                            const int_v& pvIndex,
                                            const float_v& massMotherPDG,
                                            const float_v& massMotherPDGSigma,
                                            std::vector< std::vector<KFParticle> >* vMotherPrim,
                                            std::vector<KFParticle>* vMotherSec)
{
  float_m isPrimary(pvIndex>-1);
  
  int_v trackId( &(vTracks.Id()[0]), idTracks );
  KFParticleSIMD V0(vV0,nElements);
  KFParticleSIMD track(vTracks, idTracks, trackPDG);
  track.SetId(trackId);
    
  float_m isSameParticle = float_m((abs(mother.PDG()) ==    int_v(4122)) ||
                                   (abs(mother.PDG()) ==  int_v(114122)) ||
                                   (abs(mother.PDG()) ==  int_v(204122)) ||
                                   (abs(mother.PDG()) ==  int_v(504122)) ||
                                   (abs(mother.PDG()) ==  int_v(404122)) ||
                                   (abs(mother.PDG()) ==     int_v(425)) ||
                                   (abs(mother.PDG()) ==     int_v(427)) ||
                                   (abs(mother.PDG()) ==  int_v(200411)) ||
                                   (abs(mother.PDG()) ==  int_v(300411)) ||
                                   (abs(mother.PDG()) ==  int_v(300431)) ||
                                   (abs(mother.PDG()) ==  int_v(400431)) ||
                                   (abs(mother.PDG()) ==     int_v(411)) ||
                                   (abs(mother.PDG()) ==     int_v(431)) ||
                                   (abs(mother.PDG()) ==     int_v(429)) ||
                                   (abs(mother.PDG()) == int_v(1003334)) ||
                                   (abs(mother.PDG()) ==    int_v(3001)) ||
                                   (abs(mother.PDG()) ==    int_v(3006)) ||
                                   (abs(mother.PDG()) ==    int_v(3007)) ||
                                   (abs(mother.PDG()) ==    int_v(3009)) ||
                                   (abs(mother.PDG()) ==    int_v(3011)) );
  if( isSameParticle.isEmpty() )
  {
//     float_v ds[2] = {0.f,0.f};
//     float_v dsdr[4][6];
//     track.GetDStoParticle( V0, ds, dsdr );
//     track.TransportToDS(ds[0], dsdr[0]);
//     V0.TransportToDS(ds[1], dsdr[3]);

    const KFParticleSIMD* vDaughtersPointer[2] = {&track, &V0};
    mother.Construct(vDaughtersPointer, 2, 0);
  }
  else
  {
    int_v motherPDG = mother.PDG();
    mother = V0;
    mother.SetPDG(motherPDG);
    track.TransportToPoint(V0.Parameters());
    mother += track;
  }

  float_m active = float_m(int_v::IndexesFromZero() < int(nElements));
  
  float_m saveParticle(active);
  saveParticle &= (mother.Chi2()/static_cast<float_v>(mother.NDF()) < cuts[2] );
  saveParticle &= KFPMath::Finite(mother.GetChi2());
//   saveParticle &= float_m( mother.NDF() == int_v(1) );
  saveParticle &= (mother.GetChi2() > 0.0f);
  saveParticle &= (mother.GetChi2() == mother.GetChi2());

  if( saveParticle.isEmpty() ) { return; }

  int_m isSameTrack(false);
  for(unsigned int iD=0; iD<V0.DaughterIds().size(); iD++)
    isSameTrack |= ( int_v(V0.DaughterIds()[iD]) == int_v(trackId) );
  
  saveParticle &= ( !static_cast<float_m>(isSameTrack));
  if( saveParticle.isEmpty() ) { return; }        
      
  float_v lMin(1.e8f);
  float_v ldlMin(1.e8f);
  float_m isParticleFromVertex(false);

  for(int iP=0; iP<fNPV; iP++)
  {
    float_m isParticleFromVertexLocal;
    mother.GetDistanceToVertexLine(PrimVtx[iP], l[iP], dl[iP], &isParticleFromVertexLocal);
    isParticleFromVertex |= isParticleFromVertexLocal;
    float_v ldl = (l[iP]/dl[iP]);
    lMin( (l[iP] < lMin) && active) = l[iP];
    ldlMin( (ldl < ldlMin) && active) = ldl;
  }
  saveParticle &= (lMin < 200.f);
  saveParticle &= ((float_m(!isPrimary) && isParticleFromVertex) || float_m(isPrimary) );
  if( saveParticle.isEmpty() ) { return; }

  isSameParticle = isSameParticle || isPrimary;
  if(!((isSameParticle).isFull()))
  {
    float_m isParticleFromVertexLocal;
    float_v l1, dl1;
    V0.GetDistanceToVertexLine(mother, l1, dl1, &isParticleFromVertexLocal);
    
    saveParticle &= ( isSameParticle || ((!isSameParticle) && isParticleFromVertexLocal));
    if( saveParticle.isEmpty() ) { return; }
  }

  saveParticle &= ( (float_m(!isPrimary) && ldlMin > cuts[0]) || float_m(isPrimary) );

  int_m setLCut = abs(mother.PDG()) == 3312 || abs(mother.PDG()) == 3334 || abs(mother.PDG()) == 3001;
  saveParticle &= ( (float_m(setLCut) && lMin > float_v(fLCut)) || float_m(!setLCut) );

  ldlMin = 1.e8f;
  for(int iP=0; iP<fNPV; iP++)
  {
    motherTopo[iP] = mother;
    motherTopo[iP].SetProductionVertex(PrimVtx[iP]);
    motherTopo[iP].GetDecayLength(l[iP], dl[iP]);
    float_v ldl = (l[iP]/dl[iP]);
    ldlMin( (ldl < ldlMin) && active) = ldl;
  }

//   float_m isCharm = float_m( (abs(mother.PDG()) == 411)    ||
//                              (abs(mother.PDG()) == 427)    ||
//                              (abs(mother.PDG()) == 429)    ||
//                              (abs(mother.PDG()) == 431)    ||
//                              (abs(mother.PDG()) == 4122)   ||
//                              (abs(mother.PDG()) == 104122) ||
//                              (abs(mother.PDG()) == 114122) ||
//                              (abs(mother.PDG()) == 204122) ||
//                              (abs(mother.PDG()) == 304122) ||
//                              (abs(mother.PDG()) == 314122) ||
//                              (abs(mother.PDG()) == 404122) );
//   
//   saveParticle &= ( isCharm && lMin>0.025f ) || !isCharm;
                       
  vector<int> iPrimVert[float_vLen];
  float_m isPrimaryPart(false);

  for(int iP=0; iP<fNPV; iP++)
  {
    const float_v& motherTopoChi2Ndf = motherTopo[iP].GetChi2()/float_v(motherTopo[iP].GetNDF());
    const float_m isPrimaryPartLocal = ( motherTopoChi2Ndf < cuts[1] );
    isPrimaryPart |= isPrimaryPartLocal;
    for(int iV=0; iV<float_vLen; iV++)
    {
      if(isPrimaryPartLocal[iV])
        iPrimVert[iV].push_back(iP);
    }
  }
            
  for(unsigned int iv=0; iv<nElements; iv++)
  {
    if(!saveParticle[iv]) continue; 
    
    mother.GetKFParticle(mother_temp, iv);
    if( mother.PDG()[iv] == 3312 )
    {
      fLPi.push_back(mother_temp);
      fLPiPIndex.push_back( V0.DaughterIds()[1][iv] );
    }
    
//     if(  mother.PDG()[iv] == 100411 )
//     {
//       fK0PiPlus.push_back(mother_temp);
//       fK0PiMinusIndex.push_back( V0.DaughterIds()[0][iv] );
//       continue;
//     }
    
    //TODO check if needed with current implementation
    // reset daughter ids for 3- and 4-particle decays
    if( (abs(mother.PDG()[iv]) == 411)  ||
        (abs(mother.PDG()[iv]) == 429) ||
        (abs(mother.PDG()[iv]) == 431)  ||
        (abs(mother.PDG()[iv]) == 4122) ||
        (abs(mother.PDG()[iv]) == 114122) ||
        (abs(mother.PDG()[iv]) == 204122) ||
        (abs(mother.PDG()[iv]) == 314122) ||
        (abs(mother.PDG()[iv]) == 404122) ||
        (abs(mother.PDG()[iv]) == 504122) ||
        (abs(mother.PDG()[iv]) == 425) ||
        (abs(mother.PDG()[iv]) == 427) ||
        (abs(mother.PDG()[iv]) == 200411) ||
        (abs(mother.PDG()[iv]) == 300411) ||
        (abs(mother.PDG()[iv]) == 300431) ||
        (abs(mother.PDG()[iv]) == 400431) ||
        (abs(mother.PDG()[iv]) == 1003334) ||
        (abs(mother.PDG()[iv]) == 3001) )
    {
      mother_temp.CleanDaughtersId();
      for(int iD=0; iD < vV0[iv]->NDaughters(); iD++)
        mother_temp.AddDaughterId( vV0[iv]->DaughterIds()[iD] );
      mother_temp.AddDaughterId(trackId[iv]);
    }
    
    if( mother.PDG()[iv] == 411 ) 
    {
      fDPlus.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == -411 ) 
    {
      fDMinus.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == 300411 ) 
    {
      fDPlus3Pi.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == -300411 ) 
    {
      fDMinus3Pi.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == 400431 ) 
    {
      fDsPlusK2Pi.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == -400431 ) 
    {
      fDsMinusK2Pi.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == 504122 ) 
    {
      fLcPlusP2Pi.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == -504122 ) 
    {
      fLcMinusP2Pi.push_back(mother_temp);
      continue;
    }
//     if( mother.PDG()[iv] == 427 )
//     {
//       fK0PiPi.push_back(mother_temp);
//       continue;
//     }
    
    if( mother.PDG()[iv] == 429 ) 
    {
      fD04.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == -429 ) 
    {
      fD04bar.push_back(mother_temp);
      continue;
    }
    
    if( mother.PDG()[iv] == 3203 ) 
      fLLn.push_back(mother_temp);
    if( mother.PDG()[iv] == 3010 ) 
      fH5LL.push_back(mother_temp);
    
    mother_temp.SetId(Particles.size());

    if(!(isPrimaryPart[iv]))
    {
      if( vMotherSec )
      {
        float mass, errMass;
        mother_temp.GetMass(mass, errMass);
        if(abs(mother.PDG()[iv]) == 3324)
        {
          vMotherSec->push_back(mother_temp);
        }
        else
        {    
          if( (fabs(mass - massMotherPDG[iv])/massMotherPDGSigma[iv]) <= 3 )
          {
            KFParticle mother_sec = mother_temp;
            mother_sec.SetNonlinearMassConstraint(massMotherPDG[iv]);
            vMotherSec->push_back(mother_temp);
          }
        }
      }
      if(!(mother.PDG()[iv] == 3006 || mother.PDG()[iv] == 3007))
        continue;
    }
    
    //check Ds+ and Lc+ candidates not to be D+
//     if(abs(mother_temp.GetPDG())==431 || abs(mother_temp.GetPDG())==4122)
//     {
//       KFPTrack dPionTrack;
//       vTracks.GetTrack(dPionTrack, idTracks[iv]);
//       KFParticle dPion(dPionTrack, 211);
//       
//       KFParticle dMeson = *vV0[iv];
//       dMeson += dPion;
//       float dMass, dMassError;
//       dMeson.GetMass(dMass, dMassError);
//       if(fabs(dMass - KFParticleDatabase::Instance()->GetDPlusMass())/KFParticleDatabase::Instance()->GetDPlusMassSigma() < 3) continue;
//     }
    
    if(abs(mother.GetPDG()[iv]) == 521 || abs(mother.GetPDG()[iv]) == 529 || abs(mother.GetPDG()[iv]) == 511 || abs(mother.GetPDG()[iv]) == 519)
    {
      KFParticle daughter_temp = *vV0[iv];
      float massPDG = KFParticleDatabase::Instance()->GetDPlusMass();
      float massSigmaPDG = KFParticleDatabase::Instance()->GetDPlusMassSigma();
      if(abs(mother.GetPDG()[iv]) == 521 || abs(mother.GetPDG()[iv]) == 529)
      {
        massPDG = KFParticleDatabase::Instance()->GetD0Mass();
        massSigmaPDG = KFParticleDatabase::Instance()->GetD0MassSigma();
      }
      float mass, dm;
      daughter_temp.GetMass(mass,dm);
      if( (fabs(mass - massPDG)/massSigmaPDG) > 3 ) continue;
      
//       KFParticleSIMD daughter_tempSIMD(daughter_temp);
//       daughter_tempSIMD.SetProductionVertex(PrimVtx[0]);
//       if(daughter_tempSIMD.GetChi2()[0]/daughter_tempSIMD.GetNDF()[0] < 3. ) continue;
      
      daughter_temp.SetId(Particles.size());
      daughter_temp.SetPDG(-1);
      mother_temp.SetId(Particles.size()+1);
      mother_temp.CleanDaughtersId();
      mother_temp.AddDaughterId(Particles.size());
      mother_temp.AddDaughterId(trackId[iv]);
      Particles.push_back(daughter_temp);
    }
    Particles.push_back(mother_temp);

    if( abs(mother.GetPDG()[iv]) == 3334 ) //Omega-
    {
      float mass, errMass;
      mother_temp.GetMass(mass, errMass);
      
      vector< vector<KFParticle> >* motherVector = &fPrimCandidates[7];
      if( mother.GetPDG()[iv] == 3334 )
        motherVector = &fPrimCandidates[8];
        
      mother_temp.SetNonlinearMassConstraint(massMotherPDG[iv]);

      if( (fabs(mass - massMotherPDG[iv])/massMotherPDGSigma[iv]) <= 3 )
        for(unsigned int iP=0; iP<iPrimVert[iv].size(); iP++)
          (*motherVector)[iPrimVert[iv][iP]].push_back(mother_temp);
    }
    
    if(vMotherPrim)
    {
      if( !((abs(mother.GetPDG()[iv]) == 3312) || (abs(mother.GetPDG()[iv]) == 3324))) continue;
      float mass, errMass;

      mother_temp.GetMass(mass, errMass);
      if(abs(mother.PDG()[iv]) == 3324)
      {
        for(unsigned int iP=0; iP<iPrimVert[iv].size(); iP++)
          (*vMotherPrim)[iPrimVert[iv][iP]].push_back(mother_temp);
      }
      else
      {
        mother_temp.SetNonlinearMassConstraint(massMotherPDG[iv]);

        if( (fabs(mass - massMotherPDG[iv])/massMotherPDGSigma[iv]) <= 3 )
          for(unsigned int iP=0; iP<iPrimVert[iv].size(); iP++)
            (*vMotherPrim)[iPrimVert[iv][iP]].push_back(mother_temp);
      }
    }
  }
}

void KFParticleFinder::FindTrackV0Decay(vector<KFParticle>& vV0,
                                        const int V0PDG,
                                        KFPTrackVector& vTracks,
                                        const int q,
                                        const int firstTrack,
                                        const int lastTrack,
                                        vector<KFParticle>& Particles,    
                                        std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                                        int v0PVIndex,
                                        kfvector_float* ChiToPrimVtx,
                                        vector< vector<KFParticle> >* vMotherPrim,
                                        vector<KFParticle>* vMotherSec)
{
  if( (vV0.size() < 1) || ((lastTrack-firstTrack) < 1) ) return;
  KFParticle mother_temp;

  KFParticle* v0Pointer[float_v::Size];

  KFParticleSIMD mother, track;
  std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> > motherTopo(fNPV);

  kfvector_floatv l(fNPV), dl(fNPV);

  float_v cuts[3];

  // for secondary V0
  unsigned int nBufEntry = 0;
  float_v dS;
  uint_v idTrack;
  int_v trackPDGMother(-1);
    
  int_v pvIndexMother(-1);
  
  float_v massMotherPDG(Vc::Zero), massMotherPDGSigma(Vc::Zero);
  int_v motherParticlePDG(Vc::Zero);
//   Particles.reserve(Particles.size() + vV0.size());

  bool isCharm = ((abs(V0PDG) == 421) || (abs(V0PDG) == 411) || (abs(V0PDG) == 429) || (abs(V0PDG) == 420) || (abs(V0PDG) == 419)) && (v0PVIndex<0);

  for(unsigned int iV0=0; iV0 < vV0.size(); iV0++)
  {    
    int iNegDaughter = vV0[iV0].DaughterIds()[0];
    int iPosDaughter = vV0[iV0].DaughterIds()[1];
    
    for(int iTr=firstTrack; iTr < lastTrack; iTr += float_vLen)
    {
      const int NTracks = (iTr + float_vLen < lastTrack) ? float_vLen : (lastTrack - iTr);

      const int_v& trackPDG = reinterpret_cast<const int_v&>(vTracks.PDG()[iTr]);
      const int_v& trackPVIndex = reinterpret_cast<const  int_v&>(vTracks.PVIndex()[iTr]);
      
      const int_m& isTrackSecondary = (trackPVIndex < 0);
      const int_m& isSecondary = int_m( v0PVIndex < 0 ) && isTrackSecondary;
      const int_m& isPrimary   = int_m( v0PVIndex >= 0 ) && (!isTrackSecondary);
      const int_m& isSamePV = (isPrimary && (v0PVIndex == trackPVIndex)) || !(isPrimary);

      float_m closeDaughters = float_m(isSamePV) && float_m(int_v::IndexesFromZero() < int(NTracks));

//       if(v0PVIndex < 0)
//       {
//         KFParticleSIMD v0(vV0[iV0]);
//         track.Load(vTracks, iTr, trackPDG);
     
//         float_v dsV0, dsTrack;
//         float_v dsdrV0[6] = {0.f,0.f,0.f,0.f,0.f,0.f}; 
//         float_v dsdrTrack[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
//         float_v par1[8], cov1[36], par2[8], cov2[36];
//         v0.GetDStoParticle(track, dsV0, dsTrack);
//         v0.Transport(dsV0, dsdrV0, par1, cov1);
//         track.Transport(dsTrack, dsdrTrack, par2, cov2);
//   
//         const float_v& dx = par1[0] - par2[0];
//         const float_v& dy = par1[1] - par2[1];
//         const float_v& dz = par1[2] - par2[2];
//         const float_v& r2 = dx*dx + dy*dy + dz*dz;
//         
//         const float_v vtx[3] = {(par1[0] + par2[0])/2.f,
//                                 (par1[1] + par2[1])/2.f,
//                                 (par1[2] + par2[2])/2.f, };
// 
//         v0.CorrectErrorsOnS(par1, vtx, cov1);
//         track.CorrectErrorsOnS(par2, vtx, cov2);
//         
//         const float_v cov[6] = {cov1[0]+cov2[0],
//                                 cov1[1]+cov2[1],
//                                 cov1[2]+cov2[2],
//                                 cov1[3]+cov2[3],
//                                 cov1[4]+cov2[4],
//                                 cov1[5]+cov2[5] };
//         const float_v& err2 = cov[0]*dx*dx + cov[2]*dy*dy + cov[5]*dz*dz + 2.f*( cov[1]*dx*dy + cov[3]*dx*dz + cov[4]*dy*dz );
//                 
//         closeDaughters &= ( (r2 < float_v(1.f)) && (r2*r2/err2) < float_v(3.f) && isSecondary);
//         closeDaughters &= v0.GetDeviationFromParticle(track) < float_v(10.f);
//       }
      
      if(v0PVIndex < 0)
      {
        KFParticleSIMD v0(vV0[iV0]);
        track.Load(vTracks, iTr, trackPDG);
        closeDaughters &= v0.GetDistanceFromParticle(track) < float_v(fDistanceCut);
        if(closeDaughters.isEmpty()) continue;
      }
      
      int_v trackPdgPos[2];
      int_m active[2];

      int nPDGPos = 2;
      
      active[0] = int_m(closeDaughters);
      active[1] = (trackPDG == -1) && isSecondary && int_m(closeDaughters);
      
      trackPdgPos[0] = trackPDG;
      
      if( (trackPDG == -1).isEmpty() || (abs(V0PDG) ==  421) || (abs(V0PDG) ==  411) )
      {
        nPDGPos = 1;
      }
      else
      {
        trackPdgPos[0](trackPDG == -1) = q*211;
        nPDGPos = 1;//TODO
        trackPdgPos[1](isSecondary) = q*321;
      }

      for(int iPDGPos=0; iPDGPos<nPDGPos; iPDGPos++)
      {
        
        if(active[iPDGPos].isEmpty()) continue;
        
        //detetrmine a pdg code of the mother particle
        
        int_v motherPDG(-1);
        
        if( V0PDG == 3122 )
        {
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -211) ) =  3312;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  211) ) =  304122;
          motherPDG( isSecondary && int_m(abs(trackPdgPos[iPDGPos]) ==  321) ) =  3334;
          motherPDG( isPrimary   && int_m(trackPdgPos[iPDGPos] ==  211) ) =  3224; 
          motherPDG( isPrimary   && int_m(trackPdgPos[iPDGPos] ==  -211) ) =  3114;
          motherPDG( isPrimary   && int_m(trackPdgPos[iPDGPos] == -321) ) =   1003314; 
        }
        else if( V0PDG == -3122 )
        {
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  211) ) = -3312;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -211) ) = -304122;
          motherPDG( isSecondary && int_m(abs(trackPdgPos[iPDGPos]) ==  321) ) = -3334;
          motherPDG( isPrimary   && int_m(trackPdgPos[iPDGPos] ==  -211) ) =  -3224; 
          motherPDG( isPrimary   && int_m(trackPdgPos[iPDGPos] ==   211) ) =  -3114;
          motherPDG( isPrimary   && int_m(trackPdgPos[iPDGPos] ==  321) ) =  -1003314; 
        }
        else if( V0PDG == 310)
        {
          motherPDG( isPrimary && int_m(trackPdgPos[iPDGPos] ==  211) ) =  323; 
          motherPDG( isPrimary && int_m(trackPdgPos[iPDGPos] ==  -211) ) =  -323; 
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==   211) ) =  100411;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  -211) ) = -100411;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==   321) ) =  100431;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  -321) ) = -100431;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  2212) ) =  104122;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -2212) ) = -104122;
        }
        else if( V0PDG == 3312 )
          motherPDG( isPrimary && int_m(trackPdgPos[iPDGPos] ==  211) ) =  3324; 
        else if( V0PDG == -3312)
          motherPDG( isPrimary && int_m(trackPdgPos[iPDGPos] == -211) ) = -3324; 
        else if( V0PDG == 3324 )
          motherPDG( isPrimary && int_m(trackPdgPos[iPDGPos] == -321) ) =  1003334; 
        else if( V0PDG == -3324 )
          motherPDG( isPrimary && int_m(trackPdgPos[iPDGPos] ==  321) ) = -1003334;         
        else if(V0PDG ==  421)
        {
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  211) ) =  411;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  321) ) =  431;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == 2212) ) =  4122;
          const int_v& id = reinterpret_cast<const int_v&>(vTracks.Id()[iTr]);
          int_m isDMeson = isSecondary && int_m(trackPdgPos[iPDGPos] ==  211);
          active[iPDGPos] &= (!(isDMeson)) || (isDMeson && ( id > iPosDaughter) );
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -211) ) =  -521;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -321) ) =  -529;          
          motherPDG( isPrimary && int_m(trackPdgPos[iPDGPos] ==   211) ) =   10411; 
        }
        else if(V0PDG == -421)
        {
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -211) ) = -411;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -321) ) = -431;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==-2212) ) = -4122;
          const int_v& id = reinterpret_cast<const int_v&>(vTracks.Id()[iTr]);
          int_m isDMeson = isSecondary && int_m(trackPdgPos[iPDGPos] == -211);
          active[iPDGPos] &= (!(isDMeson)) || (isDMeson && ( id > iNegDaughter) );
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  211) ) = 521;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  321) ) = 529;
          motherPDG( isPrimary && int_m(trackPdgPos[iPDGPos] ==  -211) ) =  -10411; 
        }
        else if(V0PDG == 420 && q>0)
        {
          motherPDG( isSecondary && int_m(abs(trackPdgPos[iPDGPos]) ==  211) ) =  300411;
          motherPDG( isSecondary && int_m(abs(trackPdgPos[iPDGPos]) ==  321) ) =  400431;
          motherPDG( isSecondary && int_m(abs(trackPdgPos[iPDGPos]) == 2212) ) =  504122;
          const int_v& id = reinterpret_cast<const int_v&>(vTracks.Id()[iTr]);
          int_m isDMeson = isSecondary && int_m(abs(trackPdgPos[iPDGPos]) ==  211);
          active[iPDGPos] &= (!(isDMeson)) || (isDMeson && ( id > iPosDaughter) );
        }
        else if(V0PDG == 420 && q<0)
        {
          motherPDG( isSecondary && int_m(abs(trackPdgPos[iPDGPos]) ==  211) ) =  -300411;
          motherPDG( isSecondary && int_m(abs(trackPdgPos[iPDGPos]) ==  321) ) =  -400431;
          motherPDG( isSecondary && int_m(abs(trackPdgPos[iPDGPos]) == 2212) ) =  -504122;
          const int_v& id = reinterpret_cast<const int_v&>(vTracks.Id()[iTr]);
          int_m isDMeson = isSecondary && int_m(abs(trackPdgPos[iPDGPos]) ==  211);
          active[iPDGPos] &= (!(isDMeson)) || (isDMeson && ( id > iNegDaughter) );
        }
        else if(V0PDG == 411)
        {
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -211) ) = 429;
          motherPDG( isPrimary && int_m(trackPdgPos[iPDGPos] ==  -211) ) =   10421; 
        }
        else if(V0PDG == -411)
        {
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  211) ) = -429;
          motherPDG( isPrimary && int_m(trackPdgPos[iPDGPos] ==   211) ) =  -10421; 
        }
        else if(V0PDG == 419)
        {
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -211) ) = -511;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -321) ) = -519;
        }
        else if(V0PDG == -419)
        {
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == 211) ) =  511;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == 321) ) =  519;
        }
        else if(V0PDG == 429)      
          motherPDG( isPrimary && int_m(trackPdgPos[iPDGPos] ==   211) ) =   20411; 
        else if(V0PDG == -429)
          motherPDG( isPrimary && int_m(trackPdgPos[iPDGPos] ==  -211) ) =  -20411; 
        else if( V0PDG == 3002 )
        {
          const int_v& id = reinterpret_cast<const int_v&>(vTracks.Id()[iTr]);
          int_m isSameProton = (id == fLPiPIndex[iV0]);
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == 2212) && (!isSameProton)) =  3001; 
        }
        else if( V0PDG == 100411 )
        {
//           const int_v& id = reinterpret_cast<const int_v&>(vTracks.Id()[iTr]);
//           int_m isSamePiMinus = (id == fK0PiMinusIndex[iV0]);
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -211) ) =  425;           
        }
        else if( V0PDG == 100431 )
        {
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -321) ) =  427;
        }
        else if( V0PDG == 425)
        {
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  211) ) =  200411;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -211) ) = -200411;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  321) ) =  300431;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -321) ) = -300431;
        }
        else if( V0PDG == 111 )
        {
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == 2212) ) =  3222; 
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -2212) ) =  -3222; 
          motherPDG( isPrimary && int_m(trackPdgPos[iPDGPos] == 321) ) =  100323; 
          motherPDG( isPrimary && int_m(trackPdgPos[iPDGPos] == -321) ) =  -100323; 
        }
        else if( V0PDG == 3004 )
        {
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  2212) ) =  3006;
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  -211) ) =  3203; 
        }
        else if( V0PDG == -3004 )
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -2212) ) = -3006; 
        else if( V0PDG == 3005 )
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  2212) ) =  3007; 
        else if( V0PDG == -3005 )
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -2212) ) = -3007;
        else if( V0PDG == 3006 )
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -211) ) = 3008; 
        else if( V0PDG == 3007 )
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -211) ) = 3010; 
        else if( V0PDG == 3203 )
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == 2212) ) = 3009;
        else if( V0PDG == 3010 )
        {
          const int_v& id = reinterpret_cast<const int_v&>(vTracks.Id()[iTr]);
          int_m isSameProton = (id == Particles[vV0[iV0].DaughterIds()[1]].DaughterIds()[2]);
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == 2212) && (!isSameProton)) = 3011;
        }
        else if(V0PDG ==  304122)
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  211) ) =  314122;
        else if(V0PDG == -304122)
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -211) ) = -314122;          
        else if(V0PDG ==  314122)
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -211) ) =  404122;
        else if(V0PDG == -314122)
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  211) ) = -404122;
        else if(V0PDG ==  104122)
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  211) ) =  114122;
        else if(V0PDG == -104122)
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -211) ) = -114122;
        else if(V0PDG ==  114122)
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] == -211) ) =  204122;
        else if(V0PDG == -114122)
          motherPDG( isSecondary && int_m(trackPdgPos[iPDGPos] ==  211) ) = -204122;
        
        active[iPDGPos] &= (motherPDG != -1);
        if(!(fDecayReconstructionList.empty()))
        {
          for(int iV=0; iV<float_vLen; iV++)
          {
            if(!(active[iPDGPos][iV])) continue;
            if(fDecayReconstructionList.find(motherPDG[iV]) == fDecayReconstructionList.end())
            {
              active[iPDGPos][iV] = false;
              motherPDG[iV] = -1;
            }
          }
        }
        if(ChiToPrimVtx)
          active[iPDGPos] &= ( !( (abs(motherPDG) == 3334 || abs(motherPDG) == 3312 ) ) ||
                             ( (abs(motherPDG) == 3334 || abs(motherPDG) == 3312 ) && int_m(reinterpret_cast<const float_v&>((*ChiToPrimVtx)[iTr]) > float_v(fCuts2D[0])) ) );
        
        if(active[iPDGPos].isEmpty()) continue;
        
        if(isCharm)
        {
          track.Load(vTracks, iTr, trackPDG);
          const float_v& trackPt = track.Px()*track.Px() + track.Py()*track.Py();
          const int_v& nPixelHits = reinterpret_cast<const int_v&>(vTracks.NPixelHits()[iTr]);
          
          active[iPDGPos] &= int_m(trackPt >= fCutCharmPt*fCutCharmPt) && int_m(reinterpret_cast<const float_v&>((*ChiToPrimVtx)[iTr]) > fCutCharmChiPrim ) && int_m(nPixelHits >= int_v(3));
        }
        {
          int_m isCharmParticle = (abs(motherPDG) == 104122) ||
                                  (abs(motherPDG) == 204122) ||
                                  (abs(motherPDG) == 304122) ||
                                  (abs(motherPDG) == 404122) ||
                                  (abs(motherPDG) ==    425) ||
                                  (abs(motherPDG) ==    426) ||
                                  (abs(motherPDG) ==    427) ||
                                  (abs(motherPDG) == 100411) ||
                                  (abs(motherPDG) == 200411) ||
                                  (abs(motherPDG) == 100431) ||
                                  (abs(motherPDG) == 300431) ;
                 
          if(!(isCharmParticle.isEmpty()))
          {
            track.Load(vTracks, iTr, trackPDG);
            const float_v& trackPt = track.Px()*track.Px() + track.Py()*track.Py();
            const int_v& nPixelHits = reinterpret_cast<const int_v&>(vTracks.NPixelHits()[iTr]);
            
            active[iPDGPos] &= ( (int_m(trackPt >= fCutCharmPt*fCutCharmPt) && int_m(reinterpret_cast<const float_v&>((*ChiToPrimVtx)[iTr]) > fCutCharmChiPrim ) && int_m(nPixelHits >= int_v(3)) ) && isCharmParticle ) || (!isCharmParticle);
          }
        }
        
        for(int iV=0; iV<NTracks; iV++)
        {
          if(!(active[iPDGPos][iV])) continue;
          

          idTrack[nBufEntry] = iTr+iV;
          v0Pointer[nBufEntry] = &vV0[iV0];
          
          trackPDGMother[nBufEntry] = trackPdgPos[iPDGPos][iV];
          
          pvIndexMother[nBufEntry] = v0PVIndex;
          
          float massMother, massMotherSigma;
          KFParticleDatabase::Instance()->GetMotherMass(motherPDG[iV],massMother,massMotherSigma);

          massMotherPDG[nBufEntry] = massMother;
          massMotherPDGSigma[nBufEntry] = massMotherSigma;
          motherParticlePDG[nBufEntry] = motherPDG[iV];
                    
          int motherType = 0;

          switch (abs(motherPDG[iV]))
          {
            case   3312: motherType = 0; break; //Xi
            case   3334: motherType = 0; break; //Omega
            case   4122: motherType = 1; break; //LambdaC
            case 104122: motherType = 1; break; //LambdaC
            case 304122: motherType = 1; break; //LambdaC
            case 504122: motherType = 1; break; //LambdaC
            case    425: motherType = 1; break; //D0
            case    426: motherType = 1; break; //D0
            case    427: motherType = 1; break; //D0
            case 100411: motherType = 1; break; //D+
            case 300411: motherType = 1; break; //D+
            case 100431: motherType = 1; break; //Ds+
            case 400431: motherType = 1; break; //Ds+
            case    431: motherType = 1; break; //Ds+-
            case    411: motherType = 1; break; //D+-
            case    428: motherType = 1; break; //D0
            case    429: motherType = 1; break; //D0
            case    521: motherType = 1; break; //B+
            case    529: motherType = 1; break; //B+
            case    511: motherType = 1; break; //B0
            case    519: motherType = 1; break; //B0
            case   3001: motherType = 1; break; //H0
            case   3222: motherType = 1; break; //Sigma+
            case   3006: motherType = 1; break; //He4L
            case   3007: motherType = 1; break; //He5L
            case   3008: motherType = 1; break; //H4LL
            case   3009: motherType = 1; break; //H4LL
            case   3011: motherType = 1; break; //He6LL
            default:   motherType = 2; break; //resonances
          }
          for(int iCut=0; iCut<3; iCut++)
            cuts[iCut][nBufEntry] = fCutsTrackV0[motherType][iCut];

          nBufEntry++;

          if(int(nBufEntry) == float_vLen)
          {
            mother.SetPDG( motherParticlePDG );
            ConstructTrackV0Cand(vTracks,   
                                 idTrack, trackPDGMother, v0Pointer,
                                 mother, motherTopo, mother_temp,
                                 nBufEntry, l, dl, Particles, PrimVtx,
                                 cuts, pvIndexMother, massMotherPDG,
                                 massMotherPDGSigma, vMotherPrim, vMotherSec);
            nBufEntry = 0; 
          }
        }//iV
      }//iPDGPos
    }//iTr
  }

  if(nBufEntry > 0)
  {
    for(int iV=nBufEntry; iV<float_vLen; iV++)
      idTrack[iV] = idTrack[0];
    
    mother.SetPDG( motherParticlePDG );
    ConstructTrackV0Cand(vTracks,  
                          idTrack, trackPDGMother, v0Pointer,
                          mother, motherTopo, mother_temp,
                          nBufEntry, l, dl, Particles, PrimVtx,
                          cuts, pvIndexMother, massMotherPDG,
                          massMotherPDGSigma, vMotherPrim, vMotherSec);
    nBufEntry = 0; 
  }
}

void KFParticleFinder::SelectParticles(vector<KFParticle>& Particles,
                                       vector<KFParticle>& vCandidates,
                                       std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                                       const float& cutChi2Topo,
                                       const float& cutLdL,
                                       const float& mass,
                                       const float& massErr,
                                       const float& massCut)
{
  KFParticle* cand[float_vLen];
  int nCand = vCandidates.size();
  
  vector<KFParticle> newCandidates;
  kfvector_floatv l(fNPV), dl(fNPV);

  for(int iC=0; iC < nCand; iC += float_vLen)
  {
    int nEntries = (iC + float_vLen < nCand) ? float_vLen : (nCand - iC);

    for(int iv=0; iv<nEntries; iv++)
      cand[iv] = &vCandidates[iC+iv];

    KFParticleSIMD mother(cand,nEntries);
    
    float_m saveParticle(int_v::IndexesFromZero() < int(nEntries));

    float_v lMin(1.e8f);
    float_v ldlMin(1.e8f);
    float_m isParticleFromVertex(false);

    for(int iP=0; iP<fNPV; iP++)
    {
      float_m isParticleFromVertexLocal;
      mother.GetDistanceToVertexLine(PrimVtx[iP], l[iP], dl[iP], &isParticleFromVertexLocal);
      isParticleFromVertex |= isParticleFromVertexLocal;
      float_v ldl = (l[iP]/dl[iP]);
      lMin( (l[iP] < lMin) && saveParticle) = l[iP];
      ldlMin( (ldl < ldlMin) && saveParticle) = ldl;
    }
    saveParticle &= ldlMin > cutLdL;
    saveParticle &= (lMin < 200.f);
    saveParticle &= isParticleFromVertex;
    if( saveParticle.isEmpty() ) continue;

    KFParticleSIMD* candTopo = new KFParticleSIMD[fNPV];

    for(int iP=0; iP<fNPV; iP++)
    {
      candTopo[iP] = mother;
      candTopo[iP].SetProductionVertex(PrimVtx[iP]);
    }
    
    for(int iv=0; iv<nEntries; iv++)
    {
      if(!saveParticle[iv]) continue;
      
      bool isPrimary = 0;
      for(int iP=0; iP<fNPV; iP++)
      {
        if( !(KFPMath::Finite(candTopo[iP].GetChi2())[iv]) ) continue;
        if(!(candTopo[iP].GetChi2()[iv] > 0.0f)) continue;
        if(!(candTopo[iP].GetChi2()[iv]==candTopo[iP].GetChi2()[iv])) continue;
      
        if(float(candTopo[iP].GetChi2()[iv])/float(candTopo[iP].GetNDF()[iv]) <= cutChi2Topo )
          isPrimary = 1;
      }
      if(!isPrimary)
        continue;

      vCandidates[iC+iv].SetId(Particles.size());
      Particles.push_back(vCandidates[iC+iv]);
      
      float m, dm;
      vCandidates[iC+iv].GetMass(m,dm);
      if( (fabs(m - mass)/massErr) > massCut ) continue;

      vCandidates[iC+iv].SetNonlinearMassConstraint(mass);
      newCandidates.push_back(vCandidates[iC+iv]);
    }
    if(candTopo) delete [] candTopo;
  }
  
  vCandidates = newCandidates;
}

void KFParticleFinder::CombinePartPart(vector<KFParticle>& particles1,
                                       vector<KFParticle>& particles2,
                                       vector<KFParticle>& Particles,
                                       std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                                       const float* cuts,
                                       int iPV,
                                       const int MotherPDG,
                                       bool isSameInputPart,
                                       bool saveOnlyPrimary,
                                       vector< vector<KFParticle> >* vMotherPrim,
                                       vector<KFParticle>* vMotherSec,
                                       float massMotherPDG,
                                       float massMotherPDGSigma)
{
  if( (particles1.size() ==  0) || (particles2.size() ==  0) ) return;  
  if(!(fDecayReconstructionList.empty()) && (fDecayReconstructionList.find(MotherPDG) == fDecayReconstructionList.end())) return;

  KFParticle mother_temp;
  KFParticleSIMD mother;
  KFParticleSIMD *motherTopo = new KFParticleSIMD[fNPV];
  mother.SetPDG( MotherPDG );

  kfvector_floatv l(fNPV), dl(fNPV);

  KFParticle* tmpPart2[float_vLen];
  int nPart2 = particles2.size();

  bool isPrimary = (iPV >= 0);
  bool isCharm = (MotherPDG == 425) ||
                 (MotherPDG == 427) || 
                 (abs(MotherPDG) == 200411) ||
                 (abs(MotherPDG) == 404122) ||
                 (abs(MotherPDG) ==   4132) ||
                 (abs(MotherPDG) == 300431) ||
                 (abs(MotherPDG) == 204122);
  
  for(unsigned int iP1=0; iP1 < particles1.size(); iP1++)
  {
    KFParticleSIMD vDaughters[2] = {KFParticleSIMD(particles1[iP1]), KFParticleSIMD()};

    unsigned int startIndex=0;
    if(isSameInputPart) startIndex=iP1+1;
    for(int iP2=startIndex; iP2 < nPart2; iP2 += float_vLen)
    {
      int nElements = (iP2 + float_vLen < nPart2) ? float_vLen : (nPart2 - iP2);
      float_m active(int_v::IndexesFromZero() < int(nElements));

      for(int iv=0; iv<nElements; iv++)
        tmpPart2[iv] = &particles2[iP2+iv];

      vDaughters[1] = KFParticleSIMD(tmpPart2,nElements);

//       if( reconstructPi0 )
//       {
//         int indexOffset = fEmcClusters->Id()[0];
//         uint_v gammaIndex1( (unsigned int)0);
//         uint_v gammaIndex2( (unsigned int)0);
//         for(int iv=0; iv<nElements; iv++)
//         {
//           gammaIndex1[iv] = Particles[ particles2[iP2+iv].DaughterIds()[0] ].DaughterIds()[0] - indexOffset;
//           gammaIndex2[iv] = Particles[ particles2[iP2+iv].DaughterIds()[1] ].DaughterIds()[0] - indexOffset;
//         }
//         
//         KFParticleSIMD gamma1(*fEmcClusters, gammaIndex1, vDaughters[0]);
//         KFParticleSIMD gamma2(*fEmcClusters, gammaIndex2, vDaughters[0]);
//         const KFParticleSIMD* pi0Daughters[2] = {&gamma1, &gamma2};
//         
//         int_v gammaId = vDaughters[1].Id();
//         vDaughters[1].SetVtxGuess(vDaughters[0].X(), vDaughters[0].Y(), vDaughters[0].Z());
//         vDaughters[1].Construct(pi0Daughters, 2);
//         vDaughters[1].SetId(gammaId);
//         
//         float_v mass, dm;
//         vDaughters[1].GetMass(mass,dm);
//         const float& mPi0 = KFParticleDatabase::Instance()->GetPi0Mass();
//         const float& mPi0Sigma = KFParticleDatabase::Instance()->GetPi0MassSigma();
//         active &= (abs(mass - mPi0)/mPi0Sigma) < 3.f;
//         vDaughters[1].SetNonlinearMassConstraint(mPi0);
//         if(active.isEmpty()) continue;
//       }
      
      if(isCharm)
      {
        mother  = vDaughters[0];
        mother += vDaughters[1];
        mother.SetPDG( MotherPDG );
      }
      else
      {
        const KFParticleSIMD* vDaughtersPointer[2] = {&vDaughters[0], &vDaughters[1]};
        mother.Construct(vDaughtersPointer, 2, 0);
      }
  
      float_m saveParticle(active);
      saveParticle &= (mother.Chi2()/static_cast<float_v>(mother.NDF()) < cuts[2] );
      saveParticle &= KFPMath::Finite(mother.GetChi2());
      saveParticle &= (mother.GetChi2() >= 0.0f);
      saveParticle &= (mother.GetChi2() == mother.GetChi2());
      
      if( saveParticle.isEmpty() ) { continue; }

      int_m isSameTrack(false);
      for(unsigned int iD=0; iD<vDaughters[0].DaughterIds().size(); iD++)
        for(unsigned int iD1=0; iD1<vDaughters[1].DaughterIds().size(); iD1++)
          isSameTrack |= ( int_v(vDaughters[0].DaughterIds()[iD]) == int_v(vDaughters[1].DaughterIds()[iD1]) );
      saveParticle &= ( !static_cast<float_m>(isSameTrack));
      if( saveParticle.isEmpty() ) { continue; }        
      
      float_v lMin(1.e8f);
      float_v ldlMin(1.e8f);
      float_m isParticleFromVertex(false);

      for(int iP=0; iP<fNPV; iP++)
      {
        if( (iPV > -1) && (iP !=iPV) ) continue;
        float_m isParticleFromVertexLocal;
        mother.GetDistanceToVertexLine(PrimVtx[iP], l[iP], dl[iP], &isParticleFromVertexLocal);
        isParticleFromVertex |= isParticleFromVertexLocal;
        float_v ldl = (l[iP]/dl[iP]);
        lMin( (l[iP] < lMin) && active) = l[iP];
        ldlMin( (ldl < ldlMin) && active) = ldl;
      }
      saveParticle &= ( (float_m(!isPrimary) && ldlMin > cuts[0]) || float_m(isPrimary) );
      saveParticle &= (lMin < 200.f);
    
      int_m setLCut = abs(mother.PDG()) == 3000;
      saveParticle &= ( (float_m(setLCut) && lMin > float_v(fLCut)) || float_m(!setLCut) );

//         if(isPrimary && (float(ldlMin > 3) )) continue;
      saveParticle &= ((float_m(!isPrimary) && isParticleFromVertex) || float_m(isPrimary) );
      if( saveParticle.isEmpty() ) { continue; }

      float_m isSameParticle(isPrimary || isCharm);
      if(!((isSameParticle).isFull()))
      {
        float_m isParticleFromVertexLocal;
        float_v l1, dl1;
        vDaughters[0].GetDistanceToVertexLine(mother, l1, dl1, &isParticleFromVertexLocal);
        
        saveParticle &= ( isSameParticle || ((!isSameParticle) && isParticleFromVertexLocal));
        if( saveParticle.isEmpty() ) { continue; }
      }
  
      for(int iP=0; iP<fNPV; iP++)
      {
        if( (iPV > -1) && (iP !=iPV) ) continue;
        motherTopo[iP] = mother;
        motherTopo[iP].SetProductionVertex(PrimVtx[iP]);
      }
  
      vector<int> iPrimVert[float_vLen];
      float_m isPrimaryPart(false);

      for(int iP=0; iP<fNPV; iP++)
      {
        if( (iPV > -1) && (iP !=iPV) ) continue;
        const float_v& motherTopoChi2Ndf = motherTopo[iP].GetChi2()/float_v(motherTopo[iP].GetNDF());
        const float_m isPrimaryPartLocal = ( motherTopoChi2Ndf < float_v(cuts[1]) );
        isPrimaryPart |= isPrimaryPartLocal;
        for(int iV=0; iV<float_vLen; iV++)
        {
          if(isPrimaryPartLocal[iV])
            iPrimVert[iV].push_back(iP);
        }
      }
      
      for(int iv=0; iv<nElements; iv++)
      {
        if(!saveParticle[iv]) continue; 
        
        mother.GetKFParticle(mother_temp, iv);

        // reset daughter ids for 3- and 4-particle decays
        if( (abs(mother.PDG()[iv]) == 428))
        {
          mother_temp.CleanDaughtersId();
          for(int iD=0; iD < particles1[iP1].NDaughters(); iD++)
            mother_temp.AddDaughterId( particles1[iP1].DaughterIds()[iD] );
          mother_temp.AddDaughterId(tmpPart2[iv]->Id());
        }
        
        if(saveOnlyPrimary)
        {
          if(isPrimaryPart[iv])
          {
            mother_temp.SetId(Particles.size());
            Particles.push_back(mother_temp);
          }
        }
        else
        {
          mother_temp.SetId(Particles.size());
          Particles.push_back(mother_temp);
        }
        
        if(vMotherPrim || vMotherSec)
        {
          float mass, errMass;
          mother_temp.GetMass(mass, errMass);
          if( (fabs(mass - massMotherPDG)/massMotherPDGSigma) > 3.f ) continue;
          mother_temp.SetNonlinearMassConstraint(massMotherPDG);
          
          if(MotherPDG == 428)
          {
            mother_temp.CleanDaughtersId();
            for(int iD=0; iD < tmpPart2[iv]->NDaughters(); iD++)
              mother_temp.AddDaughterId( tmpPart2[iv]->DaughterIds()[iD] );
            for(int iD=0; iD < particles1[iP1].NDaughters(); iD++)
              mother_temp.AddDaughterId( particles1[iP1].DaughterIds()[iD] );            
          }
          
          if(vMotherSec && (!(isPrimaryPart[iv])) )
            vMotherSec->push_back(mother_temp);
          if(vMotherPrim)
            for(unsigned int iP=0; iP<iPrimVert[iv].size(); iP++)
              (*vMotherPrim)[iPrimVert[iv][iP]].push_back(mother_temp);
        }
      }
    }
  }
  
  if(motherTopo) delete [] motherTopo;
}


void KFParticleFinder::NeutralDaughterDecay(KFPTrackVector* vTracks,
                                          vector<KFParticle>& Particles)
{ 
  KFParticle mother_temp;
  KFParticleSIMD ChargedDaughter, MotherTrack;

  uint_v idMotherTrack;
  uint_v idChargedDaughter;
  int_v ChargedDaughterPDG(-1);
    
  int_v pvIndexMother(-1); 
  
  int outNeutralDaughterPDG[4][5]; //[iTC][iHypothesis]
  int outMotherPDG[4][5];
  
  int trTypeIndexMother[2] = {6,7};
  int trTypeIndexDaughter[2] = {0,1};

  for( int iTrTypeDaughter = 0; iTrTypeDaughter<2; iTrTypeDaughter++)
  {
    KFPTrackVector& DaughterTracks = vTracks[ trTypeIndexDaughter[iTrTypeDaughter] ];
    KFPTrackVector& MotherTracks = vTracks[ trTypeIndexMother[iTrTypeDaughter] ];

    int_v DaughterTracksSize = DaughterTracks.Size();
    int MotherTracksSize = MotherTracks.Size();

    //track categories
    int nTC = 4;
    int startTCMother[4] = {0,0,0,0};
    int endTCMother[4] = {0,0,0,0};
    int startTCDaughter[4] = {0,0,0,0};
    int endTCDaughter[4] = {0,0,0,0};

    nTC = 4;
    vector<int> nMotherHypothesis(nTC,0);
    vector< vector<int> > motherPDGHypothesis(nTC);
    vector< vector<float> > neutralDaughterMassHypothesis(nTC);


    //mu+, mu-
    startTCMother[0] = 0; endTCMother[0] = MotherTracksSize;
    startTCDaughter[0] = DaughterTracks.FirstMuon(); endTCDaughter[0] = DaughterTracks.LastMuon(); 

    nMotherHypothesis[0] = 2;


    motherPDGHypothesis[0].push_back(211);
    motherPDGHypothesis[0].push_back(321);

    neutralDaughterMassHypothesis[0].push_back(0.);
    neutralDaughterMassHypothesis[0].push_back(0.);

    outNeutralDaughterPDG[0][0]=-7000014;
    outNeutralDaughterPDG[0][1]=-8000014;

    outMotherPDG[0][0]=-7000211;
    outMotherPDG[0][1]=-7000321;

    //Pi+, Pi-
    startTCMother[1] = 0; endTCMother[1] = MotherTracksSize;
    startTCDaughter[1] = DaughterTracks.FirstPion(); endTCDaughter[1] = DaughterTracks.LastPion();

    nMotherHypothesis[1] = 5;

    motherPDGHypothesis[1].push_back(3112);
    motherPDGHypothesis[1].push_back(3222);
    motherPDGHypothesis[1].push_back(3312);
    motherPDGHypothesis[1].push_back(3334);
    motherPDGHypothesis[1].push_back(321);
    
    neutralDaughterMassHypothesis[1].push_back(0.939565);
    neutralDaughterMassHypothesis[1].push_back(0.939565);
    neutralDaughterMassHypothesis[1].push_back(1.115683);
    neutralDaughterMassHypothesis[1].push_back(1.31486);
    neutralDaughterMassHypothesis[1].push_back(0.1349766);
    
    outNeutralDaughterPDG[1][0]= 7002112;
    outNeutralDaughterPDG[1][1]=-8002112;
    outNeutralDaughterPDG[1][2]= 7003122;
    outNeutralDaughterPDG[1][3]= 7003322;
    outNeutralDaughterPDG[1][4]=-9000111;
    
    outMotherPDG[1][0]= 7003112;
    outMotherPDG[1][1]=-7003222;
    outMotherPDG[1][2]= 7003312;
    outMotherPDG[1][3]= 7003334;
    outMotherPDG[1][4]=-9000321;
    
    //K+, K-
    startTCMother[2] = 0; endTCMother[2] = MotherTracksSize;
    startTCDaughter[2] = DaughterTracks.FirstKaon(); endTCDaughter[2] = DaughterTracks.LastKaon();

    nMotherHypothesis[2] = 1;
    
    motherPDGHypothesis[2].push_back(3334);
    
    neutralDaughterMassHypothesis[2].push_back(1.115683);
    
    outNeutralDaughterPDG[2][0]= 8003122;
    
    outMotherPDG[2][0]= 8003334;

    //p+, p-
    startTCMother[3] = 0; endTCMother[3] = MotherTracksSize;
    startTCDaughter[3] = DaughterTracks.FirstProton(); endTCDaughter[3] = DaughterTracks.LastProton(); 

    nMotherHypothesis[3] = 1;
    
    motherPDGHypothesis[3].push_back(3222);
    
    neutralDaughterMassHypothesis[3].push_back(0.1349766);
    
    outNeutralDaughterPDG[3][0]=-8000111;
    
    outMotherPDG[3][0]=-8003222;
    
    

    for(int iTC=0; iTC<nTC; iTC++)
    {
      for(unsigned short iTrD=startTCDaughter[iTC]; iTrD < endTCDaughter[iTC]; iTrD += float_vLen)
      {
        const unsigned short NTracksDaughter = (iTrD + float_vLen < DaughterTracks.Size()) ? float_vLen : (DaughterTracks.Size() - iTrD);

        int_v DaughterInd = int_v::IndexesFromZero() + int(iTrD);

        int_v DaughterPDG = reinterpret_cast<const int_v&>(DaughterTracks.PDG()[iTrD]);
        int_v DaughterPVIndex = reinterpret_cast<const int_v&>(DaughterTracks.PVIndex()[iTrD]);
        int_v daughterId = reinterpret_cast<const int_v&>(DaughterTracks.Id()[iTrD]);
        
        int_v trackPdgDaughter = DaughterPDG;
        int_m activeDaughter = (DaughterPDG != -1);
        
        if( !((DaughterPDG == -1).isEmpty()) )
        {
          trackPdgDaughter(DaughterPVIndex<0 && (DaughterPDG == -1) ) = 211;
              
//             activeDaughter |= int_m(DaughterPVIndex < 0) && int_m(DaughterPDG == -1) ;
        }
        
        activeDaughter = (int_v::IndexesFromZero() < int(NTracksDaughter));
            
        ChargedDaughter.Load(DaughterTracks, iTrD, DaughterPDG);
        ChargedDaughter.SetId(daughterId);

        for(unsigned short iTrM=startTCMother[iTC]; iTrM < endTCMother[iTC]; iTrM += float_vLen)
        {
          const unsigned short NTracks = (iTrM + float_vLen < MotherTracksSize) ? float_vLen : (MotherTracksSize - iTrM);

          //const int_v& MotherPDG = reinterpret_cast<const int_v&>(MotherTracks.PDG()[iTrM]);
          //const int_v& MotherPVIndex = reinterpret_cast<const  int_v&>(MotherTracks.PVIndex()[iTrM]);              
          const int_v& motherTrackId = reinterpret_cast<const  int_v&>(MotherTracks.Id()[iTrM]);
          
          for(int iRot = 0; iRot<float_vLen; iRot++)
          {
            if(iRot>0)
            {
              DaughterPDG = DaughterPDG.rotated(1);
              DaughterPVIndex = DaughterPVIndex.rotated(1);
              DaughterInd = DaughterInd.rotated(1);
              trackPdgDaughter = trackPdgDaughter.rotated(1);
            
              ChargedDaughter.Rotate();

              activeDaughter = /*( (DaughterPDG != -1) || ( (DaughterPVIndex < 0) && (DaughterPDG == -1) ) ) &&*/ (DaughterInd < DaughterTracksSize);
            }
            
            int_v trackPdgMother;

            if(iTC==0)
              activeDaughter &= abs(DaughterPDG)==13;
            if(iTC==1)
              activeDaughter &= abs(DaughterPDG)==211;
            if(iTC==2)
              activeDaughter &= abs(DaughterPDG)==321;
            if(iTC==3)
              activeDaughter &= abs(DaughterPDG)==2212;
            if (activeDaughter.isEmpty()) continue;
            
            
            for(int iHypothesis=0; iHypothesis<nMotherHypothesis[iTC]; iHypothesis++)
            {
              if(!(fDecayReconstructionList.empty()) && (fDecayReconstructionList.find(motherPDGHypothesis[iTC][iHypothesis]) == fDecayReconstructionList.end()))
                continue;
              int_m active = activeDaughter && (int_v::IndexesFromZero() < int(NTracks));
              
              MotherTrack.Load(MotherTracks, iTrM, motherPDGHypothesis[iTC][iHypothesis]);
              
              float_v zMother = MotherTrack.Z();
              float_v zCD = ChargedDaughter.Z();
              
              //daughter particle should start after the last hit of a mother track
              active &= int_m(zCD >= (zMother - float_v(0.5f)));
              if( active.isEmpty() ) continue;
              
              KFParticleSIMD neutralDaughter = MotherTrack;
              //energy of the mother particle should be greater then of the daughter particle
              active &= int_m(neutralDaughter.E() > ChargedDaughter.E());
              if( active.isEmpty() ) continue;
              
              neutralDaughter.AddDaughterId(motherTrackId);
              neutralDaughter.NDF() = -1;
              neutralDaughter.Chi2() = 0.f;
              neutralDaughter.SubtractDaughter(ChargedDaughter);
              
              //decay point shoud be between mother and daughter tracks
              active &= int_m(neutralDaughter.Z() >= zMother - float_v(10.0f));
              active &= int_m(neutralDaughter.Z() <= zCD + float_v(10.0f));
              //set cut on chi2 of the fit of the neutral daughter
              active &= int_m(neutralDaughter.NDF() >= int_v(Vc::Zero));
              active &= int_m(neutralDaughter.Chi2()/float_v(neutralDaughter.NDF()) <= fCuts2D[1]);
              //fit should converge
              active &= int_m(neutralDaughter.Chi2() >= float_v(Vc::Zero));
              active &= int_m(neutralDaughter.Chi2() == neutralDaughter.Chi2());
              if( active.isEmpty() ) continue;
              
              //kill particle-candidates produced by clones
              active &= int_m( neutralDaughter.GetRapidity()<6.f && neutralDaughter.GetRapidity()>0.f);
              if ((iTC==1 && iHypothesis<4) || iTC==2)
                active &= int_m( !( (neutralDaughter.GetPt())<0.5f && neutralDaughter.GetRapidity()<0.5f ) );
              if (iTC==3)
                active &= int_m( !( (neutralDaughter.GetPt())<0.2f && neutralDaughter.GetRapidity()<1.f ) );
              if( active.isEmpty() ) continue;
              
              KFParticleSIMD neutralDaughterUnconstr = neutralDaughter;
              neutralDaughter.SetNonlinearMassConstraint(neutralDaughterMassHypothesis[iTC][iHypothesis]);
              
              const KFParticleSIMD* daughters[2] = {&neutralDaughter, &ChargedDaughter};
              KFParticleSIMD mother;
              mother.Construct(daughters, 2);
              
              //decay point shoud be between mother and daughter tracks
              active &= int_m(mother.Z() >= zMother);
              active &= int_m(mother.Z() <= zCD);
              //set cut on chi2 of the fit of the mother particle
              active &= int_m(mother.NDF() >= int_v(Vc::Zero));
              active &= int_m(mother.Chi2()/float_v(mother.NDF()) <= fCuts2D[1]);
              //fit should converge
              active &= int_m(mother.Chi2() >= float_v(Vc::Zero));
              active &= int_m(mother.Chi2() == mother.Chi2());
              if( active.isEmpty() ) continue;

              for(int iV=0; iV<NTracks; iV++)
              {
                if(!active[iV]) continue;
                
                neutralDaughterUnconstr.GetKFParticle(mother_temp, iV);
                int neutralId = Particles.size();
                mother_temp.SetId(neutralId);
                // if(iTC==0 && iHypothesis==0 && iTrTypeDaughter==1)
                if (iTrTypeDaughter==0)
                  mother_temp.SetPDG(-outNeutralDaughterPDG[iTC][iHypothesis]);
                else
                  mother_temp.SetPDG(outNeutralDaughterPDG[iTC][iHypothesis]);
                Particles.push_back(mother_temp);

//                   for (int i=0; i<mother_temp.NDaughters(); i++){
//                     std::cout << "Daughter ID="<< mother_temp.DaughterIds()[i] <<std::endl;
//                     std::cout << "Daughter PDG  "<< Particles[mother_temp.DaughterIds()[i]].GetPDG() <<std::endl;
//                   }
// 
//                    std::cin.get();
                mother.GetKFParticle(mother_temp, iV);
                mother_temp.SetId(Particles.size());
                mother_temp.CleanDaughtersId();
                mother_temp.AddDaughterId(ChargedDaughter.Id()[iV]);
                mother_temp.AddDaughterId(neutralId);
                // if(iTC==0 && iHypothesis==0 && iTrTypeDaughter==1)
                
                if (iTrTypeDaughter==0)  
                  mother_temp.SetPDG(-outMotherPDG[iTC][iHypothesis]);
                else
                  mother_temp.SetPDG(outMotherPDG[iTC][iHypothesis]);
                Particles.push_back(mother_temp);
                
//                   for (int i=0; i<mother_temp.NDaughters(); i++){
//                     std::cout << "Daughter ID="<< mother_temp.DaughterIds()[i] <<std::endl;
//                     std::cout << "Daughter PDG  "<< Particles[mother_temp.DaughterIds()[i]].GetPDG() <<std::endl;
//                   }
// 
//                    std::cin.get();
              }
            }
          }//iRot
        }//iTrM
      }//iTrD
    }//iTC
  }//iTrTypeDaughter
}

void KFParticleFinder::AddCandidate(const KFParticle& candidate, int iPV)
{
  //0 Ks, 1 Lambda,2 LambdaBar, 3 gamma, 4 pi0, 5 Xi, 6 XiBar, 7 Omega, 8 OmegaBar, 9 XiStar
  int iSet = -1;
  if(candidate.GetPDG() ==   310) iSet = 0;
  if(candidate.GetPDG() ==  3122) iSet = 1;
  if(candidate.GetPDG() == -3122) iSet = 2;
  if(candidate.GetPDG() ==    22) iSet = 3;
  if(candidate.GetPDG() ==   111) iSet = 4;
  if(candidate.GetPDG() ==  3312) iSet = 5;
  if(candidate.GetPDG() == -3312) iSet = 6;
  if(candidate.GetPDG() ==  3334) iSet = 7;
  if(candidate.GetPDG() == -3334) iSet = 8;
  
  if(iSet > -1)
  {
    if(iPV >= 0 && iPV<fNPV)
    {
      if(candidate.NDF() == 2)
        fPrimCandidates[iSet][iPV].push_back(candidate);
      if(candidate.NDF() == 3)
        fPrimCandidatesTopo[iSet][iPV].push_back(candidate);
      if(candidate.NDF() == 4)
        fPrimCandidatesTopoMass[iSet][iPV].push_back(candidate);
    }
    else if(iPV < 0)
    {
      fSecCandidates[iSet].push_back(candidate);
    }
  }
}

void KFParticleFinder::SetNPV(int nPV)
{
  fNPV = nPV;
  
  for(int iCandidates=0; iCandidates<fNPrimCandidatesSets; iCandidates++)
  {
    fPrimCandidates[iCandidates].clear();
    fPrimCandidates[iCandidates].resize(fNPV);
  }
  
  for(int iCandidates=0; iCandidates<fNPrimCandidatesTopoSets; iCandidates++)
  {
    fPrimCandidatesTopo[iCandidates].clear();
    fPrimCandidatesTopo[iCandidates].resize(fNPV);
    
    fPrimCandidatesTopoMass[iCandidates].clear();
    fPrimCandidatesTopoMass[iCandidates].resize(fNPV);
  }
}
