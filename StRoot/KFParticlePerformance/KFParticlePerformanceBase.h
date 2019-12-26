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

#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE

#ifndef KFParticlePERFORMANCEBASE_H
#define KFParticlePERFORMANCEBASE_H

#ifdef KFPWITHTRACKER
#include "AliHLTTPCCounters.h"

#include "AliHLTTPCPerformanceBase.h"

#include "AliHLTTPCCADef.h"
#include "AliHLTArray.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#endif

#include "KFPartEfficiencies.h"
#include "KFPVEfficiencies.h"

#include <map>
#include <string>

class TDirectory;
class TH1F;
class TH2F;
class TH3F;

class KFParticle;
class TProfile;
class TProfile2D;

/** @class KFParticlePerformanceBase
 ** @brief The base class for KFTopoPerformance.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The class contains a set of histograms, allocates memory for them, sets names and axis names.
 ** For each particle from the KF Particle Reconstruction scheme histograms with parameter distribution,
 ** efficiencies, fit QA, fit QA of daughters, histograms for the side bands method and histograms for
 ** multi-differential extraction of spectra are collected. Also, a set of histograms for quality of
 ** the reconstructed parameters of primary vertices is created: distribution of parameters; fit QA;
 ** fit QA of primary tracks; contamination of ghost, secondary (background) tracks and tracks from
 ** another primary vertex; efficiency.
 **/

class KFParticlePerformanceBase
#ifdef KFPWITHTRACKER
: public AliHLTTPCPerformanceBase
#endif
{
 public:

  KFParticlePerformanceBase();
  virtual ~KFParticlePerformanceBase(){};
    
    /// Histograms
  virtual void CreateHistos(std::string histoDir = "", TDirectory* outFile = 0, std::map<int,bool> decays = std::map<int,bool>());
  TDirectory* GetHistosDirectory() { return fHistoDir; } ///< Returns pointer to the ROOT directory with created histograms.

  /** Switch off collection of histograms requiring Monte Carlo information. Not to allocate memory should be called 
   ** before KFParticlePerformanceBase::CreateHistos(). **/
  void DoNotStoreMCHistograms()      { fStoreMCHistograms = 0; } 
  /** Switch off collection of histograms for primary and secondary candidates. Not to allocate memory should be called 
   ** before KFParticlePerformanceBase::CreateHistos(). **/
  void DoNotStorePrimSecHistograms() { fStorePrimSecHistograms = 0; }
  /** Switch off collection of Z-R histograms. Not to allocate memory should be called 
   ** before KFParticlePerformanceBase::CreateHistos(). **/
  void DoNotStoreZRHistograms()      { fStoreZRHistograms = 0; }
  
  /** Returns residual histogram with "iParameter" parameter for decay with "iDecay" number. */
  const TH1F* GetDecayResidual(const int iDecay, const int iParameter) const { return hFitQA[iDecay][iParameter];          }
  /** Returns pull histogram with "iParameter" parameter for decay with "iDecay" number. */
  const TH1F* GetDecayPull(const int iDecay, const int iParameter)     const { return hFitQA[iDecay][iParameter+nFitQA/2]; }
  
// efficiencies
  KFPartEfficiencies fParteff; ///< Object with reconstruction efficiency of short-lived particles.
  KFPVEfficiencies fPVeff; ///< Object with reconstruction efficiency of primary vertices defined by the reconstructed tracks.
  KFPVEfficiencies fPVeffMCReconstructable; ///< Object with reconstruction efficiency of primary vertices defined by the Monte Carlo tracks.
    
 protected:
  TString outfileName; ///< Name of the output file, where histograms will be stored.
  TDirectory* histodir; ///< Pointer to the ROOT directory, where histograms are created.

  int fNEvents; ///< Number of processed events.
  bool fStoreMCHistograms; ///< Flag showing if histograms requiring Monte Carlo information should be created and collected. "True" by default.
  bool fStorePrimSecHistograms; ///< Flag showing if histograms for primary and secondary candidates should be created and collected. "True" by default.
  bool fStoreZRHistograms; ///< Flag showing if Z-R histograms should be created and collected. "True" by default.

//histos
  static const int nFitQA = 16; ///< Number of fit QA histograms: residuals and pulls in X, Y, Z, Px, Py, Pz, E, M.
  TH1F *hFitDaughtersQA[KFPartEfficiencies::nParticles][nFitQA]; ///< Residuals and pulls of daughter particles at production point.
  TH1F *hFitQA[KFPartEfficiencies::nParticles][nFitQA]; ///< Residuals and pulls of the reconstructed particle: X, Y, Z at decay point, P, E, M - at production point
  TH1F *hFitQANoConstraint[KFPartEfficiencies::nParticles][nFitQA]; ///< Residuals and pulls of the particle with no constraints set.
  TH1F *hFitQAMassConstraint[KFPartEfficiencies::nParticles][nFitQA]; ///< Residuals and pulls of the particle with the mass constraint. 
  TH1F *hFitQATopoConstraint[KFPartEfficiencies::nParticles][nFitQA]; ///< Residuals and pulls of the particle with the production point constraint.
  TH1F *hFitQATopoMassConstraint[KFPartEfficiencies::nParticles][nFitQA]; ///< Residuals and pulls of the particle with the mass and production point constraints.

  static const int nDSToParticleQA = 7; ///< Number of histograms to evaluate GetDStoParticle function: residuals and pulls in X, Y, Z; distance between DCA points.
  TH1F *hDSToParticleQA[KFPartEfficiencies::nParticles][nDSToParticleQA]; ///< Histograms to evaluate KFParticleSIMD::GetDStoParticle() function
  
  /** \brief Number of histograms with parameter distributions: mass, p, pt, rapidity, decay length, c*tau, 
   ** chi/ndf, prob, theta, phi, X, Y, Z, R, L, L/dL, Mt, multiplicity. **/
  static const int nHistoPartParam = 18;
  /** Number of sets of histograms with parameter distributions: 0 - all candidates, 1 - reconstructed signal, 2 - physics background from other decays, 3 - combinatorial
   ** background (ghost), 4 - reconstructed signal for side bands method, 5- reconstructed background for side bands method, 6 - MC signal. **/
  static const int nParametersSet = 7;
  TH1F *hPartParam[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam]; ///< Parameters of all candidates.
  TH1F *hPartParamPrimary[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam]; ///< Parameters of primary candidates.
  TH1F *hPartParamPrimaryMass[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam]; ///< Parameters of primary candidates with mass constraint.
  TH1F *hPartParamPrimaryTopo[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam]; ///< Parameters of primary candidates with vertex constraint.
  TH1F *hPartParamPrimaryTopoMass[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam]; ///< Parameters of primary candidates with mass and vertex constraint.
  TH1F *hPartParamSecondary[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam]; ///< Parameters of secondary candidates.
  TH1F *hPartParamSecondaryMass[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam]; ///< Parameters of secondary candidates with mass constraint.

  static const int nHistoPartParam2D = 4; ///< Number of 2D histograms: 0 - y-pt, 1 - z-r, 2 - armenteros, 3- y-mt.
  TH2F *hPartParam2D[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam2D]; ///< 2D histograms for all candidates.
  TH2F *hPartParam2DPrimary[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam2D]; ///< 2D for primary candidates.
  TH2F *hPartParam2DPrimaryMass[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam2D]; ///< 2D for primary candidates with mass constraint.
  TH2F *hPartParam2DPrimaryTopo[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam2D]; ///< 2D for primary candidates with vertex constraint.
  TH2F *hPartParam2DPrimaryTopoMass[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam2D]; ///< 2D with mass and vertex constraints.
  TH2F *hPartParam2DSecondary[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam2D]; ///< 2D for secondary candidates.
  TH2F *hPartParam2DSecondaryMass[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam2D]; ///< 2D for secondary candidates with mass constraint.

  static const int nHistoPartParam3D = 6; ///< Number of 3D histograms: y-pt-M, y-mt-M, b-pt-M, b-y-M, b-mt-M, ct-pt-M
  TH3F *hPartParam3D[1][KFPartEfficiencies::nParticles][nHistoPartParam3D]; ///< 3D histograms.

  static const int nPartEfficiency = 9; ///< Number of efficiency plots for each decay: vs p, pt, y, z, c*tau, decay length, l, r, Mt.
  TProfile* hPartEfficiency[KFPartEfficiencies::nParticles][3][nPartEfficiency]; ///< Efficiency plots.
  static const int nPartEfficiency2D = 2;  ///< Number of 2D efficiency plots for each decay: y-pt, y-mt.
  TProfile2D* hPartEfficiency2D[KFPartEfficiencies::nParticles][3][nPartEfficiency2D]; ///< 2D efficiency plots.
  
  static const int nHistosPV = 7; ///< Number of QA histograms for primary vertices: residuals, pulls, number of lost tracks.
  TH1F *hPVFitQa[2][nHistosPV]; ///< Fit QA of primary vertices, 1D histograms.
  TH2F *hPVFitQa2D[2][2][nHistosPV-1]; ///< Fit QA of primary vertices, 2D histograms.

  /** Number of histograms with parameter distributions: x, y, z, r, Ntracks, Chi2, NDF, Chi2/NDF, prob, purity, part of ghost tracks, 
   ** part of tracks from the current vertex, number of tracks from merged vertices, number of background tracks from decays, distance in Z between clones. **/
  static const int nHistosPVParam = 15; ///< 
  TH1F *hPVParam[nHistosPVParam]; ///< Histograms for all vertex candidates.
  TH1F *hPVParamGhost[nHistosPVParam]; ///< Histograms for ghost (combinatorial background) vertex candidates.
  TH1F *hPVParamSignal[nHistosPVParam]; ///< Histograms for signal vertex candidates.
  TH1F *hPVParamPileup[nHistosPVParam]; ///< Histograms for pileup vertex candidates.
  TH1F *hPVParamBG[nHistosPVParam]; ///< Histograms for physics background (decays, secondary vertices) vertex candidates.
  static const int nHistosPVParam2D = 1; ///< Number of 2D histograms for primary vertex.
  TH2F *hPVParam2D[nHistosPVParam2D]; ///< x-y histogram. 
  
  static const int nFitPVTracksQA = 12; ///< Number of fit QA histograms for primary tracks: residuals and pulls in X, Y, Z, Px, Py, Pz.
  TH1F *hFitPVTracksQA[nFitPVTracksQA]; ///< Residuals and pulls of primary tracks at the primary vertex position.
  
  static const int nHistosTP = KFPartEfficiencies::nParticles + 8; ///< Number of histograms with chi2 primary distributions for daughter tracks.
  /** Histograms with chi2 primary distributions for daughter tracks of each decays plus 4 distributions of chi2 and 4 prob for primary, secondary,
   ** ghost and all particles. **/
  TH1F *hTrackParameters[nHistosTP]; 
  
  static const int nPVefficiency = 6; ///< Number of Efficiency plots for primary vertices for each category.
  TProfile* hPVefficiency[4][nPVefficiency]; ///< Efficiency plots for primary vertices.
  
  TDirectory *fHistoDir; ///< ROOT directory with histograms.

  bool IsCollectZRHistogram(int iParticle) const;
  bool IsCollect3DHistogram(int iParticle) const;
  bool IsCollectArmenteros(int iParticle) const;
  
 private:
  const KFParticlePerformanceBase& operator = (const KFParticlePerformanceBase&); ///< Copying of objects of this class is forbidden.
  KFParticlePerformanceBase(const KFParticlePerformanceBase&); ///< Copying of objects of this class is forbidden.
  
  void CreateFitHistograms(TH1F* histo[nFitQA], int iPart);
  void CreateEfficiencyHistograms(TProfile* histo[3][nPartEfficiency], TProfile2D* histo2[3][nPartEfficiency2D]);
  void CreateParameterHistograms(TH1F* histoParameters[KFPartEfficiencies::nParticles][nHistoPartParam],
                                 TH2F *histoParameters2D[KFPartEfficiencies::nParticles][nHistoPartParam2D],
                                 TH3F *histoParameters3D[KFPartEfficiencies::nParticles][nHistoPartParam3D],
                                 int iPart, bool drawZR = 0);
  void CreateParameterSubfolder(TString folderName, 
                                TH1F* histoParameters[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam],
                                TH2F* histoParameters2D[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam2D],
                                TH1F* histoFit[KFPartEfficiencies::nParticles][nFitQA], int iPart, bool withWrongPVHypothesis = 0);
  
  TString GetDirectoryPath();
};

#endif
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
