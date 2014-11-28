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


#ifndef KFParticlePVReconstructor_H
#define KFParticlePVReconstructor_H

/*
 * Class for Event Topology Reconstructing
 */

#include "KFVertex.h"
#include "assert.h"

#include "KFPTrack.h"

#include <vector>
using std::vector;

class KFParticle;
class KFPTrackVector;

class KFParticlePVReconstructor{
 public:
  KFParticlePVReconstructor():fParticles(0), fNParticles(0), fWeight(0.f), fBeamLine(), fIsBeamLine(0), fClusters(0), fPrimVertices(0), fChi2CutPreparation(100), fChi2Cut(16) {};
  ~KFParticlePVReconstructor(){};
  
  void Init(KFPTrackVector *tracks, int nParticles); // init array of particles
  
  void ReconstructPrimVertex(); // find primary vertex

     /// Accessors
  int NPrimaryVertices() const { return fPrimVertices.size(); }
  KFParticle &GetPrimVertex(int iPV=0)   { return fPrimVertices[iPV]; };
  KFVertex   &GetPrimKFVertex(int iPV=0)   { return fPrimVertices[iPV]; };
  vector<int>& GetPVTrackIndexArray(int iPV=0) { return fClusters[iPV].fTracks; }
  KFParticle &GetParticle(int i){ assert( i < fNParticles );          return fParticles[i];    };
  
  void SetBeamLine(KFParticle& p) { fBeamLine = p; fIsBeamLine = 1; }
  bool IsBeamLine() const { return fIsBeamLine; }
  
  void AddPV(const KFVertex &pv, const vector<int> &tracks);
  void AddPV(const KFVertex &pv);
  void CleanPV() { fClusters.clear(); fPrimVertices.clear(); }

  //set functionality for cuts
  void SetChi2PrimaryCut(float chi2) { fChi2Cut = chi2; fChi2CutPreparation = chi2*10; }
  
 private:
  KFParticlePVReconstructor &operator=(KFParticlePVReconstructor &);
  KFParticlePVReconstructor(KFParticlePVReconstructor &);

  void FindPrimaryClusters( int cutNDF = 1);

  vector<KFParticle> fParticles; // input particles
  int fNParticles;           // number of input particles

  vector<float> fWeight;
  
  KFParticle fBeamLine;
  bool fIsBeamLine;
  
  struct KFParticleCluster {
    KFParticleCluster():fTracks(0) {};
    vector<int> fTracks;
    float fP[3];
    float fC[6];
  };

  vector< KFParticleCluster > fClusters;
  vector<KFVertex> fPrimVertices;  // created primary vertex(-es) (currently only one primary vertex in possible
  
  float fChi2CutPreparation;
  float fChi2Cut;
}; // class KFParticlePVReconstructor


#endif // KFParticlePVReconstructor_H
  
