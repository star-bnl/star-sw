#ifndef StMixerCuts_H
#define StMixerCuts_H
/* **************************************************
 * 
 * Cuts namespace.
 *
 * **************************************************
 *
 *  Initial Authors:  
 *         ** Michael Lomnitz (mrlomnitz@lbl.gov)
 *            Mustafa Mustafa (mmustafa@lbl.gov)
 *
 *  ** Code Maintainer
 *
 * **************************************************
 */

#include "Rtypes.h"
#include <string>

#include "phys_constants.h"
#include "SystemOfUnits.h"

#include "StMixerCuts.h"

namespace mxeCuts
{
  enum mePid {kKaon, kPion, kProton};
  float const pidMass[3] = { M_KAON_PLUS, M_PION_PLUS, M_PROTON};
  
  //Event
  UShort_t const triggerWord = 0x1F; //first five bits see http://rnc.lbl.gov/~xdong/SoftHadron/picoDst.html
  float const maxVz = 6.0;// cm.
  float const vzVpdVz = 3.0; // 3 cm.
  float const Verror = 1.0e-5; // 
  float const Vrcut = 2.0; // 
  
  //Tracking
  float const minPt = 0.6;
  float const nHitsFit = 20;
  float const nHitsFit_nHitsPos = 0.52;
  float const dca2pVtx = 0.008;
  bool const mRequireHft = true;
  //PID
  // pions
  float const nSigmaPion = 3.0;
  float const pTofBetaDiff = 0.03;
  // kaons
  float const nSigmaKaon = 2.0;
  float const kTofBetaDiff = 0.03;
  
  //Topology
  float const massMin = 0;
  float const massMax = 2.5;
  int const massBins = 250;
  int   const nPtBins = 5;
  float const PtEdge[nPtBins+1] = {0., 1., 2., 3., 5., 10.};
  //
  float const dcaV0ToPv[nPtBins] = {0.006, 0.006, 0.006, 0.006, 0.006};
  float const decayLength[nPtBins] = {0.01, 0.01, 0.01, 0.01, 0.01};
  float const cosTheta[nPtBins] = {0.995, 0.995, 0.995, 0.995, 0.995};//0.995
  float const dcaDaughters[nPtBins] = {0.008, 0.008, 0.008, 0.008,0.008};
  float const kDca[nPtBins] = {0.008, 0.008, 0.008, 0.008,0.008};//0.008, // minimum
  float const pDca[nPtBins] = {0.008, 0.008, 0.008, 0.008,0.008};//0.008, // minimum
}
#endif
