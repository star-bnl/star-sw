#ifndef CUTS_H
#define CUTS_H

#include "StCuts.h"

/* **************************************************
 *  Cuts namespace.
 *
 *  Authors:  **Mustafa Mustafa (mmustafa@lbl.gov)
 *
 *  **Code Maintainer
 *
 * **************************************************
 */

#include <array>

namespace cuts
{
   // path to lists of triggers prescales
   // lists are obtained from http://www.star.bnl.gov/protected/common/common2014/trigger2014/plots_au200gev/
   std::string const prescalesFilesDirectoryName = "./run14AuAu200GeVPrescales";

   //event
   int const centrality = 3; // StRefMultCorr::getCentralityBin9() < centrality; 0-3 bins are for 40-80%
   std::array<unsigned int, 5> const triggers = { 450050,    // vpdmb-5-p-nobsmd-hlt 
                                                  450060,    // vpdmb-5-p-nobsmd-hlt 
                                                  450005,    // vpdmb-5-p-nobsmd 
                                                  450015,    // vpdmb-5-p-nobsmd 
                                                  450025};    // vpdmb-5-p-nobsmd 
   float const vz = 6.0;// cm.
   float const vzVpdVz = 3.0; // 3 cm.

   // vertex refit track quality
   float const vtxDca = 3.0;
   size_t const vtxNumberOfFitPoints = 20;

   //tracking
   int const nHitsFit = 15;
   bool const requireHFT = true;

   //pions
   float const nSigmaPion = 3.0;

   //kaons
   float const nSigmaKaon = 2.5;

   // tree kaonPion pair cuts
   float const cosTheta = 0; // minimum
   float const dcaDaughters = 0.0200; // maximum
   float const decayLength = 0.0030; // minimum
   float const minMass = 1.6;
   float const maxMass = 2.2;

   // histograms kaonPion pair cuts
   int   const nPtBins = 5;
   float const PtBinsEdge[nPtBins+1] = {0., 1., 2., 3., 5., 15.};//this is for optimaized cut
   float const qaPt = 1.2;
   float const qaNHitsFit = 20;
   float const qaNSigmaKaon = 2.0;
   float const qaRapidityCut = 1.0;
   float const qaDcaV0ToPv[nPtBins] = {0.0061, 0.0049, 0.0038, 0.0038, 0.0040};
   float const qaDecayLength[nPtBins] = {0.0145, 0.0181, 0.0212, 0.0247, 0.0259};
   float const qaCosTheta[nPtBins] = {0.0000, 0.0000, 0.0000, 0.0000, 0.0000};//0.995
   float const qaDcaDaughters[nPtBins] = {0.0084, 0.0066, 0.0057, 0.0050, 0.0060}; //0.0050;
   float const qaKDca[nPtBins] = {0.0103, 0.0091, 0.0095, 0.0079, 0.0058};//0.008, // minimum
   float const qaPDca[nPtBins] = {0.0110, 0.0111, 0.0086, 0.0081, 0.0062};//0.008
}
#endif
