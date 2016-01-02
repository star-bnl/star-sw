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


namespace cuts
{
   // path to lists of triggers prescales
   // lists are obtained from http://www.star.bnl.gov/protected/common/common2014/trigger2014/plots_au200gev/
   std::string const prescalesFilesDirectoryName = "./run14AuAu200GeVPrescales";

   //event
   int const centrality = 3; // StRefMultCorr::getCentralityBin9() < centrality; 0-3 bins are for 40-80%
   UShort_t const triggerWord = 0x1F; //first five bits see http://rnc.lbl.gov/~xdong/SoftHadron/picoDst.html
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
   float const qaPt = 1.2;
   float const qaNHitsFit = 20;
   float const qaNSigmaKaon = 2.0;
   float const qaCosTheta = 0.995;
   float const qaDcaDaughters = 0.0050;
   float const qaKDca = 0.008; // minimum
   float const qaPDca = 0.008;
}
#endif
