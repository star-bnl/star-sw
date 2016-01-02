#ifndef NPE_ANA_CUTS_H
#define NPE_ANA_CUTS_H
/* **************************************************
 *  Cuts namespace.
 *
 *  Authors:  **Kunsu OH        (kunsuoh@gmail.com)
 *              Mustafa Mustafa (mmustafa@lbl.gov)
 *
 *  **Code Maintainer
 *
 * **************************************************
 */

#include "Rtypes.h"
#include <string>

namespace anaCuts
{
    // path to lists of triggers prescales
    // lists are obtained from http://www.star.bnl.gov/protected/common/common2014/trigger2014/plots_au200gev/
    // Below cuts are default value of StPicoNpeEnentMaker
    
    std::string const prescalesFilesDirectoryName = "./run14AuAu200GeVPrescales";
    
    //event
    float const vz = 6.0;// cm.
    float const vzVpdVz = 3.0; // 3 cm.
    
    //tracking
    int const nHitsFit = 15;
    float const pt = .2;
    
    // electrons
    bool const requireHFT = true;
    float const nSigmaElectron = 3.;
    
    // partner
    float const nSigmaPartnerElectron = 3.;
    
    // tree electron pair cuts
    float const pairMass = 0.2;
    float const pairMassHigh = 0.4;
    float const pairHighPt = 2.;
    float const pairDca = 1;
    float const positionX = 200.;
    float const positionY = 200.;
    float const positionZ = 200.;
    
    // histograms electron pair cuts
    float const qaNHitsFit = 20;
    float const qaNSigmaElectronMax = 3.0;
    float const qaNSigmaElectronMin = -1.0;
    float const qaPairDca = 0.1;
    float const qaPairMass = 0.1;
}
#endif
