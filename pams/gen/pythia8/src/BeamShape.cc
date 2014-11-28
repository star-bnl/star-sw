// BeamShape.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the BeamShape class. 

#include "BeamShape.h"

namespace Pythia8 {
 
//**************************************************************************

// The BeamShape class.

//*********

// Initialize beam parameters.

void BeamShape::init() { 

  // Main flags.
  allowMomentumSpread = Settings::flag("Beams:allowMomentumSpread");
  allowVertexSpread   = Settings::flag("Beams:allowVertexSpread");

  // Parameters for beam A momentum spread.
  sigmaPxA            = Settings::parm("Beams:sigmaPxA");
  sigmaPyA            = Settings::parm("Beams:sigmaPyA");
  sigmaPzA            = Settings::parm("Beams:sigmaPzA");
  maxDevA             = Settings::parm("Beams:maxDevA");

  // Parameters for beam B momentum spread.
  sigmaPxB            = Settings::parm("Beams:sigmaPxB");
  sigmaPyB            = Settings::parm("Beams:sigmaPyB");
  sigmaPzB            = Settings::parm("Beams:sigmaPzB");
  maxDevB             = Settings::parm("Beams:maxDevB");
 
  // Parameters for beam vertex spread.
  sigmaVertexX        = Settings::parm("Beams:sigmaVertexX");
  sigmaVertexY        = Settings::parm("Beams:sigmaVertexY");
  sigmaVertexZ        = Settings::parm("Beams:sigmaVertexZ");
  maxDevVertex        = Settings::parm("Beams:maxDevVertex");
  sigmaTime           = Settings::parm("Beams:sigmaTime");
  maxDevTime          = Settings::parm("Beams:maxDevTime");
 
  // Parameters for beam vertex offset.
  offsetX             = Settings::parm("Beams:offsetVertexX"); 
  offsetY             = Settings::parm("Beams:offsetVertexY");
  offsetZ             = Settings::parm("Beams:offsetVertexZ");
  offsetT             = Settings::parm("Beams:offsetTime");

}

//*********

// Set the two beam momentum deviations and the beam vertex.

void BeamShape::pick() { 

  // Reset all values.
  deltaPxA = deltaPyA = deltaPzA = deltaPxB = deltaPyB = deltaPzB
    = vertexX = vertexY = vertexZ = vertexT = 0.;

  // Set beam A momentum deviation by a three-dimensional Gaussian.
  if (allowMomentumSpread) {
    double totalDev, gauss;
    do {
      totalDev = 0.;
      if (sigmaPxA > 0.) {
        gauss     = Rndm::gauss();
        deltaPxA  = sigmaPxA * gauss;
        totalDev += gauss * gauss; 
      }
      if (sigmaPyA > 0.) {
        gauss     = Rndm::gauss();
        deltaPyA  = sigmaPyA * gauss;
        totalDev += gauss * gauss; 
      }
      if (sigmaPzA > 0.) {
        gauss     = Rndm::gauss();
        deltaPzA  = sigmaPzA * gauss;
        totalDev += gauss * gauss; 
      }
    } while (totalDev > maxDevA * maxDevA); 

    // Set beam B momentum deviation by a three-dimensional Gaussian.
    do {
      totalDev = 0.;
      if (sigmaPxB > 0.) {
        gauss     = Rndm::gauss();
        deltaPxB  = sigmaPxB * gauss;
        totalDev += gauss * gauss; 
      }
      if (sigmaPyB > 0.) {
        gauss     = Rndm::gauss();
        deltaPyB  = sigmaPyB * gauss;
        totalDev += gauss * gauss; 
      }
      if (sigmaPzB > 0.) {
        gauss     = Rndm::gauss();
        deltaPzB  = sigmaPzB * gauss;
        totalDev += gauss * gauss; 
      }
    } while (totalDev > maxDevB * maxDevB); 
  }  

  // Set beam vertex location by a three-dimensional Gaussian.
  if (allowVertexSpread) {
    double totalDev, gauss;
    do {
      totalDev = 0.;
      if (sigmaVertexX > 0.) {
        gauss     = Rndm::gauss();
        vertexX   = sigmaVertexX * gauss;
        totalDev += gauss * gauss; 
      }
      if (sigmaVertexY > 0.) {
        gauss     = Rndm::gauss();
        vertexY   = sigmaVertexY * gauss;
        totalDev += gauss * gauss; 
      }
      if (sigmaVertexZ > 0.) {
        gauss     = Rndm::gauss();
        vertexZ   = sigmaVertexZ * gauss;
        totalDev += gauss * gauss; 
      }
    } while (totalDev > maxDevVertex * maxDevVertex);

    // Set beam collision time by a Gaussian.
    if (sigmaTime > 0.) {
      do gauss    = Rndm::gauss();
      while (abs(gauss) > maxDevTime);
      vertexT     = sigmaTime * gauss;
    }

    // Add offset to beam vertex.
    vertexX      += offsetX;
    vertexY      += offsetY;
    vertexZ      += offsetZ;
    vertexT      += offsetT;
  }  

}
 
//**************************************************************************

} // end namespace Pythia8

