/***************************************************************************
 *
 * $Id: StParticleTypes.hh,v 1.3 2014/06/25 14:19:24 jwebb Exp $
 *
 * Author: Thomas Ullrich, May 99 (based on Geant4 code, see below) 
 ***************************************************************************
 *
 * The design of the StParticleDefinition class and all concrete
 * classes derived from it is largely based on the design of the 
 * G4ParticleDefinition class from Geant4 (RD44).
 * Although the code is in large parts different (modified or rewritten)
 * and adapted to the STAR framework the basic idea stays the same.
 *
 ***************************************************************************
 *
 * $Log: StParticleTypes.hh,v $
 * Revision 1.3  2014/06/25 14:19:24  jwebb
 * Added psi prime --> e+e-
 *
 * Revision 1.2  2000/04/06 22:25:42  ullrich
 * Added phi and omega. More STAR specific Geant IDs.
 *
 * Revision 1.1  1999/05/14 18:50:01  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StParticleTypes_hh
#define StParticleTypes_hh

// Bosons
#include "StGamma.hh"
#include "StOpticalPhoton.hh"

// Leptons
#include "StAntiNeutrinoE.hh"
#include "StAntiNeutrinoMu.hh"
#include "StAntiNeutrinoTau.hh"
#include "StElectron.hh"
#include "StMuonMinus.hh"
#include "StMuonPlus.hh"
#include "StNeutrinoE.hh"
#include "StNeutrinoMu.hh"
#include "StNeutrinoTau.hh"
#include "StPositron.hh"
#include "StTauMinus.hh"
#include "StTauPlus.hh"

// Mesons
#include "StAntiBMesonZero.hh"
#include "StAntiBsMesonZero.hh"
#include "StAntiDMesonZero.hh"
#include "StAntiKaonZero.hh"
#include "StBMesonMinus.hh"
#include "StBMesonPlus.hh"
#include "StBMesonZero.hh"
#include "StBsMesonZero.hh"
#include "StDMesonMinus.hh"
#include "StDMesonPlus.hh"
#include "StDMesonZero.hh"
#include "StDsMesonMinus.hh"
#include "StDsMesonPlus.hh"
#include "StEta.hh"
#include "StEtaPrime.hh"
#include "StJPsi.hh"
#include "StPsi2s.hh"
#include "StKaonMinus.hh"
#include "StKaonPlus.hh"
#include "StKaonZero.hh"
#include "StKaonZeroLong.hh"
#include "StKaonZeroShort.hh"
#include "StOmegaMeson.hh"
#include "StPhi.hh"
#include "StPionMinus.hh"
#include "StPionPlus.hh"
#include "StPionZero.hh"
#include "StRhoMinus.hh"
#include "StRhoPlus.hh"
#include "StRhoZero.hh"

// Baryons
#include "StAntiLambda.hh"
#include "StAntiLambdacPlus.hh"
#include "StAntiNeutron.hh"
#include "StAntiOmegaMinus.hh"
#include "StAntiOmegacZero.hh"
#include "StAntiProton.hh"
#include "StAntiSigmaMinus.hh"
#include "StAntiSigmaPlus.hh"
#include "StAntiSigmaZero.hh"
#include "StAntiSigmacPlus.hh"
#include "StAntiSigmacPlusPlus.hh"
#include "StAntiSigmacZero.hh"
#include "StAntiXiMinus.hh"
#include "StAntiXiZero.hh"
#include "StAntiXicPlus.hh"
#include "StAntiXicZero.hh"
#include "StLambda.hh"
#include "StLambdacPlus.hh"
#include "StNeutron.hh"
#include "StOmegaMinus.hh"
#include "StOmegacZero.hh"
#include "StProton.hh"
#include "StSigmaMinus.hh"
#include "StSigmaPlus.hh"
#include "StSigmaZero.hh"
#include "StSigmacPlus.hh"
#include "StSigmacPlusPlus.hh"
#include "StSigmacZero.hh"
#include "StXiMinus.hh"
#include "StXiZero.hh"
#include "StXicPlus.hh"
#include "StXicZero.hh"

// Ions
#include "StAlpha.hh"
#include "StDeuteron.hh"
#include "StHe3.hh"
#include "StTriton.hh"

#endif
