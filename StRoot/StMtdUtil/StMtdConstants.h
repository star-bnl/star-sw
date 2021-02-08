#ifndef STMTDCONSTANTS_HH
#define STMTDCONSTANTS_HH

//------------------------//
// Geometry constants
//------------------------//
const Int_t    gMtdNBacklegs             = 30;   // Total number of backlegs
const Int_t    gMtdNModules              = 5;    // Total number of MTD modules per backleg
const Int_t    gMtdNCells                = 12;   // Total number of MTD cells per module.
const Int_t    gMtdNChannels             = 24;   // Total number of MTD channels per module. One cell has two channels.
const Int_t    gMtdNModulesAll           = 150;  // Total number of MTD modules

const Double_t gMtdCellLength            = 87.0; // Length of a MTD cell (cm)
const Double_t gMtdCellWidth             = 3.8;  // Width of a MTD cell (cm)
const Double_t gMtdCellGap               = 0.6;  // Gap between MTD cells (cm)
const Double_t gMtdCellDriftV            = 56.;  // Drift velocity of electronic signal travelling on MTD cells (ps/cm)
const Double_t gMtdRadiusDiff            = 4.99; // Relative differece of difference modules in radius (cm)

const Double_t gMtdBacklegPhiWidth       = 8.*(TMath::Pi())/180.;   // Width of backleg in phi (rad)
const Double_t gMtdBacklegPhiGap         = 4.*(TMath::Pi())/180.;   // Gap between backleg in phi (rad)
const Double_t gMtdFirstBacklegPhiCenter = 90.*(TMath::Pi())/180.;  // Center of backleg 1 at phi = 90 degree (rad)
const Double_t gMtdMinRadius             = 392.802; // Minimum radius of MTD system extracted from geometry file (cm)
const Double_t gMtdMaxRadius             = 418.865; // Maximum radius of MTD system extracted from geometry file (cm)

const Double_t gMtdMuonELossInEmc        = 0.215;   // Energy loss of muons in EMC (GeV/c)
const Double_t gMtdMuonELossInCoil       = 0.176;   // Energy loss of muons in Coil (GeV/c)
const Double_t gMtdConvertTdcToNs        = 25./1024;  // Conversion from TDC to ns

#endif
