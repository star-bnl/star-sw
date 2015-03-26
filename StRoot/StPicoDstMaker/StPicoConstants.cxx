#include "StPicoConstants.h"
#include "TMath.h"

ClassImp(Pico)

   Float_t Pico::mMass[nPar] = {0.13957, 0.49368, 0.93827, 0.000511};
   Float_t Pico::mMassV0[nV0] = {0.49765, 1.11568};

   UShort_t Pico::USHORTMAX = 65535;
   Short_t  Pico::SHORTMAX  = 32767;
   UChar_t  Pico::UCHARMAX  = 255;
   Char_t   Pico::CHARMAX   = 127;

   Int_t Pico::mCent_Year10_200GeV[nCen] ={10,21,41,72,118,182,266,375,441}; // Run10 very preliminary
//   Int_t Pico::mCent_Year10_200GeV[nCen] ={14,30,56,94,146,217,312,431,510}; // Run4
   Int_t Pico::mCent_Year10_39GeV[nCen] ={7,15,28,50,81,125,185,265,316}; // Run10 39 GeV
   Int_t Pico::mCent_Year10_11_5GeV[nCen] ={7,14,26,44,70,105,153,184,221}; // Run10 11.5 GeV
   Int_t Pico::mCent_Year10_7_7GeV[nCen] ={6,12,22,37,58,87,128,154,185}; // Run10 7.7 GeV
   Int_t Pico::mCent_Year11_19_6GeV[nCen] = {8,16,28,47,75,111,161,227,268}; // Run11 19.6 GeV
   Int_t Pico::mCent_Year11_27GeV[nCen] = {8,16,28,47,75,111,161,227,268}; // Run11 27 GeV (Copy from 19.6)
   Int_t Pico::mCent_Year11_200GeV[nCen] = {10,21,41,72,118,182,266,375,441}; // Run11 200 GeV (Copy from Run10 200 GeV)

  // event selectioin
//   Int_t Pico::mTriggerId[nTrigger] = {
//     290001,290004,  // mb
//     290003,         // mb-slow-bbc
//     290501,         // ht-11
//     290053, 290054, // vpd-tac
//     290002,         // vpd-tac-slow
//     290070,         // tof-150-fast
//     290060          // tof-150-slow
//   }; // 7.7
//   Int_t Pico::mTriggerId[nTrigger] = {310004,310014}; // 11.5
//   Int_t Pico::mTriggerId[nTrigger] = {280001,280002,280501}; // 39
//   Int_t Pico::mTriggerId[nTrigger] = {260001,260011,260021,260031}; // 200
//   Int_t Pico::mTriggerId[nTrigger] = {
//     340001, 340011, 340021,   // mb1-fast
//     340002, 340012, 340022,   // mb2-slow
//     340500,                   // ht-11
//     340300,                   // tof-200-fast
//     340301,                   // tof-250-slow
//     340068,                   // bbc-large-tof-fast
//     340069                    // bbc-large-tof-slow
//   }; // 19.6 GeV
//   Int_t Pico::mTriggerId[nTrigger] = {
//     360001, // mb1-fast
//     360002, // mb2-slow
//     360500, // ht-11
//     360300, // tof-550-fast
//     360301  // tof-550-slow
////     360031, // zdc-mon-tac
////     360051, // vpd-mon-tac
////     360053, // bbc-small-mon-narrow
////     360054, // bbc-small-mon-wide (equivalent to bbc-small-mon-narrow with prescale ??)
////     360080, // bbc-small-tof-mon-wide
////     360081  // bbc-small-tof-mon-narrow
//   }; // 27 GeV (Run11)
//   Int_t Pico::mTriggerId[nTrigger] = {
//       350001, // vpd-zdc-mb
//       350011, // vpd-zdc-mb
//       350003, // vpd-zdc-mb-protected
//       350013, // vpd-zdc-mb-protected
//       350023, // vpd-zdc-mb-protected
//       350033, // vpd-zdc-mb-protected
//       350043  // vpd-zdc-mb-protected
//   }; // 200 GeV (Run11)
/*   Int_t Pico::mTriggerId[nTrigger] = {
       370011, // vpdmb-nobsmd
       370001, // vpdmb
       370022, // bbcmb
       370501, // bht0-vpdmb
       370511, // bht1-vpdmb
       370531, // bht2
       370542, // bht0-bbcmb-tof0
       370546, // bht1-bbcmb-tof0
       370522, // bht2-bbcmb
       370601, // jp0
       370611, // jp1
       370621, // jp2
       370641, // ajp
       370301, // bbcmb-tof0
       370361, // tofmult3-vpd
       370341  // tofmult4
   }; // pp200 GeV (Run12)
*/

/*
Int_t Pico::mTriggerId[nTrigger] = {
  430001, // VPDMB
  430011, // VPDMB
  430021, // VPDMB
  430031, // VPDMB
  430005, // BBCMB
  430015, // BBCMB
  430004, // ZDCMB
  430201, // BHT0*BBCMB*TOF0
  430211, // BHT0*BBCMB*TOF0
  430202, // BHT0*VPD
  430222, // BHT0*VPD
  430232, // BHT0*VPD
  430203, // BHT1*VPDMB
  430223, // BHT1*VPDMB
  430243, // BHT1*VPDMB
  430204, // BHT2
  430207, // BHT2*BBCMB
  430216, // BHT3
  430236  // BHT3
}; // Run13 pp510 GeV st_physics

Int_t Pico::mTriggerIdMtd[nTriggerMtd] = {
  430103,    // dimuon
  430113,    // dimuon
  430102,    // e-mu 
  430112,    // e-mu
  430122,    // e-mu
  430101,    // single-muon
  430111     // single-muon
}; // Run13 pp510 GeV st_mtd
*/


Int_t Pico::mTriggerId[nTrigger] = {
	// st_physics stream
	450050,    // vpdmb-5-p-nobsmd-hlt (production_mid_2014, production_low_2014)
	450060,    // vpdmb-5-p-nobsmd-hlt (production_mid_2014, production_low_2014)
	450005,    // vpdmb-5-p-nobsmd (production_2014)
	450015,    // vpdmb-5-p-nobsmd (production_2014, production_mid_2014, production_low_2014)
	450025,    // vpdmb-5-p-nobsmd (production_mid_2014, production_low_2014)
	450014,    // VPDMB-5-nobsmd
	450024,    // VPDMB-5-nobsmd
	450008,    // VPDMB-5 (production_2014, production_mid_2014, production_low_2014)
	450018,    // VPDMB-5 (production_2014, production_mid_2014, production_low_2014)
	450010,    // VPDMB-30 (production_2014, production_mid_2014, production_low_2014)
	450020,    // VPDMB-30 (production_2014, production_mid_2014, production_low_2014)
	450013,    // VPD-ZDC-novtx-mon (production_2014, production_mid_2014, production_low_2014)
	450023,    // VPD-ZDC-novtx-mon (production_2014, production_mid_2014, production_low_2014)
	450009,    // vpdmb-5-p-nobsmd-ssd-hlt (production_mid_2014, production_low_2014)
	450012,    // ZDC-mon (production_2014, production_mid_2014, production_low_2014)
	450022,    // ZDC-mon (production_2014, production_mid_2014, production_low_2014)
	450011,    // MB-mon (production_2014, production_mid_2014, production_low_2014)
	450021,    // MB-mon (production_2014, production_mid_2014, production_low_2014)
	450103,    // Central-5 (production_2014, production_mid_2014, production_low_2014)
	450201,    // BHT1*VPDMB-30 (production_2014, production_mid_2014, production_low_2014)
	450211,    // BHT1*VPDMB-30 (production_2014, production_mid_2014, production_low_2014)
	450202,    // BHT2*VPDMB-30 (production_2014, production_mid_2014, production_low_2014)
	450212,    // BHT2*VPDMB-30 (production_2014, production_mid_2014, production_low_2014)
	450203,    // BHT3 (production_2014, production_mid_2014, production_low_2014)
	450213    // BHT3 (production_2014, production_mid_2014, production_low_2014)
};


Int_t Pico::mTriggerIdMtd[nTriggerMtd] = {
  // st_mtd stream
  450601, // dimuon
  450611, // dimuon
  450621, // dimuon
  450631, // dimuon
  450641, // dimuon
  450604,    // dimuon-30-hft (production_2014)
  450605,    // dimuon-5-hft (production_mid_2014, production_low_2014)
  450606,    // dimuon-5-hft (production_mid_2014)
  450602,    // e-mu (production_2014)
  450612,    // e-mu (production_2014, production_low_2014)
  450622,    // e-mu (production_2014, production_low_2014)
  450632,    // e-mu (production_mid_2014)
  450642,    // e-mu (production_2014, production_low_2014)
  450600,    // single-muon (production_2014)
  450610,    // single-muon (production_2014, production_low_2014)
  450620,    // single-muon (production_2014, production_low_2014)
  450630,    // single-muon (production_mid_2014)
  450640     // single-muon (production_2014, production_low_2014)
}; // Run14 AuAu200 GeV StMtd Stream


   Float_t Pico::mVzMax = 100;
   Float_t Pico::mVrMax =   2;
   Int_t Pico::mRefMultMin = 0;                    // >=

  // track selection
   Float_t Pico::mPtMin = 0.1;
   Int_t   Pico::mNHitsFitMin = 15;               // >=
   Float_t Pico::mRatioMin = 0.52;
   Float_t Pico::mGDcaMax = 10.;

   Float_t Pico::mPtTpcFlowMax = 2.0;
   Float_t Pico::mPtTpcFlowMin = 0.15;
   Float_t Pico::mDcaTpcFlowMax = 2.0;
   Float_t Pico::mEtaTpcFlowMax = 1.0;
   Int_t Pico::mNHitsTpcFlowMin = 15;              // >=
   Float_t Pico::mPtFtpcFlowMax = 2.0;
   Float_t Pico::mPtFtpcFlowMin = 0.1;
   Float_t Pico::mDcaFtpcFlowMax = 2.0;
   Float_t Pico::mEtaFtpcFlowMin = 2.5;
   Float_t Pico::mEtaFtpcFlowMax = 4.0;
   Int_t Pico::mNHitsFtpcFlowMin = 5;              // >=

  // V0 cuts
   Float_t Pico::mV0DaughterNHitsFitMin     = 15;         // >= : TPC tracks
   Float_t Pico::mV0DaughterNSigmaPionMax   = 3.0;
   Float_t Pico::mV0DaughterNSigmaKaonMax   = 0.0;
   Float_t Pico::mV0DaughterNSigmaProtonMax = 3.0;
   Float_t Pico::mV0DcaDaughtersMax         = 0.85;

   Float_t Pico::mV0DaughterDca2VertexPtMax = 1.0;
  // Kshort
   Float_t Pico::mV0KsNSigmaPionMax     = 3.0;
   Float_t Pico::mV0KsPionDca2VertexMin = 0.75;
   Float_t Pico::mV0KsDca2VertexMax     = 2.0;
   Float_t Pico::mV0KsDecayLengthMin    = 2.0;
   Float_t Pico::mV0KsDecayLengthMax    = 300.;
   Float_t Pico::mV0KsMassWindowMax     = 0.1;
  // Lambda
   Float_t Pico::mV0LambdaNSigmaPionMax       = 3.0;
   Float_t Pico::mV0LambdaNSigmaProtonMax     = 3.0;
   Float_t Pico::mV0LambdaPionDca2VertexMin   = 1.0;
   Float_t Pico::mV0LambdaProtonDca2VertexMin = 0.4;
   Float_t Pico::mV0LambdaDca2VertexMax       = 5.0;
   Float_t Pico::mV0LambdaDecayLengthMin      = 2.0;
   Float_t Pico::mV0LambdaDecayLengthMax      = 300.;
   Float_t Pico::mV0LambdaMassWindowMax       = 0.1;
