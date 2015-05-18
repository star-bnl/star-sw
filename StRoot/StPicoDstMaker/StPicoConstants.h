#ifndef StPicoConstants_h
#define StPicoConstants_h
#include "Rtypes.h"

  enum {
     nTrk     = 50000,
     nCen     = 9,
     nEW      = 4,
     nDet     = 2,
     nPhi     = 360,
     EE       = 0,
     EW       = 1,
     WE       = 2,
     WW       = 3,
     FarWest  = 0,
     West     = 1,
     East     = 2,
     FarEast  = 3,
     others   = 0,
     tpcFlow  = 1,
     ftpcFlow = 2,
     pos      = 0,    // V0 daughter numbering
     neg      = 1,
     nPar     = 4,    // 4 types of particles
     pion     = 0,    // particle type in V0 finder
     kaon     = 1,
     proton   = 2,
     electron = 3,
     nV0      = 2,
     ks       = 0,
     lambda   = 1,
//     nTrigger = 9  // 7.7, 11.5 GeV (Run10)
//     nTrigger = 3  // 39 GeV (Run10)
//     nTrigger = 4  // 200 GeV (Run10)
//     nTrigger = 11 // Run11 19.6 GeV
//     nTrigger = 5 // Run11 27 GeV
//     nTrigger = 7 // Run11 200 GeV
//     nTrigger = 16 // Run12 pp200 GeV
//     nTrigger = 19, // Run13 pp510 GeV st_physics
//     nTriggerMtd = 7 // Run13 pp510 GeV st_mtd
    nTrigger    = 25, // Run14 AuAu 200GeV st_physics
    nTriggerMtd = 18 // Run14 AuAu 200GeV st_mtd
  };


class Pico {
public:

  static Float_t mMass[nPar];// = {0.13957, 0.49368, 0.93827, 0.000511};
  static Float_t mMassV0[nV0];// = {0.49765, 1.11568};

  static UShort_t USHORTMAX;// = 65535;
  static Short_t  SHORTMAX;//  = 32767;
  static UChar_t  UCHARMAX;//  = 255;
  static Char_t   CHARMAX;//   = 127;
  
  static Int_t mCent_Year10_200GeV[nCen];// ={14,30,56,94,146,217,312,431,510};
  static Int_t mCent_Year10_39GeV[nCen];// ={8,16,29,51,82,126,186,266,317};
  static Int_t mCent_Year10_11_5GeV[nCen];
  static Int_t mCent_Year10_7_7GeV[nCen];
  static Int_t mCent_Year11_19_6GeV[nCen];
  static Int_t mCent_Year11_27GeV[nCen];
  static Int_t mCent_Year11_200GeV[nCen];
    
  // event selectioin
  static Int_t mTriggerId[nTrigger];// = {280001,280002};
  static Int_t mTriggerIdMtd[nTriggerMtd];// added for Run14+ st_mtd stream data
  static Float_t mVzMax;// = 40;
  static Float_t mVrMax;// = 2;
  static Int_t mRefMultMin;// = 8;                    // >=
  
  // track selection
  static Float_t mPtMin;// = 0.1;
  static Int_t mNHitsFitMin;// = 15;                 // >=
  static Float_t mRatioMin;// = 0; //0.52;  -> removed for Run14 AuAu200GeV
  static Float_t mGDcaMax;// = 10.;
    
  static Float_t mPtTpcFlowMax;// = 2.0;
  static Float_t mPtTpcFlowMin;// = 0.15;
  static Float_t mDcaTpcFlowMax;// = 2.0;
  static Float_t mEtaTpcFlowMax;// = 1.0;
  static Int_t mNHitsTpcFlowMin;// = 15;              // >=
  static Float_t mPtFtpcFlowMax;// = 2.0;
  static Float_t mPtFtpcFlowMin;// = 0.1;
  static Float_t mDcaFtpcFlowMax;// = 2.0;
  static Float_t mEtaFtpcFlowMin;// = 2.5;     
  static Float_t mEtaFtpcFlowMax;// = 4.0;
  static Int_t mNHitsFtpcFlowMin;// = 5;              // >=
            
  // V0 cuts
  static Float_t mV0DaughterNHitsFitMin;// = 15;         // >= : TPC tracks
  static Float_t mV0DaughterNSigmaPionMax;// = 3.0
  static Float_t mV0DaughterNSigmaKaonMax;// = 0.0
  static Float_t mV0DaughterNSigmaProtonMax;// = 3.0
  static Float_t mV0DcaDaughtersMax;//     = 0.8;

  static Float_t mV0DaughterDca2VertexPtMax;// = 2.0;
  // Kshort
  static Float_t mV0KsNSigmaPionMax;//     = 3.0;
  static Float_t mV0KsPionDca2VertexMin;// = 0.75;
  static Float_t mV0KsDca2VertexMax;//     = 1.0;
  static Float_t mV0KsDecayLengthMin;//    = 2.0;
  static Float_t mV0KsDecayLengthMax;//    = 1000.;
  static Float_t mV0KsMassWindowMax;//     = 0.1;
  // Lambda
  static Float_t mV0LambdaNSigmaPionMax;//       = 3.0;  
  static Float_t mV0LambdaNSigmaProtonMax;//     = 3.0;
  static Float_t mV0LambdaPionDca2VertexMin;//   = 1.0;
  static Float_t mV0LambdaProtonDca2VertexMin;// = 0.4;
  static Float_t mV0LambdaDca2VertexMax;//       = 3.0;
  static Float_t mV0LambdaDecayLengthMin;//      = 2.0;
  static Float_t mV0LambdaDecayLengthMax;//      = 1000.;
  static Float_t mV0LambdaMassWindowMax;//       = 0.1;

  ClassDef(Pico, 1)
};              
#endif
