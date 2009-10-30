// $Id: PAI.h,v 1.2 2009/10/30 21:12:00 fisyak Exp $
//////////////////////////////////////////////////////////
//   This class has been automatically generated 
//     (Thu Nov 29 16:25:03 2001 by ROOT version3.02/01)
//   from TTree PAI/H.Bichel Calculation Summary
//   found on file: dEdX2T.root
//////////////////////////////////////////////////////////
#ifndef PAI_h
#define PAI_h
#include "TObject.h"
class TGraph;
class PAI : public TObject {
 public:
  virtual ~PAI();
  static void    xGasIni(Int_t &lun);
  static void    xGasTab(Float_t &g, Float_t &mdNdX, Int_t &mNoInTable, Float_t *mEnergy, Float_t *mdNdE);
  static Float_t xFintera(Float_t &x, Float_t *A, Float_t *F, Int_t &N); 
  void           xNext(Float_t BetaGamma, Float_t &dX, Float_t &dE);
  void           xGenerate(Int_t NsSteps=11, Int_t Nevents = 200000);
  static PAI*    Instance(Int_t NoBetaGammas=101, Int_t NoEntries=500, Double_t BetaGammaLog10Min = -1, Double_t BetaGammaLog10Max = 4);
  TGraph**      GetGraphs() {return mGraphs;}
private:
  PAI(Int_t NoBetaGammas=101, Int_t NoEntries=500, Double_t BetaGammaLog10Min = -1, Double_t BetaGammaLog10Max = 4);
  Int_t    mNoBetaGammas;
  Int_t    mNoEntries;
  Int_t    mNoInTable;
  Double_t mBetaGammaLog10Min;
  Double_t mBetaGammaLog10Max;
  Double_t mDeltaBetaGammaLog10;
  Float_t *mEnergy;
  Float_t *mdNdE;
  Float_t *mdNdX;
  //  TGraph2D  *mGraph;
  TGraph  **mGraphs;
  static  PAI *fgInstance;
  ClassDef(PAI,0)
};
#endif
//$Log: PAI.h,v $
//Revision 1.2  2009/10/30 21:12:00  fisyak
//Freeze version rcf9108.F, Clean up
//
