// $Id: StMCInitApp.h,v 1.1.2.1 2021/05/09 02:26:14 perev Exp $
//
//
// Class StMCInitApp
// ------------------


#ifndef STMC_INITAPP_H
#define STMC_INITAPP_H
#include "TVirtualMC.h"

class StMCSimplePrimaryGenerator;

#include "GCall.h"
class StMCInitApp : public GCall
{
  public:
    StMCInitApp();
    virtual ~StMCInitApp(){}    
    // methods
   int Fun();

//		Setters
  void SetNPrim(int nTrk) 			{fNPrim =nTrk;}
  void SetGCode(int gCode) 			{fGCode =gCode; fOpt="G";}
  void SetPDG(int pCode) 			{fGCode =pCode; fOpt="" ;}
  void SetPt (double ptMin,double ptMax) 	{fPt [0]=ptMin; fPt[1] =ptMax;}
  void SetEta(double etMin,double etMax) 	{fEta[0]=etMin; fEta[1]=etMax;}
  void SetPhi(double phMin,double phMax) 	{fPhi[0]=phMin; fPhi[1]=phMax;}
  void SetZ  (double zzMin,double zzMax) 	{fZ  [0]=zzMin; fZ  [1]=zzMax;}

  StMCSimplePrimaryGenerator *generator(){ return mGenerator; }

  protected:

  StMCSimplePrimaryGenerator *mGenerator;

    // data members
   int fNPrim;
   int fGCode;
double fPt[2];
double fEta[2];
double fPhi[2];
double fZ[2];
TString fOpt;

    ClassDef(StMCInitApp,0) // Extended TParticle
};

#endif //STMC_GENERATEPRIMARIES_H   
   


