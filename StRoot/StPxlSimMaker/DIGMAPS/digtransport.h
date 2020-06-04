#ifndef MAPS_DIGTRANSPORT_H
#define MAPS_DIGTRANSPORT_H

#include <TNamed.h>
#include <TList.h>
#include <TGraph.h>
#include "Riostream.h"
#include "vector"

// ROOT classes
#include "TString.h"
#include "TObject.h"
#include "TVector.h"
#include "TFile.h"
#include "TSystem.h"
#include "TRandom.h"
#include "TH1.h"
#include "TH2.h"
#include "TObjArray.h"
#include "TTree.h"
#include "TBranch.h"
#include "TClonesArray.h"
using namespace std;

class DIGInitialize;
class DIGPlane;
class DIGADC;
class DIGBeam;
class DIGTransport;
class DIGParticle;
class DIGAction;
class DIGEvent;
class DIGMAPS;
//==============================================================================
class DIGTransport : public TObject {
 public:
  DIGTransport();
  DIGTransport(Float_t myvar);
  DIGTransport(DIGTransport& adigtransport);
  virtual ~DIGTransport();
  void    Clear(const Option_t * /*option*/ = "");
  void PrintInfo();

  Float_t GetMyvar(){return fMyvar;}
  
  void SetMyvar(Float_t Myvar);

  void SetChargeModel(Int_t ChargeModel);
  void SetLorentz2DModel_Cp0(Float_t Lorentz2DModel_Cp0);
  void SetLorentz2DModel_Cp1(Float_t Lorentz2DModel_Cp1);
  void SetRangeLimit_InPitchUnit(Float_t RangeLimit_InPitchUnit);

  void SetGauss2DModel_sigma1_Cp0(Float_t Gauss2DModel_sigma1_Cp0);
  void SetGauss2DModel_sigma1_Cp1(Float_t Gauss2DModel_sigma1_Cp1);
  void SetGauss2DModel_sigma2_Cp0(Float_t Gauss2DModel_sigma2_Cp0);
  void SetGauss2DModel_sigma2_Cp1(Float_t Gauss2DModel_sigma2_Cp1);
  void SetGauss2DModel_weight(Float_t Gauss2DModel_weight);

  Int_t GetChargeModel(){return fChargeModel;}

  Float_t GetLorentz2DModel_Cp0(){return fLorentz2DModel_Cp0;}
  Float_t GetLorentz2DModel_Cp1(){return fLorentz2DModel_Cp1;}
  Float_t GetRangeLimit_InPitchUnit(){return fRangeLimit_InPitchUnit;}

  Float_t GetGauss2DModel_sigma1_Cp0(){return fGauss2DModel_sigma1_Cp0;}
  Float_t GetGauss2DModel_sigma1_Cp1(){return fGauss2DModel_sigma1_Cp1;}
  Float_t GetGauss2DModel_sigma2_Cp0(){return fGauss2DModel_sigma2_Cp0;}
  Float_t GetGauss2DModel_sigma2_Cp1(){return fGauss2DModel_sigma2_Cp1;}
  Float_t GetGauss2DModel_weight(){return fGauss2DModel_weight;}

  Float_t GetLorGaussModel_Norm1_Cp0(){return fLorGaussModel_Norm1_Cp0;}
  Float_t GetLorGaussModel_Norm1_Cp1(){return fLorGaussModel_Norm1_Cp1;}
  Float_t GetLorGaussModel_Norm1_Cp2(){return fLorGaussModel_Norm1_Cp2;}
  Float_t GetLorGaussModel_sigma_Cp0(){return fLorGaussModel_sigma_Cp0;}
  Float_t GetLorGaussModel_sigma_Cp1(){return fLorGaussModel_sigma_Cp1;}
  Float_t GetLorGaussModel_C_Cp0(){return fLorGaussModel_C_Cp0;}
  Float_t GetLorGaussModel_C_Cp1(){return fLorGaussModel_C_Cp1;}
  Float_t GetLorGaussModel_Norm_Cp0(){return fLorGaussModel_Norm_Cp0;}
  Float_t GetLorGaussModel_Norm_Cp1(){return fLorGaussModel_Norm_Cp1;}

  void SetLorGaussModel_Norm1_Cp0(Float_t LorGaussModel_Norm1_Cp0);
  void SetLorGaussModel_Norm1_Cp1(Float_t LorGaussModel_Norm1_Cp1);
  void SetLorGaussModel_Norm1_Cp2(Float_t LorGaussModel_Norm1_Cp2);
  void SetLorGaussModel_sigma_Cp0(Float_t LorGaussModel_sigma_Cp0);
  void SetLorGaussModel_sigma_Cp1(Float_t LorGaussModel_sigma_Cp1);
  void SetLorGaussModel_C_Cp0(Float_t LorGaussModel_C_Cp0);
  void SetLorGaussModel_C_Cp1(Float_t LorGaussModel_C_Cp1);
  void SetLorGaussModel_Norm_Cp0(Float_t LorGaussModel_Norm_Cp0);
  void SetLorGaussModel_Norm_Cp1(Float_t LorGaussModel_Norm_Cp1);


  void SetlorlorgausModel_Norm1_Cp0(Float_t   lorlorgausModel_Norm1_Cp0){florlorgausModel_Norm1_Cp0=lorlorgausModel_Norm1_Cp0;}	    
  void SetlorlorgausModel_Norm1_Cp1(Float_t   lorlorgausModel_Norm1_Cp1){florlorgausModel_Norm1_Cp1=lorlorgausModel_Norm1_Cp1;}	    
  void SetlorlorgausModel_x01_Cp0(Float_t   lorlorgausModel_x01_Cp0){florlorgausModel_x01_Cp0=lorlorgausModel_x01_Cp0;}		    
  void SetlorlorgausModel_x01_Cp1(Float_t   lorlorgausModel_x01_Cp1){florlorgausModel_x01_Cp1=lorlorgausModel_x01_Cp1;}	    
  void SetlorlorgausModel_sigmax1_Cp0(Float_t   lorlorgausModel_sigmax1_Cp0){florlorgausModel_sigmax1_Cp0=lorlorgausModel_sigmax1_Cp0;}	    
  void SetlorlorgausModel_sigmax1_Cp1(Float_t   lorlorgausModel_sigmax1_Cp1){florlorgausModel_sigmax1_Cp1=lorlorgausModel_sigmax1_Cp1;}	    
  void SetlorlorgausModel_y01_Cp0(Float_t   lorlorgausModel_y01_Cp0){florlorgausModel_y01_Cp0=lorlorgausModel_y01_Cp0;}		    
  void SetlorlorgausModel_y01_Cp1(Float_t   lorlorgausModel_y01_Cp1){florlorgausModel_y01_Cp1=lorlorgausModel_y01_Cp1;}		    
  void SetlorlorgausModel_sigmay1_Cp0(Float_t   lorlorgausModel_sigmay1_Cp0){florlorgausModel_sigmay1_Cp0=lorlorgausModel_sigmay1_Cp0;}	    
  void SetlorlorgausModel_sigmay1_Cp1(Float_t   lorlorgausModel_sigmay1_Cp1){florlorgausModel_sigmay1_Cp1=lorlorgausModel_sigmay1_Cp1;}	    
  void SetlorlorgausModel_Gamma_Cp0(Float_t   lorlorgausModel_Gamma_Cp0){florlorgausModel_Gamma_Cp0=lorlorgausModel_Gamma_Cp0;}	    
  void SetlorlorgausModel_Gamma_Cp1(Float_t   lorlorgausModel_Gamma_Cp1){florlorgausModel_Gamma_Cp1=lorlorgausModel_Gamma_Cp1;}	    
  void SetlorlorgausModel_x0_Cp0(Float_t   lorlorgausModel_x0_Cp0){florlorgausModel_x0_Cp0=lorlorgausModel_x0_Cp0;}		    
  void SetlorlorgausModel_x0_Cp1(Float_t   lorlorgausModel_x0_Cp1){florlorgausModel_x0_Cp1=lorlorgausModel_x0_Cp1;}		    
  void SetlorlorgausModel_y0_Cp0(Float_t   lorlorgausModel_y0_Cp0){florlorgausModel_y0_Cp0=lorlorgausModel_y0_Cp0;}		    
  void SetlorlorgausModel_y0_Cp1(Float_t   lorlorgausModel_y0_Cp1){florlorgausModel_y0_Cp1=lorlorgausModel_y0_Cp1;}		    
  void SetlorlorgausModel_norm_Cp0(Float_t   lorlorgausModel_norm_Cp0){florlorgausModel_norm_Cp0=lorlorgausModel_norm_Cp0;}	    
  void SetlorlorgausModel_norm_Cp1(Float_t   lorlorgausModel_norm_Cp1){florlorgausModel_norm_Cp1=lorlorgausModel_norm_Cp1;}	    
  void SetlorlorgausModel_normgaus2_Cp0(Float_t   lorlorgausModel_normgaus2_Cp0){florlorgausModel_normgaus2_Cp0=lorlorgausModel_normgaus2_Cp0;}	    
  void SetlorlorgausModel_normgaus2_Cp1(Float_t   lorlorgausModel_normgaus2_Cp1){florlorgausModel_normgaus2_Cp1=lorlorgausModel_normgaus2_Cp1;}	    
  void SetlorlorgausModel_sigma2_Cp0(Float_t   lorlorgausModel_sigma2_Cp0){florlorgausModel_sigma2_Cp0=lorlorgausModel_sigma2_Cp0;}	    
  void SetlorlorgausModel_sigma2_Cp1(Float_t   lorlorgausModel_sigma2_Cp1){florlorgausModel_sigma2_Cp1=lorlorgausModel_sigma2_Cp1;}            

  Float_t GetlorlorgausModel_Norm1_Cp0(){return   florlorgausModel_Norm1_Cp0;}	    
  Float_t GetlorlorgausModel_Norm1_Cp1(){return   florlorgausModel_Norm1_Cp1;}	    
  Float_t GetlorlorgausModel_x01_Cp0(){return   florlorgausModel_x01_Cp0;}		    
  Float_t GetlorlorgausModel_x01_Cp1(){return   florlorgausModel_x01_Cp1;}	    
  Float_t GetlorlorgausModel_sigmax1_Cp0(){return   florlorgausModel_sigmax1_Cp0;}	    
  Float_t GetlorlorgausModel_sigmax1_Cp1(){return   florlorgausModel_sigmax1_Cp1;}	    
  Float_t GetlorlorgausModel_y01_Cp0(){return   florlorgausModel_y01_Cp0;}		    
  Float_t GetlorlorgausModel_y01_Cp1(){return   florlorgausModel_y01_Cp1;}	    
  Float_t GetlorlorgausModel_sigmay1_Cp0(){return   florlorgausModel_sigmay1_Cp0;}	    
  Float_t GetlorlorgausModel_sigmay1_Cp1(){return   florlorgausModel_sigmay1_Cp1;}	    
  Float_t GetlorlorgausModel_Gamma_Cp0(){return   florlorgausModel_Gamma_Cp0;}	    
  Float_t GetlorlorgausModel_Gamma_Cp1(){return   florlorgausModel_Gamma_Cp1;}	    
  Float_t GetlorlorgausModel_x0_Cp0(){return   florlorgausModel_x0_Cp0;}		    
  Float_t GetlorlorgausModel_x0_Cp1(){return   florlorgausModel_x0_Cp1;}		    
  Float_t GetlorlorgausModel_y0_Cp0(){return   florlorgausModel_y0_Cp0;}		    
  Float_t GetlorlorgausModel_y0_Cp1(){return   florlorgausModel_y0_Cp1;}    
  Float_t GetlorlorgausModel_norm_Cp0(){return   florlorgausModel_norm_Cp0;}	    
  Float_t GetlorlorgausModel_norm_Cp1(){return   florlorgausModel_norm_Cp1;} 
  Float_t GetlorlorgausModel_normgaus2_Cp0(){return   florlorgausModel_normgaus2_Cp0;}	    
  Float_t GetlorlorgausModel_normgaus2_Cp1(){return   florlorgausModel_normgaus2_Cp1;}	    
  Float_t GetlorlorgausModel_sigma2_Cp0(){return   florlorgausModel_sigma2_Cp0;}	    
  Float_t GetlorlorgausModel_sigma2_Cp1(){return   florlorgausModel_sigma2_Cp1;}            


  Float_t   Getf1dimgauslor_Norm_g_1st(){return     f1dimgauslor_Norm_g_1st	 ;}
  Float_t   Getf1dimgauslor_x0_g_1st(){return       f1dimgauslor_x0_g_1st	 ;}
  Float_t   Getf1dimgauslor_sigma_g_1st(){return    f1dimgauslor_sigma_g_1st ;}
  Float_t   Getf1dimgauslor_Gamma_lor_1st(){return  f1dimgauslor_Gamma_lor_1st ;}
  Float_t   Getf1dimgauslor_x0_lor_1st(){return     f1dimgauslor_x0_lor_1st	; }
  Float_t   Getf1dimgauslor_norm_lor_1st(){return   f1dimgauslor_norm_lor_1st;}
  Float_t   Getf1dimgauslor_Norm_g_2nd(){return     f1dimgauslor_Norm_g_2nd	; }
  Float_t   Getf1dimgauslor_x0_g_2nd(){return       f1dimgauslor_x0_g_2nd	; } 
  Float_t   Getf1dimgauslor_sigma_g_2nd(){return    f1dimgauslor_sigma_g_2nd ;}
  Float_t   Getf1dimgauslor_Gamma_lor_2nd(){return  f1dimgauslor_Gamma_lor_2nd;} 
  Float_t   Getf1dimgauslor_x0_lor_2nd(){return     f1dimgauslor_x0_lor_2nd	 ; } 
  Float_t   Getf1dimgauslor_norm_lor_2nd(){return f1dimgauslor_norm_lor_2nd  ;}

  void   Setf1dimgauslor_Norm_g_1st(Float_t l1dimgauslor_Norm_g_1st){     f1dimgauslor_Norm_g_1st =l1dimgauslor_Norm_g_1st;	 }
  void   Setf1dimgauslor_x0_g_1st(Float_t l1dimgauslor_x0_g_1st ){       f1dimgauslor_x0_g_1st	=l1dimgauslor_x0_g_1st; }
  void   Setf1dimgauslor_sigma_g_1st(Float_t l1dimgauslor_sigma_g_1st){    f1dimgauslor_sigma_g_1st=l1dimgauslor_sigma_g_1st; }
  void   Setf1dimgauslor_Gamma_lor_1st(Float_t l1dimgauslor_Gamma_lor_1st){  f1dimgauslor_Gamma_lor_1st=l1dimgauslor_Gamma_lor_1st; }
  void   Setf1dimgauslor_x0_lor_1st(Float_t l1dimgauslor_x0_lor_1st){     f1dimgauslor_x0_lor_1st=l1dimgauslor_x0_lor_1st	; }
  void   Setf1dimgauslor_norm_lor_1st(Float_t l1dimgauslor_norm_lor_1st){   f1dimgauslor_norm_lor_1st=l1dimgauslor_norm_lor_1st;}
  void   Setf1dimgauslor_Norm_g_2nd(Float_t l1dimgauslor_Norm_g_2nd ){     f1dimgauslor_Norm_g_2nd=l1dimgauslor_Norm_g_2nd;	 }
  void   Setf1dimgauslor_x0_g_2nd(Float_t l1dimgauslor_x0_g_2nd){       f1dimgauslor_x0_g_2nd	=l1dimgauslor_x0_g_2nd ;} 
  void   Setf1dimgauslor_sigma_g_2nd(Float_t l1dimgauslor_sigma_g_2nd){    f1dimgauslor_sigma_g_2nd =l1dimgauslor_sigma_g_2nd;}
  void   Setf1dimgauslor_Gamma_lor_2nd(Float_t l1dimgauslor_Gamma_lor_2nd){  f1dimgauslor_Gamma_lor_2nd=l1dimgauslor_Gamma_lor_2nd;} 
  void   Setf1dimgauslor_x0_lor_2nd(Float_t l1dimgauslor_x0_lor_2nd){     f1dimgauslor_x0_lor_2nd=l1dimgauslor_x0_lor_2nd;	  } 
  void   Setf1dimgauslor_norm_lor_2nd(Float_t l1dimgauslor_norm_lor_2nd){ f1dimgauslor_norm_lor_2nd =l1dimgauslor_norm_lor_2nd; }





 protected:
  Int_t   fChargeModel;

  Float_t   fLorentz2DModel_Cp0;
  Float_t   fLorentz2DModel_Cp1;
  Float_t   fRangeLimit_InPitchUnit;

  Float_t   fGauss2DModel_sigma1_Cp0;
  Float_t   fGauss2DModel_sigma1_Cp1;
  Float_t   fGauss2DModel_sigma2_Cp0;
  Float_t   fGauss2DModel_sigma2_Cp1;
  Float_t   fGauss2DModel_weight;

  Float_t   fLorGaussModel_Norm1_Cp0;
  Float_t   fLorGaussModel_Norm1_Cp1;
  Float_t   fLorGaussModel_Norm1_Cp2;
  Float_t   fLorGaussModel_sigma_Cp0;
  Float_t   fLorGaussModel_sigma_Cp1;
  Float_t   fLorGaussModel_C_Cp0;
  Float_t   fLorGaussModel_C_Cp1;
  Float_t   fLorGaussModel_Norm_Cp0;
  Float_t   fLorGaussModel_Norm_Cp1;

  Float_t   florlorgausModel_Norm1_Cp0 ;
  Float_t   florlorgausModel_Norm1_Cp1 ;
  Float_t   florlorgausModel_x01_Cp0 ;
  Float_t   florlorgausModel_x01_Cp1 ;
  Float_t   florlorgausModel_sigmax1_Cp0 ;
  Float_t   florlorgausModel_sigmax1_Cp1 ;
  Float_t   florlorgausModel_y01_Cp0 ;
  Float_t   florlorgausModel_y01_Cp1 ;
  Float_t   florlorgausModel_sigmay1_Cp0 ;
  Float_t   florlorgausModel_sigmay1_Cp1 ;
  Float_t   florlorgausModel_Gamma_Cp0 ;
  Float_t   florlorgausModel_Gamma_Cp1 ;
  Float_t   florlorgausModel_x0_Cp0 ;
  Float_t   florlorgausModel_x0_Cp1 ;
  Float_t   florlorgausModel_y0_Cp0 ;
  Float_t   florlorgausModel_y0_Cp1 ;
  Float_t   florlorgausModel_norm_Cp0 ;
  Float_t   florlorgausModel_norm_Cp1 ;
  Float_t   florlorgausModel_normgaus2_Cp0 ;
  Float_t   florlorgausModel_normgaus2_Cp1 ;
  Float_t   florlorgausModel_sigma2_Cp0 ;
  Float_t   florlorgausModel_sigma2_Cp1 ;

  //"Norm_g","x0_g","sigma_g","Gamma_lor","x0_lor","norm_lor"
  Float_t   f1dimgauslor_Norm_g_1st;
  Float_t   f1dimgauslor_x0_g_1st;
  Float_t   f1dimgauslor_sigma_g_1st;
  Float_t   f1dimgauslor_Gamma_lor_1st;
  Float_t   f1dimgauslor_x0_lor_1st;
  Float_t   f1dimgauslor_norm_lor_1st;
  Float_t   f1dimgauslor_Norm_g_2nd;
  Float_t   f1dimgauslor_x0_g_2nd;
  Float_t   f1dimgauslor_sigma_g_2nd;
  Float_t   f1dimgauslor_Gamma_lor_2nd;
  Float_t   f1dimgauslor_x0_lor_2nd;
  Float_t   f1dimgauslor_norm_lor_2nd;


  Float_t fMyvar;

  ClassDef(DIGTransport,1);
};



//==============================================================================

#endif
