//$Id: StSstDynamicControl.h,v 1.1 2015/06/23 16:26:19 jeromel Exp $
//
//$Log: StSstDynamicControl.h,v $
//Revision 1.1  2015/06/23 16:26:19  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:32  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#ifndef STAR_StSstDynamicControl
#define STAR_StSstDynamicControl
#include "Rtypes.h"
class St_sstSlsCtrl;

class StSstDynamicControl {
 public:
  StSstDynamicControl();
  StSstDynamicControl(St_sstSlsCtrl *sstCtrl);
  ~StSstDynamicControl();
  
  Int_t     getnElectronInAMip();
  Int_t     getadcDynamic();
  Int_t     geta128Dynamic();
  Int_t     getnbitEncoding();
  Double_t  getpairCreationEnergy();
  Float_t   getdaqCutValue();
  
  void      setnElectronInAMip(Int_t val);
  void      setadcDynamic(Int_t val);          
  void      seta128Dynamic(Int_t val);          
  void      setnbitEncoding(Int_t val);
  void      setnstripInACluster(Int_t val);
  void      setpairCreationEnergy(Double_t val);         
  void      setparDiffP(Double_t val);
  void      setparDiffN(Double_t val);
  void      setparIndRightP(Double_t val);
  void      setparIndRightN(Double_t val);
  void      setparIndLeftP(Double_t val);
  void      setparIndLeftN(Double_t val);
  void      setdaqCutValue(Float_t val);
  void      printParameters();

 private:
  Int_t       mnElectronInAMip;    //!   Number of electrons created in 300 um of Si 22500e-
  Int_t       madcDynamic;         //!   Dynamic range of the ADC  in mip 
  Int_t       ma128Dynamic;        //!   Dynamic range of the readout chip in mip
  Int_t       mnbitEncoding;       //!   Number of adc bit for encoding
  Int_t       mnstripInACluster;   //!   Number of strips in a cluster
  Double_t    mpairCreationEnergy; //!   Energy required to create a e+e-pair(GeV)     
  Double_t    mparDiffP;           //!              
  Double_t    mparDiffN;           //!
  Double_t    mparIndRightP;       //!      
  Double_t    mparIndRightN;       //!
  Double_t    mparIndLeftP;        //!
  Double_t    mparIndLeftN;        //!
  Float_t     mdaqCutValue;        //!   DAQ cut in sigma unit equi. S/N
};

inline Int_t    StSstDynamicControl::getnElectronInAMip()    { return  mnElectronInAMip; }
inline Int_t    StSstDynamicControl::getadcDynamic()         { return  madcDynamic;}
inline Int_t    StSstDynamicControl::geta128Dynamic()        { return  ma128Dynamic;}
inline Int_t    StSstDynamicControl::getnbitEncoding()       { return  mnbitEncoding;}
inline Double_t StSstDynamicControl::getpairCreationEnergy() { return  mpairCreationEnergy;}
inline Float_t  StSstDynamicControl::getdaqCutValue()        { return  mdaqCutValue;}

inline void     StSstDynamicControl::setnElectronInAMip(Int_t val)       { mnElectronInAMip = val ;}
inline void     StSstDynamicControl::setadcDynamic(Int_t val)            { madcDynamic = val ;}
inline void     StSstDynamicControl::seta128Dynamic(Int_t val)           { ma128Dynamic = val ;}
inline void     StSstDynamicControl::setnbitEncoding(Int_t val)          { mnbitEncoding = val ;}
inline void     StSstDynamicControl::setnstripInACluster(Int_t val)      { mnstripInACluster = val ;}
inline void     StSstDynamicControl::setpairCreationEnergy(Double_t val) { mpairCreationEnergy = val ;}
inline void     StSstDynamicControl::setparDiffP(Double_t val)              { mparDiffP = val ;}
inline void     StSstDynamicControl::setparDiffN(Double_t val)              { mparDiffN = val ;}
inline void     StSstDynamicControl::setparIndRightP(Double_t val)          { mparIndRightP = val ;}
inline void     StSstDynamicControl::setparIndRightN(Double_t val)          { mparIndRightN = val ;}
inline void     StSstDynamicControl::setparIndLeftP(Double_t val)           { mparIndLeftP = val ;}
inline void     StSstDynamicControl::setparIndLeftN(Double_t val)           { mparIndLeftN = val ;}
inline void     StSstDynamicControl::setdaqCutValue(Float_t val)         { mdaqCutValue = val ;}

#endif

 
