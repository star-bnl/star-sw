// $Id: StSsdDynamicControl.h,v 1.1 2006/10/16 16:43:29 bouchet Exp $
//
// $Log: StSsdDynamicControl.h,v $
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.5  2006/09/15 21:03:14  bouchet
// id_mctrack is using for setIdTruth and propagated to the hit
//
// Revision 1.4  2005/03/18 14:22:40  lmartin
// missing CVS header added
//

/*!
 * \class StSsdDynamicControl
 * \author B.Hippolyte, C.Suire
 * \date 2004
 *
 *  Storage class for the dynamic parameters
 *  depending on the readout electronics
 *  specifications 
 * 
 */

#ifndef STAR_StSsdDynamicControl
#define STAR_StSsdDynamicControl
#include "Rtypes.h"
/* #include "StDbLib/StDbDefs.hh" */

/* class StDbManager; */
/* class StDbConfigNode; */
class St_slsCtrl;

class StSsdDynamicControl {
 public:
  StSsdDynamicControl();
  StSsdDynamicControl(St_slsCtrl *slsCtrl);
  ~StSsdDynamicControl();
  
/*   StDbManager* mDbMgr;           //! */
/*   StDbConfigNode* maccess;      //! */

  Int_t       getnElectronInAMip();
  Int_t       getadcDynamic();
  Int_t       geta128Dynamic();
  Int_t       getnbitEncoding();
  Double_t    getpairCreationEnergy();
  Float_t     getdaqCutValue();
  
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

inline Int_t      StSsdDynamicControl::getnElectronInAMip()    { return  mnElectronInAMip; }
inline Int_t      StSsdDynamicControl::getadcDynamic()         { return  madcDynamic;}
inline Int_t      StSsdDynamicControl::geta128Dynamic()        { return  ma128Dynamic;}
inline Int_t      StSsdDynamicControl::getnbitEncoding()       { return  mnbitEncoding;}
inline Double_t   StSsdDynamicControl::getpairCreationEnergy() { return  mpairCreationEnergy;}
inline Float_t    StSsdDynamicControl::getdaqCutValue()        { return  mdaqCutValue;}

inline void     StSsdDynamicControl::setnElectronInAMip(Int_t val)       { mnElectronInAMip = val ;}
inline void     StSsdDynamicControl::setadcDynamic(Int_t val)            { madcDynamic = val ;}
inline void     StSsdDynamicControl::seta128Dynamic(Int_t val)           { ma128Dynamic = val ;}
inline void     StSsdDynamicControl::setnbitEncoding(Int_t val)          { mnbitEncoding = val ;}
inline void     StSsdDynamicControl::setnstripInACluster(Int_t val)      { mnstripInACluster = val ;}
inline void     StSsdDynamicControl::setpairCreationEnergy(Double_t val) { mpairCreationEnergy = val ;}
inline void     StSsdDynamicControl::setparDiffP(Double_t val)              { mparDiffP = val ;}
inline void     StSsdDynamicControl::setparDiffN(Double_t val)              { mparDiffN = val ;}
inline void     StSsdDynamicControl::setparIndRightP(Double_t val)          { mparIndRightP = val ;}
inline void     StSsdDynamicControl::setparIndRightN(Double_t val)          { mparIndRightN = val ;}
inline void     StSsdDynamicControl::setparIndLeftP(Double_t val)           { mparIndLeftP = val ;}
inline void     StSsdDynamicControl::setparIndLeftN(Double_t val)           { mparIndLeftN = val ;}
inline void     StSsdDynamicControl::setdaqCutValue(Float_t val)         { mdaqCutValue = val ;}

#endif

 
