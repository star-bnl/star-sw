// $Id: StSsdDynamicControl.h,v 1.5 2006/09/15 21:03:14 bouchet Exp $
//
// $Log: StSsdDynamicControl.h,v $
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
/* #include "StDbLib/StDbDefs.hh" */

/* class StDbManager; */
/* class StDbConfigNode; */
class St_slsCtrl;
class StSsdPointMaker;
class StSsdDynamicControl{
 public:
  StSsdDynamicControl();
  StSsdDynamicControl(St_slsCtrl *slsCtrl);
  ~StSsdDynamicControl();
  
/*   StDbManager* mDbMgr;           //! */
/*   StDbConfigNode* maccess;      //! */

  int       getnElectronInAMip();
  int       getadcDynamic();
  int       geta128Dynamic();
  int       getnbitEncoding();
  double    getpairCreationEnergy();
  float     getdaqCutValue();
  
  void      setnElectronInAMip(int val);
  void      setadcDynamic(int val);          
  void      seta128Dynamic(int val);          
  void      setnbitEncoding(int val);
  void      setnstripInACluster(int val);
  void      setpairCreationEnergy(double val);         
  void      setparDiffP(double val);
  void      setparDiffN(double val);
  void      setparIndRightP(double val);
  void      setparIndRightN(double val);
  void      setparIndLeftP(double val);
  void      setparIndLeftN(double val);
  void      setdaqCutValue(float val);
  void      printParameters();

 private:
  int       mnElectronInAMip;    //!   Number of electrons created in 300 um of Si 22500e-
  int       madcDynamic;         //!   Dynamic range of the ADC  in mip 
  int       ma128Dynamic;        //!   Dynamic range of the readout chip in mip
  int       mnbitEncoding;       //!   Number of adc bit for encoding
  int       mnstripInACluster;   //!   Number of strips in a cluster
  double    mpairCreationEnergy; //!   Energy required to create a e+e-pair(GeV)     
  double    mparDiffP;           //!              
  double    mparDiffN;           //!
  double    mparIndRightP;       //!      
  double    mparIndRightN;       //!
  double    mparIndLeftP;        //!
  double    mparIndLeftN;        //!
  float     mdaqCutValue;        //!   DAQ cut in sigma unit equi. S/N
};

inline int      StSsdDynamicControl::getnElectronInAMip()    { return  mnElectronInAMip; }
inline int      StSsdDynamicControl::getadcDynamic()         { return  madcDynamic;}
inline int      StSsdDynamicControl::geta128Dynamic()        { return  ma128Dynamic;}
inline int      StSsdDynamicControl::getnbitEncoding()       { return  mnbitEncoding;}
inline double   StSsdDynamicControl::getpairCreationEnergy() { return  mpairCreationEnergy;}
inline float    StSsdDynamicControl::getdaqCutValue()        { return  mdaqCutValue;}

inline void     StSsdDynamicControl::setnElectronInAMip(int val)       { mnElectronInAMip = val ;}
inline void     StSsdDynamicControl::setadcDynamic(int val)            { madcDynamic = val ;}
inline void     StSsdDynamicControl::seta128Dynamic(int val)           { ma128Dynamic = val ;}
inline void     StSsdDynamicControl::setnbitEncoding(int val)          { mnbitEncoding = val ;}
inline void     StSsdDynamicControl::setnstripInACluster(int val)      { mnstripInACluster = val ;}
inline void     StSsdDynamicControl::setpairCreationEnergy(double val) { mpairCreationEnergy = val ;}
inline void     StSsdDynamicControl::setparDiffP(double val)              { mparDiffP = val ;}
inline void     StSsdDynamicControl::setparDiffN(double val)              { mparDiffN = val ;}
inline void     StSsdDynamicControl::setparIndRightP(double val)          { mparIndRightP = val ;}
inline void     StSsdDynamicControl::setparIndRightN(double val)          { mparIndRightN = val ;}
inline void     StSsdDynamicControl::setparIndLeftP(double val)           { mparIndLeftP = val ;}
inline void     StSsdDynamicControl::setparIndLeftN(double val)           { mparIndLeftN = val ;}
inline void     StSsdDynamicControl::setdaqCutValue(float val)         { mdaqCutValue = val ;}

#endif

 
