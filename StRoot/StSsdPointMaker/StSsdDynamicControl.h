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

  int       getNElectronInAMip();
  int       getADCDynamic();
  int       getA128Dynamic();
  int       getNBitEncoding();
  double    getPairCreationEnergy();
  float     getDAQCutValue();
  
  void      setNElectronInAMip(int val);
  void      setADCDynamic(int val);          
  void      setA128Dynamic(int val);          
  void      setNBitEncoding(int val);
  void      setNStripInACluster(int val);
  void      setPairCreationEnergy(double val);         
  void      setParDiffP(double val);
  void      setParDiffN(double val);
  void      setParIndRightP(double val);
  void      setParIndRightN(double val);
  void      setParIndLeftP(double val);
  void      setParIndLeftN(double val);
  void      setDAQCutValue(float val);
  void      printParameters();

 private:
  int       mNElectronInAMip;    //!   Number of electrons created in 300 um of Si 22500e-
  int       mADCDynamic;         //!   Dynamic range of the ADC  in mip 
  int       mA128Dynamic;        //!   Dynamic range of the readout chip in mip
  int       mNBitEncoding;       //!   Number of adc bit for encoding
  int       mNStripInACluster;   //!   Number of strips in a cluster
  double    mPairCreationEnergy; //!   Energy required to create a e+e-pair(GeV)     
  double    mparDiffP;           //!              
  double    mparDiffN;           //!
  double    mparIndRightP;       //!      
  double    mparIndRightN;       //!
  double    mparIndLeftP;        //!
  double    mparIndLeftN;        //!
  float     mDAQCutValue;        //!   DAQ cut in sigma unit equi. S/N
};

inline int      StSsdDynamicControl::getNElectronInAMip()    { return  mNElectronInAMip; }
inline int      StSsdDynamicControl::getADCDynamic()         { return  mADCDynamic;}
inline int      StSsdDynamicControl::getA128Dynamic()        { return  mA128Dynamic;}
inline int      StSsdDynamicControl::getNBitEncoding()       { return  mNBitEncoding;}
inline double   StSsdDynamicControl::getPairCreationEnergy() { return  mPairCreationEnergy;}
inline float    StSsdDynamicControl::getDAQCutValue()        { return  mDAQCutValue;}

inline void     StSsdDynamicControl::setNElectronInAMip(int val)       { mNElectronInAMip = val ;}
inline void     StSsdDynamicControl::setADCDynamic(int val)            { mADCDynamic = val ;}
inline void     StSsdDynamicControl::setA128Dynamic(int val)           { mA128Dynamic = val ;}
inline void     StSsdDynamicControl::setNBitEncoding(int val)          { mNBitEncoding = val ;}
inline void     StSsdDynamicControl::setNStripInACluster(int val)      { mNStripInACluster = val ;}
inline void     StSsdDynamicControl::setPairCreationEnergy(double val) { mPairCreationEnergy = val ;}
inline void     StSsdDynamicControl::setParDiffP(double val)              { mparDiffP = val ;}
inline void     StSsdDynamicControl::setParDiffN(double val)              { mparDiffN = val ;}
inline void     StSsdDynamicControl::setParIndRightP(double val)          { mparIndRightP = val ;}
inline void     StSsdDynamicControl::setParIndRightN(double val)          { mparIndRightN = val ;}
inline void     StSsdDynamicControl::setParIndLeftP(double val)           { mparIndLeftP = val ;}
inline void     StSsdDynamicControl::setParIndLeftN(double val)           { mparIndLeftN = val ;}
inline void     StSsdDynamicControl::setDAQCutValue(float val)         { mDAQCutValue = val ;}

#endif

 
