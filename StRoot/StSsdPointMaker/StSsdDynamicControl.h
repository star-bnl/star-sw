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

class St_slsCtrl;
class StSsdDynamicControl{
 public:
  StSsdDynamicControl();
  StSsdDynamicControl(St_slsCtrl *slsCtrl);
  ~StSsdDynamicControl();
  
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
  void      setPairCreationEnergy(double val);         
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
inline void     StSsdDynamicControl::setPairCreationEnergy(double val) { mPairCreationEnergy = val ;}
inline void     StSsdDynamicControl::setDAQCutValue(float val)         { mDAQCutValue = val ;}

#endif

 
