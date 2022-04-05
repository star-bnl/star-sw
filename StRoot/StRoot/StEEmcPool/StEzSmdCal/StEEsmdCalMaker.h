// $Id: StEEsmdCalMaker.h,v 1.3 2014/08/06 11:43:01 jeromel Exp $

#ifndef STAR_StEEsmdCalMaker
#define STAR_StEEsmdCalMaker

/*!
 *                                                                     
 * \class  StEEsmdCalMaker
 * \author Balewski
 * \date   
 * \brief  
 *
 Access EEMC data & DB  from muDst in StRoot-framework
 Only muDst data are decoded by this class 
 Uses EEsmdCal class to do any analysis
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif

class TObjArray  ;
class StMuDstMaker;

#include "EEsmdCal.h"

class StEEsmdCalMaker : public StMaker , public  EEsmdCal{

 private: 
  StMuDstMaker* mMuDstMaker;  
  int   unpackMuDst();
  int MCflag;

  // ideal calibration used by Fast simulator 
  float * mfixEmTgain; ///<  (adc=g*de )ideal electromagnetic gains for Towers
  float mfixPgain; ///< (adc=g*de ) fixed gain for pre/post shower
  float mfixSMDgain; ///< (adc=g*de ) fixed gain for SMD


 public: 
  StEEsmdCalMaker(const char *self="stEEsoloPi0", const char* muDstMakerName="muDstMaker");
  virtual       ~StEEsmdCalMaker();
  virtual Int_t Init();
  virtual Int_t InitRun  (int runNo );
  virtual Int_t Finish();
  virtual Int_t  Make();
  void SetHList(TObjArray * x){HList=x;} 
  void SetSector(int x);
  void SetMCflag(int x=1) {MCflag=x;}

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StEEsmdCalMaker.h,v 1.3 2014/08/06 11:43:01 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StEEsmdCalMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StEEsmdCalMaker.h,v $
// Revision 1.3  2014/08/06 11:43:01  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.2  2004/07/27 21:59:47  balewski
// now runs on muDst as well
//
// Revision 1.1  2004/06/12 04:09:25  balewski
// start
//
// Revision 1.1  2004/06/06 04:54:10  balewski
// dual analyzis
//
// Revision 1.2  2004/04/14 19:34:01  balewski
