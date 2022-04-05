/***************************************************************************
 *
 * $Id: StTofSimMaker.h,v 1.5 2014/08/06 11:43:49 jeromel Exp $
 *
 * Author:  Frank Geurts
 ***************************************************************************
 *
 * Description: StTofSimMaker virtual base class for TOFp Simulations
 *
 ***************************************************************************
 *
 * $Log: StTofSimMaker.h,v $
 * Revision 1.5  2014/08/06 11:43:49  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.4  2003/09/10 19:47:38  perev
 * ansi corrs
 *
 * Revision 1.3  2003/07/25 04:34:45  geurts
 * - upper adc and tdc limits
 * - geometry initialization moved to InitRun()
 *
 * Revision 1.2  2002/12/12 01:43:46  geurts
 * Introduced InitRun() and FinishRun() members.
 * TofData in TofCollection is filled with adc and tdc data.
 * Extra checks for StEvent object to prevent null pointers.
 * Primitive ADC response function, disabled slatResponseExp().
 *
 * Revision 1.1  2001/09/28 19:11:11  llope
 * first version
 *
 **************************************************************************/
#ifndef STTOFSIMMAKER_HH
#define STTOFSIMMAKER_HH
#include "StMaker.h"

class StEvent;
class TH1F;
class StTofCalibration;
class StTofSimParam;
class StTofGeometry;
class StTofMCSlat;
struct g2t_ctf_hit_st;

class StTofSimMaker : public StMaker{
 private:
  StTofGeometry*    mGeomDb;//! 
  StTofCalibration* mCalibDb;//!
  StTofSimParam*    mSimDb;//!

  StEvent *mEvent;//!
  TH1F* mdE;//!
  TH1F* mdS;//!
  TH1F* mNumberOfPhotoelectrons;//!
  TH1F* mT;//!
  TH1F* mTime;//!
  TH1F* mTime1;//!
  TH1F* mPMlength;//!
  TH1F* mAdc;//!
  TH1F* mTdc;//!


 protected:
  StTofMCSlat detectorResponse(g2t_ctf_hit_st*);
  void fillRaw(void);
  void electronicNoise(void);
  void fillEvent();
  float slatResponseExp(float&);

 public: 
  StTofSimMaker(const char *name="TofSim");
  virtual ~StTofSimMaker();
  virtual Int_t Init();
  Int_t  InitRun(int);
  Int_t  FinishRun(int);
  virtual Int_t  Make();
  virtual Int_t  Finish();

  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StTofSimMaker.h,v 1.5 2014/08/06 11:43:49 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

  ClassDef(StTofSimMaker,0)
};
#endif
