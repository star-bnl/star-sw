#ifndef STAR_StppTrigMaker
#define STAR_StppTrigMaker
   
//////////////////////////////////////////////////////////////////////////
//                                                              
// StppTrigMaker : Triggering high Pt events
//                                                                     
//  Copy selected info from staf tables to my class of JTable 
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class StEvent;
class StppMiniDst;

#include "TH2.h"// for histos
class TH1F;
class TH2F;
struct trig_set
{ float ctb_time_max_ns;
  float ctb_ener_thr_mev;
  int ctb_sumADC_min; 
  int mwc_sect_mult_min; };

class StppTrigMaker : public StMaker 
{  
 
 private: //....................................................

  struct trig_set set;
  Int_t Finish();
  TH1F *h1;
  TH1F *h2;
  TH1F *h3;
  TH1F *h4;
  TH1F *h5;
  TH1F *h6;
  TH1F *h7;
  TH2F *h25;

  TH1F *hj[8];

  // static Char_t  m_VersionCVS = "$Id: StppTrigMaker.h,v 1.1.1.1 2001/01/31 14:00:07 balewski Exp $";
 
   Bool_t drawinit;
// static Char_t  m_VersionCVS = "$Id: StppTrigMaker.h,v 1.1.1.1 2001/01/31 14:00:07 balewski Exp $";


 protected:  //....................................................
  

 public: //....................................................
   StppTrigMaker(const char *name="ppTrig");
   void Setup(float x1,  float x2, int i1, int i2){
     set.ctb_time_max_ns=x1;
     set.ctb_ener_thr_mev=x2;
     set.ctb_sumADC_min=i1;
     set.mwc_sect_mult_min=i2;} ;
   virtual       ~StppTrigMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
   int  decision ; // see description 
   virtual const char *GetCVS() const
     {static const char cvs[]="Tag $Name:  $ $Id: StppTrigMaker.h,v 1.1.1.1 2001/01/31 14:00:07 balewski Exp $ built "__DATE__" "__TIME__ ; return cvs;}
   
   ClassDef(StppTrigMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
















