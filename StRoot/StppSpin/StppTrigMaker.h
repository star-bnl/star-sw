//  modified by JB 2/2/01: trigOnCtb() isolated and upgraded
// 
//*-- Author : George , Jan Balewski 
// $Id: StppTrigMaker.h,v 1.3 2001/04/10 16:28:54 perev Exp $
// $Log: StppTrigMaker.h,v $
// Revision 1.3  2001/04/10 16:28:54  perev
// ClassDef(,1) -> "ClassDef ( ,0 )
//
// Revision 1.2  2001/02/28 19:06:13  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                     
//  Emulates trigger response for the M-C data
// 
//                                                                     
//////////////////////////////////////////////////////////////////////////
#ifndef STAR_StppTrigMaker
#define STAR_StppTrigMaker
   
//////////////////////////////////////////////////////////////////////////
//                                                              
// StppTrigMaker : Triggering high Pt events
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

struct trig_set {
  float CtbTofMax_ns; // (ns)
  float CtbDEnThres_mev; // (MeV)
  int CtbnSlatMax, CtbnDiPatchMax;
};

class StppTrigMaker : public StMaker 
{  
 
 private: //....................................................

  struct trig_set set;
  Int_t Finish();
  TH1F *h1;
  TH1F *h5;
  TH1F *h6;
  TH1F *h7;
  TH2F *h25;

  TH1F *hge[16];
  TH1F *hctb[16];
  void trigOnCtb(int&, int&, int&);
  void trigOnMwc(int&, int&);
  int getGeneratedLP(float &, int&, int&);
  void addMiniDst();

  // static Char_t  m_VersionCVS = "$Id: StppTrigMaker.h,v 1.3 2001/04/10 16:28:54 perev Exp $";
 
  // static Char_t  m_VersionCVS = "$Id: StppTrigMaker.h,v 1.3 2001/04/10 16:28:54 perev Exp $";

 protected:  //....................................................
  
 public: //....................................................
   void Setup(float x1, float x2,  int i1, int i2){
     set.CtbTofMax_ns=x1;
     set.CtbDEnThres_mev=x2;
     set.CtbnSlatMax=i1;
     set.CtbnDiPatchMax=i2;
   } ;
   StppTrigMaker(const char *name="ppTrig");
   virtual       ~StppTrigMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
   int  decision ; // see description 
   virtual const char *GetCVS() const
     {static const char cvs[]="Tag $Name:  $ $Id: StppTrigMaker.h,v 1.3 2001/04/10 16:28:54 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}
   
   ClassDef(StppTrigMaker,0)   //StAF chain virtual base class for Makers
};

#endif
















