//*-- Author : Jan Balewski
//  
// JB 3/30/01 - divorce with MC. Only StEvent is used. No evaluation
//
// $Id: StppLPfindMaker.h,v 1.1.1.2 2001/04/21 00:43:13 fisyak Exp $
// $Log: StppLPfindMaker.h,v $
// Revision 1.1.1.2  2001/04/21 00:43:13  fisyak
// *** empty log message ***
//
// Revision 1.4  2001/04/12 15:19:09  balewski
// *** empty log message ***
//
// Revision 1.2  2001/02/28 19:06:12  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
//   Search for the leading charge particle in the event                //
//   use DST as input
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StSpinPPMaker
#define STAR_StSpinPPMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

//class St_jdata1;

class StEvent;
class St_dst_track;
class St_tpt_track;
class dst_track_st;
class StPrimaryVertex;

class StppLPfindMaker : public StMaker 
{
 private: 
  // static Char_t  m_VersionCVS = "$Id: StppLPfindMaker.h,v 1.1.1.2 2001/04/21 00:43:13 fisyak Exp $";

  // setup
  float EtaCut;
  TH1F *hv[16];
  StEvent *stEvent;
  StPrimaryVertex* primV;

  void init_histo();

  float  PtPz2Eta(float, float);

  void DcaTract2Vert(dst_track_st *rLP,float &delZ, float &lpRxy, float &delRxy);
  int nEVtot,nEVfound;
 protected:

 public: 
  StppLPfindMaker(const char *name="ppLPfind0");
  virtual       ~StppLPfindMaker();
  virtual Int_t Init();
  virtual Int_t Finish();
  virtual Int_t  Make();

  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 
  
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StppLPfindMaker.h,v 1.1.1.2 2001/04/21 00:43:13 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  
  ClassDef(StppLPfindMaker, 0)   //StAF chain virtual base class for Makers
};
    
#endif
    



