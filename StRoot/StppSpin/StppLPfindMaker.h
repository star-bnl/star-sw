//*-- Author : Jan Balewski
//  
// $Id: StppLPfindMaker.h,v 1.2 2001/02/28 19:06:12 balewski Exp $
// $Log: StppLPfindMaker.h,v $
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
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StSpinPPMaker
#define STAR_StSpinPPMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "LeadPartAnal.h"

class St_jdata1;

class St_dst_track;
class St_tcl_tphit;
class St_g2t_track;
class g2t_track_st;

class St_g2t_tpc_hit;
class StEvent;
class St_tpt_track;

class StppLPfindMaker : public StMaker 
{
 private: 
  // static Char_t  m_VersionCVS = "$Id: StppLPfindMaker.h,v 1.2 2001/02/28 19:06:12 balewski Exp $";

  // setup
  float EtaCut;
  int MinTclPts;
  int  g2tTpcNhitCut;
  float h2hMatchRmin;
  float t2tMatchEff;
  
  StEvent *stEvent;
  TH1F *hv[16];

  void init_histo();
  void printStat(); 

  float  PtPz2Eta(float, float);

  void getGeneratedLP(St_g2t_track *, g2t_track_st **);

  St_jdata1 *pickHit( g2t_track_st *, St_g2t_tpc_hit *);
  St_jdata1 *pickHit(St_dst_track * , St_tcl_tphit *,int);

  void copyTpcTr2Dst(St_tpt_track * ,St_dst_track *);
  int match1Gt2Rt(St_jdata1 *,St_jdata1 *, int *);

  //some counters
  struct  LeadPartAnal lpa[MX_LPA];
  int nEVtot, nTrigOK, nAcc;
 protected:

 public: 
  StppLPfindMaker(const char *name="ppLPfind0");
  virtual       ~StppLPfindMaker();
  virtual Int_t Init();
  virtual Int_t Finish();
  virtual void Clear(const char *opt);
  virtual Int_t  Make();
  int *trigDecision;

  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 
  
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StppLPfindMaker.h,v 1.2 2001/02/28 19:06:12 balewski Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  
  ClassDef(StppLPfindMaker, 1)   //StAF chain virtual base class for Makers
};
    
#endif
    



