//*-- Author : David Hardtke, based on Jan Balewskis template
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
//   Maker for minuit based vertex finder
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StMinuitVertexMaker
#define STAR_StMinuitVertexMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif


class StEvent;
class StPrimaryVertex;
class StMinuitVertexFinder;
class TNtuple;
class StMinuitVertexMaker : public StMaker 
{
 private: 
  // static Char_t  m_VersionCVS = "$Id: StMinuitVertexMaker.h,v 1.1 2002/12/05 23:42:46 hardtke Exp $";

  // setup
  bool use_beamline;
  bool use_CTB;
  float EtaCut;
  TNtuple *ntuple;
  StEvent *stEvent;
  StPrimaryVertex* primV;

  StMinuitVertexFinder *myfinder;

  void DoFit();
  bool FillStEvent();
  void MakeEvalNtuple();

  int nEVtot,nEVfound;
 protected:

 public: 
  StMinuitVertexMaker(const char *name="MinuitVertex");
  virtual       ~StMinuitVertexMaker();
  virtual Int_t Init();
  virtual Int_t InitRun  (int runumber);
  virtual Int_t Finish();
  virtual Int_t  Make();
  void UseBeamLine();
  void DoNotUseBeamLine();
  void UseCTB();
  void DoNotUseCTB();
  bool eval;

  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 
  
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StMinuitVertexMaker.h,v 1.1 2002/12/05 23:42:46 hardtke Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  
  ClassDef(StMinuitVertexMaker, 0)   //StAF chain virtual base class for Makers
};
    
#endif
    



