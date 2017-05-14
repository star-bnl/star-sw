/*!
 * \class StGenericVertexMaker
 * \author David Hardtke, based on Jan Balewskis template
 *
 * Maker for minuit based vertex finder
 * Lee Barnby - modification, becomes StGenericVertexMaker
 *
 * $Id: StGenericVertexMaker.h,v 1.22 2017/05/12 18:37:24 smirnovd Exp $
 *
 */

   
#ifndef STAR_StGenericVertexMaker
#define STAR_StGenericVertexMaker

#include "StChain/StMaker.h"


class StEvent;
class StPrimaryVertex;
class StMinuitVertexFinder;
class StGenericVertexFinder;
class TNtuple;

class StGenericVertexMaker : public StMaker 
{
 private: 
  // control and cuts
  Bool_t  useITTF;
  Bool_t  useBeamline;
  Bool_t  useCTB;
  Bool_t  usePCT;
  Bool_t  useBTOF;
  Bool_t  eval;
  /// Finder will by called externally (by StiMaker). If set to true no finding
  /// actually done
  Bool_t  externalFindUse;
  Int_t   minTracks;

  TNtuple *mEvalNtuple;    /// Ntuple for evaluation purposes

  StEvent *mEvent;
  StPrimaryVertex* primV;
  StGenericVertexFinder *theFinder;

  Bool_t DoFit(); ///Find and fit the primary vertex
  void const FillStEvent();
  void MakeEvalNtuple();

  Int_t nEvTotal,nEvGood;

 public: 
  StGenericVertexMaker(const char *name="GenericVertex");
  virtual       ~StGenericVertexMaker();
  virtual Int_t Init();
  virtual Int_t InitRun (Int_t runumber);
  virtual void  Clear(const char* opt="");
  virtual Int_t Finish();
  virtual Int_t Make();
  StGenericVertexFinder* GetGenericFinder(){return theFinder;}

  void UseBeamLine()            {SetAttr("BeamLine"       , kTRUE );}
  void DoNotUseBeamLine()       {SetAttr("BeamLine"       , kFALSE);}
  void UseCTB()                 {SetAttr("CTB"            , kTRUE );}
  void DoNotUseCTB()            {SetAttr("CTB"            , kFALSE);}
  void DoEval()                 {SetAttr("eval"           , kTRUE );}
  void SetInternalFind()        {SetAttr("externalFindUse", kFALSE);}
  void SetUseITTF()             {SetAttr("ITTF"           , kTRUE );}
  void SetDoNotUseITTF()        {SetAttr("ITTF"           , kFALSE);}
  void SetMinimumTracks(Int_t n){SetAttr("minTracks"      , n     );}
  void UsePCT()                 {SetAttr("PCT"            , kTRUE );}
  void DoNotUsePCT()            {SetAttr("PCT"            , kFALSE);}

  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StGenericVertexMaker.h,v 1.22 2017/05/12 18:37:24 smirnovd Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  
  ClassDef(StGenericVertexMaker, 0)   //StAF chain virtual base class for Makers
};
    
#endif
