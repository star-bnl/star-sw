/*!
 * \class StGenericVertexMaker
 * \author David Hardtke, based on Jan Balewskis template
 *
 * Maker for minuit based vertex finder
 * Lee Barnby - modification, becomes StGenericVertexMaker
 *
 * $Id: StGenericVertexMaker.h,v 1.11 2007/10/23 05:29:38 genevb Exp $
 *
 */

   
#ifndef STAR_StGenericVertexMaker
#define STAR_StGenericVertexMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif


class StEvent;
class StPrimaryVertex;
class StMinuitVertexFinder;
class StGenericVertexFinder;
class TNtuple;

class StGenericVertexMaker : public StMaker 
{
 private: 
  // control and cuts
  Bool_t  use_ITTF;
  Bool_t  usebeamline;
  Bool_t  useCTB;
  Bool_t  eval;
  Bool_t  externalFindUse; /// Finder will by called externally (by StiMaker)
  Int_t   minTracks;

  TNtuple *mEvalNtuple;    /// Ntuple for evaluation purposes

  StEvent *mEvent;
  StPrimaryVertex* primV;
  StGenericVertexFinder *theFinder;

  Bool_t DoFit(); ///Find and fit the primary vertex
  void const FillStEvent();
  void MakeEvalNtuple();

  Int_t nEvTotal,nEvGood;

 protected:
  int m_Mode2; // auxiliary flags 

 public: 
  StGenericVertexMaker(const char *name="GenericVertex");
  virtual       ~StGenericVertexMaker();
  virtual Int_t Init();
  virtual Int_t InitRun (int runumber);
  virtual void  Clear(const char* opt="");
  virtual Int_t Finish();
  virtual Int_t Make();
  void SetMode2(int x) 	{m_Mode2=x;}
  int  GetMode2() 	{return m_Mode2; }
  inline StGenericVertexFinder* GetGenericFinder(){return (StGenericVertexFinder*)theFinder;};

  inline void UseBeamLine()		{usebeamline    = kTRUE; }
  inline void DoNotUseBeamLine()	{usebeamline    = kFALSE;}
  inline void UseCTB()			{useCTB         = kTRUE; }
  inline void DoNotUseCTB()		{useCTB         = kFALSE;}
  inline void DoEval()			{eval           = kTRUE; }
  inline void SetInternalFind()		{externalFindUse= kFALSE;}
  inline void SetUseITTF()		{use_ITTF       = kTRUE; }
  inline void SetDoNotUseITTF()		{use_ITTF       = kFALSE;}
  inline void SetMinimumTracks(int n)   {minTracks      = n;}

  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StGenericVertexMaker.h,v 1.11 2007/10/23 05:29:38 genevb Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  
  ClassDef(StGenericVertexMaker, 0)   //StAF chain virtual base class for Makers
};
    
#endif
    



