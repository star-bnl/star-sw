//! $Id: St_QA_Maker.h,v 1.40 2000/03/28 19:19:20 fine Exp $
//! $Log: St_QA_Maker.h,v $
//! Revision 1.40  2000/03/28 19:19:20  fine
//! Adjuested to ROOT 2.24
//!
//! Revision 1.39  2000/02/09 19:22:23  kathy
//! protect MakeHistEval method so that if there is no geant dataset, it skips out
//!
//! Revision 1.38  2000/02/07 19:49:07  kathy
//! removed L3 trigger histograms and methods that created them - this table is no longer standard on the DST; created methods BookHistEval and MakeHistEval for geant vs reco evaluation histograms; filled geant vs reco evaluation histograms for table-based data
//!
//! Revision 1.37  1999/11/23 19:00:51  lansdell
//! Reorganized Make() and include files (Gene)
//!
//! Revision 1.36  1999/11/19 22:44:45  kathy
//! took histogram booking out of St_QA_Maker as per Thomas' request and put it into separate class StQABookHist which can now be used also by Curtis' class to book histograms - thanks for your help Gene!
//!
//! Revision 1.35  1999/11/18 22:48:44  kathy
//! remove commented out lines
//!
//! Revision 1.34  1999/11/18 22:34:14  kathy
//! removed some histograms of variables that no longer exist and change some limits
//!
//! Revision 1.33  1999/09/29 16:46:32  kathy
//! changed code so it would compile in .dev due to changes in DST tables - I even used cons instead of makel - wow! - I just changed variables or commented out some histograms that use now-non-existant variables so it would compile - later I will go through and redefine histograms as needed
//!
//! Revision 1.32  1999/09/23 18:54:11  kathy
//! fix some histogram limits, add about 10 histograms - just so we know number rows in each table - had to include some more tables to do this
//!
//! Revision 1.31  1999/09/21 15:05:38  kathy
//! comment out unneccessary method: SetPntrToHistUtil because now I'm making it totally independent of the histograms printing at the end - also put in doc directory and html file - basically empty now
//!
//! Revision 1.30  1999/09/20 20:12:19  kathy
//! moved the histogram utility methods out of St_QA_Maker and into StHistUtil because they can really be used by any Maker and associated histograms
//!
//! Revision 1.29  1999/07/17 01:51:21  kathy
//! changed limits and titles of some histograms
//!
//! Revision 1.28  1999/07/15 13:57:41  perev
//! cleanup
//!
//! Revision 1.27  1999/07/14 23:23:00  kathy
//! a lot of changes to hist limits and fixes to titles and added a few new hist
//!
//! Revision 1.26  1999/07/12 16:39:35  kathy
//! hopefully last change for globtrk,event_summary and primtrk histograms
//!
//! Revision 1.25  1999/07/09 23:04:07  kathy
//! hopefully getting to final round of fixes to globtrk and primtrk histograms
//!
//! Revision 1.24  1999/07/09 13:14:19  kathy
//! now have put in new primtrk histograms to match the globtrk ones
//!
//! Revision 1.23  1999/07/07 16:58:34  kathy
//! put log scales on some histograms
//!
//! Revision 1.22  1999/07/02 21:56:57  kathy
//! update for tables which exist in 99f AND put in changes to event summary and globtrk histogram sets requested by offline analysis meeting
//!
//! Revision 1.21  1999/06/15 14:44:57  kathy
//! fix St_QA_Maker
//!
//! Revision 1.19  1999/06/11 20:05:54  kathy
//! put in method FindHists to find the histogram directory, since it can be in different places depending on how/where you make the histograms
//!
//! Revision 1.18  1999/05/10 20:03:56  kathy
//! add new member function ExamineLogYList and RemoveFromLogYList
//!
//! Revision 1.17  1999/05/10 17:16:18  kathy
//! added new member function SetDefaultLogYList and implemented and tested
//!
//! Revision 1.16  1999/05/07 17:18:30  kathy
//! new method AddToLogYList implemented and tested on solaris
//!
//! Revision 1.15  1999/05/05 19:35:53  kathy
//! add new method ListHists and clean up
//!
//! Revision 1.14  1999/04/21 20:19:19  kathy
//! put in comments and cleaned up - works for mdc2 dst in dev now
//!
//! Revision 1.13  1999/04/20 01:16:59  fisyak
//! Add check on. no of tracks in dE/dX
//!
//! Revision 1.12  1999/03/11 21:13:14  kathy
//! update to hist limits
//!
//! Revision 1.11  1999/03/09 16:30:24  fine
//! Workqround of the St_io_Maker bug
//!
//! Revision 1.10  1999/03/07 19:26:16  fine
//! QA->SetPostScriptFile(psFile) has been introduced
//!
//! Revision 1.9  1999/03/07 16:53:33  fine
//! New method DrawHists
//!
//! Revision 1.8  1999/03/05 21:19:38  kathy
//! added new histograms
//!
//! Revision 1.7  1999/03/03 23:34:30  kathy
//! fixes to histograms
//!
//! Revision 1.6  1999/02/26 18:42:34  kathy
//! added vertex histograms
//
//! Revision 1.5  1999/02/25 19:25:39  kathy
//! fix up histograms
//
//! Revision 1.4  1999/02/24 21:15:04  kathy
//! fixed histograms and added a few new ones
//
//! Revision 1.3  1999/02/23 22:22:22  kathy
//! changes to histograms: titles changed so they'll be in order and redundant ones removed
//
//! Revision 1.2  1999/02/22 21:27:18  kathy
//! moved hist from St_glb_Maker to St_QA_Maker and had to rename some etc
//
//! Revision 1.1  1999/02/08 19:28:33  didenko
//! fixed directory level
//
//! Revision 1.3  1999/01/22 22:21:07  didenko
//! header file for  QA maker
//
//! Revision 1.2  1998/12/21 19:43:19  fisyak
//! Move ROOT includes to non system
//
//! Revision 1.1  1998/11/01 16:42:26  fisyak
//! dst analysis
//
//! Revision 1.4  1998/10/31 00:26:13  fisyak
//! Makers take care about branches
//
//! Revision 1.3  1998/10/06 18:00:34  perev
//! cleanup
//
//! Revision 1.2  1998/09/08 22:43:11  fisyak
//! Modify St_QA_Maker to account new calling sequence
//
//! Revision 1.1  1998/08/18 14:06:07  fisyak
//! Add to bfc dst
//
//! Revision 1.3  1998/08/10 02:32:07  fisyak
//! Clean up
//
//! Revision 1.2  1998/07/20 15:08:15  fisyak
//! Add tcl and tpt
//
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  St_QA_Maker class for QA Histograms using dst tables                 //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

#ifndef STAR_St_QA_Maker
#define STAR_St_QA_Maker

#include "StQABookHist.h"
#include "St_DataSet.h"

//////////////////////////////////////////////////////////////////////////

class St_QA_Maker : public StQABookHist {
 private:
  //! static Char_t m_VersionCVS = "$Id: St_QA_Maker.h,v 1.40 2000/03/28 19:19:20 fine Exp $";

  St_DataSet *dst;        //! Pointer to current dataset - dst
  
//------------------------------------------------------------------------
  
 public: 

  St_QA_Maker(const char *name="QA", const char *title="event/QA");
  virtual       ~St_QA_Maker();
  virtual Int_t  Init();
  virtual Int_t  Finish();
  virtual Int_t  Make();

  virtual void   MakeHistEvSum();
  virtual void   MakeHistGlob();
  virtual void   MakeHistDE();
  virtual void   MakeHistPrim();
  virtual void   MakeHistGen();
  virtual void   MakeHistV0();
  virtual void   MakeHistPID();
  virtual void   MakeHistVertex();
  virtual void   MakeHistXi();
  virtual void   MakeHistPoint();
  virtual void   MakeHistKink();
  virtual void   MakeHistV0Eval();
  virtual void   MakeHistRich();
  virtual void   MakeHistEval();

// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_QA_Maker.h,v 1.40 2000/03/28 19:19:20 fine Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(St_QA_Maker, 1)   //StAF chain virtual base class for Makers
    };
    
#endif









