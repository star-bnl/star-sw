// $Id: St_ebye_Maker.h,v 1.8 1999/07/15 13:58:00 perev Exp $
// $Log: St_ebye_Maker.h,v $
// Revision 1.8  1999/07/15 13:58:00  perev
// cleanup
//
// Revision 1.7  1999/03/12 14:39:41  perev
// New maker schema
//
// Revision 1.6  1999/01/27 00:17:55  dhammika
// EbyE PKG works for more than one event in ROOT
//
// Revision 1.5  1999/01/05 14:11:08  dhammika
// Updated to be in synch with stardev and the latest SCA V2.0 
//
// Revision 1.4  1998/10/31 00:26:13  fisyak
// Makers take care about branches
//
// Revision 1.3  1998/10/06 18:00:36  perev
// cleanup
//
// Revision 1.2  1998/08/07 19:26:10  dhammika
// event by event chain in root
//
// Revision 1.1  1998/08/05 14:33:37  fisyak
// Add ebye
//
// Revision 1.1  1998/07/21 00:36:46  fisyak
// tcl and tpt
//
#ifndef STAR_St_ebye_Maker
#define STAR_St_ebye_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_ebye_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_sca_switch;
class St_sca_filter_const;
class St_sca_const;
class St_dst_run_header;
class St_dst_event_header ;
class St_dst_track ;
class St_XDFFile;
class St_sca_in;
class St_sca_out;
class St_sca_prior;
class St_ebye_Maker : public StMaker {
 private:
               St_sca_switch              *m_sca_switch;           //!
               St_sca_const               *m_sca_const;            //!
               St_sca_filter_const        *m_sca_filter_const;     //!
               St_sca_prior               *m_sca_prior;            //!
               St_sca_out                 *m_sca_ensemble_ave;     //!
               St_dst_run_header          *this_dst_run_header;    //!
               St_dst_event_header        *this_dst_event_header;  //!
               St_dst_track               *this_dst_track;         //!
               St_sca_in                  *this_sca_in;            //!
               St_sca_out                 *this_sca_out;           //!
 protected:
 public: 
                  St_ebye_Maker(const char *name="ebye");
   virtual       ~St_ebye_Maker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   Int_t SetmakePrior(Bool_t flag=kFALSE);          // *MENU*
   Int_t SetmakeEnsembleAve(Bool_t flag=kFALSE);    // *MENU*
   Int_t SetdoAnalysis(Bool_t flag=kFALSE);         // *MENU*
   Int_t SetnEvents(Int_t     nEvents=0);           // *MENU*
   Int_t PutPrior();                                // *MENU*
   Int_t PutEnsembleAve();                          // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_ebye_Maker.h,v 1.8 1999/07/15 13:58:00 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_ebye_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
