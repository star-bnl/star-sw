// $Id: St_ebye_Maker.h,v 1.3 1998/10/06 18:00:36 perev Exp $
// $Log: St_ebye_Maker.h,v $
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
class St_foo_dst_event_summary ;
class St_foo_dst_track ;
class St_XDFFile;
class St_particle;
class St_sca_in;
class St_sca_out;
class St_sca_prior;
class St_ebye_Maker : public StMaker {
 private:
               Bool_t drawinit;
               St_sca_switch   *m_sca_switch;        //!
               St_sca_const    *m_sca_const;         //!
               St_sca_filter_const *m_sca_filter_const;  //!
               St_foo_dst_event_summary   *m_dst_event_summary;          //!
               St_foo_dst_track    *m_dsttrack;          //!
               St_particle     *m_particle;          //!
               St_sca_in       *m_sca_in;            //!
               St_sca_out *m_sca_out;            //!
               St_sca_prior *m_sca_prior;            //!
               St_sca_out *m_sca_ensemble_ave;            //!
 protected:
 public: 
                  St_ebye_Maker();
                  St_ebye_Maker(const char *name, const char *title);
   virtual       ~St_ebye_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   void SetDeltaD(Bool_t flag=kFALSE);     // *MENU*
   void SetDimension(Bool_t flag=kFALSE);  // *MENU*
   void SetEntropy(Bool_t flag=kFALSE);    // *MENU*
   void SetmakePrior(Bool_t flag=kFALSE);  // *MENU*
   void SetdoAnalysis(Bool_t flag=kFALSE); // *MENU*
   ClassDef(St_ebye_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
