// $Id: St_ctf_Maker.h,v 1.9 2014/08/06 11:43:54 jeromel Exp $
// $Log: St_ctf_Maker.h,v $
// Revision 1.9  2014/08/06 11:43:54  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.8  2003/09/10 19:47:44  perev
// ansi corrs
//
// Revision 1.7  1999/07/15 13:57:49  perev
// cleanup
//
// Revision 1.6  1999/03/11 03:55:07  perev
// new schema
//
// Revision 1.5  1999/02/23 21:25:43  llope
// fixed histograms, added 1/beta vs p
//
// Revision 1.4  1999/02/06 00:15:47  fisyak
// Add adc/tdc histograms
//
// Revision 1.3  1999/01/25 23:39:13  fisyak
// Add tof
//
// Revision 1.2  1999/01/02 19:08:14  fisyak
// Add ctf
//
// Revision 1.1  1999/01/01 02:39:38  fisyak
// Add ctf Maker
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:31  perev
// cleanup
//
// Revision 1.5  1998/08/26 12:15:13  fisyak
// Remove asu & dsl libraries
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
#ifndef STAR_St_ctf_Maker
#define STAR_St_ctf_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_ctf_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_ctg_geo;
class St_ctg_slat_phi;
class St_ctg_slat_eta;
class St_ctg_slat;
class St_cts_mpara;
class TH1F;
class TH2F;
class St_ctf_Maker : public StMaker {
 private:
   Bool_t drawinit;
   St_ctg_geo       *m_ctb;          //!
   St_ctg_slat_phi  *m_ctb_slat_phi; //!
   St_ctg_slat_eta  *m_ctb_slat_eta; //!
   St_ctg_slat      *m_ctb_slat;     //!
   St_cts_mpara     *m_cts_ctb;      //!
   St_ctg_geo       *m_tof;          //!
   St_ctg_slat_phi  *m_tof_slat_phi; //!
   St_ctg_slat_eta  *m_tof_slat_eta; //!
   St_ctg_slat      *m_tof_slat;     //!
   St_cts_mpara     *m_cts_tof;      //!
 protected:
   TH1F *m_adcc;   //!
   TH1F *m_adct;   //!
   TH2F *m_tsvsp;  //!
   TH2F *m_tsvsp1; //!
 public: 
                  St_ctf_Maker(const char *name="ctf");
   virtual       ~St_ctf_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_ctf_Maker.h,v 1.9 2014/08/06 11:43:54 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

   ClassDef(St_ctf_Maker,0)   //StAF chain virtual base class for Makers
};

#endif
