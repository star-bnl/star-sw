// $Id: St_l3t_Maker.h,v 1.2 1999/02/19 14:39:32 fisyak Exp $
// $Log: St_l3t_Maker.h,v $
// Revision 1.2  1999/02/19 14:39:32  fisyak
// New version from Pablo, tpc safe
//
// Revision 1.6  1999/01/12 19:50:20  sakrejda
// QA histograms added to the tpt maker
//
// Revision 1.5  1999/01/08 23:19:42  sakrejda
// histogramming added
//
// Revision 1.4  1998/10/31 00:26:24  fisyak
// Makers take care about branches
//
// Revision 1.3  1998/10/06 18:00:51  perev
// cleanup
//
// Revision 1.2  1998/08/18 14:05:04  fisyak
// Add to bfc dst
//
// Revision 1.1  1998/07/21 00:36:47  fisyak
// tcl and tpt
//
#ifndef STAR_St_l3t_Maker
#define STAR_St_l3t_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_l3t_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_sl3TpcPara;
class TH1F;

class St_l3t_Maker : public StMaker {
 private:
    Bool_t drawinit;
    St_sl3TpcPara *m_sl3TpcPara; //! L3 tpc tracking parameters
    void              MakeHistograms();// Histograms for tracking
 protected:
    TH1F *m_l3_hits_on_track; //!number of hits assigned to a reconstructed track
    TH1F *m_l3_azimuth;       //!azimuthal angle
    TH1F *m_l3_tan_dip;       //!tangent of the dip angle
    TH1F *m_l3_r0;            //!radius for the first point
 public: 
   St_l3t_Maker(const char *name="tpc_tracks", const char *title="event/data/tpc/tracks");
   virtual       ~St_l3t_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   ClassDef(St_l3t_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
