// $Id: St_l3t_Maker.h,v 1.6 1999/12/23 18:09:08 yepes Exp $
// $Log: St_l3t_Maker.h,v $
// Revision 1.6  1999/12/23 18:09:08  yepes
// Double interface to read DAQ format or tpchit_st from tpc
//
// Revision 1.6  1999/12/21           yepes
// A lot of changes. Move to DAQ or offline format
// Revision 1.5  1999/07/15 13:58:16  perev
// cleanup
//
// Revision 1.4  1999/03/12 15:27:33  perev
// New maker schema
//
// Revision 1.3  1999/02/19 17:39:33  fisyak
// change given name of Maker
//
// Revision 1.1  1999/02/12 13:52:50  fisyak
// l3t maker from Pablo
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
    St_sl3TpcPara *m_sl3TpcPara; //! L3 tpc tracking parameters
    void              MakeHistograms();// Histograms for tracking
 protected:
    TH1F *m_l3_hits_on_track; //!number of hits assigned to a reconstructed track
    TH1F *m_l3_azimuth;       //!azimuthal angle
    TH1F *m_l3_tan_dip;       //!tangent of the dip angle
    TH1F *m_l3_r0;            //!radius where track parameters are given
    TH1F *m_l3_z0;            //!z0  where track parameters are given
    TH1F *m_l3_pt;            //!track pt
    TH1F *m_l3_XyChi2    ;    //!chi2 in xy plane
    TH1F *m_l3_SzChi2    ;    //!chi2 in sz plane
    TH1F *m_l3_nHitsSector;   //!nHits per sector
    TH1F *m_l3_nTracksSector; //!nTracks per sector
    TH1F *m_l3_cpuTimeSector; //!cpu  Time spend per sector
    TH1F *m_l3_realTimeSector;//!real Time spend per sector
 public: 
   St_l3t_Maker(const char *name="l3Tracks");
   virtual       ~St_l3t_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual Int_t  MakeOnLine();
   virtual Int_t  MakeOffLine();
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_l3t_Maker.h,v 1.6 1999/12/23 18:09:08 yepes Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_l3t_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
