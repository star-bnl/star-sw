// $Id: St_emc_Maker.h,v 1.1 1998/12/06 10:23:31 akio Exp $
// $Log: St_emc_Maker.h,v $
// Revision 1.1  1998/12/06 10:23:31  akio
// Creation
//
// Revision 1.2  1998/12/06 09:57:13  akio
// put histgrams
//
// Revision 1.1  1998/12/03 22:27:26  akio
// New emc maker
#ifndef STAR_St_emc_Maker
#define STAR_St_emc_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_emc_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_emc_calib_header;
class St_emc_pedestal;
class St_emc_adcslope;
class TH1F;
class TH2F;
class St_emc_Maker : public StMaker {
 private:
  Bool_t drawinit;
  St_emc_calib_header *m_ped_bemc_h; //!
  St_emc_pedestal     *m_ped_bemc;   //!
  St_emc_calib_header *m_slp_bemc_h; //!
  St_emc_adcslope     *m_slp_bemc;   //!
  void MakeHistograms();             // Histograms
 protected:
   TH1F *m_emc_etot;    //!
   TH2F *m_emc_energy;  //! 
   TH2F *m_emc_nhit;    //! 
   TH2F *m_bemc_hit;    //!
   TH2F *m_bprs_hit;    //!
   TH2F *m_bsmde_hit;   //!
   TH2F *m_bsmdp_hit;   //!
   TH2F *m_eemc_hit;    //!
   TH2F *m_eprs_hit;    //!
   TH2F *m_esmde_hit;   //!
   TH2F *m_esmdp_hit;   //!
 public: 
                  St_emc_Maker(const char *name="emc_hits", const char *title="event/data/emc/hits");
   virtual       ~St_emc_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   ClassDef(St_emc_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif









