// $Id: St_ems_Maker.h,v 1.2 1998/12/03 22:30:10 akio Exp $
// $Log: St_ems_Maker.h,v $
// Revision 1.2  1998/12/03 22:30:10  akio
// Include dep_e_toadc and emc_adc_sim
//
// Revision 1.1  1998/11/30 21:18:31  fisyak
// ems raw data Maker
#ifndef STAR_St_ems_Maker
#define STAR_St_ems_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_ems_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_ems_control;
class St_control_toadc;
class St_ems_cal_control;
class St_emc_calib_header;
class St_emc_pedestal;
class St_emc_adcslope;
class St_calb_calg;
class St_ems_Maker : public StMaker {
 private:
   Bool_t drawinit;
   St_ems_control      *m_ems_control;    //!
   St_control_toadc    *m_control_toadc;  //!
   St_ems_cal_control  *m_ems_cal_control;//!
   St_emc_calib_header *m_org_ped_bemc_h; //!
   St_emc_pedestal     *m_org_ped_bemc;   //!
   St_emc_calib_header *m_org_slp_bemc_h; //!
   St_emc_adcslope     *m_org_slp_bemc;   //!
   St_calb_calg        *m_calb_calg;      //!
 protected:
 public: 
                  St_ems_Maker(const char *name="emc_raw", const char *title="event/raw_data/emc");
   virtual       ~St_ems_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   ClassDef(St_ems_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif


