// $Id: St_ems_Maker.h,v 1.7 1999/07/01 15:59:12 pavlinov Exp $ 
// $Log: St_ems_Maker.h,v $
// Revision 1.7  1999/07/01 15:59:12  pavlinov
// added QA histograms and new switches for control of maker
//
// Revision 1.6  1999/03/20 22:36:35  perev
// maker new schema
//
// Revision 1.5  1998/12/15 22:38:47  akio
// Add some comments
//
// Revision 1.4  1998/12/06 10:25:45  akio
// re-commit
//
// Revision 1.1  1998/11/30 21:18:31  fisyak
// ems raw data Maker
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
#ifndef STAR_St_ems_Maker
#define STAR_St_ems_Maker
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_ems_Maker class for begin_html <FONT COLOR="RED">EMC Simulation</FONT> end_html dataset//
// The maker do next steps for transition from Geant hits to ADC:       //
//   (same as chain 'ems' in bfc.kumac)                                 //
// PAM<FONT COLOR="BLUE">emc_interface2</FONT>  Transition from Geant hits to Deposit Energy   // 
//                       in each cells of detectors;                    //
// PAM<FONT COLOR="BLUE">dep_e_toadc</FONT>  Transition from Deposit Energy  in each cells of // 
//                       detectors to ADC;                              //
// PAM<FONT COLOR="BLUE">emc_adc_sim</FONT>  Taking into account pedestal and slope variation// 
//                       (only for BEMC now).                           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "TH2.h"
#include "TH1.h"
#include "emc/inc/emc_def.h"
#include "St_emc_Maker/StEmcGeom.h"
#include "St_emc_hits_Table.h"

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
  St_ems_control      *m_ems_control;     //!
  St_control_toadc    *m_control_toadc;   //! 
  St_ems_cal_control  *m_ems_cal_control; //!
  St_emc_calib_header *m_org_ped_bemc_h;  //!
  St_emc_pedestal     *m_org_ped_bemc;    //!
  St_emc_calib_header *m_org_slp_bemc_h;  //!
  St_emc_adcslope     *m_org_slp_bemc;    //!
  St_calb_calg        *m_calb_calg;       //!  

  StEmcGeom *mGeom[MAXDET]; //! Geometry 
  Short_t mHistControl;     // Do histogramms (1) or no (0)
  Short_t mBEMC;            // Switch for BEMC; 0 => off; 1=> on
  Short_t mEEMC;            // Switch for EEMC; 0 => off; 1=> on

protected:
  TH2F *m_nhit;           //! 
  TH2F *m_etot;           //!
  TH2F *m_hits[MAXDET];   //!
  TH2F *m_energy[MAXDET]; //!
  TH1F *m_adc[MAXDET];    //!
public: 
  St_ems_Maker(const char *name="emc_raw");
  ~St_ems_Maker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual void  PrintInfo();
  virtual void  printNameOfTables();
  virtual void  bookHistograms(const Int_t);
  virtual void  makeHistograms(const Int_t, St_emc_hits*);

  Short_t getBEMC(){return mBEMC;}
  Short_t getEEMC(){return mEEMC;}
  Short_t getHistControl(){return mHistControl;}
  void   printmBEMC();
  void   printmEEMC();
  void   setBEMC(Short_t key){mBEMC = key; if (Debug()) printmBEMC();}
  void   setEEMC(Short_t key){mEEMC = key; if (Debug()) printmEEMC();}
  void   setHistControl(Short_t key) {mHistControl = key;}
  ClassDef(St_ems_Maker, 1)  
};

#endif
