// $Id: St_emc_Maker.h,v 1.6 1999/07/02 03:01:56 pavlinov Exp $
// $Log: St_emc_Maker.h,v $
// Revision 1.6  1999/07/02 03:01:56  pavlinov
// Little corrections for Linux
//
// Revision 1.4  1999/02/12 19:19:17  akio
// *** empty log message ***
//
// Revision 1.3  1998/12/21 19:45:40  fisyak
// Move ROOT includes to non system
//
// Revision 1.2  1998/12/15 22:39:52  akio
// Add emc_hit object and  adc_to_energy in here.
//
#ifndef STAR_St_emc_Maker
#define STAR_St_emc_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_emc_Maker class for <FONT COLOR="RED">EMc Calibration</FONT> dataset     //
//
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "TH2.h"
#include "St_ems_hits_Table.h"
#include "St_emc_pedestal_Table.h"
#include "St_emc_adcslope_Table.h"
#include "St_emc_calib_header_Table.h"
#include "St_emc_hits_Table.h"
#include "StEmcHitCollection.h"
#include "emc/inc/emc_def.h"

class St_emc_Maker : public StMaker {
private:
  Bool_t drawinit; 
  Int_t  m_mode;               // mode=0/1 No/Create copy in emc_hits Table;
  St_DataSet     *mEmcCalib;   //!  For StEmcCollection::ADCtoEnergy
  void MakeHistograms();       // Filling QA Histograms
protected:
  TH2F *m_nhit;           //! 
  TH2F *m_etot;           //!
  TH2F *m_hits[MAXDET];   //!
  TH2F *m_energy[MAXDET]; //!
public: 
  St_emc_Maker(const char *name="emc_hit", const char *title="event/data/emc/hits");
  virtual ~St_emc_Maker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual void Set_mode (Int_t m = 0){m_mode = m;}; // *MENU*  
  virtual void PrintInfo();
  St_DataSet     *getEmcCalib()   {return mEmcCalib;};
  ClassDef(St_emc_Maker, 1)   //Macro
};

#endif
