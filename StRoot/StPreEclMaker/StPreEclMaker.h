//
// $Id: StPreEclMaker.h,v 1.1 2000/05/15 21:24:01 subhasis Exp $
//
// $Log: StPreEclMaker.h,v $
// Revision 1.1  2000/05/15 21:24:01  subhasis
// initial version
//
//
// Authors: Subhasis Chattopadhyay,
//          Aleksei Pavlinov , July 1999.
//          initial version from Akio Ogawa    
//    

#ifndef STAR_StPreEclMaker
#define STAR_StPreEclMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include <TH1.h>
#include <TH2.h>
#include "St_emc_Maker/StEmcHitCollection.h"
#include "StEmcPreClusterCollection.h"
#include "StBemcPreClusterCollection.h"
#include "StBsmdePreClusterCollection.h"
#include "StBsmdpPreClusterCollection.h"
#include "emc_def.h"

class StPreEclMaker : public StMaker {
private:
//  Bool_t drawinit; 
//  Int_t  m_mode;           // 
  void MakeHistograms();   // Filling QA Histograms
protected:
  TH2F *m_ncl;             //! 
  TH2F *m_etot;            //!
  TH2F *m_sig_e;           //!
  TH2F *m_sig_p;           //!
  TH2F *m_cl[MAXDET];      //!
  TH2F *m_energy[MAXDET];  //!
  TH1F *m_HitsInCl[MAXDET];//!
  TH1F *m_EnergyCl[MAXDET];//!
  TH1F *m_EtaInCl[MAXDET];//!
  TH1F *m_PhiInCl[MAXDET];//!
public: 
  StPreEclMaker(const char *name="ecl", const char *title="event/data/emc/hits");
  virtual ~StPreEclMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t fillStEvent();
  virtual void PrintInfo();
  ClassDef(StPreEclMaker, 1)// Electromagnetic PreClusters maker
};

#endif
