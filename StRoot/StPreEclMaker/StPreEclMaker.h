//
// $Id: StPreEclMaker.h,v 1.5 2000/09/08 21:48:00 suaide Exp $
//
//
// $Log: StPreEclMaker.h,v $
// Revision 1.5  2000/09/08 21:48:00  suaide
// See README for details
//
// Revision 1.4  2000/08/24 22:11:35  suaide
//
//
// restored some files for background compatibility
//
// Revision 1.3  2000/08/24 19:45:37  suaide
//
//
// small modifications: some cout has been removed
//
// Revision 1.2  2000/08/24 11:26:48  suaide
//
//
// by A. A. P. Suaide - 2000/08/24 07:25:00
//
// Notes:
//
// 1. Full StEvent Compatible
// 2. Read hits from StEvent object
// 3. Write clusters in StEvent format and old format to keep background
//    compatibility
// 4. Do clustering in bemc, bprs, bsmde, bsmdp
// 5. Included method StPreEclMaker::SetClusterCollection
//
// Revision 1.1  2000/05/15 21:24:01  subhasis
// initial version
//
//
// Authors: Alexandre A. P. Suaide (version 1.2)
//          Subhasis Chattopadhyay,
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
#include "emc_def.h"

class StPreEclMaker : public StMaker {
private:
  Bool_t        kStEvOk;
  void          MakeHistograms();   // Filling QA Histograms
  virtual Int_t fillStEvent();
protected:
  TH2F          *m_ncl;             //! 
  TH2F          *m_etot;            //!
  TH2F          *m_sig_e;           //!
  TH2F          *m_sig_p;           //!
  TH2F          *m_cl[MAXDET];      //!
  TH2F          *m_energy[MAXDET];  //!
  TH1F          *m_HitsInCl[MAXDET];//!
  TH1F          *m_EnergyCl[MAXDET];//!
  TH1F          *m_EtaInCl[MAXDET]; //!
  TH1F          *m_PhiInCl[MAXDET]; //!
public: 
                StPreEclMaker(const char *name="ecl", const char *title="event/data/emc/hits");
  virtual       ~StPreEclMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual void  PrintInfo();
          void  SetClusterConditions(char*,Int_t,Float_t,Float_t,Float_t);
          void  SetClusterConditions(char*,Int_t,Float_t,Float_t,Float_t,Bool_t);
          
  ClassDef(StPreEclMaker, 1)// Electromagnetic PreClusters maker
};

#endif
