//
// $Id: StPreEclMaker.h,v 1.17 2004/09/03 03:09:26 suaide Exp $
//
//
// $Log: StPreEclMaker.h,v $
// Revision 1.17  2004/09/03 03:09:26  suaide
// changes in the histograms
//
// Revision 1.16  2003/09/10 19:47:27  perev
// ansi corrs
//
// Revision 1.15  2003/05/26 13:44:19  suaide
// added setPrint() method
//
// Revision 1.14  2003/03/20 23:45:37  jeromel
// GetCVS() added
//
// Revision 1.13  2003/01/23 03:49:59  jeromel
// Include changed
//
// Revision 1.12  2001/10/15 01:40:20  pavlinov
// Added Clear method and EmcCollection not in .data
//
// Revision 1.11  2001/05/02 15:28:19  suaide
// the default option is not to clear the old emc info. use it only
// if you want to re-run cluster finder
//
// Revision 1.10  2001/05/01 18:17:16  suaide
// modified to erase old EMC points and clusters if they exist, so it
// is possible to run cluster maker again with different thresholds
//
// Revision 1.9  2001/04/20 22:23:51  pavlinov
// Clean up
//
// Revision 1.8  2001/04/17 23:51:57  pavlinov
// Clean up before MDC4
//
// Revision 1.7  2001/02/06 18:23:58  pavlinov
// Changed the algorithm of finding of EMC's collection in StPreEclMaker
//
// Revision 1.6  2001/02/01 22:23:14  suaide
// Fixed some memory leaks
//
// Revision 1.5  2000/09/08 21:48:00  suaide
//
//
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
#include "StEmcUtil/others/emcInternalDef.h"

class St_emcClusterParam;

class StPreEclMaker : public StMaker {
private:
  void          MakeHistograms(Int_t,StEmcPreClusterCollection*);   // Filling QA Histograms
  virtual Int_t fillStEvent(Int_t,StEmcPreClusterCollection*);
protected:
  TH2F          *m_ncl;             //! 
  TH2F          *m_etot;            //!
  TH2F          *m_sig_e;           //!
  TH2F          *m_sig_p;           //!
  TH1F          *m_HitsInCl[MAXDET];//!
  TH1F          *m_EnergyCl[MAXDET];//!
  
  // the following histograms will be created and filled only if the
  // mFilHisto is set to kTRUE
  TH2F          *m_cl[MAXDET];      //!
  TH2F          *m_energy[MAXDET];  //!
  TH1F          *m_EtaInCl[MAXDET]; //!
  TH1F          *m_PhiInCl[MAXDET]; //!

  Bool_t        mPrint;
  Bool_t        mFillHisto;

  St_emcClusterParam* mParam;       //!
public: 
                StPreEclMaker(const char *name="ecl", const char *title="event/data/emc/hits");
  virtual       ~StPreEclMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual void   Clear(Option_t *option="");

  virtual Int_t ClearEmc();
  virtual void  PrintInfo();
          void  SetClusterConditions(char*,Int_t,Float_t,Float_t,Float_t);
          void  SetClusterConditions(char*,Int_t,Float_t,Float_t,Float_t,Bool_t);
  St_emcClusterParam* getParam() {return mParam;} 
          void  setPrint(Bool_t a) { mPrint = a;}
  void          setFillHisto(Bool_t a) {mFillHisto = a;} ///< Turns on/off histogram filling
  
  Bool_t doClearEmc;

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StPreEclMaker.h,v 1.17 2004/09/03 03:09:26 suaide Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
        
  ClassDef(StPreEclMaker,0)// Electromagnetic PreClusters maker
};

#endif
