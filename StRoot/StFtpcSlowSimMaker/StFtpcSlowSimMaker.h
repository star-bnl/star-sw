// $Id: StFtpcSlowSimMaker.h,v 1.1 2000/11/23 10:16:43 hummler Exp $
// $Log: StFtpcSlowSimMaker.h,v $
// Revision 1.1  2000/11/23 10:16:43  hummler
// New FTPC slow simulator in pure maker form
//
//
#ifndef STAR_StFtpcSlowSimMaker
#define STAR_StFtpcSlowSimMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcSlowSimMaker class                                             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_fss_gas;
class St_fss_param;
class St_fcl_padtrans; 
class St_fcl_det;
class St_fcl_zrow;

class TH1F;
class TH2F;

class StFtpcSlowSimMaker : public StMaker {
 private:
  // static Char_t m_VersionCVS = "$Id: StFtpcSlowSimMaker.h,v 1.1 2000/11/23 10:16:43 hummler Exp $";
  // Int_t         m_mode;        // mode 1 = primaries;
  St_fss_gas      *m_fss_gas;  //!
  St_fss_param    *m_fss_param;//!
  St_fcl_padtrans *m_padtrans; //!
  St_fcl_det      *m_det;      //!
  St_fcl_zrow     *m_zrow;      //!
  void                  MakeHistograms();// Histograms for FTPC slow simulator
  
 protected:
  TH1F            *m_nadc;        //! FTPC raw data adc output
  TH1F            *m_nsqndx;      //! FTPC raw data sequence index
  TH2F            *m_nadc_index1; //! FTPC # of total adcs vs. # in FTPC East
 public: 
  StFtpcSlowSimMaker(const char *name="ftpc_raw");
  virtual       ~StFtpcSlowSimMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  // virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFtpcSlowSimMaker.h,v 1.1 2000/11/23 10:16:43 hummler Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  
  ClassDef(StFtpcSlowSimMaker, 1)   //StAF chain virtual base class for Makers
};
    
#endif
    
