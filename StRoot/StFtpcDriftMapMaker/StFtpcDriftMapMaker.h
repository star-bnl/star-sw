// $Id: StFtpcDriftMapMaker.h,v 1.1 2000/12/20 08:44:02 jcs Exp $
// $Log: StFtpcDriftMapMaker.h,v $
// Revision 1.1  2000/12/20 08:44:02  jcs
// Replace pam/ftpc/fmg with maker
//
//
#ifndef STAR_StFtpcDriftMapMaker
#define STAR_StFtpcDriftMapMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcDriftMapMaker class                                            //
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

class StFtpcDriftMapMaker : public StMaker {
 private:
  // static Char_t m_VersionCVS = "$Id: StFtpcDriftMapMaker.h,v 1.1 2000/12/20 08:44:02 jcs Exp $";
  // Int_t         m_mode;        // mode 1 = primaries;
  St_fss_gas      *m_fss_gas;  //!
  St_fss_param    *m_fss_param;//!
  St_fcl_padtrans *m_padtrans; //!
  St_fcl_det      *m_det;      //!
  St_fcl_zrow     *m_zrow;      //!
  void                  MakeHistograms();// Histograms for FTPC drift map
  
 protected:
 public: 
  StFtpcDriftMapMaker(const char *name="ftpc_raw");
  virtual       ~StFtpcDriftMapMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  // virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFtpcDriftMapMaker.h,v 1.1 2000/12/20 08:44:02 jcs Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  
  ClassDef(StFtpcDriftMapMaker, 1)  
};
    
#endif
    
