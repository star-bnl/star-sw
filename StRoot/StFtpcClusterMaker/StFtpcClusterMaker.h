// $Id: StFtpcClusterMaker.h,v 1.1 1999/11/02 09:41:28 jcs Exp $
// $Log: StFtpcClusterMaker.h,v $
// Revision 1.1  1999/11/02 09:41:28  jcs
// add source files to empty StFtpcClusterMaker
//
//
#ifndef STAR_StFtpcClusterMaker
#define STAR_StFtpcClusterMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcClusterMaker virtual base class for Maker                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class TH1F;
class TH2F;

class St_fcl_ampoff;
class St_fcl_ampslope;
class St_fcl_timeoff;
class St_fcl_padtrans;
class St_fcl_det;
class St_fcl_zrow;
class St_ffs_fspar;
class St_ffs_gaspar;

class StFtpcClusterMaker : public StMaker {
 private:
   Bool_t drawinit;
// static Char_t  m_VersionCVS = "$Id: StFtpcClusterMaker.h,v 1.1 1999/11/02 09:41:28 jcs Exp $";
   St_fcl_ampoff   *m_ampoff;    //!
   St_fcl_ampslope *m_ampslope;  //!
   St_fcl_timeoff  *m_timeoff;   //!
   St_fcl_padtrans *m_padtrans;  //!
   St_fcl_det      *m_det;       //!
   St_fcl_zrow     *m_zrow;      //!
   St_ffs_fspar    *m_fspar;    //!
   St_ffs_gaspar   *m_gaspar;   //!
   void             MakeHistograms();// Histograms for FTPC cluster finder
 
 protected:
   TH1F            *m_flags;       //! quality control flags
   TH1F            *m_row;         //! rows
   TH1F            *m_sector;      //! sectors
   TH1F            *m_pads;        //! pads
   TH1F            *m_timebins;    //! timebins 
   TH2F            *m_row_sector;  //! row vs. sector 
   TH2F            *m_npad_nbin;   //! number of pads vs. number of timebins
 public: 
                  StFtpcClusterMaker(const char *name="ftpc_hits");
   virtual       ~StFtpcClusterMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFtpcClusterMaker.h,v 1.1 1999/11/02 09:41:28 jcs Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StFtpcClusterMaker, 1)   //StAF chain virtual base class for Makers
};
#endif
