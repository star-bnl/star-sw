// $Id: StFtpcClusterMaker.h,v 1.3 2000/01/27 09:47:18 hummler Exp $
// $Log: StFtpcClusterMaker.h,v $
// Revision 1.3  2000/01/27 09:47:18  hummler
// implement raw data reader, remove type ambiguities that bothered kcc
//
// Revision 1.2  1999/12/02 13:20:59  hummler
// Move cluster processing from maker to cluster finder class.
// (Preparations for new raw data implementation.)
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

#ifndef __CINT__
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/RICH/RICH_Reader.hh"

#endif /*__CINT__*/

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
class DetectorReader;

class StFtpcClusterMaker : public StMaker {
 private:
   Bool_t drawinit;
// static Char_t  m_VersionCVS = "$Id: StFtpcClusterMaker.h,v 1.3 2000/01/27 09:47:18 hummler Exp $";
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
  {static const char cvs[]="Tag $Name:  $ $Id: StFtpcClusterMaker.h,v 1.3 2000/01/27 09:47:18 hummler Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StFtpcClusterMaker, 1)   //StAF chain virtual base class for Makers
};
#endif
