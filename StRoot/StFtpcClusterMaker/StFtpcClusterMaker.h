// $Id: StFtpcClusterMaker.h,v 1.8 2001/03/06 23:33:56 jcs Exp $
// $Log: StFtpcClusterMaker.h,v $
// Revision 1.8  2001/03/06 23:33:56  jcs
// use database instead of params
//
// Revision 1.7  2000/11/20 11:39:16  jcs
// remove remaining traces of fspar table
//
// Revision 1.6  2000/11/14 13:08:21  hummler
// add charge step calculation, minor cleanup
//
// Revision 1.5  2000/08/03 14:39:00  hummler
// Create param reader to keep parameter tables away from cluster finder and
// fast simulator. StFtpcClusterFinder now knows nothing about tables anymore!
//
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

class St_fcl_det;
class St_ffs_gaspar;
class DetectorReader;
class St_ftpcPadrowZ;
class St_ftpcEField;
class St_ftpcVDrift;
class St_ftpcDeflection;
class St_ftpcdVDriftdP;
class St_ftpcdDeflectiondP;
class St_ftpcAmpSlope;
class St_ftpcAmpOffset;
class St_ftpcTimeOffset;

class StFtpcClusterMaker : public StMaker {
 private:
   Bool_t drawinit;
// static Char_t  m_VersionCVS = "$Id: StFtpcClusterMaker.h,v 1.8 2001/03/06 23:33:56 jcs Exp $";
   St_fcl_det      *m_det;       //!
   St_ffs_gaspar   *m_gaspar;   //!
   St_ftpcPadrowZ       *m_padrow_z;      //!
   St_ftpcEField        *m_efield;        //!
   St_ftpcVDrift        *m_vdrift;        //!
   St_ftpcDeflection    *m_deflection;    //!
   St_ftpcdVDriftdP     *m_dvdriftdp;     //!
   St_ftpcdDeflectiondP *m_ddeflectiondp; //!
   St_ftpcAmpSlope      *m_ampslope;      //!
   St_ftpcAmpOffset     *m_ampoffset;     //!
   St_ftpcTimeOffset     *m_timeoffset;   //!
   void             MakeHistograms();// Histograms for FTPC cluster finder
 
 protected:
   TH1F            *m_flags;       //! quality control flags
   TH1F            *m_row;         //! rows
   TH1F            *m_sector;      //! sectors
   TH1F            *m_pads;        //! pads
   TH1F            *m_timebins;    //! timebins 
   TH2F            *m_row_sector;  //! row vs. sector 
   TH2F            *m_npad_nbin;   //! number of pads vs. number of timebins
   TH2F            *m_csteps;      //! charge step by (6*row)+sector
 public: 
                  StFtpcClusterMaker(const char *name="ftpc_hits");
   virtual       ~StFtpcClusterMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFtpcClusterMaker.h,v 1.8 2001/03/06 23:33:56 jcs Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StFtpcClusterMaker, 1)   //StAF chain virtual base class for Makers
};
#endif
