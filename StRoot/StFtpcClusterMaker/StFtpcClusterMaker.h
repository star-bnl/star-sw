// $Id: StFtpcClusterMaker.h,v 1.16 2002/02/10 21:14:44 jcs Exp $
// $Log: StFtpcClusterMaker.h,v $
// Revision 1.16  2002/02/10 21:14:44  jcs
// create separate radial chargestep histograms for Ftpc west and east
//
// Revision 1.15  2001/11/21 12:44:44  jcs
// make ftpcGas database table available to FTPC cluster maker
//
// Revision 1.14  2001/10/29 12:53:23  jcs
// select FTPC drift maps according to flavor of magnetic field
//
// Revision 1.13  2001/10/19 09:41:22  jcs
// tZero now in data base in ftpcElectronics
//
// Revision 1.12  2001/10/12 14:33:08  jcs
// create and fill charge step histograms for FTPC East and West
//
// Revision 1.11  2001/07/12 07:45:53  jcs
// define radial position of clusters histogram
//
// Revision 1.10  2001/04/02 12:10:20  jcs
// get FTPC calibrations,geometry from MySQL database and code parameters
// from StarDb/ftpc
//
// Revision 1.9  2001/03/19 15:52:47  jcs
// use ftpcDimensions from database
//
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

class DetectorReader;
class St_ftpcClusterPars;
class St_ftpcFastSimGas;
class St_ftpcFastSimPars;
class St_ftpcDimensions;
class St_ftpcPadrowZ;
class St_ftpcEField;
class St_ftpcVDrift;
class St_ftpcDeflection;
class St_ftpcdVDriftdP;
class St_ftpcdDeflectiondP;
class St_ftpcAmpSlope;
class St_ftpcAmpOffset;
class St_ftpcTimeOffset;
class St_ftpcDriftField;
class St_ftpcGas;
class St_ftpcElectronics;

class StFtpcClusterMaker : public StMaker {
 private:
   Bool_t drawinit;
// static Char_t  m_VersionCVS = "$Id: StFtpcClusterMaker.h,v 1.16 2002/02/10 21:14:44 jcs Exp $";
   St_ftpcClusterPars   *m_clusterpars;           //!
   St_ftpcFastSimGas    *m_fastsimgas;            //!
   St_ftpcFastSimPars   *m_fastsimpars;           //!
   St_ftpcDimensions    *m_dimensions;            //!
   St_ftpcPadrowZ       *m_padrow_z;              //!
   St_ftpcEField        *m_efield;                //!
   St_ftpcVDrift        *m_vdrift;                //!
   St_ftpcDeflection    *m_deflection;            //!
   St_ftpcdVDriftdP     *m_dvdriftdp;             //!
   St_ftpcdDeflectiondP *m_ddeflectiondp;         //!
   St_ftpcAmpSlope      *m_ampslope;              //!
   St_ftpcAmpOffset     *m_ampoffset;             //!
   St_ftpcTimeOffset    *m_timeoffset;            //!
   St_ftpcDriftField    *m_driftfield;            //!
   St_ftpcGas           *m_gas;                   //!
   St_ftpcElectronics   *m_electronics;           //!
   void             MakeHistograms();// Histograms for FTPC cluster finder
 
 protected:
   TH1F            *m_cluster_radial_West; //! radial position of clusters in FTPC West
   TH1F            *m_cluster_radial_East; //! radial position of clusters in FTPC East
   TH1F            *m_chargestep_West; //! FTPC West charge step
   TH1F            *m_chargestep_East; //! FTPC East charge step
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
   virtual Int_t InitRun(int);
   virtual Int_t Init();
   virtual Int_t  Make();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFtpcClusterMaker.h,v 1.16 2002/02/10 21:14:44 jcs Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StFtpcClusterMaker, 1)   //StAF chain virtual base class for Makers
};
#endif
