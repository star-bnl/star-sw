// $Id: StFtpcClusterMaker.h,v 1.32 2014/08/06 11:43:16 jeromel Exp $
// $Log: StFtpcClusterMaker.h,v $
// Revision 1.32  2014/08/06 11:43:16  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.31  2009/08/04 08:37:28  jcs
// When the flaser option is included in the bfc, the 'perfect' gain table and
// adjustAverageWest = adjustAverageEast = 0.0, will be used for cluster finding
//
// Revision 1.30  2008/01/07 14:46:10  jcs
// create and fill the special set of Ftpc point histograms used to evaluate
// the Ftpc gain scan runs when bfc option fgain is in the chain
//
// Revision 1.29  2005/10/14 07:29:01  jcs
// Calculate microsecondsPerTimebin from RHIC ClockFrequency
// If RHIC ClockFrequency = 0, use default value from database
//
// Revision 1.28  2005/03/23 14:32:28  jcs
// additional changes for using body + extra temperatures starting with y2005
//
// Revision 1.27  2004/09/27 12:54:27  jcs
// pad vs. time histograms moved to St_QA_Maker
// set radial step histogram line color; red = FTPC East, blue=FTPC West
//
// Revision 1.26  2004/09/03 20:35:04  perev
// Big LeakOff + mem optimisation
//
// Revision 1.25  2004/02/12 19:38:46  oldi
// Removal of intermediate tables.
//
// Revision 1.24  2003/09/10 19:47:16  perev
// ansi corrs
//
// Revision 1.23  2003/08/21 14:27:23  jcs
// remove temporary fix to prevent segmentation violation which occurred when  more than one run per job
//
// Revision 1.22  2003/07/15 09:35:49  jcs
// do not re-flavor FTPC drift maps if already flavored to avoid creating
// memory leak.
// meomory leak will occur if flavor (i.e. magnetic field) changes within one
// *.fz file (only possible for MC data)
//
// Revision 1.21  2003/06/12 10:01:25  jcs
// renamed ftpcClusterGeometry database table to ftpcClusterGeom
// (name was too long)
//
// Revision 1.20  2003/06/11 12:06:03  jcs
// get inner cathode and cluster geometry parameters from database
//
// Revision 1.19  2003/02/27 22:56:58  jcs
// use the ftpc body temperature readings to make temperature/pressure corrections
//
// Revision 1.18  2003/01/14 12:58:01  jcs
// use Geometry_ftpc/ftpcAsicMap to control corrections for error in Y2001-2002
// FTPC asic mapping
//
// Revision 1.17  2002/03/01 14:22:20  jcs
// add additional histograms to monitor cluster finding
//
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

class St_db_Maker;
class DetectorReader;
class St_ftpcClusterPars;
class St_ftpcFastSimGas;
class St_ftpcFastSimPars;
class St_ftpcDimensions;
class St_ftpcPadrowZ;
class St_ftpcAsicMap;
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
class St_ftpcInnerCathode;
class St_ftpcClusterGeom;
class St_ftpcTemps;
class StEvent;
class StFtpcHitCollection;
class TObjArray;

class StFtpcClusterMaker : public StMaker {
 private:
   Bool_t drawinit;
   Bool_t laserRun;
// static Char_t  m_VersionCVS = "$Id: StFtpcClusterMaker.h,v 1.32 2014/08/06 11:43:16 jeromel Exp $";
   St_db_Maker *mDbMaker;                         //!
   St_ftpcClusterPars   *m_clusterpars;           //!
   St_ftpcFastSimGas    *m_fastsimgas;            //!
   St_ftpcFastSimPars   *m_fastsimpars;           //!
   St_ftpcDimensions    *m_dimensions;            //!
   St_ftpcPadrowZ       *m_padrow_z;              //!
   St_ftpcAsicMap       *m_asicmap;               //!
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
   St_ftpcInnerCathode  *m_cathode;               //!
   St_ftpcClusterGeom   *m_clustergeo;            //!
   St_ftpcTemps         *m_temps;                 //!
   void             MakeHistograms();// Histograms for FTPC cluster finder
 
 protected:
   StEvent*             mCurrentEvent;   //!
   StFtpcHitCollection* mFtpcHitColl;    //!
   TObjArray*           mHitArray;       //!
  

   char             m_ThBeg[1];
   TH1F            *m_cluster_radial_West; //! radial position of clusters in FTPC West
   TH1F            *m_cluster_radial_East; //! radial position of clusters in FTPC East
   TH1F            *m_chargestep_West; //! FTPC West charge step
   TH1F            *m_chargestep_East; //! FTPC East charge step
   TH1F            *m_flags;       //! quality control flags
   TH1F            *m_row;         //! rows
   TH1F            *m_sector;      //! sectors
   //TH1F            *m_pads;        //! pads
   //TH1F            *m_timebins;    //! timebins 
   TH2F     *m_pnt_xyFW;    //! xy dist. of hits, ftpcW
   TH2F     *m_pnt_xyFE;    //! xy dist. of hits, ftpcE
   TH2F     *m_pnt_padtimeFW;    //! padlength vs timelength of hits, ftpcW
   TH1F     *m_pnt_planeF;  //! plane dist. of hits, ftpc
   TH2F     *m_pnt_padtimeFE;    //! padlength vs timelength of hits, ftpcE
   TH2F            *m_row_sector;  //! row vs. sector 
   //TH2F            *m_npad_nbin;   //! number of pads vs. number of timebins
   TH2F            *m_csteps;      //! charge step by (6*row)+sector
   TH2F            *m_hitsvspad;   //! number of found hits over cluster padlength 
   TH2F            *m_hitsvstime;  //! number of found hits over cluster timelength 
   TH1F            *m_maxadc_West;
   TH1F            *m_maxadc_East;
   TH1F            *m_charge_West;
   TH1F            *m_charge_East;
   char             m_ThEnd[1];

   Float_t          microsecondsPerTimebin;
   

 public: 
                  StFtpcClusterMaker(const char *name="ftpc_hits");
   virtual       ~StFtpcClusterMaker();
   virtual Int_t InitRun(int);
   virtual Int_t Init();
   virtual Int_t Make();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFtpcClusterMaker.h,v 1.32 2014/08/06 11:43:16 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

   ClassDef(StFtpcClusterMaker,0)   //StAF chain virtual base class for Makers
};
#endif
