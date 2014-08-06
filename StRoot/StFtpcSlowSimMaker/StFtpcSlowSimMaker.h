// $Id: StFtpcSlowSimMaker.h,v 1.15 2014/08/06 11:43:17 jeromel Exp $
// $Log: StFtpcSlowSimMaker.h,v $
// Revision 1.15  2014/08/06 11:43:17  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.14  2005/10/26 14:07:32  jcs
// Calculate  microsecondsPerTimebin from RHIC clock frequency if available,
// otherwise use default from database
//
// Revision 1.13  2005/03/23 14:33:18  jcs
// changes to use body + extra temperature readings starting with y2005
// (necessary for embedding)
//
// Revision 1.12  2003/09/10 19:47:17  perev
// ansi corrs
//
// Revision 1.11  2003/07/03 13:25:50  fsimon
// Added database access for cathode offset information.
//
// Revision 1.10  2003/02/28 13:00:27  jcs
// for embedding, calculate temperature,pressure corrections using values from offline database
//
// Revision 1.9  2003/02/14 16:53:49  fsimon
// Add functionality that allows for different temperature corrections
// in west and east, important for embedding.
//
// Revision 1.8  2003/01/14 12:58:25  jcs
// use Geometry_ftpc/ftpcAsicMap to control corrections for error in Y2001-2002
// FTPC asic mapping
//
// Revision 1.7  2002/10/16 12:29:18  fsimon
// Include ftpcAmpSlope, ftpcAmpOffset and ftpcTimeOffset in Database access
// permits usage of gain factors and time offset in the simulator
//
// Revision 1.6  2001/10/29 12:56:55  jcs
// select FTPC drift maps according to flavor of magnetic field
//
// Revision 1.5  2001/10/19 09:42:34  jcs
// tZero now in data base in ftpcElectronics
//
// Revision 1.4  2001/04/02 12:04:36  jcs
// get FTPC calibrations,geometry from MySQL database and code parameters from StarDb/ftpc
//
// Revision 1.3  2001/03/19 15:53:10  jcs
// use ftpcDimensions from database
//
// Revision 1.2  2001/03/06 23:36:12  jcs
// use database instead of params
//
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

#ifndef __CINT__
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/RICH/RICH_Reader.hh"
#endif /*__CINT__*/

class St_db_Maker;
class St_ftpcClusterPars;
class St_ftpcFastSimGas;
class St_ftpcFastSimPars;
class St_ftpcDimensions;
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
class St_ftpcSlowSimGas;
class St_ftpcSlowSimPars;
class St_ftpcInnerCathode;
class St_ftpcTemps;

class TH1F;
class TH2F;

class StFtpcSlowSimMaker : public StMaker {
 private:
  // static Char_t m_VersionCVS = "$Id: StFtpcSlowSimMaker.h,v 1.15 2014/08/06 11:43:17 jeromel Exp $";
  // Int_t         m_mode;        // mode 1 = primaries;
   St_db_Maker *mDbMaker;                         //!
   St_ftpcClusterPars   *m_clusterpars;           //!
   //St_ftpcFastSimGas    *m_fastsimgas;            //!
   //St_ftpcFastSimPars   *m_fastsimpars;           //!
   St_ftpcSlowSimGas    *m_slowsimgas;     //!
   St_ftpcSlowSimPars   *m_slowsimpars;    //!   
   St_ftpcDimensions    *m_dimensions;            //!
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
   St_ftpcTemps         *m_temps;                 //!
 
  void                  MakeHistograms();// Histograms for FTPC slow simulator
  
 protected:
  TH1F            *m_nadc;        //! FTPC raw data adc output
  TH1F            *m_nsqndx;      //! FTPC raw data sequence index
  TH2F            *m_nadc_index1; //! FTPC # of total adcs vs. # in FTPC East

  Float_t          microsecondsPerTimebin;

 public: 
  StFtpcSlowSimMaker(const char *name="ftpc_raw");
  virtual       ~StFtpcSlowSimMaker();
  virtual Int_t InitRun(int);
  virtual Int_t Init();
  virtual Int_t  Make();
  // virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFtpcSlowSimMaker.h,v 1.15 2014/08/06 11:43:17 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  
  ClassDef(StFtpcSlowSimMaker,0)   //StAF chain virtual base class for Makers
};
    
#endif
    
