// $Id: StFtpcSlowSimMaker.h,v 1.6 2001/10/29 12:56:55 jcs Exp $
// $Log: StFtpcSlowSimMaker.h,v $
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
class St_ftpcClusterPars;
class St_ftpcSlowSimGas;
class St_ftpcSlowSimPars;
class St_ftpcDimensions;
class St_ftpcEField;
class St_ftpcVDrift;
class St_ftpcDeflection;
class St_ftpcdVDriftdP;
class St_ftpcdDeflectiondP;
class St_ftpcGas;
class St_ftpcDriftField;
class St_ftpcElectronics;

class TH1F;
class TH2F;

class StFtpcSlowSimMaker : public StMaker {
 private:
  // static Char_t m_VersionCVS = "$Id: StFtpcSlowSimMaker.h,v 1.6 2001/10/29 12:56:55 jcs Exp $";
  // Int_t         m_mode;        // mode 1 = primaries;
  St_ftpcClusterPars   *m_clusterpars;    //!
  St_ftpcSlowSimGas    *m_slowsimgas;     //!
  St_ftpcSlowSimPars   *m_slowsimpars;    //!
  St_ftpcDimensions    *m_dimensions;     //!
  St_ftpcEField        *m_efield;         //!
  St_ftpcVDrift        *m_vdrift;         //!
  St_ftpcDeflection    *m_deflection;     //!
  St_ftpcdVDriftdP     *m_dvdriftdp;      //!
  St_ftpcdDeflectiondP *m_ddeflectiondp;  //!
  St_ftpcGas           *m_gas;            //! 
  St_ftpcDriftField    *m_driftfield;     //!
  St_ftpcElectronics   *m_electronics;    //!

  void                  MakeHistograms();// Histograms for FTPC slow simulator
  
 protected:
  TH1F            *m_nadc;        //! FTPC raw data adc output
  TH1F            *m_nsqndx;      //! FTPC raw data sequence index
  TH2F            *m_nadc_index1; //! FTPC # of total adcs vs. # in FTPC East
 public: 
  StFtpcSlowSimMaker(const char *name="ftpc_raw");
  virtual       ~StFtpcSlowSimMaker();
  virtual Int_t InitRun(int);
  virtual Int_t Init();
  virtual Int_t  Make();
  // virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFtpcSlowSimMaker.h,v 1.6 2001/10/29 12:56:55 jcs Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  
  ClassDef(StFtpcSlowSimMaker, 1)   //StAF chain virtual base class for Makers
};
    
#endif
    
