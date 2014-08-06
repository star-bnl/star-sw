// $Id: StFtpcDriftMapMaker.h,v 1.12 2014/08/06 11:43:16 jeromel Exp $
// $Log: StFtpcDriftMapMaker.h,v $
// Revision 1.12  2014/08/06 11:43:16  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.11  2009/11/10 12:30:48  jcs
// replace StMagUtilities with StarMagField
//
// Revision 1.10  2006/08/02 13:57:57  jcs
// add deltaAr argument to allow user to change gas compostion (default: deltaAr=0)
//
// Revision 1.9  2003/09/10 19:47:16  perev
// ansi corrs
//
// Revision 1.8  2001/10/23 07:27:48  jcs
// implement new StFtpcDbReader constructor
//
// Revision 1.7  2001/07/12 18:17:18  jcs
// remove unnecessary calls to StarDb/ftpc tables
//
// Revision 1.6  2001/05/17 20:45:19  jcs
// change to use Jim Thomas StMagUtilities
//
// Revision 1.5  2001/04/02 12:06:36  jcs
// get FTPC calibrations,geometry from MySQL database
//
// Revision 1.4  2001/03/19 15:53:05  jcs
// use ftpcDimensions from database
//
// Revision 1.3  2001/03/09 13:54:26  jcs
// write out cstructs with new values so that they can be added to database
//
// Revision 1.2  2001/03/07 15:12:39  jcs
// use MySQL database instead of params
//
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

#include "StarMagField.h"

class St_db_Maker;
class St_ftpcDimensions;
class St_ftpcPadrowZ;
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

class StFtpcDriftMapMaker : public StMaker {
 private:
   char*   fTableName;      // c-structure name that is same as table in database
   char*   fOutputFileName; // file name for output
  // static Char_t m_VersionCVS = "$Id: StFtpcDriftMapMaker.h,v 1.12 2014/08/06 11:43:16 jeromel Exp $";
  // Int_t         m_mode;        // mode 1 = primaries;
   St_db_Maker *mDbMaker;                         //!
   St_ftpcDimensions    *m_dimensions;    //!
   St_ftpcPadrowZ       *m_padrow_z;      //!
   St_ftpcEField        *m_efield;        //!
   St_ftpcVDrift        *m_vdrift;        //!
   St_ftpcDeflection    *m_deflection;    //!
   St_ftpcdVDriftdP     *m_dvdriftdp;     //!
   St_ftpcdDeflectiondP *m_ddeflectiondp; //!
   St_ftpcGas           *m_gas;           //!
   St_ftpcDriftField    *m_driftfield;    //!
   St_ftpcElectronics   *m_electronics;   //!
  void                  MakeHistograms();// Histograms for FTPC drift map
  
 protected:
 public: 
  StFtpcDriftMapMaker(const StarMagField::EBField map, const Float_t factor,const Float_t deltaAr);
  virtual       ~StFtpcDriftMapMaker();

  // virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFtpcDriftMapMaker.h,v 1.12 2014/08/06 11:43:16 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  
  ClassDef(StFtpcDriftMapMaker,0)  
};
    
#endif
    
