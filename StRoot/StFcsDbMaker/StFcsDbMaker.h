/***************************************************************************
 * $Id: StFcsDbMaker.h,v 1.2 2021/05/27 14:02:25 akio Exp $
 * \author: akio ogawa
 ***************************************************************************
 *
 * Description: FCS DB access Maker
 *
 ***************************************************************************
 *
 * $Log: StFcsDbMaker.h,v $
 * Revision 1.2  2021/05/27 14:02:25  akio
 * clean up Clear and fixGain/corr
 *
 * Revision 1.1  2021/03/30 13:40:07  akio
 * FCS code after peer review and moved from $CVSROOT/offline/upgrades/akio
 *
 * Revision 1.22  2021/02/25 21:53:50  akio
 * Int_t -> int
 *
 * Revision 1.21  2021/02/24 22:56:19  akio
 * Modified for STAR code review (Dmitry)
 *
 * Revision 1.20  2021/02/23 22:18:23  akio
 * Modified for STAr code review (Jason)
 *
 * Revision 1.19  2021/02/12 20:09:50  akio
 * Adding getIdfromSCmap()
 *
 * Revision 1.18  2021/02/09 21:54:23  akio
 * Using StEnumeration
 *
 * Revision 1.17  2021/02/05 17:23:25  akio
 * Adding access to STAR offline DB tables.
 * Adding getFromName/getDetFromName from David.
 *
 * Revision 1.16  2021/01/05 18:15:01  akio
 * added setPedestal()
 *
 * Revision 1.15  2020/12/30 20:17:55  akio
 * adding SC map access
 *
 * Revision 1.14  2020/09/03 19:43:20  akio
 * Updating SC map and adding patchpanel & cable color map
 *
 * Revision 1.13  2020/05/29 18:53:40  akio
 * Adding EPD as PRES maps, STAR coordinate for 4x4 trigger patch, renming map files to be used for DAQ as Tonko specifies
 *
 * Revision 1.12  2020/05/04 15:48:22  akio
 * adding input file for DAQ
 *
 * Revision 1.11  2019/10/23 13:34:38  akio
 * Adding getZDepth, and take out Ecal front space (for SiPM/Fee) from offsets
 * so that x/z offsets are now pointing to actual ecal tower front & near beam corner.
 *
 * Revision 1.10  2019/07/10 06:13:34  akio
 * Adding reading of gains from text files
 *
 * Revision 1.9  2019/07/03 16:18:49  akio
 * correcting a comment
 *
 * Revision 1.8  2019/06/27 16:10:32  akio
 * adding getLocalXYinCell
 *
 * Revision 1.7  2019/06/26 18:03:07  akio
 * change default to mRun19=0 (futture full FCS)
 *
 * Revision 1.6  2019/06/25 16:38:59  akio
 * Fixed y offset for run19
 * Added setting run# and time dependent (for preshower yoffset only for now)
 *
 * Revision 1.5  2019/06/21 17:28:55  akio
 * dealing with 5cm offsent when leakyHcal
 *
 * Revision 1.4  2019/06/07 18:16:55  akio
 * *** empty log message ***
 *
 * Revision 1.3  2019/03/13 20:46:19  akio
 * formatting
 *
 * Revision 1.2  2019/03/13 20:29:30  akio
 * update for run19
 *
 * Revision 1.1  2018/11/14 16:50:13  akio
 * FCS codes in offline/upgrade/akio
 *
 **************************************************************************/

#ifndef STFCSDBMAKER_H
#define STFCSDBMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StFcsDb;
class StFcsDbPulse;

class StFcsDbMaker : public StMaker {
public: 
  StFcsDbMaker(const char *name="fcsDbMkr");
  virtual ~StFcsDbMaker();
  virtual int  Init();
  int  InitRun(int runNumber);
  void  Clear(Option_t *option);
  int  Make();
  void setDbAccess(int v){mDbAccess=v;}
 
private:
  StFcsDb *mFcsDb;
  int mDbAccess=1;
  StFcsDbPulse* mFcsDbPulse;

  virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag " __DATE__ " " __TIME__ ; return cvs;}
  ClassDef(StFcsDbMaker,0)   //StAF chain virtual base class for Makers        
};

#endif
  

