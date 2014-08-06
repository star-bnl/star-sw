// $Id: StMuEEDemoMaker.h,v 1.3 2014/08/06 11:43:03 jeromel Exp $

#ifndef STAR_StMuEEDemoMaker
#define STAR_StMuEEDemoMaker

/*!
 *                                                                     
 * \class  StMuEEDemoMaker
 * \author fisyak
 * \date   1998/07/20
 * \brief  virtual base class for Maker
 *
 * This commented block at the top of the header file is considered as
 * the class description to be present on the this class Web page. 
 *
 * 
 * StMuEEDemoMaker virtual base class for Maker                        
 * Template Maker doing nothing. See README file in StRoot/StMuEEDemoMaker
 *
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StMuDstMaker;
class EEmcGeomSimple;
class StEEmcSmdGeom;
class StEEmcDb;

class StMuEEDemoMaker : public StMaker {
 private:
  StMuDstMaker* mMuDstMaker;
  EEmcGeomSimple *geomTw;
  StEEmcSmdGeom  *geomSmd;
  StEEmcDb *eeDb;

 public: 
  StMuEEDemoMaker(const char *self="MuEEDemo", const char* muDstMakerName="muDstMaker");
  virtual       ~StMuEEDemoMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  
  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StMuEEDemoMaker.h,v 1.3 2014/08/06 11:43:03 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StMuEEDemoMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StMuEEDemoMaker.h,v $
// Revision 1.3  2014/08/06 11:43:03  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.2  2009/02/04 20:33:26  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.1  2003/08/27 22:56:30  balewski
// example of access to EEMC data+DB+geom from muDst
//
// Revision 1.14  2002/11/26 23:49:40  jeromel
// Small modif after Art's note ... doxygen issue + cleanup
//
// Revision 1.13  2002/04/28 01:28:36  jeromel
// Reshaped comments for doxygen. Hopefully, users will propagate this good
// habit.
//
// Revision 1.12  1999/09/24 22:03:09  perev
// Add InitRun & FinishRun to template maker
//
// Revision 1.11  1999/07/15 13:57:44  perev
// cleanup
//
// Revision 1.10  1999/07/10 22:59:17  fine
// Some comments have been introduced to show html docs
//
// Revision 1.9  1999/03/11 03:33:16  perev
// new schema
//
// Revision 1.8  1999/03/10 15:02:07  fine
// HTML link to STAR problem report form has been introduced
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:31  perev
// cleanup
//
// Revision 1.5  1998/08/26 12:15:13  fisyak
// Remove asu & dsl libraries
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
