/*!
 * \class StGlobalTrack 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StGlobalTrack.h,v 2.7 2012/05/07 14:42:57 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StGlobalTrack.h,v $
 * Revision 2.7  2012/05/07 14:42:57  fisyak
 * Add handilings for Track to Fast Detectors Matching
 *
 * Revision 2.6  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.5  2006/05/24 17:28:19  ullrich
 * Added track-at-DCA geometry.
 *
 * Revision 2.4  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.3  2002/02/22 22:56:48  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/03/24 03:34:48  perev
 * clone() -> clone() const
 *
 * Revision 2.1  1999/10/28 22:25:39  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:15  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StGlobalTrack_hh
#define StGlobalTrack_hh

#include "StTrack.h"
#include "StDcaGeometry.h"
class StGlobalTrack;
ostream&  operator<<(ostream& os,  const StGlobalTrack& t);

class StGlobalTrack : public StTrack {
 public:
  StGlobalTrack() : mDcaGeometry(0) {}
  StGlobalTrack(const StGlobalTrack&);
  StGlobalTrack& operator=(const StGlobalTrack&);
  ~StGlobalTrack() {SafeDelete(mDcaGeometry);}
  
  StTrackType     type() const  { return global; }
  const StVertex* vertex() const  { return 0; }
  
  const StDcaGeometry* dcaGeometry() const  {return mDcaGeometry;}
  StDcaGeometry* dcaGeometry()  {return mDcaGeometry;}
  void setDcaGeometry(StDcaGeometry* dca) {mDcaGeometry=dca;}
  
  void Print(Option_t *option="") const {cout << option << *this << endl; }
 protected:
  StDcaGeometry *mDcaGeometry;
  
  ClassDef(StGlobalTrack,2)
};

#endif
