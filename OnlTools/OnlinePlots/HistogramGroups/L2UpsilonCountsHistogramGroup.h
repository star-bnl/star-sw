#ifndef L2UpsilonCountsHistogramGroup_h
#define L2UpsilonCountsHistogramGroup_h


#include "Rtypes.h"
#include "HistogramGroup.h"

class L2UpsilonCountsHistogramGroup : public HistogramGroup {
 public:
  L2UpsilonCountsHistogramGroup(const char* group="L2 upsilon", const char* subGroup="counts", const char* trigger="ups", const char* detector="any");
  ~L2UpsilonCountsHistogramGroup();
  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 
 private:
 TH1* hTag;
 TH1* hTime;
 TH1* hEvent;
 TH1* hNumberOfHotTowers;
 TH1* hAbordRate;
 TH1* hAbordRateCurrent;
 int mNumberOfHotTowers;
 int mHotTowerChanges;

 ClassDef(L2UpsilonCountsHistogramGroup,1) ;
};


#endif


/*************************************************************************************
 $Id: L2UpsilonCountsHistogramGroup.h,v 1.1 2009/01/23 16:08:12 jeromel Exp $
 *************************************************************************************
 $Log: L2UpsilonCountsHistogramGroup.h,v $
 Revision 1.1  2009/01/23 16:08:12  jeromel
 Import from online/RTS/src/

 Revision 1.3  2007/04/25 17:52:06  laue
 Minor updates to plots.
 Fixed the SVT hybrid and anode mapping

 Revision 1.2  2007/04/05 16:49:25  laue
 *** empty log message ***

 Revision 1.1  2007/03/21 17:06:07  laue
 Some new histogram groups.
 Some groups just moved from the Infrastructure folder to this new folder

*************************************************************************************/
