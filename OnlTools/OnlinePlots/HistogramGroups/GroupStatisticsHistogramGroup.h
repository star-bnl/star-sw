#ifndef GroupStatisticsHistogramGroup_h
#define GroupStatisticsHistogramGroup_h


#include "Rtypes.h"
#include "HistogramGroup.h"


class GroupStatisticsHistogramGroup : public HistogramGroup {
 public:
  GroupStatisticsHistogramGroup(const char* group="trigger", const char* subGroup="event and trigger groups", const char* trigger="any", const char* detector="any");
  ~GroupStatisticsHistogramGroup();
  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 
 private:
 TH1* hEventGroups;
 TH1* hDetectorGroups;
 void  drawEvpGroupLabels();
 void  drawDetectorLabels();

  ClassDef(GroupStatisticsHistogramGroup,1) ;
};


#endif
