#ifndef DSM_LAYER_B101_2013_HH
#define DSM_LAYER_B101_2013_HH

#include "DSMLayer_B101_2009.hh"

//struct DSMLayer_B101_2013 : public StDSMLayer{
struct DSMLayer_B101_2013 : public DSMLayer_B101_2009{
  DSMLayer_B101_2013();
  //bool read(const TriggerDataBlk& event);
  //void write(DSMLayer<TriggerDataBlk>& layer);
  void run();
//  void run(int runnumber);
};

#endif	// DSM_LAYER_B101_2013_HH
