#ifndef DSM_LAYER_B001_2014_B_HH
#define DSM_LAYER_B001_2014_B_HH

#include "DSMLayer_B001_2009.hh"

struct DSMLayer_B001_2014_B : public DSMLayer_B001_2009{
  DSMLayer_B001_2014_B();
  void run();
  void write(DSMLayer<TriggerDataBlk>& layer);
};

#endif	// DSM_LAYER_B101_2013_HH
