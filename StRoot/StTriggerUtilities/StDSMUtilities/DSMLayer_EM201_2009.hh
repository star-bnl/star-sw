#ifndef DSM_LAYER_EM201_2009_HH
#define DSM_LAYER_EM201_2009_HH

#include "StDSMLayer.hh"

struct DSMLayer_EM201_2009 : public DSMLayer<TriggerDataBlk> {
  DSMLayer_EM201_2009();
  bool read(const TriggerDataBlk& event);
  virtual void write(DSMLayer<TriggerDataBlk>& layer);
  virtual void run();
};

#endif	// DSM_LAYER_EM201_2009_HH
