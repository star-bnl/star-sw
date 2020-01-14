#ifndef DSM_LAYER_E101_2009_HH
#define DSM_LAYER_E101_2009_HH

#include "StDSMLayer.hh"

struct DSMLayer_E101_2009 : public DSMLayer<TriggerDataBlk> {
  DSMLayer_E101_2009();
  bool read(const TriggerDataBlk& event);
  virtual void write(DSMLayer<TriggerDataBlk>& layer);
  virtual void run();
};

#endif	// DSM_LAYER_E101_2009_HH
