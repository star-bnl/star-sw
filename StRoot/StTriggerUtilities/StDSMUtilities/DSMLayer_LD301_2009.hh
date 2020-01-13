#ifndef DSM_LAYER_LD301_2009_HH
#define DSM_LAYER_LD301_2009_HH

#include "StDSMLayer.hh"

struct DSMLayer_LD301_2009 : public DSMLayer<TriggerDataBlk> {
  DSMLayer_LD301_2009();
  bool read(const TriggerDataBlk& event);
  virtual void write(DSMLayer<TriggerDataBlk>&) {}
  virtual void run();
};

#endif	// DSM_LAYER_LD301_2009_HH
