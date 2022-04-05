//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#ifndef DSM_LAYER_B001_2009_HH
#define DSM_LAYER_B001_2009_HH

#include "StDSMLayer.hh"
struct DSMLayer_B001_2009 : public DSMLayer<TriggerDataBlk> {
  DSMLayer_B001_2009();
  bool read(const TriggerDataBlk& event);
  virtual void write(DSMLayer<TriggerDataBlk>& layer);
  virtual void run();
};

#endif	// DSM_LAYER_B101_2009_HH
