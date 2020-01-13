#include "DSMAlgo_LD301_2009.hh"
#include "DSMLayer_LD301_2009.hh"

//DSMLayer_LD301_2009::DSMLayer_LD301_2009() : StDSMLayer(1)//<TriggerDataBlk>(1)
DSMLayer_LD301_2009::DSMLayer_LD301_2009() : DSMLayer<TriggerDataBlk>(1)
{
  front().name = "LD301";
}

bool DSMLayer_LD301_2009::read(const TriggerDataBlk& event)
{
  L1_DSM_Data* L1data = (L1_DSM_Data*)((char*)&event+event.L1_DSM_ofl.offset);
  copy_and_swap8(front().channels, L1data->lastDSM);
  return true;
}

void DSMLayer_LD301_2009::run()
{
  DSMAlgo_LD301_2009()(front());
}
