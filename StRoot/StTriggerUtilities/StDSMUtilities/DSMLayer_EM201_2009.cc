//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#include "DSMAlgo_EM201_2009.hh"
#include "DSMLayer_B101_2009.hh"
#include "DSMLayer_E101_2009.hh"
#include "DSMLayer_EM201_2009.hh"

DSMLayer_EM201_2009::DSMLayer_EM201_2009() : DSMLayer<TriggerDataBlk>(1)
{
  front().name = "EM201";
}

bool DSMLayer_EM201_2009::read(const TriggerDataBlk& event)
{
  L1_DSM_Data* L1data = (L1_DSM_Data*)((char*)&event+event.L1_DSM_ofl.offset);
  copy_and_swap8(front().channels, L1data->EMC);
  return true;
}

void DSMLayer_EM201_2009::write(DSMLayer<TriggerDataBlk>& layer)
{
  layer[0].channels[3] = front().output;
}

void DSMLayer_EM201_2009::run()
{
  DSMAlgo_EM201_2009()(front());
}
