//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#include "DSMAlgo_BC101_2009.hh"
#include "DSMLayer_B001_2009.hh"
#include "DSMLayer_B101_2009.hh"

DSMLayer_B101_2009::DSMLayer_B101_2009() : DSMLayer<TriggerDataBlk>(6)
{
  for (int dsm = 0; dsm < 6; ++dsm)
    (*this)[dsm].setName("BC", 1, dsm);
}

void DSMLayer_B101_2009::read(const TriggerDataBlk& event)
{
  if (event.MainX[BC1_CONF_NUM].offset && event.MainX[BC1_CONF_NUM].length) {
    BELayerBlock* bc1 = (BELayerBlock*)((int)&event+event.MainX[BC1_CONF_NUM].offset);
    for (int dsm = 0; dsm < 6; ++dsm)
      copy_and_swap8((*this)[dsm].channels, &bc1->BEMClayer1[dsm*8]);
  }
}

void DSMLayer_B101_2009::write(DSMLayer<TriggerDataBlk>& layer)
{
  for (int ch = 0; ch < 6; ++ch)
    layer[0].channels[ch] = (*this)[ch].output;
}

void DSMLayer_B101_2009::run()
{
  for_each(begin(), end(), DSMAlgo_BC101_2009());
}
