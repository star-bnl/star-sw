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
  for (size_t dsm = 0; dsm < size(); ++dsm)
    (*this)[dsm].setName("BC", 1, dsm);
}

bool DSMLayer_B101_2009::read(const TriggerDataBlk& event)
{
  bool bc1_in = event.MainX[BC1_CONF_NUM].offset && event.MainX[BC1_CONF_NUM].length;

  if (bc1_in) {
    BELayerBlock* bc1 = (BELayerBlock*)((char*)&event+event.MainX[BC1_CONF_NUM].offset);
    for (size_t dsm = 0; dsm < size(); ++dsm)
      copy_and_swap8((*this)[dsm].channels, &bc1->BEMClayer1[dsm*8]);
  }

  return bc1_in;
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
