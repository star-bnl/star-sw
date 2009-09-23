//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#include "DSMAlgo_EE101_2009.hh"
#include "DSMAlgo_EE102_2009.hh"
#include "DSMLayer_E001_2009.hh"
#include "DSMLayer_E101_2009.hh"

DSMLayer_E101_2009::DSMLayer_E101_2009() : DSMLayer<TriggerDataBlk>(2)
{
  (*this)[0].name = "EE101";
  (*this)[1].name = "EE102";
}

bool DSMLayer_E101_2009::read(const TriggerDataBlk& event)
{
  bool bc1_in = event.MainX[BC1_CONF_NUM].offset && event.MainX[BC1_CONF_NUM].length;

  if (bc1_in) {
    BELayerBlock* bc1 = (BELayerBlock*)((char*)&event+event.MainX[BC1_CONF_NUM].offset);
    for (size_t dsm = 0; dsm < size(); ++dsm)
      copy_and_swap8((*this)[dsm].channels, &bc1->EEMClayer1[dsm*8]);
  }

  return bc1_in;
}

void DSMLayer_E101_2009::write(DSMLayer<TriggerDataBlk>& layer)
{
  for (int ch = 6; ch < 8; ++ch)
    layer[0].channels[ch] = (*this)[ch-6].output;
}

void DSMLayer_E101_2009::run()
{
  DSMAlgo_EE101_2009()((*this)[0]); // EE101
  DSMAlgo_EE102_2009()((*this)[1]); // EE102
}
