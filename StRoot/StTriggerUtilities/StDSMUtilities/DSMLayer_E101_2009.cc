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
  front().name = "EE101";
  back ().name = "EE102";
}

void DSMLayer_E101_2009::read(const TriggerDataBlk& event)
{
  if (event.MainX[BC1_CONF_NUM].offset && event.MainX[BC1_CONF_NUM].length) {
    BELayerBlock* bc1 = (BELayerBlock*)((int)&event+event.MainX[BC1_CONF_NUM].offset);
    for (int dsm = 0; dsm < 2; ++dsm)
      copy_and_swap8((*this)[dsm].channels, &bc1->EEMClayer1[dsm*8]);
  }
}

void DSMLayer_E101_2009::write(DSMLayer<TriggerDataBlk>& layer)
{
  for (int ch = 6; ch < 8; ++ch)
    layer[0].channels[ch] = (*this)[ch-6].output;
}

void DSMLayer_E101_2009::run()
{
  DSMAlgo_EE101_2009()(front()); // EE101
  DSMAlgo_EE102_2009()(back ()); // EE102
}
