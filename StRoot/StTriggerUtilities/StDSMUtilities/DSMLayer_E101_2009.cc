#include "DSMAlgo_EE101_2009.hh"
#include "DSMAlgo_EE102_2009.hh"

#include "DSMLayer_E001_2009.hh"
#include "DSMLayer_E101_2009.hh"

//DSMLayer_E101_2009::DSMLayer_E101_2009() : StDSMLayer(2) //DSMLayer<TriggerDataBlk>(2)
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
/*
void DSMLayer_E101_2009::run(int runnumber)
{
  int yrs =  2000 + runnumber/1000000 - 1;
  printf("E101: yrs = %d\n", yrs);
  if(yrs == 2009 || yrs == 2011 || yrs == 2012 || (yrs == 2013 && runnumber < 14081067))
    {
      printf("E101: 2009 DSM Algorithm...\n");
      DSMAlgo_EE101_2009()((*this)[0]); // EE101
      DSMAlgo_EE102_2009()((*this)[1]); // EE102
    }else if((yrs == 2013 && runnumber >= 14081067) || yrs > 2013)
    {
      printf("E101: 2013 DSM Algorithm...\n");
      DSMAlgo_EE101_2013()((*this)[0]); // EE101
      DSMAlgo_EE102_2013()((*this)[1]); // EE102
    }
      
}
*/
