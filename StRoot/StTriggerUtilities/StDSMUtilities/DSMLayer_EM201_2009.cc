#include "DSMAlgo_EM201_2009.hh"
#include "DSMLayer_B101_2009.hh"
#include "DSMLayer_E101_2009.hh"
#include "DSMLayer_EM201_2009.hh"

#include "DSMAlgo_EM201_2013.hh"
#include "DSMAlgo_EM201_2013_a.hh"

//DSMLayer_EM201_2009::DSMLayer_EM201_2009() : StDSMLayer(1) //DSMLayer<TriggerDataBlk>(1)
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
/*
void DSMLayer_EM201_2009::run(int runnumber)
{
  int yrs = 2000 + runnumber/1000000 - 1;
  printf("EM201: yrs = %d\n", yrs);
  if(yrs == 2009 || yrs == 2010 || yrs == 2011 || yrs == 2012 || (yrs == 2013 && runnumber < 14081067))
    {
      printf("EM201: 2009 DSM Algorithm...\n");
      DSMAlgo_EM201_2009()(front());
    }else if(yrs == 2013 && runnumber < 14084042)
    {
      printf("EM201: 2013 DSM Algorithm A...\n");
      DSMAlgo_EM201_2013_a()(front());
    }else if((yrs == 2013 && runnumber >= 14084042) || yrs > 2013)
    {
      printf("EM201: 2013 DSM Algorithm\n");
      DSMAlgo_EM201_2013()(front());
   }
}
*/
