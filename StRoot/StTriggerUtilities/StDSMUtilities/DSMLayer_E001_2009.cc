#include "DSMAlgo_EE001_2009.hh"
#include "DSMAlgo_EE002_2009.hh"
#include "DSMLayer_E001_2009.hh"

//DSMLayer_E001_2009::DSMLayer_E001_2009() : StDSMLayer(9) //DSMLayer<TriggerDataBlk>(9)
DSMLayer_E001_2009::DSMLayer_E001_2009() : DSMLayer<TriggerDataBlk>(9)
{
  for (size_t dsm = 0; dsm < size(); ++dsm)
    (*this)[dsm].setName("EE", 0, dsm);
}

bool DSMLayer_E001_2009::read(const TriggerDataBlk& event)
{
  bool bc1_in = event.MainX[BC1_CONF_NUM].offset && event.MainX[BC1_CONF_NUM].length;

  if (bc1_in) {
    // 9 DSMs * 16 channels
    char cbuffer[9*16];
    BELayerBlock* bc1 = (BELayerBlock*)((char*)&event+event.MainX[BC1_CONF_NUM].offset);
    for (size_t dsm = 0; dsm < size(); ++dsm) {
      copy_and_swap16(&cbuffer[dsm*16], &bc1->EEMC[dsm*16]);
      char* cpMin = cbuffer+dsm*16;
      char* cpMax = cpMin+15;
      short* sp = (*this)[dsm].channels;
      for (char* cp = cpMin; cp < cpMax; cp += 3) {
	int* ip = (int*)cp;
	*sp++ = *ip & 0xfff;
	*sp++ = *ip >> 12 & 0xfff;
      }
    }
  }

  return bc1_in;
}

void DSMLayer_E001_2009::write(DSMLayer<TriggerDataBlk>& layer)
{
  // EE101

  layer[0].channels[0] = (*this)[0].output; // EE001
  layer[0].channels[1] = (*this)[1].output       & 0xffff; // EE002 JP1 (0-15)
  layer[0].channels[2] = (*this)[1].output >> 16 & 0xffff; // EE002 JP6 (16-31)
  layer[0].channels[3] = (*this)[2].output; // EE003
  layer[0].channels[4] = (*this)[3].output; // EE004
  layer[0].channels[5] = (*this)[4].output       & 0xffff; // EE005 JP1 (0-15)

  // EE102

  layer[1].channels[0] = (*this)[4].output >> 16 & 0xffff; // EE005 JP6 (16-31)
  layer[1].channels[1] = (*this)[5].output; // EE006
  layer[1].channels[2] = (*this)[6].output; // EE007
  layer[1].channels[3] = (*this)[7].output       & 0xffff; // EE008 JP1 (0-15)
  layer[1].channels[4] = (*this)[7].output >> 16 & 0xffff; // EE008 JP6 (16-31)
  layer[1].channels[5] = (*this)[8].output; // EE009
}

void DSMLayer_E001_2009::run()
{
  DSMAlgo_EE001_2009()((*this)[0]); // EE001
  DSMAlgo_EE002_2009()((*this)[1]); // EE002
  DSMAlgo_EE001_2009()((*this)[2]); // EE003
  DSMAlgo_EE001_2009()((*this)[3]); // EE004
  DSMAlgo_EE002_2009()((*this)[4]); // EE005
  DSMAlgo_EE001_2009()((*this)[5]); // EE006
  DSMAlgo_EE001_2009()((*this)[6]); // EE007
  DSMAlgo_EE002_2009()((*this)[7]); // EE008
  DSMAlgo_EE001_2009()((*this)[8]); // EE009
}
/*
void DSMLayer_E001_2009::run(int runnumber)
{
  int yrs = 2000 + runnumber/1000000 - 1;
  printf("E001: yrs = %d\n", yrs);
  if(yrs == 2009 || yrs == 2010 || yrs == 2011 || yrs == 2012 || (yrs == 2013 && runnumber < 14081067))
    {
      printf("E001 2009 DSM Algorithm...\n");
      DSMAlgo_EE001_2009()((*this)[0]); // EE001
      DSMAlgo_EE002_2009()((*this)[1]); // EE002
      DSMAlgo_EE001_2009()((*this)[2]); // EE003
      DSMAlgo_EE001_2009()((*this)[3]); // EE004
      DSMAlgo_EE002_2009()((*this)[4]); // EE005
      DSMAlgo_EE001_2009()((*this)[5]); // EE006
      DSMAlgo_EE001_2009()((*this)[6]); // EE007
      DSMAlgo_EE002_2009()((*this)[7]); // EE008
      DSMAlgo_EE001_2009()((*this)[8]); // EE009
    }else if((yrs == 2013 && runnumber >= 14081067) || yrs > 2013)
    {
      printf("E001 2013 DSM Algorithm...\n");
      DSMAlgo_EE001_2013()((*this)[0]); // EE001
      DSMAlgo_EE002_2013()((*this)[1]); // EE002
      DSMAlgo_EE001_2013()((*this)[2]); // EE003
      DSMAlgo_EE001_2013()((*this)[3]); // EE004
      DSMAlgo_EE002_2013()((*this)[4]); // EE005
      DSMAlgo_EE001_2013()((*this)[5]); // EE006
      DSMAlgo_EE001_2013()((*this)[6]); // EE007
      DSMAlgo_EE002_2013()((*this)[7]); // EE008
      DSMAlgo_EE001_2013()((*this)[8]); // EE009
    }
}*/
