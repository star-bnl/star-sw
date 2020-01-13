//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#include "DSMAlgo_BW001_2009.hh"
#include "DSMAlgo_BW003_2009.hh"
#include "DSMAlgo_BE001_2009.hh"
#include "DSMAlgo_BE003_2009.hh"
#include "DSMLayer_B001_2009.hh"

//DSMLayer_B001_2009::DSMLayer_B001_2009() : StDSMLayer(30) //<TriggerDataBlk>(30)
DSMLayer_B001_2009::DSMLayer_B001_2009() : DSMLayer<TriggerDataBlk>(30)
{
  // West

  for (int dsm = 0; dsm < 15; ++dsm)
    (*this)[dsm].setName("BW", 0, dsm);

  // East

  for (int dsm = 15; dsm < 30; ++dsm)
    (*this)[dsm].setName("BE", 0, dsm-15);
}

bool DSMLayer_B001_2009::read(const TriggerDataBlk& event)
{
  // BEMC West

  bool bcw_in = event.MainX[BCW_CONF_NUM].offset && event.MainX[BCW_CONF_NUM].length;

  if (bcw_in) {
    BWestBlock* bcw = (BWestBlock*)(((char *)&event)+event.MainX[BCW_CONF_NUM].offset);
    // 15 DSMs * 16 channels
    char cbuffer[15*16];
    for (int dsm = 0; dsm < 15; ++dsm) {
      copy_and_swap16(&cbuffer[dsm*16], &bcw->BEMCWest[dsm*16]);
      char* cpMin = &cbuffer[dsm*16];
      char* cpMax = cpMin+15;
      short* sp = (*this)[dsm].channels;
      for (char* cp = cpMin; cp < cpMax; cp += 3) {
	int* ip = (int*)cp;
	*sp++ = *ip & 0xfff;
	*sp++ = *ip >> 12 & 0xfff;
      }
    }
  }

  // BEMC East

  bool bce_in = event.MainX[BCE_CONF_NUM].offset && event.MainX[BCE_CONF_NUM].length;

  if (bce_in) {
    BEastBlock* bce = (BEastBlock*)(((char *)&event)+event.MainX[BCE_CONF_NUM].offset);
    // 15 DSMs * 16 channels
    char cbuffer[15*16];
    for (int dsm = 0; dsm < 15; ++dsm) {
      copy_and_swap16(&cbuffer[dsm*16], &bce->BEMCEast[dsm*16]);
      char* cpMin = &cbuffer[dsm*16];
      char* cpMax = cpMin+15;
      short* sp = (*this)[dsm+15].channels;
      for (char* cp = cpMin; cp < cpMax; cp += 3) {
	int* ip = (int*)cp;
	*sp++ = *ip & 0xfff;
	*sp++ = *ip >> 12 & 0xfff;
      }
    }
  }

  return (bcw_in || bce_in);
}

void DSMLayer_B001_2009::write(DSMLayer<TriggerDataBlk>& layer)
{
  int dsm = 0;
  int dsmWest = 0;
  int dsmEast = 15;

  // Loop over BEMC layer 1 DSM's

  while (dsm < 6) {

    // BC101/103/105

    layer[dsm].channels[0] = (*this)[dsmEast++].output; // BE001/BE006/BE011
    layer[dsm].channels[1] = (*this)[dsmWest++].output; // BW001/BW006/BW011
    layer[dsm].channels[2] = (*this)[dsmEast++].output; // BE002/BE007/BE012
    layer[dsm].channels[3] = (*this)[dsmWest++].output; // BW002/BW007/BW012
    layer[dsm].channels[4] = (*this)[dsmEast].output & 0xffff; // BE003/BE008/BE013 JP1 (0-15)
    layer[dsm].channels[5] = (*this)[dsmWest].output & 0xffff; // BW003/BW008/BW013 JP1 (0-15)

    ++dsm;

    // BC102/104/106

    layer[dsm].channels[0] = (*this)[dsmEast++].output >> 16 & 0xffff; // BE003/BE008/BE013 JP6 (16-31)
    layer[dsm].channels[1] = (*this)[dsmWest++].output >> 16 & 0xffff; // BW003/BW008/BW013 JP6 (16-31)
    layer[dsm].channels[2] = (*this)[dsmEast++].output; // BE004/BE009/BE014
    layer[dsm].channels[3] = (*this)[dsmWest++].output; // BW004/BW009/BW014
    layer[dsm].channels[4] = (*this)[dsmEast++].output; // BE005/BE010/BE015
    layer[dsm].channels[5] = (*this)[dsmWest++].output; // BW005/BW010/BW015

    ++dsm;
  }
}

void DSMLayer_B001_2009::run()
{
  // West

  int dsm = 0;

  while (dsm < 15) {
    DSMAlgo_BW001_2009()((*this)[dsm++]); // BW001/BW006/BW011
    DSMAlgo_BW001_2009()((*this)[dsm++]); // BW002/BW007/BW012
    DSMAlgo_BW003_2009()((*this)[dsm++]); // BW003/BW008/BW013
    DSMAlgo_BW001_2009()((*this)[dsm++]); // BW004/BW009/BW014
    DSMAlgo_BW001_2009()((*this)[dsm++]); // BW005/BW010/BW015
  }

  // East

  while (dsm < 30) {
    DSMAlgo_BE001_2009()((*this)[dsm++]); // BE001/BE006/BE011
    DSMAlgo_BE001_2009()((*this)[dsm++]); // BE002/BE007/BE012
    DSMAlgo_BE003_2009()((*this)[dsm++]); // BE003/BE008/BE013
    DSMAlgo_BE001_2009()((*this)[dsm++]); // BE004/BE009/BE014
    DSMAlgo_BE001_2009()((*this)[dsm++]); // BE005/BE010/BE015
  }
}
