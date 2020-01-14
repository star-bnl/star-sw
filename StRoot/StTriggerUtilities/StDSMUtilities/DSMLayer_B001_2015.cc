#include "DSMLayer_B001_2015.hh"

#include "DSMAlgo_BE001_2015.hh"
#include "DSMAlgo_BE003_2015.hh"
#include "DSMAlgo_BW001_2015.hh"
#include "DSMAlgo_BW003_2015.hh"

DSMLayer_B001_2015::DSMLayer_B001_2015() : DSMLayer_B001_2009()
{
  printf("DSMLayer_B001_2015 constructor\n");
  // West

  for (int dsm = 0; dsm < 15; ++dsm)
    (*this)[dsm].setName("BW", 0, dsm);

  // East

  for (int dsm = 15; dsm < 30; ++dsm)
    (*this)[dsm].setName("BE", 0, dsm-15);
}


void DSMLayer_B001_2015::run()
{
  int dsm = 0;

  while (dsm < 15) {
    DSMAlgo_BW001_2015()((*this)[dsm++]); // BW001/BW006/BW011
    DSMAlgo_BW001_2015()((*this)[dsm++]); // BW002/BW007/BW012
    DSMAlgo_BW003_2015()((*this)[dsm++]); // BW003/BW008/BW013
    DSMAlgo_BW001_2015()((*this)[dsm++]); // BW004/BW009/BW014
    DSMAlgo_BW001_2015()((*this)[dsm++]); // BW005/BW010/BW015
  }

  // East

  while (dsm < 30) {
    DSMAlgo_BE001_2015()((*this)[dsm++]); // BE001/BE006/BE011
    DSMAlgo_BE001_2015()((*this)[dsm++]); // BE002/BE007/BE012
    DSMAlgo_BE003_2015()((*this)[dsm++]); // BE003/BE008/BE013
    DSMAlgo_BE001_2015()((*this)[dsm++]); // BE004/BE009/BE014
    DSMAlgo_BE001_2015()((*this)[dsm++]); // BE005/BE010/BE015
  }
}
