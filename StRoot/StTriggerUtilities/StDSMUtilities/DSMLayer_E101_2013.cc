#include "DSMLayer_E101_2013.hh"

#include "DSMAlgo_EE101_2013.hh"
#include "DSMAlgo_EE102_2013.hh"

DSMLayer_E101_2013::DSMLayer_E101_2013() : DSMLayer_E101_2009() 
{
  printf("DSMLayer_E101_2013 constructor\n");
  for (size_t dsm = 0; dsm < size(); ++dsm)
    (*this)[dsm].setName("EE", 1, dsm);
}

void DSMLayer_E101_2013::run()
{
  DSMAlgo_EE101_2013()((*this)[0]); // EE101
  DSMAlgo_EE102_2013()((*this)[1]); // EE102
}
