#include "DSMLayer_B101_2013.hh"

#include "DSMAlgo_BC101_2013.hh"

//DSMLayer_B101_2013::DSMLayer_B101_2009() : StDSMLayer(6)//: DSMLayer<TriggerDataBlk>(6)
DSMLayer_B101_2013::DSMLayer_B101_2013() : DSMLayer_B101_2009()//: DSMLayer<TriggerDataBlk>(6)
{
  printf("DSMLayer_B101_2013 constructor\n");
  for (size_t dsm = 0; dsm < size(); ++dsm)
    (*this)[dsm].setName("BC", 1, dsm);
}
void DSMLayer_B101_2013::run()
{
  for_each(begin(), end(), DSMAlgo_BC101_2013());
}
