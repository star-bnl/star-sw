#include "DSMLayer_B101_2015.hh"

#include "DSMAlgo_BC101_2015.hh"

DSMLayer_B101_2015::DSMLayer_B101_2015() : DSMLayer_B101_2009()
{
  printf("DSMLayer_B101_2015 constructor\n");
  for (size_t dsm = 0; dsm < size(); ++dsm)
    (*this)[dsm].setName("BC", 1, dsm);
}
void DSMLayer_B101_2015::run()
{
  for_each(begin(), end(), DSMAlgo_BC101_2015());
}
