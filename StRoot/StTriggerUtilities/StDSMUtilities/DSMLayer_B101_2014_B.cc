#include "DSMLayer_B101_2014_B.hh"

#include "DSMAlgo_BC101_2014_b.hh"

DSMLayer_B101_2014_B::DSMLayer_B101_2014_B() : DSMLayer_B101_2009()
{
  printf("DSMLayer_B101_2014_B constructor\n");
  for (size_t dsm = 0; dsm < size(); ++dsm)
    (*this)[dsm].setName("BC", 1, dsm);
}
void DSMLayer_B101_2014_B::run()
{
  for_each(begin(), end(), DSMAlgo_BC101_2014_b());
}
