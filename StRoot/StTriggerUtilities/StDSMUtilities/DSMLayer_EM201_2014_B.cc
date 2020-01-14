#include "DSMLayer_EM201_2014_B.hh"

#include "DSMAlgo_EM201_2014_b.hh"

DSMLayer_EM201_2014_B::DSMLayer_EM201_2014_B() : DSMLayer_EM201_2009() 
{
  printf("DSMLayer_EM201_2014_B constructor\n");
  front().name = "EM201";
}

void DSMLayer_EM201_2014_B::run()
{
  DSMAlgo_EM201_2014_b()(front()); // EM201
}
