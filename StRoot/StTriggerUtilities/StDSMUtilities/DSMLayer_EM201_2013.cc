#include "DSMLayer_EM201_2013.hh"

#include "DSMAlgo_EM201_2013.hh"

DSMLayer_EM201_2013::DSMLayer_EM201_2013() : DSMLayer_EM201_2009() 
{
  printf("DSMLayer_EM201_2013 constructor\n");
  front().name = "EM201";
}

void DSMLayer_EM201_2013::run()
{
  DSMAlgo_EM201_2013()(front()); // EM201
}
