#include "DSMLayer_EM201_2013_A.hh"

#include "DSMAlgo_EM201_2013_a.hh"

DSMLayer_EM201_2013_A::DSMLayer_EM201_2013_A() : DSMLayer_EM201_2009() 
{
  printf("DSMLayer_EM201_2013_A constructor\n");
  front().name = "EM201";
}

void DSMLayer_EM201_2013_A::run()
{
  DSMAlgo_EM201_2013_a()(front()); // EM201
}
