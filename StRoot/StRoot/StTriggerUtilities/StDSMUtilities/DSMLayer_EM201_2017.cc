#include "DSMLayer_EM201_2017.hh"

#include "DSMAlgo_EM201_2017.hh"

DSMLayer_EM201_2017::DSMLayer_EM201_2017() : DSMLayer_EM201_2009() 
{
  //printf("DSMLayer_EM201_2015 constructor\n");
  front().name = "EM201";
}

void DSMLayer_EM201_2017::run()
{
  DSMAlgo_EM201_2017()(front()); // EM201
}
