#include "DSMLayer_EM201_2015.hh"

#include "DSMAlgo_EM201_2015.hh"

DSMLayer_EM201_2015::DSMLayer_EM201_2015() : DSMLayer_EM201_2009() 
{
  printf("DSMLayer_EM201_2015 constructor\n");
  front().name = "EM201";
}

void DSMLayer_EM201_2015::run()
{
  DSMAlgo_EM201_2015()(front()); // EM201
}
