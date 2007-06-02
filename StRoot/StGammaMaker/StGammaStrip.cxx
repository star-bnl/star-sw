#include "StGammaStrip.h"
#include <iostream>

ClassImp(StGammaStrip);

StGammaStrip::StGammaStrip(){ /* nada */ };

void StGammaStrip::print()
{
  std::cout << "sec="<<(int)sector<<" plane="<<(int)plane<<" index="<<(int)index<<" energy="<<energy<<std::endl;
}
