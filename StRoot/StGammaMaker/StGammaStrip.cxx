#include "StGammaStrip.h"
#include <iostream>

ClassImp(StGammaStrip);

void StGammaStrip::print()
{

    std::cout << "sec = " << (int)sector << " plane = " << (int)plane
              << " index = " << (int)index << " energy = "
              << energy << std::endl;
              
}
