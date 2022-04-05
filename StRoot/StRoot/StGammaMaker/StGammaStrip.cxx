#include "StGammaStrip.h"
#include <iostream>

ClassImp(StGammaStrip);

StGammaStrip::StGammaStrip()
{

    index = 0;
    sector = 0;
    plane = 0;
    energy = 0;
    adc = 0;
    stat = 0;
    fail = 0;
    position = 0;
    left = 0;
    right = 0;

}

void StGammaStrip::print()
{

    std::cout << "sec = " << (int)sector << " plane = " << (int)plane
              << " index = " << (int)index << " energy = "
              << energy << std::endl;
              
}
