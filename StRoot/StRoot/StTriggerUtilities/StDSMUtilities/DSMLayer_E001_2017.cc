#include "DSMAlgo_EE001_2017.hh"
#include "DSMAlgo_EE002_2017.hh"
#include "DSMLayer_E001_2017.hh"

DSMLayer_E001_2017::DSMLayer_E001_2017() : DSMLayer_E001_2009()
{
  for (size_t dsm = 0; dsm < size(); ++dsm)
    (*this)[dsm].setName("EE", 0, dsm);
}

void DSMLayer_E001_2017::run()
{
  DSMAlgo_EE001_2017()((*this)[0]); // EE001
  DSMAlgo_EE002_2017()((*this)[1]); // EE002
  DSMAlgo_EE001_2017()((*this)[2]); // EE003
  DSMAlgo_EE001_2017()((*this)[3]); // EE004
  DSMAlgo_EE002_2017()((*this)[4]); // EE005
  DSMAlgo_EE001_2017()((*this)[5]); // EE006
  DSMAlgo_EE001_2017()((*this)[6]); // EE007
  DSMAlgo_EE002_2017()((*this)[7]); // EE008
  DSMAlgo_EE001_2017()((*this)[8]); // EE009
}
