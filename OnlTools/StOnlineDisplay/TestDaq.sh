#!/bin/sh
stardev
echo clean up
rm -rf TestDaq TestDaq.cxx
echo Create the source code:
cat >TestDaq.cxx <<TESTDAQ
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "DAQ_READER/daqReader.h"
#include "StMemStat.h"

class TestDaq {
public:
    TestDaq(const char* fileName="st_physics_adc_10118050_raw_4320001.daq");
   ~TestDaq(){;}
};

static void ReadEvent(daqReader *rrr,int sector = 0) 
{
   if (rrr) {
      StMemStat::PrintMem("1. StDataReadModule::tpcReader  . . .");
      daq_dta *dd= rrr->det("tpx")->get("legacy",sector);
      StMemStat::PrintMem("2. StDataReadModule::tpcReader  . . .");
      if (!dd) {
         dd= rrr->det("tpc")->get("legacy",sector);
      }
      StMemStat::PrintMem("4. StDataReadModule::tpcReader  . . .");
   }
}

static void Open(const char* fileName="st_physics_adc_10118050_raw_4320001.daq") 
{
    daqReader  *reader= new daqReader((char*)fileName);
    // Get one event
    char *currentData =reader->get(0,EVP_TYPE_ANY);
    if (currentData) ReadEvent(reader);
    delete reader; 
}

TestDaq::TestDaq(const char* fileName) 
{
   Open(fileName);
   Open(fileName);
   Open(fileName);
}
namespace {
  TestDaq  a;
}
int main(int argc, const char *arv[]) {return 0;}

TESTDAQ
ls -l TestDaq.cxx
echo Compile TestDaq.cxx
g++ -m32 -fPIC -pipe -Wall -Woverloaded-virtual -ansi -Wno-long-long -g -Dsl53_gcc432 -I$STAR/StRoot/RTS/src -I$STAR/StRoot/RTS/include -I$STAR/RTS/trg/include -I$STAR/.${STAR_HOST_SYS}/include `root-config --cflags --libs`  -o TestDaq -L $STAR_LIB -lStarRoot -lTable -lRTS TestDaq.cxx
echo Execute ./TestDaq
./TestDaq


