#ifndef __StarHerwig6__
#define __StarHerwig6__

#include "StarGenerator/BASE/StarGenerator.h"
#include "Herwig6.h"
#include <map>
#include <vector>
using namespace std;

class StarHerwig6 : public StarGenerator
{
 public:
  StarHerwig6( const Char_t *name="Herwig6" );
  ~StarHerwig6(){ /* nothing */ };

  Int_t Init();
  Int_t Generate();
  void Clear( const Option_t *opts );

  /// Refernces for Common Blocks:
  /// Ex: Name_t %funcname(){ return *address_of_block(); }
  /// HEPEUP Common Block
  HEPEUP_t &hepeup(){return * address_of_hepeup(); }
  /// HWGUP
  HWGUP_t &hwgup(){return * address_of_hwgup(); }
  /// HEPEVT Standard event common block
  HEPEVT_t &hepevt(){return * address_of_hepevt(); }
  /// HWBEAM, HWBMCH, HWPROC: Beams, process, and number of events
  HWBEAM_t &hwbeam(){return * address_of_hwbeam(); }
  HWPROC_t &hwproc(){return * address_of_hwproc(); }
  /// Basic parameters (and quantities derived from them)
  HWPRAM_t &hwpram(){return * address_of_hwpram(); }
  /// Other HERWIG branching, event and hard subprocess common blocks
  HWBRCH_t &hwbrch(){return * address_of_hwbrch(); }
  HWEVNT_t &hwevnt(){return * address_of_hwevnt(); }
  HWHARD_t &hwhard(){return * address_of_hwhard(); }
  /// Custom common block to make certain variables accessible
  CUSTOM_t &custom(){return * address_of_custom(); }

  /// SetProcess sets the Herwig process.
  void SetProcess( Int_t proccess );
  /// SetBeams reaches into a common block and sets the beam species.
  void SetBeams( std::string beam1, std::string beam2 ){ HWSetBeams(beam1,beam2);}

 private:
 protected:

  /// HWInit runs all of the fortan functions necessary to initialize Herwig and get it ready to generate events. In the middle of the initilization procedure is where particles which are not to be decayed are specifed. The particles which are to be made stable are passed to it as a vector of strings.
  void HWInit( std::vector<string> particles ){ InitializeEvent( particles ); }
  /// HWGenerate runs all of the fortran functions necessary to generate a Herwig event
  void HWGenerate(){ GenerateEvent(); };
  /// HWFinish cleans up after Herwig
  void HWFinish(){ HWEFIN(); }
  
  ClassDef(StarHerwig6,1);

  void FillPP( StarGenEvent *event );
  void FillEP( StarGenEvent *event );

  map<Int_t,Int_t> mStatusCode;

};

#endif
