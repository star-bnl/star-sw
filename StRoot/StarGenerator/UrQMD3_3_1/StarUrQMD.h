#ifndef __StarUrQMD__
#define __StarUrQMD__

#include "StarGenerator/BASE/StarGenerator.h"
#include "UrQMD.h"
#include <map>
#include <vector>
using namespace std;

class StarUrQMD : public StarGenerator
{
 public:
  StarUrQMD( const Char_t *name="UrQMD3_3_1" );
  ~StarUrQMD(){ /* nothing */ };

  Int_t Init();
  Int_t Generate();
  void Clear( const Option_t *opts="" ){ /* nada */ };

  /// Refernces for Common Blocks:
  /// Ex: Name_t %funcname(){ return *address_of_block(); }
  ENERGIES_t &energies(){return * address_of_energies(); }
  SYS_t &sys(){return * address_of_sys(); }
  RSYS_t &rsys(){return * address_of_rsys(); }
  CUTS_t &cuts(){return * address_of_cuts(); }
  SPDATA_t &spdata(){return * address_of_spdata(); }
  ISYS_t &isys(){return * address_of_isys(); }
  COOR_t &coor(){return * address_of_coor(); }
  FRAG_t &frag(){return * address_of_frag(); }
  AIOS_t &aios(){return * address_of_aios(); }
  POTS_t &pots(){return * address_of_pots(); }
  SCOOR_t &scoor(){return * address_of_scoor(); }
  SISYS_t &sisys(){return * address_of_sisys(); }
  SSYS_t &ssys(){return * address_of_ssys(); }
  RTDELAY_t &rtdelay(){return * address_of_rtdelay(); }
  ITDELAY_t &itdelay(){return * address_of_itdelay(); }
  SVINFO_t &svinfo(){return * address_of_svinfo(); }
  FFERMI_t &ffermi(){return * address_of_ffermi(); }
  PEQ_t &peq(){return * address_of_peq(); }

  //void InitializeUrQMD(){ iurqmd(); }
  //void GenerateEvent(){ genevt(); }

 private:
 protected:

  void InitializeUrQMD();
  void GenerateEvent();


  map<TString,Int_t> InputParametersInt;
  map<TString,Double_t> InputParametersDouble;
  map<TString,TString> InputParametersString;

  ClassDef(StarUrQMD,1);

  void FillPP( StarGenEvent *event );
  void FillEP( StarGenEvent *event );
  void FillAA( StarGenEvent *event ){ /* implement */ };

  map<Int_t,Int_t> mStatusCode;

  std::vector<std::string> StableParticles;


};

#endif
