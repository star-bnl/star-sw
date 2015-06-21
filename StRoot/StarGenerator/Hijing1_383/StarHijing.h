#ifndef __StarHijing_h__
#define __StarHijing_h__

/*!
  \class StarHijing

  \brief Interface to the HIJING event generator

  StarHijing provides the user interface for steering the HIJING event
  generator, and provides the connection between HIJING and the concrete
  Monte Carlo application.

  To configure HIJING for a particular simulation, the user should refer
  to the HIJING manual: http://ntc0.lbl.gov/~xnwang/hijing/.

  The HIPARNT, HIMAIN1, HIMAIN2 and LUDAT3 common blocks have been exposed
  as C structures.

  Example:
  \example ../macros/starsim.hijing.C

  Code snippet showing how to setup a 200 GeV AuAu collision.

  \code
  StarHijing *hijing = new StarHijing("hijing");
  hijing->SetTitle("Hijing 1.383");

  hijing->SetFrame("CMS",200.0);
  hijing->SetBlue("Au");
  hijing->SetYell("Au");

  hijing->SetImpact(0.0, 30.0);

  primary -> AddGenerator(hijing);
  \endcode

  
  \author Jason C. Webb
 
 */

#include "StarGenerator/BASE/StarGenerator.h"

#include "Hijing.h"
#include <map>
using namespace std;

class StarHijing : public StarGenerator
{
 public:
  StarHijing( const Char_t *name="Hijing" );
  ~StarHijing(){ /* nada */ };

  Int_t Init();
  Int_t Generate();

  /// Returns a reference to the hijing parameters
  HiParnt_t &hiparnt(){ return *address_of_hiparnt();}

  /// Returns a reference to the hijing main1 block
  HiMain1_t &himain1(){ return *address_of_himain1(); }
  /// Returns a refernece to the hijing main2 block
  HiMain2_t &himain2(){ return *address_of_himain2(); }

  /// Returns a reference to the ludat3 (pydat3) common block
  Ludat3_t &ludat3(){ return *address_of_ludat3(); }

  /// Returns the compressed particle code (used in mdcy, mdme,... )
  /// from the jetset id of the particle
  Int_t LuComp( Int_t jetsetid );

 private:
 protected:
  ClassDef(StarHijing,1);

  void FillAA( StarGenEvent *event );

  map<Int_t, Int_t> mStatusCode;
  map<Int_t, Int_t> mParticleCode;

  // Count number of spectator protons and neutrons
  Int_t mNumberOfSpectatorProtons[2];
  Int_t mNumberOfSpectatorNeutrons[2];

  Int_t mNumberOfBeamProtons[2];
  Int_t mNumberOfBeamNeutrons[2];

  /// Given the event generator's native particle code, returns the corresponding
  /// PDG code
  /// @param code The event generator's native code
  Int_t pdgid(const Int_t &code);

};

#endif
