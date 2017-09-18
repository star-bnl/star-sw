#ifndef __StarPythia8_h__
#define __StarPythia8_h__

/*!
  \class StarPythaia8

  \brief Interface to Pythia 8

  StarPythia8 provides the user interface to the Pythia 8 event generator.
  To configure pythia8 for a specific run, users are encouraged to consult
  the Pythia 8 manual: http://home.thep.lu.se/~torbjorn/pythia81html/Welcome.html.
  In particular the section on the settings scheme http://home.thep.lu.se/~torbjorn/pythia81html/SettingsScheme.html.

  The StarPythia8::Set() method passes configuration strings to the instance
  of pythia, as described under the "Operation" heading.

  Configuration of the beam parameters (energy, frame, species, etc...)
  is through methods defined on the StarGenerator base class.

  Code snippet illustrating how to setup pythia8 for W production at sqrt(s)=510 GeV

   \code
   StarPythia8 *pythia8 = new StarPythia8();    
   {
      pythia8->SetFrame("CMS", 510.0);
      pythia8->SetBlue("proton");
      pythia8->SetYell("proton");
      
      pythia8->Set("WeakSingleBoson:all=off");
      pythia8->Set("WeakSingleBoson:ffbar2W=on");
      pythia8->Set("24:onMode=0");              // switch off all W+/- decaus
      pythia8->Set("24:onIfAny 11 -11");        // switch on for decays to e+/-
      
   }
   primary -> AddGenerator( pythia8 );  
   \endcode

The general way that you configure Pythia8 is by passing a string to the generator which follows the form

System:Flag = value1 ... valueN

StarPythia8 (which is what the pythia8 variable points to in the example macro) passes these flags to Pythia8 using the Set 
method.  So in general, you'll be calling it like

     pythia8 -> Set( "System:Flag = value1 ... valueN" )

     [ FYI -- The Set method really is just a wrapper to the Pythia8::Pythia::readString( string ) method.  ]

So then you need to figure out what the flags are you want to set.  These are documented in the Pythia8 manual.  

For example, you go to the manual and follow the link that says "QCD" under "Process Selection"...
http://home.thep.lu.se/~torbjorn/pythia81html/QCDProcesses.html

You see a flag under "Hard QCD" processes, which is something which would be useful for jet production.

     flag  HardQCD:all   (default = off)
     Common switch for the group of all hard QCD processes, as listed separately in the following.

To set this parameter in the simulation, yould would call the Set method with

     pythia8 -> Set( "HardQCD:all = on" );

  \author Jason C. Webb

 */

/**
  \example ../macros/starsim.pythia8.C
  Example of how to run pythia8 events.
 */

#include "StarGenerator/BASE/StarGenerator.h"
#include "Pythia.h"

class StarGenPPEvent;
class StarGenEPEvent;

class StarPythia8 : public StarGenerator
{

 public:
  StarPythia8( const Char_t *name="Pythia8" );
  ~StarPythia8(){ /* nada */ };

  /// Initialize the event generator
  Int_t Init();
  /// Generate one event
  Int_t Generate();

  /// Pass a string to Pythia8::Pythia::readString(), for user configuration.
  void Set( const Char_t *s ){ mPythia -> readString(s); }

  /// Return end-of-run statistics
  StarGenStats Stats();

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StarPythia8.h,v 1.8 2014/08/06 11:43:57 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

 private:
 protected:

  Int_t InitCMS ( Int_t blue, Int_t yell );
  Int_t InitFIXT( Int_t blue, Int_t yell );
  Int_t Init3MOM( Int_t blue, Int_t yell );
  Int_t Init4MOM( Int_t blue, Int_t yell );
  Int_t Init5MOM( Int_t blue, Int_t yell );

  Pythia8::Pythia *mPythia;

  void FillPP( StarGenEvent *event );
  void FillEP( StarGenEvent *event );

  ClassDef(StarPythia8,1);

};

#endif
