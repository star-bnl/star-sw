This README file contains development notes for the EvtGen-Pythia8 interface.
Pythia8 version number 8.153 or above is required.

Changed EvtPythia to use Pythia 8. The same EvtPythia ("PYTHIA") name/class is used, but
the old interface to the fortran Pythia 6 has been replaced by Pythia 8 C++ routines.
All of the Pythia 8 work is done in a new class, EvtGenExternal/EvtPythiaEngine, which 
inherits from the pure abstract base class EvtGenModels/EvtAbsExternalGen. Only the 
EvtPythiaEngine class talks to Pythia 8 rotines; EvtPythia does not need to know about 
specific Pythia 8 functions. The class EvtExternalGenFactory creates an instance of the 
Pythia engine whenever Pythia decays are used. All cloned EvtPythia models that are 
created when new PYTHIA decay modes are encountered in the decay.dec file use the same
pointer to the Pythia engine. Other external generators, such as the C++ interfaced Tauola 
and Photos, use this same abstract factory interface.

The configure script has been modified to specify the location of the new 
Pythia 8 libraries. This can be done by using the PYTHIA8DATA environment 
variable, which is usually set when building Pythia 8 libraries. Otherwise it should
be set using the --pythiadir=[directory] option.

The main class EvtGen initialises the Pythia interface by doing the following:

  // Set the Pythia external generator
  EvtExternalGenFactory* externalGenerators = EvtExternalGenFactory::getInstance();
  std::string xmlDir("./xmldoc");

  // We are using Pythia 6 physics codes in the decay.dec file(s).
  bool convertPhysCode(true);
  externalGenerators->definePythiaGenerator(xmlDir, convertPhysCode);

The xmlDir string is the location of the XML data files used by Pythia 8. If the 
PYTHIA8DATA environment variable is set, then Pythia 8 should automatically pick up 
the right data files. Otherwise, the value of xmlDir must point to a valid directory
containing the Pythia 8 XML data file ParticleData.xml.


One consequence of using Pythia 8 is that the physics decay codes have been changed.
The EvtPythiaEngine class can convert the old codes to the new versions by selecting
the conversion boolean in the constructor to true, as shown above. In future, 
integer codes specified at the end of each PYTHIA decay line in decay.dec should be 
converted to the new values, and the above boolean set to false (default option).

In addition, Pythia 6 decay modes using "q g q'bar" (physics model integer 33) 
combinations should be replaced with just "q q'bar" definitions (physics mode 32), 
since Pythia 8 will generate gluons automatically. In fact the old "q g q'bar" 
definitions will not work, owing to colour connection issues with how partons are 
generated in Pythia 8. The present DECAY.DEC file has these modifications done 
(only needed for 6 modes).

Also, there are some decays with non-standard Pythia 6 physics integers that do not
specify any daughters, e.g:

Decay anti-Xi_c0
1.000          PYTHIA       84;
Enddecay

For these cases, EvtPythiaEngine will detect that no daughters are defined and will 
just keep Pythia 8 defaults. Otherwise, errors are generated when such decays are used.

At the moment, there is a limitation in that Pythia 8 anti-particle decays are assumed 
to have the same decay modes as particle decays. When a decay.dec file is defined it
is important that any Pythia modes defined for, say B+, are also the same as for B-
(with the same branching fractions, but the daughter particles are charge conjugates).
This was the case for the previous Pythia6-EvtGen implementation.
It is possible to turn on/off (anti)particle Pythia decays to simulate "asymmetries", 
but this option is not yet implemented in EvtPythiaEngine. For Pythia modes, CP 
violation is simulated using the B mixing models in EvtGen, and not by Pythia itself. 
It is possible to have two different decay channel definitions for B+ and B-
in a decay.dec file, but the Pythia generator will use the B+ decay information only,
applying charge conjugates to get all B- decays. For example, if a user.dec file only
redefined B- to have one (Pythia) decay channel, Pythia 8 will actually use the B+
decay table to generate the B- decay. However, it is unlikely that users will define
B- and B+ to have different modes that are not charge-conjugates of each other.
If there is a use case when such behaviour is required then this will be revisited.


It is now possible to use alias particle definitions for Pythia decays, and
these aliases in turn can use Pythia decays or other decay models. Each alias should 
then decay according to any definitions specified in the decay.dec file. 
For example:

Alias Mytau- tau-
Alias Mytau+ tau+
ChargeConj Mytau+ Mytau-

Alias Mypi0 pi0

Decay Mytau-
0.5         nu_tau     gamma   pi-   pi0           PYTHIA    41; # old Pythia6 physics code
0.5         nu_tau     pi0     pi-   Mypi0         PYTHIA    41;
Enddecay
#
CDecay Mytau+
#
Decay Mypi0
1.0       gamma gamma PYTHIA 0;
Enddecay

