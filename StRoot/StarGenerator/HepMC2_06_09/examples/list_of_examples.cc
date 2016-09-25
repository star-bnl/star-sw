	/// example to generate events and write output
	/// example to generate events and perform simple event selection
	/// example to read the file written by pythia_out
	/// example to generate events, write them, and read them back
	/// \example fio/example_MyPythia.cc
	 
	/// Example of generating events with Pythia using HepMC/PythiaWrapper.h 
	/// Events are read into the HepMC event record from the FORTRAN HEPEVT 
	/// common block using the IO_HEPEVT strategy -- nothing is done with them.
	/// This program is just used to find the total time required to transfer
	/// from HEPEVT into the HepMC event record.
	/// \example example_MyPythiaOnlyToHepMC.cc
	 
	/// \example fio/example_MyHerwig.cc
	 
	/// Example of applying an event selection to the events written to file
	/// using example_MyPythia.cxx
	/// Events containing a photon of pT > 25 GeV pass the selection and are
	/// written to "example_EventSelection.dat"
	/// \example example_EventSelection.cc
	 
	/// Example of building an event and a particle data table from scratch
	/// This is meant to be of use for persons implementing HepMC inside a MC 
	/// event generator
	/// \example example_BuildEventFromScratch.cc
	 
	/// Example of how to convert from another vector class to a SimpleVector.
	/// This example uses CLHEP::HepLorentzVector
	/// \example example_VectorConversion.cc

	/// This example shows low to use the particle and vertex iterators
	/// \example example_UsingIterators.cc
	 
	/// This example generates Pythia events and fills
	/// cross section information from pyint5.
	/// The example uses streaming I/O to write a file and then read it.
	/// \example fio/example_PythiaStreamIO.cc
	 
        /// Multiple events in memory at the same time
	/// \example fio/testHerwigCopies.cc
	 
	/// Multiple events in memory at the same time
	/// \example fio/testPythiaCopies.cc
	 
	/// This example converts from ThreeVector and FourVector to 
	/// CLHEP::Hep3Vector and CLHEP::HepLorentzVector
	/// Similar (or perhaps templated) conversion methods could be added to
	/// any vector class.
	/// \example VectorConversion.h
	 

// This file is used by Doxygen when generating the documentation.
