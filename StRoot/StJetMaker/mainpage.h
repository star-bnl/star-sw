/*
  \mainpage

  \section Introduction Introduction

  The jet finding software consists of two separate software libraries: StJetFinder and StJetMaker.
  
  \subsection StJetFinder StJetFinder
  
  This library is essentially independent of the STAR framework.
  It consists of a set of classes that encapsulate various jet finding algorithms. These algorithms
  are implemented so that they operate on a list of four-vector measurements. Thus, the algorithms
  will work equally well (and easily) on measurements from various STAR subsystems, or the results
  of a pQCD Monte-Carlo calculation.
  
  \subsection StJetMaker StJetMaker

  This library provides the necessary interface to the STAR framework to run the jet
  finding algorithms. Roughly put, this consists of software to:
  <p>
  Wrap subsystem measurements (tracks, towers, etc) into four momenta. This happens in StEmcTpcFourPMaker.
  <p>
  Instantiate and run the jet algorithms. This is handled by StJetMaker, which outsources the
  task to StppJetAnalyzer.
  <p>
  Store the results in a persistent fashion. The results of a given algorithm are stored as
  an StJets object that is hung as a branch on the jet TTree. This tree is saved to file. These
  tasks are handled by StJetMaker
  <p>
  Read the jet TTree back into memory after jet finding, i.e. in another root session. The tree
  can be read into memory and correlated with the corresponding StMuDst TTree that actually is
  stored in a different file. This is handled by StJetReader.

  \section Using Using the code
  \subsection Getting Getting the code
  The code is now in cvs and builds and runs under DEV.  To get a local copy of the code yourself, do:
  <p>
  cvs co StRoot/StJetFinder
  <p>
  cvs co StRoot/StJetMaker
  <p>
  cons -noR +StJ

  \subsection Finder Running the jet finder
  To run the jet finder do:
  <p>
  root4star -b -q StRoot/StJetMaker/RunJetFinder.C
  <p>
  This will read a MuDst.root file and run both the cone and kt cluster jet finders, storing
  the output jet TTree in a file called Jets_out_emc.root And that's it! To change the parameters
  of the jet finders used, or change the track cuts, see lines 59-86 of RunJetReader.C.  You can
  add as many jet finders as you like, and you can use whatever track/jet cuts you like.

  \subsection Reader Running the jet reader
  To run the jet reader do:
  <p>
  root4star -b -q StRoot/StJetMaker/RunJetReader.C
  <p>
  This will read both the MuDst.root file and the jet TTree, which is stored in a separate file.
  And that's it! You will see some information about the jets printed to the screen.  To fill
  your own histograms, simply write a function or class and follow the example found in
  StJetMaker/StJetReader::exampleEventAna()

  \section References References
  A useful first reference is Mike's thesis (star.physics.yale.edu/users/miller/Thesis.ps) and references therein.
  Chapter 4 discuss the basics of jet finding and points to the most current references in the HEP community.
  Appendix A briefly documents the general code design.
  
 */
