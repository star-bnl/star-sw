// Pythia.cc is a part of the PYTHIA event generator.
// Copyright (C) 2012 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the Pythia class.

#include "Pythia.h"

// Access time information.
#include <ctime>

// Allow string and character manipulation.
#include <cctype>

namespace Pythia8 {
 
//==========================================================================

// The Pythia class.

//--------------------------------------------------------------------------

// Constants: could be changed here if desired, but normally should not.
// These are of technical nature, as described for each.

// Maximum number of tries to produce parton level from given input.
const int Pythia::NTRY          = 10; 

// Negative integer to denote that no subrun has been set.
const int Pythia::SUBRUNDEFAULT = -999; 

//--------------------------------------------------------------------------
  
// Constructor. 

Pythia::Pythia(string xmlDir) { 
    
  // Initial values for pointers to PDF's.
  useNewPdfA      = false; 
  useNewPdfB      = false; 
  useNewPdfHard   = false; 
  useNewPdfPomA   = false; 
  useNewPdfPomB   = false; 
  pdfAPtr         = 0; 
  pdfBPtr         = 0; 
  pdfHardAPtr     = 0; 
  pdfHardBPtr     = 0; 
  pdfPomAPtr      = 0; 
  pdfPomBPtr      = 0; 

  // Initial values for pointers to Les Houches Event objects.
  doLHA           = false;
  useNewLHA       = false;
  lhaUpPtr        = 0;

  //Initial value for couplings pointer
  couplingsPtr    = &couplings;

  // Initial value for pointer to external decay handler.
  decayHandlePtr  = 0;

  // Initial value for pointer to user hooks.
  userHooksPtr    = 0;

  // Initial value for pointer to merging hooks.
  doMerging          = false;
  hasMergingHooks    = false;
  hasOwnMergingHooks = false;
  mergingHooksPtr    = 0;

  // Initial value for pointer to beam shape.
  useNewBeamShape = false;
  beamShapePtr    = 0;

  // Initial values for pointers to timelike and spacelike showers.
  useNewTimes     = false;
  useNewSpace     = false;
  timesDecPtr     = 0;
  timesPtr        = 0;
  spacePtr        = 0;

  // Find path to data files, i.e. xmldoc directory location.
  // Environment variable takes precedence, else use constructor input. 
  xmlPath     = "";
  const char* PYTHIA8DATA = "PYTHIA8DATA"; 
  char* envPath = getenv(PYTHIA8DATA);
  if (envPath != 0 && *envPath != '\0') {
    int i = 0;
    while (*(envPath+i) != '\0') xmlPath += *(envPath+(i++)); 
  } 
  else xmlPath = xmlDir;
  if (xmlPath[ xmlPath.length() - 1 ] != '/') xmlPath += "/";

  // Read in files with all flags, modes, parms and words.
  settings.initPtr( &info);
  string initFile = xmlPath + "Index.xml";
  isConstructed = settings.init( initFile);
  if (!isConstructed) { 
    info.errorMsg("Abort from Pythia::Pythia: settings unavailable");
    return;
  }

  // Read in files with all particle data.
  particleData.initPtr( &info, &settings, &rndm, couplingsPtr);
  string dataFile = xmlPath + "ParticleData.xml";
  isConstructed = particleData.init( dataFile);
  if (!isConstructed) {
    info.errorMsg("Abort from Pythia::Pythia: particle data unavailable");
    return;
  }

  // Write the Pythia banner to output. 
  banner();

  // Not initialized until at the end of the init() call.
  isInit = false;
  info.addCounter(0);
 
} 

//--------------------------------------------------------------------------
  
// Destructor.

Pythia::~Pythia() { 

  // Delete the PDF's created with new.
  if (useNewPdfHard && pdfHardAPtr != pdfAPtr) delete pdfHardAPtr; 
  if (useNewPdfHard && pdfHardBPtr != pdfBPtr) delete pdfHardBPtr; 
  if (useNewPdfA) delete pdfAPtr; 
  if (useNewPdfB) delete pdfBPtr; 
  if (useNewPdfPomA) delete pdfPomAPtr; 
  if (useNewPdfPomB) delete pdfPomBPtr; 

  // Delete the Les Houches object created with new.
  if (useNewLHA) delete lhaUpPtr;

  // Delete the MergingHooks object created with new.
  if (hasOwnMergingHooks) delete mergingHooksPtr;

  // Delete the BeamShape object created with new.
  if (useNewBeamShape) delete beamShapePtr;

  // Delete the timelike and spacelike showers created with new.
  if (useNewTimes) delete timesPtr;
  if (useNewSpace) delete spacePtr;

} 

//--------------------------------------------------------------------------

// Read in one update for a setting or particle data from a single line.

bool Pythia::readString(string line, bool warn) {

  // Check that constructor worked.
  if (!isConstructed) return false;

  // If empty line then done.
  if (line.find_first_not_of(" \n\t\v\b\r\f\a") == string::npos) return true;

  // If first character is not a letter/digit, then taken to be a comment.
  int firstChar = line.find_first_not_of(" \n\t\v\b\r\f\a");
  if (!isalnum(line[firstChar])) return true; 

  // Send on particle data to the ParticleData database.
  if (isdigit(line[firstChar])) 
    return particleData.readString(line, warn);

  // Everything else sent on to Settings.
  return settings.readString(line, warn);

}

//--------------------------------------------------------------------------

// Read in updates for settings or particle data from user-defined file.

bool Pythia::readFile(string fileName, bool warn, int subrun) {

  // Check that constructor worked.
  if (!isConstructed) return false;

  // Open file for reading.
  const char* cstring = fileName.c_str();
  ifstream is(cstring);  
  if (!is.good()) {
    info.errorMsg("Error in Pythia::readFile: did not find file", fileName);
    return false;
  }

  // Hand over real work to next method.
  return readFile( is, warn, subrun);

}

//--------------------------------------------------------------------------

// Read in updates for settings or particle data
// from user-defined stream (or file).

bool Pythia::readFile(istream& is, bool warn, int subrun) {

  // Check that constructor worked.
  if (!isConstructed) return false;

  // Read in one line at a time.
  string line;
  bool accepted = true;
  int subrunNow = SUBRUNDEFAULT;
  while ( getline(is, line) ) {

    // Check whether entered new subrun.
    int subrunLine = readSubrun( line, warn);
    if (subrunLine >= 0) subrunNow = subrunLine; 

    // Process the line if in correct subrun.
    if ( (subrunNow == subrun || subrunNow == SUBRUNDEFAULT)
       && !readString( line, warn) ) accepted = false;

  // Reached end of input file.
  };
  return accepted;

}

//--------------------------------------------------------------------------

// Routine to pass in pointers to PDF's. Usage optional.

bool Pythia::setPDFPtr( PDF* pdfAPtrIn, PDF* pdfBPtrIn, PDF* pdfHardAPtrIn, 
  PDF* pdfHardBPtrIn, PDF* pdfPomAPtrIn, PDF* pdfPomBPtrIn) {

  // Delete any PDF's created in a previous init call.
  if (useNewPdfHard && pdfHardAPtr != pdfAPtr) delete pdfHardAPtr;
  if (useNewPdfHard && pdfHardBPtr != pdfBPtr) delete pdfHardBPtr;
  if (useNewPdfA) delete pdfAPtr;
  if (useNewPdfB) delete pdfBPtr;
  if (useNewPdfPomA) delete pdfPomAPtr;
  if (useNewPdfPomB) delete pdfPomBPtr;

  // Reset pointers to be empty.
  useNewPdfA    = false;
  useNewPdfB    = false;
  useNewPdfHard = false;
  useNewPdfPomA = false;
  useNewPdfPomB = false;
  pdfAPtr       = 0;
  pdfBPtr       = 0;
  pdfHardAPtr   = 0;
  pdfHardBPtr   = 0;
  pdfPomAPtr    = 0;
  pdfPomBPtr    = 0;

  // Switch off external PDF's by zero as input.
  if (pdfAPtrIn == 0 && pdfBPtrIn == 0) return true;

  // The two PDF objects cannot be one and the same.
  if (pdfAPtrIn == pdfBPtrIn) return false;

  // Save pointers.  
  pdfAPtr       = pdfAPtrIn;
  pdfBPtr       = pdfBPtrIn;

  // By default same pointers for hard-process PDF's.
  pdfHardAPtr   = pdfAPtrIn;
  pdfHardBPtr   = pdfBPtrIn;
  
  // Optionally allow separate pointers for hard process.
  if (pdfHardAPtrIn != 0 && pdfHardBPtrIn != 0) {
    if (pdfHardAPtrIn == pdfHardBPtrIn) return false;
    pdfHardAPtr = pdfHardAPtrIn;
    pdfHardBPtr = pdfHardBPtrIn;
  }
  
  // Optionally allow pointers for Pomerons in the proton.
  if (pdfPomAPtrIn != 0 && pdfPomBPtrIn != 0) {
    if (pdfPomAPtrIn == pdfPomBPtrIn) return false;
    pdfPomAPtr  = pdfPomAPtrIn;
    pdfPomBPtr  = pdfPomBPtrIn;
  }
  
  // Done.
  return true;
}

//--------------------------------------------------------------------------

// Routine to initialize with the variable values of the Beams kind.

bool Pythia::init() {

  // Check that constructor worked.
  isInit = false;
  if (!isConstructed) { 
    info.errorMsg("Abort from Pythia::init: constructur "
      "initialization failed");
    return false;
  }

  // Begin initialization. Find which frame type to use.
  info.addCounter(1);
  frameType = mode("Beams:frameType");

  // Initialization with internal processes: read in and set values.
  if (frameType < 4 ) {
    doLHA     = false;
    boostType = frameType;
    idA       = mode("Beams:idA");
    idB       = mode("Beams:idB");
    eCM       = parm("Beams:eCM");
    eA        = parm("Beams:eA");
    eB        = parm("Beams:eB");
    pxA       = parm("Beams:pxA");
    pyA       = parm("Beams:pyA");
    pzA       = parm("Beams:pzA");
    pxB       = parm("Beams:pxB");
    pyB       = parm("Beams:pyB");
    pzB       = parm("Beams:pzB");
  
   // Initialization with a Les Houches Event File or an LHAup object.
  } else {
    doLHA     = true;
    boostType = 2;
    string lhef   = word("Beams:LHEF");
    bool skipInit = flag("Beams:newLHEFsameInit");

    // For file input: delete any old LHAup object and create new one.
    if (frameType == 4) {
      if (useNewLHA) delete lhaUpPtr;
      const char* cstring = lhef.c_str();
      lhaUpPtr   = new LHAupLHEF(cstring);
      useNewLHA  = true;

      // Check that file was properly opened.
      if (!lhaUpPtr->fileFound()) {
        info.errorMsg("Abort from Pythia::init: "
          "Les Houches Event File not found");
        return false;
      }

    // For object input: at least check that not null pointer.
    } else {
       if (lhaUpPtr == 0) {
        info.errorMsg("Abort from Pythia::init: "
          "LHAup object not found");
        return false;
      }
    } 

    // Send in pointer to info. Store or replace LHA pointer in other classes.
    lhaUpPtr->setPtr( &info);
    processLevel.setLHAPtr( lhaUpPtr);

    // If second time around, only with new file, then simplify.
    if (skipInit) {
      isInit = true;
      info.addCounter(2);
      return true;
    }

    // Set up values related to user hooks.
    if (frameType == 4) {
      doUserMerging = settings.flag("Merging:doUserMerging");
      doMGMerging   = settings.flag("Merging:doMGMerging");
      doKTMerging   = settings.flag("Merging:doKTMerging");
      doMerging     = doUserMerging || doMGMerging || doKTMerging;
      if (!doUserMerging){
        if (mergingHooksPtr) delete mergingHooksPtr;
        mergingHooksPtr = new MergingHooks();
        hasOwnMergingHooks = true;
      }
      hasMergingHooks  = (mergingHooksPtr > 0);
      if (hasMergingHooks) mergingHooksPtr->setLHEInputFile( lhef);
    }

    // Set LHAinit information (in some external program).
    if (!lhaUpPtr->setInit()) {
      info.errorMsg("Abort from Pythia::init: "
        "Les Houches initialization failed");
      return false;
    }

    // Extract beams from values set in an LHAinit object.
    idA = lhaUpPtr->idBeamA();
    idB = lhaUpPtr->idBeamB();
    eA  = lhaUpPtr->eBeamA();
    eB  = lhaUpPtr->eBeamB();
  }

  // Back to common initialization. Reset error counters. 
  nErrEvent = 0;
  info.errorReset();
  info.setTooLowPTmin(false);
  info.setSigma( 0, 0, 0, 0., 0., 0.);

  // Initialize data members extracted from database.
  doProcessLevel   = settings.flag("ProcessLevel:all");
  doPartonLevel    = settings.flag("PartonLevel:all");
  doHadronLevel    = settings.flag("HadronLevel:all");
  doDiffraction    = settings.flag("SoftQCD:all") 
                  || settings.flag("SoftQCD:singleDiffractive") 
                  || settings.flag("SoftQCD:doubleDiffractive");
  decayRHadrons    = settings.flag("RHadrons:allowDecay");
  doMomentumSpread = settings.flag("Beams:allowMomentumSpread");
  doVertexSpread   = settings.flag("Beams:allowVertexSpread");
  abortIfVeto      = settings.flag("Check:abortIfVeto");
  checkEvent       = settings.flag("Check:event");
  nErrList         = settings.mode("Check:nErrList");
  epTolErr         = settings.parm("Check:epTolErr");
  epTolWarn        = settings.parm("Check:epTolWarn");

  // Initialise merging hooks.
  if (hasMergingHooks && doMerging)
    mergingHooksPtr->init( settings, &info, &particleData );

  // Initialize the random number generator.
  if ( settings.flag("Random:setSeed") )  
    rndm.init( settings.mode("Random:seed") );

  // Check that combinations of settings are allowed; change if not.
  checkSettings();

  // Initialize couplings (needed to initialize resonances).
  // Check if SUSY couplings need to be read in
  if( !initSLHA()) info.errorMsg("Error in Pythia::init: "
    "Could not read SLHA file");
  if (couplingsPtr->isSUSY) {
    // Initialize the SM and SUSY.
    coupSUSY.init( settings, &rndm); 
    coupSUSY.initSUSY(&slha, &settings, &particleData);
    couplingsPtr = (Couplings *) &coupSUSY;
  } else {
    // Initialize the SM couplings.
    couplingsPtr->init( settings, &rndm);
  }

  // Reset couplingsPtr to the correct place.
  particleData.initPtr( &info, &settings, &rndm, couplingsPtr);

  // Set headers to distinguish the two event listing kinds. 
  int startColTag = settings.mode("Event:startColTag");
  process.init("(hard process)", &particleData, startColTag);
  event.init("(complete event)", &particleData, startColTag);

  // Final setup stage of particle data, notably resonance widths.
  particleData.initWidths( resonancePtrs);

  // Set up R-hadrons particle data, where relevant.
  rHadrons.init( &info, settings, &particleData, &rndm);

  // Set up values related to user hooks.
  hasUserHooks  = (userHooksPtr > 0);
  doVetoProcess = (hasUserHooks) 
                ? userHooksPtr->canVetoProcessLevel() : false;
  doVetoPartons = (hasUserHooks) 
                ? userHooksPtr->canVetoPartonLevel() : false;
  if (hasUserHooks) userHooksPtr->initPtr( &info, &settings, &particleData,
    &rndm, &beamA, &beamB, &beamPomA, &beamPomB, couplingsPtr, &partonSystems, 
    &sigmaTot); 

  // Set up objects for timelike and spacelike showers.
  if (timesDecPtr == 0 || timesPtr == 0) {
    TimeShower* timesNow = new TimeShower();
    if (timesDecPtr == 0) timesDecPtr = timesNow;
    if (timesPtr == 0) timesPtr = timesNow; 
    useNewTimes = true;
  }
  if (spacePtr == 0) {
    spacePtr    = new SpaceShower();
    useNewSpace = true;
  }

  // Initialize showers, especially for simple showers in decays. 
  timesPtr->initPtr( &info, &settings, &particleData, &rndm, couplingsPtr, 
    &partonSystems, userHooksPtr);
  timesDecPtr->initPtr( &info, &settings, &particleData, &rndm, couplingsPtr, 
    &partonSystems, userHooksPtr);
  spacePtr->initPtr( &info, &settings, &particleData, &rndm,
    &partonSystems, userHooksPtr);
  timesDecPtr->init( 0, 0);

  // Set up values related to beam shape.
  if (beamShapePtr == 0) {
    beamShapePtr    = new BeamShape();
    useNewBeamShape = true;
  } 
  beamShapePtr->init( settings, &rndm);

  // Check that beams and beam combination can be handled.
  if (!checkBeams()) {
    info.errorMsg("Abort from Pythia::init: "
      "checkBeams initialization failed");
    return false;
  }

  // Do not set up beam kinematics when no process level.
  if (!doProcessLevel) boostType = 1;
  else {
    
    // Set up beam kinematics.
    if (!initKinematics()) {
      info.errorMsg("Abort from Pythia::init: "
        "kinematics initialization failed");
      return false;
    }

    // Set up pointers to PDFs.
    if (!initPDFs()) {
      info.errorMsg("Abort from Pythia::init: PDF initialization failed");
      return false;
    }
  
    // Set up the two beams and the common remnant system.
    StringFlav* flavSelPtr = hadronLevel.getStringFlavPtr();
    beamA.init( idA, pzAcm, eA, mA, &info, settings, &particleData, &rndm, 
      pdfAPtr, pdfHardAPtr, isUnresolvedA, flavSelPtr);
    beamB.init( idB, pzBcm, eB, mB, &info, settings, &particleData, &rndm,
      pdfBPtr, pdfHardBPtr, isUnresolvedB, flavSelPtr);

    // Optionally set up new alternative beams for these Pomerons.
    if ( doDiffraction) { 
      beamPomA.init( 990,  0.5 * eCM, 0.5 * eCM, 0., &info, settings,
        &particleData, &rndm, pdfPomAPtr, pdfPomAPtr, false, flavSelPtr); 
      beamPomB.init( 990, -0.5 * eCM, 0.5 * eCM, 0., &info, settings,
	&particleData, &rndm, pdfPomBPtr, pdfPomBPtr, false, flavSelPtr); 
    }
  }

  // Send info/pointers to process level for initialization.
  if ( doProcessLevel && !processLevel.init( &info, settings, &particleData, 
    &rndm, &beamA, &beamB, couplingsPtr, &sigmaTot, doLHA, &slha, userHooksPtr,
    sigmaPtrs) ) {
    info.errorMsg("Abort from Pythia::init: "
      "processLevel initialization failed");
    return false;
  }

  // Send info/pointers to parton level for initialization.
  if ( doPartonLevel && doProcessLevel && !partonLevel.init( &info, settings, 
    &particleData, &rndm, &beamA, &beamB, &beamPomA, &beamPomB, couplingsPtr, 
    &partonSystems, &sigmaTot, timesDecPtr, timesPtr, spacePtr, &rHadrons, 
    userHooksPtr, mergingHooksPtr, false) ) {
    info.errorMsg("Abort from Pythia::init: "
      "partonLevel initialization failed" );
    return false;
  }

  // Send info/pointers to parton level for trial shower initialization.
  if ( doMerging && !trialPartonLevel.init( &info, settings, &particleData, 
    &rndm, &beamA, &beamB, &beamPomA, &beamPomB, couplingsPtr, &partonSystems, 
    &sigmaTot, timesDecPtr, timesPtr, spacePtr, &rHadrons, NULL,
    mergingHooksPtr, true) ) {
    info.errorMsg("Abort from Pythia::init: "
      "trialPartonLevel initialization failed");
    return false;
  }
 
  // Send info/pointers to hadron level for initialization.
  // Note: forceHadronLevel() can come, so we must always initialize. 
  if ( !hadronLevel.init( &info, settings, &particleData, &rndm,  
    couplingsPtr, timesDecPtr, &rHadrons, decayHandlePtr, 
    handledParticles) ) {
    info.errorMsg("Abort from Pythia::init: "
      "hadronLevel initialization failed");
    return false;
  }
   
  // Optionally check particle data table for inconsistencies.
  if ( settings.flag("Check:particleData") ) 
    particleData.checkTable( settings.mode("Check:levelParticleData") );

  // Optionally show settings and particle data, changed or all.
  bool showCS  = settings.flag("Init:showChangedSettings");
  bool showAS  = settings.flag("Init:showAllSettings");
  bool showCPD = settings.flag("Init:showChangedParticleData");
  bool showCRD = settings.flag("Init:showChangedResonanceData");
  bool showAPD = settings.flag("Init:showAllParticleData");
  int  show1PD = settings.mode("Init:showOneParticleData");
  bool showPro = settings.flag("Init:showProcesses");
  if (showCS)      settings.listChanged();
  if (showAS)      settings.listAll();
  if (show1PD > 0) particleData.list(show1PD);
  if (showCPD)     particleData.listChanged(showCRD);
  if (showAPD)     particleData.listAll();

  // Listing options for the next() routine.
  nCount       = settings.mode("Next:numberCount");
  nShowLHA     = settings.mode("Next:numberShowLHA");
  nShowInfo    = settings.mode("Next:numberShowInfo");
  nShowProc    = settings.mode("Next:numberShowProcess");
  nShowEvt     = settings.mode("Next:numberShowEvent");
  showSaV      = settings.flag("Next:showScaleAndVertex");
  showMaD      = settings.flag("Next:showMothersAndDaughters");

  // Succeeded.
  isInit = true; 
  info.addCounter(2);
  if (useNewLHA && showPro) lhaUpPtr->listInit(); 
  return true;

}

//--------------------------------------------------------------------------

// Routine to initialize with CM energy.

bool Pythia::init( int idAin, int idBin, double eCMin) {

  // Set arguments in Settings database.
  settings.mode("Beams:idA",  idAin);
  settings.mode("Beams:idB",  idBin);
  settings.mode("Beams:frameType",  1);
  settings.parm("Beams:eCM", eCMin);
  
  // Send on to common initialization.
  return init();

}

//--------------------------------------------------------------------------

// Routine to initialize with two collinear beams,  energies specified.

bool Pythia::init( int idAin, int idBin, double eAin, double eBin) {

  // Set arguments in Settings database.
  settings.mode("Beams:idA",  idAin);
  settings.mode("Beams:idB",  idBin);
  settings.mode("Beams:frameType",  2);
  settings.parm("Beams:eA",      eAin);
  settings.parm("Beams:eB",      eBin);
  
  // Send on to common initialization.
  return init();

}

//--------------------------------------------------------------------------

// Routine to initialize with two beams specified by three-momenta.

bool Pythia::init( int idAin, int idBin, double pxAin, double pyAin, 
  double pzAin, double pxBin, double pyBin, double pzBin) {

  // Set arguments in Settings database.
  settings.mode("Beams:idA",  idAin);
  settings.mode("Beams:idB",  idBin);
  settings.mode("Beams:frameType",  3);
  settings.parm("Beams:pxA",    pxAin);
  settings.parm("Beams:pyA",    pyAin);
  settings.parm("Beams:pzA",    pzAin);
  settings.parm("Beams:pxB",    pxBin);
  settings.parm("Beams:pyB",    pyBin);
  settings.parm("Beams:pzB",    pzBin);
  
  // Send on to common initialization.
  return init();

}

//--------------------------------------------------------------------------

// Routine to initialize when all info is given in a Les Houches Event File.

bool Pythia::init( string LesHouchesEventFile, bool skipInit) {

  // Set arguments in Settings database.
  settings.mode("Beams:frameType",              4);
  settings.word("Beams:LHEF", LesHouchesEventFile);
  settings.flag("Beams:newLHEFsameInit", skipInit);
  
  // Send on to common initialization.
  return init();

}

//--------------------------------------------------------------------------

// Routine to initialize when beam info is given in an LHAup object.

bool Pythia::init( LHAup* lhaUpPtrIn) {

  // Store LHAup object and set arguments in Settings database.
  setLHAupPtr( lhaUpPtrIn);
  settings.mode("Beams:frameType", 5);
  
  // Send on to common initialization.
  return init();

}

//--------------------------------------------------------------------------

// Check that combinations of settings are allowed; change if not.

void Pythia::checkSettings() {

  // Double rescattering not allowed if ISR or FSR.
  if ((settings.flag("PartonLevel:ISR") || settings.flag("PartonLevel:FSR"))
    && settings.flag("MultipartonInteractions:allowDoubleRescatter")) {
    info.errorMsg("Warning in Pythia::checkSettings: "
        "double rescattering switched off since showering is on");
    settings.flag("MultipartonInteractions:allowDoubleRescatter", false);
  }

}

//--------------------------------------------------------------------------

// Check that beams and beam combination can be handled. Set up unresolved.

bool Pythia::checkBeams() {

  // Absolute flavours. If not to do process level then no check needed.
  int idAabs = abs(idA);
  int idBabs = abs(idB);
  if (!doProcessLevel) return true;

  // Neutrino beams always unresolved, charged lepton ones conditionally.
  bool isLeptonA  = (idAabs > 10 && idAabs < 17);
  bool isLeptonB  = (idBabs > 10 && idBabs < 17);
  bool isUnresLep = !settings.flag("PDF:lepton");
  isUnresolvedA   = isLeptonA && (idAabs%2 == 0 || isUnresLep);
  isUnresolvedB   = isLeptonB && (idBabs%2 == 0 || isUnresLep);

  // Lepton-lepton collisions OK (including neutrinos) if both (un)resolved.
  if (isLeptonA && isLeptonB && isUnresolvedA == isUnresolvedB) return true;

  // Hadron-hadron collisions OK, with Pomeron counted as hadron.
  bool isHadronA = (idAabs == 2212) || (idA == 111) || (idAabs == 211) 
                || (idA == 990);
  bool isHadronB = (idBabs == 2212) || (idA == 111)|| (idBabs == 211) 
                || (idB == 990);
  if (isHadronA && isHadronB) return true;

  // If no case above then failed.
  info.errorMsg("Error in Pythia::init: cannot handle this beam combination");
  return false;

}

//--------------------------------------------------------------------------

// Calculate kinematics at initialization. Store beam four-momenta.

bool Pythia::initKinematics() {

  // Find masses. Initial guess that we are in CM frame.
  mA       = particleData.m0(idA);
  mB       = particleData.m0(idB);
  betaZ    = 0.;
  gammaZ   = 1.;

  // Collinear beams not in CM frame: find CM energy.
  if (boostType == 2) {
    eA     = max(eA, mA);
    eB     = max(eB, mB);
    pzA    = sqrt(eA*eA - mA*mA);
    pzB    = -sqrt(eB*eB - mB*mB);
    pAinit = Vec4( 0., 0., pzA, eA);
    pBinit = Vec4( 0., 0., pzB, eB);
    eCM    = sqrt( pow2(eA + eB) - pow2(pzA + pzB) );

    // Find boost to rest frame.
    betaZ  = (pzA + pzB) / (eA + eB);
    gammaZ = (eA + eB) / eCM;
    if (abs(betaZ) < 1e-10) boostType = 1;
  }

  // Completely general beam directions: find CM energy.
  else if (boostType == 3) {
    eA     = sqrt( pxA*pxA + pyA*pyA + pzA*pzA + mA*mA);
    eB     = sqrt( pxB*pxB + pyB*pyB + pzB*pzB + mB*mB);
    pAinit = Vec4( pxA, pyA, pzA, eA);
    pBinit = Vec4( pxB, pyB, pzB, eB);
    eCM = (pAinit + pBinit).mCalc();

    // Find boost+rotation needed to move from/to CM frame.
    MfromCM.reset();
    MfromCM.fromCMframe( pAinit, pBinit);
    MtoCM = MfromCM;
    MtoCM.invert();
  }
 
  // Fail if CM energy below beam masses.
  if (eCM < mA + mB) {
    info.errorMsg("Error in Pythia::initKinematics: too low energy");
    return false;
  }

  // Set up CM-frame kinematics with beams along +-z axis.
  pzAcm    = 0.5 * sqrtpos( (eCM + mA + mB) * (eCM - mA - mB) 
           * (eCM - mA + mB) * (eCM + mA - mB) ) / eCM;
  pzBcm    = -pzAcm;
  eA       = sqrt(mA*mA + pzAcm*pzAcm);
  eB       = sqrt(mB*mB + pzBcm*pzBcm);

  // If in CM frame then store beam four-vectors (else already done above).
  if (boostType != 2 && boostType != 3) {
    pAinit = Vec4( 0., 0., pzAcm, eA);
    pBinit = Vec4( 0., 0., pzBcm, eB);
  }

  // Store main info for access in process generation.
  info.setBeamA( idA, pzAcm, eA, mA);
  info.setBeamB( idB, pzBcm, eB, mB);
  info.setECM( eCM);

  // Must allow for generic boost+rotation when beam momentum spread.
  if (doMomentumSpread) boostType = 3;

  // Done.
  return true;

}

//--------------------------------------------------------------------------

// Set up pointers to PDFs.

bool Pythia::initPDFs() {

  // Delete any PDF's created in a previous init call.
  if (useNewPdfHard) {
    if (pdfHardAPtr != pdfAPtr) {
      delete pdfHardAPtr;
      pdfHardAPtr = 0;
    }
    if (pdfHardBPtr != pdfBPtr) {
      delete pdfHardBPtr;
      pdfHardBPtr = 0;
    }
    useNewPdfHard = false;
  }
  if (useNewPdfA) {
    delete pdfAPtr;
    useNewPdfA    = false;
    pdfAPtr       = 0;
  }
  if (useNewPdfB) {
    delete pdfBPtr;
    useNewPdfB    = false;
    pdfBPtr       = 0;
  }
  if (useNewPdfPomA) {
    delete pdfPomAPtr;
    useNewPdfPomA = false;
    pdfPomAPtr    = 0;
  }
  if (useNewPdfPomB) {
    delete pdfPomBPtr;
    useNewPdfPomB = false;
    pdfPomBPtr    = 0;
  }

  // Set up the PDF's, if not already done.
  if (pdfAPtr == 0) {
    pdfAPtr     = getPDFPtr(idA); 
    if (pdfAPtr == 0 || !pdfAPtr->isSetup()) {
      info.errorMsg("Error in Pythia::init: "
        "could not set up PDF for beam A");
      return false;
    }
    pdfHardAPtr = pdfAPtr;
    useNewPdfA  = true;
  }
  if (pdfBPtr == 0) {
    pdfBPtr     = getPDFPtr(idB); 
    if (pdfBPtr == 0 || !pdfBPtr->isSetup()) {
      info.errorMsg("Error in Pythia::init: "
        "could not set up PDF for beam B");
      return false;
    }
    pdfHardBPtr = pdfBPtr;
    useNewPdfB  = true;
  }

  // Optionally set up separate PDF's for hard process.
  if (settings.flag("PDF:useHard") && useNewPdfA && useNewPdfB) {
    pdfHardAPtr = getPDFPtr(idA, 2);      
    if (!pdfHardAPtr->isSetup()) return false;
    pdfHardBPtr = getPDFPtr(idB, 2);      
    if (!pdfHardBPtr->isSetup()) return false;
    useNewPdfHard = true;
  }

  // Optionally set up Pomeron PDF's for diffractive physics.
  if ( doDiffraction) { 
    if (pdfPomAPtr == 0) {
      pdfPomAPtr    = getPDFPtr(990);
      useNewPdfPomA = true; 
    }
    if (pdfPomBPtr == 0) {
      pdfPomBPtr    = getPDFPtr(990);
      useNewPdfPomB = true; 
    }
  }

  // Done.
  return true;

}

//--------------------------------------------------------------------------

// Main routine to generate the next event, using internal machinery.

bool Pythia::next() {

  // Regularly print how many events have been generated.
  int nPrevious = info.getCounter(3);
  if (nCount > 0 && nPrevious > 0 && nPrevious%nCount == 0)
    cout << "\n Pythia::next(): " << nPrevious 
         << " events have been generated " << endl;

  // Set/reset info counters specific to each event.
  info.addCounter(3);
  for (int i = 10; i < 13; ++i) info.setCounter(i);

  // Simpler option when only HadronLevel to be generated.
  if (!doProcessLevel) {

    // Reset info array (while event record contains data).
    info.clear();

    // Set correct energy for system.
    Vec4 pSum = 0.;
    for (int i = 1; i < event.size(); ++i) 
      if (event[i].isFinal()) pSum += event[i].p();
    event[0].p( pSum );
    event[0].m( pSum.mCalc() );

    // Generate hadronization and decays.
    bool status = forceHadronLevel();
    if (status) info.addCounter(4);
    if (status && nPrevious < nShowEvt)  event.list(showSaV, showMaD);
    return status;
  }

  // Reset arrays.
  info.clear();
  process.clear();
  event.clear();
  partonSystems.clear();
  beamA.clear();
  beamB.clear();
  beamPomA.clear();
  beamPomB.clear();

  // Can only generate event if initialization worked.
  if (!isInit) {
    info.errorMsg("Abort from Pythia::next: "
      "not properly initialized so cannot generate events"); 
    return false;
  }

  // Pick beam momentum spread and beam vertex. 
  if (doMomentumSpread || doVertexSpread) beamShapePtr->pick(); 

  // Recalculate kinematics when beam momentum spread.
  if (doMomentumSpread) nextKinematics();
 
  // Outer loop over hard processes; only relevant for user-set vetoes.
  for ( ; ; ) {
    info.addCounter(10);
    bool hasVetoed = false;

    // Provide the hard process that starts it off. Only one try.
    info.clear();
    process.clear();

    if ( !processLevel.next( process) ) {
      if (doLHA && info.atEndOfFile()) info.errorMsg("Abort from "
        "Pythia::next: reached end of Les Houches Events File"); 
      else info.errorMsg("Abort from Pythia::next: "
        "processLevel failed; giving up"); 
      return false;
    }

    info.addCounter(11);

    // Possiblility to perform CKKWL merging on this event
    if(doMerging) {
      hasVetoed = mergeProcess();
      if (hasVetoed) {
        if (abortIfVeto) return false; 
        continue;
      } 
    }

    // Possibility for a user veto of the process-level event.
    if (doVetoProcess) {
      hasVetoed = userHooksPtr->doVetoProcessLevel( process);
      if (hasVetoed) {
        if (abortIfVeto) return false; 
        continue;
      } 
    }

    // Possibility to stop the generation at this stage.
    if (!doPartonLevel) {
      boostAndVertex( true, true);
      processLevel.accumulate();
      info.addCounter(4);
      if (doLHA && nPrevious < nShowLHA) lhaUpPtr->listEvent(); 
      if (nPrevious < nShowInfo) info.list();
      if (nPrevious < nShowProc) process.list(showSaV, showMaD);
      return true;
    }

    // Save spare copy of process record in case of problems.
    Event processSave = process;
    info.addCounter(12);
    for (int i = 14; i < 19; ++i) info.setCounter(i);
  
    // Allow up to ten tries for parton- and hadron-level processing.
    bool physical   = true;
    for (int iTry = 0; iTry < NTRY; ++ iTry) {
      info.addCounter(14);
      physical = true;

      // Restore original process record if problems.
      if (iTry > 0) process = processSave;

      // Reset event record and (extracted partons from) beam remnants.
      event.clear();
      beamA.clear();
      beamB.clear();
      beamPomA.clear();
      beamPomB.clear();
      partonSystems.clear();
   
      // Parton-level evolution: ISR, FSR, MPI.
      if ( !partonLevel.next( process, event) ) {
        // Skip to next hard process for failure owing to deliberate veto.
        hasVetoed = partonLevel.hasVetoed(); 
        if (hasVetoed) {
          if (abortIfVeto) return false; 
          break;
        } 
        // Else make a new try for other failures.
        info.errorMsg("Error in Pythia::next: "
          "partonLevel failed; try again"); 
        physical = false; 
        continue;
      }
      info.addCounter(15);

      // Possibility for a user veto of the parton-level event.
      if (doVetoPartons) {
        hasVetoed = userHooksPtr->doVetoPartonLevel( event);
        if (hasVetoed) {
          if (abortIfVeto) return false; 
          break;
        }
      }

      // Boost to lab frame (before decays, for vertices).
      boostAndVertex( true, true);

      // Possibility to stop the generation at this stage.
      if (!doHadronLevel) {
        processLevel.accumulate();
        partonLevel.accumulate();

        // Optionally check final event for problems.
        if (checkEvent && !check()) {
          info.errorMsg("Abort from Pythia::next: "
            "check of event revealed problems");
          return false;
        }
        info.addCounter(4);
        if (doLHA && nPrevious < nShowLHA) lhaUpPtr->listEvent(); 
        if (nPrevious < nShowInfo) info.list();
        if (nPrevious < nShowProc) process.list(showSaV, showMaD);
        if (nPrevious < nShowEvt)  event.list(showSaV, showMaD);
        return true;
      }

      // Hadron-level: hadronization, decays.
      info.addCounter(16);
      if ( !hadronLevel.next( event) ) {
        info.errorMsg("Error in Pythia::next: "
          "hadronLevel failed; try again"); 
        physical = false; 
        continue;
      }

      // If R-hadrons have been formed, then (optionally) let them decay.
      if (decayRHadrons && rHadrons.exist() && !doRHadronDecays()) {
        info.errorMsg("Error in Pythia::next: "
          "decayRHadrons failed; try again"); 
        physical = false; 
        continue;
      }
      info.addCounter(17);
 
      // Optionally check final event for problems.
      if (checkEvent && !check()) {
        info.errorMsg("Error in Pythia::next: "
          "check of event revealed problems");
        physical = false; 
        continue;
      }

      // Stop parton- and hadron-level looping if you got this far.
      info.addCounter(18);
      break;
    }

    // If event vetoed then to make a new try.
    if (hasVetoed)  {
      if (abortIfVeto) return false; 
      continue;
    }

    // If event failed any other way (after ten tries) then give up.
    if (!physical) {
      info.errorMsg("Abort from Pythia::next: "
        "parton+hadronLevel failed; giving up");
      return false;
    }

    // Process- and parton-level statistics. Event scale.
    processLevel.accumulate();
    partonLevel.accumulate();
    event.scale( process.scale() );

    // End of outer loop over hard processes. Done with normal option.
    info.addCounter(13);
    break;
  }

  // List events.
  if (doLHA && nPrevious < nShowLHA) lhaUpPtr->listEvent(); 
  if (nPrevious < nShowInfo) info.list();
  if (nPrevious < nShowProc) process.list(showSaV,showMaD);
  if (nPrevious < nShowEvt)  event.list(showSaV, showMaD);

  // Done.
  info.addCounter(4);
  return true;

}

//--------------------------------------------------------------------------

// Generate only the hadronization/decay stage, using internal machinery.
// The "event" instance should already contain a parton-level configuration. 

bool Pythia::forceHadronLevel(bool findJunctions) {

  // Can only generate event if initialization worked.
  if (!isInit) {
    info.errorMsg("Abort from Pythia::forceHadronLevel: "
      "not properly initialized so cannot generate events"); 
    return false;
  }

  // Check whether any junctions in system. (Normally done in ProcessLevel.)
  if (findJunctions) {
    event.clearJunctions();
    processLevel.findJunctions( event);
  }

  // Save spare copy of event in case of failure.
  Event spareEvent = event;
  
  // Allow up to ten tries for hadron-level processing.
  bool physical = true;
  for (int iTry = 0; iTry < NTRY; ++ iTry) {
    physical = true;

    // Hadron-level: hadronization, decays.
    if (hadronLevel.next( event)) break;

    // If failure then warn, restore original configuration and try again.
    info.errorMsg("Error in Pythia::forceHadronLevel: "
      "hadronLevel failed; try again"); 
    physical = false; 
    event    = spareEvent;
  }
   
  // Done for simpler option.
  if (!physical)  {
    info.errorMsg("Abort from Pythia::forceHadronLevel: "
      "hadronLevel failed; giving up"); 
    return false;
  }

  // Optionally check final event for problems.
  if (checkEvent && !check()) {
    info.errorMsg("Abort from Pythia::forceHadronLevel: "
      "check of event revealed problems");
    return false;
  }

  // Done.
  return true;

}

//--------------------------------------------------------------------------

// Recalculate kinematics for each event when beam momentum has a spread.

void Pythia::nextKinematics() {

  // Read out momentum shift to give current beam momenta.
  pAnow = pAinit + beamShapePtr->deltaPA();
  pAnow.e( sqrt(pAnow.pAbs2() + mA * mA) );
  pBnow = pBinit + beamShapePtr->deltaPB();
  pBnow.e( sqrt(pBnow.pAbs2() + mB * mB) );

  // Construct CM frame kinematics.
  eCM   = (pAnow + pBnow).mCalc();
  pzAcm = 0.5 * sqrtpos( (eCM + mA + mB) * (eCM - mA - mB) 
        * (eCM - mA + mB) * (eCM + mA - mB) ) / eCM;
  pzBcm = -pzAcm;
  eA    = sqrt(mA*mA + pzAcm*pzAcm);
  eB    = sqrt(mB*mB + pzBcm*pzBcm);

  // Set relevant info for other classes to use.
  info.setBeamA( idA, pzAcm, eA, mA);
  info.setBeamB( idB, pzBcm, eB, mB);
  info.setECM( eCM);
  beamA.newPzE( pzAcm, eA);
  beamB.newPzE( pzBcm, eB);

  // Set boost/rotation matrices from/to CM frame.
  MfromCM.reset();
  MfromCM.fromCMframe( pAnow, pBnow);
  MtoCM = MfromCM;
  MtoCM.invert();

}

//--------------------------------------------------------------------------

// Boost from CM frame to lab frame, or inverse. Set production vertex.

void Pythia::boostAndVertex( bool toLab, bool setVertex) {

  // Boost process from CM frame to lab frame.
  if (toLab) {
    if      (boostType == 2) process.bst(0., 0., betaZ, gammaZ);
    else if (boostType == 3) process.rotbst(MfromCM);

    // Boost nonempty event from CM frame to lab frame.
    if (event.size() > 0) {       
      if      (boostType == 2) event.bst(0., 0., betaZ, gammaZ);
      else if (boostType == 3) event.rotbst(MfromCM);
    }

  // Boost process from lab frame to CM frame.
  } else {
    if      (boostType == 2) process.bst(0., 0., -betaZ, gammaZ);
    else if (boostType == 3) process.rotbst(MtoCM);

    // Boost nonempty event from lab frame to CM frame.
    if (event.size() > 0) {       
      if      (boostType == 2) event.bst(0., 0., -betaZ, gammaZ);
      else if (boostType == 3) event.rotbst(MtoCM);
    }
  }

  // Set production vertex; assumes particles are in lab frame and at origin.
  if (setVertex && doVertexSpread) {
    Vec4 vertex = beamShapePtr->vertex();
    for (int i = 0; i < process.size(); ++i) process[i].vProd( vertex);
    for (int i = 0; i < event.size(); ++i) event[i].vProd( vertex);
  }  

}

//--------------------------------------------------------------------------

// Perform R-hadron decays, either as part of normal evolution or forced.

bool Pythia::doRHadronDecays( ) {

  // Check if R-hadrons exist to be processed.
  if ( !rHadrons.exist() ) return true;

  // Do the R-hadron decay itself.
  if ( !rHadrons.decay( event) ) return false;

  // Perform showers in resonance decay chains.
  if ( !partonLevel.resonanceShowers( process, event, false) ) return false; 

  // Subsequent hadronization and decays.
  if ( !hadronLevel.next( event) ) return false;

  // Done.
  return true;

}

//--------------------------------------------------------------------------

// Print statistics on event generation.

void Pythia::stat() {

  // Read out settings for what to include.
  bool showPrL = settings.flag("Stat:showProcessLevel");
  bool showPaL = settings.flag("Stat:showPartonLevel");
  bool showErr = settings.flag("Stat:showErrors");
  bool reset   = settings.flag("Stat:reset");

  // Statistics on cross section and number of events.
  if (doProcessLevel) { 
    if (showPrL) processLevel.statistics(false);
    if (reset)   processLevel.resetStatistics(); 
  }  

  // Statistics from other classes, currently multiparton interactions.
  if (showPaL) partonLevel.statistics(false);  
  if (reset)   partonLevel.resetStatistics();   

  // Summary of which and how many warnings/errors encountered.
  if (showErr) info.errorStatistics();
  if (reset)   info.errorReset();

}

//--------------------------------------------------------------------------

// Print statistics on event generation - deprecated version.

void Pythia::statistics(bool all, bool reset) {

  // Statistics on cross section and number of events. 
  if (doProcessLevel) processLevel.statistics(reset);  

  // Statistics from other classes, e.g. multiparton interactions.
  if (all) partonLevel.statistics(reset);  

  // Summary of which and how many warnings/errors encountered.
  info.errorStatistics();
  if (reset) info.errorReset();

}

//--------------------------------------------------------------------------

// Write the Pythia banner, with symbol and version information.

void Pythia::banner(ostream& os) {

  // Read in version number and last date of change.
  double versionNumber = settings.parm("Pythia:versionNumber");
  int versionDate = settings.mode("Pythia:versionDate");
  string month[12] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};  

  // Get date and time.
  time_t t = time(0);
  char dateNow[12];
  strftime(dateNow,12,"%d %b %Y",localtime(&t));
  char timeNow[9];
  strftime(timeNow,9,"%H:%M:%S",localtime(&t));
  
  os << "\n"
     << " *-------------------------------------------" 
     << "-----------------------------------------* \n"
     << " |                                           "
     << "                                         | \n"
     << " |  *----------------------------------------" 
     << "--------------------------------------*  | \n"
     << " |  |                                        "
     << "                                      |  | \n"
     << " |  |                                        "
     << "                                      |  | \n"
     << " |  |   PPP   Y   Y  TTTTT  H   H  III    A  "
     << "    Welcome to the Lund Monte Carlo!  |  | \n" 
     << " |  |   P  P   Y Y     T    H   H   I    A A " 
     << "    This is PYTHIA version " << fixed << setprecision(3) 
     << setw(5) << versionNumber << "      |  | \n"
     << " |  |   PPP     Y      T    HHHHH   I   AAAAA"
     << "    Last date of change: " << setw(2) << versionDate%100 
     << " " << month[ (versionDate/100)%100 - 1 ] 
     << " " << setw(4) << versionDate/10000 <<  "  |  | \n"
     << " |  |   P       Y      T    H   H   I   A   A"
     << "                                      |  | \n"
     << " |  |   P       Y      T    H   H  III  A   A"
     << "    Now is " << dateNow << " at " << timeNow << "    |  | \n"
     << " |  |                                        " 
     << "                                      |  | \n"
     << " |  |   Torbjorn Sjostrand;  Department of As" 
     << "tronomy and Theoretical Physics,      |  | \n"
     << " |  |      Lund University, Solvegatan 14A, S"
     << "E-223 62 Lund, Sweden;                |  | \n"
     << " |  |      phone: + 46 - 46 - 222 48 16; e-ma"
     << "il: torbjorn@thep.lu.se               |  | \n"
     << " |  |   Stefan Ask;  Department of Physics, U"
     << "niversity of Cambridge,               |  | \n"
     << " |  |      Cavendish Laboratory, JJ Thomson A"
     << "ve., Cambridge CB3 0HE, UK;           |  | \n"
     << " |  |      phone: + 41 - 22 - 767 6707; e-mai"
     << "l: Stefan.Ask@cern.ch                 |  | \n"
     << " |  |   Richard Corke;  Department of Astrono" 
     << "my and Theoretical Physics,           |  | \n"
     << " |  |      Lund University, Solvegatan 14A, S"
     << "E-223 62 Lund, Sweden;                |  | \n"
     << " |  |      e-mail: richard.corke@thep.lu.se  "
     << "                                      |  | \n"
     << " |  |   Stephen Mrenna;  Computing Division, "
     << "Simulations Group,                    |  | \n"
     << " |  |      Fermi National Accelerator Laborat"
     << "ory, MS 234, Batavia, IL 60510, USA;  |  | \n"
     << " |  |      phone: + 1 - 630 - 840 - 2556; e-m"
     << "ail: mrenna@fnal.gov                  |  | \n"
     << " |  |   Stefan Prestel;  Department of Astron" 
     << "omy and Theoretical Physics,          |  | \n"
     << " |  |      Lund University, Solvegatan 14A, S"
     << "E-223 62 Lund, Sweden;                |  | \n"
     << " |  |      phone: + 46 - 46 - 222 06 64; e-ma"
     << "il: stefan.prestel@thep.lu.se         |  | \n"
     << " |  |   Peter Skands;  Theoretical Physics, C"
     << "ERN, CH-1211 Geneva 23, Switzerland;  |  | \n"
     << " |  |      phone: + 41 - 22 - 767 2447; e-mai"
     << "l: peter.skands@cern.ch               |  | \n"
     << " |  |                                        " 
     << "                                      |  | \n"
     << " |  |   The main program reference is the 'Br"
     << "ief Introduction to PYTHIA 8.1',      |  | \n"
     << " |  |   T. Sjostrand, S. Mrenna and P. Skands"
     << ", Comput. Phys. Comm. 178 (2008) 85   |  | \n"
     << " |  |   [arXiv:0710.3820]                    " 
     << "                                      |  | \n"
     << " |  |                                        "
     << "                                      |  | \n"
     << " |  |   The main physics reference is the 'PY"
     << "THIA 6.4 Physics and Manual',         |  | \n"
     << " |  |   T. Sjostrand, S. Mrenna and P. Skands"
     << ", JHEP05 (2006) 026 [hep-ph/0603175]. |  | \n"
     << " |  |                                        "
     << "                                      |  | \n"
     << " |  |   An archive of program versions and do" 
     << "cumentation is found on the web:      |  | \n"
     << " |  |   http://www.thep.lu.se/~torbjorn/Pythi" 
     << "a.html                                |  | \n"
     << " |  |                                        "
     << "                                      |  | \n"
     << " |  |   This program is released under the GN"
     << "U General Public Licence version 2.   |  | \n"
     << " |  |   Please respect the MCnet Guidelines f"
     << "or Event Generator Authors and Users. |  | \n"     
     << " |  |                                        "
     << "                                      |  | \n"
     << " |  |   Disclaimer: this program comes withou"
     << "t any guarantees.                     |  | \n"
     << " |  |   Beware of errors and use common sense"
     << " when interpreting results.           |  | \n"
     << " |  |                                        "
     << "                                      |  | \n"
     << " |  |   Copyright (C) 2012 Torbjorn Sjostrand" 
     << "                                      |  | \n"
     << " |  |                                        "
     << "                                      |  | \n"
     << " |  |                                        "
     << "                                      |  | \n"
     << " |  *----------------------------------------" 
     << "--------------------------------------*  | \n"
     << " |                                           "
     << "                                         | \n"
     << " *-------------------------------------------" 
     << "-----------------------------------------* \n" << endl;

}

//--------------------------------------------------------------------------

// Check for lines in file that mark the beginning of new subrun.

int Pythia::readSubrun(string line, bool warn, ostream& os) {

  // If empty line then done.
  int subrunLine = SUBRUNDEFAULT;  
  if (line.find_first_not_of(" \n\t\v\b\r\f\a") == string::npos) 
    return subrunLine;

  // If first character is not a letter, then done.
  string lineNow = line;
  int firstChar = lineNow.find_first_not_of(" \n\t\v\b\r\f\a");
  if (!isalpha(lineNow[firstChar])) return subrunLine; 

  // Replace an equal sign by a blank to make parsing simpler.
  while (lineNow.find("=") != string::npos) {
    int firstEqual = lineNow.find_first_of("=");
    lineNow.replace(firstEqual, 1, " ");   
  }

  // Get first word of a line.
  istringstream splitLine(lineNow);
  string name;
  splitLine >> name;

  // Replace two colons by one (:: -> :) to allow for such mistakes.
  while (name.find("::") != string::npos) {
    int firstColonColon = name.find_first_of("::");
    name.replace(firstColonColon, 2, ":");   
  }

  // Convert to lowercase.
  for (int i = 0; i < int(name.length()); ++i) 
    name[i] = std::tolower(name[i]); 

  // If no match then done.
  if (name != "main:subrun") return subrunLine; 

  // Else find new subrun number and return it.
  splitLine >> subrunLine;
  if (!splitLine) {
    if (warn) os << "\n PYTHIA Warning: Main:subrun number not"
        << " recognized; skip:\n   " << line << endl;
    subrunLine = SUBRUNDEFAULT; 
  } 
  return subrunLine;

}

//--------------------------------------------------------------------------

// Check that the final event makes sense: no unknown id codes;
// charge and energy-momentum conserved.

bool Pythia::check(ostream& os) {

  // Reset. 
  bool physical     = true;
  bool listVertices = false;
  bool listSystems  = false;
  bool listBeams    = false;
  iErrId.resize(0);
  iErrCol.resize(0);
  iErrNan.resize(0);
  iErrNanVtx.resize(0);
  Vec4 pSum;
  double chargeSum  = 0.;

  // Incoming beams counted with negative momentum and charge.
  if (doProcessLevel) {
    pSum      = - (event[1].p() + event[2].p());
    chargeSum = - (event[1].charge() + event[2].charge());

  // If no ProcessLevel then sum final state of process record.
  } else if (process.size() > 0) {
    pSum = - process[0].p();
    for (int i = 0; i < process.size(); ++i) 
      if (process[i].isFinal()) chargeSum -= process[i].charge(); 

  // If process not filled, then use outgoing primary in event.
  } else {
    pSum = - event[0].p();
    for (int i = 1; i < event.size(); ++i) 
      if (event[i].statusAbs() < 10 || event[i].statusAbs() == 23) 
        chargeSum -= event[i].charge();
  } 
  double eLab = abs(pSum.e());

  // Loop over particles in the event. 
  for (int i = 0; i < event.size(); ++i) {

    // Look for any unrecognized particle codes.
    int id = event[i].id();
    if (id == 0 || !particleData.isParticle(id)) {
      ostringstream errCode;
      errCode << ", id = " << id;
      info.errorMsg("Error in Pythia::check: "
        "unknown particle code", errCode.str()); 
      physical = false;
      iErrId.push_back(i);

    // Check that colour assignments are the expected ones.
    } else {
      int colType = event[i].colType();
      int col     = event[i].col();
      int acol    = event[i].acol();
      if ( (colType ==  0 && (col  > 0 || acol  > 0))
        || (colType ==  1 && (col <= 0 || acol  > 0))    
        || (colType == -1 && (col  > 0 || acol <= 0))    
        || (colType ==  2 && (col <= 0 || acol <= 0)) ) {
        ostringstream errCode;
        errCode << ", id = " << id << " cols = " << col << " " << acol;
        info.errorMsg("Error in Pythia::check: "
          "incorrect colours", errCode.str()); 
        physical = false;
        iErrCol.push_back(i);
      }        
    }

    // Look for particles with not-a-number energy/momentum/mass.
    if (abs(event[i].px()) >= 0. && abs(event[i].py()) >= 0. 
      && abs(event[i].pz()) >= 0.  && abs(event[i].e()) >= 0. 
      && abs(event[i].m()) >= 0.) ;
    else {   
      info.errorMsg("Error in Pythia::check: "
        "not-a-number energy/momentum/mass"); 
      physical = false;
      iErrNan.push_back(i);
    }

    // Look for particles with not-a-number vertex/lifetime.
    if (abs(event[i].xProd()) >= 0. && abs(event[i].yProd()) >= 0. 
      && abs(event[i].zProd()) >= 0.  && abs(event[i].tProd()) >= 0. 
      && abs(event[i].tau()) >= 0.) ;
    else {   
      info.errorMsg("Error in Pythia::check: "
        "not-a-number vertex/lifetime"); 
      physical     = false;
      listVertices = true;
      iErrNanVtx.push_back(i);
    }

    // Add final-state four-momentum and charge.      
    if (event[i].isFinal()) {
      pSum      += event[i].p();
      chargeSum += event[i].charge();
    }

  // End of particle loop.
  }

  // Check energy-momentum/charge conservation.
  double epDev = abs(pSum.e()) + abs(pSum.px()) + abs(pSum.py())
    + abs(pSum.pz());
  if (epDev > epTolErr * eLab) { 
    info.errorMsg("Error in Pythia::check: energy-momentum not conserved"); 
    physical = false;
  } else if (epDev > epTolWarn * eLab) { 
    info.errorMsg("Warning in Pythia::check: "
      "energy-momentum not quite conserved"); 
  }
  if (abs(chargeSum) > 0.1) {
    info.errorMsg("Error in Pythia::check: charge not conserved"); 
    physical = false;
  }

  // Check that beams and event records agree on incoming partons.
  // Only meaningful for resolved beams.
  if (info.isResolved())
  for (int iSys = 0; iSys < beamA.sizeInit(); ++iSys) {
    int eventANw  = partonSystems.getInA(iSys);
    int eventBNw  = partonSystems.getInB(iSys); 
    int beamANw   = beamA[iSys].iPos();  
    int beamBNw   = beamB[iSys].iPos(); 
    if (eventANw != beamANw || eventBNw != beamBNw) {
      info.errorMsg("Error in Pythia::check: "
        "event and beams records disagree"); 
      physical    = false;
      listSystems = true;
      listBeams   = true;
    }
  }

  // Done for sensible events.
  if (physical) return true;

  // Print (the first few) flawed events.
  if (nErrEvent < nErrList) {
    os << " PYTHIA erroneous event info: \n";
    if (iErrId.size() > 0) {
      os << " unknown particle codes in lines ";
      for (int i = 0; i < int(iErrId.size()); ++i) 
        os << iErrId[i] << " ";
      os << "\n";
    }
    if (iErrCol.size() > 0) {
      os << " incorrect colour assignments in lines ";
      for (int i = 0; i < int(iErrCol.size()); ++i) 
        os << iErrCol[i] << " ";
      os << "\n";
    }
    if (iErrNan.size() > 0) {
      os << " not-a-number energy/momentum/mass in lines ";
      for (int i = 0; i < int(iErrNan.size()); ++i) 
        os << iErrNan[i] << " ";
      os << "\n";
    }
    if (iErrNanVtx.size() > 0) {
      os << " not-a-number vertex/lifetime in lines ";
      for (int i = 0; i < int(iErrNanVtx.size()); ++i) 
        os << iErrNanVtx[i] << " ";
      os << "\n";
    }
    if (epDev > epTolErr * eLab) os << scientific << setprecision(3)
      << " total energy-momentum non-conservation = " << epDev << "\n";
    if (abs(chargeSum) > 0.1) os << fixed << setprecision(2) 
      << " total charge non-conservation = " << chargeSum << "\n"; 
    info.list();
    event.list(listVertices);
    if (listSystems) partonSystems.list();
    if (listBeams) beamA.list();
    if (listBeams) beamB.list();
  }

  // Update error counter. Done also for flawed event.
  ++nErrEvent;
  return false;

}

//--------------------------------------------------------------------------

// Routine to set up a PDF pointer.

PDF* Pythia::getPDFPtr(int idIn, int sequence) {

  // Temporary pointer to be returned. 
  PDF* tempPDFPtr = 0;

  // One option is to treat a Pomeron like a pi0.
  if (idIn == 990 && settings.mode("PDF:PomSet") == 2) idIn = 111;

  // Proton beam, normal choice.
  if (abs(idIn) == 2212 && sequence == 1) {
    int  pSet      = settings.mode("PDF:pSet");
    bool useLHAPDF = settings.flag("PDF:useLHAPDF");

    // Use internal sets.
    if (!useLHAPDF) {
      if      (pSet == 1) tempPDFPtr = new GRV94L(idIn);
      else if (pSet == 2) tempPDFPtr = new CTEQ5L(idIn);
      else if (pSet <= 6) 
           tempPDFPtr = new MSTWpdf(idIn, pSet - 2, xmlPath, &info);
      else tempPDFPtr = new CTEQ6pdf(idIn, pSet - 6, xmlPath, &info);
    }
    
    // Use sets from LHAPDF.
    else {
      string LHAPDFset    = settings.word("PDF:LHAPDFset");
      int    LHAPDFmember = settings.mode("PDF:LHAPDFmember");
      tempPDFPtr = new LHAPDF(idIn, LHAPDFset, LHAPDFmember, 1, &info);

      // Optionally allow extrapolation beyond x and Q2 limits.
      tempPDFPtr->setExtrapolate( settings.flag("PDF:extrapolateLHAPDF") );
    }
  }

  // Proton beam, special choice for the hard process.
  else if (abs(idIn) == 2212) {
    int  pSet      = settings.mode("PDF:pHardSet");
    bool useLHAPDF = settings.flag("PDF:useHardLHAPDF");

    // Use internal sets.
    if (!useLHAPDF) {
      if      (pSet == 1) tempPDFPtr = new GRV94L(idIn);
      else if (pSet == 2) tempPDFPtr = new CTEQ5L(idIn);
      else if (pSet <= 6) 
           tempPDFPtr = new MSTWpdf(idIn, pSet - 2, xmlPath, &info);
      else tempPDFPtr = new CTEQ6pdf(idIn, pSet - 6, xmlPath, &info);
    }
    
    // Use sets from LHAPDF.
    else {
      string LHAPDFset    = settings.word("PDF:hardLHAPDFset");
      int    LHAPDFmember = settings.mode("PDF:hardLHAPDFmember");
      tempPDFPtr = new LHAPDF(idIn, LHAPDFset, LHAPDFmember, 2, &info);

      // Optionally allow extrapolation beyond x and Q2 limits.
      tempPDFPtr->setExtrapolate( settings.flag("PDF:extrapolateLHAPDF") );
    }
  }

  // Pion beam (or, in one option, Pomeron beam). 
  else if (abs(idIn) == 211 || idIn == 111) {
    bool useLHAPDF = settings.flag("PDF:piUseLHAPDF");

    // Use internal sets.
    if (!useLHAPDF) {
      tempPDFPtr = new GRVpiL(idIn);
    }
    
    // Use sets from LHAPDF.
    else {
      string LHAPDFset    = settings.word("PDF:piLHAPDFset");
      int    LHAPDFmember = settings.mode("PDF:piLHAPDFmember");
      tempPDFPtr = new LHAPDF(idIn, LHAPDFset, LHAPDFmember, 1, &info);

      // Optionally allow extrapolation beyond x and Q2 limits.
      tempPDFPtr->setExtrapolate( settings.flag("PDF:extrapolateLHAPDF") );
    }
  }

  // Pomeron beam, if not treated like a pi0 beam.
  else if (idIn == 990) {
    int    pomSet  = settings.mode("PDF:PomSet");
    double rescale = settings.parm("PDF:PomRescale");

    // A generic Q2-independent parametrization.
    if (pomSet == 1) { 
      double gluonA      = settings.parm("PDF:PomGluonA");
      double gluonB      = settings.parm("PDF:PomGluonB");
      double quarkA      = settings.parm("PDF:PomQuarkA");
      double quarkB      = settings.parm("PDF:PomQuarkB");
      double quarkFrac   = settings.parm("PDF:PomQuarkFrac");
      double strangeSupp = settings.parm("PDF:PomStrangeSupp");
      tempPDFPtr = new PomFix( 990, gluonA, gluonB, quarkA, quarkB, 
        quarkFrac, strangeSupp);
    }
    
    // The H1 Q2-dependent parametrizations. Initialization requires files.
    else if (pomSet == 3 || pomSet == 4) 
      tempPDFPtr = new PomH1FitAB( 990, pomSet - 2, rescale, xmlPath, &info); 
    else if (pomSet == 5) 
      tempPDFPtr = new PomH1Jets( 990, rescale, xmlPath, &info); 
    else if (pomSet == 6) 
      tempPDFPtr = new PomH1FitAB( 990, 3, rescale, xmlPath, &info); 
  }

  // Lepton beam: neutrino, resolved charged lepton or unresolved ditto.
  else if (abs(idIn) > 10 && abs(idIn) < 17) {
    if (abs(idIn)%2 == 0) tempPDFPtr = new NeutrinoPoint(idIn);
    else if (settings.flag("PDF:lepton")) tempPDFPtr = new Lepton(idIn);
    else tempPDFPtr = new LeptonPoint(idIn);
  }

  // Failure for unrecognized particle.
  else tempPDFPtr = 0;
  
  // Done.
  return tempPDFPtr; 
}

//--------------------------------------------------------------------------

// Initialize SUSY Les Houches Accord data.

bool Pythia::initSLHA() {

  // Initial and settings values.
  int    ifailLHE    = 1;
  int    ifailSpc    = 1;
  int    readFrom    = settings.mode("SLHA:readFrom");
  string lhefFile    = settings.word("Beams:LHEF");
  string slhaFile    = settings.word("SLHA:file");
  int    verboseSLHA = settings.mode("SLHA:verbose");
  bool   slhaUseDec  = settings.flag("SLHA:useDecayTable");

  // No SUSY by default
  couplingsPtr->isSUSY = false;

  // Option with no SLHA read-in at all.
  if (readFrom == 0) return true;  

  // First check LHEF header (if reading from LHEF)
  if (readFrom == 1 && lhefFile != "void") {
    ifailLHE = slha.readFile(lhefFile, verboseSLHA, slhaUseDec );    
  }

  // If LHEF read successful, everything needed should already be ready
  if (ifailLHE == 0) {
    ifailSpc = 0;
    couplingsPtr->isSUSY = true;
  // If no LHEF file or no SLHA info in header, read from SLHA:file
  } else {
    lhefFile = "void";
    if ( settings.word("SLHA:file") == "none"
	 || settings.word("SLHA:file") == "void" 
	 || settings.word("SLHA:file") == "" 
	 || settings.word("SLHA:file") == " ") return true;      
    ifailSpc = slha.readFile(slhaFile,verboseSLHA, slhaUseDec);
  }

  // In case of problems, print error and fail init.
  if (ifailSpc != 0) {
    info.errorMsg("Error in Pythia::initSLHA: "
      "problem reading SLHA file", slhaFile);
    return false;
  } else {
    couplingsPtr->isSUSY = true;
  }
  
  // Check spectrum for consistency. Switch off SUSY if necessary.
  ifailSpc = slha.checkSpectrum();

  // ifail >= 1 : no MODSEL found -> don't switch on SUSY
  if (ifailSpc == 1) {
    // no SUSY, but MASS ok
    couplingsPtr->isSUSY = false;
    info.errorMsg("Warning in Pythia::initSLHA: "
		  "No MODSEL found, keeping internal SUSY switched off");    
  } else if (ifailSpc >= 2) {
    // no SUSY, but problems    
    info.errorMsg("Warning in Pythia::initSLHA: "
		      "Problem with SLHA MASS or QNUMBERS.");    
    couplingsPtr->isSUSY = false;
  }
  // ifail = 0 : MODSEL found, spectrum OK
  else if (ifailSpc == 0) {
    // Print spectrum. Done. 
    slha.printSpectrum(0);
  }
  else if (ifailSpc < 0) {
    info.errorMsg("Warning in Pythia::initSLHA: "
		      "Problem with SLHA spectrum.", 
		      "\n Only using masses and switching off SUSY.");
    settings.flag("SUSY:all", false);
    couplingsPtr->isSUSY = false;
    slha.printSpectrum(ifailSpc);
  } 

  // Import qnumbers
  if ( (ifailSpc == 1 || ifailSpc == 0) && slha.qnumbers.size() > 0) {
    for (int iQnum=0; iQnum < int(slha.qnumbers.size()); iQnum++) {
      // Always use positive id codes
      int id = abs(slha.qnumbers[iQnum](0));
      ostringstream idCode;
      idCode << id;      
      if (particleData.isParticle(id)) {
	info.errorMsg("Warning in Pythia::initSLHA: "
		      "ignoring QNUMBERS", "for id = "+idCode.str()
		      +" (already exists)", true);
      } else {
	int qEM3    = slha.qnumbers[iQnum](1);
	int nSpins  = slha.qnumbers[iQnum](2);
	int colRep  = slha.qnumbers[iQnum](3);
	int hasAnti = slha.qnumbers[iQnum](4);
	// Translate colRep to PYTHIA colType
	int colType = 0;
	if (colRep == 3) colType = 1;
	else if (colRep == -3) colType = -1;
	else if (colRep == 8) colType = 2;
	else if (colRep == 6) colType = 3;
	else if (colRep == -6) colType = -3;
	// Default name: PDG code
	string name, antiName;
	ostringstream idStream;
	idStream<<id;
	name     = idStream.str();
	antiName = "-"+name;	
	if (iQnum < int(slha.qnumbersName.size())) {
	  name = slha.qnumbersName[iQnum];
	  if (iQnum < int(slha.qnumbersAntiName.size())) 
	    antiName = slha.qnumbersAntiName[iQnum];
	  if (antiName == "") {
	    if (name.find("+") != string::npos) {
	      antiName = name;
	      antiName.replace(antiName.find("+"),1,"-");
	    } else if (name.find("-") != string::npos) {
	      antiName = name;
	      antiName.replace(antiName.find("+"),1,"-");
	    } else {
	      antiName = name+"bar";
	    }
	  }
	}
	if ( hasAnti == 0) {
	  antiName = "";
	  particleData.addParticle(id, name, nSpins, qEM3, colType); 
	} else {
	  particleData.addParticle(id, name, antiName, nSpins, qEM3, colType); 
	}
      }
    }
  }

  // Import mass spectrum.
  bool   keepSM     = settings.flag("SLHA:keepSM");
  double minMassSM  = settings.parm("SLHA:minMassSM");
  double massMargin = settings.parm("SLHA:minDecayDeltaM");
  if (ifailSpc == 1 || ifailSpc == 0) {

    // Loop through to update particle data.
    int    id = slha.mass.first();
    for (int i = 1; i <= slha.mass.size() ; i++) {
      double mass = abs(slha.mass(id));

      // Ignore masses for known SM particles or particles with 
      // default masses < minMassSM; overwrite masses for rest.
      if (keepSM && (id < 25 || (id > 80 && id < 1000000))) ;
      else if (id < 1000000 && particleData.m0(id) < minMassSM) {
	ostringstream idCode;
	idCode << id;      
	info.errorMsg("Warning in Pythia::initSLHA: "
	  "ignoring MASS entry", "for id = "+idCode.str()
	  +" (m0 < SLHA:minMassSM)", true);
      } 
      else particleData.m0(id,mass);
      id = slha.mass.next();
    };

  }

  // Update decay data.
  for (int iTable=0; iTable < int(slha.decays.size()); iTable++) {
    
    // Pointer to this SLHA table
    LHdecayTable* slhaTable=&(slha.decays[iTable]);
    
    // Extract ID and create pointer to corresponding particle data object
    int idRes     = slhaTable->getId();
    ParticleDataEntry* particlePtr 
      = particleData.particleDataEntryPtr(idRes);
    
    // Ignore decay channels for known SM particles or particles with 
    // default masses < minMassSM; overwrite masses for rest.
    if (keepSM && (idRes < 25 || (idRes > 80 && idRes < 1000000))) continue;
    else if (idRes < 1000000 && particleData.m0(idRes) < minMassSM) {
      ostringstream idCode;
      idCode << idRes;      
      info.errorMsg("Warning in Pythia::initSLHA: "
        "ignoring DECAY table", "for id = " + idCode.str()
	+ " (m0 < SLHA:minMassSM)", true);
      continue;
    }
    
    // Extract and store total width (absolute value, neg -> switch off)
    double widRes = abs(slhaTable->getWidth());
    particlePtr->setMWidth(widRes);

    // Reset decay table of the particle. Allow decays, treat as resonance.
    if (slhaTable->size() > 0) {
      particlePtr->clearChannels();
      particleData.mayDecay(idRes,true);
      particleData.isResonance(idRes,true);
    }        
    
    // Reset to stable if width <= 0.0
    if (slhaTable->getWidth() <= 0.0) particleData.mayDecay(idRes,false);
    
    // Set initial minimum mass.
    double brWTsum   = 0.;
    double massWTsum = 0.;
    
    // Loop over SLHA channels, import into Pythia, treating channels
    // with negative branching fractions as having the equivalent positive
    // branching fraction, but being switched off for this run
    for (int iChannel=0 ; iChannel<slhaTable->size(); iChannel++) {
      LHdecayChannel slhaChannel = slhaTable->getChannel(iChannel);
      double brat      = slhaChannel.getBrat();
      vector<int> idDa = slhaChannel.getIdDa();
      if (idDa.size() >= 9) {
	info.errorMsg("Error in Pythia::initSLHA: "
			  "max number of decay products is 8.");
      } else if (idDa.size() <= 1) {
	info.errorMsg("Error in Pythia::initSLHA: "
			  "min number of decay products is 2.");	  
      }
      else {
	int onMode = 1;
	if (brat < 0.0) onMode = 0;
	
	// Check phase space, including margin
	double massSum = massMargin;
	for (int jDa=0; jDa<int(idDa.size()); ++jDa) 
	  massSum += particleData.m0( idDa[jDa] ); 
	if (onMode == 1 && brat > 0.0 && massSum > particleData.m0(idRes) ) {
	  // String containing decay name
	  ostringstream errCode;
	  errCode << idRes <<" ->";
	  for (int jDa=0; jDa<int(idDa.size()); ++jDa) errCode<<" "<<idDa[jDa];
	  info.errorMsg("Warning in Pythia::initSLHA: switching off decay",  
            errCode.str() + " (mRes - mDa < minDecayDeltaM)"
            "\n       (Note: cross sections will be scaled by remaining"
	    " open branching fractions!)" , true);
	  onMode=0;
	}
	
	// Branching-ratio-weighted average mass in decay.
	brWTsum   += abs(brat);
	massWTsum += abs(brat) * massSum;
	
	// Add channel
	int id0 = idDa[0];
	int id1 = idDa[1];
	int id2 = (idDa.size() >= 3) ? idDa[2] : 0;
	int id3 = (idDa.size() >= 4) ? idDa[3] : 0;
	int id4 = (idDa.size() >= 5) ? idDa[4] : 0;
	int id5 = (idDa.size() >= 6) ? idDa[5] : 0;
	int id6 = (idDa.size() >= 7) ? idDa[6] : 0;
	int id7 = (idDa.size() >= 8) ? idDa[7] : 0;
	particlePtr->addChannel(onMode,abs(brat),101,
				      id0,id1,id2,id3,id4,id5,id6,id7);
	
      }
    }
    
    // Set minimal mass, but always below nominal one.
    if (slhaTable->size() > 0) {
      double massAvg = massWTsum / brWTsum;
      double massMin = min( massAvg, particlePtr->m0()) - massMargin;
      particlePtr->setMMin(massMin);
    }
  }
  
  return true;
  
}

//--------------------------------------------------------------------------

// Function to perform CKKWL merging on the input event.

bool Pythia::mergeProcess() {

  // Reset weight of the event.
  mergingHooksPtr->setWeight(1.);
  info.setMergingWeight(1.);
  double wgt = 1.0;

  // Store candidates for the splitting V -> qqbar'.
  mergingHooksPtr->storeHardProcessCandidates( process);

  // Calculate number of clustering steps.
  int nSteps = mergingHooksPtr->getNumberOfClusteringSteps( process);

  // Save number of hard final state quarks / leptons.
  int nQuarks     = mergingHooksPtr->nHardOutPartons();
  int nLeptons    = mergingHooksPtr->nHardOutLeptons();
  bool isHadronic = (mergingHooksPtr->nHardInLeptons() == 0);

  // Set QCD 2->2 starting scale different from arbirtrary scale in LHEF!
  // --> Set to pT of partons.
  double pT = 0.;
  for( int i=0; i < process.size(); ++i)
  if(process[i].isFinal() && process[i].colType() != 0) {
    pT = process[i].pT();
    break;
  }

  // Count number of top quarks to distinguish process with stable top pair
  // from pure QCD di-jets.
  int nTops = 0;
  for( int i=0; i < process.size(); ++i)
    if(process[i].isFinal() && process[i].idAbs() == 6) ++nTops;

  // Declare pdfWeight and alpha_s weight.
  double RN = rndm.flat();
  process.scale(0.0);

  // Generate all histories.
  History FullHistory( nSteps, 0.0, process, Clustering(), mergingHooksPtr,
            beamA, beamB, &particleData, &info, true, true, 1.0, 0);

  // Perform reweighting with Sudakov factors, save alpha_s ratios and
  // PDF ratio weights.
  wgt = FullHistory.weightTREE( &trialPartonLevel,
    mergingHooksPtr->AlphaS_FSR(), mergingHooksPtr->AlphaS_ISR(), RN);

  // Event with production scales set for further (trial) showering
  // and starting conditions for the shower.
  FullHistory.getStartingConditions( RN, process );

  // Allow to dampen histories in which the lowest multiplicity reclustered
  // state does not pass the lowest multiplicity cut of the matrix element.
  double dampWeight = mergingHooksPtr->dampenIfFailCuts(
    FullHistory.lowestMultProc(RN) );

  // Save the weight of the event for histogramming. Only change the
  // event weight after trial shower on the matrix element
  // multiplicity event (= in doVetoStep).
  wgt *= dampWeight;

  // For pure QCD dijet events (only!), set the process scale to the
  // transverse momentum of the outgoing partons.
  if ( isHadronic && nQuarks == 2 && nLeptons == 0 && nTops == 0
    && nSteps == 0 ) process.scale(pT);

  // Save the weight of the event for histogramming.
  mergingHooksPtr->setWeight(wgt);
  info.setMergingWeight(wgt);

  // If the weight of the event is zero, do not continue evolution.
  if(wgt == 0.) return true;

  // Done
  return false;

}

//==========================================================================

} // end namespace Pythia8
