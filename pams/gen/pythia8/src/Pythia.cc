// Pythia.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the Pythia class.

#include "Pythia.h"

// Access time information.
#include <ctime>

// Allow string and character manipulation.
#include <cctype>

namespace Pythia8 {
 
//**************************************************************************

// The Pythia class.

//*********

// Constants: could be changed here if desired, but normally should not.
// These are of technical nature, as described for each.

// Maximum number of tries to produce parton level from given input.
const int Pythia::NTRY          = 10; 

// Negative integer to denote that no subrun has been set.
const int Pythia::SUBRUNDEFAULT = -999; 

//*********
  
// Constructor. 

Pythia::Pythia(string xmlDir) { 
    
  // Initial values for pointers to PDF's.
  useNewPdfA      = false; 
  useNewPdfB      = false; 
  useNewPdfHard   = false; 
  pdfAPtr         = 0; 
  pdfBPtr         = 0; 
  pdfHardAPtr     = 0; 
  pdfHardBPtr     = 0; 

  // Initial values for pointers to Les Houches Event objects.
  doLHA           = false;
  useNewLHA       = false;
  lhaUpPtr        = 0;

  // Initial value for pointer to external decay handler.
  decayHandlePtr  = 0;

  // Initial value for pointer to user hooks.
  userHooksPtr    = 0;

  // Initial value for pointer to beam shape.
  useNewBeamShape = false;
  beamShapePtr    = 0;

  // Initial values for pointers to timelike and spacelike showers.
  useNewTimes     = false;
  useNewSpace     = false;
  timesDecPtr     = 0;
  timesPtr        = 0;
  spacePtr        = 0;

  // Send Info pointer to handle error printout/counting correctly.
  settings.initPtr( &info);
  ParticleDataEntry::initPtr( &info);
  particleData.initPtr( &info);

  // Find path to data files, i.e. xmldoc directory location.
  // Environment variable takes precedence, else use constructor input. 
  string path     = "";
  const char* PYTHIA8DATA = "PYTHIA8DATA"; 
  char* envPath = getenv(PYTHIA8DATA);
  if (envPath != 0 && *envPath != '\0') {
    int i = 0;
    while (*(envPath+i) != '\0') path += *(envPath+(i++)); 
  } 
  else path = xmlDir;
  if (path[ path.length() - 1 ] != '/') path += "/";

  // Read in files with all flags, modes, parms and words.
  string initFile = path + "Index.xml";
  isConstructed = settings.init( initFile);
  if (!isConstructed) { 
    info.errorMsg("Abort from Pythia::Pythia: settings unavailable");
    return;
  }

  // Read in files with all particle data.
  string dataFile = path + "ParticleData.xml";
  isConstructed = particleData.init( dataFile);
  if (!isConstructed) {
    info.errorMsg("Abort from Pythia::Pythia: particle data unavailable");
    return;
  }

  // Write the Pythia banner to output. 
  banner();

  // Set headers to distinguish the two event listing kinds.
  process.init("(hard process)");
  event.init("(complete event)");

  // Not initialized until at the end of initInternal.
  isInit         = false;
 
} 

//*********
  
// Destructor.

Pythia::~Pythia() { 

  // Delete the PDF's created with new.
  if (useNewPdfHard && pdfHardAPtr != pdfAPtr) delete pdfHardAPtr; 
  if (useNewPdfHard && pdfHardBPtr != pdfBPtr) delete pdfHardBPtr; 
  if (useNewPdfA) delete pdfAPtr; 
  if (useNewPdfB) delete pdfBPtr; 

  // Delete the Les Houches object created with new.
  if (useNewLHA) delete lhaUpPtr;

  // Delete the BeamShape object created with new.
  if (useNewBeamShape) delete beamShapePtr;

  // Delete the timelike and spacelike showers created with new.
  if (useNewTimes) delete timesPtr;
  if (useNewSpace) delete spacePtr;

} 

//*********

// Read in one update for a setting or particle data from a single line.

bool Pythia::readString(string line, bool warn) {

  // Check that constructor worked.
  if (!isConstructed) return false;

  // If empty line then done.
  if (line.find_first_not_of(" ") == string::npos) return true;

  // If first character is not a letter/digit, then taken to be a comment.
  int firstChar = line.find_first_not_of(" ");
  if (!isalnum(line[firstChar])) return true; 

  // Send on particle data to the ParticleData database.
  if (isdigit(line[firstChar])) 
    return particleData.readString(line, warn);

  // Everything else sent on to Settings.
  return settings.readString(line, warn);

}

//*********

// Read in updates for settings or particle data from user-defined file.

bool Pythia::readFile(string fileName, bool warn, int subrun) {

  // Check that constructor worked.
  if (!isConstructed) return false;

  // Open file with updates.
  const char* cstring = fileName.c_str();
  ifstream is(cstring);  
  if (!is) {
    info.errorMsg("Error in Pythia::readFile: did not find file", fileName);
    return false;
  }

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

//*********

// Routine to pass in pointers to PDF's. Usage optional.

bool Pythia::setPDFPtr( PDF* pdfAPtrIn, PDF* pdfBPtrIn, PDF* pdfHardAPtrIn, 
    PDF* pdfHardBPtrIn) {

  // The routine can have no effect if PDF's already assigned.
  if (pdfAPtr != 0 || pdfBPtr != 0) return false;

  // The two PDF objects cannot be one and the same, or unassigned.
  if (pdfAPtrIn == pdfBPtrIn || pdfAPtrIn == 0 || pdfBPtrIn == 0) return false;

  // Save pointers.  
  pdfAPtr = pdfAPtrIn;
  pdfBPtr = pdfBPtrIn;

  // By default same pointers for hard-process PDF's.
  pdfHardAPtr = pdfAPtrIn;
  pdfHardBPtr = pdfBPtrIn;
  
  // Optionally allow separate pointers for hard process.
  if (pdfHardAPtrIn == 0 || pdfHardBPtrIn == 0) return true;
  if (pdfHardAPtrIn == pdfHardBPtrIn) return false;
  pdfHardAPtr = pdfHardAPtrIn;
  pdfHardBPtr = pdfHardBPtrIn;
  
  // Done.
  return true;
}

//*********

// Routine to initialize with CM energy.

bool Pythia::init( int idAin, int idBin, double eCMin) {

  // Read in and set values.
  idA       = idAin;
  idB       = idBin;
  frameType = 1;
  eCM       = eCMin;
  doLHA     = false;

  // Send on to common initialization.
  bool status = initInternal();
  if (!status) info.errorMsg("Abort from Pythia::init: initialization failed");
  return status;

}

//*********

// Routine to initialize with two collinear beams,  energies specified.

bool Pythia::init( int idAin, int idBin, double eAin, double eBin) {

  // Read in and set values.
  idA       = idAin;
  idB       = idBin;
  frameType = 2;
  eA        = eAin;
  eB        = eBin;
  doLHA     = false;

  // Send on to common initialization. 
  bool status = initInternal();
  if (!status) info.errorMsg("Abort from Pythia::init: initialization failed");
  return status;

}

//*********

// Routine to initialize with two beams specified by three-momenta.

bool Pythia::init( int idAin, int idBin, double pxAin, double pyAin, 
  double pzAin, double pxBin, double pyBin, double pzBin) {

  // Read in and set values.
  idA       = idAin;
  idB       = idBin;
  frameType = 3;
  pxA       = pxAin;
  pyA       = pyAin;
  pzA       = pzAin;
  pxB       = pxBin;
  pyB       = pyBin;
  pzB       = pzBin;
  doLHA     = false;

  // Send on to common initialization. 
  bool status = initInternal();
  if (!status) info.errorMsg("Abort from Pythia::init: initialization failed");
  return status;

}

//*********

// Routine to initialize when all info is given in a Les Houches Event File.

bool Pythia::init( string LesHouchesEventFile, bool skipInit) {

  // Destroy any previous LHAup object.
  if (useNewLHA) delete lhaUpPtr;

  // Create LHAup object. Send in pointer to info.
  const char* cstring = LesHouchesEventFile.c_str();
  lhaUpPtr   = new LHAupLHEF(cstring);
  lhaUpPtr->setPtr( &info);
  doLHA      = true;
  useNewLHA  = true;

  // Store or replace LHA pointer in other classes.
  processLevel.setLHAPtr( lhaUpPtr);

  // If second time around, only with new file, then simplify.
  if (skipInit) return true;

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
  frameType = 2;

  // Now do normal initialization. List info if there.
  bool status = initInternal();
  lhaUpPtr->listInit(); 
  if (!status) info.errorMsg("Abort from Pythia::init: initialization failed");
  return status;

}

//*********

// Routine to initialize with the variable values of the Beams kind.

bool Pythia::init() {

  // Check if to read from Les Houches Event File set, and is so send on.
  if (mode("Beams:frameType") == 4) {
    string lhef   = word("Beams:LHEF");
    bool skipInit = flag("Main:LHEFskipInit");
    return init( lhef, skipInit);
  }  

  // Read in and set values.
  idA       = mode("Beams:idA");
  idB       = mode("Beams:idB");
  frameType = mode("Beams:frameType");
  eCM       = parm("Beams:eCM");
  eA        = parm("Beams:eA");
  eB        = parm("Beams:eB");
  pxA       = parm("Beams:pxA");
  pyA       = parm("Beams:pyA");
  pzA       = parm("Beams:pzA");
  pxB       = parm("Beams:pxB");
  pyB       = parm("Beams:pyB");
  pzB       = parm("Beams:pzB");
  doLHA     = false;

  // Send on to common initialization.
  bool status = initInternal();
  if (!status) info.errorMsg("Abort from Pythia::init: initialization failed");
  return status;

}

//*********

// Routine to initialize when beam info is given in an LHAup object.

bool Pythia::init( LHAup* lhaUpPtrIn) {

  // Save and set flag for subsequent usage of LHAup object.
  lhaUpPtr = lhaUpPtrIn;
  doLHA      = true;

  // Store LHA pointer in other classes.
  processLevel.setLHAPtr( lhaUpPtr);

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
  frameType = 2;

  // Now do normal initialization. List info if there.
  bool status = initInternal();
  lhaUpPtr->listInit();
  if (!status) info.errorMsg("Abort from Pythia::init: initialization failed");
  return status;

}

//*********

// Main routine to initialize the generation process.
// (The alternative init forms end up in this one.)

bool Pythia::initInternal() {

  // Check that constructor worked.
  isInit = false;
  if (!isConstructed) return false;

  // Reset error counters. 
  nErrEvent = 0;
  info.errorReset();

  // Initialize data members extracted from database.
  doProcessLevel   = settings.flag("ProcessLevel:all");
  doPartonLevel    = settings.flag("PartonLevel:all") && doProcessLevel;
  doHadronLevel    = settings.flag("HadronLevel:all");
  doMomentumSpread = settings.flag("Beams:allowMomentumSpread");
  doVertexSpread   = settings.flag("Beams:allowVertexSpread");
  checkEvent       = settings.flag("Check:event");
  nErrList         = settings.mode("Check:nErrList");
  epTolErr         = settings.parm("Check:epTolErr");
  epTolWarn        = settings.parm("Check:epTolWarn");

  // Initialize the random number generator.
  if ( settings.flag("Random:setSeed") )  
    Rndm::init( settings.mode("Random:seed") );

  // Initialize tunes to e+e- and pp/ppbar data.
  initTunes();

  // Initialize couplings (needed to initialize resonances).
  AlphaEM::initStatic(); 
  CoupEW::initStatic(); 
  VCKM::initStatic(); 

  // Initialize some aspects of particle data, including resonances.
  ParticleDataEntry::initStatic();
  particleData.initBWmass();
  particleData.initResonances(resonancePtrs);

  // Set up values related to user hooks.
  hasUserHooks  = (userHooksPtr > 0);
  doVetoProcess = (hasUserHooks) 
                ? userHooksPtr->canVetoProcessLevel() : false;
  doVetoPartons = (hasUserHooks) 
                ? userHooksPtr->canVetoPartonLevel() : false; 

  // Set up values related to beam shape.
  if (beamShapePtr == 0) {
    beamShapePtr = new BeamShape();
    useNewBeamShape = true;
  } 
  beamShapePtr->init();

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
  timesPtr->initPtr( &info);
  timesDecPtr->initPtr( &info);
  spacePtr->initPtr( &info);
  timesDecPtr->init( 0, 0);

  // Check that beams and beam combination can be handled.
  // Only allow neutrinos as beams when leptons unresolved.
  bool canHandleBeams = false;
  int idAabs = abs(idA);
  int idBabs = abs(idB);
  if (doProcessLevel) {
    if (idAabs == 2212 && idBabs == 2212) canHandleBeams = true;
    else if ( idAabs == idBabs && (idAabs == 11 || idAabs == 13
      || idAabs == 15) ) canHandleBeams = true;
    else if ( idAabs > 10 && idAabs < 17 && idA * idB < 0
      && !settings.flag("PDF:lepton") ) {
      if (idAabs == idBabs) canHandleBeams = true;
      int idMax  = max(idAabs, idBabs);
      int idMin  = min(idAabs, idBabs);
      if (idMax - idMin == 1 && idMax%2 == 0) canHandleBeams = true; 
    }
    if (!canHandleBeams) {
      info.errorMsg("Error in Pythia::init: "
        "cannot handle this beam combination");
      return false;
    }
  }

  // Do not set up beam kinematics when no process level.
  if (!doProcessLevel) frameType = 1;
  else {
    
    // Set up beam kinematics.
    if (!initKinematics()) return false;

    // Set up the PDF's, if not already done.
    if (pdfAPtr == 0) {
      pdfAPtr     = getPDFPtr(idA); 
      if (!pdfAPtr->isSetup()) return false;
      pdfHardAPtr = pdfAPtr;
      useNewPdfA  = true;
    }
    if (pdfBPtr == 0) {
      pdfBPtr     = getPDFPtr(idB); 
      if (!pdfBPtr->isSetup()) return false;
      pdfHardBPtr = pdfBPtr;
      useNewPdfB  = true;
    }

    // Optionally set up separate PDF's for hard process.
    if (settings.flag("PDF:useHard")) {
      pdfHardAPtr = getPDFPtr(idA, 2);      
      if (!pdfHardAPtr->isSetup()) return false;
      pdfHardBPtr = getPDFPtr(idB, 2);      
      if (!pdfHardBPtr->isSetup()) return false;
      useNewPdfHard  = true;
    }
  
    // Set up the two beams and the common remnant system.
    StringFlav* flavSel = hadronLevel.getStringFlavPtr();
    bool isUnresolvedA = ( ParticleDataTable::isLepton(idA) 
      && !settings.flag("PDF:lepton") );
    bool isUnresolvedB = ( ParticleDataTable::isLepton(idB) 
      && !settings.flag("PDF:lepton") );
    beamA.init( idA, pzAcm, eA, mA, &info, pdfAPtr, pdfHardAPtr, 
      isUnresolvedA, flavSel);
    beamB.init( idB, pzBcm, eB, mB, &info, pdfBPtr, pdfHardBPtr, 
      isUnresolvedB, flavSel);
  }

  // Send info/pointers to process level for initialization.
  if ( doProcessLevel && !processLevel.init( &info, &beamA, &beamB, 
    &sigmaTot, doLHA, &slha, userHooksPtr, sigmaPtrs) ) return false;

  // Send info/pointers to parton level for initialization.
  if ( doPartonLevel && !partonLevel.init( &info, &beamA, &beamB, 
    &sigmaTot, timesDecPtr, timesPtr, spacePtr, userHooksPtr) ) 
    return false;

  // Send info/pointers to hadron level for initialization.
  // Note: forceHadronLevel() can come, so we must always initialize. 
  if ( !hadronLevel.init( &info, timesDecPtr, decayHandlePtr, 
    handledParticles) ) return false;

  // Optionally check particle data table for inconsistencies.
  if ( settings.flag("Check:particleData") ) 
    particleData.checkTable( settings.mode("Check:levelParticleData") );

  // Succeeded.
  isInit = true; 
  return true;
}

//*********

// Calculate kinematics at initialization. Store beam four-momenta.

bool Pythia::initKinematics() {

  // Find masses. Initial guess that we are in CM frame.
  mA       = ParticleDataTable::m0(idA);
  mB       = ParticleDataTable::m0(idB);
  betaZ    = 0.;
  gammaZ   = 1.;

  // Collinear beams not in CM frame: find CM energy.
  if (frameType == 2) {
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
    if (abs(betaZ) < 1e-10) frameType = 1;
  }

  // Completely general beam directions: find CM energy.
  else if (frameType == 3) {
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
  if (eCM < mA + mB) return false;

  // Set up CM-frame kinematics with beams along +-z axis.
  pzAcm    = 0.5 * sqrtpos( (eCM + mA + mB) * (eCM - mA - mB) 
           * (eCM - mA + mB) * (eCM + mA - mB) ) / eCM;
  pzBcm    = -pzAcm;
  eA       = sqrt(mA*mA + pzAcm*pzAcm);
  eB       = sqrt(mB*mB + pzBcm*pzBcm);

  // If in CM frame then store beam four-vectors (else already done above).
  if (frameType != 2 && frameType != 3) {
    pAinit = Vec4( 0., 0., pzAcm, eA);
    pBinit = Vec4( 0., 0., pzBcm, eB);
  }

  // Store main info for access in process generation.
  info.setBeamA( idA, pzAcm, eA, mA);
  info.setBeamB( idB, pzBcm, eB, mB);
  info.setECM( eCM);

  // Must allow for generic boost+rotation when beam momentum spread.
  if (doMomentumSpread) frameType = 3;

  // Done.
  return true;

}

//*********

// Initialize tunes to e+e- and pp/ppbar data.

void Pythia::initTunes() {

  // Modes to use. Fast return if all is default.
  int eeTune = settings.mode("Tune:ee");
  int ppTune = settings.mode("Tune:pp");
  if (eeTune == 0 && ppTune == 0) return;

  // Marc Montull's tune to particle composition at LEP1.
  if (eeTune == 101) {  
    settings.parm("StringZ:aLund",            0.76  );
    settings.parm("StringZ:bLund",            0.58  );   // default
    settings.parm("StringFlav:probStoUD",     0.22  );
    settings.parm("StringFlav:probQQtoQ",     0.08  );
    settings.parm("StringFlav:probSQtoQQ",    0.75  );
    settings.parm("StringFlav:probQQ1toQQ0",  0.025 );
    settings.parm("StringFlav:mesonUDvector", 0.5   );
    settings.parm("StringFlav:mesonSvector",  0.6   );
    settings.parm("StringFlav:mesonCvector",  1.5   );
    settings.parm("StringFlav:mesonBvector",  2.5   );
    settings.parm("StringFlav:etaSup",        0.60  );
    settings.parm("StringFlav:etaPrimeSup",   0.15  );
    settings.parm("StringFlav:popcornSpair",  1.0   );
    settings.parm("StringFlav:popcornSmeson", 1.0   );
  }

}

//*********

// Main routine to generate the next event, using internal machinery.

bool Pythia::next() {

  // Simpler option when only HadronLevel to be generated.
  if (!doProcessLevel) {

    // Set correct energy for system.
    Vec4 pSum = 0.;
    for (int i = 1; i < event.size(); ++i) 
      if (event[i].isFinal()) pSum += event[i].p();
    event[0].p( pSum );
    event[0].m( pSum.mCalc() );

    // Generate hadronization and decays.
    return forceHadronLevel();
  }

  // Reset arrays.
  info.clear();
  process.clear();
  event.clear();

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
    bool hasVetoed = false;

    // Provide the hard process that starts it off. Only one try.
    info.clear();
    process.clear();
    if ( !processLevel.next( process) ) {
      info.errorMsg("Abort from Pythia::next: "
        "processLevel failed; giving up"); 
      return false;
    }

    // Possibility for a user veto of the process-level event.
    if (doVetoProcess) {
      hasVetoed = userHooksPtr->doVetoProcessLevel( process);
      if (hasVetoed) continue;
    }

    // Possibility to stop the generation at this stage.
    if (!doPartonLevel) {
      boostAndVertex( true, true);
      processLevel.accumulate();
      return true;
    }
  
    // Allow up to ten tries for parton- and hadron-level processing.
    bool physical   = true;
    bool hasBoosted = false;
    for (int iTry = 0; iTry < NTRY; ++ iTry) {

      physical = true;

      // Restore process record to CM frame if it was boosted.
      if (hasBoosted) {
        boostAndVertex( false, false);
        hasBoosted = false;
      }

      // Reset event record and (extracted partons from) beam remnants.
      event.clear();
      beamA.clear();
      beamB.clear();
   
      // Parton-level evolution: ISR, FSR, MI.
      if ( !partonLevel.next( process, event) ) {
        // Skip to next hard process for failure owing to deliberate veto.
        hasVetoed = partonLevel.hasVetoed(); 
        if (hasVetoed) break;
        // Else make a new try for other failures.
        info.errorMsg("Error in Pythia::next: "
          "partonLevel failed; try again"); 
        physical = false; 
        continue;
      }

      // Possibility for a user veto of the parton-level event.
      if (doVetoPartons) {
        hasVetoed = userHooksPtr->doVetoPartonLevel( event);
        if (hasVetoed) break;
      }

      // Boost to lab frame (before decays, for vertices).
      boostAndVertex( true, true);
      if (frameType == 2 || frameType == 3) hasBoosted = true;

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
        return true;
      }

      // Hadron-level: hadronization, decays.
      if ( !hadronLevel.next( event) ) {
        info.errorMsg("Error in Pythia::next: "
          "hadronLevel failed; try again"); 
        physical = false; 
        continue;
      }

      // Stop parton- and hadron-level looping if you got this far.
      break;
    }

    // If event vetoed then to make a new try.
    if (hasVetoed) continue;

    // If event failed any other way (after ten tries) then give up.
    if (!physical) {
      info.errorMsg("Abort from Pythia::next: "
        "parton+hadronLevel failed; giving up");
      return false;
    }

    // Process- and parton-level statistics. 
    processLevel.accumulate();
    partonLevel.accumulate();

    // End of outer loop over hard processes. Done with normal option.
    break;
  }

  // Optionally check final event for problems.
  if (checkEvent && !check()) {
    info.errorMsg("Abort from Pythia::next: "
      "check of event revealed problems");
    return false;
  }

  // Done.
  return true;

}

//*********

// Generate only the hadronization/decay stage, using internal machinery.
// The "event" instance should already contain a parton-level configuration. 

bool Pythia::forceHadronLevel() {

  // Can only generate event if initialization worked.
  if (!isInit) {
    info.errorMsg("Abort from Pythia::forceHadronLevel: "
      "not properly initialized so cannot generate events"); 
    return false;
  }

  // Check whether any junctions in system. (Normally done in ProcessLevel.)
  event.clearJunctions();
  processLevel.findJunctions( event);

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

//*********

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

//*********

// Boost from CM frame to lab frame, or inverse. Set production vertex.

void Pythia::boostAndVertex( bool toLab, bool setVertex) {

  // Boost process from CM frame to lab frame.
  if (toLab) {
    if      (frameType == 2) process.bst(0., 0., betaZ, gammaZ);
    else if (frameType == 3) process.rotbst(MfromCM);

    // Boost nonempty event from CM frame to lab frame.
    if (event.size() > 0) {       
      if      (frameType == 2) event.bst(0., 0., betaZ, gammaZ);
      else if (frameType == 3) event.rotbst(MfromCM);
    }

  // Boost process from lab frame to CM frame.
  } else {
    if      (frameType == 2) process.bst(0., 0., -betaZ, gammaZ);
    else if (frameType == 3) process.rotbst(MtoCM);

    // Boost nonempty event from lab frame to CM frame.
    if (event.size() > 0) {       
      if      (frameType == 2) event.bst(0., 0., -betaZ, gammaZ);
      else if (frameType == 3) event.rotbst(MtoCM);
    }
  }

  // Set production vertex; assumes particles are in lab frame and at origin
  if (setVertex && doVertexSpread) {
    Vec4 vertex = beamShapePtr->vertex();
    for (int i = 0; i < process.size(); ++i) process[i].vProd( vertex);
    for (int i = 0; i < event.size(); ++i) event[i].vProd( vertex);
  }  

}

//*********

// Print statistics on event generation.

void Pythia::statistics(bool all, bool reset) {

  // Statistics on cross section and number of events. 
  if (doProcessLevel) processLevel.statistics(reset);  

  // Statistics from other classes, e.g. multiple interactions.
  if (all) partonLevel.statistics(reset);  

  // Summary of which and how many warnings/errors encountered.
  info.errorStatistics();
  if (reset) info.errorReset();

}

//*********

// Write the Pythia banner, with symbol and version information.

void Pythia::banner(ostream& os) {

  // Read in version number and last date of change.
  double versionNumber = Settings::parm("Pythia:versionNumber");
  int versionDate = Settings::mode("Pythia:versionDate");
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
     << " |  |   Main author: Torbjorn Sjostrand; CERN" 
     << "/PH, CH-1211 Geneva, Switzerland,     |  | \n"
     << " |  |     and Department of Theoretical Physi"
     << "cs, Lund University, Lund, Sweden;    |  | \n"
     << " |  |     phone: + 41 - 22 - 767 82 27; e-mai"
     << "l: torbjorn@thep.lu.se                |  | \n"
     << " |  |   Author: Stephen Mrenna; Computing Div"
     << "ision, Simulations Group,             |  | \n"
     << " |  |     Fermi National Accelerator Laborato"
     << "ry, MS 234, Batavia, IL 60510, USA;   |  | \n"
     << " |  |     phone: + 1 - 630 - 840 - 2556; e-ma"
     << "il: mrenna@fnal.gov                   |  | \n"
     << " |  |   Author: Peter Skands; CERN/PH, CH-121" 
     << "1 Geneva, Switzerland,                |  | \n"
     << " |  |     and Theoretical Physics Department,"
     << "                                      |  | \n"
     << " |  |     Fermi National Accelerator Laborato"
     << "ry, MS 106, Batavia, IL 60510, USA;   |  | \n"
     << " |  |     phone: + 41 - 22 - 767 24 59; e-mai"
     << "l: skands@fnal.gov                    |  | \n"
     << " |  |                                        " 
     << "                                      |  | \n"
     << " |  |   The main program reference is the 'Br"
     << "ief Introduction to PYTHIA 8.1',      |  | \n"
     << " |  |   T. Sjostrand, S. Mrenna and P. Skands"
     << ", arXiv:0710.3820                     |  | \n"
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
     << " |  |   Copyright (C) 2008 Torbjorn Sjostrand" 
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

//*********

// Check for lines in file that mark the beginning of new subrun.

int Pythia::readSubrun(string line, bool warn, ostream& os) {

  // If empty line then done.
  int subrunLine = SUBRUNDEFAULT;  
  if (line.find_first_not_of(" ") == string::npos) return subrunLine;

  // If first character is not a letter, then done.
  string lineNow = line;
  int firstChar = lineNow.find_first_not_of(" ");
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

//*********

// Check that the final event makes sense: no unknown id codes;
// charge and energy-momentum conserved.

bool Pythia::check(ostream& os) {

  // Reset. 
  bool physical = true;
  iErrId.resize(0);
  iErrCol.resize(0);
  iErrNan.resize(0);
  Vec4 pSum;
  double chargeSum = 0.;

  // Incoming beams counted with negative momentum and charge.
  if (doProcessLevel) {
    pSum = - (event[1].p() + event[2].p());
    chargeSum = - (event[1].charge() + event[2].charge());

  // If no ProcessLevel then sum momentum and charge in initial state.
  } else {
    pSum = - event[0].p();
    for (int i = 0; i < process.size(); ++i) 
      if (process[i].isFinal()) chargeSum -= process[i].charge();
  } 
  double eLab = abs(pSum.e());

  // Loop over particles in the event. 
  for (int i = 0; i < event.size(); ++i) {

    // Look for any unrecognized particle codes.
    int id = event[i].id();
    if (id == 0 || !ParticleDataTable::isParticle(id)) {
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

    // Add final-state four-momentum and charge.      
    if (event[i].isFinal()) {
      pSum += event[i].p();
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
    if (epDev > epTolErr * eLab) os << scientific << setprecision(3)
      << " total energy-momentum non-conservation = " << epDev << "\n";
    if (abs(chargeSum) > 0.1) os << fixed << setprecision(2) 
      << " total charge non-conservation = " << chargeSum << "\n"; 
    info.list();
    event.list();
  }

  // Update error counter. Done also for flawed event.
  ++nErrEvent;
  return false;

}

//*********

// Routine to set up a PDF pointer.

PDF* Pythia::getPDFPtr(int idIn, int sequence) {

  PDF* tempPDFPtr;

  // Proton beam, normal choice.
  if (abs(idIn) == 2212 && sequence == 1) {
    int  pSet      = settings.mode("PDF:pSet");
    bool useLHAPDF = settings.flag("PDF:useLHAPDF");

    // Use internal sets.
    if (!useLHAPDF) {
      if (pSet == 1) tempPDFPtr = new GRV94L(idIn);
      else tempPDFPtr = new CTEQ5L(idIn);
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

  // Proton beam, special choice for the hard process..
  else if (abs(idIn) == 2212) {
    int  pSet      = settings.mode("PDF:pHardSet");
    bool useLHAPDF = settings.flag("PDF:useHardLHAPDF");

    // Use internal sets.
    if (!useLHAPDF) {
      if (pSet == 1) tempPDFPtr = new GRV94L(idIn);
      else tempPDFPtr = new CTEQ5L(idIn);
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

  // Lepton beam; resolved or not.
  else {
    if (settings.flag("PDF:lepton") && abs(idIn)%2 == 1) 
      tempPDFPtr = new Lepton(idIn);
    else tempPDFPtr = new LeptonPoint(idIn);
  }
  
  // Done.
  return tempPDFPtr; 
}

//*********

} // end namespace Pythia8


