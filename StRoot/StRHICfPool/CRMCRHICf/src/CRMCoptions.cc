#include <CRMCconfig.h>
#include <CRMCoptions.h>

#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <math.h>

#include <tclap/CmdLine.h>

//#include <boost/program_options.hpp>
// namespace po = boost::program_options;

using namespace std;

double mass(int id)
/// return mass in GeV
{
  double mass;
  switch (id)
  {
    case 120: // pi+
      mass = 0.13957018;
      break;
    case 130: // k+
      mass = 0.493677;
      break;
    default: // proton
      mass = 0.938272046;
  }
  return mass;
}

CRMCoptions::CRMCoptions(int argc, char **argv)
    : fError(false)
#if WITH_HEPMC3
    , fOutputMode(eHepMC3GZ)
#elif WITH_HEPMC
    , fOutputMode(eHepMCGZ)
#else
    , fOutputMode(eLHE)
#endif
    , fNCollision(500)
    , fSeed(0)
    , fProjectileId(1)
    , fTargetId(1)
    , fHEModel(0)
    , fTypout(0)
    , fProjectileMomentum(255.)
    , fTargetMomentum(-255.)
    , fParamFileName("crmc.param ")
    , fOutputFileName("")
    , fRHICfRunType("")
    , fJobIndex("")
    , fRivetAnalyses()
    , fRivetSearch()
    , fRivetPreloads()
    , fProduceTables(false)
    , fSeedProvided(false)
    , fTest(false)
    , fCSMode(false)
{
  CheckEnvironment();
  ParseOptions(argc, argv);
}


void CRMCoptions::CheckEnvironment()
{
  const char *crmcRoot = getenv("CONEX_ROOT");
  if (crmcRoot == 0)
  {
    crmcRoot = getenv("PWD");
  }

  /*
  const fs::path basepath(argv[0]);
  iUtl::Config::Init(basepath.parent_path() / "../..", argc, argv);

    ptime now = second_clock::local_time();
    date today = now.date();
    const date ignoreUntil = today - days(deltaDay) - months(deltaMonth) -
  years(deltaYear); fIgnoreUntilDay = ignoreUntil.day(); fIgnoreUntilMonth =
  ignoreUntil.month(); fIgnoreUntilYear = ignoreUntil.year();


  */
}


void CRMCoptions::ParseOptions(int argc, char **argv)
{
  std::ostringstream vers;
  vers << CRMC_VERSION_MAJOR << "." <<CRMC_VERSION_MINOR << "." << CRMC_VERSION_PATCH ;
  std::string version = vers.str();
    
  TCLAP::CmdLine cmd("Options for CRMC", ' ', version);

  ostringstream model_desc;
  model_desc << "model [0=EPOS.LHC-R (default), 1=EPOS.no.had.resc"
#ifdef CRMC_QGSJET01
             << ", 2=QGSJET01"
#endif
#ifdef CRMC_GHEISHA
             << ", 3=Gheisha"
#endif
#ifdef CRMC_PYTHIA
             << ", 4=Pythia_6.4.28"
#endif
#ifdef CRMC_HIJING
             << ", 5=Hijing_1.38"
#endif
#ifdef CRMC_SIBYLL
             << ", 6=Sibyll_2.3e"
#endif
#ifdef CRMC_QGSJETII04
             << ", 7=QGSJETII-04"
#endif
#ifdef CRMC_PHOJET
             << ", 8=Phojet"
#endif
#ifdef CRMC_QGSJETII03
             << ", 11=QGSJETII-03"
#endif
#ifdef CRMC_DPMJET06
             << ", 12=DPMJet 3.0-6"
#endif
#ifdef CRMC_DPMJET17
             << ", 12=DPMJet-III_2017.1"
#endif
#ifdef CRMC_DPMJET19
             << ", 12=DPMJet-III_2019.1"
#endif
#ifdef CRMC_QGSJETIII
             << ", 13=QGSJETIII-01"
#endif
             << "]";

  TCLAP::ValueArg<string> output("o",
                                 "output_mode",
                                 "hepmc, hepmcgz (default), root, lhe, lhegz, rivet"
                                 "hepmc2, hepmc2gz, hepmc3, hepmc3gz",
                                 false, // required
#if WITH_HEPMC3
                                 "hepmcgz", // default
#elif WITH_HEPMC
                                 "hepmcgz", // default
#else
                                 "lhegz", // default
#endif
                                 "string");
  cmd.add(output);

  TCLAP::ValueArg<int> seed(
      "s", "seed", "random seed between 0 and 1e9 (default: random)", false, 0, "int");
  cmd.add(seed);

  TCLAP::ValueArg<int> number(
      "n", "number", "number of collisions (default: 500)", false, 500, "int");
  cmd.add(number);

  TCLAP::ValueArg<int> model("m", "model", model_desc.str().c_str(), false, 0, "int");
  cmd.add(model);

  TCLAP::ValueArg<double> proMom(
      "p", "projectile-momentum", "momentum/(GeV/c) (default: 3500)", false, 0, "double");
  cmd.add(proMom);

  TCLAP::ValueArg<int> proId(
      "i", "projectile-id", "PDG or Z*10000+A*10 (default: proton)", false, 1, "int");
  cmd.add(proId);

  TCLAP::ValueArg<double> tarMom(
      "P", "target-momentum", "momentum/(GeV/c) (default: -3500)", false, 0, "double");
  cmd.add(tarMom);

  TCLAP::ValueArg<int> tarId(
      "I", "target-id", "PDG or Z*10000+A*10 (default: proton)", false, 1, "int");
  cmd.add(tarId);

  TCLAP::ValueArg<double> sqrtS("S", "sqrts", "sqrt(s/GeV**2)", false, 0, "double");
  cmd.add(sqrtS);

  TCLAP::ValueArg<string> config("c", "config", "config file", false, "", "string");
  cmd.add(config);

  TCLAP::ValueArg<string> out(
      "f", "out", "output file name (auto if none provided)", false, "", "string");
  cmd.add(out);

  TCLAP::ValueArg<string> runType(
    "R", "runType", "RHICf Run Type", false, "", "string");
  cmd.add(runType);

  TCLAP::ValueArg<string> jobIndex(
    "J", "jobIndex", "Specific job index", false, "", "string");
  cmd.add(jobIndex);

  TCLAP::SwitchArg tables("t", "produce-tables", "create tables if none are found", true);
  cmd.add(tables);

  //("filter,F", po::value<string>(), "specify file with filter commands")

  TCLAP::SwitchArg test("T", "test", "Run test mode", false);
  cmd.add(test);

  TCLAP::SwitchArg xs("x",
                      "cross-section",
                      "calculate and print cross section only",
                      false);
  cmd.add(xs);

  TCLAP::MultiArg<string> analysis(
      "a", "analysis", "Rivet analysis to execute", false, "string");
  cmd.add(analysis);

  TCLAP::MultiArg<string> include(
      "r", "rivet-include", "add to Rivet search path", false, "string");
  cmd.add(include);

  TCLAP::MultiArg<string> preload(
      "L", "preload", "add preloaded data to Rivet", false, "string");
  cmd.add(preload);

  try
  {
    cmd.parse(argc, argv);
  }
  catch (std::exception &e)
  {
    std::cout << e.what() << std::endl;
    exit(EXIT_FAILURE);
  }

  const string om = output.getValue();
  if (om == "hepmc3gz") // ---------------- HepMC3 + gzip
  {
#ifndef WITH_HEPMC3
    cerr << " Compile with HepMC3 first " << endl;
    exit(1);
#endif
    fOutputMode = eHepMC3GZ;
  }
  else if (om == "hepmc3") // ------------ HepMC3
  {
#ifndef WITH_HEPMC3
    cerr << " Compile with HepMC3 first " << endl;
    exit(1);
#endif
    fOutputMode = eHepMC3;
  }
  else if (om == "hepmc2gz") // ------------- HepMC2 + gzip
  {
#ifndef WITH_HEPMC
    cerr << " Compile with HepMC2 first " << endl;
    exit(1);
#endif
    fOutputMode = eHepMCGZ;
  }
  else if (om == "hepmc2") // --------------- HepMC2
  {
#ifndef WITH_HEPMC
    cerr << " Compile with HepMC2 first " << endl;
    exit(1);
#endif
    fOutputMode = eHepMC;
  }
  else if (om == "hepmcgz") // ---------- generic HepMC2 or HepMC3 + gzip
  {
#if WITH_HEPMC3
    fOutputMode = eHepMC3GZ;
#elif WITH_HEPMC
    fOutputMode = eHepMCGZ;
#else
    cerr << " Compile with HepMC3 or HepMC2 first " << endl;
    exit(1);
#endif
  }
  else if (om == "hepmc") // ------------------- generic HepMC2 or HepMC3
  {
#if WITH_HEPMC3
    fOutputMode = eHepMC3;
#elif WITH_HEPMC
    fOutputMode = eHepMC;
#else
    cerr << " Compile with HepMC3 or HepMC2 first " << endl;
    exit(1);
#endif
  }
  else if (om == "lhe") // ------- LHE
  {
    fOutputMode = eLHE;
  }
  else if (om == "lhegz")
  {
    fOutputMode = eLHEGZ;
  }
  else if (om == "rivet")
  {
#ifndef WITH_RIVET
    cerr << " Compile with Rivet first " << endl;
    exit(1);
#endif
    fOutputMode = eRivet;
  }
  else if (om == "root")
  {
#ifdef WITH_ROOT
    fOutputMode = eROOT;
#else
    cerr << " Compile with ROOT first " << endl;
    exit(1);
#endif
  }
  else
  {
    cerr << " Wrong output type: " << om << endl;
    cerr << " Check --help for more information" << endl;
    exit(1);
  }
  fOutputMode = eHepMC3;
  cout << "Output type has been forced to HepMC3 type for RHICf simulation" << endl;

  // check if either sqrt(s) or the momenta option was used
  if (sqrtS.isSet() && (tarMom.isSet() || proMom.isSet()))
  {
    cerr << "You can either specify sqrt(s) or the beam momenta." << endl;
    exit(1);
  }

  if (fOutputMode == eLHE || fOutputMode == eLHEGZ)
    fTypout = 1;

#ifdef WITH_ROOT
  if (fOutputMode == eROOT)
    fTypout = -1;
#endif

  // parameter readout
  if (seed.isSet())
  {
    fSeed = seed.getValue();
    if (fSeed < 0)
    {
      cerr << " Seed is negative: " << fSeed << endl;
      exit(1);
    }
    if (fSeed > 1e9)
    {
      cerr << " Seed too large (>1e9): " << fSeed << endl;
      exit(1);
    }
  }

  if (number.isSet())
    fNCollision = number.getValue();

  if (model.isSet())
    fHEModel = model.getValue();

  if (proMom.isSet())
    fProjectileMomentum = proMom.getValue();

  if (tarMom.isSet())
    fTargetMomentum = tarMom.getValue();

  if (proId.isSet())
    fProjectileId = proId.getValue();

  if (tarId.isSet())
    fTargetId = tarId.getValue();

  if (config.isSet())
  {
    fParamFileName = config.getValue();
    fParamFileName += ' '; // space needed at the end for Fortran command "index". "trim"
                           // needs f90 therefore not used here
  }

  if (out.isSet())
    fOutputFileName = out.getValue();

  if (runType.isSet())
    fRHICfRunType = runType.getValue();

  if (jobIndex.isSet())
    fJobIndex = jobIndex.getValue();

  if (tables.getValue())
    fProduceTables = tables.getValue();

  // if (opt.count("filter")) {
  // fFilter = opt["filter"].as<string>();
  //}

  fTest = test.getValue();
  if (test.getValue())
  {
#if WITH_HEPMC3
    fOutputMode = eHepMC3GZ;
#elif WITH_HEPMC
    fOutputMode = eHepMC;
#else
    // Countint for Test only implemented in these output policies
    cerr << "Need HepMC2 or HepMC3 output option." << endl;
    exit(1);
#endif
  }

  fCSMode = xs.getValue();
  if (xs.getValue())
  {
    fOutputMode = eNone;
    fNCollision = 1;
  }

  // check if random seed was provided, otherwise generate one
  fSeedProvided = fSeed;
  if (!fSeedProvided)
  {
    if (fTest)
      fSeed = 123; // if test and no seed provided fix to get same results
    else
    {
      ifstream urandom("/dev/urandom", ios::in | ios::binary);
      urandom.read((char *)&fSeed, sizeof(fSeed) / sizeof(char));
      urandom.close();
      fSeed = abs(fSeed) % 999999999;
    }
  }

  // treat sqrts setting
  if (sqrtS.isSet())
  {
    const double sqrts = sqrtS.getValue();
    const double e = sqrts / 2.;
    fTargetMomentum = -sqrt((e + mass(fTargetId)) * (e - mass(fTargetId)));
    fProjectileMomentum = sqrt((e + mass(fProjectileId)) * (e - mass(fProjectileId)));
    fSqrts = sqrts;
  }
  else // set fSqrts
  {
    const double eTarget
        = sqrt(fTargetMomentum * fTargetMomentum + pow(mass(fTargetId), 2));
    const double eProjectile
        = sqrt(fProjectileMomentum * fProjectileMomentum + pow(mass(fProjectileId), 2));
    fSqrts = sqrt(((eTarget + eProjectile) + (fTargetMomentum + fProjectileMomentum))
                  * ((eTarget + eProjectile) - (fTargetMomentum + fProjectileMomentum)));
  }

  if (analysis.isSet())
  {
    fRivetAnalyses.insert(fRivetAnalyses.end(),analysis.begin(),analysis.end());
  }
  else if (fOutputMode == eRivet)
  {
    cerr << " Rivet output required analysis to be specified. Check help. " << endl;
    exit(1);
  }
  if (include.isSet())
    fRivetSearch.insert(fRivetSearch.end(),include.begin(),include.end());
  if (preload.isSet())
    fRivetPreloads.insert(fRivetPreloads.end(),preload.begin(),preload.end());

  if (!(fOutputMode == eRivet)
      && (!fRivetAnalyses.empty() || !fRivetPreloads.empty() || !fRivetSearch.empty()))
  {
    cerr << "You specified Rivet-specific options, but not Rivet as output format... Why?"
         << endl;
    exit(1);
  }

  DumpConfig();
}

string CRMCoptions::ParticleName(const int pid) const
{
  if (pid < 10000)
  {

    switch (pid)
    {
      case 120:
        return "pi";
        break; // no pdg
      case -120:
        return "antipi";
        break; // no pdg
      case 211:
        return "pi";
        break;
      case -211:
        return "antipi";
        break;
      case 1:
        return "p";
        break; // pdg d-quark
      case -1:
        return "antip";
        break; // pdg anti d-quark
      case 12:
        return "C";
        break; // pdg: nu-e
      case 208:
        return "Pb";
        break; // no pdg
      case 2212:
        return "p";
        break;
      case -2212:
        return "antip";
        break;
      case 2112:
        return "n";
        break; // not so useful
      case -2112:
        return "antin";
        break; // not so useful
      default:
      {
        ostringstream ss;
        ss << "pdg" << pid;
        return ss.str();
        break;
      }
    }
  }
  const int Z = pid / 10000;
  const int A = (pid % 10000) / 10;

  switch (Z)
  {
    case 1:
      return "p";
      break;
    case 2:
      return "He";
      break;
    case 6:
      return "C";
      break;
    case 7:
      return "N";
      break;
    case 8:
      return "O";
      break;
    case 26:
      return "Fe";
      break;
    case 82:
      return "Pb";
      break;
  }
  ostringstream ss;
  ss << "A" << A << "Z" << Z;
  return ss.str();
}

void CRMCoptions::DumpConfig() const
{
  // if particles have same momentum the minus sign was probably forgotten
  if (fTargetMomentum == fProjectileMomentum)
  {
    cerr << " Beam particles at rest in the centre-of-mass frame. Did you forget a (-) "
            "sign for the target momentum? "
         << endl;
    cerr << "          exit ..." << endl;
    exit(1);
  }

  if (fTest)
  {
    cout << "\n          >> crmc Test mode <<\n\n" << endl;
  }
  else
  {
    cout << "\n          >> crmc <<\n\n";
  }
  cout << "  seed:                       " << fSeed
       << (fSeedProvided ? " (provided by user)" : " (automatic)") << "\n"
       << "  projectile id:              " << fProjectileId;
  if (fProjectileId / 10000 > 0)
  {
    const int Z = (fProjectileId % 1000000000 ) / 10000;
    const int A = (fProjectileId % 10000) / 10;
    cout << " (A=" << A << ", Z=" << Z << ")";
  }
  else
  {
    const string pname = ParticleName(fProjectileId);
    if (pname.find("pdg") != 0)
    {
      cout << " (" << pname << ")";
    }
  }
  cout << "\n"
       << "  projectile momentum:        " << fProjectileMomentum << "\n"
       << "  target id:                  " << fTargetId;
  if (fTargetId / 10000 > 0)
  {
    const int Z = (fTargetId % 1000000000 ) / 10000;
    const int A = (fTargetId % 10000) / 10;
    cout << " (A=" << A << ", Z=" << Z << ")";
  }
  else
  {
    const string pname = ParticleName(fTargetId);
    if (pname.find("pdg") != 0)
    {
      cout << " (" << pname << ")";
    }
  }
  cout << "\n"
       << "  target momentum:            " << fTargetMomentum << "\n\n";

  cout << "  number of collisions:       " << fNCollision << "\n"
       << "  parameter file name:        " << fParamFileName << "\n";
  if (!fTest && !fCSMode)
  {
    cout << "  output file format:         ";
    switch (fOutputMode)
    {
      case eHepMC:
        cout << "HepMC2";
        break;
      case eHepMCGZ:
        cout << "HepMC2+gzip\n";
        break;
      case eHepMC3:
        cout << "HepMC3\n";
        break;
      case eHepMC3GZ:
        cout << "HepMC3+gzip\n";
        break;
      case eLHE:
        cout << "LHE\n";
        break;
      case eLHEGZ:
        cout << "LHE + gzip\n";
        break;
      case eROOT:
        cout << "ROOT\n";
        break;
      case eRivet:
        cout << "RIVET\n";
        break;
      default:
        cout << "unknown\n";
    }
    cout << "  output file name:           " << GetOutputFileName() << "\n";
  }
  cout << "  HE model:                   " << fHEModel;

  switch (fHEModel)
  {
    case 0:
      cout << " (EPOS.LHC-R full (slow)) \n";
      break;
    case 1:
      cout << " (EPOS.LHC-R without hadronic rescattering (fast)) \n";
      break;
    case 2:
      cout << " (QGSJET01) \n";
      break;
    case 3:
      cout << " (Gheisha)\n ";
      break;
    case 4:
      cout << " (Pythia)\n ";
      break;
    case 5:
      cout << " (Hijing)\n ";
      break;
    case 6:
      cout << " (Sibyll 2.3e)\n ";
      break;
    case 7:
      cout << " (QGSJETII-04) \n";
      break;
    case 8:
      cout << " (Phojet) \n";
      break;
    case 11:
      cout << " (QGSJETII-03) \n";
      break;
#ifdef CRMC_DPMJET06
    case 12:
      cout << " (DPMJet 3.0-6) \n";
      break;
#endif
#ifdef CRMC_DPMJET17
    case 12:
      cout << " (DPMJet-III 2017.1) \n";
      break;
#endif
#ifdef CRMC_DPMJET19
    case 12:
      cout << " (DPMJet-III 2019.1) \n";
      break;
#endif
    case 13:
      cout << " (QGSJETIII-01) \n";
      break;
    default:
      cerr << " (unknown model) \n";
      exit(1);
  }
  cout << endl;

  if (fOutputMode == eRivet)
  {
    cout << "Rivet analyses:" << endl;
    for (auto ana : fRivetAnalyses)
      cout << "   - " << ana << endl;
    cout << "Rivet search path: " << (fRivetSearch.empty() ? "n/a" : "") << endl;
    for (auto srch : fRivetSearch)
      cout << "   - " << srch << endl;
    cout << "Rivet preloads: " << (fRivetPreloads.empty() ? "n/a" : "") << endl;
    for (auto pre : fRivetPreloads)
      cout << "   - " << pre << endl;
  }

  cout.setf(ios::showpoint);
  cout.setf(ios::fixed);
  cout.precision(3);
}


string CRMCoptions::GetOutputTypeEnding() const
{
  switch (fOutputMode)
  {
#ifdef WITH_HEPMC
    case eHepMC:
      return ".hepmc";
      break;
    case eHepMCGZ:
      return ".hepmc.gz";
      break;
#endif
#ifdef WITH_HEPMC3
    case eHepMC3:
      return ".hepmc";
      break;
    case eHepMC3GZ:
      return ".hepmc.gz";
      break;
#endif
    case eLHE:
      return ".lhe";
      break;
    case eLHEGZ:
      return ".lhe.gz";
      break;
#ifdef WITH_ROOT
    case eROOT:
      return ".root";
      break;
#endif
#ifdef WITH_RIVET
    case eRivet:
      return ".yoda";
      break;
#endif
  }
  return ".unknown";
}

string CRMCoptions::GetOutputFileName() const
{
  // open output file and connect tree

  const char *crmcOutDir = getenv("CRMC_OUT");
  if (crmcOutDir == 0)
  {
    crmcOutDir = getenv("PWD");
  }

  // check compatibility of path with lhe files
  if (GetOutputTypeEnding().find("lhe") != string::npos
      && string(crmcOutDir).find(".lhe") != string::npos)
  {
    cerr << " path error - path contains '.lhe' : " << crmcOutDir << endl;
    cerr << "          exit ..." << endl;
    exit(1);
  }

  // if file name provided
  if (fOutputFileName != "")
  {
    // check compatibility of file name with lhe files
    if (GetOutputTypeEnding().find("lhe") != string::npos
        && fOutputFileName.find(".lhe") == string::npos)
    {
      cerr << " file name error - extension is not '.lhe' : " << fOutputFileName << endl;
      cerr << "          exit ..." << endl;
      exit(1);
    }
    {
      return fOutputFileName;
    }
  }

  // create file name based on options
  ostringstream crmcFileName;
  crmcFileName << crmcOutDir << "/"
               << "crmc_";
  switch (fHEModel)
  {
    case 0:
      crmcFileName << "eposlhcr";
      break;
    case 1:
      crmcFileName << "eposnhs";
      break;
    case 2:
      crmcFileName << "qgsjet";
      break;
    case 3:
      crmcFileName << "gheisha";
      break;
    case 4:
      crmcFileName << "pythia";
      break;
    case 5:
      crmcFileName << "hijing";
      break;
    case 6:
      crmcFileName << "sibyll";
      break;
    case 7:
      crmcFileName << "qgsjetII04";
      break;
    case 8:
      crmcFileName << "phojet";
      break;
    case 11:
      crmcFileName << "qgsjetII03";
      break;
#ifdef CRMC_DPMJET06
    case 12:
      crmcFileName << "dpmjet3.0-6";
      break;
#endif
#ifdef CRMC_DPMJET17
    case 12:
      crmcFileName << "dpmjetIII.17";
      break;
#endif
#ifdef CRMC_DPMJET19
    case 12:
      crmcFileName << "dpmjetIII.19";
      break;
#endif
    case 13:
      crmcFileName << "qgsjetIII";
      break;
    default:
      cerr << " crmcOut: error - unknown model " << fHEModel << endl;
      cerr << "          exit ..." << endl;
      exit(1);
      break;
  }

  crmcFileName << "_" << fSeed << "_";

  crmcFileName << ParticleName(fProjectileId) << "_";
  crmcFileName << ParticleName(fTargetId);

  crmcFileName << "_" << fProjectileMomentum << GetOutputTypeEnding();

  return crmcFileName.str();
}
