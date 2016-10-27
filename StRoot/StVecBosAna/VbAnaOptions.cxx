
#include <getopt.h>
#include <cstdlib>

#include "VbAnaOptions.h"


using namespace std;


/** Default constructor. */
VbAnaOptions::VbAnaOptions() : AnaOptions(),
   fOptions("Allowed options"), fOptionsValues(),
   fFitSinePhase(0), fFitSineOffset(0), fUseOtherSolution(false), fOldStyle(false)
{
   // Declare the supported options
   fOptions.add_options()
      ("help,h",          "Print help message")
      ("filelist,f",      po::value<string>(&fListName), "Name of the file with input list")
      ("max-events,n",    po::value<uint32_t>(&fMaxEventsUser)->default_value(0), "Maximum number of events to process")
      ("save-graphs,g",   po::value<bool>(&fSaveGraphs)->implicit_value(true), "Process input as Monte-Carlo")
      ("tpm,t",           po::value<float>(&fTracksPtMin)->default_value(0.2), "Minimum Pt for each track and cluster in the Recoil")
      ("mctype,t",        po::value<int>(&fMcType)->default_value(0), "Type of MC - 0 -> real data (default), 1 -> W+ MC, 2 -> W- MC, 3 -> Any other Mc")
      ("rhicrun,r",       po::value<unsigned short>(&fRhicRunId)->default_value(11), "RHIC run id, e.g. 9, 11, 12, 13, ... (run 11 default)")
      ("monte-carlo,m",   po::value<bool>(&fIsMc)->implicit_value(true), "Process input as Monte-Carlo")
      ("wboson,w",        "Process input events as W boson events. Mutually exclusive with --zboson")
      ("zboson,z",        "Process input events as Z boson events. Mutually exclusive with --wboson")
      ("wboson-other,o",  po::value<bool>(&fUseOtherSolution)->implicit_value(true), "Use this flag to select second " \
                          "solution for longitudinal W momentum component")
      ("fit-sine-phase",  po::value<double>(&fFitSinePhase)->default_value(M_PI_2)->implicit_value(DBL_MAX), "Value for phase")
      ("fit-sine-offset", po::value<double>(&fFitSineOffset)->default_value(0)->implicit_value(DBL_MAX), "Value for offset")
      ("old-style",       po::value<bool>(&fOldStyle)->implicit_value(true), "run with olds style ROOT Containers ")
   ;

   fOutFileName = "vbana";
}


string VbAnaOptions::GetRootFileName()  const { return GetResultsDir() + "/hist/" + fOutFileName + GetSuffix() + ".root"; }
double VbAnaOptions::GetFitSinePhase()  const { return fFitSinePhase; }
double VbAnaOptions::GetFitSineOffset() const { return fFitSineOffset; }
bool   VbAnaOptions::UseOtherSolution() const { return fUseOtherSolution; }
bool   VbAnaOptions::GetIsMc()          const { return fIsMc; }
bool   VbAnaOptions::GetOldStyle()      const { return fOldStyle; }


/**
 * Takes the standard command line arguments and parses them with the boost program_options utility.
 * Additional checks are implemented to verify the validity of the supplied arguments.
 */
void VbAnaOptions::ProcessOptions(int argc, char **argv)
{
   po::store(po::parse_command_line(argc, argv, fOptions), fOptionsValues);
   po::notify(fOptionsValues);

   if (fOptionsValues.count("help"))
   {
      cout << fOptions << endl;
      exit(EXIT_FAILURE);
   }

   Info("ProcessOptions", "User provided options:");

   if (fOptionsValues.count("filelist"))
   {
      cout << "filelist: " << fListName << endl;
      ifstream file_list(GetListFileName().c_str());
      if (!file_list.good()) {
         Error("VbAnaOptions", "Filelist \"%s\" does not exist", GetListFileName().c_str());
         exit(EXIT_FAILURE);
      }
   } else {
      Error("VbAnaOptions", "Filelist not set");
      exit(EXIT_FAILURE);
   }

   if (fOptionsValues.count("max-events"))
   {
      cout << "max-events: " << fMaxEventsUser << endl;
   }

   cout << "save-graphs:  "  << fSaveGraphs  << endl;
   cout << "tracks-ptmin (tpm): "  << fTracksPtMin << endl;
   cout << "Monte Carlo type: "  << fMcType << endl;
   cout << "RHIC run Id: "  << fRhicRunId << endl;
   cout << "monte-carlo:  "  << fIsMc        << endl;

   if (fOptionsValues.count("zboson") && fOptionsValues.count("wboson"))
   {
      Error("VbAnaOptions", "Only one option can be specified at a time: -w or -z");
      exit(EXIT_FAILURE);
   } else if (fOptionsValues.count("zboson")) {
      fBosonType = kZBoson;
   } else if (fOptionsValues.count("wboson")) {
      fBosonType = kWBoson;
   }

   cout << "w/z boson type: " << fBosonType << endl;
   cout << "wboson-other: " << fUseOtherSolution << endl;
   cout << "old-style: " << fOldStyle << endl;

   if (fOptionsValues.count("fit-sine-phase"))
   {
      cout << "fit-sine-phase: " << fFitSinePhase << endl;
   }

   if (fOptionsValues.count("fit-sine-offset"))
   {
      cout << "fit-sine-offset: " << fFitSineOffset << endl;
   }

   AnaOptions::VerifyOptions();

   MakeOutDir();
}


/** */
void VbAnaOptions::Print(const Option_t* opt) const
{
   Info("Print", "Print members:");
   PrintAsPhp();
}


/** */
void VbAnaOptions::PrintAsPhp(FILE *f) const
{
   AnaOptions::PrintAsPhp(f);
}
