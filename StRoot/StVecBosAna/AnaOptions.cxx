
#include <getopt.h>
#include <cstdlib>

#include "AnaOptions.h"


ClassImp(AnaOptions)

using namespace std;


/** */
AnaOptions::AnaOptions() : TObject()
   , fSuffix("")
   , fModes(0)
   , fDoReconstructJets(kFALSE)
   , fTracksPtMin(0.2)
   , fJetPtMin(0.5)
   , fRhicRunId(11)
   , fAnaDateTime(0)
   , fAnaTimeReal(0)
   , fAnaTimeCpu (0)
   , fEnvVars()
   , fFileStdLog(0)
   , fFileStdLogName("stdoe")
   , fFlagCopyResults(kFALSE)
   , fFlagUpdateDb(kFALSE)
   , fUserGroup()
   , fListName("undefined")
   , fOutFileName("vbasym")
   , fMaxEventsUser(0)
   , fSaveGraphs(false)
   , fMcType(0)
   , fIsMc(kFALSE)
   , fIsMcWp(kFALSE)
   , fBosonType(kWBoson)
{
   ReadEnvVars();
}


/** */
AnaOptions::~AnaOptions()
{
   if (fFileStdLog) fclose(fFileStdLog);
}


/** */
void AnaOptions::ReadEnvVars()
{
   if (getenv("VBASYM_DIR")) {
      fEnvVars["VBASYM_DIR"] = getenv("VBASYM_DIR");
   } else {
	   Warning("ReadEnvVars", "Environment variable VBASYM_DIR not set. Will assume current dir ./");
      fEnvVars["VBASYM_DIR"] = ".";
	}

   if (getenv("VBASYM_RESULTS_DIR")) {
      fEnvVars["VBASYM_RESULTS_DIR"] = getenv("VBASYM_RESULTS_DIR");
	} else {
	   Warning("ReadEnvVars", "Environment variable VBASYM_RESULTS_DIR not set. Will assume current dir ./");
      fEnvVars["VBASYM_RESULTS_DIR"] = ".";
	}

   fUserGroup = *gSystem->GetUserInfo();
}


/** */
void AnaOptions::MakeOutDir()
{
   if (GetResultsDir().size() > 200) {
      Error("MakeOutDir", "Output directory name is too long");
      exit(EXIT_FAILURE);
   }

   if (gSystem->mkdir(GetResultsDir().c_str()) < 0) {
      Warning("MakeOutDir", "Directory %s already exists", GetResultsDir().c_str());
   } else {
      Info("MakeOutDir", "Created directory %s", GetResultsDir().c_str());
      gSystem->Chmod(GetResultsDir().c_str(), 0775);
   }
}


string AnaOptions::GetResultsDir() const
{
   return fEnvVars.find("VBASYM_RESULTS_DIR")->second + "/" + fListName;
}


string AnaOptions::GetVbAsymDir()      const { return fEnvVars.find("VBASYM_DIR")->second; }
string AnaOptions::GetSuffix()         const { return !fSuffix.empty() ? "_" + fSuffix : "" ; }
string AnaOptions::GetImageDir()       const { return GetResultsDir() + "/images" + GetSuffix(); }
string AnaOptions::GetStdLogFileName() const { return GetResultsDir() + "/" + fFileStdLogName + GetSuffix() + ".log"; }
string AnaOptions::GetRootFileName()   const { return GetResultsDir() + "/" + fOutFileName + GetSuffix() + ".root"; }
string AnaOptions::GetListFileName()   const { return GetVbAsymDir() + "/runlists/" + fListName; }
EBosonType AnaOptions::GetBosonType()  const { return fBosonType; }
bool AnaOptions::SaveGraphs()          const { return fSaveGraphs; }
int AnaOptions::McType()               const { return fMcType; }
bool AnaOptions::IsMc()                const { return fIsMc; }
bool AnaOptions::IsMcWp()              const { return fIsMcWp; }
uint32_t AnaOptions::GetMaxEventsUser() const { return fMaxEventsUser; }
float AnaOptions::GetTracksPtMin()     const { return fTracksPtMin; }


/** */
void AnaOptions::ProcessOptions(int argc, char **argv)
{
   int          option_index = 0;
   stringstream sstr;

   static struct option long_options[] = {
      {"log",                 optional_argument,   NULL,   'l'},
      {"sfx",                 required_argument,   NULL,   AnaOptions::OPTION_SUFFIX},
      {"list",                required_argument,   NULL,   'f'},
      {"jets",                no_argument,         NULL,   'j'},
      {"jet-pt-min",          required_argument,   NULL,   AnaOptions::OPTION_JETS_PT_MIN},
      {"jpm",                 required_argument,   NULL,   AnaOptions::OPTION_JETS_PT_MIN},
      {"tpm",                 required_argument,   NULL,   AnaOptions::OPTION_TRACKS_PT_MIN},
      {"run",                 required_argument,   NULL,   AnaOptions::OPTION_RHIC_RUN_ID},
      {"mctype",              required_argument,   NULL,   AnaOptions::OPTION_MC_TYPE},
      {"mc",                  no_argument,         NULL,   'm'},
      {"mcwp",                no_argument,         NULL,   'mwp'},
      {"wboson",              no_argument,         NULL,   'w'},
      {"zboson",              no_argument,         NULL,   'z'},
      {NULL, 0, NULL, 0}
   };

   int c;

   while ((c = getopt_long(argc, argv, "?hl::f:n:jr:mwz", long_options, &option_index)) != -1)
   {
      switch (c) {

      case '?':
      case 'h':
         PrintUsage();
         exit(EXIT_FAILURE);

      case 'l':
         fFileStdLogName = (optarg != 0 ? optarg : "");
         break;

      case 'f':
         SetListName(optarg);
         Info("ProcessOptions", "Found fListName: %s", optarg);
         break;

      case 'n':
         fMaxEventsUser = atoi(optarg);
         Info("ProcessOptions", "Found fMaxEventsUser: %d", fMaxEventsUser);
         break;

      case 'j':
         fDoReconstructJets = kTRUE;
         break;

      case AnaOptions::OPTION_TRACKS_PT_MIN:
         sstr.clear();
         sstr.str(string(optarg));
         sstr >> fTracksPtMin;
         Info("ProcessOptions", "Found fTracksPtMin: %f", fTracksPtMin);
         break;

      case AnaOptions::OPTION_JETS_PT_MIN:
         sstr.clear();
         sstr.str(string(optarg));
         sstr >> fJetPtMin;
         Info("ProcessOptions", "Found fJetPtMin: %f", fJetPtMin);
         break;

      case 'r':
      case AnaOptions::OPTION_RHIC_RUN_ID:
         sstr.clear();
         sstr.str(string(optarg));
         sstr >> fRhicRunId;
         Info("ProcessOptions", "Found fRhicRunId: %d", fRhicRunId);
         break;

      case AnaOptions::OPTION_MC_TYPE:
         sstr.clear();
         sstr.str(string(optarg));
         sstr >> fMcType;
         Info("ProcessOptions", "Found fMcType: %d", fMcType);
         // fMcType = 0 -> data
         // fMcType = 1 -> W+ MC
         // fMcType = 2 -> W- MC
         // fMcType > 3 -> Any other MC
         break;

      case 'm':
         fIsMc = kTRUE;
         break;

      case 'mwp':
         fIsMc   = kTRUE;
         fIsMcWp = kTRUE;
         break;

      case 'w':
         fBosonType = kWBoson;
         break;

      case 'z':
         fBosonType = kZBoson;
         break;

      default:
         Error("ProcessOptions", "Unknown option provided");
         PrintUsage();
         exit(0);
         break;
      }
   }
}


/** */
void AnaOptions::VerifyOptions()
{
   // The file list must be specified
   if (fListName.empty()) {
      Error("VerifyOptions", "File with input list files must be specified");
      PrintUsage();
      exit(EXIT_FAILURE);
   }

   if (fRhicRunId < 9 || (fRhicRunId > 13 && fRhicRunId != 131 && fRhicRunId != 132) ) {
      Error("VerifyOptions", "Don't know anything about RHIC run %d. Try 9 <= value <= 13  ", fRhicRunId);
      PrintUsage();
      exit(EXIT_FAILURE);
   }
}


/** */
void AnaOptions::Print(const Option_t* opt) const
{
   Info("Print", "Print members:");
   PrintAsPhp();
}


/** */
void AnaOptions::PrintAsPhp(FILE *f) const
{
   fprintf(f, "$rc['fOutFileName'] = \"%s\";\n", fOutFileName.c_str());
   fprintf(f, "$rc['fSuffix']      = \"%s\";\n", fSuffix.c_str());
   fprintf(f, "$rc['fModes']       = %#010x;\n", fModes);
   fprintf(f, "$rc['fAnaDateTime'] = %u;\n",     (UInt_t) fAnaDateTime);
   fprintf(f, "$rc['fAnaTimeReal'] = %f;\n",     fAnaTimeReal);
   fprintf(f, "$rc['fAnaTimeCpu']  = %f;\n",     fAnaTimeCpu);

   stringstream ssEnvs("");

   ssEnvs << "array(";

   for (Str2StrMap::const_iterator ienv=fEnvVars.begin(); ienv!=fEnvVars.end(); ienv++) {
      ssEnvs << "'" << ienv->first << "'"  << " => " << "\"" << ienv->second << "\"";
      ssEnvs << (ienv != (--fEnvVars.end()) ? ", " : "");
   }

   ssEnvs << ")";

   fprintf(f, "$rc['fEnvVars']             = %s;\n", ssEnvs.str().c_str());

   fprintf(f, "$rc['fFileStdLogName']      = \"%s\";\n", fFileStdLogName.c_str());
   fprintf(f, "$rc['fFlagCopyResults']     = %d;\n", fFlagCopyResults);
   fprintf(f, "$rc['fFlagUpdateDb']        = %d;\n", fFlagUpdateDb);

   fprintf(f, "$rc['fUserGroup_fUser']     = \"%s\";\n", fUserGroup.fUser.Data());
   fprintf(f, "$rc['fUserGroup_fRealName'] = \"%s\";\n", fUserGroup.fRealName.Data());

   fprintf(f, "\n");
}


/** */
void AnaOptions::PrintUsage()
{
   cout << endl;
   cout << "Options:" << endl;
   cout << " -h, -?                               : Print this help" << endl;
   cout << " -l, --log=[filename]                 : Optional log file to redirect stdout and stderr" << endl;
   cout << " -r, -f, --list                       : Input list file" << endl;
   cout << " -m, --mc                             : Process input as monte-carlo" << endl;
   cout << " -mwp, --mcwp                         : Process input as W+ monte-carlo" << endl;
   cout << endl;
}
