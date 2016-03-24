#ifndef AnaOptions_h
#define AnaOptions_h

#include <iostream>
#include <bitset>
#include <string>
#include <sstream>
#include <sys/stat.h>
#include <stdint.h>

#include "TObject.h"
#include "TSystem.h"

#include "Globals.h"


/**
 * Processes and controls user options provided in the command line.
 */
class AnaOptions : public TObject
{
public:

   enum Flag   {FLAG_CREATE_THUMBS     = 0x10000800};

   enum Option {OPTION_SUFFIX          = 0x20000002,
                OPTION_JETS_PT_MIN     = 0x20000003,
                OPTION_RHIC_RUN_ID     = 0x20000004,
                OPTION_TRACKS_PT_MIN   = 0x20000005,
                OPTION_MC_TYPE         = 0x20000006};

public:

   std::string      fSuffix;            //< Additional unique identifier for analysis job
   uint64_t         fModes;
   bool             fDoReconstructJets;
   float            fTracksPtMin;
   float            fJetPtMin;         
   unsigned short   fRhicRunId;         //< RHIC run id, e.g. 9, 11, 12, 13, ...
   time_t           fAnaDateTime;       //< Date/time when data analysis started
   double           fAnaTimeReal;       //< Time in seconds to process input raw file
   double           fAnaTimeCpu;        //< Time in seconds to process input raw file
   Str2StrMap       fEnvVars;           //< Map with user defined environment variables
   FILE            *fFileStdLog;        //!
   std::string      fFileStdLogName;
   bool             fFlagCopyResults;
   bool             fFlagUpdateDb;
   UserGroup_t      fUserGroup;

   AnaOptions();
   virtual ~AnaOptions();

   std::string  GetResultsDir() const;
   std::string  GetVbAsymDir() const;
   std::string  GetSuffix() const;
   std::string  GetImageDir() const;
   std::string  GetStdLogFileName() const;
   std::string  GetRootFileName() const;
   std::string  GetListFileName() const;
   EBosonType   GetBosonType() const;
   float        GetTracksPtMin() const;
   bool         SaveGraphs() const;
   bool         IsMc() const;
   int          McType() const;
   bool         IsMcWp() const;
   void         SetListName(std::string listName) { fListName = listName; }
   std::string  GetListName() const { return fListName; }
   uint32_t     GetMaxEventsUser() const;
   virtual void ProcessOptions(int argc, char **argv);
   virtual void VerifyOptions();
   void         Print(const Option_t* opt="") const;
   void         PrintAsPhp(FILE *f=stdout) const;
   virtual void PrintUsage();

protected:

   void ReadEnvVars();
   void MakeOutDir();

   std::string fListName;
   std::string fOutFileName;
   uint32_t    fMaxEventsUser;
   bool        fSaveGraphs;
   int         fMcType;   
   bool        fIsMc;
   bool        fIsMcWp;
   EBosonType  fBosonType;

   ClassDef(AnaOptions, 1)
};

#endif
