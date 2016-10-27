#ifndef VbAnaOptions_h
#define VbAnaOptions_h

#include <iostream>
#include <bitset>
#include <string>
#include <sstream>
#include <sys/stat.h>

#include "AnaOptions.h"
#include "Globals.h"

#include <boost/program_options/options_description.hpp>
#include <boost/program_options/variables_map.hpp>
#include <boost/program_options/parsers.hpp>

namespace po = boost::program_options;


/**
 * Processes and verifies input options provided by the user in the command
 * line.
 */ 
class VbAnaOptions : public AnaOptions
{
public:

   enum Flag   {FLAG_CREATE_THUMBS     = 0x10000800};
   enum Option {OPTION_SUFFIX          = 0x20000002};

   VbAnaOptions();

   std::string  GetRootFileName() const;
   double       GetFitSinePhase() const;
   double       GetFitSineOffset() const;
   bool         UseOtherSolution() const;
   bool         GetIsMc() const;
   bool         GetOldStyle() const;
   virtual void ProcessOptions(int argc, char **argv);
   void         Print(const Option_t* opt="") const;
   void         PrintAsPhp(FILE *f=stdout) const;

protected:

   po::options_description fOptions;
   po::variables_map       fOptionsValues;
   double                  fFitSinePhase;
   double                  fFitSineOffset;
   bool                    fUseOtherSolution;
   bool                    fOldStyle;
};

#endif
