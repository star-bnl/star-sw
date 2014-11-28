#ifndef StHbtFlowPicoReader_h
#define StHbtFlowPicoReader_h
#include "StHbtMaker/Base/StHbtEventReader.hh"
#include "StHbtMaker/Infrastructure/StHbtString.hh"
#include "flow_pDST.h"
#include <vector>
#include <string.h>
/* http://www.star.bnl.gov/STAR/comp/pkg/dev/StRoot/StHbtMaker/doc/#Reader
outlines the requirements for this type of class.
 */


class StHbtFlowPicoReader : public StHbtEventReader
{
 public:
  StHbtFlowPicoReader(string to_do_list_inp, int nevents);
  ~StHbtFlowPicoReader();
  using StHbtEventReader::Init; // do not hide it
  int Init(); /* read the list of files to be processed */
  StHbtEvent* ReturnHbtEvent(); /* if no StHbtEvent can be returned, 
				   return a null pointer. */
  StHbtString Report();
  int GetNevents(); /* returns events read from input */
  //  int WriteHbtEvent(StHbtEvent*);

 private:
  string to_do_list_inp; // file name with the list of input files
  int crrnt_eventI; // number of c 
  int NEVENTS;   // number of events to analyze as requested by user
  long Nentries;
  int crrnt_entry;
  int NEvt_in_chain;
  int nb; // number of bytes in  the current entry
  int Nbytes; // running sum of nb
  flow_pDST* pDST;
  TChain* DST_Chain;
  vector<string> L_runs;
  ClassDef(StHbtFlowPicoReader,1)
};

inline int StHbtFlowPicoReader::GetNevents() {return NEvt_in_chain;}
#endif
