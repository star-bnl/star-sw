/***************************************************************************
 *
 * $Id: StMuChainMaker.h,v 1.4 2002/04/15 22:38:11 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
#ifndef StMuChainMaker_hh
#define StMuChainMaker_hh

#include <string>
class TChain;
class StMuDbReader;

class StMuChainMaker  {
 public:
  StMuChainMaker(const char* name="MuDst");
  virtual ~StMuChainMaker();

  string** subFilter(string filter);
  TChain* make(string dir, string file, string filter, int maxFiles=10);
  TChain* fromList(string file,int maxFiles);
  TChain* fromFile(string file, int maxFiles);
  TChain* fromDir(string dir, int maxFiles);

  string basename(string);
  string dirname(string);
  string buildFileName(string dir, string fileName, string extention);

 private:
  StMuDbReader* mDbReader;
  string** mSubFilters;
  string mTreeName;
  bool pass(string file, string**  filters);

ClassDef(StMuChainMaker,0)
};

#endif

/***************************************************************************
 *
 * $Log: StMuChainMaker.h,v $
 * Revision 1.4  2002/04/15 22:38:11  laue
 * made destructors virtual
 *
 * Revision 1.3  2002/04/15 22:29:28  laue
 * updates
 *
 * Revision 1.2  2002/04/11 14:19:30  laue
 * - update for RH 7.2
 * - decrease default arrays sizes
 * - add data base readerfor number of events in a file
 *
 * Revision 1.1  2002/04/01 22:42:30  laue
 * improved chain filter options
 *
 *
 **************************************************************************/
