/***************************************************************************
 *
 * $Id: StMuDbReader.h,v 1.2 2002/04/15 22:38:11 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
#ifndef StMuDbReader_hh
#define StMuDbReader_hh

#include <string>
#include <vector>
#include <list>
//!#include <pair>

/* class FourTuple; */
/* class FourTupleVector; */


class StMuDbReader  {
 public:
  static StMuDbReader* instance();
  static StMuDbReader* Instance();

  int createDB(const char* dbName, const char* inputList);
  int addDb(const char* db);
  void showDb();
  void sortDb();
  int entries(const char* file);
 protected:
  StMuDbReader();
  virtual ~StMuDbReader();
 private:
  static StMuDbReader* _instance;
  //  FourTupleVector* mFourTupleVector;

  vector< pair<string,int> > mDb;
  vector< pair<string,int> >::iterator iter;

  ClassDef(StMuDbReader,0)
};

#endif

/***************************************************************************
 *
 * $Log: StMuDbReader.h,v $
 * Revision 1.2  2002/04/15 22:38:11  laue
 * made destructors virtual
 *
 * Revision 1.1  2002/04/11 14:19:30  laue
 * - update for RH 7.2
 * - decrease default arrays sizes
 * - add data base readerfor number of events in a file
 *
 * Revision 1.1  2002/04/01 22:42:30  laue
 * improved chain filter options
 *
 *
 **************************************************************************/
