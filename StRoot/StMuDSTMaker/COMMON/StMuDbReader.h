/***************************************************************************
 *
 * $Id: StMuDbReader.h,v 1.4 2002/05/04 23:56:30 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/
#ifndef StMuDbReader_hh
#define StMuDbReader_hh

#include <string>
#include <vector>
#include <list>

/** 
    @class StMuDbReader 
    Singleton class to create and read a poor man's database  holding the 
    number of events per MuDst.root file
*/
class StMuDbReader  {
 public:
    static StMuDbReader* instance();
    static StMuDbReader* Instance();

    int createDB(const char* dbFile, const char* inputList); 
    int addDb(const char* dbFile); 
    void showDb(); 
    void sortDb(); 
    int entriesDb(); 
    int entries(const char* file); 
 protected:
    StMuDbReader();
    virtual ~StMuDbReader();
 private:
    static StMuDbReader* _instance;
    
    /// the internal database, a vector containing pairs of file names and number of events */
    vector< pair<string,int> > mDb; 
    vector< pair<string,int> >::iterator iter; 
    
    ClassDef(StMuDbReader,0)
};

#endif

/// number of entries in internal data base
inline int StMuDbReader::entriesDb() { return mDb.size(); }

/***************************************************************************
 *
 * $Log: StMuDbReader.h,v $
 * Revision 1.4  2002/05/04 23:56:30  laue
 * some documentation added
 *
 * Revision 1.3  2002/04/17 21:04:16  laue
 * minor updates
 *
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
