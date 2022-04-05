/***************************************************************************
 *
 * $Id: StMuChainMaker.h,v 1.12 2004/05/02 04:10:13 perev Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
/** @class StMuChainMaker
    Helper class to create a TChain of Root files
*/

#ifndef StMuChainMaker_hh
#define StMuChainMaker_hh


#include <vector>
#include <utility>
#include <string>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif


typedef pair<string,int> StMuStringIntPair;
typedef vector<StMuStringIntPair> StMuStringIntPairVector;
typedef vector<StMuStringIntPair>::iterator StMuStringIntPairVectorIterator;

class TChain;
class StMuDbReader;

class StMuChainMaker  {
public:
    StMuChainMaker(const char* name="MuDst");
    virtual ~StMuChainMaker();
    static void setUseFileCatalog(const char* connection="mysql://duvall.star.bnl.gov:3306/FileCatalog_BNL") {
	mSQLConnection = string(connection);
    }
    
    TChain* make(string dir, string file, string filter, int maxFiles=10);
    void fromFileCatalog(string file);
    void fromList(string file);
    void fromFile(string file);
    void fromDir(string dir);
    
    void subFilter(string filter);
    string basename(string);
    string dirname(string);
    string buildFileName(string dir, string fileName, string extention);

protected:
    static string mSQLConnection;
    TChain *mChain;
    StMuDbReader* mDbReader;
    string mSubFilters[100];
    string mTreeName;
    int mFileCounter;
    int mMaxFiles;
    StMuStringIntPairVector mFileList;
    bool pass(string file, string*  filters);
    void add(StMuStringIntPairVector);
    void add(StMuStringIntPair);
    
    ClassDef(StMuChainMaker,0)
	};

#endif

/***************************************************************************
 *
 * $Log: StMuChainMaker.h,v $
 * Revision 1.12  2004/05/02 04:10:13  perev
 * private => protected
 *
 * Revision 1.11  2003/04/21 18:18:53  laue
 * Modifications for the new scheduler implementation:
 * - the filenames and the number of events per files are now supplied
 * - files on local disk are given in the rootd format
 *
 * Revision 1.10  2003/04/15 16:17:11  laue
 * Minor changes to be able to filter MuDst.root files. The StMuDstFilterMaker
 * is just an example of how to do it. It has be be customized (spoilers,
 * chrome weels, etc)  by the user.
 *
 * Revision 1.9  2003/01/23 21:59:50  laue
 * Modification to compile on Solaris.
 *
 * Revision 1.8  2002/12/19 19:44:25  laue
 * update to read number of events from database, for files ending with .list
 *
 * Revision 1.7  2002/11/18 14:29:31  laue
 * update for Yuri's new StProbPidTraits
 *
 * Revision 1.6  2002/05/04 23:56:29  laue
 * some documentation added
 *
 * Revision 1.5  2002/04/17 21:04:16  laue
 * minor updates
 *
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
