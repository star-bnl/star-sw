/***************************************************************************
 *
 * $Id: StDbConfigNodeImpl.cc,v 1.11 2016/05/25 20:40:01 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Payload Implementatoin of pure virtual Node (directory) class
 *               StDbConfigNode --> to hold list of dbtables
 *
 ***************************************************************************
 *
 * $Log: StDbConfigNodeImpl.cc,v $
 * Revision 1.11  2016/05/25 20:40:01  dmitry
 * coverity - reverse_inull
 *
 * Revision 1.10  2016/05/24 20:26:48  dmitry
 * coverity - unreachable delete loop suppression
 *
 * Revision 1.9  2011/11/28 17:03:08  dmitry
 * dbv override support in StDbLib,StDbBroker,St_db_Maker
 *
 * Revision 1.8  2007/03/08 22:07:22  deph
 * Fixed small memory leak in removeTable
 *
 * Revision 1.7  2004/01/15 00:02:25  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.6  2003/09/16 22:44:17  porter
 * got rid of all ostrstream objects; replaced with StString+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.5  2003/09/02 17:57:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.4  2003/01/10 04:19:20  porter
 * added feature of getting timestamp list (but no data) for a table.
 * fixed 2 features sometimes used in online in query-by-whereclause.
 * removed a stray 'cout' in a routine that is rarely accessed
 *
 * Revision 1.3  2001/10/26 16:35:28  porter
 * improved directory search
 *
 * Revision 1.2  2001/02/09 23:06:24  porter
 * replaced ostrstream into a buffer with ostrstream creating the
 * buffer. The former somehow clashed on Solaris with CC5 iostream (current .dev)
 *
 * Revision 1.1  2001/01/22 18:37:52  porter
 * Update of code needed in next year running. This update has little
 * effect on the interface (only 1 method has been changed in the interface).
 * Code also preserves backwards compatibility so that old versions of
 * StDbLib can read new table structures.
 *  -Important features:
 *    a. more efficient low-level table structure (see StDbSql.cc)
 *    b. more flexible indexing for new systems (see StDbElememtIndex.cc)
 *    c. environment variable override KEYS for each database
 *    d. StMessage support & clock-time logging diagnostics
 *  -Cosmetic features
 *    e. hid stl behind interfaces (see new *Impl.* files) to again allow rootcint access
 *    f. removed codes that have been obsolete for awhile (e.g. db factories)
 *       & renamed some classes for clarity (e.g. tableQuery became StDataBaseI
 *       and mysqlAccessor became StDbSql)
 *
 *
 **************************************************************************/
#include "stdb_streams.h"
#include <string.h>
#include "StDbConfigNodeImpl.hh"
#include "StDbManagerImpl.hh" // could be StDbManager.hh but for dbEnvList def
#include "StDbTableIterImpl.hh"
#include "StDbElementIndex.hh"

#define __CLASS__ "StDbConfigNodeImpl"

////////////////////////////////////////////////////////////////

StDbConfigNodeImpl::StDbConfigNodeImpl(StDbConfigNode* parent, const char* nodeName, const char* configName): StDbConfigNode(parent,nodeName,configName), mindexRef(-1) {
  melementIndex = new StDbElementIndex();
  setConfigured(false);
  mcanRollBack=false;
  updateDbInfo();  
}

////////////////////////////////////////////////////////////////

StDbConfigNodeImpl::StDbConfigNodeImpl(StDbConfigNode* parent, StDbNode& node): StDbConfigNode(parent,node), mindexRef(-1) {
  melementIndex = new StDbElementIndex();
  updateDbInfo();
}

////////////////////////////////////////////////////////////////

StDbConfigNodeImpl::StDbConfigNodeImpl(StDbType type, StDbDomain domain, const char* nodeName, const char* configName): StDbConfigNode(type,domain,nodeName,configName), mindexRef(-1) {
  melementIndex = new StDbElementIndex();
  updateDbInfo();
}

////////////////////////////////////////////////////////////////

StDbConfigNodeImpl::~StDbConfigNodeImpl(){
deleteTables();
delete melementIndex;
};

////////////////////////////////////////////////////////////////
void
StDbConfigNodeImpl::updateDbInfo(){ 
#define __METHOD__ "updateDbInfo()"

 if(mparentNode)melementIndex->addElementIndex(mparentNode->getElementIndex());

  // If StarDb Type, then name holds map to real Db type
  // while the domain may still be StarDb
  // Else if StarDb domain, then name holds map to real domain type
  // Else both type & domain are that of parent (which maybe User defined)

  misConfigured = false;
  if(mparentNode){ //top level node will have type&domain set by the manager.
   if(mdbType==dbStDb){
    mdbType=StDbManager::Instance()->getDbType(mname);
   } else if(mdbDomain==dbStar) {
    mdbDomain=StDbManager::Instance()->getDbDomain(mname);
   }
  }

  if(!mnodeType) mnodeType=mstrDup("DB");
  // replace nodetype of DB to Config moving from 1 db to another
  // & allow overwriting of versionkey with environment variable
  if(strcmp(mnodeType,"DB")==0){
     setIsDbNode(true);
     delete [] mnodeType; 
     mnodeType=mstrDup("Config");
     setBranchID(0);
     char* version=StDbManager::Instance()->getExternalVersion(mdbType,mdbDomain);
     if(version && strcmp(version,mversion)){
       StString nm;
       nm<<" Overriding Key="<<mversion<<" with Environment Var Key="<<version;
       nm<<" for DataBase=";
       nm<<StDbManager::Instance()->printDbName(mdbType,mdbDomain);
       StDbManager::Instance()->printInfo((nm.str()).c_str(),dbMWarn,__LINE__,__CLASS__,__METHOD__);
       setVersion(version);
     }
  }

#undef __METHOD__
}

////////////////////////////////////////////////////////////////
int
StDbConfigNodeImpl::buildTree(int opt){
#define __METHOD__ "buildTree(int opt)"

 StDataBaseI *db = StDbManager::Instance()->findDb(mdbType, mdbDomain);

 if(!db)
  return StDbManager::Instance()->printInfo(" No DB found for Node=",mname,dbMErr,__LINE__,__CLASS__,__METHOD__);

 if(db->QueryDb(this)){
  // check if we should connect via environment variables
  if(isDbNode() && mdbDomain==dbStar){
    dbEnvList* elist = StDbManager::Instance()->getEnvList(mname);
    if(elist){
      addChildren(elist);
      delete elist;
    }
  }
  // load descriptors & element structure as needed 
  updateDbTables(opt);
  if(mfirstChildNode)mfirstChildNode->buildTree(opt);
 }

 if(mnextNode)mnextNode->buildTree(opt);

 return 1;
#undef __METHOD__
}

////////////////////////////////////////////////////////////////////////
void StDbConfigNodeImpl::updateDbTables(int opt){

 if(!mhasData)return;
 StDataBaseI* db=StDbManager::Instance()->findDb(mdbType, mdbDomain);
 if(!db) return;

 StDbTableIter* itr = getStDbTableIter();
 StDbTable* table;
 while((itr) && !(itr->done())){
    table=itr->next();
    updateDbTable(table,opt);
   }
 if(itr)delete itr;

}
////////////////////////////////////////////////////////////////////////
void StDbConfigNodeImpl::updateDbTable(StDbTable* table, int opt){

 if(!table) return;
 StDataBaseI* db=StDbManager::Instance()->findDb(mdbType, mdbDomain);
 if(!db)return;

 int  nRows; 
 char* elements=table->printElementName();
 if(elements){
    int* eList=db->selectElements(elements,melementIndex,nRows);
    if(eList){
       table->setElementID(eList,nRows);
       delete [] eList;
    }
  } 
 if(!opt) db->QueryDescriptor(table);
}

////////////////////////////////////////////////////////////////////////

StDbTable*
StDbConfigNodeImpl::addDbTable(const char* tableName, const char* version){

  // just like addTable but also loads the descriptor from the database
  StDbTable*   table = addTable(tableName,version);
  updateDbTable(table);
  return table;
}

////////////////////////////////////////////////////////////////////////

StDbTable*
StDbConfigNodeImpl::addTable(const char* tableName, const char* version){

  StDbTable* table = 0;
  table = StDbManager::Instance()->newDbTable(mdbName,tableName);
  if (table) { 
	mTables.push_back(table);
	table->setVersion((char*)version);
 	table->setNodeType("table");
  }
  StDataBaseI* db = StDbManager::Instance()->findDb(mdbType, mdbDomain);
  if(db && table && db->QueryDb((StDbNode*)table)) { mhasData = true; }
  return table;
}

////////////////////////////////////////////////////////////////
StDbTable*
StDbConfigNodeImpl::addTable(StDbNode* node){

  StDbTable* table = StDbManager::Instance()->newDbTable(node);
  if(table){
     mTables.push_back(table);
     mhasData=true;
  }

return table;
}

////////////////////////////////////////////////////////////////
StDbTable*
StDbConfigNodeImpl::findTable(const char* name, const char* subPath){

if(subPath && (strcmp(subPath,"/")==0))return findLocalTable(name);

StDbConfigNode* node = findConfigNode(subPath);
if(node)return node->findLocalTable(name);

// if we got here then we can't find it
 StDbTable* table=0;
return table;
}

////////////////////////////////////////////////////////////////

StDbTable*
StDbConfigNodeImpl::findLocalTable(const char* name){

  TableList::iterator itr;
  StDbTable* table=0;
    for(itr = mTables.begin(); itr!=mTables.end(); ++itr){
      if((*itr)->checkName(name)){
	table=*itr;
        break;
      }
    }
return table;
}

////////////////////////////////////////////////////////////////
void
StDbConfigNodeImpl::removeTable(StDbTable* table){
 
  if(!table)return;
  TableList::iterator itr;
  StDbTable* myTable=0;

    for(itr = mTables.begin(); itr!=mTables.end(); ++itr){
        myTable=*itr;
        if(myTable && compareTables(myTable,table)){
                 delete *itr;
                 mTables.erase(itr);   break;
        }
        myTable=0;
    }
}

////////////////////////////////////////////////////////////////
void
StDbConfigNodeImpl::setTablesFlavor(const char* flavor){

  TableList::iterator itr;
  for(itr = mTables.begin(); itr!=mTables.end(); ++itr){
      if((*itr))(*itr)->setFlavor(flavor);
   }
}

////////////////////////////////////////////////////////////////
void
StDbConfigNodeImpl::setTablesProdTime(unsigned int ptime){

  TableList::iterator itr;
  for(itr = mTables.begin(); itr!=mTables.end(); ++itr){
      if((*itr))(*itr)->setProdTime(ptime);
   }
}

////////////////////////////////////////////////////////////////
void
StDbConfigNodeImpl::setTablesProdTimeOverride(unsigned int ptime, char* dbType, char* dbDomain) {
  TableList::iterator itr;
  for(itr = mTables.begin(); itr!=mTables.end(); ++itr) {
    if((*itr))(*itr)->setProdTime(ptime);
  }
}

////////////////////////////////////////////////////////////////
bool
StDbConfigNodeImpl::compareTables(StDbTable* tab1, StDbTable* tab2){

bool retVal=false;
// compare name & version
 if((strcmp(tab1->printName(),tab2->printName())!=0)) return retVal;
 if((strcmp(tab1->printVersion(),tab2->printVersion())!=0)) return retVal;
// compare each row identifier
    int nRows1; int nRows2;
    int* elements1 = tab1->getElementID(nRows1);
    int* elements2 = tab2->getElementID(nRows2);
    if(nRows1 != nRows2) return retVal;
    int i;
    for(i=0;i<nRows1;i++)if(elements1[i] != elements2[i])break;    
    if(i==nRows1)retVal=true;
return retVal;
}

///////////////////////////////////////////////////////////////
StDbElementIndex* StDbConfigNodeImpl::getElementIndex(){return melementIndex; }

///////////////////////////////////////////////////////////////
void
StDbConfigNodeImpl::setElementIndexInfo(const char* indexName, int indexID){
  mindexRef=melementIndex->addNameValuePair(indexName,indexID);
}

////////////////////////////////////////////////////////////////
void
StDbConfigNodeImpl::getElementIndexInfo(char*& indexName, int& indexID){

  if(mindexRef < 0){ indexName=0; return; }
    indexName=melementIndex->getIndexName(mindexRef);
    indexID  =melementIndex->getIndexVal(mindexRef);
}  

////////////////////////////////////////////////////////////////
int
StDbConfigNodeImpl::getNumIndeces() const { return melementIndex->getNumIndeces();}

////////////////////////////////////////////////////////////////
StDbTableIter*
StDbConfigNodeImpl::getStDbTableIter(){
  StDbTableIter* itr = new StDbTableIterImpl(this);
  return itr;
}


////////////////////////////////////////////////////////////////
void
StDbConfigNodeImpl::resetConfig(const char* configName, int opt){

if(mfirstChildNode)mfirstChildNode->deleteTree();
if(mhasData)deleteTables();
setVersion(configName);
buildTree(opt);
}


////////////////////////////////////////////////////////////////
void
StDbConfigNodeImpl::deleteTables(){
  for( auto &it : mTables ) delete it;
  mTables.clear();
  mhasData=false;
}

///////////////////////////////////////////////////////////////////////
void
StDbConfigNodeImpl::addChildren(dbEnvList* elist){
#define __METHOD__ "addChildren(dbEnvList*)"

  for(int i=0; i<elist->num; i++){
    char* id=strstr(elist->envVar[i],"_");
    if(id){
      id++;
      if(strcmp(id,mname) && !findChildConfigNode(id)){
        StString nm;
        nm<<" Adding DataBase="<<elist->envVar[i]<<" with KEY=";
        nm<<elist->envDef[i]<<" from Environment variable definition";
	StDbManager::Instance()->printInfo((nm.str()).c_str(),dbMWarn,__LINE__,__CLASS__,__METHOD__);    
//VP        delete [] nMes;
            new StDbConfigNodeImpl(this,id,elist->envDef[i]);
      }
    }
  }
#undef __METHOD__
}      

/////////////////////////////////////////////////////////////////
void
StDbConfigNodeImpl::printTables(int depth){

  if(StDbManager::Instance()->IsVerbose()){
    StString os;
    for(int k=0;k<depth;k++)os<<" ";
    
    string pdepth=os.str();
   TableList::iterator itr;
   for(itr = mTables.begin(); itr!=mTables.end(); ++itr){
     cout<<pdepth<<"Table="<<(*itr)->printName()<<", Version="<<(*itr)->printVersion();
      int nRows;
      int* eIDs=(*itr)->getElementID(nRows);
      cout <<", numRows="<<nRows;
      if(nRows==1)cout<<", elementID="<<eIDs[0];
      cout<<endl;
    }
//VP    delete [] pdepth;
  }
}

void
StDbConfigNodeImpl::printNumberStats() {
#define __METHOD__ "printNumberStats()"

unsigned int numNodes, numTables, numBytes;
 numNodes=numTables=numBytes=0;
 getNumberStats(numNodes,numTables, numBytes);
 double kbs = ((double)numBytes)/1000.0;
 StString cos;
 cos<<"******************** Number Stats ******************** "<<stendl;
 cos<<"Total Number of Nodes        = "<<numNodes              <<stendl;
 cos<<"Total Number of Tables       = "<<numTables             <<stendl;
 cos<<"Total Size of Data in Tables = "<<kbs<<" kBytes"        <<stendl;
 cos<<"******************************************************" <<stendl;

 StDbManager::Instance()->printInfo((cos.str()).c_str(),dbMConnect,__LINE__,__CLASS__,__METHOD__);
//VP delete [] nstats;

#undef __METHOD__
}

///////////////////////////////////////////////////////////////////////
void
StDbConfigNodeImpl::getNumberStats(unsigned int& numNodes, unsigned int& numTables, unsigned int& numBytes){

 numNodes++;
 StDbTableIter* itr = getStDbTableIter();
 StDbTable* table;
 while((itr) && !(itr->done())){
    table=itr->next();
    numTables++;
    numBytes+=(table->getTableSize()*table->GetNRows());
   }
 if(itr)delete itr;
 if(mfirstChildNode)mfirstChildNode->getNumberStats(numNodes,numTables,numBytes);
 if(mnextNode)mnextNode->getNumberStats(numNodes,numTables,numBytes);
}
#undef __CLASS__

