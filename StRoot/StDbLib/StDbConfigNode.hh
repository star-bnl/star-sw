#ifndef STDBCONFIGNODE_HH
#define STDBCONFIGNODE_HH

//#include "StDbTableComponent.h"
#include "StDbDefs.hh"
class StDbTableComponent;

class StDbFactoryI;
class TableIter;


#ifndef __CINT__
#include <list>
#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StDbTableComponent*, allocator<StDbTableComponent*> > TableList;
#else
typedef list<StDbTableComponent*> TableList;
#endif
#endif
#ifdef __CINT__
class TableList;
#endif

class TableIter;

class StDbConfigNode {

 friend class TableIter;

private:

 char* mnodeName;
 char* mconfigName;
 StDbType mdbType;
 StDbDomain mdbDomain;

 StDbConfigNode* mfirstChildNode;
 StDbConfigNode* mnextNode;
 StDbConfigNode* mparentNode;

 
protected:

  void zeroNodes(){
    mfirstChildNode = 0;
    mnextNode = 0;
    mparentNode = 0;
    mhasData = false;
    mnodeName = 0;
    mconfigName = 0;
    mfactory = 0;
  };


 StDbFactoryI* mfactory;
 TableList mTables;
 bool mhasData;
 void deleteTables();

public:

  StDbConfigNode(): mnodeName(0), mconfigName(0) { zeroNodes();};
  StDbConfigNode(StDbType type, StDbDomain domain, const char* nodeName, const char* configName);
  StDbConfigNode(StDbConfigNode* parent, const char* nodeName, const char* configName);

  virtual ~StDbConfigNode() {if(mconfigName) delete [] mconfigName;
                     if(mnodeName) delete [] mnodeName;};

  virtual void setName(const char* name); 
  virtual char * getName() const ;
  virtual void setConfigName(const char* name); 
  virtual char * getConfigName() const ;
  virtual void setDbType(StDbType type) {mdbType = type;};
  virtual StDbType getDbType() const { return mdbType;};
  virtual void setDbDomain(StDbDomain domain) {mdbDomain = domain;};
  virtual StDbDomain getDbDomain() const { return mdbDomain;};

  virtual void resetConfig(const char* configName);
  
  virtual void setNextNode(StDbConfigNode* node) { mnextNode = node;}; 
  virtual void setParentNode(StDbConfigNode* node); 
  virtual void setFirstChildNode(StDbConfigNode* node); 
  virtual void appendNode(StDbConfigNode* node); 

  virtual void deleteTree();
  virtual void buildTree();

  virtual StDbConfigNode* getNextNode() const { return mnextNode;} ;
  virtual StDbConfigNode* getParentNode() const {return mparentNode;}; 
  virtual StDbConfigNode* getFirstChildNode() const {return mfirstChildNode;}; 
 
  virtual bool hasChildren();
  virtual bool hasData();
  virtual void printTree();

  virtual void addTable(const char* tableName, int version, int elementID);
  virtual TableIter* getTableIter();
  virtual StDbConfigNode* findConfigNode(StDbType type, StDbDomain domain);

  virtual bool isNode(StDbType type, StDbDomain domain);

}; 

inline
bool StDbConfigNode::isNode(StDbType type, StDbDomain domain){
bool retVal = false;
if(mdbDomain == domain && mdbType == type)retVal = true;
return retVal;
}

inline
bool StDbConfigNode::hasData(){ return mhasData;};

inline
bool StDbConfigNode::hasChildren(){
bool retVal = false;
if(mfirstChildNode)retVal=true;
return retVal;
}

#endif










