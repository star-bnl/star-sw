#ifndef STDBTABLECOMPONENT_HH
#define STDBTABLECOMPONENT_HH


#include "StDbDefs.hh"
#include "typeAcceptor.hh"
#include "StDbAccessor.h"
#include "TObject.h"

class StDbTableComponent : public TObject {

protected:

  StDbAccessor maccessor;//!
char* mtableName;

public:

  StDbTableComponent(const char* tableName): mtableName(0) { setTableName(tableName);
                                               maccessor.endTime = -1;}

  StDbTableComponent(StDbTableComponent& table);

  virtual ~StDbTableComponent(){if(mtableName)delete [] mtableName;};

  virtual StDbAccessor getAccessor() const ;
  virtual void setAccessor(StDbAccessor a) ;

  virtual char* getTableName() const;
  virtual void setTableName(const char* name);
  virtual StDbType getDbType() const  ;
  virtual void setDbType(StDbType type) ;
  virtual StDbDomain getDbDomain() const ;
  virtual void setDbDomain(StDbDomain domain); 
  virtual int getRequestTime() const ;
  virtual void setRequestTime(int time) ;
  virtual int getVersion() const ;
  virtual void setVersion(int version) ;
  virtual int getEndTime() const ;
  virtual void setEndTime(int time) ;
  virtual int getBeginTime() const  ;
  virtual void setBeginTime(int time) ;
  virtual int getElementID() const ;
  virtual void setElementID(int id) ; 
  virtual int getSchemaID() const ; 
  virtual void setSchemaID(int id) ; 


  virtual void StreamAccessor(typeAcceptor* accept);

  // these are overloaded in real table class ... should be pure virtual...
  virtual StDbTableComponent* duplicate(){return (new StDbTableComponent(*this));};
  virtual void Streamer(typeAcceptor* accept){};

  ClassDef(StDbTableComponent, 1)
 };

inline 
StDbAccessor StDbTableComponent::getAccessor() const {return maccessor;}

inline 
void StDbTableComponent::setAccessor(StDbAccessor a) {maccessor = a;}


inline 
StDbType StDbTableComponent::getDbType() const  { return maccessor.dbType; }

inline 
void StDbTableComponent::setDbType(StDbType type) {maccessor.dbType = type; }

inline 
StDbDomain StDbTableComponent::getDbDomain() const { return maccessor.dbDomain; }

inline 
void StDbTableComponent::setDbDomain(StDbDomain domain) {maccessor.dbDomain = domain; }

inline 
int StDbTableComponent::getRequestTime() const { return maccessor.requestTime; }

inline 
void StDbTableComponent::setRequestTime(int time) { maccessor.requestTime = time; }

inline 
int StDbTableComponent::getVersion() const { return maccessor.version; }

inline 
void StDbTableComponent::setVersion(int version) {maccessor.version = version; }

inline 
int StDbTableComponent::getEndTime() const { return maccessor.endTime; }

inline 
void StDbTableComponent::setEndTime(int time) {maccessor.endTime = time; }

inline 
int StDbTableComponent::getBeginTime() const  { return maccessor.beginTime; }

inline 
void StDbTableComponent::setBeginTime(int time) {maccessor.beginTime = time; }

inline 
int StDbTableComponent::getElementID() const { return maccessor.elementID; }

inline 
void StDbTableComponent::setElementID(int id) {maccessor.elementID = id; }

inline 
int StDbTableComponent::getSchemaID() const { return maccessor.schemaID; }

inline 
void StDbTableComponent::setSchemaID(int id) {maccessor.schemaID = id; }


#endif









