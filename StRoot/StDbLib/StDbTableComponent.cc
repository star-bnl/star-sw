#include "StDbTableComponent.h"
#include <string.h>
#include <iostream.h>

ClassImp(StDbTableComponent)

StDbTableComponent::StDbTableComponent(StDbTableComponent& table){
mtableName=0;
setTableName(table.getTableName());
maccessor = table.getAccessor();

}


void
StDbTableComponent::setTableName(const char* name){

  if(mtableName)delete [] mtableName;
  mtableName = new char[strlen(name)+1];
  strcpy(mtableName,name);

}

char* 
StDbTableComponent::getTableName() const { return strdup(mtableName); }

void
StDbTableComponent::StreamAccessor(typeAcceptor* accept){

  //  cout << "stream sID " << endl;
   accept->pass("schemaID",maccessor.schemaID,sizeof(maccessor.schemaID));
   //  cout << "stream bt " << endl;
   accept->pass("beginTime",maccessor.beginTime,sizeof(maccessor.beginTime));
   //  cout << "stream et " << endl;
   accept->pass("endTime",maccessor.endTime,sizeof(maccessor.endTime));
   //  cout << "stream v " << endl;
   accept->pass("version",maccessor.version,sizeof(maccessor.version));
   //  cout << "stream eID " << endl;
   accept->pass("elementID",maccessor.elementID,sizeof(maccessor.elementID));

}







