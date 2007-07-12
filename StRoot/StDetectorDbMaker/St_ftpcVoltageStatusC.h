#ifndef St_ftpcVoltageStatusC_h
#define St_ftpcVoltageStatusC_h

#include "TChair.h"
#include "tables/St_ftpcVoltageStatus_Table.h"

class St_ftpcVoltageStatusC : public TChair {
 public:
  static St_ftpcVoltageStatusC* 	instance();
  ftpcVoltageStatus_st 	*Struct(Int_t i = 0) 	{return ((St_ftpcVoltageStatus*) instance()->Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return instance()->GetNRows();}
  UInt_t 	runNumber(Int_t i = 0) 	        {return Struct(i)->runNumber;}
  UInt_t 	startStatusTime(Int_t i = 0) 	{return Struct(i)->startStatusTime;}
  UInt_t 	endStatusTime(Int_t i = 0) 	{return Struct(i)->endStatusTime;}
  UInt_t 	statusWest(Int_t i = 0) 	{return Struct(i)->statusWest;}
  UInt_t 	statusEast(Int_t i = 0) 	{return Struct(i)->statusEast;}
  UInt_t        getStatusFTPCEast()             {return statusEast();}
  UInt_t        getStatusFTPCWest()             {return statusWest();}
 protected:
  St_ftpcVoltageStatusC(St_ftpcVoltageStatus *table=0) : TChair(table) {}
  virtual ~St_ftpcVoltageStatusC() {SafeDelete(fgInstance);}
 private:
  static St_ftpcVoltageStatusC* fgInstance;
  ClassDefChair(St_ftpcVoltageStatus, ftpcVoltageStatus_st )
  ClassDef(St_ftpcVoltageStatusC,1) //C++ TChair for ftpcVoltageStatus table class
};
#endif
