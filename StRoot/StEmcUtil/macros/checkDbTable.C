/*------------------------------------------------------------------------------
| 
| checkDBTable.C 
| Macro to compare a table in a file with a table saved on STAR
| databse
| 
| Argument is the file name. The file names for tables to be saved on DB
| includes information about the kind of table and time stamp. The macro
| gets this information to make the comparison. For the table on Db, an
| object of the class StEmcDbHandler is created which returns
| table in the appropriate format to be compared to the one in the file.
|
| author: Marcia Maria de Moura 2005-03-22
|
|-----------------------------------------------------------------------------*/

bool checkDbTable(char* fileSource)
{  
  bool equal=true;  
  TFile* tableFile=new TFile(fileSource);    
  
  TString tempString = fileSource;
  char slash = "/";
  int index = tempString.Last(slash);
  TString fileName = tempString(index+1,tempString.Length()-index);
  
  int index2 = fileName.Index(".");
  
  TString tableName = fileName(0,index2);
  TString date = fileName(index2+1,4) + "-" +
                 fileName(index2+5,2) + "-" +
		 fileName(index2+7,2);

  TString time = fileName(index2+10,2) + ":" +
                 fileName(index2+12,2) + ":" +
		 fileName(index2+14,2);

  TString timeStamp = date + " " + time;  

  StEmcDbHandler* dbHandler = new StEmcDbHandler();
  dbHandler->setTableName(tableName.Data());
  dbHandler->setTimeStamp(timeStamp.Data());    
  
  
  if (tableName=="bemcCalib" || tableName=="bprsCalib" )
  {
    emcCalib_st* tableInFile = (emcCalib_st*) tableFile->Get(tableName.Data());
    emcCalib_st* tableInDb   = (emcCalib_st*) dbHandler->getDbTable()->GetTable();
    if (!compare(tableInFile,tableInDb)) equal=false;
  }
  
  if (tableName=="bsmdeCalib" || tableName=="bsmdpCalib" )
  {
    smdCalib_st* tableInFile = (smdCalib_st*) tableFile->Get(tableName.Data());
    smdCalib_st* tableInDb   = (smdCalib_st*) dbHandler->getDbTable()->GetTable();
    if (!compare(tableInFile,tableInDb)) equal=false;
  }
  
  if (tableName=="bemcGain" || tableName=="bprsGain" )
  {
    emcGain_st* tableInFile = (emcGain_st*) tableFile->Get(tableName.Data());
    emcGain_st* tableInDb   = (emcGain_st*) dbHandler->getDbTable()->GetTable();
     if (!compare(tableInFile,tableInDb)) equal=false;
  }
  
  if (tableName=="bsmdeGain" || tableName=="bsmdpGain" )
  {
    smdGain_st* tableInFile = (smdGain_st*) tableFile->Get(tableName.Data());
    smdGain_st* tableInDb   = (smdGain_st*) dbHandler->getDbTable()->GetTable();
    if (!compare(tableInFile,tableInDb)) equal=false;
  } 
  
  if (tableName=="bemcPed" || tableName=="bprsPed" )
  {
    emcPed_st* tableInFile = (emcPed_st*) tableFile->Get(tableName.Data());
    emcPed_st* tableInDb   = (emcPed_st*) dbHandler->getDbTable()->GetTable();
    if (!compare(tableInFile,tableInDb)) equal=false;
  }
 
  if (tableName=="bsmdePed" || tableName=="bsmdpPed" )
  {
    smdPed_st* tableInFile = (smdPed_st*) tableFile->Get(tableName.Data());
    smdPed_st* tableInDb   = (smdPed_st*) dbHandler->getDbTable()->GetTable();
    if (!compare(tableInFile,tableInDb)) equal=false;
  }

  if (tableName=="bemcStatus" || tableName=="bprsStatus" )
  {
    emcStatus_st* tableInFile = (emcStatus_st*) tableFile->Get(tableName.Data());
    emcStatus_st* tableInDb   = (emcStatus_st*) dbHandler->getDbTable()->GetTable();
    if (!compare(tableInFile,tableInDb)) equal=false;
  }

  if (tableName=="bsmdeStatus" || tableName=="bsmdpStatus" )
  {
    smdStatus_st* tableInFile = (smdStatus_st*) tableFile->Get(tableName.Data());
    smdStatus_st* tableInDb   = (smdStatus_st*) dbHandler->getDbTable()->GetTable();
    if (!compare(tableInFile,tableInDb)) equal=false;
  }
  
  if (tableName=="emcTriggerPed")
  {
    emcTriggerPed_st* tableInFile = (emcTriggerPed_st*) tableFile->Get(tableName.Data());
    emcTriggerPed_st* tableInDb   = (emcTriggerPed_st*) dbHandler->getDbTable()->GetTable();
    if (!compare(tableInFile,tableInDb)) equal=false;
  }
      
  if (tableName=="emcTriggerStatus")
  {
    emcTriggerStatus_st* tableInFile = (emcTriggerStatus_st*) tableFile->Get(tableName.Data());
    emcTriggerStatus_st* tableInDb   = (emcTriggerStatus_st*) dbHandler->getDbTable()->GetTable();
    if (!compare(tableInFile,tableInDb)) equal=false;
  }

  if (tableName=="emcTriggerLUT")
  {
    emcTriggerLUT_st* tableInFile = (emcTriggerLUT_st*) tableFile->Get(tableName.Data());
    emcTriggerLUT_st* tableInDb   = (emcTriggerLUT_st*) dbHandler->getDbTable()->GetTable();
    if (!compare(tableInFile,tableInDb)) equal=false;
  }
  return equal;

}

// Functions to compare two tables
//------------------------------------------------------------------------------
bool compare(emcCalib_st* t1, emcCalib_st* t2)
{
  bool equal = true;  
  for(int i = 0; i<4800;i++) 
  {
    for(int j=0;j<5;j++)
      if(t1->AdcToE[i][j]!=t2->AdcToE[i][j]) equal = false
    if(t1->Status[i]!=t2->Status[i]) equal = false;
  }
  return equal;
} 
//------------------------------------------------------------------------------
bool compare(smdCalib_st* t1, smdCalib_st* t2)
{
  bool equal = true;  
  for(int i = 0; i<18000;i++) 
  { 
    for(int j=0;j<5;j++)
      if(t1->AdcToE[i][j]!=t2->AdcToE[i][j]) equal = false
    if(t1->Status[i]!=t2->Status[i]) equal = false;
  }
  return equal;
}
//------------------------------------------------------------------------------
bool compare(emcStatus_st* t1, emcStatus_st* t2)
{
  bool equal = true;  
  for(int i = 0; i<4800;i++) 
  { 
    if(t1->Status[i]!=t2->Status[i]) equal = false;
  }
  return equal;
}
//------------------------------------------------------------------------------
bool compare(smdStatus_st* t1, smdStatus_st* t2)
{
  bool equal = true;  
  for(int i = 0; i<18000;i++) 
  { 
    if(t1->Status[i]!=t2->Status[i]) equal = false;
  }
  return equal;
}
//------------------------------------------------------------------------------
bool compare(emcGain_st* t1, emcGain_st* t2)
{
  bool equal = true;  
  for(int i = 0; i<4800;i++) 
  { 
    if(t1->Gain[i]!=t2->Gain[i]) equal = false;
  }
  return equal;
}
//------------------------------------------------------------------------------
bool compare(smdGain_st* t1, smdGain_st* t2)
{
  bool equal = true;  
  for(int i = 0; i<18000;i++) 
  { 
    if(t1->Gain[i]!=t2->Gain[i]) equal = false;
  }
  return equal;
}
//------------------------------------------------------------------------------
bool compare(emcPed_st* t1, emcPed_st* t2)
{
  bool equal = true;  
  for(int i = 0; i<4800;i++) 
  { 
    if(t1->AdcPedestal[i]!=t2->AdcPedestal[i]) equal = false;
    if(t1->AdcPedestalRMS[i]!=t2->AdcPedestalRMS[i]) equal = false;
    if(t1->Status[i]!=t2->Status[i]) equal = false;
  }
  return equal;
}
//------------------------------------------------------------------------------
bool compare(smdPed_st* t1, smdPed_st* t2)
{
  bool equal = true;  
  for(int i = 0; i<18000;i++) 
  { 
    if(t1->AdcPedestal[i][0]!=t2->AdcPedestal[i][0]) equal = false;
    if(t1->AdcPedestal[i][1]!=t2->AdcPedestal[i][1]) equal = false;
    if(t1->AdcPedestal[i][2]!=t2->AdcPedestal[i][2]) equal = false;
    if(t1->AdcPedestalRMS[i]!=t2->AdcPedestalRMS[i]) equal = false;
    if(t1->Status[i]!=t2->Status[i]) equal = false;
  }
  return equal;
}
//------------------------------------------------------------------------------
bool compare(emcTriggerPed_st* t1, emcTriggerPed_st* t2)
{
  bool equal = true;  
  for(int i = 0; i<4800;i++) 
  { 
    if(t1->PedShift!=t2->PedShift) equal = false;
    for(int c=0;c<30;c++) 
    {
      for(int p=0;p<10;p++) 
        if(t1->BitConversionMode[c][p]!=t2->BitConversionMode[c][p]) equal = false;
      for(int p=0;p<160;p++) 
        if(t1->Ped[c][p]!=t2->Ped[c][p]) equal = false;
    }
  }
  return equal;
}
//------------------------------------------------------------------------------
bool compare(emcTriggerStatus_st* t1, emcTriggerStatus_st* t2)
{
  bool equal = true;  
  for(int i = 0; i<4800;i++) 
  { 
    if(t1->PedShift!=t2->PedShift) equal = false;
    for(int c=0;c<30;c++) 
    {
      for(int p=0;p<10;p++)
      { 
        if(t1->PatchStatus[c][p]!=t2->PatchStaus[c][p]) equal = false;
        if(t1->HighTowerStatus[c][p]!=t2->HighTowerStaus[c][p]) equal = false;
      }
      for(int p=0;p<160;p++) 
        if(t1->TowerStatus[c][p]!=t2->TowerStatus[c][p]) equal = false;
    }
  }
  return equal;
}
//------------------------------------------------------------------------------
bool compare(emcTriggerLUT_st* t1, emcTriggerLUT_st* t2)
{
  if(notLoaded) loadLibrariesCompare();
  bool equal = true;   
  for(int c=0;c<30;c++) 
  {
    for(int p=0;p<10;p++)
    { 
      if(t1->FormulaTag[c][p]!=t2->FormulaTag[c][p]) equal = false;
      if(t1->FormulaParameter0[c][p]!=t2->FormulaParameter0[c][p]) equal = false;
      if(t1->FormulaParameter1[c][p]!=t2->FormulaParameter1[c][p]) equal = false;
      if(t1->FormulaParameter2[c][p]!=t2->FormulaParameter2[c][p]) equal = false;
      if(t1->FormulaParameter3[c][p]!=t2->FormulaParameter3[c][p]) equal = false;
      if(t1->FormulaParameter4[c][p]!=t2->FormulaParameter4[c][p]) equal = false;
      if(t1->FormulaParameter5[c][p]!=t2->FormulaParameter5[c][p]) equal = false;
    }
  }
  return equal;
}
//******************************************************************************
