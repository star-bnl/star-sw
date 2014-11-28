/*------------------------------------------------------------------------------
| 
| checkDBTable.C 
| Macro to compare a table in a file with a table saved on STAR
| databse
| 
| First argument is the file name. The file names for tables to be saved on DB
| includes information about the kind of table and time stamp. The macro
| gets this information to make the comparison. For the table on Db, an
| object of the class StEmcDbHandler is created which returns
| table in the appropriate format to be compared to the one in the file.
| 
| Second argument is the maximum entry time in the database. It means that the
| table had to be *saved* before that time. Do not mistake this as the
| timestamp
|
| Third argument is the flavor the tables is save in the database
|
|
| author: Marcia Maria de Moura 2005-03-22
|
|-----------------------------------------------------------------------------*/
std::string DBTIME;
bool checkDbTable(char* fileSource, char* MAXENTRY = "2030-01-01 00:00:00",char* flavor = "ofl")
{  
  bool equal=true;  
  TFile* tableFile=new TFile(fileSource);    
  
  TString tempString = fileSource;
  char slash = '/';
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
cout << "Requesting table name=" << tableName << ", timestamp=" << timeStamp << " flavor=" << flavor << " maxentry=" << MAXENTRY << endl;
  dbHandler->setTableName(tableName.Data());
  dbHandler->setTimeStamp(timeStamp.Data());
  dbHandler->setMaxEntryTime(MAXENTRY);    
  dbHandler->setFlavor(flavor);    

  StDbTable *table =  dbHandler->getDbTable();  
  DBTIME = dbHandler->timeToSqlTime(table->getBeginDateTime());
  if (tableName=="bemcCalib" || tableName=="bprsCalib" )
  {
    emcCalib_st* tableInFile = (emcCalib_st*) ((St_emcCalib*)tableFile->Get(tableName.Data()))->GetTable();
    emcCalib_st* tableInDb   = (emcCalib_st*) table->GetTable();
    equal=compare(tableInFile,tableInDb);
  }
  
  if (tableName=="bsmdeCalib" || tableName=="bsmdpCalib" )
  {
    smdCalib_st* tableInFile = (smdCalib_st*) ((St_smdCalib*)tableFile->Get(tableName.Data()))->GetTable();
    smdCalib_st* tableInDb   = (smdCalib_st*) table->GetTable();
    equal=compare(tableInFile,tableInDb);
  }
  
  if (tableName=="bemcGain" || tableName=="bprsGain" )
  {
    emcGain_st* tableInFile = (emcGain_st*) ((St_emcGain*)tableFile->Get(tableName.Data()))->GetTable();
    emcGain_st* tableInDb   = (emcGain_st*) table->GetTable();
    equal=compare(tableInFile,tableInDb);
  }
  
  if (tableName=="bsmdeGain" || tableName=="bsmdpGain" )
  {
    smdGain_st* tableInFile = (smdGain_st*) ((St_smdGain*)tableFile->Get(tableName.Data()))->GetTable();
    smdGain_st* tableInDb   = (smdGain_st*) table->GetTable();
    equal=compare(tableInFile,tableInDb);
  } 
  
  if (tableName=="bemcPed" || tableName=="bprsPed" )
  {
    emcPed_st* tableInFile = (emcPed_st*) ((St_emcPed*)tableFile->Get(tableName.Data()))->GetTable();
    emcPed_st* tableInDb   = (emcPed_st*) table->GetTable();
    equal=compare(tableInFile,tableInDb);
  }
 
  if (tableName=="bsmdePed" || tableName=="bsmdpPed" )
  {
    smdPed_st* tableInFile = (smdPed_st*) ((St_smdPed*)tableFile->Get(tableName.Data()))->GetTable();
    smdPed_st* tableInDb   = (smdPed_st*) table->GetTable();
    equal=compare(tableInFile,tableInDb);
  }

  if (tableName=="bemcStatus" || tableName=="bprsStatus" )
  {
    emcStatus_st* tableInFile = (emcStatus_st*) ((St_emcStatus*)tableFile->Get(tableName.Data()))->GetTable();
    emcStatus_st* tableInDb   = (emcStatus_st*) table->GetTable();
    equal=compare(tableInFile,tableInDb);
  }

  if (tableName=="bsmdeStatus" || tableName=="bsmdpStatus" )
  {
    smdStatus_st* tableInFile = (smdStatus_st*) ((St_smdStatus*)tableFile->Get(tableName.Data()))->GetTable();
    smdStatus_st* tableInDb   = (smdStatus_st*) table->GetTable();
    equal=compare(tableInFile,tableInDb);
  }
  
  if (tableName=="emcTriggerPed")
  {
    emcTriggerPed_st* tableInFile = (emcTriggerPed_st*) ((St_emcTriggerPed*)tableFile->Get(tableName.Data()))->GetTable();
    emcTriggerPed_st* tableInDb   = (emcTriggerPed_st*) table->GetTable();
    equal=compare(tableInFile,tableInDb);
  }
      
  if (tableName=="emcTriggerStatus")
  {
    emcTriggerStatus_st* tableInFile = (emcTriggerStatus_st*) ((St_emcTriggerStatus*)tableFile->Get(tableName.Data()))->GetTable();
    emcTriggerStatus_st* tableInDb   = (emcTriggerStatus_st*) table->GetTable();
    equal=compare(tableInFile,tableInDb);
  }

  if (tableName=="emcTriggerLUT")
  {
    emcTriggerLUT_st* tableInFile = (emcTriggerLUT_st*) ((St_emcTRiggerLUT*)tableFile->Get(tableName.Data()))->GetTable();
    emcTriggerLUT_st* tableInDb   = (emcTriggerLUT_st*) table->GetTable();
    equal=compare(tableInFile,tableInDb);
  }
  tableFile->Close();
  delete tableFile;
  delete dbHandler;
    cout << (equal ? "EQUAL" : "NOT EQUAL") << endl;
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
      if(t1->AdcToE[i][j]!=t2->AdcToE[i][j]) equal = false;
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
      if(t1->AdcToE[i][j]!=t2->AdcToE[i][j]) equal = false;
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
    if(t1->Status[i]!=t2->Status[i]) 
    {
     equal = false;
     //cout <<"i = "<<i<<"  "<<(int)t1->Status[i]<<"  "<<(int)t2->Status[i]<<endl;
    }
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
