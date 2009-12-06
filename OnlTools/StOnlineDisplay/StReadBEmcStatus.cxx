#include "St_emcOnlineStatus_Table.h"
#include "StReadBEmcStatus.h"
#include <qstring.h>
#include "TSystem.h"
#include <errno.h>
#include <qstringlist.h> 
#include <stdlib.h>

ClassImp(ReadEmcStatusTable)
//_______________________________________________________
void ReadEmcStatusTable::Init()
{
  NextState(kOpenFile);           
}

//_______________________________________________________
void ReadEmcStatusTable::OpenFile()
{
   NextState(kTimeStamp);           
   if (!gSystem->AccessPathName((const char*)fFileName)) {
      fFile = fopen((const char*)fFileName,"r");
      if (fFile == NULL) 
      {
        fErrorText = strerror(errno);  
        NextState(kError);           
      }
   } else {
      fErrorText = "Can not access the file";
      NextState(kError);               
   }
}
//_______________________________________________________
void ReadEmcStatusTable::TimeStamp()
{
     if (GetLine() != kError) {
       // look for the tie stamp patter there:
       if ( fString.contains("Timestamp:") ) {
         //  # Timestamp: 2007-04-02 18:50:02
         fDate = fString; //(yearIndx,dateSep-1);
         fDate.replace(QChar('-'),"");
         fTime = fString;//(dateSep+1);
         fTime.replace(QChar(':'),"");
         printf(" date =  %s; time = %s \n"
               , (const char*)fDate
               , (const char*)fTime);
         NextState(kStartData);    
       }
    }       
}
//_______________________________________________________
void ReadEmcStatusTable::StartData()
{
   // Lool for the first 
   if ( (GetLine() != kError) && (fString.contains("SoftId") ) )
   {      NextState(kGetData);                                  }   
}

//_______________________________________________________
void ReadEmcStatusTable::GetData()
{
   // Lool for the first 
   if (GetLine() != kError)
   {
      if (!fBemcOnlineStatus) 
         fBemcOnlineStatus =  new St_emcOnlineStatus();
      QStringList list = QStringList::split("\t",fString);
      int i = 0;
      bool ok;
      emcOnlineStatus_st row;
      QStringList::Iterator it = list.begin(); ++it; //skip the first column
      for (;it != list.end(); ++it,i++ ) {
         // fill the table row
         unsigned char value = 0;
         float fValue = 0;
         
         if (i != 5) value = (unsigned char) (*it).toUShort(&ok);
         else fValue = (*it).toFloat(&ok);
         if (ok) {
         
           switch (i) {
             case 0: row.Crate       = value; break; // Crate 
             case 1: row.CrateSeq    = value; break; // Crate seq
             case 2: row.TowerMask   = value; break; // Tower unmasked?
             case 3: row.PatchMaskHT = value; break; // Patch unmasked in HT?
             case 4: row.PatchMaskSum= value; break; // Patch unmasked in sum?
             case 6: row.TriggerPatch= value; break; // TriggerPatch
             case 5: row.Pedestal    = fValue;break; // Pedestal    
          }
        } else {
          NextState(kError);               
          fErrorText = "Can not parse the line";
          break;
       }
     }
     row.SoftId = fBemcOnlineStatus->GetNRows() + 1;
     fBemcOnlineStatus->AddAt(&row);
     if (row.SoftId>=4800) NextState(kCloseFile);           
   }
}
//_________________________________row.SoftId______________________
void ReadEmcStatusTable::CloseFile()
{
   if (fFile) fclose(fFile); fFile = 0;
   NextState(kFinish);           
}

//_______________________________________________________
void ReadEmcStatusTable::ErrorNode()
{
    NextState(kFinish);           
}
      
//_______________________________________________________
void ReadEmcStatusTable::Finish()
{
    NextState(kFinish);           
}

//_______________________________________________________
void ReadEmcStatusTable::StateLoop()
{
   while(State() != kFinish) OneStep();
}
//_______________________________________________________
int ReadEmcStatusTable::GetLine()
{
  char *buf = 0;
  size_t len;
  if (getline(&buf, &len, fFile) != -1) {
    fString = buf; fString.replace('\n',"");
    free(buf); 
  } else {
    fErrorText = strerror(errno);  
    NextState(kError);           
  }
  return State();
}
//_______________________________________________________
TTable *ReadEmcStatusTable::Table() const
{ return fBemcOnlineStatus;} 
//_______________________________________________________
int ReadEmcStatusTable::NextState(int state) 
{
   if (state == kError) 
      Error("NextState","%s",(const char*)fErrorText);
   int s = fOldState;
   fOldState = fCurrentState; 
   fCurrentState = state;
   return s;
}
