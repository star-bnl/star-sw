#ifndef STAR_ReadEmcStatusTable
#define STAR_ReadEmcStatusTable
#ifndef __CINT__
#include <qstring.h>
#else
class QString;
#endif

class St_emcOnlineStatus; 
class TTable;

class ReadEmcStatusTable : public TObject 
{
   enum { kInit,    kOpenFile, kTimeStamp, kStartData
         ,kGetData, kCloseFile
         ,kError,   kFinish };
   private:
      int fCurrentState;
      int fOldState;
      int fNextState;
      int fFinishState;
      TString fFileName;
      FILE *fFile;
      QString fString;
      QString fErrorText;
      St_emcOnlineStatus *fBemcOnlineStatus;
      QString fDate;
      QString fTime;
   protected:
     int State() const { return fCurrentState;}
     bool IsError() const { return fCurrentState == kError;}
     int NextState(int state);
     void Init();
     int  GetLine();
     void OpenFile();
     void TimeStamp();
     void StartData();
     void GetData();
     void CloseFile();
     void ErrorNode();
     void Finish();
   public:
      //______________________________________
      ReadEmcStatusTable(const char *fileName=0) :      
         fCurrentState(kInit),fOldState(kInit)
        ,fNextState(kInit),fFinishState(kFinish)
        ,fFile(0),fBemcOnlineStatus(0)
        { SetFileName(fileName);}      
   
     ~ReadEmcStatusTable(){if (fFile) CloseFile();}
     
      //______________________________________
      void SetFileName(const char *fileName) 
      {
         if (fileName && fileName[0]) {
           fFileName = fileName;
           NextState(kInit);           
         } else {
           NextState(kError);           
         }
      }
      //______________________________________
      int OneStep() {
         switch (fCurrentState) {
            case kInit:      Init();     break;
            case kOpenFile:  OpenFile(); break;
            case kTimeStamp: TimeStamp();break;
            case kStartData: StartData();break;
            case kGetData:   GetData();  break;
            case kCloseFile: CloseFile();break;
            case kError:     ErrorNode();    break;
            case kFinish:    Finish();   break;
            };
         return State();
     }
     void StateLoop();
     TTable *Table() const;
     
     ClassDef(ReadEmcStatusTable,0)
};

#endif
