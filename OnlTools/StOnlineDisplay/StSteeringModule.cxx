//*-- Author : Victor Perevoztchikov
// 
// $Id: StSteeringModule.cxx,v 1.1 2009/12/01 01:33:33 fine Exp $


#include "StSteeringModule.h"
#include "TROOT.h"
#include "TDataSetIter.h"
#include "StDataReadModule.h"
#include "StDisplayModule.h"
#include "StDetectorGeometryModule.h"

#include "St_db_Maker/St_db_Maker.h"
#include "StTpcDb/StTpcDbMaker.h"
#include "StTpcDb/StTpcDb.h"


// STAR makers


// ClassImp(StSteeringModule)

//_____________________________________________________________________________
/// StSteeringModule constructor
/*!
  const char *name -  the name of this constructor
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  See <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A>

 */
StSteeringModule::StSteeringModule(const char *name):TModule(name)
{
 // -----------------------------------
 //  St_db_Maker *dbMk = new St_db_Maker("dbName","MySQL:StarDb","$STAR/StarDb");
   if (gSystem->AccessPathName("dbSnapshot.root")) {
     LOG_ERROR << "Can not open Db snapshot file. " << "Check the \"dbSnapshot.root\" file" << endm;
   }
   St_db_Maker *dbMk = new St_db_Maker("dbName","$STAR/StarDb","");
   new StTpcDbMaker("tpcdb");
   dbMk->SetAttr("dbSnapshot","dbSnapshot.root");
  
   dbMk->SetDateTime(20090312, 94451);
   dbMk->SetDebug(1);
  //   dbMk->SetDateTime(20071117,103638);
  //  dbMk->InitRun(8343021);
  // ready to go !!!

   fDetectorGeometryModule = new StDetectorGeometryModule();
   SetFileName("starcomplete.root");
//      AddVolume("Blue ring");
//      AddVolume("Yellow ring");

   //  data module
   fDataReadModule         = new StDataReadModule();
/*   
   int argc = qApp->argc();
   if (argc > 1) {
         const char *daqFileArg = qApp->argv()[2];
         if (daqFileArg[0] == '@') daqFileArg++; // workaround; ROOT eats the real file names ;-(
         QString daqFileName = daqFileArg;
	      if (QFileInfo(daqFileName).isReadable()) 
             fDataReadModule->SetDaqFileName(daqFileName);
	      else   { 
	        daqFileName = "";
	     }
      }
*/      
//      fDataReadModule->SetDaqFileName("CuCu72.daq"); // move to StStartDisplay
      fDataReadModule->SetMagneticField(0.005);
 
   fDisplayModule = new StDisplayModule();
}


//_____________________________________________________________________________
/// This is teering Module  destructor
/*!
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  see: <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> 

 */
StSteeringModule::~StSteeringModule(){
  //
}


//_____________________________________________________________________________
/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t StSteeringModule::Init(){
  // Create tables
  // Create Histograms 
//   SetNumber(21);
   
   // QMutexLocker lock(fMutex);
   
   Int_t res = TModule::Init();
   if (fDisplayModule && fDataReadModule) 
    {
       // Transfer providers
       fDisplayModule->SetColorProvider( fDataReadModule->GetColorProvider());
       fDisplayModule->SetSizeProvider ( fDataReadModule->GetSizeProvider() );
    }
   TDataSet *ds = GetDataBase("Calibrations/emc/map");
   assert(ds);
   ds  = GetDataBase("RunLog/onl/starClockOnl");
   assert(ds);
   ds  =GetDataBase("Geometry/tpc");
   assert(ds);
   Maker("dbName")->SetActive(kFALSE);
  // Maker("tpcdb")->SetActive(kFALSE);
   return res;
}


//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StSteeringModule::Make(){
   //
   //  PrintInfo();
   //
   // QMutexLocker lock(fMutex);
   TModule::Make();
//   printf("\n"); 
//   ls(0); 
   return kStOK;
}
//____________________________________________________________________________
//
/// StDetectorGeometryInterface methods
//____________________________________________________________________________

//_____________________________________________________________________________
void   StSteeringModule::SetFileName(const char* fileName)
{
   // QMutexLocker lock(fMutex);
   if (fDetectorGeometryModule) fDetectorGeometryModule->SetFileName(fileName);
}

//_____________________________________________________________________________
void   StSteeringModule::SetRecording(bool on)
{
    // QMutexLocker lock(fMutex);
    if (fDisplayModule) fDisplayModule->SetRecording(on);
} 


//_____________________________________________________________________________
bool  StSteeringModule::Recording()  const
{
    // QMutexLocker lock(fMutex);
    return fDisplayModule ? fDisplayModule->Recording(): false;
} 


//_____________________________________________________________________________
Int_t  StSteeringModule::BuildGeometry()
{
   // QMutexLocker lock(fMutex);
   if (fDetectorGeometryModule) return fDetectorGeometryModule->BuildGeometry();      
   return kStErr;
}      

//_____________________________________________________________________________
void   StSteeringModule::AddVolume(const char *name)
{
   // QMutexLocker lock(fMutex);

   // Replace the input detector nick name with the real if available
   if (fDetectorGeometryModule) {   }
}      

//_____________________________________________________________________________
void   StSteeringModule::RemoveVolume(const char *name)
{
   // QMutexLocker lock(fMutex);
   if (fDetectorGeometryModule) {   }
}

//_____________________________________________________________________________
void   StSteeringModule::Modified()
{
   // QMutexLocker lock(fMutex);
   if (fDetectorGeometryModule)  fDetectorGeometryModule->Modified();
}

//_____________________________________________________________________________
void   StSteeringModule::PrintVolumes()
{
   // QMutexLocker lock(fMutex);
   if (fDetectorGeometryModule) fDetectorGeometryModule->PrintVolumes();        
}

//_____________________________________________________________________________
//
/// StDisplayInterface methods
//_____________________________________________________________________________

//_____________________________________________________________________________
 void StSteeringModule::SetCanvas(TCanvas *c)
 {
    // QMutexLocker lock(fMutex);
    if (fDisplayModule ) fDisplayModule->SetCanvas(c);   
 }

//_____________________________________________________________________________
Int_t StSteeringModule::DisplayEvent(Bool_t refresh)
{
   // QMutexLocker lock(fMutex);
   Int_t ref = 0;
   if (fDisplayModule) {
      fDisplayModule->Clear();
      ref  = fDisplayModule->DisplayEvent(refresh);
   }
   return ref;
}
//_____________________________________________________________________________
void  StSteeringModule::Refresh() 
{
   // QMutexLocker lock(fMutex);
   if (fDisplayModule)  fDisplayModule ->Refresh();
}

//_____________________________________________________________________________
void  StSteeringModule::Set3DViewer( TQtRootViewer3D *viewer) 
{
   // QMutexLocker lock(fMutex);
   if (fDisplayModule)  fDisplayModule ->Set3DViewer(viewer);
}

//_____________________________________________________________________________
Int_t StSteeringModule::DisplayGeometry(Bool_t refresh, Bool_t ifModified)
{
   // QMutexLocker lock(fMutex);
   return fDisplayModule ? fDisplayModule->DisplayGeometry(refresh,ifModified) : 0;
}
//_____________________________________________________________________________
Int_t  StSteeringModule::NextFile()
{
   // QMutexLocker lock(fMutex);
   return (fDataReadModule) ? fDataReadModule->NextFile() : kStOK;
}

//_____________________________________________________________________________
Int_t  StSteeringModule::NextEvent()
{
   // QMutexLocker lock(fMutex);
   return (fDataReadModule) ? fDataReadModule->NextEvent() : kStOK;
}
//_____________________________________________________________________________
void  StSteeringModule::SetL3TracksOn(Int_t on)
{
   // QMutexLocker lock(fMutex);
   if (fDisplayModule)  fDisplayModule ->SetL3TracksOn(on);
   if (fDataReadModule) fDataReadModule->SetL3TracksOn(on);
}
 
//_____________________________________________________________________________
void  StSteeringModule::SetL3HitsOn(Int_t on)
{
   // QMutexLocker lock(fMutex);
   if (fDisplayModule)  fDisplayModule ->SetL3HitsOn(on);
   if (fDataReadModule) fDataReadModule->SetL3HitsOn(on);
}
//_____________________________________________________________________________
void  StSteeringModule::SetEmcHitsOn(Int_t on)
{
   // QMutexLocker lock(fMutex);
   if (fDisplayModule)  fDisplayModule ->SetEmcHitsOn(on);
   if (fDataReadModule) fDataReadModule->SetEmcHitsOn(on);
}

// St Data interfcace
      
//_____________________________________________________________________________
void StSteeringModule::SetMagneticField(int field)
{
    // QMutexLocker lock(fMutex);
    if (fDataReadModule) 
       fDataReadModule->SetMagneticField(field/10000.0);
}

//_____________________________________________________________________________
void StSteeringModule::SetDaqFileName(const char *fileName)
{
    // QMutexLocker lock(fMutex);

    if (fDataReadModule) 
       fDataReadModule->SetDaqFileName(fileName);   
}
//_____________________________________________________________________________
void StSteeringModule::SetEventNumber(int eventNumber)
{
     // QMutexLocker lock(fMutex);
     if (fDataReadModule) 
       fDataReadModule->SetEventNumber(eventNumber);
}
//_____________________________________________________________________________
Int_t StSteeringModule::RemakeEvent()
{
    // QMutexLocker lock(fMutex);

    return fDataReadModule ?
       fDataReadModule->Remake() : kStErr;
}
//_____________________________________________________________________________
const TString &StSteeringModule::MountPoint() const 
{
    static TString dummy;
    // QMutexLocker lock(fMutex);
    return fDataReadModule ?
       fDataReadModule->MountPoint() : dummy;
}
 
//_____________________________________________________________________________
void  StSteeringModule::SetCoin3DReady(Bool_t ready)
{
   // QMutexLocker lock(fMutex);

    if (fDisplayModule) 
       fDisplayModule->SetCoin3DReady(ready);
}

//_____________________________________________________________________________
void StSteeringModule::SetGuiObject(QObject *gui)
{
   // QMutexLocker lock(fMutex);
   if (fDataReadModule) fDataReadModule->SetGuiObject(gui);
}
//_____________________________________________________________________________
void  StSteeringModule::NextEventsSlot(int interval)
{
   LOG_DEBUG << "StSteeringModule::NextEventsSlot: " <<  interval << endm;

   if (fDataReadModule) fDataReadModule->NextEventsSlot(interval);
}
//_____________________________________________________________________________
void  StSteeringModule::ResetConnection()
{
   // QMutexLocker lock(fMutex);
   if (fDataReadModule) fDataReadModule->DeletePool();
}

//_____________________________________________________________________________
void  StSteeringModule::StopEvents()
{
   // QMutexLocker lock(fMutex);
   if (fDataReadModule) fDataReadModule->StopEvents();
}
//_____________________________________________________________________________
Bool_t  StSteeringModule::IsDisplayNext() const 
{
      // QMutexLocker lock(fMutex);
   return ((StMaker*)fDataReadModule) == CurrentMaker(); 
}
