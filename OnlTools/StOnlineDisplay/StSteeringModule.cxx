//*-- Author : Victor Perevoztchikov
// 
// $Id: StSteeringModule.cxx,v 1.17 2018/06/08 15:36:08 genevb Exp $


#include "StSteeringModule.h"
#include "TSystem.h"
#include "TROOT.h"
#include "TPad.h"
#include "TDataSetIter.h"
#include "StDataReadModule.h"
#include "StuDraw3DEvent.h"

#include "St_db_Maker/St_db_Maker.h"
#include "StTpcDb/StTpcDbMaker.h"
#include "StTpcDb/StTpcDb.h"
#include "StMagFMaker.h"
#include <QtCore/QCoreApplication>
#include <QtCore/QTimer>

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
, fAnimate(-1),fAnimating(kFALSE)
{
 // -----------------------------------
 //  St_db_Maker *dbMk = new St_db_Maker("dbName","MySQL:StarDb","$STAR/StarDb");
   if (gSystem->AccessPathName("dbSnapshot.root")) {
     LOG_ERROR << "Can not open Db snapshot file. " << "Check the \"dbSnapshot.root\" file" << endm;
   }
   new StMagFMaker;
//   St_db_Maker *dbMk = new St_db_Maker("dbName","MySQL:StarDb","$STAR/StarDb");
   St_db_Maker *dbMk = new St_db_Maker("dbName","$STAR/StarDb","");
   new StTpcDbMaker("tpcdb");
   dbMk->SetAttr("dbSnapshot","dbSnapshot.root");
  
   dbMk->SetDateTime(20090312, 94451);
   dbMk->SetDebug(1);

   //  data module
   fDataReadModule         = new StDataReadModule();
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
StSteeringModule::~StSteeringModule()
{
  //
}


//_____________________________________________________________________________
/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t StSteeringModule::Init(){
  // Create tables
  // Create Histograms 
//   SetNumber(21);
   

   Int_t res = TModule::Init();
   TDataSet *ds = GetInputDB("Calibrations/emc/map");
   assert(ds);
   ds  = GetInputDB("RunLog/onl/starClockOnl");
   assert(ds);
   ds  =GetInputDB("Geometry/tpc");
   assert(ds);
   Maker("dbName")->SetActive(kFALSE);
  // Maker("tpcdb")->SetActive(kFALSE);
   return res;
}


//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StSteeringModule::Make(){

   //  PrintInfo();
   return TModule::Make();
}
//____________________________________________________________________________
//
/// StDetectorGeometryInterface methods
//____________________________________________________________________________

//_____________________________________________________________________________
void   StSteeringModule::SetFileName(const char* fileName)
{
   if (fileName){}
}

//_____________________________________________________________________________
void   StSteeringModule::SetRecording(bool on)
{
     if (fDataReadModule) fDataReadModule->SetRecording(on);
} 


//_____________________________________________________________________________
bool  StSteeringModule::Recording()  const
{
    return (fDataReadModule) ? fDataReadModule->Recording() : kFALSE;
} 


//_____________________________________________________________________________
Int_t  StSteeringModule::BuildGeometry()
{
   return kStOk;
}      

//_____________________________________________________________________________
void   StSteeringModule::AddVolume(const char *name)
{
   // Replace the input detector nick name with the real if available
   if (name) {   }
}      

//_____________________________________________________________________________
void   StSteeringModule::RemoveVolume(const char *name)
{
   // QMutexLocker lock(fMutex);
   if (name) {   }
}

//_____________________________________________________________________________
void   StSteeringModule::Modified()
{
}

//_____________________________________________________________________________
void   StSteeringModule::PrintVolumes()
{
}

//_____________________________________________________________________________
//
/// StDisplayInterface methods
//_____________________________________________________________________________

//_____________________________________________________________________________
 void StSteeringModule::SetCanvas(TCanvas *c)
 {
    if (c ){ }
 }

//_____________________________________________________________________________
Int_t StSteeringModule::DisplayEvent(Bool_t refresh)
{

   Int_t ref = 0;
   if (refresh) {   }
   return ref;
}
//_____________________________________________________________________________
void  StSteeringModule::Refresh() 
{
 
}

//_____________________________________________________________________________
void  StSteeringModule::Set3DViewer( TQtRootViewer3D *viewer) 
{
   if (viewer){}
}

//_____________________________________________________________________________
Int_t StSteeringModule::DisplayGeometry(Bool_t refresh, Bool_t ifModified)
{
 
   return  0;
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
   if (fDataReadModule) fDataReadModule->SetL3TracksOn(on);
}
 
//_____________________________________________________________________________
void  StSteeringModule::SetL3HitsOn(Int_t on)
{
   if (fDataReadModule) fDataReadModule->SetL3HitsOn(on);
}
//_____________________________________________________________________________
void  StSteeringModule::SetEmcHitsOn(Int_t on)
{
   if (fDataReadModule) fDataReadModule->SetEmcHitsOn(on);
}

// St Data interfcace
      
//_____________________________________________________________________________
void StSteeringModule::SetMagneticField(int field)
{
    if (fDataReadModule) 
       fDataReadModule->SetMagneticField(field/10000.0);
}

//_____________________________________________________________________________
void StSteeringModule::SetDaqFileName(const char *fileName)
{

    if (fDataReadModule) 
       fDataReadModule->SetDaqFileName(fileName);   
}

//_____________________________________________________________________________
void StSteeringModule::SetRunNumber(int runNumber)
{
     if (fDataReadModule) 
       fDataReadModule->SetRunNumber(runNumber);
}

//_____________________________________________________________________________
void StSteeringModule::SetEventNumber(int eventNumber)
{
     if (fDataReadModule) 
       fDataReadModule->SetEventNumber(eventNumber);
}
//_____________________________________________________________________________
Int_t StSteeringModule::RemakeEvent()
{

    return fDataReadModule ?
       fDataReadModule->Remake() : kStErr;
}
//_____________________________________________________________________________
const TString &StSteeringModule::MountPoint() const 
{
    static TString dummy;
    return fDataReadModule ?
       fDataReadModule->MountPoint() : dummy;
}
 
//_____________________________________________________________________________
void  StSteeringModule::SetCoin3DReady(Bool_t ready)
{
    if (ready) {}      
}

//_____________________________________________________________________________
void StSteeringModule::SetGuiObject(QObject *gui)
{
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
   if (fDataReadModule) fDataReadModule->DeletePool();
}

//_____________________________________________________________________________
void  StSteeringModule::StopEvents()
{
   Animate(-1);
//   if (fDataReadModule) fDataReadModule->StopEvents();
}
//_____________________________________________________________________________
Bool_t  StSteeringModule::IsDisplayNext() const 
{
   return ((StMaker*)fDataReadModule) == CurrentMaker(); 
}

//_____________________________________________________________________________
void  StSteeringModule::Animating() 
{
  fAnimating = kTRUE; 
        Clear();
        int res = Make();
    if ( (res == kStOk) || (res == kStERR) ) {
       //if (gPad) gPad->Update();
       QCoreApplication::processEvents();
    } else {
       StopEvents();
    }
  fAnimating = kFALSE; 
  Animate(fAnimate);
}
//_____________________________________________________________________________
void  StSteeringModule::Animate(Int_t ms) 
{
  fAnimate = ms ? ms : 10;
  if (fAnimate>=0)  QTimer::singleShot(fAnimate, this, SLOT(Animating())); 
}

//_____________________________________________________________________________
void  StSteeringModule::SetDemo(Bool_t on)
{
   if (fDataReadModule) fDataReadModule->SetDemo(on); 
}

//_____________________________________________________________________________
Bool_t  StSteeringModule::Demo() const
{
     return (fDataReadModule) ? fDataReadModule->Demo(): kFALSE; 
} 

//_____________________________________________________________________________
StuDraw3DEvent * StSteeringModule::Display()
{
   return (fDataReadModule) ? fDataReadModule->Display(): 0;
}
//_____________________________________________________________________________
void   StSteeringModule::SetDrawOption(Option_t *option)
{
   if (fDataReadModule) fDataReadModule->Display()->SetDrawOption(option);
}
