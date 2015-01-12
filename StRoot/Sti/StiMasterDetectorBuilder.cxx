#include "StiMasterDetectorBuilder.h"
#include "TGeoManager.h"
#include "StiDetector.h"
#include "StMaker.h"
StiMasterDetectorBuilder::StiMasterDetectorBuilder(bool active)
  : StiDetectorBuilder("MasterDetectorBuilder",active)
{}

StiMasterDetectorBuilder::~StiMasterDetectorBuilder()
{}

/*! Reset this builder to a  null state. 
 <p>
 A reset erases all detectors held by this builder.
*/
void StiMasterDetectorBuilder::reset()
{
  clear();
}

/*! Build all the detector groups and their elementary detector components registered with this builder.
 */
void StiMasterDetectorBuilder::build(StMaker&source)
{
  //#define __Physics_Node_Clear__
#ifndef __Physics_Node_Clear__
  if (! gGeoManager) StiVMCToolKit::GetVMC();

  TGeoManager *gGeoManagerSV = gGeoManager; gGeoManager = 0;
  StMaker *chain=StMaker::GetChain();

  LOG_INFO << "We are StiMasterDetectorBuilder::build() and will clone the geometry if exists" << endm;
  if ( chain ){
    TDatime t = chain->StMaker::GetDBTime();
    LOG_INFO << "Timestamp check - Date/Time from chain is " << t.GetDate() << "/" << t.GetTime() << endm;
  }

#if 1
  if ( gGeoManagerSV ){
    LOG_INFO << "Create a clone of VmcGeometry for reconstruction" << endm;
    gGeoManagerSV->Clone("CloneGeom");
  } else {
    LOG_INFO << "Could not get a pointer to gGeoManager " << endm;
  }
#else
  if ( chain ) {
    LOG_INFO << "StiMasterDetectorBuilder::build() -I- Create clone of VmcGeometry by reinitialization for reconstruction" <<endm;
    TDataSet *set = chain->GetDataBase("VmcGeometry/Geometry", &chain->StMaker::GetDBTime());
    delete set;
  }
#endif
#endif /* ! __Physics_Node_Clear__ */
  LOG_INFO << "StiMasterDetectorBuilder::build() -I- Started"<<endm;
  vector<StiDetectorBuilder*>::iterator iter;
  UInt_t nRows=0;
  for (iter=begin();       iter!=end();       iter++)    {
    LOG_INFO << "StiMasterDetectorBuilder::build() -I- Calling Group Builder named:" << (*iter)->getName()<<endm;
    if (!*iter) {LOG_INFO <<"   pointer is corrupted!!!!!!!!!!!!!!!!!!!"<<endm;}
    (*iter)->build(source);
    nRows+=(*iter)->getNRows();
  }
  LOG_INFO << "StiMasterDetectorBuilder::build() -I- Will build local array"<<endm;
  setNRows(nRows);
  UInt_t row=0;
  for (iter=begin();       iter!=end();       iter++)    {
    for (UInt_t i=0;i<(*iter)->getNRows();i++)	{
      UInt_t nSectors = (*iter)->getNSectors(i);
      setNSectors(row,nSectors);
      for (UInt_t sector=0;sector<nSectors;sector++)	    {
	StiDetector *detector = (*iter)->getDetector(i,sector);
	if (detector && detector->isActive()) {
	  if (! sector) {
	    LOG_INFO << "StiMasterDetectorBuilder::build() -I- row:"<<row << "\t" << *detector << endm;
	  }
	  setDetector(row,sector,detector);
	}
      }
      row++;
    }
  }
#ifndef __Physics_Node_Clear__
  if (gGeoManagerSV) {
    //#define __CHECK_SHARED_OBJECTS__
#ifdef __CHECK_SHARED_OBJECTS__
    // Clean up shared objects 
    enum {NoLists = 12};
    TSeqCollection *listSV[NoLists] = {
      gGeoManagerSV->GetListOfMaterials(),      // TList                 
      gGeoManagerSV->GetListOfMedia(),	        // TList                 
      gGeoManagerSV->GetListOfNodes(),          // TObjArray             
      gGeoManagerSV->GetListOfPhysicalNodes(),  // TObjArray             
      gGeoManagerSV->GetListOfOverlaps(),	// TObjArray             
      gGeoManagerSV->GetListOfMatrices(),       // TObjArray             
      gGeoManagerSV->GetListOfVolumes(),        // TObjArray             
      gGeoManagerSV->GetListOfGVolumes(),       // TObjArray             
      gGeoManagerSV->GetListOfShapes(),	        // TObjArray             
      gGeoManagerSV->GetListOfGShapes(),	// TObjArray             
      gGeoManagerSV->GetListOfUVolumes(),       // TObjArray             
      gGeoManagerSV->GetListOfTracks()};        // TObjArray             
    TSeqCollection *list[NoLists] = {
      gGeoManager->GetListOfMaterials(),      // TList                 
      gGeoManager->GetListOfMedia(),	      // TList                 
      gGeoManager->GetListOfNodes(),          // TObjArray             
      gGeoManager->GetListOfPhysicalNodes(),  // TObjArray             
      gGeoManager->GetListOfOverlaps(),       // TObjArray             
      gGeoManager->GetListOfMatrices(),       // TObjArray             
      gGeoManager->GetListOfVolumes(),        // TObjArray             
      gGeoManager->GetListOfGVolumes(),       // TObjArray             
      gGeoManager->GetListOfShapes(),	      // TObjArray             
      gGeoManager->GetListOfGShapes(),	      // TObjArray             
      gGeoManager->GetListOfUVolumes(),       // TObjArray             
      gGeoManager->GetListOfTracks()};	      // TObjArray             
    for (Int_t l = 0; l < NoLists; l++) {
      if (listSV[l] && list[l]) {
	TIter nextSV(listSV[l]);
	TObject *oSV = 0;
	while ((oSV = nextSV())) {
	  TIter next(list[l]);
	  TObject *o = 0;
	  while ((o = next())) {
	    if (o == oSV) {
	      LOG_INFO << "Duplicate object " << o->GetName() << "\t" << o->GetTitle() << endm;
	      Int_t indx = list[l]->IndexOf(o);
	      if (indx >= 0) {
		list[l]->RemoveAt(indx);
	      }
	    }
	  }
	}
      }
    }
    //    delete gGeoManager;
#endif /* __CHECK_SHARED_OBJECTS__ */
    gGeoManager = gGeoManagerSV;
  }
#else /* __Physics_Node_Clear__ */
  LOG_INFO << "StiMasterDetectorBuilder::build() -I- ClearPhysicalNodes"<<endm;
  gGeoManager->ClearPhysicalNodes(kTRUE);
#undef __Physics_Node_Clear__
#endif /* !  __Physics_Node_Clear__ */
  LOG_INFO << "StiMasterDetectorBuilder::build() -I- Done"<<endm;
}

/*! Return true if this builder has not served all detector objects currently registered with it.
 */
bool StiMasterDetectorBuilder::hasMore() const
{ 
  //LOG_INFO << "StiMasterDetectorBuilder::build() -I- Started"<<endm;
  vector<StiDetectorBuilder*>::const_iterator iter;
  for (iter=begin();
       iter!=end();
       iter++)
    {
      //LOG_INFO << "StiMasterDetectorBuilder::hasMore() -I- Calling Group Builder named:" << (*iter)->getName()<<endm;
      if((*iter)->hasMore()) return true;
    }
  return false;
}

/*! Find and return the next available detector object registered with this builder. Return a null pointer if there are 
no detector object left to server.
 */
StiDetector * StiMasterDetectorBuilder::next()
{
  //LOG_INFO << "StiMasterDetectorBuilder::next() -I- Started"<<endm;
  vector<StiDetectorBuilder*>::const_iterator iter;
  for (iter=begin();
       iter!=end();
       iter++)
    {
      if((*iter)->hasMore()) return (*iter)->next();
    }
  return 0;
}

void StiMasterDetectorBuilder::add(StiDetectorBuilder *builder)
{
  push_back(builder);
}

StiDetectorBuilder * StiMasterDetectorBuilder::get(const string & name)
{
	// iterate through the list to find the requested object.
	  vector<StiDetectorBuilder*>::const_iterator iter;
  for (iter=begin();
       iter!=end();
       iter++)
    {
      //LOG_INFO << "StiMasterDetectorBuilder::next() -I- Calling Group Builder named:" << (*iter)->getName()<<endm;
      if((*iter)->isName(name)) return *iter;
    }
	throw runtime_error("StiMasterDetectorBuilder::get(const string & name) -E- Requested object not found");
}
