#include "StiMasterDetectorBuilder.h"
#include "TGeoManager.h"
#include "StiDetector.h"
#include "StMaker.h"
StiMasterDetectorBuilder::StiMasterDetectorBuilder(bool active)
  : StiDetectorBuilder("MasterDetectorBuilder",active,"none")
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
  TGeoManager *gGeoManagerSV = gGeoManager; gGeoManager = 0;
  if (StMaker::GetChain()) {
    cout << "StiMasterDetectorBuilder::build() -I- Create clone of VmcGeometry by reinitialization for recontruction" <<endl;
    TDataSet *set = StMaker::GetChain()->GetDataBase("VmcGeometry/Geometry", &StMaker::GetChain()->StMaker::GetDBTime());
    delete set;
  }
  cout << "StiMasterDetectorBuilder::build() -I- Started"<<endl;
  vector<StiDetectorBuilder*>::iterator iter;
  unsigned int nRows=0;
  for (iter=begin();
       iter!=end();
       iter++)
    {
      cout << "StiMasterDetectorBuilder::build() -I- Calling Group Builder named:" << (*iter)->getName()<<endl;
      if (!*iter) cout <<"   pointer is corrupted!!!!!!!!!!!!!!!!!!!"<<endl;
      (*iter)->build(source);
      nRows+=(*iter)->getNRows();
    }
  cout << "StiMasterDetectorBuilder::build() -I- Will build local array"<<endl;
  setNRows(nRows);
  unsigned int row=0;
  for (iter=begin();
       iter!=end();
       iter++)
    {
      cout << "StiMasterDetectorBuilder::build() -I- Builder:"<<(*iter)->getName()<<endl;
      for (unsigned int i=0;i<(*iter)->getNRows();i++)
	{
	  cout << "StiMasterDetectorBuilder::build() -I- row:"<<row;
	  unsigned int nSectors = (*iter)->getNSectors(i);
	  setNSectors(row,nSectors);
	  Int_t ifrow = 0;
	  for (unsigned int sector=0;sector<nSectors;sector++)
	    {
	      StiDetector *detector = (*iter)->getDetector(i,sector);
	      if (!ifrow && detector) {cout << "\t" << detector->getName(); ifrow++;}
	      setDetector(row,sector,detector);
	    }
	  cout << endl;
	  row++;
	}
    }
  if (gGeoManagerSV) {
#define __CHECK_SHARED_OBJECTS__
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
	      cout << "Duplicate object " << o->GetName() << "\t" << o->GetTitle() << endl;
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
  cout << "StiMasterDetectorBuilder::build() -I- Done"<<endl;
}

/*! Return true if this builder has not served all detector objects currently registered with it.
 */
bool StiMasterDetectorBuilder::hasMore() const
{ 
  //cout << "StiMasterDetectorBuilder::build() -I- Started"<<endl;
  vector<StiDetectorBuilder*>::const_iterator iter;
  for (iter=begin();
       iter!=end();
       iter++)
    {
      //cout << "StiMasterDetectorBuilder::hasMore() -I- Calling Group Builder named:" << (*iter)->getName()<<endl;
      if((*iter)->hasMore()) return true;
    }
  return false;
}

/*! Find and return the next available detector object registered with this builder. Return a null pointer if there are 
no detector object left to server.
 */
StiDetector * StiMasterDetectorBuilder::next()
{
  //cout << "StiMasterDetectorBuilder::next() -I- Started"<<endl;
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
      //cout << "StiMasterDetectorBuilder::next() -I- Calling Group Builder named:" << (*iter)->getName()<<endl;
      if((*iter)->isName(name)) return *iter;
    }
	throw runtime_error("StiMasterDetectorBuilder::get(const string & name) -E- Requested object not found");
}
