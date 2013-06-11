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
  if (! gGeoManager) StiVMCToolKit::GetVMC();

  TGeoManager *gGeoManagerSV = gGeoManager; gGeoManager = 0;
  StMaker *chain=StMaker::GetChain();

  LOG_INFO << "We are StiMasterDetectorBuilder::build() and will clone the geometry if exists" << endm;
  if ( chain ){
    TDatime t = chain->StMaker::GetDBTime();
    LOG_INFO << "Timestanp check - Date/Time from chain is " << t.GetDate() << "/" << t.GetTime() << endm;
  }

#if 1
  if ( gGeoManagerSV ){
    LOG_INFO << "Create a clone of VmcGeometry for reconstruction" << endm;
    gGeoManagerSV->Clone("CloneGeom");
  } else {
    LOG_INFO << "Could not get a pointer to gGeoManager (??)" << endm;
  }
#else
  if ( chain ) {
    cout << "StiMasterDetectorBuilder::build() -I- Create clone of VmcGeometry by reinitialization for reconstruction" <<endl;
    TDataSet *set = chain->GetDataBase("VmcGeometry/Geometry", &chain->StMaker::GetDBTime());
    delete set;
  }
#endif
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
    //VP    SafeDelete(gGeoManager); 
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
