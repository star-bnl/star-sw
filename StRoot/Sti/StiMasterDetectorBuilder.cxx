#include "StiMasterDetectorBuilder.h"
#include "Sti/Base/Messenger.h"

StiMasterDetectorBuilder::StiMasterDetectorBuilder()
  : StiDetectorBuilder("MasterDetectorBuilder")
{}

StiMasterDetectorBuilder::~StiMasterDetectorBuilder()
{}

/// Add a detector group builder to this composite builder.
void StiMasterDetectorBuilder::addBuilder(StiDetectorBuilder *builder)
{
  _builders.push_back(builder);
}

/*! Reset this builder to a  null state. 
 <p>
 A reset erases all detectors held by this builder.
*/
void StiMasterDetectorBuilder::reset()
{
  _builders.clear();
}

/*! Build all the detector groups and their elementary detector components registered with this builder.
 */
void StiMasterDetectorBuilder::build()
{
  _messenger << "StiMasterDetectorBuilder::build() - INFO - Started"<<endl;
  vector<StiDetectorBuilder*>::iterator iter;
  unsigned int nRows=0;
  for (iter=_builders.begin();
       iter!=_builders.end();
       iter++)
    {
      _messenger << "StiMasterDetectorBuilder::build() - INFO - Calling Group Builder named:" << (*iter)->getName()<<endl;
      (*iter)->build();
      nRows+=(*iter)->getNRows();
    }
  _messenger << "StiMasterDetectorBuilder::build() - INFO - Will build local array"<<endl;
  setNRows(nRows);
  unsigned int row=0;
  for (iter=_builders.begin();
       iter!=_builders.end();
       iter++)
    {
      _messenger << "StiMasterDetectorBuilder::build() - INFO - Builder:"<<(*iter)->getName()<<endl;
      for (unsigned int i=0;i<(*iter)->getNRows();i++)
	{
	  _messenger << "StiMasterDetectorBuilder::build() - INFO - row:"<<row<<endl;
	  unsigned int nSectors = (*iter)->getNSectors(i);
	  setNSectors(row,nSectors);
	  for (unsigned int sector=0;sector<nSectors;sector++)
	    {
	      setDetector(row,sector,(*iter)->getDetector(i,sector));
	    }
	  row++;
	}
    }
  _messenger << "StiMasterDetectorBuilder::build() - INFO - Done"<<endl;
}

/*! Return true if this builder has not served all detector objects currently registered with it.
 */
bool StiMasterDetectorBuilder::hasMore() const
{ 
  //_messenger << "StiMasterDetectorBuilder::build() - INFO - Started"<<endl;
  vector<StiDetectorBuilder*>::const_iterator iter;
  for (iter=_builders.begin();
       iter!=_builders.end();
       iter++)
    {
      //_messenger << "StiMasterDetectorBuilder::hasMore() - INFO - Calling Group Builder named:" << (*iter)->getName()<<endl;
      if((*iter)->hasMore()) return true;
    }
  return false;
}

/*! Find and return the next available detector object registered with this builder. Return a null pointer if there are 
no detector object left to server.
 */
StiDetector * StiMasterDetectorBuilder::next()
{
  //_messenger << "StiMasterDetectorBuilder::next() - INFO - Started"<<endl;
  vector<StiDetectorBuilder*>::const_iterator iter;
  for (iter=_builders.begin();
       iter!=_builders.end();
       iter++)
    {
      //_messenger << "StiMasterDetectorBuilder::next() - INFO - Calling Group Builder named:" << (*iter)->getName()<<endl;
      if((*iter)->hasMore()) return (*iter)->next();
    }
  return 0;
}
