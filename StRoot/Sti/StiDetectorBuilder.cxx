#include <fstream.h>
#include "Sti/Base/Messenger.h"
#include "Sti/Base/MessageType.h"
#include "Sti/StiDetector.h"
#include "Sti/StiShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiToolkit.h"
#include "StThreeVector.hh"

StiDetectorBuilder::StiDetectorBuilder(const string & name,bool active)
  : Named(name+"Builder"),
    _groupId(-1),
    _active(active),
    _detectorFactory( StiToolkit::instance()->getDetectorFactory() ),
    _trackingParameters(new StiTrackingParameters(name,name)),
    _messenger(*Messenger::instance(MessageType::kDetectorMessage) )
{
  _messenger << "StiDetectorBuilder::StiDetectorBuilder() - INFO - Instantiating builder named:"<<name<<endl;
  //MessageType::getTypeByCode(MessageType::kDetectorMessage)->setOstream(new ofstream("DetectorMessageFile"));
}

StiDetectorBuilder::~StiDetectorBuilder()
{}

bool StiDetectorBuilder::hasMore() const 
{
  //_messenger<<"StiDetectorBuilder::hasMore() - INFO - Started"<<endl;
  return mDetectorIterator != mDetectorMap.end();
} // hasMore()

StiDetector * StiDetectorBuilder::next()
{
  //_messenger<<"StiDetectorBuilder::hasMore() - INFO - Started"<<endl;
  if (mDetectorIterator != mDetectorMap.end())
    return (mDetectorIterator++)->second;
  else 
    return 0;
} // next()

StiMaterial* StiDetectorBuilder::findMaterial(const string& szName) const
{
  materialMap::const_iterator where = mMaterialMap.find(NameMapKey(szName));
  return (where!= mMaterialMap.end()) ? (*where).second : 0;
} // findMaterial()

StiShape* StiDetectorBuilder::findShape(const string& szName) const
{
  shapeMap::const_iterator where = mShapeMap.find(NameMapKey(szName));
  return (where!=mShapeMap.end()) ? (*where).second: 0;
} // findShape()

StiDetector* StiDetectorBuilder::findDetector(const string& szName) const
{
  detectorMap::const_iterator where = mDetectorMap.find(NameMapKey(szName));
  return (where!=mDetectorMap.end()) ? (*where).second: 0;
} // findDetector()

StiMaterial * StiDetectorBuilder::add(StiMaterial *material)
{  
  NameMapKey key(material->getName());
  mMaterialMap.insert( materialMapValType(key,material) );
  return material;
}

StiShape * StiDetectorBuilder::add(StiShape *shape)
{
  NameMapKey key(shape->getName());
  mShapeMap.insert( shapeMapValType(key, shape) );
	return shape;
}

StiDetector * StiDetectorBuilder::add(unsigned int row, unsigned int sector, StiDetector *detector)
{
  if (row>_nRows)
    {
      string message = "StiDetectorBuilder::add() - ERROR - argument row out of bound:";
      message+=row;
      message+=">";
      message+=_nRows;
      throw runtime_error(message.c_str());
    }
  if (sector>_nSectors[row])
    {
      string message = "StiDetectorBuilder::add() - ERROR - argument sector out of bound";
      throw runtime_error(message.c_str());
    }
  //_messenger<<"StiDetectorBuilder::add() - INFO - Row:"<<row<<" Sector:"<<sector<<" Name:"<<detector->getName()<<endl;
  _detectors[row][sector] = detector;
  return add(detector);
}

/*! Add the given detector to the list of detectors known to this builder.
    Complete the "build" of this detector. 
 */

StiDetector * StiDetectorBuilder::add(StiDetector *detector)
{
  NameMapKey key(detector->getName());
  mDetectorMap.insert( detectorMapValType(key, detector) );
  //complete the building of this detector element
  // in the base class nothing is actually done
  // but ROOT stuff is built in the drawable version of this class.
  detector->setGroupId(_groupId);
  detector->setTrackingParameters(_trackingParameters);
  return detector;
}


void StiDetectorBuilder::build()
{
  loadDb();
  buildMaterials();
  buildShapes();
  buildDetectors();
  mDetectorIterator = mDetectorMap.begin();
}

void StiDetectorBuilder::buildMaterials()
{
  /* base class does nothing */
}

void StiDetectorBuilder::buildShapes()
{
  /* base class does nothing */
}

void StiDetectorBuilder::buildDetectors()
{
  /* base class does nothing */
}
void StiDetectorBuilder::loadDb()
{}

