#include "StiObjectFactory.h"

int StiObjectFactory::defaultOriginalSize      = 10000;
int StiObjectFactory::defaultIncrementSize     =  5000;
int StiObjectFactory::defaultMaxIncrementCount =     2;

StiObjectFactory::StiObjectFactory( const char *newName,
				    int original, 
				    int incremental, 
				    int  max) : StiFactory(newName)
{
  //------------------------------------------------------------------
  // Constructor, sets the original size of the container, the incremental size of 
  // the container used whenever its must be enlarged, and an upper limit on the
  // number of times the container can be enlarged - as a safety feature to limit 
  // undue increase of the container size
  //------------------------------------------------------------------

  originalSize          = original>0    ? original    : defaultOriginalSize;
  incrementalSize       = incremental>0 ? incremental : defaultIncrementSize;
  maxIncrementCount     = max>0         ? max         : defaultMaxIncrementCount;
  currentSize      = 0;
  nextObjectIndex  = 0;
  incrementCount   = 0;
  container = new TObjArray(originalSize);
  if (container)
    currentSize = container->GetSize();
}

StiObjectFactory::~StiObjectFactory()
{
  //------------------------------------------------------------------
  // D-tor
  // Delete the object container
  //------------------------------------------------------------------
  delete container;
}

TObject * StiObjectFactory::getObject()
{
  //------------------------------------------------------------------
  // serve the next object
  //------------------------------------------------------------------
  if (nextObjectIndex<originalSize)
    {
      // return next object available
      return (*container)[nextObjectIndex++];
    }
  else
    { 
      if (incrementCount< maxIncrementCount)
	{
	  // expand container size
	  container->Expand(currentSize+incrementalSize);
	  currentSize = container->GetSize();
	  createObjects(incrementalSize);
	  return (*container)[nextObjectIndex++];
	}
      else
	{
	  // s.o.l.
	  cout << "StiObjectFactory::getObject() - FATAL" << endl
	       << "     Too many expension request " << endl
	       << "     incrementCount : " << incrementCount << endl;
	  return 0;
	}
    }
}

void StiObjectFactory::reset()
{
  //----------------------------------------------------------
  // Declare all objects owned by this container as unused.
  //----------------------------------------------------------
  nextObjectIndex    = 0; 
}

void StiObjectFactory::setIncrementalSize(int increment)
{
  //------------------------------------------------------------------
  // Set the increment used in further expension of the container
  //------------------------------------------------------------------
  incrementalSize = increment;
}

void StiObjectFactory::setMaxIncrementCount(int maxCount)
{
  //------------------------------------------------------------------
  // Set the max number of permitted expension
  //------------------------------------------------------------------
  maxIncrementCount = maxCount;
}

int StiObjectFactory::getOriginalSize()
{
  //------------------------------------------------------------------
  // Get the original size of the container
  //------------------------------------------------------------------
  return originalSize;
}

int StiObjectFactory::getIncrementalSize()
{
  //------------------------------------------------------------------
  // Get the value of the increment used in further expensions of
  // the container
  //------------------------------------------------------------------
  return incrementalSize;
}

int StiObjectFactory::getMaxIncrementCount()
{
  //------------------------------------------------------------------
  // Get the max number of permitted expension
  //------------------------------------------------------------------
  return maxIncrementCount;
}

int StiObjectFactory::getCurrentSize()
{
  //------------------------------------------------------------------
  // Get the current size of the container
  //------------------------------------------------------------------
  return currentSize;
}

int StiObjectFactory::getNextObjectIndex()
{
  //------------------------------------------------------------------
  // Get the index  of the next object (within the container) to be 
  // served by the factory
  //------------------------------------------------------------------
  return nextObjectIndex;
}

