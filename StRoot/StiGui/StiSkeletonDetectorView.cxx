#include "StiGui/StiSkeletonDetectorView.h"
#include "StiGui/StiRootDrawableDetector.h"

StiSkeletonDetectorView::StiSkeletonDetectorView(const string & name, 
						     const string & description, 
						     StiDetectorBuilder*builder)
  : StiDetectorView(name,description,builder)
{}

StiSkeletonDetectorView::~StiSkeletonDetectorView()
{}

void StiSkeletonDetectorView::activate()
{
  StiDetector * detector;
  StiRootDrawableDetector* rootDrawableDetector;
  int nRows = getBuilder()->getNRows();
  for (int row=0;row<nRows;row++)
    {
      int nSectors = getBuilder()->getNSectors(row);
      for (int sector=0;sector<nSectors;sector++)
	{
	  detector = getBuilder()->getDetector(row,sector);
	  if (detector)
	    {
	      rootDrawableDetector = static_cast<StiRootDrawableDetector*>(detector);
	      if (rootDrawableDetector)
		{
		  if (row==0 || row==nRows-1)
		    rootDrawableDetector->setVisible(true);
		  else
		    rootDrawableDetector->setVisible(false);
		}
	    }
	}
    }
}


