#include "StiGui/StiActiveDetectorView.h"
#include "StiGui/StiRootDrawableDetector.h"

StiActiveDetectorView::StiActiveDetectorView(const string & name, 
					     const string & description, 
					     StiDetectorBuilder*builder)
  : StiDetectorView(name,description,builder)
{}

StiActiveDetectorView::~StiActiveDetectorView()
{}

void StiActiveDetectorView::activate()
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
	  if (detector && detector->isActive() )
	    {
	      rootDrawableDetector = static_cast<StiRootDrawableDetector*>(detector);
	      if (rootDrawableDetector)
		{
		  rootDrawableDetector->setVisible(true);
		}
	    }
	}
    }
}


