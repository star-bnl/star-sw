#include "StiGui/StiAllVisibleDetectorView.h"
#include "StiGui/StiRootDrawableDetector.h"

StiAllVisibleDetectorView::StiAllVisibleDetectorView(const string & name, 
						     const string & description, 
						     StiDetectorBuilder*builder)
  : StiDetectorView(name,description,builder)
{}

StiAllVisibleDetectorView::~StiAllVisibleDetectorView()
{}

void StiAllVisibleDetectorView::activate()
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
		  rootDrawableDetector->setVisible(false);
		}
	    }
	}
    }
}


