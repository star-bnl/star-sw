#include <stdexcept>
#include <Stiostream.h>
#include "StiView.h"
#include "Sti/StiDetectorContainer.h"
#include "StiGui/StiRootDrawableDetector.h"
#include "StiGui/StiDrawable.h"
#include "Sti/StiToolkit.h"

void StiZoomSkeletonView::setToDefault()
{
    StiDetectorContainer& rdet = *(StiToolkit::instance()->getDetectorContainer());
    StiRootDrawableDetector* layer = dynamic_cast<StiRootDrawableDetector*>(*rdet);
    if (!layer) {
	cout <<"Error! MainFrame::setCurrentDetectorToDefault():";
	cout <<"Failed to get drawable detector"<<endl;
	return;
    }
    layer->setColor(1);
    
    //Keep all silicon layers visible
    const string& name = layer->getName();
    string::size_type where = name.find("Svt");

    if ( where != name.npos && layer->isOn() ) {
	layer->setVisible(true);
	return;
    }

    //Keep Tpc layer 45 visible
    where = name.find("Tpc");
    string::size_type where2 = name.find("Padrow_0/");
    if (where!=name.npos && where2!=name.npos && layer->isOn()) {
	layer->setVisible(true);
    }
    else {    //else, hide!
	layer->setVisible(false);
    }
}

void StiSkeletonView::setToDefault()
{
  StiDetectorContainer& rdet = *(StiToolkit::instance()->getDetectorContainer());
    StiRootDrawableDetector* layer = dynamic_cast<StiRootDrawableDetector*>(*rdet);
    if (!layer) {
	cout <<"Error! MainFrame::setCurrentDetectorToDefault() Error:\t";
	cout <<"Failed to get drawable detector"<<endl;
	return;
    }
    layer->setColor(1);
    
    //Keep all active silicon layers visible
    const string& name = layer->getName();
    string::size_type where = name.find("Svt");
    
    if ( where != name.npos && layer->isOn() ) {
	layer->setVisible(true);
	return;
    }

    //Keep Tpc layer 45 visible
    where = name.find("Tpc");
    string::size_type where2 = name.find("Ofc");
    if (where!=name.npos && where2!=name.npos && layer->isOn()) {
	layer->setVisible(true);
    }
    else {    //else, hide!
	layer->setVisible(false);
    }
}

void StiManualView::setToDefault()
{
    StiDetectorContainer& rdet = *(StiToolkit::instance()->getDetectorContainer());
    StiRootDrawableDetector* layer = dynamic_cast<StiRootDrawableDetector*>(*rdet);
    if (!layer) {
	cout <<"Error! MainFrame::setCurrentDetectorToDefault():";
	cout <<"Failed to get drawable detector"<<endl;
	return;
    }
    layer->setColor(1);
}
