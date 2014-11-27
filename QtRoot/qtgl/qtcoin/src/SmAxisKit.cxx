/**************************************************************************\
 *
 *  This file is part of the SmallChange extension library for Coin.
 *  Copyright (C) 1998-2003 by Systems in Motion.  All rights reserved.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  ("GPL") version 2 as published by the Free Software Foundation.
 *  See the file LICENSE.GPL at the root directory of this source
 *  distribution for additional information about the GNU GPL.
 *
 *  For using SmallChange with software that can not be combined with the
 *  GNU GPL, and for taking advantage of the additional benefits of our
 *  support services, please contact Systems in Motion about acquiring
 *  a Coin Professional Edition License.
 *
 *  See <URL:http://www.coin3d.org> for  more information.
 *
 *  Systems in Motion, Teknobyen, Abels Gate 5, 7030 Trondheim, NORWAY.
 *  <URL:http://www.sim.no>.
 *
\**************************************************************************/

#include "SmAxisKit.h"

#include <Inventor/nodekits/SoShapeKit.h>
#include <Inventor/nodes/SoTranslation.h>
#include <Inventor/nodes/SoBaseColor.h>
#include <Inventor/nodes/SoSeparator.h>
#include <Inventor/nodes/SoComplexity.h>
#include <Inventor/sensors/SoFieldSensor.h>
#include <Inventor/nodes/SoLineSet.h>
#include <Inventor/nodes/SoCoordinate3.h>
#include <Inventor/nodes/SoDrawStyle.h>
#include <Inventor/nodes/SoCube.h>
#include <Inventor/errors/SoDebugError.h>

// #include <SmallChange/nodes/SoText2Set.h>
#include "SoText2Set.h"

// *************************************************************************

SO_KIT_SOURCE(SmAxisKit);

// *************************************************************************

class SmAxisKitP {

public:
  SmAxisKitP(SmAxisKit * master) {
    this->master = master;
  }

  SoFieldSensor * axisRangeSensor;
  SoFieldSensor * markerIntervalSensor;
  SoFieldSensor * markerHeightSensor;
  SoFieldSensor * textIntervalSensor;
  SoFieldSensor * digitsSensor;

  SoSeparator * axisRoot;
  SoSeparator * generateAxis(void) const;
  SoSeparator * setupMasterNodes(void) const;

private:
  SmAxisKit * master;
};

#define PRIVATE(p) ((p)->pimpl)
#define PUBLIC(p) ((p)->master)

// *************************************************************************

static void fieldsChangedCallback(void * classObject, SoSensor * sensor);

// *************************************************************************

SmAxisKit::SmAxisKit()
{
  PRIVATE(this) = new SmAxisKitP(this);

  SO_KIT_CONSTRUCTOR(SmAxisKit);

#if defined(__COIN__)
  SO_KIT_ADD_CATALOG_ENTRY(topSeparator, SoSeparator, TRUE, this, "", TRUE);
#else
#define EMPTY \x0 // Inventor doesn't handle ""...
  SO_KIT_ADD_CATALOG_ENTRY(topSeparator, SoSeparator, TRUE, this, EMPTY, TRUE);
#undef EMPTY
#endif // OIV kit specification


  SO_KIT_ADD_FIELD(axisRange, (SbVec2f(0.0f, 30.0f)));
  SO_KIT_ADD_FIELD(markerInterval, (1.0f));
  SO_KIT_ADD_FIELD(markerHeight, (0.2f));
  SO_KIT_ADD_FIELD(textInterval, (5.0f));
  SO_KIT_ADD_FIELD(digits, (2));
  SO_KIT_ADD_FIELD(axisName, (SbString("")));

  SO_KIT_INIT_INSTANCE();

  PRIVATE(this)->axisRoot = new SoSeparator;
  PRIVATE(this)->axisRoot->ref();

  PRIVATE(this)->axisRoot->addChild(PRIVATE(this)->generateAxis());

  setPart("topSeparator", PRIVATE(this)->axisRoot);

  PRIVATE(this)->axisRangeSensor = new SoFieldSensor(fieldsChangedCallback,PRIVATE(this));
  PRIVATE(this)->axisRangeSensor->setPriority(0);
  PRIVATE(this)->axisRangeSensor->attach(&this->axisRange);

  PRIVATE(this)->markerIntervalSensor = new SoFieldSensor(fieldsChangedCallback,PRIVATE(this));
  PRIVATE(this)->markerIntervalSensor->setPriority(0);
  PRIVATE(this)->markerIntervalSensor->attach(&this->markerInterval);

  PRIVATE(this)->markerHeightSensor = new SoFieldSensor(fieldsChangedCallback, PRIVATE(this));
  PRIVATE(this)->markerHeightSensor->setPriority(0);
  PRIVATE(this)->markerHeightSensor->attach(&this->markerHeight);

  PRIVATE(this)->textIntervalSensor = new SoFieldSensor(fieldsChangedCallback,PRIVATE(this));
  PRIVATE(this)->textIntervalSensor->setPriority(0);
  PRIVATE(this)->textIntervalSensor->attach(&this->textInterval);

  PRIVATE(this)->digitsSensor = new SoFieldSensor(fieldsChangedCallback,PRIVATE(this));
  PRIVATE(this)->digitsSensor->setPriority(0);
  PRIVATE(this)->digitsSensor->attach(&this->digits);
}

SmAxisKit::~SmAxisKit()
{
  delete PRIVATE(this)->axisRangeSensor;
  delete PRIVATE(this)->markerIntervalSensor;
  delete PRIVATE(this)->textIntervalSensor;
  delete PRIVATE(this)->digitsSensor;

  PRIVATE(this)->axisRoot->unref();
}

void
SmAxisKit::initClass(void)
{
  SO_KIT_INIT_CLASS(SmAxisKit, SoBaseKit, "BaseKit");
}

// *************************************************************************

SbBool
SmAxisKit::affectsState(void) const
{
  return FALSE;
}

// *************************************************************************

SoSeparator *
SmAxisKitP::setupMasterNodes(void) const
{
  float range = (PUBLIC(this)->axisRange.getValue()[1] - PUBLIC(this)->axisRange.getValue()[0]);

  // Axis
  SoSeparator * masterAxis = new SoSeparator;
  SoSeparator * sep1 = new SoSeparator;
  SoTranslation * trans1 = new SoTranslation;
  SoTranslation * centerTrans = new SoTranslation;
  SoBaseColor * axisColor = new SoBaseColor;

  SoCoordinate3 * axisCoords = new SoCoordinate3;
  SoLineSet * axisLine = new SoLineSet;
  axisLine->numVertices = 2;
  axisCoords->point.set1Value(0, 0.0f, 0.0f, 0.0f);
  axisCoords->point.set1Value(1, range, 0.0f, 0.0f);

  axisColor->rgb.setValue(1.0f, 1.0f, 1.0f);
  trans1->translation.setValue(range/2, 0.0f, 0.0f);
  centerTrans->translation.setValue(-range/2, 0.0f, 0.0f);

  sep1->addChild(centerTrans);
  sep1->addChild(axisColor);
  sep1->addChild(axisCoords);
  sep1->addChild(axisLine);

  SoSeparator * axisnamesep = new SoSeparator;
  SoTranslation * nameTrans = new SoTranslation;
  nameTrans->translation.setValue((SbAbs(range)/2)*1.05f, 0.0f, 0.0f);

  SoText2Set * newaxisname = new SoText2Set;
  newaxisname->renderOutline = TRUE;
  newaxisname->string.connectFrom(&PUBLIC(this)->axisName);

  axisnamesep->addChild(axisColor);
  axisnamesep->addChild(nameTrans);
  axisnamesep->addChild(newaxisname);

  masterAxis->addChild(sep1);
  masterAxis->addChild(axisnamesep);
  return masterAxis;
}

SoSeparator *
SmAxisKitP::generateAxis(void) const
{
  SoSeparator * root = new SoSeparator;
  float range = (PUBLIC(this)->axisRange.getValue()[1] - PUBLIC(this)->axisRange.getValue()[0]);
  SbBool rangeDecreasing = FALSE;
  if (range < 0)
    rangeDecreasing = TRUE;

  range = SbAbs(range);


  SoTranslation * centerTrans = new SoTranslation;
  centerTrans->translation.setValue(-range/2, 0.0f, 0.0f);

  root->addChild(this->setupMasterNodes());

  // Markers
  SoSeparator * sep3 = new SoSeparator;
  SoLineSet * marker1 = new SoLineSet;
  SoLineSet * marker2 = new SoLineSet;
  SoCoordinate3 * markerCoords1 = new SoCoordinate3;
  SoCoordinate3 * markerCoords2 = new SoCoordinate3;
  SoTranslation * mtrans1 = new SoTranslation;
  // SoTranslation * mtrans2 = new SoTranslation;
  SoBaseColor * markerColor = new SoBaseColor;

  markerCoords1->point.set1Value(0, 0.0f, 0.0f, 0.0f);
  markerCoords1->point.set1Value(1, 0.0f, PUBLIC(this)->markerHeight.getValue(), 0.0f);
  markerCoords2->point.set1Value(0, 0.0f, 0.0f, 0.0f);
  markerCoords2->point.set1Value(1, 0.0f, PUBLIC(this)->markerHeight.getValue() * 2, 0.0f);

  marker1->numVertices = 2;
  marker2->numVertices = 2;

  mtrans1->translation.setValue(PUBLIC(this)->markerInterval.getValue(), 0.0f, 0.0f);

  markerColor->rgb.setValue(1.0f, 1.0f, 0.7f);

  sep3->addChild(centerTrans);
  sep3->addChild(markerColor);

  float pos = 0.0f;
  int counter = 0;


  assert(PUBLIC(this)->markerInterval.getValue() >= 0.0f);

  // Interval == 0 will skip all markers.
  if (PUBLIC(this)->markerInterval.getValue() > 0.0f) {

    while (pos <= range) {

      if (counter == 5) {
        sep3->addChild(markerCoords2);
        sep3->addChild(marker2);
        counter = 0;
      }
      else {
        sep3->addChild(markerCoords1);
        sep3->addChild(marker1);
      }

      sep3->addChild(mtrans1);
      pos += PUBLIC(this)->markerInterval.getValue();
      ++counter;
    }
    root->addChild(sep3);

  }


  // Text
  SoSeparator * textsep = new SoSeparator;
  SoTranslation * ttrans1 = new SoTranslation;
  SoTranslation * ttrans2 = new SoTranslation;
  ttrans1->translation.setValue(PUBLIC(this)->textInterval.getValue(), 0.0f, 0.0f);
  ttrans2->translation.setValue(0.0f, PUBLIC(this)->markerHeight.getValue() * 2.2f, 0.0f);

  SoBaseColor * markerTextColor = new SoBaseColor;
  markerTextColor->rgb.setValue(0.7f, 0.7f, 0.7f);

  textsep->addChild(markerTextColor);
  textsep->addChild(centerTrans);
  textsep->addChild(ttrans2);

  SbString tmpstr;
  const char * tmptext = (tmpstr.sprintf("%%.%df", PUBLIC(this)->digits.getValue())).getString();
  pos = 0.0f;
  float markerValue = 0;

  assert(PUBLIC(this)->textInterval.getValue() >= 0.0f);

  // Interval == 0 will skip all text markers.
  if (PUBLIC(this)->textInterval.getValue() > 0.0f) {
    while (pos <= range) {

      SbString mtext;
      SoText2Set * markerText = new SoText2Set;
      markerText->string.setValue(mtext.sprintf(tmptext, (markerValue + PUBLIC(this)->axisRange.getValue()[0])));

      textsep->addChild(markerText);
      textsep->addChild(ttrans1);

      pos += PUBLIC(this)->textInterval.getValue();

      if (rangeDecreasing)
        markerValue -= PUBLIC(this)->textInterval.getValue();
      else
        markerValue += PUBLIC(this)->textInterval.getValue();
    }
    root->addChild(textsep);

  }

  return root;
}

static void
fieldsChangedCallback(void * classObject, SoSensor * /*sensor*/ )
{
  SmAxisKitP * thisp = (SmAxisKitP *) classObject;  // Fetch caller object

  assert(thisp->axisRoot->getNumChildren() == 1);
  thisp->axisRoot->replaceChild(0, thisp->generateAxis());
}

// *************************************************************************
