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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */
#include "SmallChange.h"
#include "SoText2Set.h"
#include "SmAxisKit.h"

#ifdef ENABLE_FULL_SMALL_CHANGE

#include <SmallChange/misc/Init.h>
#include <SmallChange/SmallChange.h>
#include <SmallChange/elements/GLDepthBufferElement.h>
#include <SmallChange/nodes/AutoFile.h>
#include <SmallChange/nodes/Coinboard.h>
#include <SmallChange/nodes/SmDepthBuffer.h>
#include <SmallChange/nodes/ViewportRegion.h>
#include <SmallChange/nodes/SmSwitchboard.h>
#include <SmallChange/nodes/SmSwitchboardOperator.h>
#include <SmallChange/nodes/SoLODExtrusion.h>
#include <SmallChange/nodes/SoPointCloud.h>
#include <SmallChange/nodes/SoTCBCurve.h>
#include <SmallChange/nodes/SoText2Set.h>
#include <SmallChange/nodes/SmVertexArrayShape.h>
#include <SmallChange/actions/SmToVertexArrayShapeAction.h>
#include <SmallChange/actions/SoTweakAction.h>
#include <SmallChange/actions/SoGenerateSceneGraphAction.h>
#include <SmallChange/engines/Rot2Heading.h>
#include <SmallChange/engines/SmInverseRotation.h>
#include <SmallChange/nodekits/LegendKit.h>
#include <SmallChange/nodekits/SoFEMKit.h>
#include <SmallChange/nodekits/SmTooltipKit.h>
#include <SmallChange/nodekits/SmCameraControlKit.h>
#include <SmallChange/nodekits/SmDynamicObjectKit.h>
#include <SmallChange/eventhandlers/SmExaminerEventHandler.h>
#include <SmallChange/eventhandlers/SmHelicopterEventHandler.h>
#include <SmallChange/eventhandlers/SmSphereEventHandler.h>
#include <SmallChange/eventhandlers/SmPanEventHandler.h>

#include <SmallChange/nodes/CoinEnvironment.h>
#include <SmallChange/nodes/PickCallback.h>
#include <SmallChange/nodes/PickSwitch.h>
#include <SmallChange/nodes/ShapeScale.h>
#include <SmallChange/nodes/SkyDome.h>
#include <SmallChange/nodes/SmTooltip.h>
#include <SmallChange/nodes/SmHQSphere.h>
#include <SmallChange/engines/CubicSplineEngine.h>

#include <SmallChange/elements/UTMElement.h>
#include <SmallChange/nodes/UTMCamera.h>
#include <SmallChange/nodes/UTMPosition.h>
#include <SmallChange/nodes/UTMCoordinate.h>

#include <SmallChange/nodekits/SmWellLogKit.h>
#include <SmallChange/nodekits/SmGeoMarkerKit.h>
#include <SmallChange/nodes/SmBillboardClipPlane.h>
#include <SmallChange/nodekits/SmNormalsKit.h>
#include <SmallChange/nodekits/SmAxisDisplayKit.h>
#include <SmallChange/nodekits/SmAxisKit.h>
#include <SmallChange/nodes/SmHeadlight.h>
#include <SmallChange/draggers/SmRangeTranslate1Dragger.h>
#include <SmallChange/nodes/SmMarkerSet.h>
#include <SmallChange/nodes/SmCoordinateSystem.h>
#include <SmallChange/nodes/SmViewpointWrapper.h>
#include <SmallChange/nodekits/SmPopupMenuKit.h>
#include <SmallChange/nodekits/SmTrackPointKit.h>
#include <SmallChange/nodes/SmTrack.h>
#include <SmallChange/nodes/SmLazyFile.h>
#include <SmallChange/nodekits/SmAnnotationWall.h>
#include <SmallChange/nodekits/SmAnnotationAxis.h>

#include <SmallChange/nodekits/SmPieChart.h>
#include <SmallChange/nodes/SmTextureText2.h>
#include <SmallChange/nodes/SmTextureText2Collector.h>
#include <SmallChange/nodes/SmTextureFont.h>
#include <SmallChange/nodes/SmShadowText2.h>
#endif 

namespace {
void
smallchange_init(void)
{

  SmAxisKit::initClass();
  SoText2Set::initClass();

#ifdef ENABLE_FULL_SMALL_CHANGE
  AutoFile::initClass();
  GLDepthBufferElement::initClass();
  Coinboard::initClass();
  SmDepthBuffer::initClass();
  ViewportRegion::initClass();
  SmSwitchboard::initClass();
  SmSwitchboardOperator::initClass();
  Rot2Heading::initClass();
  SmInverseRotation::initClass();
  LegendKit::initClass();
  SoFEMKit::initClass();
  SmTooltipKit::initClass();
  SoLODExtrusion::initClass();
  SoPointCloud::initClass();
  SoTCBCurve::initClass();
  SoText2Set::initClass();
  SkyDome::initClass();
  CoinEnvironment::initClass();
  PickCallback::initClass();
  PickSwitch::initClass();
  ShapeScale::initClass();
  SoTweakAction::initClass();
  SoGenerateSceneGraphAction::initClass();
  SmTooltip::initClass();

  CubicSplineEngine::initClass();

  UTMElement::initClass();
  UTMPosition::initClass();
  UTMCamera::initClass();
  UTMCoordinate::initClass();

  SmCameraControlKit::initClass();
  SmEventHandler::initClass();
  SmExaminerEventHandler::initClass();
  SmSphereEventHandler::initClass();
  SmHelicopterEventHandler::initClass();
  SmPanEventHandler::initClass();

  SmWellLogKit::initClass();
  SmHQSphere::initClass();
  SmGeoMarkerKit::initClass();
  SmBillboardClipPlane::initClass();
  SmNormalsKit::initClass();
  SmAxisDisplayKit::initClass();
  SmAxisKit::initClass();
  SmHeadlight::initClass();
  SmRangeTranslate1Dragger::initClass();
  SmMarkerSet::initClass();

  SmVertexArrayShape::initClass();
  SmToVertexArrayShapeAction::initClass();

  SmCoordinateSystem::initClass();
  SmViewpointWrapper::initClass();
  SmPopupMenuKit::initClass();
  SmLazyFile::initClass();

  SmPieChart::initClass();
#if defined(__COIN__) && COIN_MAJOR_VERSION >= 3
  SmDynamicObjectKit::initClass();
  SmTrackPointKit::initClass();
  SmTrack::initClass();
#endif // temporary compile fix

  SmTextureFontElement::initClass();
  SmTextureFont::initClass();

  SmTextureText2::initClass();
  SmShadowText2::initClass();

  SmTextureText2CollectorElement::initClass();
  SmTextureText2Collector::initClass();

  SmAnnotationWall::initClass();
  SmAnnotationAxis::initClass();
#endif
}
}
void SmallChange::init(void)
{
  smallchange_init();
}

void SmallChange::cleanup(void)
{
  // FIXME: Implement
}
