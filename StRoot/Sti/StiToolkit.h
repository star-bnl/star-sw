/** 
 * @file  StiToolkit.h
 * @brief Abstract interface for a STI toolkit
 * @author Claude A Pruneau, Wayne State University, 
 * @date   March 2001
 * @copyright 2001, STAR  Experiment at BNL, All rights reserved.  
 *  
 * Permission to use, copy, modify and distribute this software and its
 * documentation strictly for non-commercial purposes is hereby granted 
 * without fee, provided that the above copyright notice appears in all
 * copies and that both the copyright notice and this permission notice
 * appear in the supporting documentation. The authors make no claims 
 * about the suitability of this software for any purpose. It is     
 * provided "as is" without express or implied warranty.             
 */
#ifndef StiToolkit_H
#define StiToolkit_H 1

#include <string>
using std::string;
#include "StiFactoryTypes.h"

// common object containers
class 	StiDetectorContainer;
class 	StiHitContainer;
class 	StiHitFiller;
class 	StiTrackContainer;
// service and convenience class objects.
class 	StiGeometryTransform;
class 	StiCoordinateTransform;
class 	StiDetectorFinder;
class 	StiSeedFinder;
class 	StiTrackFinder;
class 	StiTrackFilter;
class 	StiTrackFitter;
class 	StiTrackMerger;
//class 	StiDisplayManager;
//class 	StiEvaluator;
//class 	StiEventAssociator;
class   StiIOBroker;
class   StiDisplayManager;
class   StAssociationMaker;

/** 
 * @class StiToolkit
 * @brief Definition of toolkit
 */
class StiToolkit 
{
public:
  
 	virtual StiObjectFactoryInterface<StiHit> * getHitFactory()=0;
	//virtual StiTrackFactory * getTrackFactory()=0;
	virtual StiObjectFactoryInterface<StiKalmanTrack> * getTrackFactory()=0;
	virtual StiObjectFactoryInterface<StiDetector>  * getDetectorFactory()=0;
	virtual StiObjectFactoryInterface<StiDetectorNode>  * getDetectorNodeFactory()=0;
	virtual StiObjectFactoryInterface<StiKalmanTrackNode> * getTrackNodeFactory()=0;
	
	// common object containers
	virtual StiDetectorContainer  * getDetectorContainer()=0;
	virtual StiHitContainer       * getHitContainer()=0;
	virtual StiTrackContainer     * getTrackContainer()=0;

	// service and convenience class objects.
	virtual StiGeometryTransform * getGeometryTransform()=0;
	virtual StiCoordinateTransform * getCoordinateTransform()=0;
	virtual StiDetectorFinder    * getDetectorFinder()=0;
	virtual StiSeedFinder        * getTrackSeedFinder()=0;
	virtual StiTrackFinder       * getTrackFinder()=0;
	virtual StiTrackFilter       * getTrackFilter()=0;
	virtual StiTrackFitter       * getTrackFitter()=0;
	virtual StiTrackMerger       * getTrackMerger()=0;
	virtual StiDisplayManager    * getDisplayManager()=0;
	//	virtual StiEvaluator         * getEvaluator(const string&)=0;
	//virtual StiEvaluator         * getEvaluator()=0;
	//virtual StiEventAssociator   * getEventAssociator()=0;
	virtual StiIOBroker * getIOBroker()=0;
	virtual StAssociationMaker * getAssociationMaker()=0;
	virtual void setAssociationMaker(StAssociationMaker * a)=0;
	virtual StiHitFiller * getHitFiller()=0;
	
	static StiToolkit *instance();
	static void kill();

 protected:

	static StiToolkit * sInstance;
};

#endif

