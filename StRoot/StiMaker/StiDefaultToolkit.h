/** 
 * @file  StiDefaultToolkit.h
 * @brief Default Implementation of the StiToolkit Abstract interface
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
#ifndef StiDefaultToolkit_H
#define StiDefaultToolkit_H 1
#include "../Sti/StiToolkit.h"

/** 
 * @class StiDefaultToolkit
 * @brief Definition of toolkit
 */
class StiDefaultToolkit : public StiToolkit
{
public:
  
 	virtual StiObjectFactoryInterface<StiHit> * getHitFactory();
	//virtual StiTrackFactory * getTrackFactory();
	virtual StiObjectFactoryInterface<StiKalmanTrack> * getTrackFactory();
	virtual StiObjectFactoryInterface<StiDetector>  * getDetectorFactory();
	virtual StiObjectFactoryInterface<StiDetectorNode>  * getDetectorNodeFactory();
	virtual StiObjectFactoryInterface<StiKalmanTrackNode> * getTrackNodeFactory();
	
	// common object containers
	virtual StiDetectorContainer  * getDetectorContainer();
	virtual StiHitContainer       * getHitContainer();
	virtual StiTrackContainer     * getTrackContainer();

	// service and convenience class objects.
	virtual StiGeometryTransform * getGeometryTransform();
	virtual StiCoordinateTransform * getCoordinateTransform();
	virtual StiDetectorFinder    * getDetectorFinder();
	virtual StiSeedFinder        * getTrackSeedFinder();
	virtual StiTrackFinder       * getTrackFinder();
	virtual StiTrackFilter       * getTrackFilter();
	virtual StiTrackFitter       * getTrackFitter();
	virtual StiTrackMerger       * getTrackMerger();
	virtual StiDisplayManager    * getDisplayManager();
	//virtual StiEvaluator         * getEvaluator(const string& file);
	//virtual StiEvaluator         * getEvaluator();
	//virtual StiEventAssociator   * getEventAssociator();
	virtual StiIOBroker * getIOBroker();
	virtual StiHitFiller * getHitFiller();
	virtual StAssociationMaker * getAssociationMaker();
	virtual void setAssociationMaker(StAssociationMaker * a);
	
	virtual StiHitErrorCalculator * getHitErrorCalculator();
	
protected:

	friend class StiToolkit;

  /** 
   * Protected ctor
   */
  StiDefaultToolkit() ;

  /** 
   * Dtor. Must delete all objects currently allocated.
   */
  virtual ~StiDefaultToolkit();
  
  
  // small object factories
  StiObjectFactoryInterface<StiHit> * hitFactory;
  StiObjectFactoryInterface<StiKalmanTrack> * trackFactory;
  StiObjectFactoryInterface<StiDetector> * detectorFactory;
  StiObjectFactoryInterface<StiDetectorNode> * detectorNodeFactory;
  StiObjectFactoryInterface<StiKalmanTrackNode> * trackNodeFactory;
  
  // common object containers
  StiDetectorContainer      * detectorContainer;
  StiHitContainer           * hitContainer;
  StiTrackContainer         * trackContainer;
  
  // service and convenience class objects.
  StiGeometryTransform  * geometryTransform;
  StiCoordinateTransform  * coordinateTransform;
  StiDetectorFinder     * detectorFinder;
  StiSeedFinder         * trackSeedFinder;
  //StiTrackSeedFinder  * trackSeedFinder;
  StiTrackFilter        * trackFilter;
  StiTrackFinder        * trackFinder;
  StiTrackFitter        * trackFitter;
  StiTrackMerger        * trackMerger;
  StiDisplayManager     * displayManager;
  //StiEvaluator          * evaluator;
  //StiEventAssociator    * eventAssociator;
  StiIOBroker * ioBroker;
  StiHitFiller * hitFiller;
  StAssociationMaker * associationMaker; 
  StiHitErrorCalculator * hitErrorCalculator;
  
};

#endif

