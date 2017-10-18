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
#include "StiVMC/StiToolkit.h"
#include "StiVMC/StiLocalTrackSeedFinder.h"

/** 
 * @class StiDefaultToolkit
 * @brief Definition of toolkit
 */
class StiDefaultToolkit : public StiToolkit
{
public:
  
  StiDefaultToolkit() ;
  virtual Factory<StiHit>            *HitFactory()         {return _hitFactory	       ?_hitFactory	    :_hitFactory	  = GetHitFactory();}	   
  virtual Factory<StiKalmanTrack>    *TrackFactory()	   {return _trackFactory       ?_trackFactory       :_trackFactory	  = GetTrackFactory();}        
  virtual Factory<StiDetector>       *DetectorFactory()    {return _detectorFactory    ?_detectorFactory    :_detectorFactory     = GetDetectorFactory();}    
  virtual Factory<StiDetectorNode >  *DetectorNodeFactory(){return _detectorNodeFactory?_detectorNodeFactory:_detectorNodeFactory = GetDetectorNodeFactory();}
  virtual Factory<StiKalmanTrackNode>*TrackNodeFactory()   {return _trackNodeFactory   ?_trackNodeFactory   :_trackNodeFactory    = GetTrackNodeFactory();}   
  virtual StiDetectorContainer       *DetectorContainer()  {return _detectorContainer  ?_detectorContainer  :_detectorContainer   = GetDetectorContainer();}  
  virtual StiHitContainer            *HitContainer()	   {return _hitContainer       ?_hitContainer       :_hitContainer	  = GetHitContainer();}        
  virtual StiKalmanTrackContainer    *TrackContainer()     {return _trackContainer     ?_trackContainer     :_trackContainer      = GetTrackContainer();}     
  virtual StiLocalTrackSeedFinder    *TrackSeedFinder()    {return _trackSeedFinder    ?_trackSeedFinder    :_trackSeedFinder     = GetTrackSeedFinder();}    
  virtual StiVertexFinder            *VertexFinder()	   {return _vertexFinder       ?_vertexFinder       :_vertexFinder        = GetVertexFinder();}       
  Factory<StiHit>                    *GetHitFactory();	       
  Factory<StiKalmanTrack>    	     *GetTrackFactory();       
  Factory<StiDetector>       	     *GetDetectorFactory();    
  Factory<StiDetectorNode >  	     *GetDetectorNodeFactory();
  Factory<StiKalmanTrackNode>	     *GetTrackNodeFactory();   
  StiDetectorContainer       	     *GetDetectorContainer();  
  StiHitContainer            	     *GetHitContainer();       
  StiKalmanTrackContainer    	     *GetTrackContainer();     
  StiLocalTrackSeedFinder    	     *GetTrackSeedFinder();    
  StiVertexFinder              	     *GetVertexFinder();       
  virtual int Truth(const StiHit *hit);
  
 protected:
  virtual ~StiDefaultToolkit();
  
  Factory<StiHit>              *_hitFactory;	      
  Factory<StiKalmanTrack>      *_trackFactory;	      
  Factory<StiDetector>         *_detectorFactory;     
  Factory< StiDetectorNode >   *_detectorNodeFactory; 
  Factory<StiKalmanTrackNode>  *_trackNodeFactory;    
  StiDetectorContainer         *_detectorContainer;   
  StiHitContainer              *_hitContainer;	      
  StiKalmanTrackContainer      *_trackContainer;      
  StiLocalTrackSeedFinder      *_trackSeedFinder;     
  StiVertexFinder              *_vertexFinder;        

};

#endif


