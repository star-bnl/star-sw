// $Id: StDetectorGeometryInterface.h,v 1.1 2009/12/01 01:33:33 fine Exp $

#ifndef STAR_StDetectorGeometryInterface
#define STAR_StDetectorGeometryInterface

/*!
 *                                                                     
 * \class  StDetectorGeometryInterface
 * \author  Fine
 * \date   2004/11/17
 * \brief  Online display module
 *
 * This commented block at the top of the header file is considered as
 * the class description to be present on the this class Web page. 
 *
 * 
 * StDetectorGeometryInterface virtual base class for Maker                        
 * Template Maker doing nothing. See README file in StRoot/StDetectorGeometryInterface
 *
 *
 */                                                                      

class StDetectorGeometryInterface  {

 public: 
  StDetectorGeometryInterface(){;}
  virtual       ~StDetectorGeometryInterface(){;}
  
   virtual void   AddVolume(const char *name)      =0;
   virtual Int_t  BuildGeometry()                  =0;
   virtual void   Modified()                       =0;
   virtual void   PrintVolumes()                   =0;
   virtual void   RemoveVolume(const char *name)   =0;
   virtual void   SetFileName(const char* fileName)=0;
};
#endif
