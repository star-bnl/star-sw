// $Id: Link_Def.h,v 1.2 2009/01/04 17:46:14 kocolosk Exp $

/*****************************************************************************
 * @author Adam Kocoloski
 *
 * Used by Makefile to generate standalone library for reading trees.
 *****************************************************************************/

// StarClassLibrary
#pragma link C++ class StThreeVector<float>-;
#pragma link C++ class StThreeVector<double>-;
#pragma link C++ class StHelix+;
#pragma link C++ class StPhysicalHelix+;
#pragma link C++ function abs(const StThreeVector<float>&);                                      
#pragma link C++ function abs(const StThreeVector<double>&);                                      
#pragma link C++ function cross_product(const StThreeVector<float>&, const StThreeVector<float>&); 
#pragma link C++ function cross_product(const StThreeVector<float>&, const StThreeVector<double>&); 
#pragma link C++ function operator+ (const StThreeVector<float>&, const StThreeVector<float>&);    
#pragma link C++ function operator+ (const StThreeVector<float>&, const StThreeVector<double>&);    
#pragma link C++ function operator- (const StThreeVector<float>&, const StThreeVector<float>&);    
#pragma link C++ function operator- (const StThreeVector<float>&, const StThreeVector<double>&);    
#pragma link C++ function operator* (const StThreeVector<float>&, const StThreeVector<float>&);    
#pragma link C++ function operator* (const StThreeVector<float>&, const StThreeVector<double>&);    
#pragma link C++ function operator* (const StThreeVector<float>&, double);                     
#pragma link C++ function operator* (double, const StThreeVector<float>&);                     
#pragma link C++ function operator/ (const StThreeVector<float>&, double);                     
#pragma link C++ function operator<<(ostream&, const StThreeVector<double>&);                     
#pragma link C++ function operator>>(istream&, StThreeVector<double>&);                           
#pragma link C++ function abs(const StThreeVector<double>&);                                         
#pragma link C++ function cross_product(const StThreeVector<double>&, const StThreeVector<double>&);    
#pragma link C++ function cross_product(const StThreeVector<double>&, const StThreeVector<float>&);    
#pragma link C++ function operator+ (const StThreeVector<double>&, const StThreeVector<double>&);       
#pragma link C++ function operator+ (const StThreeVector<double>&, const StThreeVector<float>&);       
#pragma link C++ function operator- (const StThreeVector<double>&, const StThreeVector<float>&);       
#pragma link C++ function operator- (const StThreeVector<double>&, const StThreeVector<double>&);       
#pragma link C++ function operator* (const StThreeVector<double>&, const StThreeVector<float>&);       
#pragma link C++ function operator* (const StThreeVector<double>&, const StThreeVector<double>&);       
#pragma link C++ function operator* (const StThreeVector<double>&, double);                        
#pragma link C++ function operator* (double, const StThreeVector<double>&);                        
#pragma link C++ function operator/ (const StThreeVector<double>&, double);                        
#pragma link C++ function operator== (const StHelix&, const StHelix&);                         
#pragma link C++ function operator!= (const StHelix&, const StHelix&);                         
#pragma link C++ function operator<<(ostream&, const StHelix&);                         
#pragma link C++ typedef StThreeVectorF;
#pragma link C++ class StParticleDefinition+;
#pragma link C++ class StParticleTable+;

// St_base
#pragma link C++ class StObject-;
#pragma link C++ class StUUId-;
#pragma link C++ class StXRef-;
#pragma link C++ class StXRefMain-;

// StEvent
#pragma link C++ class StRunInfo+;
#pragma link C++ enum StDetectorId;

// StMiniMcEvent
#pragma link C++ class StMiniMcPair+;
#pragma link C++ class StTinyMcTrack+;
#pragma link C++ class StTinyRcTrack+;

// StChargedPionEvent
#pragma link C++ class std::vector< StChargedPionJetParticle >;
#pragma link C++ class std::vector< StChargedPionMcTrack >;
#pragma link C++ class std::vector< StChargedPionJet >;
#pragma link C++ class std::vector< StMiniMcPair >;
#pragma link C++ class std::vector< StChargedPionPythiaRow >;
#pragma link C++ class StChargedPionBaseEv+;
#pragma link C++ class StChargedPionEvent+;
#pragma link C++ class StChargedPionJet+;
#pragma link C++ class StChargedPionJetParticle+;
#pragma link C++ class StChargedPionMcEvent+;
#pragma link C++ class StChargedPionTrack+;
#pragma link C++ class StChargedPionPythiaRow+;
#pragma link C++ class StChargedPionVertex+;

/*****************************************************************************
 * $Log: Link_Def.h,v $
 * Revision 1.2  2009/01/04 17:46:14  kocolosk
 * add StDetectorId enumeration to standalone lib
 *
 * Revision 1.1  2008/12/29 16:01:38  kocolosk
 * support for a standalone StChargedPionEvent library. Requires checked-out
 * copies of St_base, StarClassLibrary, StEvent, StMiniMcEvent, and of course
 * StChargedPionAnalysisMaker.
 *
 *****************************************************************************/
