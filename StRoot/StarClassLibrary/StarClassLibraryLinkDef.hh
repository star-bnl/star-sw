// LinkDef.h
/***************************************************************************
 *
 * $Id: StarClassLibraryLinkDef.hh,v 1.11 2010/10/18 21:55:11 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:  
 *
 * This file tells rootcint for which classes the method interface
 * stubs should be generated. A trailing - in the class name tells
 * rootcint to not generate the Streamer() method. This is necessary
 * for those classes that need a customized Streamer() method. A
 * trailing ! in the class name tells rootcint to not generate the
 * operator>>(TBuffer &b, MyClass *&obj) method. This is necessary
 * to be able to write pointers to objects of classes not inheriting
 * from TObject. 
 *   1. This file must  be the last argument on the rootcint command line. 
 *   2. Note that this file name MUST contain the string:
 *      LinkDef.h or linkdef.h
 *
 ***************************************************************************
 *
 * $Log: StarClassLibraryLinkDef.hh,v $
 * Revision 1.11  2010/10/18 21:55:11  fisyak
 * Warn off for gcc4.5.1 64bits
 *
 * Revision 1.10  2006/01/09 23:47:27  fisyak
 * Add missing methods (found by Zhangbu) to Cint dictionary
 *
 * Revision 1.9  2005/09/22 20:09:21  fisyak
 * Make StLorentzVector persistent
 *
 * Revision 1.8  2005/07/06 18:49:57  fisyak
 * Replace StHelixD, StLorentzVectorD,StLorentzVectorF,StMatrixD,StMatrixF,StPhysicalHelixD,StThreeVectorD,StThreeVectorF by templated version
 *
 * Revision 1.7  2003/05/22 21:01:02  perev
 * Remove redundant dependency
 *
 * Revision 1.6  2003/05/07 20:34:23  perev
 * functions for CINT added
 *
 * Revision 1.5  2003/04/30 20:39:43  perev
 * Warnings cleanup. Modified lines marked VP
 *
 * Revision 1.4  2001/12/05 23:34:44  ullrich
 * Added Victor modifications to cope with error recovery.
 *
 * Revision 1.3  2001/02/15 22:23:31  fisyak
 * Make rootcint happy on Solaris
 *
 * Revision 1.2  2000/09/28 02:06:10  perev
 * non automatic streamer added
 *
 * Revision 1.1  1999/02/27 21:04:53  fisyak
 * Account change for StarClassLibrary
 *
 * Revision 1.1  1999/01/30 03:59:05  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:28:02  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifdef __CINT__
// General:
#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

#pragma link C++ class BetheBloch;

#pragma link C++ class StThreeVector<float>-;
#pragma link C++ class StThreeVector<double>-;
#pragma link C++ class StLorentzVector<float>+;
#pragma link C++ class StLorentzVector<double>+;
#pragma link C++ class StMatrix<float>-;
#pragma link C++ class StMatrix<double>-;
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
#pragma link C++ function operator<<(ostream&, const StThreeVector<float>&);                     
#pragma link C++ function operator>>(istream&, StThreeVector<float>&);                           

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
#pragma link C++ function operator<< (ostream&, const StLorentzVector<float>&);                 
#pragma link C++ function operator+ (const StLorentzVector<float>&, const StLorentzVector<float>&);        
#pragma link C++ function operator- (const StLorentzVector<float>&, const StLorentzVector<float>&);        
#pragma link C++ function operator* (const StLorentzVector<float>&, const StLorentzVector<float>&);        
#pragma link C++ function operator* (const StLorentzVector<float>&, double);                       
#pragma link C++ function operator* (double, const StLorentzVector<float>&);                         
#pragma link C++ function operator/ (const StLorentzVector<float>&, double);                         
#pragma link C++ function abs(const StLorentzVector<float>&);

#pragma link C++ function operator+ (const StLorentzVector<double>&, const StLorentzVector<float>&);  
#pragma link C++ function operator+ (const StLorentzVector<float>&, const StLorentzVector<double>&);        
#pragma link C++ function operator+ (const StLorentzVector<double>&, const StLorentzVector<double>&);        
#pragma link C++ function operator- (const StLorentzVector<double>&, const StLorentzVector<float>&);  
#pragma link C++ function operator- (const StLorentzVector<float>&, const StLorentzVector<double>&);        
#pragma link C++ function operator- (const StLorentzVector<double>&, const StLorentzVector<double>&);  
#pragma link C++ function operator<< (ostream&, const StLorentzVector<double>&);                 
#pragma link C++ function operator* (const StLorentzVector<double>&, const StLorentzVector<float>&);  
#pragma link C++ function operator* (const StLorentzVector<float>&, const StLorentzVector<double>&);        
#pragma link C++ function operator* (const StLorentzVector<double>&, const StLorentzVector<double>&);            
#pragma link C++ function operator* (const StLorentzVector<double>&, double);                     
#pragma link C++ function operator* (double, const StLorentzVector<double>&);                     
#pragma link C++ function operator/ (const StLorentzVector<double>&, double);                     
#pragma link C++ function abs(const StLorentzVector<double>&);

#pragma link C++ function operator*(const StMatrix<float>&,const StMatrix<float>&);            
#pragma link C++ function operator*(const StMatrix<float>&, const StThreeVector<float>&);      
#pragma link C++ function operator*(const StMatrix<float>&, const StThreeVector<double>&);      
#pragma link C++ function operator*(const StThreeVector<float>&, const StMatrix<float>&);      
#pragma link C++ function operator*(const StThreeVector<double>&, const StMatrix<float>&);      
#pragma link C++ function operator*(const StMatrix<float>&, const StLorentzVector<float>&);    
#pragma link C++ function operator*(const StMatrix<float>&, const StLorentzVector<double>&);    
#pragma link C++ function operator*(const StLorentzVector<float>&, const StMatrix<float>&);    
#pragma link C++ function operator*(const StLorentzVector<double>&, const StMatrix<float>&);    
#pragma link C++ function operator+(const StMatrix<float>&,const StMatrix<float>&);            
#pragma link C++ function operator-(const StMatrix<float>&,const StMatrix<float>&);            
#pragma link C++ function operator<<(ostream&, const StMatrix<float>&);                    
#pragma link C++ function norm_infinity(const StMatrix<float>&);                            
#pragma link C++ function normInfinity(const StMatrix<float>&);                             
#pragma link C++ function norm1(const StMatrix<float>&);                                    

#pragma link C++ function operator*(const StMatrix<double>&,const StMatrix<double>&);             
#pragma link C++ function operator*(const StMatrix<double>&,const StMatrix<float>&);             
#pragma link C++ function operator*(const StMatrix<float>&,const StMatrix<double>&);             
#pragma link C++ function operator*(const StMatrix<double>&, const StThreeVector<float>&);       
#pragma link C++ function operator*(const StMatrix<double>&, const StThreeVector<double>&);       
#pragma link C++ function operator*(const StThreeVector<float>&, const StMatrix<double>&);       
#pragma link C++ function operator*(const StThreeVector<double>&, const StMatrix<double>&);       
#pragma link C++ function operator*(const StMatrix<double>&, const StLorentzVector<float>&);     
#pragma link C++ function operator*(const StMatrix<double>&, const StLorentzVector<double>&);     
#pragma link C++ function operator*(const StLorentzVector<float>&, const StMatrix<double>&);     
#pragma link C++ function operator*(const StLorentzVector<double>&, const StMatrix<double>&);     
#pragma link C++ function operator+(const StMatrix<double>&,const StMatrix<double>&);             
#pragma link C++ function operator+(const StMatrix<float>&,const StMatrix<double>&);             
#pragma link C++ function operator+(const StMatrix<double>&,const StMatrix<float>&);             
#pragma link C++ function operator-(const StMatrix<double>&,const StMatrix<double>&);             
#pragma link C++ function operator-(const StMatrix<float>&,const StMatrix<double>&);             
#pragma link C++ function operator-(const StMatrix<double>&,const StMatrix<float>&);             
#pragma link C++ function operator<<(ostream&, const StMatrix<double>&);                     
#pragma link C++ function norm_infinity(const StMatrix<double>&);                             
#pragma link C++ function normInfinity(const StMatrix<double>&);                              
#pragma link C++ function norm1(const StMatrix<double>&);                                     

#pragma link C++ function operator== (const StHelix&, const StHelix&);                         
#pragma link C++ function operator!= (const StHelix&, const StHelix&);                         
#pragma link C++ function operator<<(ostream&, const StHelix&);                         
//IncFile=StThreeVector.hh
//IncFile=StThreeVectorF.hh
//IncFile=StThreeVectorD.hh
//IncFile=StLorentzVector.hh
//IncFile=StLorentzVectorF.hh
//IncFile=StLorentzVectorD.hh
//IncFile=StMatrix.hh
//IncFile=StMatrixF.hh
//IncFile=StMatrixD.hh
//IncFile=StHelix.hh
//IncFile=StHelixD.hh
//IncFile=StPhysicalHelix.hh
//IncFile=StPhysicalHelixD.hh
#pragma link C++ typedef StThreeVectorF;
#pragma link C++ typedef StThreeVectorD;
#pragma link C++ typedef StLorentzVectorF;
#pragma link C++ typedef StLorentzVectorD;
#pragma link C++ typedef StMatrixF;
#pragma link C++ typedef StMatrixD;
#pragma link C++ typedef StHelixD;
#pragma link C++ typedef StPhysicalHelixD;
#pragma link C++ typedef pairD;             

#endif

