// LinkDef.h
/***************************************************************************
 *
 * $Id: StSclRootLinkDef.hh,v 1.1 1999/01/30 03:59:05 fisyak Exp $
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
 * $Log: StSclRootLinkDef.hh,v $
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

#pragma link C++ class StThreeVectorF;
#pragma link C++ function operator<<(ostream&, const StThreeVectorF&);                  
#pragma link C++ function operator>>(istream&, StThreeVectorF&);                        
#pragma link C++ function abs(const StThreeVectorF&);                                      
#pragma link C++ function cross_product(const StThreeVectorF&, const StThreeVectorF&); 
#pragma link C++ function cross_product(const StThreeVectorF&, const StThreeVectorD&); 
#pragma link C++ function operator+ (const StThreeVectorF&, const StThreeVectorF&);    
#pragma link C++ function operator+ (const StThreeVectorF&, const StThreeVectorD&);    
#pragma link C++ function operator- (const StThreeVectorF&, const StThreeVectorF&);    
#pragma link C++ function operator- (const StThreeVectorF&, const StThreeVectorD&);    
#pragma link C++ function operator* (const StThreeVectorF&, const StThreeVectorF&);    
#pragma link C++ function operator* (const StThreeVectorF&, const StThreeVectorD&);    
#pragma link C++ function operator* (const StThreeVectorF&, double);                     
#pragma link C++ function operator* (double, const StThreeVectorF&);                     
#pragma link C++ function operator/ (const StThreeVectorF&, double);                     

#pragma link C++ class StThreeVectorD;
#pragma link C++ function operator<<(ostream&, const StThreeVectorD&);                     
#pragma link C++ function operator>>(istream&, StThreeVectorD&);                           
#pragma link C++ function abs(const StThreeVectorD&);                                         
#pragma link C++ function cross_product(const StThreeVectorD&, const StThreeVectorD&);    
#pragma link C++ function cross_product(const StThreeVectorD&, const StThreeVectorF&);    
#pragma link C++ function operator+ (const StThreeVectorD&, const StThreeVectorD&);       
#pragma link C++ function operator+ (const StThreeVectorD&, const StThreeVectorF&);       
#pragma link C++ function operator- (const StThreeVectorD&, const StThreeVectorF&);       
#pragma link C++ function operator- (const StThreeVectorD&, const StThreeVectorD&);       
#pragma link C++ function operator* (const StThreeVectorD&, const StThreeVectorF&);       
#pragma link C++ function operator* (const StThreeVectorD&, const StThreeVectorD&);       
#pragma link C++ function operator* (const StThreeVectorD&, double);                        
#pragma link C++ function operator* (double, const StThreeVectorD&);                        
#pragma link C++ function operator/ (const StThreeVectorD&, double);                        

#pragma link C++ class StLorentzVectorF;
#pragma link C++ function operator<< (ostream&, const StLorentzVectorF&);                 
#pragma link C++ function operator+ (const StLorentzVectorF&, const StLorentzVectorF&);        
#pragma link C++ function operator- (const StLorentzVectorF&, const StLorentzVectorF&);        
#pragma link C++ function operator* (const StLorentzVectorF&, const StLorentzVectorF&);        
#pragma link C++ function operator* (const StLorentzVectorF&, double);                       
#pragma link C++ function operator* (double, const StLorentzVectorF&);                         
#pragma link C++ function operator/ (const StLorentzVectorF&, double);                         
#pragma link C++ function abs(const StLorentzVectorF&);

#pragma link C++ class StLorentzVectorD;
#pragma link C++ function operator+ (const StLorentzVectorD&, const StLorentzVectorF&);  
#pragma link C++ function operator+ (const StLorentzVectorF&, const StLorentzVectorD&);        
#pragma link C++ function operator+ (const StLorentzVectorD&, const StLorentzVectorD&);        
#pragma link C++ function operator- (const StLorentzVectorD&, const StLorentzVectorF&);  
#pragma link C++ function operator- (const StLorentzVectorF&, const StLorentzVectorD&);        
#pragma link C++ function operator- (const StLorentzVectorD&, const StLorentzVectorD&);  
#pragma link C++ function operator<< (ostream&, const StLorentzVectorD&);                 
#pragma link C++ function operator* (const StLorentzVectorD&, const StLorentzVectorF&);  
#pragma link C++ function operator* (const StLorentzVectorF&, const StLorentzVectorD&);        
#pragma link C++ function operator* (const StLorentzVectorD&, const StLorentzVectorD&);            
#pragma link C++ function operator* (const StLorentzVectorD&, double);                     
#pragma link C++ function operator* (double, const StLorentzVectorD&);                     
#pragma link C++ function operator/ (const StLorentzVectorD&, double);                     
#pragma link C++ function abs(const StLorentzVectorD&);

#pragma link C++ class StMatrixF-;
#pragma link C++ class StMatrixRowF;
#pragma link C++ class StMatrixRowConstF;
#pragma link C++ function operator*(const StMatrixF&,const StMatrixF&);            
#pragma link C++ function operator*(const StMatrixF&, const StThreeVectorF&);      
#pragma link C++ function operator*(const StMatrixF&, const StThreeVectorD&);      
#pragma link C++ function operator*(const StThreeVectorF&, const StMatrixF&);      
#pragma link C++ function operator*(const StThreeVectorD&, const StMatrixF&);      
#pragma link C++ function operator*(const StMatrixF&, const StLorentzVectorF&);    
#pragma link C++ function operator*(const StMatrixF&, const StLorentzVectorD&);    
#pragma link C++ function operator*(const StLorentzVectorF&, const StMatrixF&);    
#pragma link C++ function operator*(const StLorentzVectorD&, const StMatrixF&);    
#pragma link C++ function operator+(const StMatrixF&,const StMatrixF&);            
#pragma link C++ function operator-(const StMatrixF&,const StMatrixF&);            
#pragma link C++ function operator<<(ostream&, const StMatrixF&);                    
#pragma link C++ function norm_infinity(const StMatrixF&);                            
#pragma link C++ function normInfinity(const StMatrixF&);                             
#pragma link C++ function norm1(const StMatrixF&);                                    

#pragma link C++ class StMatrixD-;
#pragma link C++ class StMatrixRowD;
#pragma link C++ class StMatrixRowConstD;
#pragma link C++ function operator*(const StMatrixD&,const StMatrixD&);             
#pragma link C++ function operator*(const StMatrixD&,const StMatrixF&);             
#pragma link C++ function operator*(const StMatrixF&,const StMatrixD&);             
#pragma link C++ function operator*(const StMatrixD&, const StThreeVectorF&);       
#pragma link C++ function operator*(const StMatrixD&, const StThreeVectorD&);       
#pragma link C++ function operator*(const StThreeVectorF&, const StMatrixD&);       
#pragma link C++ function operator*(const StThreeVectorD&, const StMatrixD&);       
#pragma link C++ function operator*(const StMatrixD&, const StLorentzVectorF&);     
#pragma link C++ function operator*(const StMatrixD&, const StLorentzVectorD&);     
#pragma link C++ function operator*(const StLorentzVectorF&, const StMatrixD&);     
#pragma link C++ function operator*(const StLorentzVectorD&, const StMatrixD&);     
#pragma link C++ function operator+(const StMatrixD&,const StMatrixD&);             
#pragma link C++ function operator+(const StMatrixF&,const StMatrixD&);             
#pragma link C++ function operator+(const StMatrixD&,const StMatrixF&);             
#pragma link C++ function operator-(const StMatrixD&,const StMatrixD&);             
#pragma link C++ function operator-(const StMatrixF&,const StMatrixD&);             
#pragma link C++ function operator-(const StMatrixD&,const StMatrixF&);             
#pragma link C++ function operator<<(ostream&, const StMatrixD&);                     
#pragma link C++ function norm_infinity(const StMatrixD&);                             
#pragma link C++ function normInfinity(const StMatrixD&);                              
#pragma link C++ function norm1(const StMatrixD&);                                     

#pragma link C++ class StHelixD;
#pragma link C++ class pairD;
#pragma link C++ function operator== (const StHelixD&, const StHelixD&);                         
#pragma link C++ function operator!= (const StHelixD&, const StHelixD&);                         
#pragma link C++ function operator<<(ostream&, const StHelixD&);                         

#pragma link C++ class StPhysicalHelixD;

#endif
