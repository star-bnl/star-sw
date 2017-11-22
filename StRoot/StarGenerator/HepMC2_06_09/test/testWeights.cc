//////////////////////////////////////////////////////////////////////////
// testWeights.cc
// 
// garren@fnal.gov, January 2010
// test WeightContainer
//////////////////////////////////////////////////////////////////////////

#include <assert.h>
#include <iostream>
#include <string>
#include <vector>

#include "HepMC/WeightContainer.h"
#include <stdexcept>

int main() {

   HepMC::WeightContainer w;

   // original functionality
   w.push_back(2.0);
   w.push_back(4.56);
   assert( w[0] == 2.0 );
   assert( w[1] == 4.56 );
   assert( w.size() == 2 );
   assert( !w.empty() );
   
   std::vector<double> vec;
   for( int i = 0; i < 15; ++i ) {
       double x = (double)i + 0.14*(double)i;
       vec.push_back( x );
   }
   double v1 = vec[0];
   w = vec;
   assert( w.size() == 15 );
   w.pop_back();
   assert( w.size() == 14 );

   // new functionality
   std::size_t vs = vec.size();
   std::string nm = "tau";
   w[nm] = 3.1;
   assert( w.size() == (vs) );
   w["my_pdf"] = 11.3445;
   //w['PDF_SET_3'] = 10.33;
   assert( w[nm] == 3.1 );
   assert( w[0] == v1 );
   assert( w.size() == (vs+1) );
   assert( w[vs-1] == 3.1 );
   
   const HepMC::WeightContainer wc = w;
   assert( wc[nm] == 3.1 );
   // lookup a nonexistent name
   try {
       double x = wc["bad"];
       std::cout << "lookup of nonexistent name returns " << x << std::endl;
   }
   catch (std::exception& e) {
       std::cout << e.what() << std::endl;
       std::cout << "HepMC testWeights: the above error is intentional" << std::endl;
   }
   // another option for user code:
   if( wc.has_key("bad") ) {
       double x = wc["bad"];
       std::cout << "lookup of nonexistent name returns " << x << std::endl;
   }

   w.write();

   return 0;
}
