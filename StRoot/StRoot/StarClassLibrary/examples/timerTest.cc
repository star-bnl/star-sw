#include "StTimer.hh"
#include <time.h>
#include <Stiostream.h>
#include <math.h>


int main()
{
   StTimer timer, totalTimer;
   
   totalTimer.start();
   timer.start();

   // Spend 5 busy sec.
   time_t begin = time(0);
   time_t now = begin;
   while (now - begin < 5) now = time(0);

   timer.stop();

   cout << "Test 1:" << endl;
   cout << "This test should require less than 5 sec CPU time. \n"
        << "The exact amount depends strongly on the system." << endl;
   cout << "The measured elapsed CPU time is: "
	<< timer.elapsedTime() << " sec\n" << endl;

   timer.reset();
   timer.start();

   const size_t NumSqrt = 1000000;
   double x;
   for (int i=0; i<NumSqrt; i++) x = ::sqrt(double(i));

   timer.stop();
   
   cout << "Test 2:" << endl;
   cout << "The CPU time to calculate " << NumSqrt
        << " square roots is: "
        << timer.elapsedTime() << " sec\n" << endl;

   cout << "The total amount of CPU seconds used \n"
        << "to execute this program is: ";
   totalTimer.stop();
   cout << totalTimer.elapsedTime() << " sec." << endl;
   
   return 0;
}
