#ifndef LORGAMMA_H
#define LORGAMMA_H

/*
Functions for safe manipulations with betta and gamma kinematical 
parameters without loss of precision at very extremal values,
such as betta very close to zero or to unity.

The main representation of gamma is gamma - 1 , to assure that
gamma will be meaningfull for little betta.

Author I. B. Smirnov, 1999 - 2002.
*/

double lorgamma_1(double betta); // gamma - 1
double lorbetta(double gamma_1);
double lorbetta2(double gamma_1);
double lorbetta(double momentum, double mass, double speed_of_light2);
//double lorbetta_1(double momentum, double mass, double speed_of_light2); //
//  1.0 - betta;

#endif
