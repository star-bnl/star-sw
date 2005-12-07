/** 
 * @file  StiDebug.h
 */
#ifndef StiDebug_H
#define StiDebug_H 1
#include "TDataSet.h"
#include "TArrayF.h"

/** 
 * @class StiDebug
 */
class StiKalmanTrack; 
class StiDebug 
{
public:
static void  Break(int kase);
static void  showTh(StiKalmanTrack *kt);
static void  show(StiKalmanTrack *kt);

};


class StiAux_t { 
public:
double resY() const {return xnl[1]-xhl[1];}
double resZ() const {return xnl[2]-xhl[2];}
double difY() const {return unl[1]-xhl[1];}
double difZ() const {return unl[2]-xhl[2];}


public: 
			// LOCAL FRAME
float xnl[3]; 		//X,Y,Z of Node. 		
float xhl[3];		//X,Y,Z of Hit.  		
float unl[3];		//X,Y,Z of Untouched Node.  	
float ca; 		//Cross angle
float nYY; float nZZ;   //Y & Z errors of Node
float hYY; float hZZ;	//Y & Z errors of Hit
float uYY; float uZZ;	//Y & Z errors of Untouched Node

			// GLOBAL FRAME
float xng[3]; 		//X,Y,Z of Node.
float xhg[3];		//X,Y,Z of Hit
float psi;    		//Psi angle
float dip;		//Dip angle

			// INVARIANT
float rho;		//Curvature
float chi2; 
//reserv
float reserv[10];
};

class StiAux : public TDataSet 
{
public:
   StiAux();
StiAux_t*	Get(int id) const;
int 		AddIt(StiAux_t *add);
int 		GetN() const  		{return fN;}
void		PrintIt(int id);
public:
int fN;
TArrayF fArr;
ClassDef(StiAux,1)

};
#endif

