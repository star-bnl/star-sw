/*:>--------------------------------------------------------------------
**: FILE:       pamcc.cc
**: HISTORY:
**:             00jan93-v000a-hpl- Created by stic Version
**:  Id: idl.y,v 1.8 1996/10/15 18:33:35 ward Exp  
**:<------------------------------------------------------------------*/
#include "pamcc.h"

#define MAX(A,B) (A>B ? A : B)
#define MIN(A,B) (A<B ? A : B)

//----------------------------------------------------------------------
class pamobj {
public:
   pamobj(long maxr, long nr, SCALARS_ST *data){
      my_maxrows = maxr;
      my_nrows = nr;
      my_data = data;
      my_stride = sizeof(SCALARS_ST);
   };
   virtual ~pamobj(){};
   virtual long nrows(){return my_nrows;};
   virtual long maxrows(){return my_maxrows;};
   virtual long rowsize(){return my_stride;};
   virtual short aShort(long nrow){
      return my_data[nrow].aShort; };
   virtual unsigned short aUshort(long nrow){
      return my_data[nrow].aUshort; };
   virtual long aLong(long nrow){
      return my_data[nrow].aLong; };
   virtual unsigned long aUlong(long nrow){
      return my_data[nrow].aUlong; };
   virtual char aChar(long nrow){
      return my_data[nrow].aChar; };
   virtual unsigned char aOctet(long nrow){
      return my_data[nrow].aOctet; };
   virtual float aFloat(long nrow){
      return my_data[nrow].aFloat; };
   virtual double aDouble(long nrow){
      return my_data[nrow].aDouble; };
private:
   SCALARS_ST *my_data;
   long my_nrows;
   long my_maxrows;
   long my_stride;
};
//----------------------------------------------------------------------

long pamcc_(
  TABLE_HEAD_ST             *t1_h,        SCALARS_ST               *t1 ,
  TABLE_HEAD_ST             *t2_h,        VECTORS_ST               *t2 )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    pamcc_
**: DESCRIPTION: Physics Analysis Module ANSI C template.
**: ARGUMENTS:
**:       IN:
**:    INOUT:
**:      OUT:
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/

   pamobj *myObj = new pamobj(t1_h->maxlen,t1_h->nok,t1);

   printf("nok = %d\n",myObj->nrows());
   printf("maxlen = %d\n",myObj->maxrows());
   printf("row size = %d\n",myObj->rowsize());
   for( int i=0;i<MIN(10,t1_h->nok);i++ ){
      printf("%d aShort = %d\n",i,myObj->aShort(i));
   }

   return STAFCV_OK;
}
