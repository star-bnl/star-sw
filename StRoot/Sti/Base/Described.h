#if !defined(AFX_DESCRIBED_H__AD46D0D8_4A85_45BE_AB18_B64D1E2A4658__INCLUDED_)
#define AFX_DESCRIBED_H__AD46D0D8_4A85_45BE_AB18_B64D1E2A4658__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include <string>
using std::string;

/*! 
\class Described
This class encapsulates the notion of "Described". It should be used as
base class to provide a "Described" property to objects.

\author Claude A Pruneau
*/

class Described  
{
public:
	virtual ~Described();

   /// Set the Describe of the object
   void setDescription(const string & description);

   /// Get the Describe of the object
   const string getDescription() const;

   /// Determine whether Describe is set, i.e object has a Describe
   bool isDescribed() const;

   /// Determine whether Describe equals given Describe
   bool isDescription(const string & description) const;

   /// Determine whether Describe equals that of given object
   bool sameDescriptionAs(const Described & described) const;

   protected:

   /// Only derived class are Described
	Described(const string & aDescribe=" ");

   string _description;
};

#endif // !defined(AFX_DESCRIBED_H__AD46D0D8_4A85_45BE_AB18_B64D1E2A4658__INCLUDED_)
