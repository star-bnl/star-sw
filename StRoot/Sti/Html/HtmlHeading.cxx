#include "HtmlHeading.h"

HtmlHeading::HtmlHeading(const string& level)
: HtmlTextElement(string("h")+level,"")
{}

HtmlHeading::HtmlHeading(const string& level, const string &text)
: HtmlTextElement(string("h")+level,text)
{}

HtmlHeading::~HtmlHeading()
{}

