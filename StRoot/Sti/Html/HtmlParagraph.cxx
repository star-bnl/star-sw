#include "HtmlParagraph.h"

HtmlParagraph::HtmlParagraph()
: HtmlTextElement("p"," ")
{}

HtmlParagraph::HtmlParagraph(const string &text)
: HtmlTextElement("p",text)
{}

HtmlParagraph::~HtmlParagraph()
{}


