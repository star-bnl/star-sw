#include "HtmlTextElement.h"

HtmlTextElement::HtmlTextElement(const string & name, const string &title)
: HtmlElement(name),
  _text(title)
{}

HtmlTextElement::~HtmlTextElement()
{}

void HtmlTextElement::write(ofstream& target)
{
  target << "<" << _name << ">" << endl
         << _text<<endl
         << "</" << _name << ">" << endl;
}

void HtmlTextElement::setText(const string& text)
{
  _text = text;
}

const string & HtmlTextElement::getText()
{
  return _text;
}

