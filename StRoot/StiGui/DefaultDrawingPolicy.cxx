#include "DefaultDrawingPolicy.h"
#include "Sti/Base/EditableParameter.h"

DefaultDrawingPolicy::DefaultDrawingPolicy(const string& name, 
					   const string& description,
					   int color, 
					   int style, 
					   double size)
  : DrawingPolicy<StiDrawable>(name,description),
    _color(color), 
    _style(style), 
    _size(size)
{}

DefaultDrawingPolicy::DefaultDrawingPolicy(const DefaultDrawingPolicy& policy)
  : DrawingPolicy<StiDrawable>(policy),
  _color(policy._color), 
  _style(policy._style), 
  _size(policy._size)
{}

const DefaultDrawingPolicy& DefaultDrawingPolicy::operator=(const DefaultDrawingPolicy& policy)
{
  _color=policy._color;
  _style=policy._style; 
  _size=policy._size;
  clear();
  initialize();
  return *this;
}
  
DefaultDrawingPolicy::~DefaultDrawingPolicy()
{}

void DefaultDrawingPolicy::police(StiDrawable * object)
{
  object->setColor(_color);
  object->setStyle(_style);
  object->setSize(_size);
}

void DefaultDrawingPolicy::initialize()
{
  add(new EditableParameter("Color","Color",&_color,_color,0,20,1,0));
  add(new EditableParameter("Style", "Style", &_style, _style, 0,30,1,0));
  add(new EditableParameter("Size", "Size", &_size, _size, 0.1,2.,0.1,0));
}
