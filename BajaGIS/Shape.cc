#include "Shape.h"

#include <QPainter>


// class Shape

BajaGIS::Shape::~Shape ()
{ }


// class PointShape

BajaGIS::PointShape::PointShape (qreal x, qreal y)
  : QGraphicsItem (0)
  , _x (x), _y (y)
{ }

BajaGIS::PointShape::~PointShape ()
{ }

QRectF
BajaGIS::PointShape::boundingRect () const
{
  return QRectF (_x, _y, 0, 0);
}

void
BajaGIS::PointShape::paint (QPainter *painter,
    QStyleOptionGraphicsItem const *option, QWidget *widget)
{
  Q_UNUSED (option);
  Q_UNUSED (widget);

  painter->drawPoint (_x, _y);
}


// class PolylineShape

BajaGIS::PolylineShape::PolylineShape (qreal x, qreal y)
  : QGraphicsPathItem (0)
{
  QPainterPath p = path ();
  p.moveTo (x, y);
  setPath (p);
}

BajaGIS::PolylineShape::~PolylineShape ()
{ }

void
BajaGIS::PolylineShape::addPoint (qreal x, qreal y)
{
  QPainterPath p = path ();
  p.lineTo (x, y);
  setPath (p);
}

// class PolygonShape

BajaGIS::PolygonShape::PolygonShape (qreal x, qreal y)
  : QGraphicsPolygonItem (0)
{
  addPoint (x, y);
}

BajaGIS::PolygonShape::~PolygonShape ()
{ }

void
BajaGIS::PolygonShape::addPoint (qreal x, qreal y)
{
  QPolygonF poly = polygon ();
  poly << QPointF (x, y);
  setPolygon (poly);
}

