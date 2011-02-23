#include "BGViewport.h"

#include <QPointF>
void
BajaGIS::BGViewport::mousePressEvent (QMouseEvent *event)
{
  QPointF point;
  switch (event->button ())
  {
    case Qt::LeftButton:
      point = mapToScene (event->x (), event->y ());
      scene()->addEllipse (point.x (), point.y (), 5, 5);
      break;
    case Qt::RightButton:
    case Qt::MidButton:
    default:
      break;
  }
}

